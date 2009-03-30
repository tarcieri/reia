%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, ast/2, transform/2]).

% Internal state of the SSA transform
-record(state, {mode=normal, bindings}).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ast(Ast, []).
  
ast(Ast, Variables) ->
  Dict = dict:from_list([{Variable, 0} || Variable <- Variables]),
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, #state{bindings=Dict}, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.

% Module declarations create a new scope
transform(State, {module, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, #state{bindings=dict:new()}, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {module, Line, Name, Expressions2}};
  
% Class declarations create a new scope
transform(State, {class, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, #state{bindings=dict:new()}, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {class, Line, Name, Expressions2}};
transform(State, {class, Line, Name, Ancestor, Expressions}) ->
  {stop, _, {class, _, _, Expressions2}} = transform(State, {class, Line, Name, Expressions}),
  {stop, State, {class, Line, Name, Ancestor, Expressions2}};
  
% Function declarations create a new scope
transform(State, {function, Line, Name, Arguments, Block, Expressions}) ->
  % Create a new scope with dict:new()
  {ok, #state{bindings=Dict}, Arguments2} = reia_visitor:transform(
    Arguments, 
    #state{mode=argument, bindings=dict:new()}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict2}, Block2} = reia_visitor:transform(Block, #state{mode=argument, bindings=Dict}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, #state{bindings=Dict2}, fun transform/2),

  % Return to the original scope
  {stop, State, {function, Line, Name, Arguments2, Block2, Expressions2}};

% Lambdas close over the outer scope, bind arguments, but don't affect the outer scope
transform(#state{bindings=Dict} = State, {lambda, Line, Arguments, Expressions}) ->
  {ok, #state{bindings=Dict2}, Arguments2} = reia_visitor:transform(
    Arguments, 
    #state{mode=argument, bindings=Dict}, 
    fun transform/2
  ),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, #state{bindings=Dict2}, fun transform/2),
  {stop, State, {lambda, Line, Arguments2, Expressions2}};

% Function references need a pass for the receiver
transform(#state{mode=Mode} = State, {funref, Line, Receiver, {identifier, _, _} = Name}) ->
  {ok, #state{bindings=Dict}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2),
  {stop, #state{mode=Mode, bindings=Dict}, {funref, Line, Receiver2, Name}};
    
% Function calls need a pass over their arguments and block
transform(#state{mode=Mode, bindings=Dict} = State, {funcall, Line, {identifier, _Line, Name} = Identifier, Arguments, Block}) ->
  {ok, #state{bindings=Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  {ok, #state{bindings=Dict3}, Block2} = reia_visitor:transform(Block, #state{mode=Mode, bindings=Dict2}, fun transform/2),
  Identifier2 = case dict:find(Name, Dict) of
    {ok, Version} ->
      {var, Line, ssa_name(Name, Version)};
    error ->
      Identifier
  end,  
  Node = {funcall, Line, Identifier2, Arguments2, Block2},
  {stop, #state{mode=Mode, bindings=Dict3}, Node};

transform(#state{mode=Mode} = State, {funcall, Line, Receiver, Name, Arguments, Block}) ->
  {ok, #state{bindings=Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, #state{bindings=Dict3}, Arguments2} = reia_visitor:transform(
    Arguments, 
    #state{mode=Mode, bindings=Dict2}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict4}, Block2} = reia_visitor:transform(
    Block, 
    #state{mode=Mode, bindings=Dict3}, 
    fun transform/2
  ),
  Node = {funcall, Line, Receiver2, Name, Arguments2, Block2},
  {stop, #state{mode=Mode, bindings=Dict4}, Node};

% Method calls are totally freaking weird due to the need to shuffle hidden 
% state around.  This pass in fact colludes with two others, reia_ivars
% before it and reia_methods afterward.  It's insane!  Anyway if you're
% still trying to comprehend what's going on the prevous pass and next
% pass need knowledge of the binding to properly complete the overall
% transformation they're trying to accomplish.  This pass versions the hidden
% state to ensure it's passed around correctly.
%
% Here's the node structure:
%
% Input node:  {method_call, Line, Name, Arguments, Block}
% Output node: {method_call, Line, Name, Arguments, Block, IvarsIn, IvarsOut}
transform(#state{mode=Mode, bindings=Dict} = State, {method_call, Line, {identifier, _Line, Name} = Identifier, Arguments, Block}) ->
  {ok, #state{bindings=Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  {ok, #state{bindings=Dict3}, Block2} = reia_visitor:transform(Block, #state{mode=Mode, bindings=Dict2}, fun transform/2),
  {Dict4, Node} = case dict:find(Name, Dict) of
    {ok, Version} ->
      N = {funcall, Line, {var, Line, ssa_name(Name, Version)}, Arguments2},
      {Dict2, N};
    error ->
      build_method_call(Line, Identifier, Arguments2, Block2, Mode, Dict3)
  end,
  {stop, #state{mode=Mode, bindings=Dict4}, Node};

transform(#state{mode=Mode} = State, {erl_funcall, Line, Module, Function, Arguments}) ->
  {ok, #state{bindings=Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Node = {erl_funcall, Line, Module, Function, Arguments2},
  {stop, #state{mode=Mode, bindings=Dict2}, Node};
  
% Arguments should initialize new entries in the SSA dict
transform(#state{mode=argument, bindings=Dict}, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      throw({error, {Line, lists:flatten(io_lib:format("argument already bound: '~s'", [Name]))}});
    error ->
      Dict2 = dict:store(Name, 0, Dict),
      Node = {identifier, Line, ssa_name(Name, 0)},
      {stop, #state{mode=argument, bindings=Dict2}, Node}
  end;

% Match expressions mutate variables on the LHS
transform(#state{mode=Mode} = State, {match, Line, In1, In2}) ->
  {ok, #state{bindings=Dict},  Out2} = reia_visitor:transform(In2, State, fun transform/2),
  {ok, #state{bindings=Dict2}, Out1} = reia_visitor:transform(
    In1, 
    #state{mode=match, bindings=Dict}, 
    fun transform/2
  ),
  {stop, #state{mode=Mode, bindings=Dict2}, {match, Line, Out1, Out2}};
  
% Case expressions bind variables in clauses
transform(#state{mode=Mode} = State, {'case', Line, Expression, Clauses}) ->
  {ok, #state{bindings=Dict}, Expression2} = reia_visitor:transform(Expression, State, fun transform/2),
  {Dict2, Clauses2} = process_clauses('case', Dict, Clauses),
  {stop, #state{mode=Mode, bindings=Dict2}, {'case', Line, Expression2, Clauses2}};

% Receive expressions work just like case statements
transform(#state{mode=Mode, bindings=Dict}, {'receive', Line, Clauses}) ->
  {Dict2, Clauses2} = process_clauses('case', Dict, Clauses),
  {stop, #state{mode=Mode, bindings=Dict2}, {'receive', Line, Clauses2}};
    
transform(#state{mode=Mode, bindings=Dict}, {'receive', Line, Clauses, AfterClause}) ->
  {Dict2, Clauses2} = process_clauses('case', Dict, Clauses),
  {stop, #state{mode=Mode, bindings=Dict2}, {'receive', Line, Clauses2, AfterClause}};

% Case clauses match against patterns
transform(#state{mode='case', bindings=Dict}, {clause, Line, Pattern, Expressions}) ->
  {ok, #state{bindings=Dict2}, Pattern2} = reia_visitor:transform(
    Pattern, 
    #state{mode=match, bindings=Dict}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict3}, Expressions2} = reia_visitor:transform(
    Expressions, 
    #state{mode=normal, bindings=Dict2}, 
    fun transform/2
  ),
  {stop, #state{mode='case', bindings=Dict3}, {clause, Line, Pattern2, Expressions2}};
    
% Catch clauses match against patterns
transform(#state{mode=Mode, bindings=Dict}, {'catch', Line, Pattern, Expressions}) ->
  {ok, #state{bindings=Dict2}, Pattern2} = reia_visitor:transform(
    Pattern, 
    #state{mode=match, bindings=Dict}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict3}, Expressions2} = reia_visitor:transform(
    Expressions, 
    #state{mode=normal, bindings=Dict2}, 
    fun transform/2
  ),
  {stop, #state{mode=Mode, bindings=Dict3}, {'catch', Line, Pattern2, Expressions2}};
    
% List comprehensions can access the outer scope but have a scope of their own
transform(#state{bindings=Dict} = State, {'lc', Line, Transform, Expressions}) ->
  {ok, #state{bindings=Dict2}, Expressions2} = reia_visitor:transform(
    Expressions, 
    #state{mode=normal, bindings=Dict}, 
    fun transform/2
  ),
  {ok, _, Transform2} = reia_visitor:transform(
    Transform, 
    #state{mode=normal, bindings=Dict2}, 
    fun transform/2
  ),
  {stop, State, {'lc', Line, Transform2, Expressions2}};

% Generate expressions match a pattern
transform(#state{mode=Mode, bindings=Dict}, {'generate', Line, Pattern, List}) ->
  {ok, #state{bindings=Dict2}, Pattern2} = reia_visitor:transform(
    Pattern, 
    #state{mode=match, bindings=Dict}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict3}, List2} = reia_visitor:transform(
    List, 
    #state{mode=Mode, bindings=Dict2}, 
    fun transform/2
  ),
  {stop, #state{mode=Mode, bindings=Dict3}, {'generate', Line, Pattern2, List2}};

% Normally identifiers are mapped to their latest version
transform(#state{mode=normal, bindings=Dict} = State, {identifier, Line, Name} = Node) ->
  case dict:find(Name, Dict) of
    {ok, Version} ->
      Node2 = {identifier, Line, ssa_name(Name, Version)},
      {stop, State, Node2};
    error ->
      case internal_variable(Name) of
        true ->
          transform(#state{mode=normal, bindings=dict:store(Name, 0, Dict)}, Node);
        false ->
          throw({error, {Line, lists:flatten(io_lib:format("unbound variable: '~s'", [Name]))}})
      end
  end;
  
% On the LHS of match expressions, variables are assigned a new version
transform(#state{mode=match, bindings=Dict}, {identifier, Line, Name}) ->
  {Dict2, Version2} = case dict:find(Name, Dict) of
    {ok, Version} ->
      {dict:store(Name, Version + 1, Dict), Version + 1};
    error ->
      {dict:store(Name, 0, Dict), 0}
  end,
  Node = {identifier, Line, ssa_name(Name, Version2)},
  {stop, #state{mode=match, bindings=Dict2}, Node};
  
% Vars are generated internally by earlier stages of the compiler and work 
% like identifiers, mapped to their latest version
transform(#state{mode=normal} = State, {var, Line, Name}) ->
  {stop, State2, {identifier, _, Name2}} = transform(State, {identifier, Line, Name}),
  {stop, State2, {var, Line, Name2}};
  
transform(#state{mode=match} = State, {var, Line, Name}) ->
  {stop, State2, {identifier, _, Name2}} = transform(State, {identifier, Line, Name}),
  {stop, State2, {var, Line, Name2}};
   
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.
  
% Process clauses, giving each branch its own dict and ensuring the highest
% numbered versions of each variable are bound at the end of each clause
process_clauses(Type, Vars, Clauses) ->
  % Proceed with the normal SSA transformation on each clause
  Clauses2 = lists:map(fun(Clause) ->
    {ok, #state{bindings=Binding}, Clause2} = reia_visitor:transform(
      Clause, 
      #state{mode=Type, bindings=Vars}, 
      fun transform/2
    ),
    {Binding, Clause2}
  end, Clauses),
  
  % Extract a nested list of bound variables and SSA versions for each clause
  ClauseVars = [Binding || {Binding, _} <- Clauses2],
  
  % Build a dict of the highest version numbers of any variables referenced 
  % in any clause
  NewestVars = lists:foldl(fun update_binding/2, dict:new(), ClauseVars),
    
  % Bind any "unsafe" variables in the clauses to the latest SSA version
  Clauses3 = [bind_unsafe_variables({Vars, Binding, NewestVars}, Clause) || {Binding, Clause} <- Clauses2],
  
  % Determine the final SSA versions of all variables known for the current binding
  FinalVars = update_binding(Vars, NewestVars),
  
  {FinalVars, Clauses3}.

% Update the OldBinding dictionary with the newest version numbers from 
% NewBinding, producing a new dictionary
update_binding(OldBinding, NewBinding) ->
  lists:foldl(fun({Var, Version}, NewestVars) ->
    case dict:find(Var, NewestVars) of
      {ok, NewestVersion} ->
        if Version > NewestVersion ->
          dict:store(Var, Version, NewestVars);
        true ->
          NewestVars
        end;
      error ->
        dict:store(Var, Version, NewestVars)
    end    
  end, OldBinding, dict:to_list(NewBinding)).
  
% The versions variables are left in after the SSA transform runs are not 
% necessarily the latest ones used among the entire set of clauses.  Worse, 
% some clauses may have bound new variables.  These variables are "unsafe"
% if any are referenced after the statement the clauses belong to.  To avoid
% this, we need to ensure that all clauses bind the same set of variables upon
% completion, while still preserving the original return value.
bind_unsafe_variables(Variables, {clause, Line, Pattern, Expressions}) ->
  {DestVersions, SourceVersions} = enumerate_unsafe_variables(Variables),
  
  % Build the match expression to bind potentially unsafe variables
  DestTuple   = {tuple, Line, [build_form_for_variable(Var, Line) || Var <- DestVersions]},
  SourceTuple = {tuple, Line, [build_form_for_variable(Var, Line) || Var <- SourceVersions]},
  MatchExpr = {match, Line, DestTuple, SourceTuple},
  
  {clause, Line, Pattern, process_return_value(Line, Expressions, MatchExpr)}.

% Build a list of potentially unsafe variables that need to be bound along with
% their source and destination versions  
enumerate_unsafe_variables({OrigVars, ClauseVars, FinalVars}) ->
  lists:foldl(fun({Var, FinalVersion}, {DestVersions, SourceVersions}) ->
    case dict:find(Var, ClauseVars) of
      {ok, ClauseVersion} ->
        if ClauseVersion < FinalVersion ->
          {[{Var, FinalVersion}|DestVersions], [{Var, ClauseVersion}|SourceVersions]};
        true ->
          {DestVersions, SourceVersions}
        end;
      error ->
        case dict:find(Var, OrigVars) of
          {ok, OrigVersion} ->
            {[{Var, FinalVersion}|DestVersions], [{Var, OrigVersion}|SourceVersions]};
          error ->
            case internal_variable(Var) of
              true -> Version = 0;
              false -> Version = nil
            end,
            {[{Var, FinalVersion}|DestVersions], [{Var, Version}|SourceVersions]}
        end
      end
  end, {[], []}, dict:to_list(FinalVars)).

% Convert a tuple returned from enumerate_unsafe_variables into the appropriate
% Erlang abstract form
build_form_for_variable({_, nil}, Line) ->
  {atom, Line, nil};
build_form_for_variable({Name, Version}, Line) ->
  {identifier, Line, ssa_name(Name, Version)}.
  
%% Convert a method's return value into a gen_server reply
process_return_value(Line, [], MatchExpr) ->
  [MatchExpr, {atom, Line, 'nil'}];
process_return_value(Line, Expressions, MatchExpr) ->
  [Result|Expressions2] = lists:reverse(Expressions),
  ReturnVar = {identifier, Line, list_to_atom("__clause_return_value_" ++ reia_compiler:nonce())},
  Result2 = {match, Line, ReturnVar, Result},
  lists:reverse([ReturnVar, MatchExpr, Result2|Expressions2]). 
  
% Generate the SSA name for a given variable, which takes the form name_version
ssa_name(Name, Version) ->
  Name2 = lists:flatten(io_lib:format("_~s_~w", [Name, Version])),
  list_to_atom(Name2).
  
% Generate a method_call node to be processed by reia_class
build_method_call(Line, Identifier, Arguments, Block, Mode, Dict) ->  
  {ok, #state{bindings=Dict2}, IvarsIn}  = reia_visitor:transform(
    {identifier, Line, '__instance_variables'}, 
    #state{mode=Mode, bindings=Dict}, 
    fun transform/2
  ),
  {ok, #state{bindings=Dict3}, IvarsOut} = reia_visitor:transform(
    {identifier, Line, '__instance_variables'}, 
    #state{mode=match, bindings=Dict2}, 
    fun transform/2
  ),
  Node = {method_call, Line, Identifier, Arguments, Block, IvarsIn, IvarsOut},
  {Dict3, Node}.

% Is the given name an internal variable that will be generated by later passes?
internal_variable('__instance_variables') -> true;
internal_variable(_Name) -> false.