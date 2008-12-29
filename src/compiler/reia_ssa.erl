%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, ast/2, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ast(Ast, []).
  
ast(Ast, Variables) ->
  Dict = dict:from_list([{Variable, 0} || Variable <- Variables]),
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, {normal, Dict}, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.

% Module declarations create a new scope
transform(State, {module, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, {normal, dict:new()}, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {module, Line, Name, Expressions2}};
  
% Class declarations create a new scope
transform(State, {class, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, {normal, dict:new()}, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {class, Line, Name, Expressions2}};
  
% Function declarations create a new scope
transform(State, {function, Line, Name, Arguments, Expressions}) ->
  % Create a new scope with dict:new()
  {ok, {_, Dict}, Arguments2} = reia_visitor:transform(Arguments, {argument, dict:new()}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict}, fun transform/2),
    
  % Return to the original scope
  {stop, State, {function, Line, Name, Arguments2, Expressions2}};

% Lambdas close over the outer scope, bind arguments, but don't affect the outer scope
transform({_, Dict} = State, {lambda, Line, Arguments, Expressions}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, {argument, Dict}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict2}, fun transform/2),
  {stop, State, {lambda, Line, Arguments2, Expressions2}};
  
% Function names are identifiers and should remain undisturbed unless they are bound variables
transform({Mode, Dict} = State, {funcall, Line, {identifier, _Line, Name} = Identifier, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Identifier2 = case dict:find(Name, Dict) of
    {ok, Version} ->
      {var, Line, ssa_name(Name, Version)};
    error ->
      Identifier
  end,  
  Node = {funcall, Line, Identifier2, Arguments2},
  {stop, {Mode, Dict2}, Node};

transform({Mode, _Dict} = State, {funcall, Line, Receiver, Name, Arguments}) ->
  {ok, {_, Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, {_, Dict3}, Arguments2} = reia_visitor:transform(Arguments, {Mode, Dict2}, fun transform/2),
  Node = {funcall, Line, Receiver2, Name, Arguments2},
  {stop, {Mode, Dict3}, Node};

transform({Mode, _Dict} = State, {funcall, Line, Receiver, Name, Arguments, Block}) ->
  {ok, {_, Dict2}, Receiver2}  = reia_visitor:transform(Receiver,  State, fun transform/2), 
  {ok, {_, Dict3}, Arguments2} = reia_visitor:transform(Arguments, {Mode, Dict2}, fun transform/2),
  {ok, {_, Dict4}, Block2} = reia_visitor:transform(Block, {Mode, Dict3}, fun transform/2),
  Node = {funcall, Line, Receiver2, Name, Arguments2, Block2},
  {stop, {Mode, Dict4}, Node};
  
% Input node:  {method_call, Line, Name, Arguments}
% Output node: {method_call, Line, Name, Arguments, IvarsIn, IvarsOut}
transform({Mode, Dict} = State, {method_call, Line, {identifier, _Line, Name} = Identifier, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  {Dict3, Node} = case dict:find(Name, Dict) of
    {ok, Version} ->
      N = {funcall, Line, {var, Line, ssa_name(Name, Version)}, Arguments2},
      {Dict2, N};
    error ->
      build_method_call(Line, Identifier, Arguments2, Mode, Dict2)
  end,
  {stop, {Mode, Dict3}, Node};

transform({Mode, _Dict} = State, {erl_funcall, Line, Module, Function, Arguments}) ->
  {ok, {_, Dict2}, Arguments2} = reia_visitor:transform(Arguments, State, fun transform/2),
  Node = {erl_funcall, Line, Module, Function, Arguments2},
  {stop, {Mode, Dict2}, Node};
  
% Arguments should initialize new entries in the SSA dict
transform({argument, Dict}, {identifier, Line, Name}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      throw({error, {Line, lists:flatten(io_lib:format("argument already bound: '~s'", [Name]))}});
    error ->
      Dict2 = dict:store(Name, 0, Dict),
      Node = {identifier, Line, ssa_name(Name, 0)},
      {stop, {argument, Dict2}, Node}
  end;

% Match expressions mutate variables on the LHS
transform({Mode, Dict}, {match, Line, In1, In2}) ->
  {ok, {_, Dict2}, Out2} = reia_visitor:transform(In2, {Mode, Dict}, fun transform/2),
  {ok, {_, Dict3}, Out1} = reia_visitor:transform(In1, {match, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {match, Line, Out1, Out2}};
  
% Case expressions bind variables in clauses
transform({Mode, Dict}, {'case', Line, Expression, Clauses}) ->
  {ok, {_, Dict2}, Expression2} = reia_visitor:transform(Expression, {Mode, Dict}, fun transform/2),
  {Dict3, Clauses2} = process_clauses('case', Dict2, Clauses),
  {stop, {Mode, Dict3}, {'case', Line, Expression2, Clauses2}};

% Case clauses match against patterns
transform({'case', Dict}, {clause, Line, Pattern, Expressions}) ->
  {ok, {_, Dict2}, Pattern2} = reia_visitor:transform(Pattern, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict2}, fun transform/2),
  {stop, {'case', Dict3}, {clause, Line, Pattern2, Expressions2}};
    
% Catch clauses match against patterns
transform({Mode, Dict}, {'catch', Line, Pattern, Expressions}) ->
  {ok, {_, Dict2}, Pattern2} = reia_visitor:transform(Pattern, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {'catch', Line, Pattern2, Expressions2}};
    
% List comprehensions can access the outer scope but have a scope of their own
transform({_, Dict} = State, {'lc', Line, Transform, Expressions}) ->
  {ok, {_, Dict2}, Expressions2} = reia_visitor:transform(Expressions, {normal, Dict}, fun transform/2),
  {ok, _, Transform2} = reia_visitor:transform(Transform, {normal, Dict2}, fun transform/2),
  {stop, State, {'lc', Line, Transform2, Expressions2}};

% Generate expressions match a pattern
transform({Mode, Dict}, {'generate', Line, Pattern, List}) ->
  {ok, {_, Dict2}, Pattern2} = reia_visitor:transform(Pattern, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, List2} = reia_visitor:transform(List, {Mode, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {'generate', Line, Pattern2, List2}};

% Normally identifiers are mapped to their latest version
transform({normal, Dict} = State, {identifier, Line, Name} = Node) ->
  case dict:find(Name, Dict) of
    {ok, Version} ->
      Node2 = {identifier, Line, ssa_name(Name, Version)},
      {stop, State, Node2};
    error ->
      case internal_variable(Name) of
        true ->
          transform({normal, dict:store(Name, 0, Dict)}, Node);
        false ->
          throw({error, {Line, lists:flatten(io_lib:format("unbound variable: '~s'", [Name]))}})
      end
  end;
  
% On the LHS of match expressions, variables are assigned a new version
transform({match, Dict}, {identifier, Line, Name}) ->
  {Dict2, Version2} = case dict:find(Name, Dict) of
    {ok, Version} ->
      {dict:store(Name, Version + 1, Dict), Version + 1};
    error ->
      {dict:store(Name, 0, Dict), 0}
  end,
  Node = {identifier, Line, ssa_name(Name, Version2)},
  {stop, {match, Dict2}, Node};
  
% Vars are generated internally by earlier stages of the compiler and work 
% like identifiers, mapped to their latest version
transform({normal, _} = State, {var, Line, Name}) ->
  {stop, State2, {identifier, _, Name2}} = transform(State, {identifier, Line, Name}),
  {stop, State2, {var, Line, Name2}};
  
transform({match, _} = State, {var, Line, Name}) ->
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
    {ok, {_, Binding}, Clause2} = reia_visitor:transform(Clause, {Type, Vars}, fun transform/2),
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
  ReturnVar = {identifier, Line, list_to_atom("__clause_return_value_" ++ nonce_name())},
  Result2 = {match, Line, ReturnVar, Result},
  lists:reverse([ReturnVar, MatchExpr, Result2|Expressions2]).
  
% Generate a single-use variable name
nonce_name() ->
  lists:flatten([io_lib:format("~.16b",[N]) || <<N>> <= erlang:md5(term_to_binary(make_ref()))]).  
  
% Generate the SSA name for a given variable, which takes the form name_version
ssa_name(Name, Version) ->
  Name2 = lists:flatten(io_lib:format("_~s_~w", [Name, Version])),
  list_to_atom(Name2).
  
% Generate a method_call node to be processed by reia_class
build_method_call(Line, Identifier, Arguments, Mode, Dict) ->  
  {ok, {_, Dict2}, IvarsIn}  = reia_visitor:transform(
    {identifier, Line, '__instance_variables'}, 
    {Mode, Dict}, 
    fun transform/2
  ),
  {ok, {_, Dict3}, IvarsOut} = reia_visitor:transform(
    {identifier, Line, '__instance_variables'}, 
    {match, Dict2}, 
    fun transform/2
  ),
  Node = {method_call, Line, Identifier, Arguments, IvarsIn, IvarsOut},
  {Dict3, Node}.

% Is the given name an internal variable that will be generated by later passes?
internal_variable('__instance_variables') -> true;
internal_variable(_Name) -> false.