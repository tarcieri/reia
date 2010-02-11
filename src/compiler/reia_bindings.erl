%
% reia_bindings: Convert Reia parse trees into Binding Annotated Format
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bindings).
-export([transform/1, transform/2, revert/1]).
-include("reia_nodes.hrl").
-include("reia_bindings.hrl").

% Internal state of the bindings annotator
-record(state, {scope=normal, bindings=dict:new()}).

%
% Public functions
%

% Annotate the given node with its current bindings
transform(Exprs) -> transform(Exprs, normal).

% Annotate the given expressions, assuming the given scope
transform(Exprs, Scope) ->
  {BAExprs, _State} = lists:mapfoldl(
    fun transform_node/2,
    #state{scope=Scope},
    Exprs
  ),
  {ok, BAExprs}.

% Return these expressions back to their standard form
revert(BAExprs) ->
  reia_syntax:map_subtrees(fun revert_node/1, BAExprs).

%
% Node transformations
%

% Module declarations create a new scope
transform_node(#module{functions=Functions} = Node, State) ->
  Fun = fun(Function) ->
    {[Function2], _State2} = reia_syntax:mapfold_subtrees(
      fun transform_node/2,
      #state{scope=module},
      [Function]
    ),
    Function2
  end,
  output(Node#module{functions=lists:map(Fun, Functions)}, State);

% Function declarations create a new scope
transform_node(#function{line=Line, name=Name, args=Args, block=Block, body=Exprs}, State) ->
  {Args2, #state{bindings=Bindings}} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    #state{scope=argument},
    Args
  ),

  {[Block2], #state{bindings=Bindings2}} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    #state{scope=argument, bindings=Bindings},
    [Block]
  ),

  {Exprs2, _State} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    #state{scope=normal, bindings=Bindings2},
    Exprs
  ),

  Node = #function{line=Line, name=Name, args=Args2, block=Block2, body=Exprs2},
  output(Node, State);
  
% Calls may actually invoke a lambda/funref bound in local scope
transform_node(#local_call{line=Line, name=Name, args=Args, block=Block} = Call, #state{bindings=Bindings} = State) ->
  {Args2, _}       = reia_syntax:mapfold_subtrees(fun transform_node/2, State, Args),
  {[Block2], _}    = reia_syntax:mapfold_subtrees(fun transform_node/2, State, [Block]),
  
  Node = case dict:find(Name, Bindings) of
    {ok, _} ->
      Receiver = #var{line=Line, name=Name},
      {[Receiver2], _} = reia_syntax:mapfold_subtrees(fun transform_node/2, State, [Receiver]),
      #var_call{line=Line, receiver=Receiver2, args=Args2, block=Block2};
    error ->
      Call#local_call{args=Args2, block=Block2}
  end,
  output(Node, State);

% Walk the LHS of a match expression in match scope
transform_node(#match{} = Node, State) ->
  {[Right], State2} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State,
    [Node#match.right]
  ),

  {[Left], State3} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State2#state{scope=match},
    [Node#match.left]
  ),

  Node2  = Node#match{left=Left, right=Right},
  State4 = State3#state{scope=State#state.scope},
  output(Node2, State4);

% Lambdas close over the outer scope, bind arguments, but don't affect the outer scope
transform_node(#lambda{line=Line, args=Args, body=Body}, State) ->
  {Args2, #state{bindings=Bindings}} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{scope=argument},
    Args
  ),
  
  {Body2, _} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{bindings=Bindings, scope=normal},
    Body
  ),
  
  output(#lambda{line=Line, args=Args2, body=Body2}, State);

% Case statements bind variables in clauses
transform_node(#'case'{line=Line, expr=Expr, clauses=Clauses}, State) ->
  {[Expr2], State2} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State,
    [Expr]
  ),
  {Clauses2, State3} = process_clauses(Clauses, State2),
  output(#'case'{line=Line, expr=Expr2, clauses=Clauses2}, State3);

% Case clauses match against patterns
transform_node(#clause{line=Line, patterns=Patterns, exprs=Body}, #state{scope=Scope} = State) ->
  {Patterns2, State2} = lists:mapfoldl(fun(Pattern, St) ->
    {[Pattern2], St2} = reia_syntax:mapfold_subtrees(
      fun transform_node/2,
      St#state{scope=match},
      [Pattern]
    ),
    {Pattern2, St2}
  end, State, Patterns),

  {Body2, State3} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State2#state{scope=normal},
    Body
  ),

  output(#clause{line=Line, patterns=Patterns2, exprs=Body2}, State3#state{scope=Scope});
      
% Catch clauses match against patterns
transform_node(#'catch'{line=Line, pattern=Pattern, body=Body}, #state{scope=Scope} = State) ->
  {[Pattern2], State2} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{scope=match},
    [Pattern]
  ),
  
  {Body2, State3} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State2#state{scope=normal},
    Body
  ),
  
  output(#'catch'{line=Line, pattern=Pattern2, body=Body2}, State3#state{scope=Scope});

% List comprehensions can access the outer scope but have a scope of their own
transform_node(#lc{line=Line, expr=Expr, generators=Generators}, State) ->
  {Generators2, #state{bindings=Bindings}} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{scope=normal},
    Generators
  ),

  {[Expr2], _} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{scope=normal, bindings=Bindings},
    [Expr]
  ),
  
  output(#lc{line=Line, expr=Expr2, generators=Generators2}, State);

% Generate expressions match a pattern
transform_node(#generate{line=Line, pattern=Pattern, source=Source}, State) ->
  {[Pattern2], State2} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State#state{scope=match},
    [Pattern]
  ),

  {[Source2], State3} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State2#state{scope=normal},
    [Source]
  ),
  
  output(#generate{line=Line, pattern=Pattern2, source=Source2}, State3);

% Arguments bind variables
transform_node(#var{name=Name} = Node, #state{scope=argument, bindings=Bindings} = State) ->
  output(Node, State#state{bindings=bind_variable(Name, Bindings)});
      
% Variables are (re)bound while in match scope
transform_node(#var{name=Name} = Node, #state{scope=match, bindings=Bindings} = State) ->
  output(Node, State#state{bindings=bind_variable(Name, Bindings)});

transform_node(Node, State) when is_integer(Node) or is_float(Node) or is_atom(Node) ->
  {Node, State};

transform_node(Node, State) ->
  {Node2, State2} = reia_syntax:mapfold_subtrees(fun transform_node/2, State, Node),
  output(Node2, State2).

output(Node, State) ->
  {#bindings{node=Node, entries=State#state.bindings}, State}.

revert_node(#bindings{node=Node}) ->
  reia_syntax:map_subtrees(fun revert_node/1, Node);
revert_node(Node) ->
  reia_syntax:map_subtrees(fun revert_node/1, Node).
    
process_clauses(Clauses, State) ->
  % Proceed with the normal SSA transformation on each clause
  ClauseBindings = lists:map(fun(Clause) ->
    {[Clause2], St2} = reia_syntax:mapfold_subtrees(
      fun transform_node/2,
      State,
      [Clause]
    ),
    {Clause2, St2}
  end, Clauses),

  % Extract a nested list of bound variables and SSA versions for each clause
  ClauseVars = [Bindings || {_, #state{bindings=Bindings}} <- ClauseBindings],

  % Build a dict of the highest version numbers of any variables referenced 
  % in any clause
  OutputVars = lists:foldl(fun update_binding/2, dict:new(), ClauseVars),
  
  % Store the output variables in the binding object  
  Clauses2 = [Clause#bindings{output=OutputVars} || {Clause, _} <- ClauseBindings],
  
  {Clauses2, State#state{bindings=OutputVars}}.
    
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
  
% Destructively rebind a given variable
bind_variable(Name, Bindings) ->
  case Name of
    '_' ->
      Bindings;
    _ ->
      case dict:find(Name, Bindings) of
        {ok, Version} -> dict:store(Name, Version + 1, Bindings);
        error         -> dict:store(Name, 0, Bindings)
      end
  end.