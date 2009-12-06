-module(reia_bindings).
-export([transform/1, transform/2]).
-include("reia_nodes.hrl").
-include("reia_bindings.hrl").

% Internal state of the bindings annotator
-record(state, {scope=normal, bindings=dict:new()}).

%
% Public functions
%

% Annotate the given node with its current bindings
transform(Exprs) -> transform(Exprs, normal).

% Annotate the given node, assuming the given scope
transform(Exprs, Scope) ->
  {BAExprs, _State} = lists:mapfoldl(
    fun transform_node/2,
    #state{scope=Scope},
    Exprs
  ),
  {ok, BAExprs}.

%
% Node transformations
%

% Module declarations create a new scope
transform_node(#module{functions=Functions} = Node, State) ->
  {Functions2, _State2} = [
    reia_syntax:mapfold_subtrees(
      fun transform_node/2,
      #state{scope=module},
      [Function]
    ) || Function <- Functions
  ],
  output(Node#module{functions=Functions2}, State);

% Function declarations create a new scope
transform_node(#function{line=Line, name=Name, arguments=Args, block=Block, body=Exprs}, State) ->
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

  Node = #function{line=Line, name=Name, arguments=Args2, block=Block2, body=Exprs2},
  output(Node, State);

% Walk the LHS of a match expression in match scope
transform_node(#match{} = Node, State) ->
  {Right, State2} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State,
    [Node#match.right]
  ),

  {Left, State3} = reia_syntax:mapfold_subtrees(
    fun transform_node/2,
    State2#state{scope=match},
    [Node#match.left]
  ),

  Node2  = Node#match{left=Left, right=Right},
  State4 = State3#state{scope=State#state.scope},
  output(Node2, State4);

% Arguments should initialize new entries in the bindings dict
transform_node(#identifier{name=Name} = Node, #state{scope=argument, bindings=Bindings} = State) ->
  output(Node, State#state{bindings=dict:store(Name, 0, Bindings)});

% Variables are (re)bound while in match scope
transform_node(#identifier{name=Name} = Node, #state{scope=match, bindings=Bindings} = State) ->
  NewBindings = case dict:find(Name, Bindings) of
    {ok, Version} -> dict:store(Name, Version + 1, Bindings);
    error         -> dict:store(Name, 0, Bindings)
  end,
  output(Node, State#state{bindings=NewBindings});

transform_node(Node, State) when is_integer(Node) or is_float(Node) or is_atom(Node) ->
  {Node, State};

transform_node(Node, State) ->
%  io:format("Unrecognized node: ~p in state ~p~n", [Node, State]),
  {Node2, State2} = reia_syntax:mapfold_subtrees(fun transform_node/2, State, Node),
  output(Node2, State2).

output(Node, State) ->
  {#bindings{node=Node, entries=State#state.bindings}, State}.