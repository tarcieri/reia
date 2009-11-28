-module(reia_bindings).
-export([transform/1]).
-include("reia_nodes.hrl").
-include("reia_bindings.hrl").

% Internal state of the bindings transform
-record(state, {scope=normal, bindings=dict:new()}).

transform(Node) ->
  {Node2, _State} = reia_syntax:mapfold_subtrees(
    fun transform/2,
    #state{},
    Node
  ),
  Node2.

% Walk the LHS of a match expression in match scope
transform(#match{} = Node, State) ->
  {Right, State2} = reia_syntax:mapfold_subtrees(
    fun transform/2,
    State,
    Node#match.right
  ),

  {Left, State3} = reia_syntax:mapfold_subtrees(
    fun transform/2,
    State2#state{scope=match},
    Node#match.left
  ),

  Node2  = Node#match{left=Left, right=Right},
  State4 = State3#state{scope=State#state.scope},
  output(Node2, State4);

% Variables are (re)bound while in match scope
transform(#identifier{name=Name} = Node, #state{scope=match} = State) ->
  NewBindings = case dict:find(Name, State#state.bindings) of
    {ok, Version} -> dict:store(Name, Version + 1);
    error         -> dict:store(Name, 0)
  end,
  output(Node, State#state{bindings=NewBindings});

transform(Node, State) ->
  output(Node, State).

output(Node, State) ->
  {#bindings{node=Node, entries=State#state.bindings}, State}.