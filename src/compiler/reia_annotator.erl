-module(reia_annotator).
-export([annotate/1]).
-include("reia_nodes.hrl").

% Internal state of the annotator
-record(state, {scope=normal, bindings=dict:new()}).

annotate(Node) ->
  {Node2, _State} = reia_syntax:mapfold_subtrees(
    fun annotate/2,
    #state{},
    Node
  ),
  Node2.

% Walk the LHS of a match expression in match scope
annotate(#match{} = Node, State) ->
  {Right, State2} = reia_syntax:mapfold_subtrees(
    fun annotate/2,
    State,
    Node#match.right
  ),

  {Left, State3} = reia_syntax:mapfold_subtrees(
    fun annotate/2,
    State2#state{scope=match},
    Node#match.left
  ),

  Node2  = Node#match{left=Left, right=Right},
  State4 = State3#state{scope=State#state.scope},
  {Node2, State4};

% Variables are (re)bound while in match scope
annotate(#identifier{name=Name} = Node, #state{scope=match} = State) ->
  NewBindings = case dict:find(Name, State#state.bindings) of
    {ok, Version} -> dict:store(Name, Version + 1);
    error         -> dict:store(Name, 0)
  end,
  {Node, State#state{bindings=NewBindings}};

annotate(Node, State) ->
  io:format("annotating: ~p~n", [Node]),
  {Node, State}.
