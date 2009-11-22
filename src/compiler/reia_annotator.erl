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

% Match expressions mutate variables on the LHS
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

annotate(Node, State) ->
  io:format("annotating: ~p~n", [Node]),
  {Node, State}.
