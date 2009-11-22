-module(reia_syntax).
-export([mapfold_subtrees/3]).

% Iterate all subtrees of a given tree, passing the given state along with it
mapfold_subtrees(Fun, State, Tree) when is_list(Tree) ->
  lists:mapfoldl(Fun, State, Tree);
mapfold_subtrees(Fun, State, Tree) when is_tuple(Tree) ->
  [Type, Line | Elements] = tuple_to_list(Tree),
  {Elements2, State2} = lists:mapfoldl(Fun, State, Elements),
  {list_to_tuple([Type, Line | Elements2]), State2};
mapfold_subtrees(_Fun, State, Tree) ->
  {Tree, State}.