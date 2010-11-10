%
% reia_syntax: Functions for manipulating Reia parse trees
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_syntax).
-export([mapfold_subtrees/3, map_subtrees/2]).

% Iterate all subtrees of a given tree, passing the given state along with it
mapfold_subtrees(Fun, State, Tree) when is_list(Tree) ->
  lists:mapfoldl(Fun, State, Tree);
mapfold_subtrees(Fun, State, Tree) when is_tuple(Tree) ->
  [Type, Line | Elements] = tuple_to_list(Tree),
  {Elements2, {_, State2}} = lists:mapfoldl(fun mapfold_element/2, {Fun, State}, Elements),
  {list_to_tuple([Type, Line | Elements2]), State2};
mapfold_subtrees(_Fun, State, Tree) ->
  {Tree, State}.

% Process an individual element, automatically walking sublists
mapfold_element(Elem, State) when is_list(Elem) ->
  lists:mapfoldl(fun mapfold_element/2, State, Elem);
mapfold_element(Elem, {Fun, State}) ->
  {Elem2, State2} = Fun(Elem, State),
  {Elem2, {Fun, State2}}.

% Iterate across all the subtrees fo a given tree
map_subtrees(Fun, Tree) ->
  {Result, _} = mapfold_subtrees(fun(Elem, _) -> {Fun(Elem), void} end, void, Tree),
  Result.