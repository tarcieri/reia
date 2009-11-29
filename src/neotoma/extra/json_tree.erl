-module(json_tree).
-export([transform/3]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(json_value, Node, _Idx) ->
  lists:nth(2, Node);
transform(object, Node, _Idx) when length(Node) =:= 3 ->
  {struct, []};
transform(object, Node, _Idx) ->
  Head = proplists:get_value(head, Node),
  Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
  {struct, [Head|Rest]};
transform(array, Node, _Idx) when length(Node) =:= 3 ->
  [];
transform(array, Node, _Idx) ->
  Head = proplists:get_value(head, Node),
  Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
  [Head|Rest];
transform(number, [Int, [],[]], _Idx) ->
  list_to_integer(lists:flatten([Int]));
transform(number, [Int, Frac, []], _Idx) ->
  list_to_float(lists:flatten([Int, Frac]));
transform(number, [Int, [], Exp], Idx) ->
  transform(number, [Int, ".0", Exp], Idx);
transform(number, Node, _Idx) ->
  list_to_float(lists:flatten(Node));
transform(string, Node, _Idx) ->
  lists:flatten(proplists:get_value(chars, Node));
transform(pair, Node, _Idx) ->
  {proplists:get_value(key, Node), proplists:get_value(value, Node)};
transform(true, _Node, _Idx) -> true;
transform(false, _Node, _Idx) -> false;
transform(null, _Node, _Idx) -> null;
transform(Symbol, Node, _Idx) when is_atom(Symbol) ->
  Node.
