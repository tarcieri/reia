-module(reia_tree).
-export([transform/3]).

transform(decimal, Node, Idx) ->
  {integer, line(Idx), list_to_integer(Node)};
transform(additive, [Val1, _, Val2], Idx) ->
  {op, line(Idx), '+', Val1, Val2};
transform(Type, Node, Idx) ->
  io:format("Type: ~w, Node: ~w, Line: ~w~n", [Type, Node, line(Idx)]),
  Node.

line({{line, N}, _}) -> N.