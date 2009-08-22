-module(reia_tree).
-export([transform/3]).
-include("reia_nodes.hrl").

transform(decimal, Node, Idx) ->
  #integer{line = line(Idx), value = list_to_integer(Node)};
transform(additive, [Val1, _, Val2], Idx) ->
  #op{line = line(Idx), type = '+', val1 = Val1, val2 = Val2};
transform(Type, Node, Idx) ->
  io:format("Type: ~w, Node: ~w, Line: ~w~n", [Type, Node, line(Idx)]),
  Node.

line({{line, N}, _}) -> N.