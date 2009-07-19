-module(arithmetic_xf).
-export([parse/1,file/1]).
-include("../../include/peg.hrl").

rule(additive) ->
  peg:choose([peg:seq([fun multitive/2,
                       peg:string("+"),
                       fun additive/2]),
              fun multitive/2]);

rule(multitive) ->
  peg:choose([peg:seq([fun primary/2,
                       peg:string("*"),
                       fun multitive/2]),
              fun primary/2]);

rule(primary) ->
  peg:choose([peg:seq([peg:string("("),
                       fun additive/2,
                       peg:string(")")]),
              fun decimal/2]);

rule(decimal) ->
  peg:charclass("[0-9]").

transform(decimal, Node) ->
  list_to_integer([Node]);
transform(primary, Node) when is_integer(Node) ->
  Node;
transform(primary, Node) when is_list(Node) ->
  lists:nth(2, Node);
transform(multitive, Node) when is_integer(Node) ->
  Node;
transform(multitive, Node) when is_list(Node) ->
  hd(Node) * lists:nth(3, Node);
transform(additive, Node) when is_integer(Node) ->
  Node;
transform(additive, Node) when is_list(Node) ->
  hd(Node) + lists:nth(3, Node).
