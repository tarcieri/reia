-module(reia_parse).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(additive) ->
  peg:choose([peg:seq([fun multitive/2, peg:string("+"), fun additive/2]), fun multitive/2]);

rule(multitive) ->
  peg:choose([peg:seq([fun primary/2, peg:string("*"), fun multitive/2]), fun primary/2]);

rule(primary) ->
  peg:choose([peg:seq([peg:string("("), fun additive/2, peg:string(")")]), fun decimal/2]);

rule(decimal) ->
  peg:charclass("[0-9]").

transform(_,Node) -> Node.