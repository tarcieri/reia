-module(reia_parse).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(add_expr) ->
  peg:seq([fun integer/2, peg:string("+"), fun integer/2]);

rule(integer) ->
  peg:one_or_more(peg:charclass("[0-9]")).

transform(Type, Node, Idx) -> reia_tree:transform(Type, Node, Idx).