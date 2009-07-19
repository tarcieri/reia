-module(json).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(json_value) ->
  peg:seq([peg:optional(fun space/2), peg:choose([fun object/2, fun array/2, fun string/2, fun number/2, fun true/2, fun false/2, fun null/2]), peg:optional(fun space/2)]);

rule(object) ->
  peg:choose([peg:seq([peg:string("{"), peg:optional(fun space/2), peg:label('head', fun pair/2), peg:label('tail', peg:zero_or_more(peg:seq([peg:optional(fun space/2), peg:string(","), peg:optional(fun space/2), fun pair/2]))), peg:optional(fun space/2), peg:string("}")]), peg:seq([peg:string("{"), peg:optional(fun space/2), peg:string("}")])]);

rule(pair) ->
  peg:seq([peg:optional(fun space/2), peg:label('key', fun string/2), peg:optional(fun space/2), peg:string(":"), peg:optional(fun space/2), peg:label('value', fun json_value/2), peg:optional(fun space/2)]);

rule(array) ->
  peg:choose([peg:seq([peg:string("["), peg:optional(fun space/2), peg:label('head', fun json_value/2), peg:label('tail', peg:zero_or_more(peg:seq([peg:optional(fun space/2), peg:string(","), peg:optional(fun space/2), fun json_value/2]))), peg:optional(fun space/2), peg:string("]")]), peg:seq([peg:string("["), peg:optional(fun space/2), peg:string("]")])]);

rule(string) ->
  peg:seq([peg:string("\""), peg:label('chars', peg:zero_or_more(peg:seq([peg:not_(peg:string("\"")), peg:choose([peg:string("\\\\"), peg:string("\\\""), peg:anything()])]))), peg:string("\"")]);

rule(number) ->
  peg:seq([fun int/2, peg:optional(fun frac/2), peg:optional(fun exp/2)]);

rule(int) ->
  peg:choose([peg:seq([peg:optional(peg:string("-")), peg:seq([fun non_zero_digit/2, peg:one_or_more(fun digit/2)])]), fun digit/2]);

rule(frac) ->
  peg:seq([peg:string("."), peg:one_or_more(fun digit/2)]);

rule(exp) ->
  peg:seq([fun e/2, peg:one_or_more(fun digit/2)]);

rule(e) ->
  peg:seq([peg:charclass("[eE]"), peg:optional(peg:choose([peg:string("+"), peg:string("-")]))]);

rule(non_zero_digit) ->
  peg:charclass("[1-9]");

rule(digit) ->
  peg:charclass("[0-9]");

rule(true) ->
  peg:string("true");

rule(false) ->
  peg:string("false");

rule(null) ->
  peg:string("null");

rule(space) ->
  peg:zero_or_more(peg:charclass("[ \t\n\s\r]")).

transform(Symbol,Node) -> json_tree:transform(Symbol, Node).