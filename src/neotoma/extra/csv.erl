-module(csv).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(rows) ->
  peg:choose([peg:seq([peg:label('head', fun row/2), peg:label('tail', peg:zero_or_more(peg:seq([fun crlf/2, fun row/2])))]), peg:string("")]);

rule(row) ->
  peg:choose([peg:seq([peg:label('head', fun field/2), peg:label('tail', peg:zero_or_more(peg:seq([fun field_sep/2, fun field/2])))]), peg:string("")]);

rule(field) ->
  peg:choose([fun quoted_field/2, peg:zero_or_more(peg:seq([peg:not_(peg:choose([fun field_sep/2, fun crlf/2])), peg:anything()]))]);

rule(quoted_field) ->
  peg:seq([peg:string("\""), peg:label('string', peg:zero_or_more(peg:choose([peg:string("\"\""), peg:seq([peg:not_(peg:string("\"")), peg:anything()])]))), peg:string("\"")]);

rule(field_sep) ->
  peg:string(",");

rule(crlf) ->
  peg:choose([peg:string("\r\n"), peg:string("\n")]).

transform(Symbol,Node) -> csv_gen:transform(Symbol, Node).