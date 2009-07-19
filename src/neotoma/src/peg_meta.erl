-module(peg_meta).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(rules) ->
  peg:seq([peg:optional(fun space/2), fun declaration_sequence/2, peg:optional(fun space/2)]);

rule(declaration_sequence) ->
  peg:seq([peg:label('head', fun declaration/2), peg:label('tail', peg:zero_or_more(peg:seq([fun space/2, fun declaration/2])))]);

rule(declaration) ->
  peg:seq([fun nonterminal/2, fun space/2, peg:string("<-"), fun space/2, fun parsing_expression/2, peg:optional(fun space/2), peg:string(";")]);

rule(parsing_expression) ->
  peg:choose([fun choice/2, fun sequence/2, fun primary/2]);

rule(choice) ->
  peg:seq([peg:label('head', fun alternative/2), peg:label('tail', peg:one_or_more(peg:seq([fun space/2, peg:string("/"), fun space/2, fun alternative/2])))]);

rule(alternative) ->
  peg:choose([fun sequence/2, fun primary/2]);

rule(primary) ->
  peg:choose([peg:seq([fun prefix/2, fun atomic/2]), peg:seq([fun atomic/2, fun suffix/2]), fun atomic/2]);

rule(sequence) ->
  peg:seq([peg:label('head', fun labeled_sequence_primary/2), peg:label('tail', peg:one_or_more(peg:seq([fun space/2, fun labeled_sequence_primary/2])))]);

rule(labeled_sequence_primary) ->
  peg:seq([peg:optional(fun label/2), fun primary/2]);

rule(label) ->
  peg:seq([fun alpha_char/2, peg:zero_or_more(fun alphanumeric_char/2), peg:string(":")]);

rule(suffix) ->
  peg:choose([fun repetition_suffix/2, fun optional_suffix/2]);

rule(optional_suffix) ->
  peg:string("?");

rule(repetition_suffix) ->
  peg:choose([peg:string("+"), peg:string("*")]);

rule(prefix) ->
  peg:choose([peg:string("&"), peg:string("!")]);

rule(atomic) ->
  peg:choose([fun terminal/2, fun nonterminal/2, fun parenthesized_expression/2]);

rule(parenthesized_expression) ->
  peg:seq([peg:string("("), peg:optional(fun space/2), fun parsing_expression/2, peg:optional(fun space/2), peg:string(")")]);

rule(nonterminal) ->
  peg:seq([fun alpha_char/2, peg:zero_or_more(fun alphanumeric_char/2)]);

rule(terminal) ->
  peg:choose([fun quoted_string/2, fun character_class/2, fun anything_symbol/2]);

rule(quoted_string) ->
  peg:choose([fun single_quoted_string/2, fun double_quoted_string/2]);

rule(double_quoted_string) ->
  peg:seq([peg:string("\""), peg:label('string', peg:zero_or_more(peg:seq([peg:not_(peg:string("\"")), peg:choose([peg:string("\\\\"), peg:string("\\\""), peg:anything()])]))), peg:string("\"")]);

rule(single_quoted_string) ->
  peg:seq([peg:string("'"), peg:label('string', peg:zero_or_more(peg:seq([peg:not_(peg:string("'")), peg:choose([peg:string("\\\\"), peg:string("\\'"), peg:anything()])]))), peg:string("'")]);

rule(character_class) ->
  peg:seq([peg:string("["), peg:label('characters', peg:one_or_more(peg:seq([peg:not_(peg:string("]")), peg:choose([peg:seq([peg:string("\\\\"), peg:anything()]), peg:seq([peg:not_(peg:string("\\\\")), peg:anything()])])]))), peg:string("]")]);

rule(anything_symbol) ->
  peg:string(".");

rule(alpha_char) ->
  peg:charclass("[a-z_]");

rule(alphanumeric_char) ->
  peg:choose([fun alpha_char/2, peg:charclass("[0-9]")]);

rule(space) ->
  peg:one_or_more(peg:choose([fun white/2, fun comment_to_eol/2]));

rule(comment_to_eol) ->
  peg:seq([peg:string("%"), peg:zero_or_more(peg:seq([peg:not_(peg:string("\n")), peg:anything()]))]);

rule(white) ->
  peg:charclass("[ \t\n\r]").

transform(Symbol,Node) -> peg_meta_gen:transform(Symbol, Node).