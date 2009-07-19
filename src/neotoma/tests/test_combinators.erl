-module(test_combinators).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

% Test the parser-combinators in the 'peg' module
-define(STARTINDEX, {{line,1},{column,1}}).
eof_test_() ->
  [
   ?_assertEqual({fail,{expected,eof,?STARTINDEX}}, (peg:eof())("abc",?STARTINDEX)),
   ?_assertEqual({eof, [], ?STARTINDEX}, (peg:eof())("",?STARTINDEX))
  ].

optional_test_() ->
  [
   ?_assertEqual({[], "xyz",?STARTINDEX}, (peg:optional(peg:string("abc")))("xyz",?STARTINDEX)),
   ?_assertEqual({"abc", "xyz",{{line,1},{column,4}}}, (peg:optional(peg:string("abc")))("abcxyz",?STARTINDEX))
  ].

not_test_() ->
  [
   ?_assertEqual({[], "xyzabc",?STARTINDEX}, (peg:not_(peg:string("abc")))("xyzabc",?STARTINDEX)),
   ?_assertEqual({fail,{expected, {no_match, "abc"}, ?STARTINDEX}}, (peg:not_(peg:string("abc")))("abcxyz",?STARTINDEX))
  ].

assert_test_() ->
  [
   ?_assertEqual({fail,{expected, {string, "abc"}, ?STARTINDEX}}, (peg:assert(peg:string("abc")))("xyzabc",?STARTINDEX)),
   ?_assertEqual({[], "abcxyz",?STARTINDEX}, (peg:assert(peg:string("abc")))("abcxyz",?STARTINDEX))
  ].

seq_test_() ->
  [
   ?_assertEqual({["abc","def"], "xyz",{{line,1},{column,7}}}, (peg:seq([peg:string("abc"), peg:string("def")]))("abcdefxyz",?STARTINDEX)),
   ?_assertEqual({fail,{expected, {string, "def"}, {{line,1},{column,4}}}}, (peg:seq([peg:string("abc"), peg:string("def")]))("abcxyz",?STARTINDEX))
  ].

choose_test_() ->
  [
   ?_assertEqual({"abc", "xyz", {{line,1},{column,4}}}, (peg:choose([peg:string("abc"), peg:string("def")]))("abcxyz",?STARTINDEX)),
   ?_assertEqual({"def", "xyz", {{line,1},{column,4}}}, (peg:choose([peg:string("abc"), peg:string("def")]))("defxyz",?STARTINDEX)),
   ?_assertEqual({"xyz", "xyz", {{line,1},{column,4}}}, (peg:choose([peg:string("abc"), peg:string("def"), peg:string("xyz")]))("xyzxyz",?STARTINDEX)),
   ?_assertEqual({fail,{expected,{string,"abc"},?STARTINDEX}}, (peg:choose([peg:string("abc"),peg:string("def")]))("xyz", ?STARTINDEX))
  ].

zero_or_more_test_() ->
  [
   ?_assertEqual({[], [], ?STARTINDEX}, (peg:zero_or_more(peg:string("abc")))("",?STARTINDEX)),
   ?_assertEqual({[], "def",?STARTINDEX}, (peg:zero_or_more(peg:string("abc")))("def",?STARTINDEX)),
   ?_assertEqual({["abc"], "def",{{line,1},{column,4}}}, (peg:zero_or_more(peg:string("abc")))("abcdef",?STARTINDEX)),
   ?_assertEqual({["abc", "abc"], "def",{{line,1},{column,7}}}, (peg:zero_or_more(peg:string("abc")))("abcabcdef",?STARTINDEX))
  ].

one_or_more_test_() ->
  [
   ?_assertEqual({fail,{expected, {at_least_one, {string, "abc"}}, ?STARTINDEX}}, (peg:one_or_more(peg:string("abc")))("def",?STARTINDEX)),
   ?_assertEqual({["abc"], "def",{{line,1},{column,4}}}, (peg:one_or_more(peg:string("abc")))("abcdef",?STARTINDEX)),
   ?_assertEqual({["abc","abc"], "def",{{line,1},{column,7}}}, (peg:one_or_more(peg:string("abc")))("abcabcdef",?STARTINDEX))
  ].

label_test_() ->
  [
   ?_assertEqual({fail,{expected, {string, "!"}, ?STARTINDEX}}, (peg:label(bang, peg:string("!")))("?",?STARTINDEX)),
   ?_assertEqual({{bang, "!"}, "",{{line,1},{column,2}}}, (peg:label(bang, peg:string("!")))("!",?STARTINDEX))
  ].

string_test_() ->
  [
   ?_assertEqual({"abc", "def",{{line,1},{column,4}}}, (peg:string("abc"))("abcdef",?STARTINDEX)),
   ?_assertEqual({fail,{expected, {string, "abc"}, ?STARTINDEX}}, (peg:string("abc"))("defabc",?STARTINDEX))
  ].

anything_test_() ->
  [
   ?_assertEqual({$a,"bcde",{{line,1},{column,2}}}, (peg:anything())("abcde",?STARTINDEX)),
   ?_assertEqual({fail,{expected, any_character, ?STARTINDEX}}, (peg:anything())("",?STARTINDEX))
  ].

charclass_test_() ->
  [
   ?_assertEqual({$+,"----",{{line,1},{column,2}}}, (peg:charclass("[+]"))("+----",?STARTINDEX)),
   ?_assertEqual({fail,{expected, {character_class, "[+]"}, ?STARTINDEX}}, (peg:charclass("[+]"))("----",?STARTINDEX))
  ].
