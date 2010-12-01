-module(test_combinators).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

% Test the parser-combinators in the 'neotoma_peg' module
-define(STARTINDEX, {{line,1},{column,1}}).
eof_test_() ->
    [
     ?_assertEqual({fail,{expected,eof,?STARTINDEX}}, (neotoma_peg:p_eof())("abc",?STARTINDEX)),
     ?_assertEqual({eof, [], ?STARTINDEX}, (neotoma_peg:p_eof())("",?STARTINDEX))
    ].

optional_test_() ->
    [
     ?_assertEqual({[], "xyz",?STARTINDEX}, (neotoma_peg:p_optional(neotoma_peg:p_string("abc")))("xyz",?STARTINDEX)),
     ?_assertEqual({"abc", "xyz",{{line,1},{column,4}}}, (neotoma_peg:p_optional(neotoma_peg:p_string("abc")))("abcxyz",?STARTINDEX))
    ].

not_test_() ->
    [
     ?_assertEqual({[], "xyzabc",?STARTINDEX}, (neotoma_peg:p_not(neotoma_peg:p_string("abc")))("xyzabc",?STARTINDEX)),
     ?_assertEqual({fail,{expected, {no_match, "abc"}, ?STARTINDEX}}, (neotoma_peg:p_not(neotoma_peg:p_string("abc")))("abcxyz",?STARTINDEX))
    ].

assert_test_() ->
    [
     ?_assertEqual({fail,{expected, {string, "abc"}, ?STARTINDEX}}, (neotoma_peg:p_assert(neotoma_peg:p_string("abc")))("xyzabc",?STARTINDEX)),
     ?_assertEqual({[], "abcxyz",?STARTINDEX}, (neotoma_peg:p_assert(neotoma_peg:p_string("abc")))("abcxyz",?STARTINDEX))
    ].

seq_test_() ->
    [
     ?_assertEqual({["abc","def"], "xyz",{{line,1},{column,7}}}, (neotoma_peg:p_seq([neotoma_peg:p_string("abc"), neotoma_peg:p_string("def")]))("abcdefxyz",?STARTINDEX)),
     ?_assertEqual({fail,{expected, {string, "def"}, {{line,1},{column,4}}}}, (neotoma_peg:p_seq([neotoma_peg:p_string("abc"), neotoma_peg:p_string("def")]))("abcxyz",?STARTINDEX))
    ].

choose_test_() ->
    [
     ?_assertEqual({"abc", "xyz", {{line,1},{column,4}}}, (neotoma_peg:p_choose([neotoma_peg:p_string("abc"), neotoma_peg:p_string("def")]))("abcxyz",?STARTINDEX)),
     ?_assertEqual({"def", "xyz", {{line,1},{column,4}}}, (neotoma_peg:p_choose([neotoma_peg:p_string("abc"), neotoma_peg:p_string("def")]))("defxyz",?STARTINDEX)),
     ?_assertEqual({"xyz", "xyz", {{line,1},{column,4}}}, (neotoma_peg:p_choose([neotoma_peg:p_string("abc"), neotoma_peg:p_string("def"), neotoma_peg:p_string("xyz")]))("xyzxyz",?STARTINDEX)),
     ?_assertEqual({fail,{expected,{string,"abc"},?STARTINDEX}}, (neotoma_peg:p_choose([neotoma_peg:p_string("abc"),neotoma_peg:p_string("def")]))("xyz", ?STARTINDEX))
    ].

zero_or_more_test_() ->
    [
     ?_assertEqual({[], [], ?STARTINDEX}, (neotoma_peg:p_zero_or_more(neotoma_peg:p_string("abc")))("",?STARTINDEX)),
     ?_assertEqual({[], "def",?STARTINDEX}, (neotoma_peg:p_zero_or_more(neotoma_peg:p_string("abc")))("def",?STARTINDEX)),
     ?_assertEqual({["abc"], "def",{{line,1},{column,4}}}, (neotoma_peg:p_zero_or_more(neotoma_peg:p_string("abc")))("abcdef",?STARTINDEX)),
     ?_assertEqual({["abc", "abc"], "def",{{line,1},{column,7}}}, (neotoma_peg:p_zero_or_more(neotoma_peg:p_string("abc")))("abcabcdef",?STARTINDEX))
    ].

one_or_more_test_() ->
    [
     ?_assertEqual({fail,{expected, {at_least_one, {string, "abc"}}, ?STARTINDEX}}, (neotoma_peg:p_one_or_more(neotoma_peg:p_string("abc")))("def",?STARTINDEX)),
     ?_assertEqual({["abc"], "def",{{line,1},{column,4}}}, (neotoma_peg:p_one_or_more(neotoma_peg:p_string("abc")))("abcdef",?STARTINDEX)),
     ?_assertEqual({["abc","abc"], "def",{{line,1},{column,7}}}, (neotoma_peg:p_one_or_more(neotoma_peg:p_string("abc")))("abcabcdef",?STARTINDEX))
    ].

label_test_() ->
    [
     ?_assertEqual({fail,{expected, {string, "!"}, ?STARTINDEX}}, (neotoma_peg:p_label(bang, neotoma_peg:p_string("!")))("?",?STARTINDEX)),
     ?_assertEqual({{bang, "!"}, "",{{line,1},{column,2}}}, (neotoma_peg:p_label(bang, neotoma_peg:p_string("!")))("!",?STARTINDEX))
    ].

string_test_() ->
    [
     ?_assertEqual({"abc", "def",{{line,1},{column,4}}}, (neotoma_peg:p_string("abc"))("abcdef",?STARTINDEX)),
     ?_assertEqual({fail,{expected, {string, "abc"}, ?STARTINDEX}}, (neotoma_peg:p_string("abc"))("defabc",?STARTINDEX))
    ].

anything_test_() ->
    [
     ?_assertEqual({$a,"bcde",{{line,1},{column,2}}}, (neotoma_peg:p_anything())("abcde",?STARTINDEX)),
     ?_assertEqual({fail,{expected, any_character, ?STARTINDEX}}, (neotoma_peg:p_anything())("",?STARTINDEX))
    ].

charclass_test_() ->
    [
     ?_assertEqual({$+,"----",{{line,1},{column,2}}}, (neotoma_peg:p_charclass("[+]"))("+----",?STARTINDEX)),
     ?_assertEqual({fail,{expected, {character_class, "[+]"}, ?STARTINDEX}}, (neotoma_peg:p_charclass("[+]"))("----",?STARTINDEX))
    ].

line_test() ->
    ?assertEqual(1, neotoma_peg:line({{line,1},{column,2}})).

column_test() ->
    ?assertEqual(2, neotoma_peg:column({{line,1},{column,2}})).

