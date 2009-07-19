-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
  peg:setup_memo(?MODULE),
  ?assertNot(undefined == get(ets_table)).

release_memo_test() ->
  peg:setup_memo(?MODULE),
  Tid = get(ets_table),
  peg:release_memo(),
  ?assertEqual(undefined, get(ets_table)),
  ?assertEqual(undefined, ets:info(Tid)).

step_memo_test() ->
  peg:setup_memo(?MODULE),
  Result = peg:p("abcdefghi", {{line,1},{column,1}}, anything, peg:anything()),
  ?assertEqual({$a, "bcdefghi", {{line,1},{column,2}}}, Result),
  Result2 = peg:p("abcdefghi", {{line,1},{column,1}}, anything, fun(_) ->
                                             throw(bork) end),
  ?assertEqual(Result, Result2).
