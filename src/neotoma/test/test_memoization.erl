-module(test_memoization).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

setup_memo_test() ->
    neotoma_peg:setup_memo(),
    ?assertNot(undefined == ets:info(get(parse_memo_table))),
    neotoma_peg:release_memo().

release_memo_test() ->
    neotoma_peg:setup_memo(),
    neotoma_peg:release_memo(),
    ?assertEqual(undefined, ets:info(get(parse_memo_table))).

step_memo_test() ->
    neotoma_peg:setup_memo(),
    Result = neotoma_peg:p("abcdefghi", {{line,1},{column,1}}, anything, neotoma_peg:p_anything()),
    ?assertEqual({$a, "bcdefghi", {{line,1},{column,2}}}, Result),
    Result2 = neotoma_peg:p("abcdefghi", {{line,1},{column,1}}, anything, fun(_) ->
                                                                                  throw(bork) end),
    ?assertEqual(Result, Result2),
    neotoma_peg:release_memo().

concurrent_memo_test() ->
    Me = self(),
    Him = spawn(fun() -> Me ! neotoma_peg:setup_memo(), receive _ -> ok after 10000 -> ok end end),
    MyTid = neotoma_peg:setup_memo(),
    receive
        Tid -> ?assertNot(Tid == MyTid),
               Him ! ok
    after 10000 -> ok
    end,
    neotoma_peg:release_memo().
