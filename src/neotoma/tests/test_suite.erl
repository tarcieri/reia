-module(test_suite).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, test_combinators},{module, test_memoization}].
