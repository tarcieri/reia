-module(test_transform).
-author("Sean Cribbs <seancribbs@gmail.com>").
-include_lib("eunit/include/eunit.hrl").

transform_equivalence_test_() ->
  [
    compare("arithmetic", ["2+4*3"]) %,
   % compare("bf", ["+++--+++--"])
  ].

compare(Name, Inputs) ->
  EName = list_to_atom(Name),
  AName = list_to_atom(Name++"_xf"),
  fun() ->
      lists:foreach(fun(I) ->
                        Expected = apply(EName, parse, [I]),
                        Actual = apply(AName, parse, [I]),
                        ?assertEqual(Expected, Actual)
                    end, Inputs)
  end.
