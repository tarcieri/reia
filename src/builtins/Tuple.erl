-module('Tuple').
-export([call/4]).

call(Tuple, '[]', {Index}, _Block) ->
  element(Index + 1, Tuple);

call(Tuple, to_s, _Args, _Block) ->
  List = tuple_to_list(Tuple),
  lists:flatten(["(", string:join([convert(Elem) || Elem <- List], ","), ")"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).