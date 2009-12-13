-module('Tuple').
-export([call/4]).

call(Tuple, '[]', {Index}, _Block) ->
  if
    Index < 0 -> element(tuple_size(Tuple) + Index + 1, Tuple);
    true      -> element(Index + 1, Tuple)
  end;

call(Tuple, to_s, _Args, _Block) ->
  List = tuple_to_list(Tuple),
  lists:flatten(["(", string:join([convert(Elem) || Elem <- List], ","), ")"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).