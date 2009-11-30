-module('Map').
-export([call/4]).

call(Map, to_s, _Args, _Block) ->
  List = [
    io_lib:format("~s=>~s", [convert(Key), convert(Value)]) ||
    {Key, Value} <- dict:to_list(Map)
  ],
  lists:flatten(["{", string:join(List, ","), "}"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).