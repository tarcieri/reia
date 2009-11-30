-module('List').
-export([call/4]).

call(List, to_s, _Args, _Block) ->
  lists:flatten(["[", string:join([convert(Elem) || Elem <- List], ","), "]"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).