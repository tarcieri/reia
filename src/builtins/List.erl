-module('List').
-export([call/4]).

call(List, '[]', {Index}, _Block) ->
  if
    Index < 0 -> lists:nth(length(List) + Index + 1, List);
    true      -> lists:nth(Index + 1, List)
  end;
  
call(List, size, _Args, _Block) ->
  length(List);
  
call(List, to_s, _Args, _Block) ->
  lists:flatten(["[", string:join([convert(Elem) || Elem <- List], ","), "]"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).