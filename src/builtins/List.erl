%
% List: Methods of the List builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('List').
-export([call/4]).

call(List, '[]', {Index}, _Block) ->
  if
    Index < 0 -> lists:nth(length(List) + Index + 1, List);
    true      -> lists:nth(Index + 1, List)
  end;
  
call(List, '[]=', {Index, Value}, _Block) ->
  if
    Index < 0 -> replace(List, length(List) + Index, Value);
    true      -> replace(List, Index, Value)
  end;
  
call(List, size, _Args, _Block) ->
  length(List);
  
call(List, to_s, _Args, _Block) ->
  lists:flatten(["[", string:join([convert(Elem) || Elem <- List], ","), "]"]);

call(List, reverse, _Args, _Block) ->
  lists:reverse(List).

replace(List, Index, Value) ->
  replace(List, 0, Index, Value).

replace([], _, _, _) ->
  throw('bad argument');
replace([_|Tail], Index, Index, Value) ->
  [Value|Tail];
replace([Head|Tail], N, Index, Value) ->
  [Head|replace(Tail, N + 1, Index, Value)].

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).