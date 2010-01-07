%
% List: Methods of the List builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('List').
-export([call/4]).
-include("../core/reia_types.hrl").
-include("../core/reia_invoke.hrl").

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

call(List, to_string, _Args, _Block) ->
  #reia_string{elements=List};

call(List, to_s, _Args, _Block) ->
  stringify(List, to_s);
  
call(List, inspect, _Args, _Block) ->
  stringify(List, inspect);
  
call(List, reverse, _Args, _Block) ->
  lists:reverse(List);
  
call(List, join, {}, Block) ->
  call(List, join, {#reia_string{elements=[]}}, Block);
call(List, join, {#reia_string{elements=Separator}}, _Block) ->
  Elements = case List of
    [] -> [];
    _  -> join_list([], List, Separator)
  end,
  #reia_string{elements=Elements};
  
call(List, map, {}, Block) ->
  lists:map(Block, List);
  
call(List, flatten, {}, _Block) ->
  lists:flatten(List);
  
call(List, to_list, {}, _Block) ->
  List.

replace(List, Index, Value) ->
  replace(List, 0, Index, Value).

replace([], _, _, _) ->
  throw('bad argument');
replace([_|Tail], Index, Index, Value) ->
  [Value|Tail];
replace([Head|Tail], N, Index, Value) ->
  [Head|replace(Tail, N + 1, Index, Value)].

stringify(List, Method) ->
  Res = lists:flatten(["[", string:join([convert(Elem, Method) || Elem <- List], ","), "]"]),
  call(Res, to_string, {}, nil).

convert(Value, Method) -> 
  ?invoke(?invoke(Value, Method, {}, nil), to_list, {}, nil).

join_list(Result, [Elem], _) ->
  lists:reverse([convert(Elem, to_s)|Result]);
join_list(Result, [Elem|Rest], Separator) ->
  join_list([Separator, convert(Elem, to_s)|Result], Rest, Separator).