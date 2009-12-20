%
% List: Methods of the List builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('List').
-export([call/4]).
-include("../core/reia_types.hrl").

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
  lists:reverse(List);
  
call(List, join, {}, Block) ->
  call(List, join, {#reia_string{members=[]}}, Block);
call(List, join, {#reia_string{members=Separator}}, _Block) ->
  Members = case List of
    [] -> #reia_string{members=[]};
    _  -> join_list([], List, Separator)
  end,
  #reia_string{members=Members}.

replace(List, Index, Value) ->
  replace(List, 0, Index, Value).

replace([], _, _, _) ->
  throw('bad argument');
replace([_|Tail], Index, Index, Value) ->
  [Value|Tail];
replace([Head|Tail], N, Index, Value) ->
  [Head|replace(Tail, N + 1, Index, Value)].

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).

join_list(Result, [Elem], _) ->
  lists:reverse([element_to_string(Elem)|Result]);
join_list(Result, [Elem|Rest], Separator) ->
  join_list([Separator,element_to_string(Elem)|Result], Rest, Separator).
  
element_to_string(Elem) ->
  reia_dispatch:call(Elem, to_s, {}, nil).