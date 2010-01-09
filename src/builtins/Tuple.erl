%
% Tuple: Methods of the Tuple builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Tuple').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Tuple, '[]', {Index}, _Block) ->
  Length = tuple_size(Tuple),
  Index2 = if
    Index < 0 -> Length + Index + 1;
    true      -> Index + 1
  end,
  if
    Index2 < 1      -> nil;
    Index2 > Length -> nil;
    true            -> element(Index2, Tuple)
  end;
  
call(Tuple, '[]=', {Index, Value}, _Block) ->
  if
    Index < 0 -> setelement(tuple_size(Tuple) + Index + 1, Tuple, Value);
    true      -> setelement(Index + 1, Tuple, Value)
  end;

call(Tuple, size, _Args, _Block) ->
  tuple_size(Tuple);

call(Tuple, to_list, _Args, _Block) ->
  tuple_to_list(Tuple);
    
call(Tuple, to_s, _Args, _Block) ->
  stringify(Tuple, to_s);
  
call(Tuple, inspect, _Args, _Block) ->
  stringify(Tuple, inspect);
  
call(Tuple, reverse, _Args, _Block) ->
  list_to_tuple(lists:reverse(tuple_to_list(Tuple))).

stringify(Tuple, Method) ->
  List = tuple_to_list(Tuple),
  List2 = case List of
    [Expr] -> 
      lists:flatten(["(", convert(Expr, Method), ",)"]);
    _      ->
      lists:flatten(["(", string:join([convert(Elem, Method) || Elem <- List], ","), ")"])
  end,
  ?invoke(List2, to_string, {}, nil).

convert(Value, Method) ->
  ?invoke(?invoke(Value, Method, [], nil), to_list, {}, nil).