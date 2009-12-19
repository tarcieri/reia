%
% Tuple: Methods of the Tuple builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Tuple').
-export([call/4]).

call(Tuple, '[]', {Index}, _Block) ->
  if
    Index < 0 -> element(tuple_size(Tuple) + Index + 1, Tuple);
    true      -> element(Index + 1, Tuple)
  end;
  
call(Tuple, '[]=', {Index, Value}, _Block) ->
  if
    Index < 0 -> setelement(tuple_size(Tuple) + Index + 1, Tuple, Value);
    true      -> setelement(Index + 1, Tuple, Value)
  end;

call(Tuple, size, _Args, _Block) ->
  tuple_size(Tuple);

call(Tuple, to_s, _Args, _Block) ->
  List = tuple_to_list(Tuple),
  case List of
    [Expr] -> 
      lists:flatten(["(", convert(Expr), ",)"]);
    _      ->
      lists:flatten(["(", string:join([convert(Elem) || Elem <- List], ","), ")"])
  end;
  
call(Tuple, reverse, _Args, _Block) ->
  list_to_tuple(lists:reverse(tuple_to_list(Tuple))).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).