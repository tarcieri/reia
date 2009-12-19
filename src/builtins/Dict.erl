%
% Dict: Methods of the Dict builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Dict').
-export([call/4]).

call(Dict, '[]', {Key}, _Block) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    error       -> nil
  end;
  
call(Dict, '[]=', {Key, Value}, _Block) ->
  dict:store(Key, Value, Dict);

call(Dict, to_s, _Args, _Block) ->
  List = [
    io_lib:format("~s=>~s", [convert(Key), convert(Value)]) ||
    {Key, Value} <- dict:to_list(Dict)
  ],
  lists:flatten(["{", string:join(List, ","), "}"]).

convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).