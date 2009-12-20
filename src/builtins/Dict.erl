%
% Dict: Methods of the Dict builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Dict').
-export([call/4]).
-include("../core/reia_types.hrl").
-include("../core/reia_invoke.hrl").

call(Dict, '[]', {Key}, _Block) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    error       -> nil
  end;
  
call(Dict, '[]=', {Key, Value}, _Block) ->
  dict:store(Key, Value, Dict);

call(Dict, to_s, _Args, _Block) ->
  stringify(Dict, to_s);

call(Dict, inspect, _Args, _Block) ->
  stringify(Dict, inspect).
  
stringify(Dict, Method) ->
  List = [
    io_lib:format("~s=>~s", [convert(Key, Method), convert(Value, Method)]) ||
    {Key, Value} <- dict:to_list(Dict)
  ],
  Result = lists:flatten(["{", string:join(List, ","), "}"]),
  ?invoke(Result, to_string, {}, nil).

convert(Value, Method) ->
  ?invoke(?invoke(Value, Method, {}, nil), to_list, {}, nil).