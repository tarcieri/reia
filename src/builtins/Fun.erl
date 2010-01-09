%
% Fun: Methods of the Fun builtin
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Fun').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Fun, inspect, _Args, _Block) ->
  {name, Name}     = erlang:fun_info(Fun, name),
  {module, Module} = erlang:fun_info(Fun, module),
  List = lists:flatten(io_lib:format("#<Fun ~s:~s}>", [Name, Module])),
  ?invoke(List, to_string, {}, nil);
  
call(Fun, to_s, Args, Block) ->
  call(Fun, inspect, Args, Block).