%
% Pid: Methods of the Pid builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Pid').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Pid, to_s, _Args, _Block) ->
  ?invoke(pid_to_list(Pid), to_string, {}, nil);
call(Pid, inspect, _Args, _Block) ->
  ?invoke(pid_to_list(Pid), to_string, {}, nil).