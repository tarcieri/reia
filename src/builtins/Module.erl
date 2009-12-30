%
% Module: Methods of the Module builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Module').
-export([call/4]).
-include("../core/reia_invoke.hrl").
-include("../core/reia_types.hrl").

call(Module, inspect, _Args, _Block) ->
  ?invoke(atom_to_list(Module#reia_module.name), to_string, {}, nil);
  
call(Module, to_s, Args, Block) ->
  call(Module, inspect, Args, Block).
  
