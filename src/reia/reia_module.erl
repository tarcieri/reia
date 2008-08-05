%
% reia_module: Magical Smerl-powered runtime module builder
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_module).
-export([build/1]).

build(Module) ->
  io:format("w00t, module: ~p~n", [Module]).