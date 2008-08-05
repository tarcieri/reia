%
% reia_module: Magical Smerl-powered runtime module builder
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_module).
-export([build/1]).

build({module, _Line, Name, Functions}) ->
  {ok, Module} = lists:foldl(
    fun(Func, Mod) -> smerl:add_func(Mod, Func) end, 
    smerl:set_export_all(smerl:new(Name), true), 
    Functions
  ),
  smerl:compile(Module);
build(_) ->
  {error, "invalid module"}.