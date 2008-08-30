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
  smerl:compile(Module, compile_options());
build(_) ->
  {error, "invalid module"}.
  
compile_options() ->
  compile_options([report_errors, report_warnings, return_errors]).
  
compile_options(Defaults) ->
  case hipe_available() of
    true  -> [native|Defaults];
    false -> Defaults
  end.
  
hipe_available() ->
  case erlang:system_info(hipe_architecture) of
    undefined -> false;
    _         -> true
  end.