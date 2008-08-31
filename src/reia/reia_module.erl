%
% reia_module: Magical Smerl-powered runtime module builder
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_module).
-export([build/1]).
-define(COMPILE_OPTIONS, [report_errors, report_warnings, return_errors]).

build({module, _Line, Name, Functions}) ->
  Module = lists:foldl(
    fun(Func, Mod) -> 
      {ok, Mod2} = smerl:add_func(Mod, Func),
      Mod2
    end, 
    new_module(Name), 
    Functions
  ),
  smerl:compile(Module, compile_options());
build(_) ->
  {error, "invalid module"}.

new_module(Name) ->
  smerl:set_export_all(smerl:new(Name), true).
  
compile_options() ->
  compile_options(?COMPILE_OPTIONS).
  
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