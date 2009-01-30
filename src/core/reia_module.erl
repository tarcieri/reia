%
% reia_module: Runtime module generation and loading
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_module).
-export([build/1, build/2]).
-define(COMPILE_OPTIONS, [report_errors, report_warnings, return_errors]).

build(Module) ->
  build(Module, []).
  
build({module, Line, Name, Functions}, Attributes) ->
  Module = [
    {attribute, Line, module, Name},
    {attribute, Line, compile, export_all}
  ] ++ [{attribute, Line, AttrName, Value} || {AttrName, Value} <- Attributes] ++ Functions,
  
  case compile:forms(Module, compile_options()) of
    {ok, Name, Bin} ->
      code:purge(Name),
      case code:load_binary(Name, atom_to_list(Name) ++ ".re", Bin) of
        {module, Name} ->
          {constant, Name};
        Error ->
          throw(Error)
      end;
    Error ->
      throw(Error)
  end;
build(_, _) ->
  throw({error, "invalid module"}).
  
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