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
  Name = Module#reia_module.name,
  case code:ensure_loaded(Name) of
    {module, Name} ->
      ?invoke(atom_to_list(Name), to_string, {}, nil);
    {error, _Error} ->
      throw({error, {Name, "not loaded"}})
  end;
  
call(Module, to_s, Args, Block) ->
  call(Module, inspect, Args, Block).
  
