%
% reia: Interface between Erlang and Reia environments
% Copyright (C)2008-09 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia).
-export([init/0]).

% Initialize the Reia environment
init() -> load_core().

% Load the core modules AOT
% This prevents naming conflicts on case insensitive filesystems between Reia
% and Erlang modules (e.g. string and String)
load_core() ->
  Modules = filelib:wildcard(base_directory() ++ "/ebin/*.beam"),
  [load_module(Module) || Module <- Modules],
  ok.
  
% Load a Reia module from the given path
load_module(Path) ->
  Module = lists:sublist(Path, length(Path) - 5), % Strip the .beam extension
  case code:load_abs(Module) of
    {module, _} -> ok;
    {error, Error} ->
      throw({error, {"couldn't load " ++ Path, Error}})
  end.
  
% Base directory of the Reia distribution
base_directory() ->
  {ok, Dir} = file:get_cwd(),
  case filelib:is_dir(Dir ++ "/ebin") of
    true  -> Dir;
    false -> {error, "can't locate the Reia distribution"}
  end.