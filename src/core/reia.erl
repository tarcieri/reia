%
% reia: Interface between Erlang and Reia environments
% Copyright (C)2008-09 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia).
-export([init/0, load_submodule/2]).

% Initialize the Reia environment
init() -> load_core().

% Internal function for loading submodules
load_submodule(Name, Attributes) ->
  {value, {parent, [Parent]}} = lists:keysearch(parent, 1, Attributes),
  ParentAttrs = Parent:module_info(attributes),
  {value, {submodules, Submodules}} = lists:keysearch(submodules, 1, ParentAttrs),
  {value, {static, Name, Bin}} = lists:keysearch(Name, 2, Submodules),
  
  % FIXME: should probably fix the filename here
  code:load_binary(Name, "submodule", Bin).

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
    false -> throw({error, "can't locate the Reia distribution"})
  end.