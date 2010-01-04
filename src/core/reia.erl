%
% reia: Interface between Erlang and Reia environments
% Copyright (C)2008-09 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia).
-export([init/0, load/1, execute_file/1, load_submodule/2]).
-include("reia_types.hrl").

%
% Public functions
%

% Initialize the Reia environment
init() -> load_core().

load(Filename) ->
  Absname = filename:absname(Filename),
  RebPath = filename:rootname(Absname) ++ ".reb",
  {ok, Bin} = reia_compiler:file(Absname),
  file:write_file(RebPath, Bin),
  io:format("Result: ~p~n", [Bin]).

%
% Internal functions
%
  
% Internal function for loading code from the 'reia' command line script
execute_file([Filename]) when is_atom(Filename) ->
  % Thunk for loading files from the CLI
  execute_file(atom_to_list(Filename));
execute_file(Filename) ->
  try
    load(Filename)
  catch Class:Error ->
    case Error of
      _ when is_list(Error) -> 
        io:format("~s~n", [Error]);
      {error, {Line, Message}} ->
        io:format("~s:~w: ~s~n", [Filename, Line, Message]);
      _ ->
        print_error(Class, Error)
    end
  end.

% Internal function for loading submodules
load_submodule(Name, Attributes) ->
  {value, {parent, [Parent]}} = lists:keysearch(parent, 1, Attributes),
  ParentAttrs = Parent:module_info(attributes),
  {value, {submodules, Submodules}} = lists:keysearch(submodules, 1, ParentAttrs),
  {value, {static, Name, Bin}} = lists:keysearch(Name, 2, Submodules),
  
  % FIXME: should probably fix the filename here
  {module, Name} = code:load_binary(Name, "submodule", Bin),
  #reia_module{name = Name}.

% Load the core modules AOT
% This prevents naming conflicts on case insensitive filesystems between Reia
% and Erlang modules (e.g. string and String)
load_core() ->
  Modules = filelib:wildcard(base_directory() ++ "/ebin/*.beam"),
  [load_module(Module) || Module <- Modules],
  ok.
  
% Load a Reia module from the given path
load_module(Path) ->
  Module = filename:rootname(Path, ".beam"),
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
  
% Display errors and stack traces for unhandled Erlang exceptions
print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50])
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).