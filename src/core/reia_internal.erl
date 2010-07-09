%
% reia_internal.erl: A potpourri of internal functions
% Copyright (C)2008-10 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia_internal).
-export([
  compile/1, compile/2, 
  execute_file/1, 
  load_submodule/2,
  load_core/0,
  load_stdlib/0,
  print_error/2,
  invoke_callable/3
]).
-include("reia_types.hrl").

% Thunk for reiac
compile([SourcePath, BinPath]) ->
  compile(SourcePath, BinPath).

% Compile and load the given Reia source code
compile(SourcePath, BinPath) ->
  {ok, Bin} = reia_compiler:file(SourcePath),
  file:write_file(BinPath, Bin).
  
% Internal function for loading code from the 'reia' command line script
execute_file([Filename]) when is_atom(Filename) ->
  % Thunk for loading files from the CLI
  execute_file(atom_to_list(Filename));
execute_file(Filename) ->
  try
    reia:load(Filename)
  catch Class:Error ->
    case Error of
      #reia_object{} ->
        #reia_string{elements=Elements} = reia:invoke(Error, to_s, {}),
        io:format("~s~n", [iolist_to_binary(Elements)]);
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
  load_core_beam(), load_core_reb(), ok.

% Load compiled BEAM files
load_core_beam() ->
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
  
% Load core REB files
load_core_reb() ->
  Files = filelib:wildcard(base_directory() ++ "/ebin/*.reb"),
  [reia_bytecode:load_file(File) || File <- Files].

load_stdlib() ->
  Files = filelib:wildcard(base_directory() ++ "/lib/*.re"),
  [reia:load(File) || File <- Files].
    
% Base directory of the Reia distribution
base_directory() ->
  % Look for a REIA_HOME environment variable, which takes precedence
  case [Home || "REIA_HOME=" ++ Home <- os:getenv()] of
    [Dir] -> Dir;
    _ ->
      % Look for the Reia distribution under the Erlang lib directory
      Dir = code:lib_dir() ++ "/reia",
      case filelib:is_dir(Dir) of
        true  -> Dir;
        false ->
          throw({error, "can't locate the Reia distribution"})
      end
  end.
  
% Display errors and stack traces for unhandled Erlang exceptions
print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50])
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M == erl_eval) or (M == ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).
  
% Handle calls to non-lambda callable types
invoke_callable(Callable, Args, Block) ->
  case Callable of
    #reia_funref{receiver=Receiver, name=Method} ->
      reia_dispatch:call(Receiver, Method, Args, Block)
  end.