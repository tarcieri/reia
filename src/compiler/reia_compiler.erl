%
% reia_compiler: Translates Reia into Erlang, then into Erlang bytecode
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([file/1, file/2, string/2, string/3, compile/2, compile/3]).
-include("reia_compile_options.hrl").
-define(parse_error(File, Line, Error), throw(lists:flatten(io_lib:format("~s:~w: ~s", [File, Line, Error])))).

% Compile the given file
file(Filename) ->
  file(Filename, []).
  
% Compile the given file with the given options.  See compile/3 below.
file(Filename, Options) ->
  {ok, Bin} = file:read_file(Filename),
  string(Filename, binary_to_list(Bin), Options).
  
% Compile the given string, treating it as if it came from the given file
string(Filename, String) ->
  string(Filename, String, []).
  
% Compile the given string with the given options.  See compile/3 below.
string(Filename, String, Options) ->
  case reia_scan:string(String) of
    {ok, Tokens, _} -> 
      case reia_parse:parse(Tokens) of
        {ok, Exprs} ->
          compile(Filename, Exprs, Options);
        {error, {_, _, [Error, []]}} ->
          ?parse_error(Filename, eof, lists:flatten([Error, "end of file"]));
        {error, {Line, _, [Error, Token]}} ->
          ?parse_error(Filename, Line, lists:flatten([Error, Token]))
      end;
    {error, {Line, _, {Error, Token}}, _} ->
      ?parse_error(Filename, Line, lists:flatten([Error, Token]))
  end.

% Compile the given expressions, which came from the given source filename
compile(Filename, Exprs) ->
  compile(Filename, Exprs, []).

% Compile the given expressions, which came from the given source filename
%
% Accepts a list of compile options, given as 2-tuples.  The compiler
% recognizes the following option keys in the form {Key, Value}:
% * scope:         Scope the given expressions are considered to exist in
% * passes:        Passes which should be applied to the input expressions
% * toplevel_args: A list of arguments the toplevel method accepts
% * erlc_args:     A list of options to be passed alogn to erlc
compile(Filename, Exprs, Options) ->
  Options2 = [{code, Exprs}|Options],
  OptRecord = lists:foldl(fun process_option/2, #compile_options{}, Options2),
  run_passes(Filename, Exprs, OptRecord).

% Build a compile_options record from the specified options list
% Since records suck so much, we have to manually specify all of the possible
% patterns and their behaviors.  Can't DRY it out.  Records suck :(
process_option({code, Code}, Options) ->
  Options#compile_options{code = Code};
process_option({scope, Scope}, Options) ->
  Options#compile_options{scope = Scope};
process_option({passes, Passes}, Options) ->
  Options#compile_options{passes = Passes};
process_option({autohipe, AutoHiPE}, Options) ->
  Options#compile_options{autohipe = AutoHiPE};
process_option({toplevel_wrapper, ToplevelWrapper}, Options) ->
  Options#compile_options{toplevel_wrapper = ToplevelWrapper};
process_option({erlc_options, Opts}, Options) ->
  Options#compile_options{erlc_options = Opts}.

run_passes(Filename, Exprs, Options) ->
  Passes = Options#compile_options.passes,
  Modules = [list_to_atom("reia_" ++ atom_to_list(Pass)) || Pass <- Passes],
  Fun = fun(Pass, Code) -> Pass:transform(Code, Options) end,
  Exprs2 = lists:foldl(Fun, Exprs, Modules),
  reia_bytecode:compile(Filename, Exprs2, Options).