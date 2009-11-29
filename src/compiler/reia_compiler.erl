%
% reia_compiler: Translates Reia into Erlang, then into Erlang bytecode
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([compile/2, compile/3]).
-include("reia_compile_options.hrl").

% Compile the given expressions, which came from the given source filename
compile(Filename, Expressions) ->
  compile(Filename, Expressions, []).

% Compile the given expressions, which came from the given source filename
%
% Accepts a list of compile options, given as 2-tuples.  The compiler
% recognizes the following option keys in the form {Key, Value}:
% * scope:              Scope the given expressions are considered to exist in
% * passes:             Passes which should be applied to the input expressions
% * toplevel_arguments: A list of arguments the toplevel method accepts
% * erlc_args:          A list of options to be passed alogn to erlc
compile(Filename, Expressions, Options) ->
  OptRecord = lists:foldl(fun process_option/2, #compile_options{}, Options),
  run_passes(Filename, Expressions, OptRecord).

% Build a compile_options record from the specified options list
% Since records suck so much, we have to manually specify all of the possible
% patterns and their behaviors.  Can't DRY it out.  Records suck :(
process_option({scope, Scope}, Options) ->
  Options#compile_options{scope = Scope};
process_option({passes, Passes}, Options) ->
  Options#compile_options{passes = Passes};
process_option({toplevel_arguments, Args}, Options) ->
  Options#compile_options{toplevel_arguments = Args};
process_option({erlc_options, Opts}, Options) ->
  Options#compile_options{erlc_options = Opts}.

run_passes(Filename, Expressions, Options) ->
  Passes = Options#compile_options.passes,
  Modules = [list_to_atom("reia_" ++ atom_to_list(Pass)) || Pass <- Passes],
  Fun = fun(Pass, Exprs) -> Pass:transform(Exprs, Options) end,
  Expressions2 = lists:foldl(Fun, Expressions, Modules),
  reia_bytecode:compile(Filename, Expressions2, Options).