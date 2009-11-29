%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bytecode).
-export([compile/2, compile/3, load/1]).
-include("reia_compile_options.hrl").

% Ideally this record is opaque to everything except this module
% No other modules should operate directly on raw Reia bytecode
-record(reia_module, {version=0, filename, base_module}).

% Load the given compiled Reia module, executing its toplevel function
load(Bin) ->
	#reia_module{filename = Filename, base_module = Module} = binary_to_term(Bin),
	Name = list_to_atom(Filename),
	code:load_binary(Name, Filename, Module),
	Result = Name:toplevel(),
	{ok, Name, Result}.
  
% Compiled evaluation of a parsed Reia file
compile(Filename, Expressions) ->
  compile(Filename, Expressions, #compile_options{}).

compile(Filename, Expressions, Options) ->
  io:format("Output Code: ~p~n", [Expressions]),
  case compile_expressions(Filename, Expressions, Options) of
    {ok, _Module, Bin} ->
      Module = #reia_module{filename=Filename, base_module=Bin},
      {ok, term_to_binary(Module)};
    error ->
      throw({error, "internal Erlang compiler error"})
  end.

% Output raw Erlang bytecode for inclusion into compiled Reia bytecode
compile_expressions(Filename, Expressions, Options) ->
  % Build argument list in the abstract format
  Args = [{var, 1, Name} || Name <- Options#compile_options.toplevel_args],

  % Create a "toplevel" function which is called when the module is loaded
  Function = {function, 1, toplevel, length(Args), [
    {clause, 1, Args, [], Expressions}
  ]},
  
  Module = [
    {attribute, 1, module, list_to_atom(Filename)},
    {attribute, 1, file, {Filename, 1}},
    {attribute, 1, code, Expressions},
    Function
  ],
  
  compile:forms(Module, Options#compile_options.erlc_options).