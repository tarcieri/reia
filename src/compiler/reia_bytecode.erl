%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bytecode).
-export([string/1, transform/1, transform/2, load/1]).

% Ideally this record is opaque to everything except this module
% No other modules should operate directly on raw Reia bytecode
-record(reia_module, {version=0, filename, base_module}).

% Pseudo string eval which generates a single-use module
string(Str) ->
  case reia_parse:parse(Str) of
	  {fail, Error} ->
		  {error, Error};
		Expression ->
      transform([Expression])
  end.

% Load the given compiled Reia module, executing its toplevel function
load(Bin) ->
	#reia_module{filename = Filename, base_module = Module} = binary_to_term(Bin),
	Name = list_to_atom(Filename),
	code:load_binary(Name, Filename, Module),
	Result = Name:toplevel(),
	{ok, Name, Result}.

% Compiled evaluation of a list of Reia expressions
transform(Expressions) ->
  transform(nonce_filename(), Expressions).
  
% Compiled evaluation of a parsed Reia file
transform(Filename, Expressions) ->
  {ok, _Module, Bin} = transform_expressions(Filename, Expressions),
  Module = #reia_module{filename=Filename, base_module=Bin},
  {ok, term_to_binary(Module)}.

% Output raw Erlang bytecode for inclusion into compiled Reia bytecode
transform_expressions(Filename, Expressions) ->  
  % Create a "toplevel" function which is called when the module is loaded
  Function = {function, 1, toplevel, 0, [
    {clause, 1, [], [], Expressions}
  ]},
  
  Module = [
    {attribute, 1, module, list_to_atom(Filename)},
    {attribute, 1, file, {Filename, 1}},
    {attribute, 1, code, Expressions},
    Function
  ],
  
  io:format("Code: ~p~n", [Module]),
  
  compile:forms(Module, [
    debug_info, 
    export_all, 
    verbose, 
    report_errors, 
    report_warnings
  ]).
  
nonce_filename() ->
  ["#Ref" ++ Ref] = io_lib:format("~p", [make_ref()]),
  "reia_eval#" ++ Ref.
  