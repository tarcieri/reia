%
% reia_bytecode: Generate bytecode from compiled Reia AST
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_bytecode).
-export([string/1, transform/1, transform/2]).
-record(reia_module, {version=0, filename, base_module, submodules}).

% Pseudo string eval which generates a single-use module
string(Str) ->
  case reia_parse:string(Str) of
    {ok, Expressions} ->
      transform(Expressions);
    Error ->
      Error
  end.

% Compiled evaluation of a list of Reia expressions
transform(Expressions) ->
  transform(nonce_filename(), Expressions).
  
% Compiled evaluation of a parsed Reia file
transform(Filename, Expressions) ->
  {ok, _Module, Bin} = transform_expressions(Filename, Expressions),
  Module = #reia_module{filename=Filename, base_module=Bin, submodules=[]},
  {ok, term_to_binary(Module)}.
  
transform_expressions(Filename, Expressions) ->  
  % Create a "toplevel" function which is called when the module is loaded
  Function = {function, 1, toplevel, 1, [
    {clause, 1, [{var, 1, '__module_data'}], [], Expressions}
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
  