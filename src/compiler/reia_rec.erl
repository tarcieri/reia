%
% reia_rec: Internal format of compiled Reia scripts
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_rec).
-export([string/1, compile/1, compile/2]).
-record(recfile, {version=0, filename, base_module, submodules}).

% Pseudo string eval which generates a single-use module
string(Str) ->
  case reia_parse:string(Str) of
    {ok, Expressions} ->
      compile(Expressions);
    Error ->
      Error
  end.

% Compiled evaluation of a list of Reia expressions
compile(Expressions) ->
  compile(nonce_filename(), Expressions).
  
% Compiled evaluation of a parsed Reia file
compile(Filename, Expressions) ->
  {ok, _Module, Bin} = compile_expressions(Filename, Expressions),
  Recfile = #recfile{filename=Filename, base_module=Bin, submodules=[]},
  {ok, term_to_binary(Recfile)}.
  
compile_expressions(Filename, Expressions) ->
  % Compile Reia expressions into Erlang ones
  Expressions2 = reia_compiler:compile(Expressions, compiler_passes()),
  
  % Create a "toplevel" function which is called when the module is loaded
  Function = {function, 1, toplevel, 1, [
    {clause, 1, [{var, 1, '__module_data'}], [], Expressions2}
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
    %{parse_transform, smart_exceptions}
  ]).
  
% Generate the list of compiler passes to run.  Filter out the old final 
% compilation stage (called "dynamic") which is slow and messy and thunks
% through the erl_eval metacircular evaluator, which makes it slow and imposes
% somewhat nebulous semantics on the evaluated code.
compiler_passes() ->
  lists:filter(fun(dynamic) -> false; (_) -> true end, reia_compiler:default_passes()).
  
nonce_filename() ->
  ["#Ref" ++ Ref] = io_lib:format("~p", [make_ref()]),
  "reia_eval#" ++ Ref.
  