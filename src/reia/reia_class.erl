%
% reia_module: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_class).
-export([build/1]).
-compile(export_all).

build({class, Line, Name, Functions}) ->
  Module = {module, Line, Name, process_functions(Functions)},
  io:format("~p~n", [Module]),
  reia_module:build(Module);
build(_) ->
  {error, "invalid class"}.

% Process incoming functions, substituting custom versions for defaults  
process_functions(Functions) ->
  {FunctionDict, Methods} = lists:foldr(
    fun process_function/2, 
    {dict:from_list(default_functions()), []}, 
    Functions
  ),
  ImmediateFunctions = [Function || {_, Function} <- dict:to_list(FunctionDict)],
  ImmediateFunctions ++ process_methods(Methods). 
    
process_function({function, _Line, Name, _Arity, _Clauses} = Function, {Dict, Functions}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      {dict:store(Name, Function, Dict), Functions};
    error ->
      {Dict, [Function|Functions]}
  end.
  
process_methods([]) ->
  [];
process_methods([FirstMeth|_] = Methods) ->
  % Extract the line number from the first method
  {function, Line, _, _, _} = FirstMeth,
  
  % New master handle_call function
  [{function, Line, handle_call, 3, lists:flatten([process_method(Method) || Method <- Methods])}].
  
process_method({function, _Line, Name, _Arity, Clauses}) ->
  [process_method_clause(Clause, Name) || Clause <- Clauses].
       
process_method_clause({clause, Line, Arguments, [], Expressions}, Name) ->
  {clause, Line, [
    {tuple, Line, [{atom, Line, Name}, argument_list_cons(Arguments, Line)]}, 
    {var, Line, '_From'}, 
    {var, Line, '_Ivars'}
  ], [], Expressions}.
  
%% Generate cons for arguments
argument_list_cons([], Line) ->
  {nil, Line};
argument_list_cons([Element|Rest], Line) ->
  {cons, Line, Element, argument_list_cons(Rest, Line)}.

% Default set of functions to incorporate into Reia classes
default_functions() ->
  [{Name, parse_function(String)} || {Name, String} <- [
    {init,           "init(Args) -> initialize(Args), {ok, dict:new()}."},
    {initialize,     "initialize(_Args) -> void."}, 
    {method_missing, "method_missing(_State, Method, _Args) -> throw({error, {Method, \"undefined\"}})."}
  ]].
  
% Parse a function from a string
parse_function(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Form} = erl_parse:parse_form(Scanned),
  Form.