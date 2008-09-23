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
  [Function || {_, Function} <- dict:to_list(FunctionDict)] ++ Methods. 
    
process_function({function, _Line, Name, _Arity, _Clauses} = Function, {Dict, Functions}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      {dict:store(Name, Function, Dict), Functions};
    error ->
      {Dict, [Function|Functions]}
  end.
      
% Default set of functions to incorporate into Reia classes
default_functions() ->
  [{Name, parse_function(String)} || {Name, String} <- [
    {initialize,     initialize()}, 
    {method_missing, method_missing()}
  ]].
  
% Default definition of the "initialize" method
initialize() ->
  "initialize() -> void.".
  
% Default definition of the "method_missing" method
method_missing() ->
  "method_missing(_State, Method, _Args) -> throw({error, {Method, \"undefined\"}}).".
  
% Parse a function from a string
parse_function(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Form} = erl_parse:parse_form(Scanned),
  Form.