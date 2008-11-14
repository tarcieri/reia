%
% reia_class: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_class).
-export([build/1, call/2]).
-compile(export_all).

build({class, Line, Name, Functions}) ->
  Module = {module, Line, Name, process_functions(Name, Functions)},
  % io:format("~p~n", [Module]),
  reia_module:build(Module);
build(_) ->
  {error, "invalid class"}.

call(Pid, {_Method, _Arguments} = Request) ->
  case gen_server:call(Pid, Request) of
    {ok, Value} -> Value;
    {error, Error} -> throw(Error)
  end.

% Process incoming functions, substituting custom versions for defaults  
process_functions(Module, Functions) ->
  {FunctionDict, Methods} = lists:foldr(
    fun process_function/2, 
    {dict:from_list(default_functions()), []}, 
    Functions
  ),
  
  DefaultFunctions = init_functions(Module),
  ImmediateFunctions = [Function || {_, Function} <- dict:to_list(FunctionDict)],
  DefaultFunctions ++ ImmediateFunctions ++ process_methods(Methods). 
    
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
  
  % Decompose the function clauses for methods into handle_call clauses
  Clauses = lists:flatten([process_method(Method) || Method <- Methods ++ default_methods()]),
  
  % Add a clause which thunks to method_missing
  MethodMissingThunk = "dispatch_method({Method, Args}, _, State) -> method_missing(State, Method, Args).",
  {function, _, _, _, MethodMissingClause} = parse_function(MethodMissingThunk),
  
  % New master handle_call function
  [{function, Line, dispatch_method, 3, Clauses ++ MethodMissingClause}].
  
process_method({function, _Line, Name, _Arity, Clauses}) ->
  [process_method_clause(Clause, Name) || Clause <- Clauses].
       
process_method_clause({clause, Line, Arguments, [], Expressions}, Name) ->
  {clause, Line, [
    {tuple, Line, [{atom, Line, Name}, argument_list_cons(Arguments, Line)]}, 
    {var, Line, '_From'}, 
    {var, Line, '__instance_variables_0'}
  ], [], process_return_value(Line, Expressions)}.

process_return_value(Line, []) ->
  process_return_value(Line, [{atom, Line, 'nil'}]);
process_return_value(Line, Expressions) ->
  [Result|Expressions2] = lists:reverse(Expressions),
  Result2 = {match, Line, {var, Line, '__return_value'}, Result},
  Result3 = {tuple, Line, [
    {atom, Line, reply},
    {tuple, Line, [{atom, Line, ok}, {var, Line, '__return_value'}]}, 
    {var, Line, final_ivars(Expressions)}
  ]},
  lists:reverse([Result3,Result2|Expressions2]).

final_ivars(Expressions) ->
  {ok, Newest, _} = reia_visitor:transform(Expressions, 0, fun newest_ivars/2),
  Name = io_lib:format("~s~w", ["__instance_variables_", Newest]),
  list_to_atom(lists:flatten(Name)).

newest_ivars(Newest, {var, _Line, Name} = Node) ->
  case atom_to_list(Name) of
    "__instance_variables_" ++ VersionStr ->
      Version = list_to_integer(VersionStr),
      Newest2 = if
        Version > Newest -> 
          Version;
        true ->
          Newest
      end,
      {stop, Newest2, Node};
    _ ->
      {stop, Newest, Node}
  end;
newest_ivars(Newest, Node) ->
  {walk, Newest, Node}.

%% Generate cons for arguments
argument_list_cons([], Line) ->
  {nil, Line};
argument_list_cons([Element|Rest], Line) ->
  {cons, Line, Element, argument_list_cons(Rest, Line)}.

% Default functions to incorporate into Reia classes
default_functions() ->
  [default_function(Function) || Function <- [
    "init(Args) -> initialize(Args), {ok, dict:new()}.",
    "initialize(_Args) -> void.", 
    "method_missing(_State, Method, _Args) -> throw({error, {Method, \"undefined\"}}).",
    "handle_call(Request, From, State) -> try dispatch_method(Request, From, State) catch throw:Error -> {reply, {error, Error}, State} end.",
    "handle_cast(_Msg, State) -> {noreply, State}.",
    "handle_info(_Info, State) -> {noreply, State}.",
    "terminate(_Reason, _State) -> ok.",
    "code_change(_OldVsn, State, _Extra) -> {ok, State}."
  ]].
  
default_function(String) ->
  Form = parse_function(String),
  {function, _, Name, _, _} = Form,
  {Name, Form}.
  
% Default methods that Reia objects respond to
default_methods() ->
  [parse_function(Function) || Function <- [
    "to_s() -> {string, <<\"#<Object>\">>}.",
    "inspect() -> {string, <<\"#<Object>\">>}."
  ]].
  
% Functions for starting a new object
init_functions(Module) ->
  [init_function(Module, Function) || Function <- ["start", "start_link"]].

init_function(Module, Function) ->
  String = [Function, "() -> {ok, Pid} = gen_server:", Function, "('", Module, "', [], []), {object, {Pid, '", Module, "'}}."],
  parse_function(lists:concat(String)).
  
% Parse a function from a string
parse_function(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Form} = erl_parse:parse_form(Scanned),
  Form.