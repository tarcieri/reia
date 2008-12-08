%
% reia_class: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_class).
-export([build/1, call/2]).
-compile(export_all).

%% Convert a Reia class definition into a Reia module which conforms to the
%% gen_server behavior, then load it into the code server
build({class, Line, Name, Functions}) ->
  Functions2 = process_functions(Name, Functions),
  % [io:format(erl_pp:form(Function)) || Function <- Functions2],
  Module = {module, Line, Name, Functions2},
  % io:format("~p~n", [Module]),
  reia_module:build(Module);
build(_) ->
  {error, "invalid class"}.

%% Call a method on a Reia object at the given Pid
call(Pid, {_Method, _Arguments} = Request) ->
  case gen_server:call(Pid, Request) of
    {ok, Value} -> Value;
    {error, Error} -> throw(Error)
  end.

%% Process incoming functions, substituting custom versions for defaults  
process_functions(Module, Functions) ->
  {FunctionDict, Methods} = lists:foldr(
    fun process_function/2, 
    {dict:from_list(default_functions()), []}, 
    Functions
  ),
    
  % Pull the initialize function out for special case processing
  {ok, InitializeMethod} = dict:find(initialize, FunctionDict),
  FunctionDict2 = dict:erase(initialize, FunctionDict),
  
  DefaultFunctions = start_functions(Module, function_arity(InitializeMethod)),
  ImmediateFunctions = [Function || {_, Function} <- dict:to_list(FunctionDict2)],
  lists:flatten([
    DefaultFunctions, 
    ImmediateFunctions, 
    initialize_method(InitializeMethod), 
    process_methods(Methods)
  ]). 
  
%% If a method name matches one of the default_functions(), then it has a
%% special purpose and should be mapped to a function rather than a method.
process_function({function, _Line, Name, _Arity, _Clauses} = Function, {Dict, Functions}) ->
  case dict:find(Name, Dict) of
    {ok, _} ->
      {dict:store(Name, Function, Dict), Functions};
    error ->
      {Dict, [Function|Functions]}
  end.
  
%% Construct the initialize method (as a function call for now, ugh)
initialize_method({function, Line, Name, _Arity, Clauses}) ->
  {function, Line, Name, 1, [initialize_clause(Clause) || Clause <- Clauses]}.

%% Process a clause of initialize
initialize_clause({clause, Line, Arguments, Guards, Expressions}) ->
  Arguments2 = [argument_list_cons(Arguments, Line)],
  InitIvars = {match, Line,
                {var, Line, '___instance_variables_0'},
                {call, Line, {remote, Line, {atom, Line, dict}, {atom, Line, new}}, []}
              },
  ReturnValue = {var, Line, final_ivars(Expressions)},
  Expressions2 = lists:flatten([InitIvars, Expressions, ReturnValue]),
  {clause, Line, Arguments2, Guards, Expressions2}.
  
%% Convert individual method definitions into a single dispatch_method function
process_methods([]) ->
  build_method_dispatch_function(1, lists:flatten([process_method(Method) || Method <- default_methods()]));
process_methods([FirstMeth|_] = Methods) ->
  % Extract the line number from the first method
  {function, Line, _, _, _} = FirstMeth,
  
  % Decompose the function clauses for methods into handle_call clauses
  Clauses = lists:flatten([process_method(Method) || Method <- Methods ++ default_methods()]),
  
  build_method_dispatch_function(Line, Clauses).

%% Generate Erlang forms for the class's method dispatch function
build_method_dispatch_function(Line, Clauses) ->
  % Add a clause which thunks to method_missing
  MethodMissingThunk = "dispatch_method({Method, Args}, _, State) -> method_missing(State, Method, Args).",
  {function, _, _, _, MethodMissingClause} = parse_function(MethodMissingThunk),
  
  % New master handle_call function
  [{function, Line, dispatch_method, 3, Clauses ++ MethodMissingClause}].
  
%% Extract a method into clauses for dispatch_method
process_method({function, _Line, Name, _Arity, Clauses}) ->
  [process_method_clause(Clause, Name) || Clause <- Clauses].
       
%% Build a clause for dispatch_method from the original clauses for a method
process_method_clause({clause, Line, Arguments, [], Expressions}, Name) ->
  {clause, Line, [
    {tuple, Line, [{atom, Line, Name}, argument_list_cons(Arguments, Line)]}, 
    {var, Line, '_From'}, 
    {var, Line, '___instance_variables_0'}
  ], [], process_return_value(Line, Expressions)}.

%% Convert a method's return value into a gen_server reply
process_return_value(Line, []) ->
  process_return_value(Line, [{atom, Line, 'nil'}]);
process_return_value(Line, Expressions) ->
  [Result|Expressions2] = lists:reverse(Expressions),
  Result2 = {match, Line, {var, Line, '__method_return_value'}, Result},
  Result3 = {tuple, Line, [
    {atom, Line, reply},
    {tuple, Line, [{atom, Line, ok}, {var, Line, '__method_return_value'}]}, 
    {var, Line, final_ivars(Expressions)}
  ]},
  lists:reverse([Result3,Result2|Expressions2]).

%% Find the name of the last SSA-transformed ___instance_variables variable
%% present in a given function.
final_ivars(Expressions) ->
  {ok, Newest, _} = reia_visitor:transform(Expressions, 0, fun newest_ivars/2),
  Name = io_lib:format("~s~w", ["___instance_variables_", Newest]),
  list_to_atom(lists:flatten(Name)).

%% Locate the number of the last SSA transformation of the __instance_variables
%% variable in a given function.
newest_ivars(Newest, {var, _Line, Name} = Node) ->
  case atom_to_list(Name) of
    "___instance_variables_" ++ VersionStr ->
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

%% Default functions to incorporate into Reia classes
default_functions() ->
  [default_function(Function) || Function <- [
    "init(Args) -> {ok, initialize(Args)}.",
    "method_missing(_State, Method, _Args) -> throw({error, {Method, \"undefined\"}}).",
    "handle_call(Request, From, State) -> try dispatch_method(Request, From, State) catch throw:Error -> {reply, {error, Error}, State} end.",
    "handle_cast(_Msg, State) -> {noreply, State}.",
    "handle_info(_Info, State) -> {noreply, State}.",
    "terminate(_Reason, _State) -> ok.",
    "code_change(_OldVsn, State, _Extra) -> {ok, State}.",
    
    % A bit sneaky here as this is an untransformed method:
    "initialize() -> nil."
  ]].

%% Parse a default function and return a dict entry for it  
default_function(String) ->
  Form = parse_function(String),
  {function, _, Name, _, _} = Form,
  {Name, Form}.
  
%% Default methods that Reia objects respond to
default_methods() ->
  [parse_function(Function) || Function <- [
    "to_s() -> {string, <<\"#<Object>\">>}.",
    "inspect() -> {string, <<\"#<Object>\">>}."
  ]].
  
%% Functions for starting a new object
start_functions(Module, Arity) ->
  [start_function(Module, Function, Arity) || Function <- ["start", "start_link"]].

start_function(Module, Function, Arity) ->
  Vars = variable_list(Arity),
  String = [Function, "("] ++ Vars ++ 
    [") -> {ok, Pid} = gen_server:", Function, "('", Module, "', ["] ++ Vars ++ [
    "], []), {object, {Pid, '", Module, "'}}."],
  parse_function(lists:concat(String)).
  
variable_list(0) ->
  [];
variable_list(Size) ->
  add_commas([lists:flatten(io_lib:format("Var~w", [N])) || N <- lists:seq(1, Size)]).
  
add_commas(List) -> 
  add_commas(List, []).
add_commas([Arg], Result) ->
  lists:reverse([Arg|Result]);
add_commas([Head|Rest], Result) ->
  add_commas(Rest, [",",Head|Result]).
  
%% Parse a function from a string
parse_function(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Form} = erl_parse:parse_form(Scanned),
  Form.
  
%% Return the arity of a function in Erlang abstract format
function_arity({function, _Line, _Name, Arity, _Clauses}) ->
  Arity.