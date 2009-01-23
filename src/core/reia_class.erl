%
% reia_class: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%
 
-module(reia_class).
-export([build/1, ast/1, inst/3, call/2]).

%% Convert a Reia class definition into a Reia module which conforms to the
%% gen_server behavior, then load it into the code server
build({class, _Line, 'Object', _Methods} = Class) ->
  reia_module:build(ast(Class));
build(Class) ->
  %{module, Line, Name, Functions} = ast(Class),
  
  % Generate the methods which are derived from this class's ancestors
  BaseMethods = add_ancestor(dict:new(), 'Object'),
  
  % Generate and compile the base class
  _BaseClass = compile_inherited_methods(BaseMethods),
  
  % Generate the method for returning the class of an object
  %ClassMethod = parse_function("class() -> {constant, '" ++ atom_to_list(Name) ++ "'}."),

  reia_module:build(ast(Class)).
  
compile_inherited_methods(MethodsDict) ->
  Methods = [Method || {_, {_, Method}} <- dict:to_list(MethodsDict)],
  Class = {class, 1, {constant, 1, 'base_class'}, Methods},
  Passes = [Pass || Pass <- reia_compiler:default_passes(), Pass /= dynamic],
  [{class, _, _, Functions}] = reia_compiler:compile([Class], Passes),
  Functions.
     
%% Add an ancestor to the given class
add_ancestor(Methods, AncestorName) when is_atom(AncestorName) ->
  case code:ensure_loaded(AncestorName) of
    {module, _} -> void;
    Error -> throw(Error)
  end,
  
  AncestorClass = case [Code || {code, Code} <- AncestorName:module_info(attributes)] of
    [[Class]] -> Class;
    _ -> throw({error, {AncestorName, "lacks a code attribute (not a Reia module?)"}})
  end,
  
  add_ancestor(Methods, AncestorClass);
add_ancestor(Methods, {class, _Line, {constant, _, AncestorName}, AncestorMethods}) ->
  lists:foldr(
    fun(Function, Dict) ->
      {function, _, {identifier, _, Name}, _, _} = Function,
      dict:store(Name, {AncestorName, Function}, Dict) 
    end,
    Methods,
    AncestorMethods
  ).
    
%% Compile a Reia class to an Erlang module
ast({class, Line, Name, Methods}) ->
  Functions2 = build_functions(Name, Methods),
  {module, Line, Name, Functions2};
ast(_) ->
  {error, "invalid class"}.
 
%% Create an instance of a given class, passing the arguments on to its
%% initialize function
inst(Class, Arguments, _Block) ->
  Obj = Class:spawn_link(),
  call(Obj, {'initialize', Arguments}),
  Obj.
 
%% Call a method on a Reia object at the given Pid
call({object, {Pid, _Class}}, Request) ->
  call(Pid, Request);
call(Pid, {_Method, _Arguments} = Request) when is_pid(Pid) ->
  case gen_server:call(Pid, Request) of
    {ok, Value} -> Value;
    {error, Error} -> throw(Error)
  end.
 
%% Process incoming methods and build the functions for the resulting module
build_functions(Module, Methods) ->
  lists:flatten([
    start_functions(Module),
    default_functions(),
    method_functions(Module, Methods)
  ]).
  
%% Build a dispatch_method function and functions for each of the mangled methods
method_functions(Module, Methods) ->
  % Decompose the function clauses for methods into handle_call clauses
  {Clauses, Functions} = process_methods(Module, Methods),
  [build_method_dispatch_function(Clauses)|Functions].
 
%% Process methods into a list of clauses for dispatch_method and functions
%% with mangled names for each method
process_methods(Module, Methods) ->
  FinalMethods = merge_methods(Module, Methods),
  {NewClauses, NewFunctions} = lists:foldl(fun(Method, {Clauses, Functions}) ->
    {Clause, Function} = process_method(Method),
    {[Clause|Clauses], [Function|Functions]}
  end, {[],[]}, FinalMethods),
  {lists:reverse(NewClauses), lists:reverse(NewFunctions)}.
  
%% Merge default methods with the user-defined ones
merge_methods(Module, Methods) ->
  MethodDict = lists:foldr(
    fun(Function, Dict) ->
      {function, _Line, Name, _Arity, _Clauses} = Function,
      dict:store(Name, Function, Dict)
    end,
    dict:from_list(default_methods(Module)),
    Methods
  ),
  [Method || {_, Method} <- dict:to_list(MethodDict)].
  
%% Extract a method into its dispatch_method clause and mangled form
process_method({function, Line, Name, _Arity, Clauses}) ->
  MangledName = reia_mangle:method(Name),
  DispatcherClause = dispatcher_clause(Name, MangledName, Line),
  Function = {function, Line, MangledName, 3,
    [process_method_clause(Clause) || Clause <- Clauses]
  },
  {DispatcherClause, Function}.
 
%% Generate a clause for dispatch_method which thunks from a real method name
%% to the given mangled name
dispatcher_clause(RealName, MangledName, Line) ->
  {clause, Line, [
    {tuple, Line, [{atom, Line, RealName}, {var, Line, 'arguments'}]},
    {var, Line, 'caller'},
    {var, Line, 'instance_variables'}
  ], [], [
    {call, Line, {atom, Line, MangledName}, [
      {var, Line, 'arguments'},
      {var, Line, 'caller'},
      {var, Line, 'instance_variables'}
    ]}
  ]}.
       
%% Build a clause for dispatch_method from the original clauses for a method
process_method_clause({clause, Line, Arguments, [], Expressions}) ->
  {clause, Line, [
    argument_list_cons(Arguments, Line),
    {var, Line, '_caller'},
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
  
%% Generate Erlang forms for the class's method dispatch function
build_method_dispatch_function(Clauses) ->
  % Add a clause which thunks to _ if no method responds
  CatchallFunc = "dispatch_method({Method, Args}, Caller, State) -> dispatch_method({'_', [Method, Args]}, Caller, State).",
  {function, _, _, _, CatchallClause} = parse_function(CatchallFunc),
  {function, 1, dispatch_method, 3, Clauses ++ CatchallClause}.
 
%% These functions are required for the generated modules to implement the
%% gen_server behavior
default_functions() ->
  [parse_function(Function) || Function <- [
    "init([]) -> {ok, dict:new()}.",
    "handle_call(Request, From, State) -> try dispatch_method(Request, From, State) catch throw:Error -> {reply, {error, Error}, State} end.",
    "handle_cast(_Msg, State) -> {noreply, State}.",
    "handle_info(_Info, State) -> {noreply, State}.",
    "terminate(_Reason, _State) -> ok.",
    "code_change(_OldVsn, State, _Extra) -> {ok, State}."
  ]].
  
%% Default methods that Reia objects respond to
default_methods(Module) ->
  NameString = lists:concat(["reia_string:from_list(\"#<", Module, ">\")"]),
  [default_method(Function) || Function <- [
    "class() -> {constant, '" ++ atom_to_list(Module) ++ "'}.",
    "initialize() -> nil.",
    "to_s() -> " ++ NameString ++ ".",
    "inspect() -> " ++ NameString ++ ".",
    "'_'(Method, _Args) -> throw({error, {Method, \"undefined\"}})."
  ]].
  
%% Parse a default function and return a dict entry for it
default_method(String) ->
  Form = parse_function(String),
  {function, _, Name, _, _} = Form,
  {Name, Form}.
  
%% Functions for starting a new object
start_functions(Module) ->
  [start_function(Module, Function) || Function <- [{"spawn", "start"}, {"spawn_link", "start_link"}]].
 
start_function(Module, {ReiaFunc, OtpFunc}) ->
  String = [
    ReiaFunc, "() -> {ok, Pid} = gen_server:", OtpFunc, "('", Module, "', [], [])," ++
    "{object, {Pid, '", Module, "'}}."
  ],
  parse_function(lists:concat(String)).
    
%% Parse a function from a string
parse_function(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Form} = erl_parse:parse_form(Scanned),
  Form.