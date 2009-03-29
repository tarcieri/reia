%
% reia_class: Build modules conforming to the gen_server behavior from Reia classes
% Copyright (C)2008 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%
 
-module(reia_class).
-export([build/1, build/2, ast/1, inst/3, call/2]).

%% Convert a Reia class definition into a Reia module which conforms to the
%% gen_server behavior, then load it into the code server
build({class, _Line, 'Object', _, _Methods} = Class) ->
  % Object gets special case behavior as it has no ancestor
  reia_module:build(ast(Class)).
  
build({class, Line, Name, Ancestor, Methods}, OrigExprs) ->
  % Generate the methods which are derived from this class's ancestors
  ParentMethods = build_inherited_methods(build_parent_from_ancestry(Ancestor)),
  
  % Generate the obj.class() method
  ClassMethod = parse_function("class({}, nil) -> {constant, '" ++ atom_to_list(Name) ++ "'}."),
  
  % Merge this class's methods with its parent
  FinalMethods = merge_with_parent([ClassMethod|Methods], ParentMethods),

  reia_module:build(ast({class, Line, Name, Ancestor, FinalMethods}), [{code, [OrigExprs]}]).
  
% Walk the ancestors living in the code server, combining them into a single
% unified parent class
build_parent_from_ancestry(AncestorName) when is_atom(AncestorName) ->
  case code:ensure_loaded(AncestorName) of
    {module, _} -> void;
    Error -> throw(Error)
  end,
  
  AncestorClass = case [Code || {code, Code} <- AncestorName:module_info(attributes)] of
    [[Class]] -> Class;
    _ -> throw({error, {AncestorName, "lacks a code attribute (not a Reia module?)"}})
  end,
  
  build_parent_from_ancestry(AncestorClass);  
build_parent_from_ancestry({class, _Line, {constant, _, 'Object'}, Methods}) ->
  merge_ancestor_methods(dict:new(), 'Object', Methods);
build_parent_from_ancestry({class, Line, Name, Methods}) ->
  build_parent_from_ancestry({class, Line, Name, {constant, Line, 'Object'}, Methods});
build_parent_from_ancestry({class, _Line, {constant, _, Name}, {constant, _, AncestorName}, Methods}) ->
  merge_ancestor_methods(build_parent_from_ancestry(AncestorName), Name, Methods).
  
merge_ancestor_methods(AncestorMethods, ClassName, Methods) ->
  lists:foldl(
    fun({function, _, {identifier, _, Name}, _, _, _} = Function, Dict) ->
      dict:store(Name, {method, ClassName, Function}, Dict) 
    end,
    AncestorMethods,
    Methods
  ).
  
merge_with_parent(Methods, AncestorMethods) ->
  FinalMethods = lists:foldl(
    fun({function, _, Name, _, _} = Function, Dict) ->
      dict:store(Name, Function, Dict)
    end,
    AncestorMethods,
    Methods
  ),
  [Method || {_, Method} <- dict:to_list(FinalMethods)].
  
build_inherited_methods(MethodsDict) ->
  Methods = compile_inherited_methods(MethodsDict),
  lists:foldl(
    fun({function, _, Name, _, _} = Function, Dict) ->
      {method, Ancestor, _} = dict:fetch(Name, MethodsDict),
      dict:store(Name, {method, Ancestor, Function}, Dict)
    end,
    dict:new(),
    Methods
  ).
  
compile_inherited_methods(MethodsDict) ->
  Methods = [Method || {_, {_, _, Method}} <- dict:to_list(MethodsDict)],
  Class = {class, 1, {constant, 1, 'base_class'}, Methods},
  Passes = [Pass || Pass <- reia_compiler:default_passes(), Pass /= dynamic],
  [{class, _, _, _, Functions}] = reia_compiler:compile([Class], Passes),
  Functions.
    
%% Compile a Reia class to an Erlang module
ast({class, Line, Name, _Ancestor, Methods}) ->
  Functions2 = build_functions(Name, Methods),
  {module, Line, Name, Functions2};
ast(_) ->
  {error, "invalid class"}.
 
%% Create an instance of a given class, passing the arguments on to its
%% initialize function
inst(Class, Arguments, Block) ->
  Obj = Class:spawn_link(),
  call(Obj, {'initialize', Arguments, Block}),
  Obj.
 
%% Call a method on a Reia object at the given Pid
call({object, {Pid, _Class}}, Request) ->
  call(Pid, Request);
call(Pid, {_Method, _Arguments, _Block} = Request) when is_pid(Pid) ->
  case gen_server:call(Pid, Request) of
    {ok, Value} -> Value;
    {error, Error} -> throw(Error)
  end.
 
%% Process incoming methods and build the functions for the resulting module
build_functions(Module, Methods) ->
  lists:flatten([
    start_functions(Module),
    default_functions(),
    method_functions(Methods)
  ]).
  
%% Build a dispatch_method function and functions for each of the mangled methods
method_functions(Methods) ->
  % Decompose the function clauses for methods into handle_call clauses
  {Clauses, Functions} = process_methods(Methods),
  [build_method_dispatch_function(Clauses)|Functions].
 
%% Process methods into a list of clauses for dispatch_method and functions
%% with mangled names for each method
process_methods(Methods) ->
  {NewClauses, NewFunctions} = lists:foldl(fun(Method, {Clauses, Functions}) ->
    {Name, Function} = extract_mangled_name_and_function(Method),
    {Clause, Function2} = build_dispatcher_clause_and_function(Function, Name),
    {[Clause|Clauses], [Function2|Functions]}
  end, {[],[]}, Methods),
  {lists:reverse(NewClauses), lists:reverse(NewFunctions)}.
  
%% Extract a method into its dispatch_method clause and mangled form
extract_mangled_name_and_function({method, Ancestor, {function, _, Name, _, _} = Function}) ->
  {reia_mangle:method(Ancestor, Name), Function};
extract_mangled_name_and_function({function, _, Name, _, _} = Function) ->
  {reia_mangle:method(Name), Function}.
  
%% Construct the new function and dispatcher clause
build_dispatcher_clause_and_function({function, Line, Name, _Arity, Clauses}, MangledName) ->
  DispatcherClause = dispatcher_clause(Name, MangledName, Line),
  Function = {function, Line, MangledName, 4,
    [process_method_clause(Clause) || Clause <- Clauses]
  },
  {DispatcherClause, Function}.
 
%% Generate a clause for dispatch_method which thunks from a real method name
%% to the given mangled name
dispatcher_clause(RealName, MangledName, Line) ->
  {clause, Line, [
    {tuple, Line, [{atom, Line, RealName}, {var, Line, 'arguments'}, {var, Line, 'block'}]},
    {var, Line, 'caller'},
    {var, Line, 'instance_variables'}
  ], [], [
    {call, Line, {atom, Line, MangledName}, [
      {var, Line, 'arguments'},
      {var, Line, 'block'},
      {var, Line, 'caller'},
      {var, Line, 'instance_variables'}
    ]}
  ]}.
       
%% Build a clause for dispatch_method from the original clauses for a method
process_method_clause({clause, Line, [], [], Expressions}) ->
  process_method_clause({clause, Line, [{tuple, Line, []}, nil], [], Expressions});
process_method_clause({clause, Line, [{tuple, _, Arguments}, Block], [], Expressions}) ->
  {clause, Line, [
    argument_list_cons(Arguments, Line),
    Block,
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
  CatchallFunc = "dispatch_method({Method, Args, Block}, Caller, State) -> dispatch_method({'_', [Method, Args], Block}, Caller, State).",
  {function, _, _, _, CatchallClause} = parse_function(CatchallFunc),
  {function, 1, dispatch_method, 3, Clauses ++ CatchallClause}.
 
%% These functions are required for the generated modules to implement the
%% gen_server behavior
default_functions() ->
  [parse_function(Function) || Function <- [
    "init([]) -> {ok, dict:new()}.",
    "handle_call(Request, From, State) -> try dispatch_method(Request, From, State) catch throw:Error -> {reply, {error, Error}, State} end.",
    "handle_cast(_Msg, State) -> {noreply, State}.",
    "handle_info(Message, State) -> {reply, _, NewState} = dispatch_method({'handle_message', [Message], nil}, unknown, State), {noreply, NewState}.",
    "terminate(_Reason, _State) -> ok.",
    "code_change(_OldVsn, State, _Extra) -> {ok, State}."
  ]].
  
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