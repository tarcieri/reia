%
% reia_compiler: Compiles Reia abstract syntax to Erlang abstract forms
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([
  default_passes/0, 
  compile/1, 
  compile/2,
  ivars/1,
  branches/1,
  ssa/1,
  methods/1,
  r2e/1, 
  dynamic/1, 
  static/1
]).

default_passes() ->
  [branches, ivars, ssa, methods, r2e, dynamic].

compile(Expressions) ->
  compile(Expressions, default_passes()).

compile(Expressions, []) ->
  Expressions;
compile(Expressions, [Pass|Passes]) ->
  compile(pass(Pass, Expressions), Passes).
  
pass({ssa, Binding}, Expressions) ->
  ssa(Expressions, Binding);
pass(Pass, Expressions) ->
  ?MODULE:Pass(Expressions).

%% Convert if statements to case statements and add default return values
branches(Expressions) ->
  reia_branches:ast(Expressions).

%% Convert Reia instance variables into internal dict representation
ivars(Expressions) ->
  reia_ivars:ast(Expressions).

%% Convert Reia forms into SSA form
ssa(Expressions) ->
  reia_ssa:ast(Expressions).
  
ssa(Expressions, Binding) ->
  Variables = [Name || {Name, _} <- Binding],
  reia_ssa:ast(Expressions, Variables).
  
%% Handle state passing to local methods
methods(Expressions) ->
  reia_methods:ast(Expressions).

%% Convert Reia forms to Erlang forms
r2e(Expressions) ->
  r2e(Expressions, []).
    
r2e([], Output) -> lists:reverse(Output);
r2e([Expression|Rest], Output) ->
  case reia_r2e:forms(Expression) of
    Expressions when is_list(Expressions) ->
      r2e(Rest, lists:reverse(Expressions) ++ Output);
    NewExpression ->
      r2e(Rest, [NewExpression|Output])
  end.
  
%% Dynamic evaluation (supporting multiple module declarations)
dynamic(Expressions) ->
  [dynamic_expression(Expression) || Expression <- Expressions].
  
%% Pass dynamic module declarations to reia_module:build/1
dynamic_expression({module, Line, _Constant, _Functions} = Module) ->
  %% Convert the module to an Erlang forms representation to pass as a call to reia_module
  Arg = erl_syntax:revert(erl_syntax:abstract(Module)),
  {call, Line, {remote, Line, {atom, Line, reia_module}, {atom, Line, build}}, [Arg]};
  
%% Pass dynamic class declarations to reia_class:build/1
dynamic_expression({class, Line, _Constant, _Functions} = Class) ->
  %% Convert the class to an Erlang forms representation to pass as a call to reia_class
  Arg = erl_syntax:revert(erl_syntax:abstract(Class)),
  {call, Line, {remote, Line, {atom, Line, reia_class}, {atom, Line, build}}, [Arg]};
    
%% Leave other toplevel expressions alone
dynamic_expression(Expression) ->
  Expression.
  
%% Static module declarations
static([{module, Line, Name, Functions}]) ->
  [{attribute, Line, module, Name}|Functions];
static(_) ->
  throw({error, "Statically compiled modules must contain one and only one module declaration"}).