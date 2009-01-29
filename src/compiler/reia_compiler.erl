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
  dynamic/2, 
  static/1,
  nonce/0
]).

default_passes() ->
  [branches, ivars, ssa, methods, r2e, dynamic].

compile(Expressions) ->
  compile(Expressions, default_passes()).

compile(Expressions, Passes) ->
  {NewExpressions, FinalPass} = walk_passes(Expressions, Passes),
  case FinalPass of
    static -> static(NewExpressions);
    dynamic -> dynamic(Expressions, NewExpressions);
    void -> NewExpressions
  end.
  
walk_passes(Expressions, [static]) ->
  {Expressions, static};
walk_passes(Expressions, [dynamic]) ->
  {Expressions, dynamic};
walk_passes(Expressions, [Pass]) ->
  {pass(Pass, Expressions), void};
walk_passes(Expressions, [Pass|Passes]) ->
  walk_passes(pass(Pass, Expressions), Passes).
  
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
dynamic(OrigExpressions, ErlExpressions) ->
  [dynamic_expression(Expression, OrigExpressions) || Expression <- ErlExpressions].
  
%% Pass dynamic module declarations to reia_module:build/1
dynamic_expression({module, Line, _Constant, _Functions} = Module, _OrigExpressions) ->
  %% Convert the module to an Erlang forms representation to pass as a call to reia_module
  Arg = erl_syntax:revert(erl_syntax:abstract(Module)),
  {call, Line, {remote, Line, {atom, Line, reia_module}, {atom, Line, build}}, [Arg]};
  
%% Pass dynamic class declarations to reia_class:build/1
dynamic_expression({class, Line, Name, _Ancestor, _Functions} = Class, OrigExpressions) ->
  %% Convert the class to an Erlang forms representation to pass as a call to reia_class
  CompiledForms = erl_syntax:revert(erl_syntax:abstract(Class)),
  
  %% Convert the original class to an Erlang forms representation
  OrigClass = find_original_class(Name, OrigExpressions),
  OrigForms = erl_syntax:revert(erl_syntax:abstract(OrigClass)),
  
  {call, Line, {remote, Line, {atom, Line, reia_class}, {atom, Line, build}}, [CompiledForms, OrigForms]};
    
%% Leave other toplevel expressions alone
dynamic_expression(Expression, _OrigExpressions) ->
  Expression.
  
find_original_class(Name, Expressions) ->
  [Class] = lists:filter(fun(Expr) ->
    case Expr of
      {class, _, {constant, _, Name}, _}    -> true;
      {class, _, {constant, _, Name}, _, _} -> true;
      _                                     -> false
    end
  end, Expressions),
  Class.
  
%% Static module declarations
static([{module, Line, Name, Functions}]) ->
  [{attribute, Line, module, Name}|Functions];
static([{class, _Line, _Name, _Ancestor, _Functions} = Class]) ->
  Module = reia_class:ast(Class),
  static([Module]);
static(_) ->
  throw({error, "Statically compiled modules must contain one and only one module or class declaration"}).

%% Generate a single-use variable name
nonce() ->
  lists:flatten([io_lib:format("~.16b",[N]) || <<N>> <= erlang:md5(term_to_binary(make_ref()))]).