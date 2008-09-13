%
% reia_compiler: Compiles Reia abstract syntax to Erlang abstract forms
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([compile/1, compile/2, ssa/1, r2e/1, dynamic/1, static/1, forms/1]).

compile(Expressions) ->
  compile(Expressions, [ssa, r2e, dynamic]).

compile(Expressions, []) ->
  Expressions;
compile(Expressions, [Pass|Passes]) ->
  compile(pass(Pass, Expressions), Passes).
  
pass({ssa, Binding}, Expressions) ->
  ssa(Expressions, Binding);
pass(Pass, Expressions) ->
  ?MODULE:Pass(Expressions).

%% Convert Reia forms into SSA form
ssa(Expressions) ->
  reia_ssa:ast(Expressions).
  
ssa(Expressions, Binding) ->
  Variables = [Name || {Name, _} <- Binding],
  reia_ssa:ast(Expressions, Variables).

%% Convert Reia forms to Erlang forms
r2e(Expressions) ->
  r2e(Expressions, []).
    
r2e([], Output) -> lists:reverse(Output);
r2e([Expression|Rest], Output) ->
  case forms(Expression) of
    Expressions when is_list(Expressions) ->
      r2e(Rest, lists:reverse(Expressions) ++ Output);
    NewExpression ->
      r2e(Rest, [NewExpression|Output])
  end.
  
%% Dynamic evaluation (supporting multiple module declarations)
dynamic(Expressions) ->
  [dynamic_expression(Expression) || Expression <- Expressions].
  
%% Pass dynamic module declarations to reia_module:build/1
dynamic_expression(Module = {module, Line, _Constant, _Functions}) ->
  %% Convert the module to an Erlang forms representation to pass as a call to reia_module
  Arg = erl_syntax:revert(erl_syntax:abstract(Module)),
  {call, Line, {remote, Line, {atom, Line, reia_module}, {atom, Line, build}}, [Arg]};
  
%% Leave other toplevel expressions alone
dynamic_expression(Expression) ->
  Expression.
  
%% Static module declarations
static([{module, Line, Name, Functions}]) ->
  [{attribute, Line, module, Name}|Functions];
static(_) ->
  throw({error, "Statically compiled modules must contain one and only one module declaration"}).

%% Module declarations
forms({module, Line, {constant, _, Name}, Functions}) ->
  {module, Line, Name, group_clauses([forms(Function) || Function <- Functions])};
  
%% Functions
forms({function, Line, {identifier, _, Name}, Arguments, Expressions}) ->
  {function, Line, Name, erlang:length(Arguments), [{clause, Line, 
    [forms(Argument) || Argument <- Arguments],
    [],
    [forms(Expression) || Expression <- Expressions]
  }]};
  
%% Pattern matching
forms({match, Line, In1, In2}) ->
  {match, Line, forms(In1), forms(In2)};
  
%% Variables
forms({identifier, Line, Name}) ->
  [FirstLetter|Rest] = atom_to_list(Name),
  {var, Line, list_to_atom([string:to_upper(FirstLetter)|Rest])};

%% Primitives
forms({nil, Line}) ->
  {atom, Line, nil};
forms({true, Line}) ->
  {atom, Line, true};
forms({false, Line}) ->
  {atom, Line, false};
  
%% Numerical types
forms(Ast = {integer, _, _}) ->
  Ast;
forms(Ast = {float, _, _}) ->
  Ast;
  
%% Atoms
forms(Ast = {atom, _, _}) ->
  Ast;
  
%% Constants
forms({constant, Line, Name}) ->
  {tuple, Line, [{atom, Line, constant}, {atom, Line, Name}]};
  
%% Strings
forms({string, Line, String}) ->
  {tuple, Line, [
    {atom, Line, string},
    {bin, Line, [{bin_element, Line, {string, Line, String}, default, default}]}
  ]};
  
%% Regexes
forms({regexp, Line, Pattern}) ->
  {tuple, Line, [
    {atom, Line, regexp}, 
    {bin, Line, [{bin_element, Line, {string, Line, Pattern}, default, default}]}
  ]};

%% Lists
forms({list, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, list},
    {tuple, Line, [
      list_to_forms(Elements, Line),
      list_to_forms([], Line)
    ]}
  ]};
  
%% Tuples
forms({tuple, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, tuple},
    {tuple, Line, [forms(Element) || Element <- Elements]}
  ]};
  
%% Dicts
forms({dict, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, dict},
    {call, Line,
      {remote, Line, {atom, Line, dict}, {atom, Line, from_list}},
      [dict_elements_forms(Elements, Line)]
    }
  ]};
  
%% Binaries
forms({binary, Line, String = {string, _, _}}) ->
  {bin, Line, [{bin_element, Line, String, default, default}]};
  
%% Lambdas
forms({lambda, Line, Args, Statements}) ->
  {tuple, Line, [
    {atom, Line, lambda},
    {'fun', Line, {clauses,[{clause, Line,
      [forms(Arg) || Arg <- Args],
      [],
      [forms(Statement) || Statement <- Statements]
    }]}}
  ]};
  
%% Ranges
forms({range, Line, Begin, End}) ->
  {tuple, Line, [
    {atom, Line, list},
    {tuple, Line, [
      {call, Line,
        {remote, Line, {atom, Line, lists}, {atom, Line, seq}},
        [forms(Begin), forms(End)]
      },
      list_to_forms([], Line)
    ]}
  ]};
  
%% Operators
forms({op, {Op, Line}, In}) ->
  reia_operators:forms(Op, Line, forms(In));
forms({op, {Op, Line}, In1, In2}) ->
  reia_operators:forms(Op, Line, forms(In1), forms(In2));
  
%% Reia function calls
forms({funcall, Line, {identifier, _, Method}, Arguments}) ->
  {call, Line, {atom, Line, Method}, [forms(Argument) || Argument <- Arguments]};
forms({funcall, Line, Receiver, {identifier, _, Method}, Arguments}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, funcall}},
    [forms(Receiver), {atom, Line, Method}, list_to_forms(Arguments, Line)]
  };
  
%% Reia function calls with pseudo-blocks
forms({funcall, Line, Receiver, {identifier, _, Method}, Arguments, Block}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, funcall}},
    [forms(Receiver), {atom, Line, Method}, list_to_forms(Arguments, Line), forms(Block)]
  };
    
%% Erlang function calls
forms({erl_funcall, Line, {identifier, _, Module}, {identifier, _, Function}, Arguments}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_erl}, {atom, Line, erl_funcall}},
    [{atom, Line, Module}, {atom, Line, Function}, list_to_forms(Arguments, Line)]
  };
  
%% Case expressions
forms({'case', Line, Expression, Clauses}) ->
  forms({'case', Line, Expression, Clauses, {else_clause, Line, [{atom, Line, nil}]}});
forms({'case', Line, Expression, Clauses, ElseClause}) ->
  {'case', Line, forms(Expression), [forms(Clause) || Clause <- Clauses ++ [ElseClause]]};
    
forms({clause, Line, Expression, Statements}) ->
  {clause, Line, [forms(Expression)], [], [forms(Statement) || Statement <- Statements]};
  
forms({else_clause, Line, Statements}) ->
  {clause, Line, [{var, Line, '_'}], [], [forms(Statement) || Statement <- Statements]};
  
%% If statements
forms({'if', Line, Expression, Statements}) ->
  forms({'if', Line, Expression, Statements, {else_clause, Line, [{atom, Line, nil}]}});

forms({'if', Line, Expression, Statements, {else_clause, ElseLine, ElseStatements}}) ->
  ElseForms = [forms(Statement) || Statement <- ElseStatements],
  {'case', Line, forms(Expression),
    [
      {clause, ElseLine, [{atom, Line, false}], [], ElseForms},
      {clause, ElseLine, [{atom, Line, nil}],   [], ElseForms},
      {clause, Line, [{var, Line, '_'}], [], [forms(Statement) || Statement <- Statements]}
    ]
  };
  
%% Try statements
forms({'try', Line, Statements, CatchClauses}) ->
  {'try', Line, 
    [forms(Statement) || Statement <- Statements],
    [],
    [forms(CatchClause) || CatchClause <- CatchClauses],
    []
  };

%% Catch clauses
forms({'catch', Line, Pattern, Statements}) ->
  {clause, Line, 
    [{tuple, Line, [{var, Line, 'ExceptionType'}, {var, Line, 'ExceptionReason'}, {var, Line, '_Lint'}]}], 
    [],
    [
      {match, Line, forms(Pattern), {tuple, Line, [
        {atom, Line, exception},
        {tuple, Line, [
          {var, Line, 'ExceptionType'},
          {var, Line, 'ExceptionReason'}
        ]}
      ]}}|[forms(Statement) || Statement <- Statements]
    ]
  };
  
%% Throw statements
forms({throw, Line, Term}) ->
  {call, Line, {atom, Line, throw}, [forms(Term)]};
  
%% List comprehensions
forms({'lc', Line, Transform, Expressions}) ->
  {lc, Line, forms(Transform), [forms(Expression) || Expression <- Expressions]};
  
%% List comprehension generators
forms({'generate', Line, Term, List}) ->
  {generate, Line, forms(Term), 
    {call, Line, {remote, Line, {atom, Line, reia_list}, {atom, Line, to_erl}}, [forms(List)]}
  }.
    
%% Group clauses of functions with the same name and arity
group_clauses(Functions) ->
  group_clauses(Functions, dict:new()).

group_clauses([], Dict) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses)} || {{Name, Arity},{Line, Clauses}} <- dict:to_list(Dict)];
group_clauses([Function|Rest], Dict) ->
  {function, Line, Name, Arity, [Clause]} = Function,
  case dict:find({Name, Arity}, Dict) of
    {ok, {Line2, Clauses}} ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line2, [Clause|Clauses]}, Dict));
    error ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line, [Clause]}, Dict))
  end.

%% Generate AST representing lists
list_to_forms([], Line) ->
  {nil, Line};
list_to_forms([Element|Rest], Line) ->
  {cons, Line, forms(Element), list_to_forms(Rest, Line)}.
  
%% Generate AST representing dict elements
dict_elements_forms([], Line) ->
  {nil, Line};
dict_elements_forms([{Key,Value}|Rest], Line) ->
  {cons, Line, {tuple, Line, [forms(Key), forms(Value)]}, dict_elements_forms(Rest, Line)}.