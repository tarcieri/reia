%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([forms/1, list_to_forms/2]).

%% Module declarations
forms({module, Line, {constant, _, Name}, Functions}) ->
  {module, Line, Name, group_clauses([forms(Function) || Function <- Functions])};

%% Class declarations
forms({class, Line, {constant, _, Name}, Functions}) ->
  {class, Line, Name, group_clauses([forms(Function) || Function <- Functions])};
  
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
  {var, Line, Name};

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
  Result = {tuple, Line, [
    {atom, Line, list},
    {tuple, Line, [
      list_to_forms([], Line),
      list_to_forms(Elements, Line)
    ]}
  ]},
  Result;
  
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
  {'fun', Line, 
    {clauses,[{clause, Line, [forms(Arg) || Arg <- Args], [], [forms(Statement) || Statement <- Statements]}]}
  };
  
%% Ranges
forms({range, Line, Begin, End}) ->
  {tuple, Line, [
    {atom, Line, list},
    {tuple, Line, [
      list_to_forms([], Line),
      {call, Line,
        {remote, Line, {atom, Line, lists}, {atom, Line, seq}},
        [forms(Begin), forms(End)]
      }
    ]}
  ]};
  
%% Operators
forms({op, Line, Op, In}) ->
  reia_operators:forms(Op, Line, forms(In));
forms({op, Line, Op, In1, In2}) ->
  reia_operators:forms(Op, Line, forms(In1), forms(In2));
  
%% Reia function calls
forms({funcall, Line, {identifier, _, Method}, Arguments}) ->
  {call, Line, {atom, Line, Method}, [forms(Argument) || Argument <- Arguments]};
forms({funcall, Line, {var, _, Var}, Arguments}) ->
  {call, Line, forms({identifier, Line, Var}), [forms(Argument) || Argument <- Arguments]};
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
  
%% Embedded literal Erlang function calls (emitted by earlier compiler passes)
forms({call, Line, Function, Arguments}) ->
  {call, Line, Function, [forms(Argument) || Argument <- Arguments]};
    
%% Begin/end blocks
forms({block, Line, Expressions}) ->
  {block, Line, [forms(Expression) || Expression <- Expressions]};
  
%% Case expressions
forms({'case', Line, Expression, Clauses}) ->
  {'case', Line, forms(Expression), [forms(Clause) || Clause <- Clauses]};
    
forms({clause, Line, Expression, Statements}) ->
  {clause, Line, [forms(Expression)], [], [forms(Statement) || Statement <- Statements]};
  
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
  };
  
%% Erlang forms generated by earlier stages of the compiler which should be
%% used verbatim
forms({erl_forms, _Line, Forms}) ->
  Forms.
    
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
  
list_to_forms([], Line) ->
  {nil, Line};
list_to_forms([Element|Rest], Line) ->
  {cons, Line, forms(Element), list_to_forms(Rest, Line)}.
  
%% Generate AST representing dict elements
dict_elements_forms([], Line) ->
  {nil, Line};
dict_elements_forms([{Key,Value}|Rest], Line) ->
  {cons, Line, {tuple, Line, [forms(Key), forms(Value)]}, dict_elements_forms(Rest, Line)}.