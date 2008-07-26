%
% reia_compiler: Compiles Reia abstract syntax to Erlang abstract forms
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_compiler).
-export([compile/1, forms/1]).

compile(Expressions) ->
  compile(Expressions, []).
  
compile([], Output) -> lists:reverse(Output);
compile([Expression|Rest], Output) ->
  case forms(Expression) of
    Expressions when is_list(Expressions) ->
      compile(Rest, lists:reverse(Expressions) ++ Output);
    NewExpression ->
      compile(Rest, [NewExpression|Output])
  end.
  
%% Module declarations
forms({module, Line, {constant, _, Constant}, Functions}) ->
  Name = constant_to_module_name(Constant),
  Module = {attribute, Line, module, Name},
  NewFunctions = group_clauses([forms(Function) || Function <- Functions]),
  [Module|NewFunctions];
  
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
  
%% Strings and regular expressions
forms({string, Line, String}) ->
  {tuple, Line, [
    {atom, Line, string},
    {bin, Line, [{bin_element, Line, {string, Line, String}, default, default}]}
  ]};
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
      {atom, Line, normal}
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
      {atom, Line, normal}
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
  }.
  
%% Generate a module name from a module declaration
constant_to_module_name(Constant) ->
  String = atom_to_list(Constant),
  {match, Matches} = regexp:matches(String, "[A-Z][a-z]+"),
  Fragments = [string:to_lower(lists:sublist(String, Start, Length)) || {Start, Length} <- Matches],
  [_|NewName] = lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end, lists:duplicate(length(Fragments), "_"), Fragments)),
  list_to_atom(NewName).
  
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