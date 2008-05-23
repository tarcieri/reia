-module(reia_compiler).
-export([compile/1, ast/1]).

compile(Expressions) ->
  compile(Expressions, []).
  
compile([], Output) -> lists:reverse(Output);
compile([Expression|Rest], Output) ->
  NewExpression = ast(Expression),
  compile(Rest, [NewExpression|Output]).
  
%% Pattern matching
ast({match, Line, In1, In2}) ->
  {match, Line, ast(In1), ast(In2)};
  
%% Variables
ast({identifier, Line, Name}) ->
  [FirstLetter|Rest] = atom_to_list(Name),
  {var, Line, list_to_atom([string:to_upper(FirstLetter)|Rest])};

%% Primitives
ast({nil, Line}) ->
  {atom, Line, nil};
ast({true, Line}) ->
  {atom, Line, true};
ast({false, Line}) ->
  {atom, Line, false};
  
%% Numerical types
ast(Ast = {integer, _, _}) ->
  Ast;
ast(Ast = {float, _, _}) ->
  Ast;
  
%% Atoms
ast(Ast = {atom, _, _}) ->
  Ast;
  
%% Strings and regular expressions
ast(Ast = {string, Line, String}) ->
  {tuple, Line, [
    {atom, Line, string},
    {bin, Line, [{bin_element, Line, {string, Line, String}, default, default}]}
  ]};
ast({regexp, Line, Pattern}) ->
  {tuple, Line, [
    {atom, Line, regexp}, 
    {bin, Line, [{bin_element, Line, {string, Line, Pattern}, default, default}]}
  ]};

%% Lists
ast({list, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, list},
    {tuple, Line, [
      list_to_ast(Elements, Line),
      {atom, Line, normal}
    ]}
  ]};
  
%% Tuples
ast({tuple, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, tuple},
    {tuple, Line, lists:map(fun(Element) -> ast(Element) end, Elements)}
  ]};
  
%% Dicts
ast({dict, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, dict},
    {call, Line,
      {remote, Line, {atom, Line, dict}, {atom, Line, from_list}},
      [dict_elements_ast(Elements, Line)]
    }
  ]};
  
%% Lambdas
ast({lambda, Line, Args, Statements}) ->
  {tuple, Line, [
    {atom, Line, lambda},
    {'fun', Line, {clauses,[{clause, Line,
      lists:map(fun reia_compiler:ast/1, Args),
      [],
      lists:map(fun reia_compiler:ast/1, Statements)
    }]}}
  ]};
  
%% Operators
ast({op, {Op, Line}, In}) ->
  reia_operators:ast(Op, Line, ast(In));
ast({op, {Op, Line}, In1, In2}) ->
  reia_operators:ast(Op, Line, ast(In1), ast(In2));
  
%% Reia function calls
ast({funcall, Line, Receiver, {identifier, _, Method}, Arguments}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, funcall}},
    [ast(Receiver), {atom, Line, Method}, list_to_ast(Arguments, Line)]
  };
  
%% Reia function calls with pseudo-blocks
ast({funcall, Line, Receiver, {identifier, _, Method}, Arguments, Block}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, funcall}},
    [ast(Receiver), {atom, Line, Method}, list_to_ast(Arguments, Line), ast(Block)]
  };
    
%% Erlang function calls
ast({erl_funcall, Line, {identifier, _, Module}, {identifier, _, Function}, Arguments}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_erl}, {atom, Line, erl_funcall}},
    [{atom, Line, Module}, {atom, Line, Function}, list_to_ast(Arguments, Line)]
  }.

%% Generate AST representing lists
list_to_ast([], Line) ->
  {nil, Line};
list_to_ast([Element|Rest], Line) ->
  {cons, Line, ast(Element), list_to_ast(Rest, Line)}.
  
%% Generate AST representing dict elements
dict_elements_ast([], Line) ->
  {nil, Line};
dict_elements_ast([{Key,Value}|Rest], Line) ->
  {cons, Line, {tuple, Line, [ast(Key), ast(Value)]}, dict_elements_ast(Rest, Line)}.