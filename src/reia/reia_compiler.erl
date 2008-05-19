-module(reia_compiler).
-export([compile/1]).

compile(Expressions) ->
  compile(Expressions, []).
  
compile([], Output) -> lists:reverse(Output);
compile([Expression|Rest], Output) ->
  NewExpression = ast(Expression),
  compile(Rest, [NewExpression|Output]).

% Primitives
ast({nil, Line}) ->
  {atom, Line, nil};
ast({true, Line}) ->
  {atom, Line, true};
ast({false, Line}) ->
  {atom, Line, false};
  
% Numerical types
ast(Ast = {integer, _, _}) ->
  Ast;
ast(Ast = {float, _, _}) ->
  Ast;
  
% Atoms
ast(Ast = {atom, _, _}) ->
  Ast;
  
% Strings and regular expressions
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

% Lists
ast({cons, Line, In1, Nil = {nil, _}}) ->
  {cons, Line, ast(In1), Nil};
ast({cons, Line, In1, In2}) ->
  {cons, Line, ast(In1), ast(In2)};
  
% Tuples
ast({tuple, Line, Elements}) ->
  {tuple, Line, [
    {atom, Line, tuple},
    {tuple, Line, lists:map(fun(Element) -> ast(Element) end, Elements)}
  ]};
  
% Operators
ast({op, {Op, Line}, In}) ->
  reia_operators:ast(Op, Line, ast(In));
ast({op, {Op, Line}, In1, In2}) ->
  reia_operators:ast(Op, Line, ast(In1), ast(In2));
  
% Erlang function calls
ast({erl_funcall, Line, {identifier, _, Module}, {identifier, _, Function}, Arguments}) ->
  {call,Line,
    {remote, Line, {atom, Line, reia_erl}, {atom, Line, erl_funcall}},
    [{atom, Line, Module}, {atom, Line, Function}, list_to_ast(Arguments, Line)]
  }.

% Convert a list to its AST representation
list_to_ast([], Line) ->
  {nil,Line};
list_to_ast([Term|Rest], Line) ->
  {cons,Line,ast(Term),list_to_ast(Rest,Line)}.