-module(reia_operators).
-export([ast/2, ast/3]).

ast('-' = Op, Ast) ->
  {op, 1, Op, {integer, 1, 0}, Ast}.
%ast('~' = Op, Ast) ->
%  erl_syntax:prefix_expr(erl_syntax:operator('bnot'), Ast).
  
ast('*' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('/' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('+' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('-' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2}.
