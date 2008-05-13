-module(reia_operators).
-export([ast/3, ast/4]).

ast('-' = Op, Line, Ast) ->
  {op, Line, Op, Ast}.
%ast('~' = Op, Ast) ->
%  erl_syntax:prefix_expr(erl_syntax:operator('bnot'), Ast).
  
ast('*' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('**', Line, Ast1, Ast2) ->
  {call, Line, {remote, 1, {atom, 1, math},{atom, 1, pow}}, [Ast1, Ast2]};
ast('/' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('+' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('-' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2}.
