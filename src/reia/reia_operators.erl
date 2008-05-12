-module(reia_operators).
-export([ast/2, ast/3]).

ast('-' = Op, Ast) ->
  {op, 1, Op, Ast}.
%ast('~' = Op, Ast) ->
%  erl_syntax:prefix_expr(erl_syntax:operator('bnot'), Ast).
  
ast('*' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('**', Ast1, Ast2) ->
  {call,1,{remote,1,{atom,1,math},{atom,1,pow}},[Ast1, Ast2]};
ast('/' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('+' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2};
ast('-' = Op, Ast1, Ast2) ->
  {op, 1, Op, Ast1, Ast2}.
