-module(reia_operators).
-export([ast/3, ast/4]).

%% Unary operators
ast('-' = Op, Line, Ast) ->
  {op, Line, Op, Ast}.

%% Exponentation
ast('**', Line, Ast1, Ast2) ->
  {call, Line, {remote, 1, {atom, 1, reia_numeric},{atom, 1, pow}}, [Ast1, Ast2]};

%% Multiplication  
ast('*' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('/' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
  
%% Addition
ast('+' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('-' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
  
%% Comparisons
ast('==' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('!=', Line, Ast1, Ast2) ->
  {op, Line, '/=', Ast1, Ast2};
ast('<' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('>' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('>=' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
ast('<=', Line, Ast1, Ast2) ->
  {op, Line, '=<', Ast1, Ast2}.

