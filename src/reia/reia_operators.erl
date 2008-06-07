-module(reia_operators).
-export([forms/3, forms/4]).

%% Unary operators
forms('-' = Op, Line, Ast) ->
  {op, Line, Op, Ast}.

%% Exponentation
forms('**', Line, Ast1, Ast2) ->
  {call, Line, {remote, 1, {atom, 1, reia_numeric},{atom, 1, pow}}, [Ast1, Ast2]};

%% Multiplication  
forms('*' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('/' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('%', Line, Ast1, Ast2) ->
  {op, Line, 'rem', Ast1, Ast2};
  
%% Addition
forms('+' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('-' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
  
%% Comparisons
forms('==' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('!=', Line, Ast1, Ast2) ->
  {op, Line, '/=', Ast1, Ast2};
forms('<' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>=' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('<=', Line, Ast1, Ast2) ->
  {op, Line, '=<', Ast1, Ast2}.

