%
% reia_operators: Compiles Reia operator abstract syntax to Erlang forms
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_operators).
-export([forms/3, forms/4]).

%% Unary operators
forms('-' = Op, Line, Ast) ->
  {op, Line, Op, Ast};
forms('+' = Op, Line, Ast) ->
  {op, Line, Op, Ast};
forms('not' = Op, Line, Ast) ->
  {op, Line, Op, Ast}.

%% Exponentation
forms('**', Line, Ast1, Ast2) ->
  {call, Line, {remote, Line, {atom, Line, 'Numeric'}, {atom, Line, pow}}, [Ast1, Ast2]};

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
  
%% Boolean operators
forms('and', Line, Ast1, Ast2) ->
  {op, Line, 'andalso', Ast1, Ast2};
forms('or', Line, Ast1, Ast2) ->
  {op, Line, 'orelse', Ast1, Ast2};
  
%% Comparison operators
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
  {op, Line, '=<', Ast1, Ast2};
forms('===', Line, Ast1, Ast2) ->
  {'try', Line,
    [{match, Line, Ast1, Ast2}, {atom, Line, true}],
    [],
    [{clause, Line,
             [{tuple, Line,
                     [{atom, Line,error},
                      {tuple, Line, [{atom, Line, badmatch}, {var, Line, '_'}]},
                      {var, Line, '_'}]}],
             [],
             [{atom, Line, false}]}],
  []}.