%
% reia_branches: Convert if and case statements to a more Erlangy structure
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_branches).
-export([ast/1, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, void, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.
  
%% If statements
transform(_, {'if', Line, Expression, Statements, {else_clause, ElseLine, ElseStatements}}) ->
  Node = {'case', Line, Expression,
    [
      {clause, ElseLine, {atom, Line, false}, ElseStatements},
      {clause, ElseLine, {atom, Line, nil}, ElseStatements},
      {clause, Line, {identifier, Line, '_'}, Statements}
    ]
  },
  {walk, void, Node};
  
%% Case statements
transform(_, {'case', Line, Expression, Clauses}) ->
  {'case', Line, Expression, Clauses ++ [{clause, Line, [{var, Line, '_'}], [{atom, Line, nil}]}]};
  
% Walk unrecognized nodes without transforming them
transform(_, Node) ->
  {walk, void, Node}.