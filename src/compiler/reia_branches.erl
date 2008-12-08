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
      {clause, ElseLine, {false, Line}, ElseStatements},
      {clause, ElseLine, {nil, Line},   ElseStatements},
      {clause, Line, {identifier, Line, '_'}, Statements}
    ]
  },
  {walk, void, Node};
  
%% Case statements
transform(_, {'case', Line, Expression, Clauses} = OrigNode) ->
  Node = case lists:any(fun is_catchall_clause/1, Clauses) of
    true ->
      OrigNode;
    false ->
      {'case', Line, Expression, 
        Clauses ++ [{clause, Line, {identifier, Line, '_'}, [{nil, Line}]
      }]}
  end,
  {walk, void, Node};
  
% Walk unrecognized nodes without transforming them
transform(_, Node) ->
  {walk, void, Node}.
    
% Is the given clause a catch-all clause?
is_catchall_clause({clause, _Line, Pattern, _Expressions}) ->
  catchall_pattern(Pattern).
  
% Is the given pattern a catch-all?
catchall_pattern({match, _Line, Pattern1, Pattern2}) ->
  case catchall_pattern(Pattern1) of
    true -> true;
    false -> catchall_pattern(Pattern2)
  end;
catchall_pattern({identifier, _, _}) ->
  true;
catchall_pattern(_) ->
  false.