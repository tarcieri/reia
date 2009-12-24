%
% reia_conditionals: Convert if and case statements to a more Erlangy structure
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_conditionals).
-export([transform/2]).
-include("reia_nodes.hrl").

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform/1, Exprs).
  
% If statements
transform(#'if'{clauses=Clauses}) ->
  if_to_case(Clauses);
  
% Case statements
transform(#'case'{line=Line, clauses=Clauses} = Node) ->
  Node = case lists:any(fun is_catchall_clause/1, Clauses) of
    true ->
      Node;
    false ->
      Catchall = #clause{
        line=Line, 
        patterns=[#identifier{line=Line, name='_'}], 
        exprs=[#nil{line=Line}]
      },
      Node#'case'{clauses=Clauses ++ [Catchall]} 
  end,
  reia_syntax:map_subtrees(fun transform/1, Node);
  
% Walk unrecognized nodes without transforming them
transform(Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node).

% Convert if statements to case statements
if_to_case(Clauses) ->
  if_to_case(lists:reverse(Clauses), #nil{line=1}).
  
if_to_case([], Output) ->
  Output;
if_to_case([Clause|Clauses], Output) ->
  % FIXME: rather than duplicating the output expression body, it would be
  % better to first 'cast' the condition to a boolean, then perform a case
  % expression based off that
  #clause{line=Line, patterns=[Condition], exprs=Exprs} = Clause,
  Node = #'case'{line=Line, expr=Condition, clauses=[
    #clause{line=Line, patterns=[#false{line=Line}], exprs=[Output]},
    #clause{line=Line, patterns=[#nil{line=Line}],   exprs=[Output]},
    #clause{line=Line, patterns=[#identifier{line=Line, name='_'}], exprs=Exprs}
  ]},
  if_to_case(Clauses, Node).
      
% Is the given clause a catch-all clause?
is_catchall_clause(#clause{patterns=Patterns}) ->
  lists:any(fun catchall_pattern/1, Patterns).
  
% Is the given pattern a catch-all?
catchall_pattern(#match{left=Left, right=Right}) ->
  case catchall_pattern(Left) of
    true -> true;
    false -> catchall_pattern(Right)
  end;
catchall_pattern(#identifier{}) ->
  true;
catchall_pattern(_) ->
  false.