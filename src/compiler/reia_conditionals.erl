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
  Node2 = case lists:any(fun is_catchall_clause/1, Clauses) of
    true ->
      Node;
    false ->
      Catchall = #clause{
        line=Line, 
        patterns=[#var{line=Line, name='_'}], 
        exprs=[#nil{line=Line}]
      },
      Node#'case'{clauses=Clauses ++ [Catchall]} 
  end,
  reia_syntax:map_subtrees(fun transform/1, Node2);
    
% Unary not
transform(#unary_op{type='not', expr=Expr}=Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node#unary_op{expr=cast_boolean(Expr)});
transform(#unary_op{type='!',   expr=Expr}=Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node#unary_op{expr=cast_boolean(Expr)});

% Boolean binary ops
transform(#binary_op{type='and', left=Left, right=Right}=Node) ->
  Node2 = Node#binary_op{left=cast_boolean(Left), right=cast_boolean(Right)},
  reia_syntax:map_subtrees(fun transform/1, Node2);
transform(#binary_op{type='or',  left=Left, right=Right}=Node) ->
  Node2 = Node#binary_op{left=cast_boolean(Left), right=cast_boolean(Right)},
  reia_syntax:map_subtrees(fun transform/1, Node2);
      
% Walk unrecognized nodes without transforming them
transform(Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node).

% Convert if statements to case statements
if_to_case(Clauses) ->
  if_to_case(lists:reverse(Clauses), #nil{line=1}).
  
if_to_case([], Output) ->
  Output;
if_to_case([Clause|Clauses], Output) ->
  #clause{line=Line, patterns=[Condition], exprs=Exprs} = Clause,
  Node = #'case'{line=Line, expr=cast_boolean(Condition), clauses=[
    #clause{line=Line, patterns=[#false{line=Line}], exprs=[Output]},
    #clause{line=Line, patterns=[#true {line=Line}], exprs=Exprs}
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
catchall_pattern(#var{}) ->
  true;
catchall_pattern(_) ->
  false.
  
% "Cast" Reia-style booleans to the Erlang equivalent.  In Reia (as in Ruby),
% everything is considered "true" in a boolean context except false and nil.
cast_boolean(Condition) ->
  Line = element(2, Condition),
  #'case'{line=Line, expr=Condition, clauses=[
    #clause{line=Line, patterns=[#false{line=Line}], exprs=[#false{line=Line}]},
    #clause{line=Line, patterns=[#nil{line=Line}],   exprs=[#false{line=Line}]},
    #clause{line=Line, patterns=[#var{line=Line, name='_'}], exprs=[#true{line=Line}]}
  ]}.