-module(csv_gen).
-export([transform/2]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(rows, Node) when length(Node) =:= 1 ->
  [];
transform(rows, Node) ->
  Head = proplists:get_value(head, Node),
  Tail = [R || [_,R] <- proplists:get_value(tail, Node)],
  [Head|Tail];
transform(row, Node) when length(Node) =:= 1 ->
  [];
transform(row, Node) ->
  Head = proplists:get_value(head, Node),
  Tail = [F || [_,F] <- proplists:get_value(tail, Node)],
  [Head|Tail];
transform(field, Node) ->
  lists:flatten(Node);
transform(quoted_field, Node) ->
  String = proplists:get_value(string, Node),
  re:replace(String, "[\"]{2}", "\"",[global, {return, list}]);
transform(Symbol, Node) when is_atom(Symbol) ->
  Node.
