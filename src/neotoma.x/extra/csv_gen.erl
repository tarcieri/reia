-module(csv_gen).
-export([transform/3]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(rows, Node, _Idx) when length(Node) =:= 1 ->
  [];
transform(rows, Node, _Idx) ->
  Head = proplists:get_value(head, Node),
  Tail = [R || [_,R] <- proplists:get_value(tail, Node)],
  [Head|Tail];
transform(row, Node, _Idx) when length(Node) =:= 1 ->
  [];
transform(row, Node, _Idx) ->
  Head = proplists:get_value(head, Node),
  Tail = [F || [_,F] <- proplists:get_value(tail, Node)],
  [Head|Tail];
transform(field, Node, _Idx) ->
  lists:flatten(Node);
transform(quoted_field, Node, _Idx) ->
  String = proplists:get_value(string, Node),
  re:replace(String, "[\"]{2}", "\"",[global, {return, list}]);
transform(Symbol, Node, _Idx) when is_atom(Symbol) ->
  Node.
