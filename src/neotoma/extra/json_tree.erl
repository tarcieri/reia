-module(json_tree).
-export([transform/2]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(json_value, Node) ->
  lists:nth(2, Node);
transform(object, Node) when length(Node) =:= 3 ->
  {struct, []};
transform(object, Node) ->
  Head = proplists:get_value(head, Node),
  Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
  {struct, [Head|Rest]};
transform(array, Node) when length(Node) =:= 3 ->
  [];
transform(array, Node) ->
  Head = proplists:get_value(head, Node),
  Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
  [Head|Rest];
transform(number, [Int, [],[]]) ->
  list_to_integer(lists:flatten(Int));
transform(number, [Int, Frac, []]) ->
  list_to_float(lists:flatten([Int, Frac]));
transform(number, [Int, [], Exp]) ->
  transform(number, [Int, ".0", Exp]);
transform(number, Node) ->
  list_to_float(lists:flatten(Node));
transform(string, Node) ->
  lists:flatten(proplists:get_value(chars, Node));
transform(pair, Node) ->
  {proplists:get_value(key, Node), proplists:get_value(value, Node)};
transform(true, _Node) -> true;
transform(false, _Node) -> false;
transform(null, _Node) -> null;
transform(Symbol, Node) when is_atom(Symbol) ->
  Node.
