-module(reia_visitor).
-export([transform/3, transform_node/2]).

transform(Expressions, State, Fun) when is_list(Expressions) ->
  {Expressions2, {State2, _Fun}} = lists:mapfoldl(fun reia_visitor:transform_node/2, {State, Fun}, Expressions),
  {ok, State2, Expressions2};
  
transform(Node, State, Fun) ->
  case Fun(State, Node) of
    {walk, State2, Node2} ->
      walk(Node2, State2, Fun);
    {stop, State2, Node2} ->
      {ok, State2, Node2};
    Value ->
      throw({error, {"invalid_transform result", Value}})
  end.
  
walk(Node, State, Fun) when is_tuple(Node) ->
  [Type, Line | Elements] = tuple_to_list(Node),
  {Elements2, {State2, _Fun}} = lists:mapfoldl(fun reia_visitor:transform_node/2, {State, Fun}, Elements),
  {ok, State2, list_to_tuple([Type, Line | Elements2])};
walk(Node, State, _Fun) when is_atom(Node) or is_integer(Node) or is_float(Node) ->
  {ok, State, Node};
walk(Node, _State, _Fun) ->
  throw({error, {"unrecognized term in AST", Node}}).
  
transform_node(Node, {State, Fun}) ->
  {ok, State2, Node2} = transform(Node, State, Fun),
  {Node2, {State2, Fun}}.