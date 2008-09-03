%
% reia_visitor: Automagical walker/transformer for Reia AST
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_visitor).
-export([transform/3]).

% Want to transform some Reia AST using the arbitrary function of your 
% choosing?  It's easy!  And you can even pass some state around while
% you're doing it.  Just hand reia_visitor:transform/3 a list of AST
% nodes or a single node, some seed state, and a function.  Here's the
% crazy Erlang type signature:
%
% transform(Expressions, State, Fun) -> {ok, NewState, NewExpressions}
%
% Types  Expressions = [term()]
%        State = term()
%        Fun = fun(State, Node)
%
% Fun must return one of the following:
% * {walk, NewState, NewNode} - keep traversing the AST, starting from NewNode
% * {stop, NewState, NewNode} - stop traversing the AST
%
transform(Expressions, State, Fun) when is_list(Expressions) ->
  {Expressions2, {State2, _Fun}} = lists:mapfoldl(fun transform_node/2, {State, Fun}, Expressions),
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
  {Elements2, {State2, _Fun}} = lists:mapfoldl(fun transform_node/2, {State, Fun}, Elements),
  {ok, State2, list_to_tuple([Type, Line | Elements2])};
walk(Node, State, _Fun) when is_atom(Node) or is_integer(Node) or is_float(Node) ->
  {ok, State, Node};
walk(Node, _State, _Fun) ->
  throw({error, {"unrecognized term in AST", Node}}).
  
transform_node(Node, {State, Fun}) ->
  {ok, State2, Node2} = transform(Node, State, Fun),
  {Node2, {State2, Fun}}.