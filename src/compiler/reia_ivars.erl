%
% reia_ivars: Transform Reia instance variables into dict representation 
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ivars).
-export([ast/1, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, toplevel, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.

transform(State, {class, Line, Name, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, class, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, State, {class, Line, Name, Expressions2}};
  
transform(class, {function, Line, Name, Arguments, Expressions}) ->
  Expressions2 = lists:map(fun(Expression) ->
      {ok, _, Expression2} = reia_visitor:transform(Expression, method, fun transform/2),
      Expression2
    end,
    Expressions
  ),
  {stop, class, {function, Line, Name, Arguments, Expressions2}};

transform(method, {match, Line, In1, In2}) ->
  {ok, _, Out2} = reia_visitor:transform(In2, method, fun transform/2),
  {ok, {_, Ivars}, Out1} = reia_visitor:transform(In1, {match, sets:new()}, fun transform/2),
  {stop, method, match_node(Line, Out1, Out2, sets:to_list(Ivars))};
  
transform({match, Ivars}, {ivar, Line, Name}) ->
  Ivars2 = sets:add_element(Name, Ivars),
  Node = {identifier, Line, placeholder_name(Name)},
  {stop, {match, Ivars2}, Node};
  
transform(_State, {ivar, Line, _Name}) ->
  throw({error, {Line, "instance variables can only be referenced in classes"}});
      
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.
  
placeholder_name(Name) ->
  list_to_atom("__placeholder_" ++ atom_to_list(Name)).
  
match_node(Line, Expr1, Expr2, []) ->
  {match, Line, Expr1, Expr2};
match_node(Line, Expr1, Expr2, Ivars) ->
  {block, Line, lists:flatten([
    {match, Line, {identifier, Line, '__match_result'}, {match, Line, Expr1, Expr2}},
    [match_ivar(Line, Ivar) || Ivar <- Ivars],
    {identifier, Line, '__match_result'}
  ])}.
  
match_ivar(Line, Name) ->
  {match, Line, 
    {identifier, Line, '__instance_variables'},
    {call, Line,
      {remote, Line, {atom, Line, dict}, {atom, Line, store}},
      [{atom, Line, Name}, {identifier, Line, placeholder_name(Name)}, {identifier, Line, '__instance_variables'}]
    }
  }.