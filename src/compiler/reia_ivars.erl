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
  
transform(_State, {ivar, Line, _Name}) ->
  throw({error, {Line, "instance variables can only be referenced in classes"}});
      
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.