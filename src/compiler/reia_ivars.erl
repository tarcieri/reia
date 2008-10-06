%
% reia_ivars: Transform Reia instance variables into dict representation 
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ivars).
-export([ast/1, ast/2, transform/2]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ast(Ast, []).
  
ast(Ast, Variables) ->
  Dict = dict:from_list([{Variable, 0} || Variable <- Variables]),
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, {toplevel, Dict}, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.

transform(_State, {ivar, Line, _Name}) ->
  throw({error, {Line, "instance variables can only be referenced in classes"}});
      
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.