%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, transform/2]).

ast(Ast) ->
  {ok, _, Ast2} = reia_visitor:transform(Ast, {normal, dict:new()}, fun reia_ssa:transform/2),
  Ast2.
  
% Function declarations create a new scope
transform(State, {function, Line, Name, Arguments, Expressions}) ->
  % Create a new scope with dict:new()
  {ok, Dict, Arguments2} = reia_visitor:transform(Arguments, {normal, dict:new()}, fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, Dict, fun transform/2),
    
  % Return to the original scope
  {stop, State, {function, Line, Name, Arguments2, Expressions2}};
  
% Match expressions mutate variables on the LHS
transform({Mode, Dict}, {match, Line, In1, In2}) ->
  {ok, {_, Dict2}, Out1} = reia_visitor:transform(In1, {match, Dict}, fun transform/2),
  {ok, {_, Dict3}, Out2} = reia_visitor:transform(In2, {Mode, Dict2}, fun transform/2),
  {stop, {Mode, Dict3}, {match, Line, Out1, Out2}};
  
% Normally identifiers are mapped to their latest version
transform({normal, _} = State, {identifier, _Line, _Name} = Node) ->
  {walk, State, Node};
  
% On the LHS of match expressions, variables are assigned a new version
transform({match, Dict}, {identifier, _Line, _Name} = Node) ->
  {stop, {match, Dict}, Node};
  
% Walk unrecognized nodes without transforming them
transform(State, Node) ->
  {walk, State, Node}.      
  