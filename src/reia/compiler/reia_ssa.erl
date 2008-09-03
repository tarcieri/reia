%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, transform/2]).

ast(Ast) ->
  {ok, placeholder, Ast2} = reia_visitor:transform(Ast, placeholder, fun reia_ssa:transform/2),
  Ast2.
  
transform(State, Node) ->
  {walk, State, Node}.