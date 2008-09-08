%
% reia_ssa: Static single assignment transformation for Reia's compiler
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([ast/1, transform/2]).

ast(Ast) ->
  {ok, _, Ast2} = reia_visitor:transform(Ast, dict:new(), fun reia_ssa:transform/2),
  Ast2.
  
transform(Dict, {function, Line, Name, Arguments, Expressions}) ->
  {ok, Dict2, Arguments2} = transform_list(Arguments, dict:new()),
  {ok, _, Expressions2} = transform_list(Expressions, Dict2),
  {stop, Dict, {function, Line, Name, Arguments2, Expressions2}};
transform(Dict, Node) ->
  {walk, Dict, Node}.
  
transform_list(List, State) ->
  reia_visitor:transform(List, State, fun transform/2).