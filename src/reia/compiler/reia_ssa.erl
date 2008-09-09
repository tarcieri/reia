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
  {ok, Dict2, Arguments2} = reia_visitor:transform(Arguments, dict:new(), fun transform/2),
  {ok, _, Expressions2} = reia_visitor:transform(Expressions, Dict2, fun transform/2),
  {stop, Dict, {function, Line, Name, Arguments2, Expressions2}};
transform(Dict, {match, Line, In1, In2}) ->
  {ok, Dict2, Out1} = reia_visitor:transform(In1, Dict, fun transform_match/2),
  {ok, Dict3, Out2} = reia_visitor:transform(In2, Dict2, fun transform/2),
  {stop, Dict3, {match, Line, Out1, Out2}};
transform(Dict, Node) ->
  {walk, Dict, Node}.  
  
transform_match(Dict, {identifier, _Line, _Name} = Arg) ->
  {stop, Dict, Arg};
transform_match(Dict, Node) ->
  {walk, Dict, Node}.
      
  