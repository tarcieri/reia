%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([transform/1]).
-include("reia_nodes.hrl").
  
%% Numerical types
transform(Ast = {integer, _, _}) -> Ast;
  
%% Operators
transform(#op{line=Line, type=Type, val1=Val1, val2=Val2}) ->
  reia_operators:transform(#op{
    line=Line, 
    type=Type, 
    val1=transform(Val1), 
    val2=transform(Val2)
  }).