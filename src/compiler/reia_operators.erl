%
% reia_operators: Compiles Reia operator abstract syntax to Erlang transform
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_operators).
-export([transform/1]).
  
%% Addition
transform(Op) -> Op.