%
% reia_calls: Copy on update support for calls to Reia builtins
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_calls).
-export([ast/1]).

ast(Expressions) ->
  Expressions.