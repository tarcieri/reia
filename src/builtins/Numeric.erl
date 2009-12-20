%
% Numeric: Methods of the Numeric builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Numeric').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Number, to_s, _Args, _Block) ->
  List = if
    is_integer(Number) ->
      integer_to_list(Number);
    is_float(Number) ->
      io_lib:format("~f", [Number])
  end,
  ?invoke(List, to_string, {}, nil);
  
call(Number, inspect, Args, Block) ->
  call(Number, to_s, Args, Block).