%
% Range: Methods of the Range builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Range').
-export([call/4]).
-include("../core/reia_types.hrl").
-include("../core/reia_invoke.hrl").

call(#reia_range{from=From, to=To}, to_list, _Args, _Block) ->
  lists:seq(From, To);
call(#reia_range{from=From, to=To}, to_s, _Args, _Block) ->
  ?invoke(convert(From, to_s) ++ ".." ++ convert(To, to_s), to_string, {}, nil);
call(#reia_range{from=From, to=To}, inspect, _Args, _Block) ->
  ?invoke(convert(From, inspect) ++ ".." ++ convert(To, inspect), to_string, {}, nil).
  
convert(Value, Method) -> 
  ?invoke(?invoke(Value, Method, {}, nil), to_list, {}, nil).