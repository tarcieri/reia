%
% Range: Methods of the Range builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Range').
-export([call/4]).
-include("../core/reia_types.hrl").

call(#reia_range{from=From, to=To}, to_list, _Args, _Block) ->
  lists:seq(From, To);
call(#reia_range{from=From, to=To}, to_s, _Args, _Block) ->
  convert(From) ++ ".." ++ convert(To).
  
convert(Value) -> reia_dispatch:call(Value, to_s, [], nil).