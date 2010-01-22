%
% Regexp: Methods of the Regexp builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Regexp').
-export([call/4]).
-include("../core/reia_types.hrl").
-include("../core/reia_invoke.hrl").

call(#reia_regexp{pattern=Pattern}, to_s, _Args, _Block) ->
  ?invoke("%r/" ++ binary_to_list(Pattern) ++ "/", to_string, {}, nil);
call(Regexp, inspect, Args, Block) ->
  call(Regexp, to_s, Args, Block).
