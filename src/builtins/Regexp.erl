%
% Regexp: Methods of the Regexp builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Regexp').
-export([call/4]).
-include("../core/reia_types.hrl").

call(#reia_regexp{pattern=Pattern}, to_s, _Args, _Block) ->
  "/" ++ binary_to_list(Pattern) ++ "/".