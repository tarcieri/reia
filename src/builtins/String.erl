%
% String: Methods of the String builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('String').
-export([call/4]).
-include("../core/reia_types.hrl").

call(#reia_string{members=Members}, to_s, _Args, _Block) ->
  "\"" ++ binary_to_list(iolist_to_binary(Members)) ++ "\"".