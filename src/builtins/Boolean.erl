%
% Boolean: Methods of the Boolean builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Boolean').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Boolean, to_s, _Args, _Block) ->
  ?invoke(atom_to_list(Boolean), to_string, {}, nil);
call(Boolean, inspect, _Args, _Block) ->
  ?invoke(atom_to_list(Boolean), to_string, {}, nil).