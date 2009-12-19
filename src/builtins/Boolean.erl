%
% Boolean: Methods of the Boolean builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Boolean').
-export([call/4]).

call(Atom, to_s, _Args, _Block) ->
  atom_to_list(Atom).