%
% Atom: Methods of the Atom builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Atom').
-export([call/4]).
-include("../core/reia_invoke.hrl").

call(Atom, to_s, _Args, _Block) ->
  ?invoke(atom_to_list(Atom), to_string, {}, nil);
  
call(Atom, inspect, _Args, _Block) ->
  List = case re:run(atom_to_list(Atom), "^[A-Za-z0-9_]+$") of
    {match, _} -> ":" ++ atom_to_list(Atom);
    nomatch    -> lists:flatten([":'", atom_to_list(Atom), "'"])
  end,
  ?invoke(List, to_string, {}, nil).