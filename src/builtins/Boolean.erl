-module('Boolean').
-export([call/4]).

call(Atom, to_s, _Args, _Block) ->
  atom_to_list(Atom).