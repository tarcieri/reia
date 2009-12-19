-module('String').
-export([call/4]).
-include("../compiler/reia_types.hrl").

call(#reia_string{members=Members}, to_s, _Args, _Block) ->
  "\"" ++ binary_to_list(iolist_to_binary(Members)) ++ "\"".