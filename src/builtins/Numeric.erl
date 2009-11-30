-module('Numeric').
-export([call/4]).

call(Number, to_s, _Args, _Block) ->
  if
    is_integer(Number) ->
      integer_to_list(Number);
    is_float(Number) ->
      lists:flatten(io_lib:format("~f", [Number]))
  end.