-module(reia_numeric).
-export([pow/2,funcall/3]).

pow(Base, Exponent) when is_integer(Base) ->
  erlang:round(math:pow(Base, Exponent));
pow(Base, Exponent) ->
  math:pow(Base, Exponent).
  
funcall(Int, to_s, []) when is_integer(Int) ->
  reia_lists:funcall(reia_erl:e2r(erlang:integer_to_list(Int)), to_string, []);
  
funcall(Float, to_s, []) when is_float(Float) ->
  [String|_] = io_lib:format("~f", [Float]),
  reia_list:funcall(reia_erl:e2r(String), to_string, []).