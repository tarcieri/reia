-module(reia_numeric).
-export([pow/2]).

pow(Base, Exponent) when is_integer(Base) ->
  round(math:pow(Base, Exponent));
pow(Base, Exponent) ->
  math:pow(Base, Exponent).