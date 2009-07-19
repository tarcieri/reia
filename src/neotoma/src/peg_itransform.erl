-module(peg_itransform).
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
  io:format("~n~p~n", [AST]),
  AST.
