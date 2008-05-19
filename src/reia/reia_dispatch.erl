-module(reia_dispatch).
-export([funcall/3]).

funcall(Receiver, Method, Arguments) ->
  io:format("r: ~p m: ~p a: ~p~n", [Receiver, Method, Arguments]).