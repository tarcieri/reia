-module(reia_dispatch).
-export([funcall/3]).

funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_lists:funcall(Receiver, Method, Arguments).