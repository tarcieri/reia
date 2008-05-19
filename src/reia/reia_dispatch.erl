-module(reia_dispatch).
-export([funcall/3]).

funcall(Receiver, Method, Arguments) when is_list(Receiver) ->
  reia_lists:funcall(Method, Arguments).