-module(reia_dispatch).
-export([funcall/3, funcall/4]).

funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_lists:funcall(Receiver, Method, Arguments).
  
funcall(Receiver = {list, _}, Method, Arguments, Block) ->
  reia_lists:funcall(Receiver, Method, Arguments, Block).