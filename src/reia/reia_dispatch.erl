-module(reia_dispatch).
-export([funcall/3, funcall/4]).

%% Funcalls that don't take blocks
funcall(Receiver, Method, Arguments) when is_integer(Receiver) or is_float(Receiver) ->
  reia_numeric:funcall(Receiver, Method, Arguments);
funcall(Receiver, Method, Arguments) when is_atom(Receiver) ->
  reia_atom:funcall(Receiver, Method, Arguments);
funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_list:funcall(Receiver, Method, Arguments);
funcall(Receiver = {regexp, _}, Method, Arguments) ->
  reia_regexp:funcall(Receiver, Method, Arguments).
  
%% Funcalls that take blocks
funcall(Receiver = {list, _}, Method, Arguments, Block) ->
  reia_list:funcall(Receiver, Method, Arguments, Block).
  
