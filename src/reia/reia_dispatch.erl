-module(reia_dispatch).
-export([funcall/3, funcall/4]).

%% Convert a Reia term to its internal representation
funcall(Receiver, to_internal, []) ->
  case Receiver of
    {dict, _} ->
      {tuple, Receiver};
    _ ->
      reia_erl:e2r(Receiver)
  end;

%% Funcalls that don't take blocks
funcall(Receiver, Method, Arguments) when is_integer(Receiver) or is_float(Receiver) ->
  reia_numeric:funcall(Receiver, Method, Arguments);
funcall(Receiver, Method, Arguments) when is_atom(Receiver) ->
  reia_atom:funcall(Receiver, Method, Arguments);
funcall(Receiver, Method, Arguments) when is_binary(Receiver) ->
  reia_binary:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_list:funcall(Receiver, Method, Arguments);
funcall(Receiver = {tuple, _}, Method, Arguments) ->
  reia_tuple:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {dict, _}, Method, Arguments) ->
  reia_dict:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {string, _}, Method, Arguments) ->
  reia_string:funcall(Receiver, Method, Arguments);
funcall(Receiver = {lambda, _}, Method, Arguments) ->
  reia_lambda:funcall(Receiver, Method, Arguments);
funcall(Receiver = {regexp, _}, Method, Arguments) ->
  reia_regexp:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(_, _, _) ->
  throw(unknown_receiver).
  
%% Funcalls that take blocks
funcall(Receiver = {list, _}, Method, Arguments, Block) ->
  reia_list:funcall(Receiver, Method, Arguments, Block).
  
%% Reia doesn't have proper pattern matching for lists yet, so use a temporary hack
%% when talking to modules implemented directly in Reia
silly_list_hack(List) ->
  {list, {List, normal}}.