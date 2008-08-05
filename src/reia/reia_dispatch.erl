%
% reia_dispatch: Dispatch logic for all Reia method invocations
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_dispatch).
-export([funcall/3, funcall/4]).

%%
%% Funcalls that don't take blocks
%%

%% Shim to translate Erlang lists into Reia ones
funcall(List, Method, Arguments) when is_list(List) ->
  reia_list:funcall({list, {List, normal}}, Method, Arguments);

%% Convert a Reia term to its internal representation
funcall(Receiver, to_internal, []) ->
  case Receiver of
    {dict, _} ->
      {tuple, Receiver};
    _ ->
      reia_erl:e2r(Receiver)
  end;

funcall(Receiver, Method, Arguments) when is_integer(Receiver) or is_float(Receiver) ->
  reia_numeric:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, Method, Arguments) when is_atom(Receiver) ->
  'Atom':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, Method, Arguments) when is_binary(Receiver) ->
  'Binary':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_list:funcall(Receiver, Method, Arguments);
funcall(Receiver = {tuple, _}, Method, Arguments) ->
  'Tuple':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {dict, _}, Method, Arguments) ->
  'Hash':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {string, _}, Method, Arguments) ->
  reia_string:funcall(Receiver, Method, Arguments);
funcall(Receiver = {lambda, _}, Method, Arguments) ->
  'Lambda':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {regexp, _}, Method, Arguments) ->
  reia_regexp:funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(_, _, _) ->
  throw(unknown_receiver).
  
%%  
%% Funcalls that take blocks
%%

%% Shim to translate Erlang lists into Reia ones
funcall(List, Method, Arguments, Block) when is_list(List) ->
  reia_list:funcall({list, {List, normal}}, Method, Arguments, Block);
  
funcall(Receiver = {list, _}, Method, Arguments, Block) ->
  reia_list:funcall(Receiver, Method, Arguments, Block).
  
%% Reia doesn't have proper pattern matching for lists yet, so use a temporary hack
%% when talking to modules implemented directly in Reia
silly_list_hack(List) ->
  {list, {List, normal}}.