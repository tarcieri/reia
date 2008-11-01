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
  reia_list:funcall({list, {[], List}}, Method, Arguments);

%% Convert a Reia term from its internal representation
funcall(Receiver, uninternalize, []) ->
  case Receiver of
    {dict, _} ->
      {tuple, Receiver};
    {regexp, _} ->
      {tuple, Receiver};
    _ ->
      reia_erl:e2r(Receiver)
  end;
  
%% Convert a term from its Reia representation to an internal Erlang one
funcall(Receiver, internalize, []) ->
  reia_erl:r2e(Receiver);

%% Dispatch a method to an object
funcall({object, {Pid, _Class}}, Method, Arguments) ->
  reia_class:call(Pid, {Method, Arguments});
funcall({constant, Name}, Method, Arguments) ->
  apply(Name, Method, Arguments);
funcall(Receiver, Method, Arguments) when is_integer(Receiver) or is_float(Receiver) ->
  'Numeric':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, Method, Arguments) when is_atom(Receiver) ->
  'Atom':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, Method, Arguments) when is_binary(Receiver) ->
  'Binary':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, Method, Arguments) when is_function(Receiver) ->
  'Lambda':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {list, _}, Method, Arguments) ->
  reia_list:funcall(Receiver, Method, Arguments);
funcall(Receiver = {tuple, _}, Method, Arguments) ->
  'Tuple':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {dict, _}, Method, Arguments) ->
  'Hash':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver = {string, _}, Method, Arguments) ->
  reia_string:funcall(Receiver, Method, Arguments);
funcall(Receiver = {regexp, _}, Method, Arguments) ->
  'Regex':funcall(Receiver, Method, silly_list_hack(Arguments));
funcall(Receiver, _, _) ->
  throw({error, unknown_receiver, Receiver}).
  
%%  
%% Funcalls that take blocks
%%

%% Shim to translate Erlang lists into Reia ones
funcall(List, Method, Arguments, Block) when is_list(List) ->
  reia_list:funcall({list, {[], List}}, Method, Arguments, Block);
  
funcall(Receiver = {list, _}, Method, Arguments, Block) ->
  reia_list:funcall(Receiver, Method, Arguments, Block).
  
%% Reia doesn't have proper pattern matching for lists yet, so use a temporary hack
%% when talking to modules implemented directly in Reia
silly_list_hack(List) ->
  {list, {[], List}}.