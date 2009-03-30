%
% reia_erl: Functions for interfacing between Reia and Erlang
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_erl).
-export([erl_funcall/3, r2e/1, e2r/1]).

% Make an Erlang function call with Reia arguments, and return Reia values
erl_funcall(Module, Function, Arguments) ->
  ErlangArguments = [r2e(Argument) || Argument <- Arguments],
  Value = apply(Module, Function, ErlangArguments),
  e2r(Value).
    
% Convert a Reia term to an Erlang term
r2e({tuple, Elements}) ->
  list_to_tuple([r2e(Element) || Element <- tuple_to_list(Elements)]);
r2e({list, _} = List) ->
  [r2e(Term) || Term <- 'List':to_erl(List)];
r2e(Term) -> Term.

% Convert an Erlang term to a Reia term
e2r(Term) when is_list(Term) ->
  {list, {[], [e2r(Element) || Element <- Term]}};
e2r(Term) when is_tuple(Term) ->
  case Term of
    {list, {Reverse, Forward}} when is_list(Forward) and is_list(Reverse) ->
      Term;
    {tuple, Tuple} when is_tuple(Tuple) ->
      Term;
    {string, String} when is_binary(String) ->
      Term;
    {dict, _} ->
      Term;
    {constant, _} ->
      Term;
    {regexp, _} ->
      Term;
    {dict, _, _, _, _, _, _, _, _} ->
      {dict, Term};
    {object, _} ->
      Term;
    _ ->
      {tuple, list_to_tuple([e2r(Element) || Element <- tuple_to_list(Term)])}
  end;
e2r(Term) -> Term.