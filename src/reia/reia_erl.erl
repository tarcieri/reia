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
r2e({list, {Elements, Order}}) ->
  rlist2elist(Elements, [], Order);
r2e({lambda, Fun}) ->
  Fun;
r2e(Term) -> Term.

% Convert a Reia list to an Erlang one
rlist2elist([], Acc, normal) ->
  lists:reverse(Acc);
rlist2elist([], Acc, reverse) ->
  Acc;
rlist2elist([Term|Rest], Acc, Order) ->
  rlist2elist(Rest, [r2e(Term)|Acc], Order).

% Convert an Erlang term to a Reia term
e2r(Term) when is_list(Term) ->
  {list, {[e2r(Element) || Element <- Term], normal}};
e2r(Term) when is_tuple(Term) ->
  case Term of
    {list, {List, Type}} when is_list(List) and is_atom(Type) ->
      Term;
    {tuple, Tuple} when is_tuple(Tuple) ->
      Term;
    {string, String} when is_binary(String) ->
      Term;
    {lambda, Lambda} when is_function(Lambda) ->
      Term;
    {dict, _} ->
      Term;
    {constant, _} ->
      Term;
    {regexp, _} ->
      Term;
    _ ->
      {tuple, list_to_tuple([e2r(Element) || Element <- tuple_to_list(Term)])}
  end;
e2r(Term) when is_function(Term) ->
  {lambda, Term};
e2r(Term) -> Term.