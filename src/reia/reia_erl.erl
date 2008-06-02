-module(reia_erl).
-export([erl_funcall/3, r2e/1, e2r/1, e2r_printable/1]).

% Make an Erlang function call with Reia arguments, and return Reia values
erl_funcall(Module, Function, Arguments) ->
  ErlangArguments = [r2e(Argument) || Argument <- Arguments],
  Value = apply(Module, Function, ErlangArguments),
  e2r(Value).
    
% Convert a Reia term to an Erlang term
r2e({string, Value}) -> 
  binary_to_list(Value);
r2e({tuple, Elements}) ->
  list_to_tuple([r2e(Element) || Element <- tuple_to_list(Elements)]);
r2e({list, {Elements, Order}}) ->
  rlist2elist(Elements, [], Order);
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
  {tuple, list_to_tuple([e2r(Element) || Element <- Term])};
e2r(Term) -> Term.

% Convert an Erlang term to a Reia term, converting printable strings
e2r_printable(Term) when is_list(Term) ->
  case io_lib:printable_list(Term) of
    true -> {string, list_to_binary(Term)};
    false -> [e2r_printable(Element) || Element <- Term]
  end;
e2r_printable(Term) when is_tuple(Term) ->
  {tuple, list_to_tuple([e2r_printable(Element) || Element <- Term])};
e2r_printable(Term) -> Term.