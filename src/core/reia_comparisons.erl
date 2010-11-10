%
% reia_comparisons: Handle special cases of the comparison operator "=="
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_comparisons).
-export([compare/2]).

% Compare Reia terms
compare(Left, Right) ->
  precompare(Left) == precompare(Right).
  
% Convert Reia terms to a form that will directly compare with Erlang's == operator
precompare({dict,_,_,_,_,_,_,_,_} = Dict) ->
  List = [{precompare(Key), precompare(Value)} || {Key, Value} <- dict:to_list(Dict)],
  {'__dict_placeholder', lists:keysort(1, List)};
precompare({reia_string, IoList}) ->
  {'__string_placeholder', iolist_to_binary(IoList)};
precompare(List) when is_list(List) ->
  [precompare(Elem) || Elem <- List];
precompare(Tuple) when is_tuple(Tuple) ->
  list_to_tuple(precompare(tuple_to_list(Tuple)));
precompare(Term) ->
  Term.