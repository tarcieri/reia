-module(reia_list).
-export([funcall/3, funcall/4, stringify_members/1]).

%%
%% Functions which don't take a block
%%

%% List#reverse
funcall({list, {Elements, Order}}, reverse, []) ->
  case Order of
    normal  -> {list, {Elements, reverse}};
    reverse -> {list, {Elements, normal}}
  end;
  
%% List#push
funcall({list, {Elements, Order}}, push, Values) ->
  case Order of
    normal ->
      push(lists:reverse(Elements), Values);
    reverse ->
      push(Elements, Values)
  end;
  
%% List#pop
funcall({list, {Elements, Order}}, pop, []) ->
  [Element|_] = case Order of
    normal  -> lists:reverse(Elements);
    reverse -> Elements
  end,
  Element;
  
%% List#unshift
funcall({list, {Elements, Order}}, unshift, Values) ->
  case Order of
    normal  ->
      unshift(Elements, Values);
    reverse ->
      unshift(lists:reverse(Elements), Values)
  end;
  
%% List#shift
funcall({list, {Elements, Order}}, shift, []) ->
  [Element|_] = case Order of
    normal  -> Elements;
    reverse -> lists:reverse(Elements)
  end,
  Element;
  
%% Lists#to_string
%%   Explicitly cast a list to a string.  Useful for converting Erlang "strings"
%%   to Reia strings.
funcall({list, {List, Order}}, to_string, []) ->
  case Order of
    normal  -> {string, list_to_binary(List)};
    reverse -> {string, list_to_binary(lists:reverse(List))}
  end;
  
%% Lists#to_s
%%   Generate a string representing a list
funcall({list, {List, Order}}, to_s, []) ->
  String = "[" ++ stringify_members(List, Order) ++ "]",
  funcall(reia_erl:e2r(String), to_string, []).
  
push(Elements, []) ->
  {list, {Elements, reverse}};
push(Elements, [Value|Rest]) ->
  push([Value|Elements], Rest).
  
unshift(Elements, []) ->
  {list, {Elements, normal}};
unshift(Elements, [Value|Rest]) ->
  unshift([Value|Elements], Rest).

%% Used by reia_tuple for stringification
stringify_members(Elements) ->
  stringify_members(Elements, normal).
    
stringify_members(Elements, Order) ->
  stringify_members(Elements, [], Order).

stringify_members([], Acc, normal) ->  lists:concat(lists:reverse(Acc));
stringify_members([], Acc, reverse) ->  lists:concat(Acc);
stringify_members([Term|Rest], Acc, Order) ->
  {string, Binary} = reia_dispatch:funcall(Term, to_s, []),
  String = binary_to_list(Binary),
  NewAcc = if 
    Rest == [] -> [String|Acc];
    true       -> [",", String|Acc]
  end,
  stringify_members(Rest, NewAcc, Order).

%%
%% Functions which take a block
%%

funcall({list, {Elements, Order}}, map, [], {lambda, Block}) ->
  {list, {lists:map(Block, Elements), Order}};
  
funcall({list, {Elements, Order}}, filter, [], {lambda, Block}) ->
  {list, {lists:filter(Block, Elements), Order}};
  
funcall({list, {Elements, _}}, reduce, [Acc0], {lambda, Block}) ->
  lists:foldl(Block, Acc0, Elements).