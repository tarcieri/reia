-module(reia_lists).
-export([funcall/3, funcall/4]).

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
    normal -> lists:reverse(Elements);
    reverse -> Elements
  end,
  Element;
  
%% List#unshift
funcall({list, {Elements, Order}}, unshift, Values) ->
  case Order of
    normal ->
      unshift(Elements, Values);
    reverse ->
      unshift(lists:reverse(Elements), Values)
  end;
  
%% List#shift
funcall({list, {Elements, Order}}, shift, []) ->
  [Element|_] = case Order of
    normal -> Elements;
    reverse -> lists:reverse(Elements)
  end,
  Element.
  
push(Elements, []) ->
  {list, {Elements, reverse}};
push(Elements, [Value|Rest]) ->
  push([Value|Elements], Rest).
  
unshift(Elements, []) ->
  {list, {Elements, normal}};
unshift(Elements, [Value|Rest]) ->
  unshift([Value|Elements], Rest).
  
funcall({list, {Elements, Order}}, map, [], {lambda, Block}) ->
  {list, {lists:map(Block, Elements), Order}}.