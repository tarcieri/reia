-module(reia_lists).
-export([funcall/3]).

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
  end.
  
push(Elements, []) ->
  {list, {Elements, reverse}};
push(Elements, [Value|Rest]) ->
  push([Value|Elements], Rest).