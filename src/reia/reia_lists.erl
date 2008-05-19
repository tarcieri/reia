-module(reia_lists).
-export([funcall/3]).

funcall({list, {Elements, Order}}, reverse, []) ->
  case Order of
    normal  -> {list, {Elements, reverse}};
    reverse -> {list, {Elements, normal}}
  end.  