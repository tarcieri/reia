-module(reia_tuple).
-export([funcall/3]).

funcall({tuple, Members}, to_s, []) ->
  String = "(" ++ reia_list:funcall(reia_erl:e2r(erlang:tuple_to_list(Members)), join, [","]) ++ ")",
  reia_list:funcall(reia_erl:e2r(String), to_string, []).