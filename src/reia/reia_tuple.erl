-module(reia_tuple).
-export([funcall/3]).

funcall({tuple, Members}, to_s, []) ->
  String = "(" ++ reia_list:stringify_members(erlang:tuple_to_list(Members)) ++ ")",
  reia_list:funcall(reia_erl:e2r(String), to_string, []).