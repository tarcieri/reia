-module(reia_string).
-export([funcall/3]).

funcall({string, String}, to_s, []) ->
  {string, list_to_binary("\"" ++ binary_to_list(String) ++ "\"")}.