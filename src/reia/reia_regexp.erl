-module(reia_regexp).
-export([funcall/3]).

funcall({regexp, Binary}, to_s, []) ->
  String = "/" ++ binary_to_list(Binary) ++ "/",
  reia_list:funcall(reia_erl:e2r(String), to_string, []).