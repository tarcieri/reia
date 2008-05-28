-module(reia_lambda).
-export([funcall/3]).

funcall({lambda, _}, to_s, []) ->
  reia_list:funcall(reia_erl:e2r("#Fun"), to_string, []).