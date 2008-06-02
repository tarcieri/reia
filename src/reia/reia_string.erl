-module(reia_string).
-export([funcall/3]).

%% String#to_s
%%   Generate a string representation of a string
funcall({string, String}, to_s, []) ->
  {string, list_to_binary("\"" ++ binary_to_list(String) ++ "\"")};
  
%% String#to_list
%%   Cast a string explicitly to a list
funcall({string, String}, to_list, []) ->
  reia_erl:e2r(binary_to_list(String));
  
%% String#to_upper
%%   Capitalize all letters string
funcall({string, String}, to_upper, []) ->
  NewString = string:to_upper(binary_to_list(String)),
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []);
  
%% String#to_lower
%%   Remove capitalization from a string
funcall({string, String}, to_lower, []) ->
  NewString = string:to_lower(binary_to_list(String)),
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []).