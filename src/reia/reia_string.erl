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
  
%% String#upcase
%%   Capitalize all letters string
funcall({string, String}, upcase, []) ->
  NewString = string:to_upper(binary_to_list(String)),
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []);
  
%% String#downcase
%%   Remove capitalization from a string
funcall({string, String}, downcase, []) ->
  NewString = string:to_lower(binary_to_list(String)),
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []);
  
%% String#capitalize
%%   Capitalize the first letter of a string, downcasing all others
funcall({string, String}, capitalize, []) ->
  [FirstLetter|Rest] = binary_to_list(String),
  NewString = string:to_upper([FirstLetter]) ++ string:to_lower(Rest),
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []).