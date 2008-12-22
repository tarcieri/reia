%
% reia_string: Methods for the String pseudo-class
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_string).
-export([funcall/3]).

%% String#to_s
%%   A noop, as strings are already strings
funcall(String = {string, _}, to_s, []) ->
  String;

%% String#inspect
%%   Inspect the contents of a string
funcall({string, String}, inspect, []) ->
  {string, list_to_binary("\"" ++ binary_to_list(String) ++ "\"")};
  
%% String#to_list
%%   Cast a string explicitly to a list
funcall({string, String}, to_list, []) ->
  reia_erl:e2r(binary_to_list(String));
  
%% String#to_binary
%%   Cast a string explicitly to a binary
funcall({string, String}, to_binary, []) ->
  String;
  
%% String#to_atom
%%   Cast a string explicitly to an atom
funcall({string, String}, to_atom, []) ->
  list_to_atom(binary_to_list(String));

%% String#to_int
%%   Cast a string explicitly to an integer
funcall({string, String}, to_int, []) ->
  list_to_integer(binary_to_list(String));

%% String#to_float
%%   Cast a string explicitly to a float
funcall({string, String}, to_float, []) ->
  list_to_float(binary_to_list(String));
  
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
%%   Capitalize the first letter of a string
funcall({string, String}, capitalize, []) ->
  [FirstLetter|Rest] = binary_to_list(String),
  NewString = string:to_upper([FirstLetter]) ++ Rest,
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []);
  
%% String#uncapitalize
%%   Uncapitalize the first letter of a string
funcall({string, String}, uncapitalize, []) ->
  [FirstLetter|Rest] = binary_to_list(String),
  NewString = string:to_lower([FirstLetter]) ++ Rest,
  reia_list:funcall(reia_erl:e2r(NewString), to_string, []);
  
%% String#length
%%   Returns the number of characters in the string
funcall({string, String}, length, []) ->
  reia_erl:e2r(string:len(binary_to_list(String)));

%% String#sub
%%   Replace a portion of a string with a given substitution
funcall({string, String}, sub, [{regexp, Regex}, {string, Replacement}]) ->
  List = binary_to_list(String),
  case re:run(String, Regex) of
    {match, [{Begin, Length}]} ->
      Start = lists:sublist(List, 1, Begin),
      End = lists:sublist(List, Begin + Length + 1, length(List)),
      List2 = Start ++ binary_to_list(Replacement) ++ End,
      {string, list_to_binary(List2)};
    nomatch ->
      {string, String}
  end.
