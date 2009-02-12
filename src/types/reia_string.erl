%
% reia_string: Methods for the String pseudo-class
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_string).
-export([funcall/3, from_list/1]).

%% String#to_s
%%   A noop, as strings are already strings
funcall(String = {string, _}, to_s, []) ->
  String;

%% String#inspect
%%   Inspect the contents of a string
funcall({string, String}, inspect, []) ->
  {string, list_to_binary("\"" ++ binary_to_list(String) ++ "\"")};
  
%% String#print
%%   Print out a string to the terminal
funcall({string, String}, print, []) ->
  io:format(binary_to_list(String)),
  nil;
  
%% String#puts
%%   Print out a string with accompanying newline
funcall({string, String}, puts, []) ->
  io:format("~s~n", [String]),
  nil;
  
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

%% String#slice
%%   Returns a sub string starting at position of Start,
%%   ending at option Stop or end of string
funcall({string, String}, slice, [Start]) ->
  NewString = reia_erl:e2r(string:sub_string(binary_to_list(String), Start)),
  reia_list:funcall(NewString, to_string, []);
funcall({string, String}, slice, [Start, Stop]) ->
  NewString = reia_erl:e2r(string:sub_string(binary_to_list(String), Start, Stop)),
  reia_list:funcall(NewString, to_string, []);

%% String#strip
%%   Returns string stripped of whitespace or optional char,
%%   in optional atom direction: left, right or both (default)
funcall({string, String}, strip, []) ->
  funcall({string, String}, strip, [$ , both]);
funcall({string, String}, strip, [{string, Bin}]) ->
  funcall({string, String}, strip, [{string, Bin}, both]);
funcall({string, String}, strip, [Direction]) ->
  funcall({string, String}, strip, [$ , Direction]);
funcall({string, String}, strip, [{string, Bin}, Direction]) ->
  Char = lists:nth(1, binary_to_list(Bin)),
  funcall({string, String}, strip, [Char, Direction]);
funcall({string, String}, strip, [Char, Direction]) ->
  NewString = reia_erl:e2r(string:strip(binary_to_list(String), Direction, Char)),
  reia_list:funcall(NewString, to_string, []);

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
  end;
  
%% String#split
%%   Split apart a string using the given regex
funcall({string, String}, split, [{regexp, Regex}]) ->
  List = binary_to_list(String),
  reia_erl:e2r([{string, Bin} || Bin <- re:split(List, Regex)]);
  
%% String#to_parsetree
%%   Parse a string into its Reia parse tree
funcall({string, String}, to_parsetree, []) ->
  List = binary_to_list(String),
  case reia:parse(List) of
    {ok, Expressions}  -> reia_erl:e2r(Expressions);
    {error, _} = Error -> throw(Error)
  end;

%% String#to_i
%%   Sloppily convert a string into an integer
funcall({string, String}, to_i, []) ->
  List = binary_to_list(String),
  case erl_scan:string(List) of
    {ok, [{integer, _, Integer}], _} -> 
      Integer;
    {ok, [{'-', _}, {integer, _, Integer}], _} ->
      -Integer;
    _ -> 
      0
  end;
  
%% String#to_f
%%   Sloppily convert a string into a float
funcall({string, String}, to_f, []) ->
  List = binary_to_list(String),
  case erl_scan:string(List) of
    {ok, [{float, _, Float}], _} -> 
      Float;
    {ok, [{'-', _}, {float, _, Float}], _} ->
      -Float;
    _ -> 
      0.0
  end.

%% Convert a string in Erlang list form to Reia form
from_list(List) ->
  {string, list_to_binary(List)}.