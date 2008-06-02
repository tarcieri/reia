-module(reia_tuple).
-export([funcall/3]).

%% Tuple#[]
%%   Retrieve an element from a Tuple
funcall({tuple, Tuple}, '[]', [Index]) ->
  reia_dispatch:funcall({list, {erlang:tuple_to_list(Tuple), normal}}, '[]', [Index]);

%% Tuple#to_list
%%   Convert a tuple to a list
funcall({tuple, Tuple}, to_list, []) ->
  {list, {erlang:tuple_to_list(Tuple), normal}};
    
%% Tuple#to_s
%%   Generate a string representation of a Tuple
funcall({tuple, Tuple}, to_s, []) ->
  Elements = [reia_dispatch:funcall(Element, to_s, []) || Element <- erlang:tuple_to_list(Tuple)],
  String = "(" ++ reia_erl:r2e(reia_list:funcall({list, {Elements, normal}}, join, [reia_erl:e2r(",")])) ++ ")",
  reia_list:funcall(reia_erl:e2r(String), to_string, []).
  
  