%
% reia_list: Methods for the List pseudo-class
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_list).
-export([funcall/3, funcall/4, to_erl/1]).

%%
%% Functions which don't take a block
%%

%% List#[]
%%   Retrieve an element in the list
funcall({list, {Forward, Reverse}}, '[]', [Index]) ->
  FwLength = length(Forward),
  if
    Index < 0 ->
      nil;
    FwLength > Index ->
      lists:nth(Index + 1, Forward);
    true ->
      RvLength = length(Reverse),
      if 
        Index >= FwLength + RvLength ->
          nil;
        true ->
          lists:nth(RvLength - (Index - FwLength + 1))
      end
  end;
      
%% List#reverse
%%   Reverse the order of a list
funcall({list, {Forward, Reverse}}, reverse, []) ->
  {list, {Reverse, Forward}};
  
%% List#push
%%   Add an element to the tail of a list
funcall({list, {Forward, Reverse}}, push, [Value]) ->
  {list, {Forward, [Value|Reverse]}};
  
%% List#pop
funcall({list, {[], []}}, pop, []) ->
  nil;
funcall({list, {Forward, []}}, pop, []) ->
  Reverse = lists:reverse(Forward),
  [Value|_] = Reverse,
  Value;
funcall({list, {_Forward, [Value|_Reverse]}}, pop, []) ->
  Value;

%% List#unshift
funcall({list, {Forward, Reverse}}, unshift, [Value]) ->
  {list, {[Value|Forward], Reverse}};
  
%% List#shift
funcall({list, {[], []}}, shift, []) ->
  nil;
funcall({list, {[], Reverse}}, shift, []) ->
  Forward = lists:reverse(Reverse),
  [Value|_] = Forward,
  Value;
funcall({list, {[Value|_Forward], _Reverse}}, shift, []) ->
  Value;
  
%% List#to_string
%%   Explicitly cast a list to a string.  Useful for converting Erlang "strings"
%%   to Reia strings.
funcall(List = {list, _}, to_string, []) ->
  {string, list_to_binary(to_erl(List))};

%% List#to_s
%%   Generate a string representing a list
funcall(List = {list, _}, to_s, []) ->
  {string, Elements} = funcall(List, join, [{string, <<",">>}]),
  String = lists:concat(["[", binary_to_list(Elements), "]"]),
  funcall(reia_erl:e2r(String), to_string, []);
  
%% List#join
%%   Join all elements of a list together with the given separator
funcall(List = {list, _}, join, []) ->
  funcall(List, join, [{string, <<"">>}]);
funcall(List = {list, _}, join, [{string, Sep}]) ->
  Elements = [element_to_string(Element) || Element <- to_erl(List)],
  String = string:join(Elements, binary_to_list(Sep)),
  funcall(reia_erl:e2r(String), to_string, []).

element_to_string({string, Binary}) ->
  binary_to_list(Binary);
element_to_string(Element) ->
  {list, {List, []}} = reia_dispatch:funcall(reia_dispatch:funcall(Element, to_s, []), to_list, []),
  List.
  
%%
%% Functions which take a block
%%

funcall(List = {list, {Elements, _Order}}, each, [], {lambda, Block}) ->
  lists:foreach(Block, Elements),
  List;

funcall({list, {Elements, Order}}, map, [], {lambda, Block}) ->
  {list, {lists:map(Block, Elements), Order}};
  
funcall({list, {Elements, Order}}, filter, [], {lambda, Block}) ->
  {list, {lists:filter(Block, Elements), Order}};
  
funcall({list, {Elements, _}}, reduce, [Acc0], {lambda, Block}) ->
  lists:foldl(Block, Acc0, Elements).
  
%% Convert a Reia list to its Erlang equivalent
to_erl({list, {Forward, Reverse}}) ->
  Forward ++ lists:reverse(Reverse).