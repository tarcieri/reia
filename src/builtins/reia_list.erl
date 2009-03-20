%
% reia_list: Methods for the List pseudo-class
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_list).
-export([funcall/4, to_erl/1]).

%%
%% Functions which don't take a block
%%

%% List#[]
%%   Retrieve an element in the list
funcall({list, {Reverse, Forward}}, '[]', [Index], nil) ->
  FwLength = length(Forward),
  RvLength = length(Reverse),
  if
    Index < 0 ->
      funcall({list, {Forward, Reverse}}, '[]', [-Index-1], nil);
    Index >= FwLength + RvLength ->
      nil;
    FwLength > Index ->
      lists:nth(Index + 1, Forward);
    true ->
      lists:nth(RvLength - (Index - FwLength), Reverse)
  end;
      
%% List#reverse
%%   Reverse the order of a list
funcall({list, {Reverse, Forward}}, reverse, [], nil) ->
  {list, {Forward, Reverse}};
  
%% List#push
%%   Add an element to the tail of a list
funcall({list, {Reverse, Forward}}, push, [Value], nil) ->
  {list, {[Value|Reverse], Forward}};
  
%% List#pop
%%   Pop an element from the tail of a list
funcall({list, {[], []}}, pop, [], nil) ->
  nil;
funcall({list, {[], Forward}}, pop, [], nil) ->
  Reverse = lists:reverse(Forward),
  [Value|_] = Reverse,
  Value;
funcall({list, {[Value|_Reverse], _Forward}}, pop, [], nil) ->
  Value;

%% List#unshift
%%   Unshift an element onto the front of a list
funcall({list, {Reverse, Forward}}, unshift, [Value], nil) ->
  {list, {Reverse, [Value|Forward]}};
  
%% List#shift
%%   Shift an element off the front of a list
funcall({list, {[], []}}, shift, [], nil) ->
  nil;
funcall({list, {Reverse, []}}, shift, [], nil) ->
  Forward = lists:reverse(Reverse),
  [Value|_] = Forward,
  Value;
funcall({list, {_Reverse, [Value|_Forward]}}, shift, [], nil) ->
  Value;
  
%% List#size
%%   Number of items in a list
funcall({list, {Reverse, Forward}}, size, [], nil) ->
  length(Reverse) + length(Forward);
  
%% List#to_tuple
%%   Explicitly cast a list to a tuple
funcall({list, _} = List, to_tuple, [], nil) ->
  {tuple, list_to_tuple(to_erl(List))};
  
%% List#to_list
%%   Cast a list to a liast, a noop
funcall({list, _} = List, to_list, [], nil) ->
  List;
 
%% List#to_string
%%   Explicitly cast a list to a string.  Useful for converting Erlang "strings"
%%   to Reia strings.
funcall({list, _} = List, to_string, [], nil) ->
  {string, list_to_binary(to_erl(List))};

%% List#to_s
%%   Generate a string representation of a list
funcall({list, _} = List, to_s, [], nil) ->
  funcall(List, inspect, [], nil);

%% List#to_hash
%%   Generate a hash based on key/val pairs
funcall({list, _} = List, to_hash, [], nil) ->
  {dict, dict:from_list([reia_erl:r2e(Element) || Element <- to_erl(List)])};
  
%% List#flatten
%%   Returns a flatten version of a given list
funcall({list, _} = List, flatten, [], nil) ->
  {list, {[], lists:flatten([preflatten(X) || X <- to_erl(List)])}};

%% List#inspect
%%   Inspect the contents of a list
funcall({list, _} = List, inspect, [], nil) ->
  Elements = [element_to_string2(Element) || Element <- to_erl(List)],
  String = lists:concat(["[", string:join(Elements, ","), "]"]),
  funcall(reia_erl:e2r(String), to_string, [], nil);
  
%% List#join
%%   Join all elements of a list together with the given separator
funcall({list, _} = List, join, [], nil) ->
  funcall(List, join, [{string, <<"">>}], nil);
funcall({list, _} = List, join, [{string, Sep}], nil) ->
  Elements = [element_to_string(Element) || Element <- to_erl(List)],
  String = string:join(Elements, binary_to_list(Sep)),
  funcall(reia_erl:e2r(String), to_string, [], nil);

%%  
%% Functions which take a block
%%

funcall({list, _} = List, each, [], Block) ->
  lists:foreach(Block, to_erl(List)),
  List;

funcall({list, _} = List, map, [], Block) ->
  {list, {[], lists:map(Block, to_erl(List))}};

funcall({list, _} = List, filter, [], Block) ->
  {list, {[], lists:filter(Block, to_erl(List))}};

funcall({list, _} = List, reduce, [Acc0], Block) ->
  lists:foldl(Block, Acc0, to_erl(List)).


element_to_string({string, Binary}) ->
  binary_to_list(Binary);
element_to_string(Element) ->
  element_to_string2(Element).
  
element_to_string2(Element) ->
  {list, {[], List}} = reia_dispatch:funcall(reia_dispatch:funcall(Element, to_s, [], nil), to_list, [], nil),
  List.
 
%% Recursively convert a list from a Reia to an Erlang representation in
%% preparation for flattening
preflatten({list, _} = List) ->
  [preflatten(X) || X <- to_erl(List)];
preflatten(X) when is_list(X) ->
  [preflatten(Y) || Y <- X];
preflatten(X) ->
  X.

%% Convert a Reia list to its Erlang equivalent
to_erl({list, {Reverse, Forward}}) ->
  Forward ++ lists:reverse(Reverse).
