-module(reia_list).
-export([funcall/3, funcall/4]).

%%
%% Functions which don't take a block
%%

%% List#[]
%%   Retrieve an element in the list
funcall({list, {Elements, Order}}, '[]', [Index]) ->
  List = case Order of
    normal -> Elements;
    reverse -> lists:reverse(Elements)
  end,
  try 
    lists:nth(Index + 1, List)
  catch
    error:function_clause -> nil
  end;

%% List#reverse
%%   Reverse the order of a list
funcall({list, {Elements, Order}}, reverse, []) ->
  case Order of
    normal  -> {list, {Elements, reverse}};
    reverse -> {list, {Elements, normal}}
  end;
  
%% List#push
%%   Add an element to the tail of a list
funcall({list, {Elements, Order}}, push, Values) ->
  case Order of
    normal ->
      push(lists:reverse(Elements), Values);
    reverse ->
      push(Elements, Values)
  end;
  
%% List#pop
funcall({list, {Elements, Order}}, pop, []) ->
  [Element|_] = case Order of
    normal  -> lists:reverse(Elements);
    reverse -> Elements
  end,
  Element;
  
%% List#unshift
funcall({list, {Elements, Order}}, unshift, Values) ->
  case Order of
    normal  ->
      unshift(Elements, Values);
    reverse ->
      unshift(lists:reverse(Elements), Values)
  end;
  
%% List#shift
funcall({list, {Elements, Order}}, shift, []) ->
  [Element|_] = case Order of
    normal  -> Elements;
    reverse -> lists:reverse(Elements)
  end,
  Element;
  
%% List#to_string
%%   Explicitly cast a list to a string.  Useful for converting Erlang "strings"
%%   to Reia strings.
funcall({list, {List, Order}}, to_string, []) ->
  case Order of
    normal  -> {string, list_to_binary(List)};
    reverse -> {string, list_to_binary(lists:reverse(List))}
  end;
  
%% List#join
%%   Join all elements of a list together with the given separator
funcall(List = {list, _}, join, []) ->
  funcall(List, join, [{string, <<"">>}]);
funcall({list, {List, Order}}, join, [{string, Sep}]) ->
  funcall(reia_erl:e2r(join(List, binary_to_list(Sep), Order)), to_string, []);
  
%% List#to_s
%%   Generate a string representing a list
funcall({list, {List, Order}}, to_s, []) ->
  String = "[" ++ join([reia_dispatch:funcall(Element, to_s, []) || Element <- List], ",", Order) ++ "]",
  funcall(reia_erl:e2r(String), to_string, []).
  
push(Elements, []) ->
  {list, {Elements, reverse}};
push(Elements, [Value|Rest]) ->
  push([Value|Elements], Rest).
  
unshift(Elements, []) ->
  {list, {Elements, normal}};
unshift(Elements, [Value|Rest]) ->
  unshift([Value|Elements], Rest).
    
join(Elements, Sep, Order) ->
  join(Elements, Sep, [], Order).

join([], _, Acc, normal) ->  lists:concat(lists:reverse(Acc));
join([], _, Acc, reverse) ->  lists:concat(Acc);
join([Term|Rest], Sep, Acc, Order) ->
  Bin = case Term of
    {string, X} ->
      X;
    _ ->
      {string, X} = reia_dispatch:funcall(Term, to_s, []),
      X
  end,
  String = binary_to_list(Bin),
  NewAcc = if 
    Rest == [] -> [String|Acc];
    true       -> [Sep, String|Acc]
  end,
  join(Rest, Sep, NewAcc, Order).

%%
%% Functions which take a block
%%

funcall({list, {Elements, Order}}, map, [], {lambda, Block}) ->
  {list, {lists:map(Block, Elements), Order}};
  
funcall({list, {Elements, Order}}, filter, [], {lambda, Block}) ->
  {list, {lists:filter(Block, Elements), Order}};
  
funcall({list, {Elements, _}}, reduce, [Acc0], {lambda, Block}) ->
  lists:foldl(Block, Acc0, Elements).