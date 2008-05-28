-module(reia_dict).
-export([funcall/3]).

funcall({dict, Dict}, to_s, []) ->
  String = "{" ++ stringify_members(Dict) ++ "}",
  reia_list:funcall(reia_erl:e2r(String), to_string, []).
  
stringify_members(Dict) ->
  stringify_members(dict:to_list(Dict), []).

stringify_members([], Acc) ->
  lists:concat(lists:reverse(Acc));
stringify_members([{Key,Value}|Rest], Acc) ->
  Element = stringify_term(Key) ++ ":" ++ stringify_term(Value),
  NewAcc = if
    Rest == [] -> [Element|Acc];
    true       -> [",", Element|Acc]
  end,
  stringify_members(Rest, NewAcc).
  
stringify_term(Value) ->
  {string, Binary} = reia_dispatch:funcall(Value, to_s, []),
  binary_to_list(Binary).