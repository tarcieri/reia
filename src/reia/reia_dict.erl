-module(reia_dict).
-export([funcall/3]).

%% Dict#[]
%%   Retrieve an element from a Dict
funcall({dict, Dict}, '[]', [Key]) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    error -> nil
  end;

%% Dict#to_s
%%   Convert a Dict to a string representation
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
  reia_erl:r2e(reia_dispatch:funcall(Value, to_s, [])).