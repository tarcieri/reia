-module(ire).
-export([start/0]).

start() ->
  String = read(),
  Result = eval(String),
  print(Result),
  start().
  
read() -> 
  io:get_line('>> ').

eval(String) ->
  {ok, Scanned, _} = reia_scan:string(String),
  {ok, Parsed} = reia_parse:parse(Scanned),
  reia_eval:exprs(Parsed).
  
print(Result) -> 
  io:format("~p~n", [Result]).