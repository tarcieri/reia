-module(ire).
-export([start/0]).

start() ->
  run(reia_eval:new_binding()).
  
run(Binding) ->
  case read() of
    eof -> io:format("~n"); % print a newline then exit
    String ->
      NewBinding = try
        eval_print(String, Binding)
      catch
        error:X -> print_error(X), 
        Binding
      end,
      run(NewBinding)
  end.
  
read() -> 
  io:get_line('>> ').

eval_print(String, Binding) ->
  {ok, Scanned, _} = reia_scan:string(String),
  {ok, Parsed} = reia_parse:parse(Scanned),
  {value, Value, NewBinding} = reia_eval:exprs(Parsed, Binding),
  print(Value),
  NewBinding.
  
print(Result) -> 
  io:format("~p~n", [Result]).
  
print_error(Error) ->
  io:format("Error: ~p~n", [Error]).