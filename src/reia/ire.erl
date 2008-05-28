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
        Class:Reason -> print_error(Class, Reason), 
        Binding
      end,
      run(NewBinding)
  end.
  
read() -> 
  io:get_line('>> ').

eval_print(String, Binding) ->
  {ok, Scanned, _} = reia_scan:scan(String),
  {ok, Parsed} = reia_parse:parse(Scanned),
  {value, Value, NewBinding} = reia_eval:exprs(Parsed, Binding),
  print(Value),
  NewBinding.

print(Value) ->
  {string, Binary} = reia_dispatch:funcall(Value, to_s, []),
  io:format("~s~n", [binary_to_list(Binary)]).

print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).