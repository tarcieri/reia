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
  case parse(String) of
    {ok, Exprs} ->
      {value, Value, NewBinding} = reia_eval:exprs(Exprs, Binding),
      print(Value),
      NewBinding;
    {error, Error} ->
      parse_error(Error),
      Binding
  end.
  
parse(String) ->
  case reia_scan:scan(String) of
    {ok, Tokens, _} -> 
      case reia_parse:parse(Tokens) of
        {ok, Exprs} ->
          {ok, Exprs};
        {error, {Line, _, [Message, Token]}} ->
          {error, {Line, io_lib:format("~s~s", [Message, Token])}}
      end;
    {error, {Line, _, {Message, Token}}, _} ->
      {error, {Line, io_lib:format("~p ~p", [Message, Token])}}
  end.

print(Value) ->
  String = reia_dispatch:funcall(Value, to_s, []),
  io:format("~s~n", [reia_erl:r2e(String)]).
  
parse_error({Line, Error}) ->
  io:format("Error: Line ~w: ~s~n", [Line, Error]).
  
print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).