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

print(Value) -> io:format("~s~n", [stringify_term(Value)]).

stringify_term(Term) when is_integer(Term) -> stringify_integer(Term);
stringify_term(Term) when is_float(Term)   -> stringify_float(Term);
stringify_term(Term) when is_atom(Term)    -> stringify_atom(Term);
stringify_term(Term) when is_tuple(Term)   -> stringify_compound(Term).

stringify_integer(Int) -> integer_to_list(Int).
stringify_float(Float) ->
  [String|_] = io_lib:format("~f", [Float]),
  String.

stringify_atom(nil)   -> "nil";
stringify_atom(true)  -> "true";
stringify_atom(false) -> "false";
stringify_atom(Atom)  -> 
  String = atom_to_list(Atom),
  case regexp:match(String, "^[A-Za-z0-9_]+$") of
    nomatch -> "$'" ++ String ++ "'";
    _       -> "$" ++ String
  end.
  
stringify_compound({string, Binary}) -> 
  "\"" ++ binary_to_list(Binary) ++ "\"";
stringify_compound({regexp, Binary}) -> 
  "/" ++ binary_to_list(Binary) ++ "/";
stringify_compound({tuple, Tuple}) ->
  "(" ++ lists:concat(stringify_list_members(tuple_to_list(Tuple), [])) ++ ")";
stringify_compound({list, List}) -> 
  "[" ++ lists:concat(stringify_list_members(List, [])) ++ "]".

stringify_list_members([], Acc) -> lists:reverse(Acc);
stringify_list_members([Term|Rest], Acc) ->
  NewAcc = if 
    Rest == [] -> [stringify_term(Term)|Acc];
    true       -> [",",stringify_term(Term)|Acc]
  end,
  stringify_list_members(Rest, NewAcc).

print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).