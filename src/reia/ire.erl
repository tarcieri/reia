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

print(Value) -> io:format("~s~n", [stringify_term(Value)]).

stringify_term(Term) when is_integer(Term) -> stringify_integer(Term);
stringify_term(Term) when is_float(Term)   -> stringify_float(Term);
stringify_term(Term) when is_list(Term)    -> stringify_list(Term);
stringify_term(Term) when is_tuple(Term)   -> stringify_compound(Term);
stringify_term(Term) when is_atom(Term)    -> stringify_atom(Term).

stringify_integer(Int) -> integer_to_list(Int).
stringify_float(Float) ->
  [String|_] = io_lib:format("~f", [Float]),
  String.

stringify_list(List) -> "[" ++ lists:concat(stringify_list_members(List, [])) ++ "]".

stringify_list_members([], Acc) -> lists:reverse(Acc);
stringify_list_members([Term|Rest], Acc) ->
  NewAcc = if 
    Rest == [] -> [stringify_term(Term)|Acc];
    true       -> [",",stringify_term(Term)|Acc]
  end,
  stringify_list_members(Rest, NewAcc).
  
stringify_compound(Term = {atom, _, _}) ->
  stringify_atom(Term);
stringify_compound(Term = {Type, Data}) ->
  case Type of
    string -> stringify_string(Term);
    regexp -> stringify_regexp(Term)
  end.
  
stringify_string({string, Binary}) -> "\"" ++ binary_to_list(Binary) ++ "\"".
stringify_regexp({regexp, Binary}) -> "/" ++ binary_to_list(Binary) ++ "/".

stringify_atom(nil)   -> "nil";
stringify_atom(true)  -> "true";
stringify_atom(false) -> "false".
  
print_error(Error) ->
  io:format("Error: ~p~n", [Error]).