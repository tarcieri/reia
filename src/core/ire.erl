%
% ire: Interactive Reia, a Read Eval Print Loop for the Reia language
% Copyright (C)2008 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(ire).
-export([init/0, start/0]).

init() ->
  io:format("Reia Interactive Shell (prerelease)~nRunning on "),
  user_drv:start('tty_sl -c -e', {ire, start, []}).

start() ->
  spawn(fun() -> loop() end).

loop() ->
  loop(reia_eval:new_binding()).

  loop(Binding) ->
    case read() of
      eof -> io:format("~n"); % print a newline then exit
      String ->
        NewBinding = try
          parse(String, Binding)
        catch
          Class:Reason -> print_error(Class, Reason),
          Binding
        end,
        loop(NewBinding)
    end.

read() ->
  read('>> ').

read(Prompt) ->
  read(standard_io, Prompt).

read(Io, Prompt) ->
  io:get_line(Io, Prompt).

read_until_complete(Input, Prompt) ->
  Input2 = [read(Prompt)|Input],
  case reia_parse:string(lists:flatten(lists:reverse(Input2))) of
    %% Need more tokens
    {error, {999999, _}} ->
      read_until_complete(Input2, Prompt);
    Result ->
      Result
  end.

parse(String, Binding) ->
  case reia_parse:string(String) of
    {ok, Exprs} ->
      eval(Exprs, Binding);

    %% Need more tokens
    {error, {999999, _}} ->
      case read_until_complete([String], '.. ') of
        {ok, Exprs} ->
          eval(Exprs, Binding);
        {error, Error} ->
          parse_error(Error),
          Binding
      end;

    {error, Error} ->
      parse_error(Error),
      Binding
  end.

eval(Exprs, Binding) ->
  {value, Value, NewBinding} = reia_eval:exprs(Exprs, Binding),
  print(Value),
  NewBinding.

print(Value) ->
  io:format("=> ~p~n", [Value]).
  %{string, String} = reia_dispatch:funcall(Value, inspect, [], nil),
  %io:format("=> ~s~n", [binary_to_list(String)]).

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