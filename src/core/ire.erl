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
      try
        {value, Value, NewBinding} = reia_eval:string(String, Binding),
        print(Value),
				loop(NewBinding)
      catch
        Class:Reason -> print_error(Class, Reason),
        loop(Binding)
      end
  end.

read() ->
  read('>> ').

read(Prompt) ->
  read(standard_io, Prompt).

read(Io, Prompt) ->
  Line = io:get_line(Io, Prompt),

	%% FIXME: Hax until I can figure out what's up with Neotoma
	[$\n|Line2] = lists:reverse(Line),
	lists:reverse(Line2).

print(Value) ->
  io:format("=> ~w~n", [Value]).

print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50])
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).