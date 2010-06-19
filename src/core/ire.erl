%
% ire: Interactive Reia, a Read Eval Print Loop for the Reia language
% Copyright (C)2008 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(ire).
-export([init/0, start/0]).
-include("reia_types.hrl").

init() ->
  io:format("Reia Interactive Shell (prerelease)~nRunning on "),
  user_drv:start('tty_sl -c -e', {ire, start, []}).

start() ->
  reia:init(),
  spawn(fun() -> loop() end).

loop() ->
  loop(reia_eval:new_binding()).

loop(Binding) ->
  NewBinding = try
    parse("", Binding, '>> ')
  catch
    Class:Reason -> print_error(Class, Reason),
    Binding
  end,
  loop(NewBinding).

read(Prompt) ->
  read(standard_io, Prompt).

read(Io, Prompt) ->
  io:get_line(Io, Prompt).
  
parse(Input, Binding, Prompt) ->
  Input2 = [read(Prompt)|Input],
  case reia_parse:string(lists:flatten(lists:reverse(Input2))) of
    {ok, []} -> %% Need more tokens
      parse(Input2, Binding, '.. ');
    {error, {eof, _}} -> %% Need more tokens
      parse(Input2, Binding, '.. ');
    {error, Error} ->
      parse_error(Error),
      Binding;
    {ok, Exprs} ->
      eval(Exprs, Binding)    
  end.

eval(Exprs, Binding) ->
  {value, Value, NewBinding} = reia_eval:exprs(Exprs, Binding),
  print(Value),
  NewBinding.

print(Value) ->
  #reia_string{elements=Elements} = reia_dispatch:call(Value, inspect, {}, nil),
  io:format("=> ~s~n", [iolist_to_binary(Elements)]).

parse_error({Line, Error}) ->
  io:format("Error: Line ~w: ~s~n", [Line, Error]).

print_error(_Class, #reia_object{} = Object) ->
	#reia_string{elements=Elements} = reia:invoke(Object, to_s, {}),
	io:format("~s~n", [iolist_to_binary(Elements)]);
print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50])
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M == erl_eval) or (M == ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).