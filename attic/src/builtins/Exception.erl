%
% Exception: Methods for the Exception builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Exception').
-export([funcall/3]).

funcall({exception, {Class, Reason}}, inspect, []) ->
  'Str':from_list(format_error(Class, Reason));
funcall(Constant, to_s, []) ->
  funcall(Constant, inspect, []).
  
format_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF).