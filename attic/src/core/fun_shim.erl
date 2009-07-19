-module(fun_shim).
-export([thunk/2]).

thunk(Fun, 0) ->
  fun() -> Fun({}, nil) end;
thunk(Fun, 1) ->
  fun(A0) -> Fun({A0}, nil) end;
thunk(Fun, 2) ->
  fun(A0, A1) -> Fun({A0, A1}, nil) end.