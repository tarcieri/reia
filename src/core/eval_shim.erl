-module(eval_shim).
-export([exprs/3]).

exprs(Expressions, Bindings, LocalFunctionHandler) ->
  % Convert the values of the current bindings back to their Reia form
  Bindings2 = [{Var, reia_erl:e2r(Val)} || {Var, Val} <- Bindings],
  
  % Call erl_eval
  {value, Value, NewBindings} = erl_eval:exprs(Expressions, Bindings2, LocalFunctionHandler),
  
  % Wrap the resulting binding in {tuple, {...}} to prevent clobbering of
  % variable names like 'tuple', 'list', etc.
  {value, Value, [{tuple, {Var, Val}} || {Var, Val} <- NewBindings]}.