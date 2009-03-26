-module(eval_shim).
-export([exprs/2]).

exprs(Expressions, Bindings) ->
  % Convert the values of the current bindings back to their Reia form
  Bindings2 = [{Var, reia_erl:e2r(Val)} || {Var, Val} <- Bindings],
  
  % Call erl_eval
  {value, Value, NewBindings} = erl_eval:exprs(Expressions, Bindings2, {value, fun local_function/2}),
  
  % Wrap the resulting binding in {tuple, {...}} to prevent clobbering of
  % variable names like 'tuple', 'list', etc.
  {value, Value, [{tuple, {Var, Val}} || {Var, Val} <- NewBindings]}.
  
local_function(Name, [Args, Block]) ->
  case Name of
    'puts' ->
      ['Main':puts({Arg}, Block) || Arg <- list_to_tuple(Args)];
    _ ->
      'Main':Name(Args, Block)
  end.