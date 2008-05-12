-module(reia_eval).
-export([exprs/2, new_binding/0]).

exprs(Expressions, Binding) ->
  erl_eval:exprs(reia_compiler:compile(Expressions), Binding).
  
new_binding() ->
  erl_eval:new_bindings().