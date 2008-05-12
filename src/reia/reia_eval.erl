-module(reia_eval).
-export([exprs/2, new_binding/0]).

exprs(ReiaAst, Binding) ->
  ErlangAst = reia_compiler:compile(ReiaAst),
  erl_eval:exprs(ErlangAst, Binding).
  
new_binding() ->
  erl_eval:new_bindings().