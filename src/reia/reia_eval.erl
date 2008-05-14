-module(reia_eval).
-export([exprs/2, new_binding/0]).

exprs(ReiaAst, Binding) ->
  %io:format("Reia AST: ~p~n", [ReiaAst]),
  ErlangAst = reia_compiler:compile(ReiaAst),
  %io:format("Erlang AST: ~p~n", [ErlangAst]),
  erl_eval:exprs(ErlangAst, Binding).
  
new_binding() ->
  erl_eval:new_bindings().