-module(reia_eval).
-export([exprs/2, new_binding/0]).

exprs(ReiaAst, Binding) ->
  %% io:format("Reia AST: ~p~n", [ReiaAst]),
  ErlangAst = reia_compiler:compile(ReiaAst),
  %% io:format("Erlang AST: ~p~n", [ErlangAst]),
  Value = erl_eval:exprs(ErlangAst, Binding),
  %% io:format("Result: ~p~n", [Value]),
  Value.
  
new_binding() ->
  erl_eval:new_bindings().