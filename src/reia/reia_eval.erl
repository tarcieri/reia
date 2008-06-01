-module(reia_eval).
-export([exprs/2, new_binding/0]).

exprs(ReiaAst, Binding) ->
  %% io:format("Reia AST: ~p~n", [ReiaAst]),
  Forms = reia_compiler:compile(ReiaAst),
  %% io:format("Erlang AST: ~p~n", [ErlangAst]),
  Value = erl_eval:exprs(Forms, Binding),
  %% io:format("Result: ~p~n", [Value]),
  Value.
  
new_binding() ->
  erl_eval:new_bindings().