-module(reia_eval).
-export([exprs/1]).

exprs(Expressions) ->
  erl_eval:exprs(reia_compiler:compile(Expressions), erl_eval:new_bindings()).