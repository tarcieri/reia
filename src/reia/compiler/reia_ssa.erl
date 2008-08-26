-module(reia_ssa).
-export([ast/1]).

ast(Ast) ->
  reia_visitor:transform(Ast, fun(Node) ->
    Node
  end).