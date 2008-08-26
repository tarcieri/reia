-module(reia_visitor).
-export([transform/2]).

transform(Ast, _Fun) ->
  Ast.