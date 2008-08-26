-module(reia_visitor).
-export([transform/2]).

transform(Expressions, Fun) ->
  lists:flatten([ast(Expression, Fun) || Expression <- Expressions]).

ast({module, Line, Name, Expressions}, Fun) ->
  Fun({module, Line, ast(Name, Fun), lists:flatten([ast(Expression, Fun) || Expression <- Expressions])});
ast(Ast, _Fun) ->
  Ast.