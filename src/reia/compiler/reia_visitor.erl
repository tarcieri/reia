-module(reia_visitor).
-export([transform/2]).

transform(Expressions, Fun) ->
  lists:flatten([ast(Expression, Fun) || Expression <- Expressions]).

ast({module, Line, Name, Expressions}, Fun) ->
  Fun({
    module, 
    Line, 
    ast(Name, Fun), 
    lists:flatten([ast(Expression, Fun) || Expression <- Expressions])
  });
ast({function, Line, Name, Arguments, Expressions}, Fun) ->
  Fun({
    function,
    Line,
    ast(Name, Fun),
    lists:flatten([ast(Argument, Fun) || Argument <- Arguments]),
    lists:flatten([ast(Expression, Fun) || Expression <- Expressions])
  });
ast({funcall, Line, Name, Expressions}, Fun) ->
  Fun({
    funcall,
    Line,
    ast(Name, Fun),
    lists:flatten([ast(Expression, Fun) || Expression <- Expressions])
  });
ast({funcall, Line, Module, Name, Expressions}, Fun) ->
  Fun({
    funcall, 
    Line, 
    ast(Module, Fun), 
    ast(Name, Fun), 
    lists:flatten([ast(Expression, Fun) || Expression <- Expressions])
  });
ast(Ast, _Fun) ->
  Ast.