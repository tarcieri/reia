-module(reia_compiler).
-export([compile/1]).

compile(Expressions) ->
  compile(Expressions, []).
  
compile([], Output) -> lists:reverse(Output);
compile([Expression|Rest], Output) ->
  NewExpression = ast(Expression),
  compile(Rest, [NewExpression|Output]).
  
ast(Ast = {integer, _, _}) ->
  Ast;
ast(Ast = {float, _, Value}) ->
  Ast;
ast({op, {Op, _}, In}) ->
  reia_operators:ast(Op, In);
ast({op, {Op, _}, In1, In2}) ->
  reia_operators:ast(Op, In1, In2).