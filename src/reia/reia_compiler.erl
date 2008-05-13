-module(reia_compiler).
-export([compile/1]).

compile(Expressions) ->
  compile(Expressions, []).
  
compile([], Output) -> lists:reverse(Output);
compile([Expression|Rest], Output) ->
  NewExpression = ast(Expression),
  compile(Rest, [NewExpression|Output]).

ast({nil, Line}) ->
  {atom, Line, nil};
ast({true, Line}) ->
  {atom, Line, true};
ast({false, Line}) ->
  {atom, Line, false};  
ast(Ast = {integer, _, _}) ->
  Ast;
ast(Ast = {float, _, _}) ->
  Ast;
ast(Ast = {string, Line, String}) ->
  {tuple, Line, [
    {atom, Line, string},
    {bin, Line, [{bin_element, Line, {string, Line, String}, default, default}]}
  ]};
ast({regexp, Line, Pattern}) ->
  {tuple, Line, [
    {atom, Line, regexp}, 
    {bin, Line, [{bin_element, Line, {string, Line, Pattern}, default, default}]}
  ]};
ast({op, {Op, Line}, In}) ->
  reia_operators:ast(Op, Line, ast(In));
ast({op, {Op, Line}, In1, In2}) ->
  reia_operators:ast(Op, Line, ast(In1), ast(In2)).