%
% reia_parse: Yecc grammar for the Reia language
% Copyright (C)2008-09 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

Nonterminals
  grammar
  expr_list
  expr
  add_expr
  mult_expr
  pow_expr
  unary_expr
  terminal_expr
  add_op
  mult_op
  pow_op
  unary_op
  number
  .
  
Terminals
  eol '(' ')'
  float integer
  '+' '-' '*' '/' '%' '**'
  .

Rootsymbol grammar.

grammar -> expr_list : '$1'.

%% Expression lists (eol delimited)
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

expr -> add_expr : '$1'.

add_expr -> mult_expr add_op add_expr :   #binary_op{line=line('$1'), type=op('$2'), val1='$1', val2='$3'}.
add_expr -> mult_expr : '$1'.

mult_expr -> pow_expr mult_op mult_expr : #binary_op{line=line('$1'), type=op('$2'), val1='$1', val2='$3'}.
mult_expr -> pow_expr : '$1'.

pow_expr -> unary_expr pow_op pow_expr :  #binary_op{line=line('$1'), type=op('$2'), val1='$1', val2='$3'}.
pow_expr -> unary_expr : '$1'.

unary_expr -> unary_op unary_expr : #unary_op{line=line('$1'), type=op('$1'), val='$2'}.
unary_expr -> terminal_expr : '$1'.

% Terminals
terminal_expr -> number     : '$1'.
terminal_expr -> '(' expr ')' : '$2'.

%% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.

%% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> '%' : '$1'.

%% Exponentation operator
pow_op -> '**' : '$1'.

%% Unary operators
unary_op -> '+'   : '$1'.
unary_op -> '-'   : '$1'.

%% Numbers
number -> float : '$1'.
number -> integer : '$1'.

Erlang code.

-export([string/1]).
-include("reia_nodes.hrl").

%% Easy interface for parsing a given string with nicely formatted errors
string(String) ->
  case reia_scan:string(String) of
    {ok, Tokens, _} -> 
      case reia_parse:parse(Tokens) of
        {ok, Exprs} ->
          {ok, Exprs};
        {error, {Line, _, [Message, Token]}} ->
          {error, {Line, lists:flatten(io_lib:format("~s~s", [Message, Token]))}}
      end;
    {error, {Line, _, {Message, Token}}, _} ->
      {error, {Line, lists:flatten(io_lib:format("~p ~p", [Message, Token]))}}
  end.

%% Keep track of line info in tokens
line(Tup) -> element(2, Tup).

%% Extract operators from op tokens
op({Op, _Line}) ->
  Op.