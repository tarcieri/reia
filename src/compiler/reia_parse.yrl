%
% reia_parse: Yecc grammar for the Reia language
% Copyright (C)2008-09 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

Nonterminals
  grammar
  expr_list
  exprs
  inline_exprs
  expr
  match_expr
  ternary_expr
  bool_expr
  comp_expr
  range_expr
  add_expr
  mult_expr
  pow_expr
  unary_expr
  call_expr
  max_expr
  clauses
  clause
  case_expr
  if_expr
  if_clause
  elseif_clauses
  elseif_clause
  else_clause
  function_identifier
  rebind_op
  bool_op
  comp_op
  add_op
  mult_op
  pow_op
  unary_op
  boolean
  call
  number
  list
  binary
  bin_elements
  bin_element
  bit_size
  bit_type_list
  bit_type_elements
  bit_type
  tail
  tuple
  dict
  dict_entries
  .
  
Terminals
  eol '(' ')' '[' ']' '{' '}'
  float integer string atom regexp true false nil 
  identifier punctuated_identifier erl 'and' 'or' 'not'
  'case' 'when' 'end' 'if' 'unless' 'elseif' 'else'
  '+' '-' '*' '/' '%' '**' ',' '.' '..' 
  '=' '=>' '$' ':' '?' '!' '~' '&' '|' '^' '<<' '>>'
  '===' '==' '!=' '>' '<' '>=' '<='
  '+=' '-=' '*=' '/=' '**=' '&=' '|=' '^=' '<<=' '>>='
  .

Rootsymbol grammar.

grammar -> expr_list : '$1'.
grammar -> '$empty' : [].

%% Expression lists (eol delimited)
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

%% Expression lists (comma delimited)
exprs -> expr : ['$1'].
exprs -> expr eol : ['$1'].
exprs -> eol exprs : '$2'.
exprs -> expr ',' exprs : ['$1'|'$3'].

%% Inline expression lists
inline_exprs -> expr ',' inline_exprs : ['$1'|'$3'].
inline_exprs -> expr : ['$1'].

%% Expressions
expr -> match_expr : '$1'.

match_expr -> ternary_expr '=' match_expr :
  #match{
    line=?line('$2'), 
    left='$1', 
    right='$3'
  }.
match_expr -> ternary_expr rebind_op match_expr :
  #binary_op{
    line=?line('$1'), 
    type=?op('$2'), 
    left='$1', 
    right='$3'
  }.
match_expr -> ternary_expr : '$1'.

ternary_expr -> bool_expr '?' bool_expr ':' bool_expr :
  #ternary_op{
    line=?line('$1'),
    left='$1',
    middle='$3',
    right='$5'
  }.
ternary_expr -> bool_expr : '$1'.

bool_expr -> bool_expr bool_op comp_expr : 
  #binary_op{
    line=?line('$1'), 
    type=?op('$2'), 
    left='$1', 
    right='$3'
  }.
bool_expr -> comp_expr : '$1'.

comp_expr -> range_expr comp_op range_expr : 
  #binary_op{
    line=?line('$1'), 
    type=?op('$2'), 
    left='$1', 
    right='$3'
  }.
comp_expr -> range_expr : '$1'.

range_expr -> range_expr '..' add_expr :
  #range{
    line=?line('$1'), 
    from='$1', 
    to='$3'
  }.
range_expr -> add_expr : '$1'.

add_expr -> add_expr add_op mult_expr :
  #binary_op{
    line=?line('$1'),
    type=?op('$2'),
    left='$1',
    right='$3'
  }.
add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op pow_expr :
  #binary_op{
    line=?line('$1'),
    type=?op('$2'),
    left='$1',
    right='$3'
  }.
mult_expr -> pow_expr : '$1'.

pow_expr -> pow_expr pow_op unary_expr :
  #binary_op{
    line=?line('$1'), 
    type=?op('$2'), 
    left='$1', 
    right='$3'
  }.
pow_expr -> unary_expr : '$1'.

unary_expr -> unary_op unary_expr :
  #unary_op{
    line=?line('$1'),
    type=?op('$1'),
    val='$2'
  }.
unary_expr -> call_expr : '$1'.

call_expr -> call : '$1'.
call_expr -> max_expr : '$1'.

max_expr -> number       : '$1'.
max_expr -> list         : '$1'.
max_expr -> binary       : '$1'.
max_expr -> tuple        : '$1'.
max_expr -> dict         : '$1'.
max_expr -> identifier   : '$1'.
max_expr -> atom         : '$1'.
max_expr -> boolean      : '$1'.
max_expr -> regexp       : '$1'.
max_expr -> string       : interpolate_string('$1').
max_expr -> case_expr    : '$1'.
max_expr -> if_expr    : '$1'.
max_expr -> '(' expr ')' : '$2'.
  
%% Assignment operators
rebind_op -> '+='  : '$1'.
rebind_op -> '-='  : '$1'.
rebind_op -> '*='  : '$1'.
rebind_op -> '/='  : '$1'.
rebind_op -> '**=' : '$1'.
rebind_op -> '&='  : '$1'.
rebind_op -> '|='  : '$1'.
rebind_op -> '^='  : '$1'.
rebind_op -> '<<=' : '$1'.
rebind_op -> '>>=' : '$1'.

%% Boolean operators
bool_op -> 'and' : '$1'.
bool_op -> 'or'  : '$1'.

%% Comparison operators
comp_op -> '===' : '$1'.
comp_op -> '==' : '$1'.
comp_op -> '!=' : '$1'.
comp_op -> '>'  : '$1'.
comp_op -> '<'  : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '<=' : '$1'.

%% Addition operators
add_op -> '+'  : '$1'.
add_op -> '-'  : '$1'.
add_op -> '|'  : '$1'.
add_op -> '^'  : '$1'.
add_op -> '<<' : '$1'.
add_op -> '>>' : '$1'.

%% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> '%' : '$1'.
mult_op -> '&' : '$1'.

%% Exponentation operator
pow_op -> '**' : '$1'.

%% Unary operators
unary_op -> '+'   : '$1'.
unary_op -> '-'   : '$1'.
unary_op -> 'not' : '$1'.
unary_op -> '!'   : '$1'.
unary_op -> '~'   : '$1'.

%% Boolean values
boolean -> true  : '$1'.
boolean -> false : '$1'.
boolean -> nil   : '$1'.

%% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

%% Lists
list -> '[' ']' :       #empty{line=?line('$1')}.
list -> '[' expr tail : #cons{line=?line('$1'), expr='$2', tail='$3'}.

tail -> ']' : #empty{line=?line('$1')}.
tail -> ',' '*' expr ']' : '$3'.
tail -> ',' expr tail : #cons{line=?line('$1'), expr='$2', tail='$3'}.

%% Binaries
binary -> '$' '[' ']' : #binary{line=?line('$1'), elements=[]}.
binary -> '$' '[' bin_elements ']' : #binary{line=?line('$1'), elements='$3'}.
binary -> '$' string :
  #binary{
    line=?line('$1'),
    elements=[#bin_element{line=?line('$1'), expression='$2'}]
  }.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> max_expr bit_size bit_type_list: 
  #bin_element{
    line=?line('$1'), 
    expression='$1', 
    size='$2', 
    type_list='$3'
  }.

bit_size -> ':' max_expr : '$2'.
bit_size -> '$empty' : default.

bit_type_list -> '/' bit_type_elements : '$2'.
bit_type_list -> '$empty' : default.

bit_type_elements -> bit_type '-' bit_type_elements : ['$1'|'$3'].
bit_type_elements -> bit_type : ['$1'].

bit_type -> atom             : element(3, '$1').
bit_type -> atom ':' integer : {element(3, '$1'), element(3,'$3')}.

%% Tuples
tuple -> '(' ')' :               #tuple{line=?line('$1'), elements=[]}.
tuple -> '(' expr ',' ')' :      #tuple{line=?line('$1'), elements=['$2']}.
tuple -> '(' expr ',' exprs ')': #tuple{line=?line('$1'), elements=['$2'|'$4']}.

%% Dicts
dict -> '{' '}' :                #dict{line=?line('$1'), elements=[]}.
dict -> '{' dict_entries '}' :   #dict{line=?line('$1'), elements='$2'}.

dict_entries -> bool_expr '=>' expr : [{'$1','$3'}]. % FIXME: change add_expr to 1 below match
dict_entries -> bool_expr '=>' expr ',' dict_entries : [{'$1','$3'}|'$5'].

%% Function identifiers
function_identifier -> identifier : '$1'.
function_identifier -> punctuated_identifier : '$1'.

%% Function calls
call -> call_expr '.' function_identifier '(' ')' :
  #remote_call{
    line      = ?line('$2'),
    receiver  = '$1',
    name      = ?identifier_name('$3'),
    arguments = [],
    block     = #nil{line=?line('$2')}
  }.

call -> call_expr '.' function_identifier '(' exprs ')' :
  #remote_call{
    line      = ?line('$2'),
    receiver  = '$1',
    name      = ?identifier_name('$3'),
    arguments = '$5',
    block     = #nil{line=?line('$2')}
  }.

call -> erl '.' identifier '(' ')' :
  #native_call{
    line      = ?line('$2'),
    module    = erlang,
    function  = ?identifier_name('$3'),
    arguments = []
  }.

call -> erl '.' identifier '(' exprs ')' :
  #native_call{
    line      = ?line('$2'),
    module    = 'erlang',
    function  = ?identifier_name('$3'),
    arguments = '$5'
  }.

call -> erl '.' identifier '.' identifier '(' ')' :
  #native_call{
    line      = ?line('$2'),
    module    = ?identifier_name('$3'),
    function  = ?identifier_name('$5'),
    arguments = []
  }.

call -> erl '.' identifier '.' identifier '(' exprs ')' :
  #native_call{
    line      = ?line('$2'),
    module    = ?identifier_name('$3'),
    function  = ?identifier_name('$5'),
    arguments = '$7'
  }.

%% Index operation
call -> call_expr '[' expr ']' :
  #binary_op{
    line=?line('$1'),
    type='[]',
    left='$1',
    right='$3'
  }.
  
%% Clauses
clauses -> clause clauses : ['$1'|'$2'].
clauses -> clause : ['$1'].

clause -> when inline_exprs eol expr_list : 
  #clause{
    line=?line('$1'), 
    patterns='$2', 
    exprs='$4'
  }.

%% Case expressions
case_expr -> 'case' expr eol clauses 'end': 
  #'case'{
    line=?line('$1'), 
    expr='$2', 
    clauses='$4'
  }.
  
%% If expressions
if_expr -> if_clause 'end' : 
  #'if'{line=?line('$1'), clauses=['$1']}.
if_expr -> if_clause else_clause 'end' : 
  #'if'{line=?line('$1'), clauses=['$1','$2']}.
if_expr -> if_clause elseif_clauses 'end' :
  #'if'{line=?line('$1'), clauses=['$1'|'$2']}.
if_expr -> if_clause elseif_clauses else_clause 'end' : 
  #'if'{line=?line('$1'), clauses=lists:flatten(['$1','$2','$3'])}.

if_clause -> 'if' expr eol expr_list : 
  #clause{line=?line('$1'), patterns=['$2'], exprs='$4'}.
if_clause -> 'unless' expr eol expr_list : 
  #clause{
    line=?line('$1'), 
    patterns=[#unary_op{line=?line('$1'), type='not', val='$2'}], 
    exprs='$4'
  }.

elseif_clauses -> elseif_clause elseif_clauses : ['$1'|'$2'].
elseif_clauses -> elseif_clause : ['$1'].
elseif_clause  -> elseif expr eol expr_list : 
  #clause{line=?line('$1'), patterns=['$2'], exprs='$4'}.

else_clause    -> else expr_list : 
  #clause{line=?line('$1'), patterns=[#true{line=?line('$1')}], exprs='$2'}.

Erlang code.

-export([string/1]).
-include("reia_nodes.hrl").
-define(line(Node), element(2, Node)).
-define(op(Node), element(1, Node)).
-define(identifier_name(Id), element(3, Id)).

%% Parse a given string with nicely formatted errors
string(String) ->
  case reia_scan:string(String) of
    {ok, Tokens, _} -> 
      case reia_parse:parse(Tokens) of
        {ok, Exprs} ->
          {ok, Exprs};
        {error, {_, _, [Message, []]}} ->
          {error, {eof, lists:flatten([Message, "end of file"])}};
        {error, {Line, _, [Message, Token]}} ->
          {error, {Line, lists:flatten([Message, Token])}}
      end;
    {error, {Line, _, {Message, Token}}, _} ->
      {error, {Line, lists:flatten(io_lib:format("~p ~p", [Message, Token]))}}
  end.
  
%% Interpolate strings, parsing the contents of #{...} tags
interpolate_string(#string{line=Line, characters=Chars}) ->
  interpolate_string(Chars, Line, [], []).

interpolate_string([], Line, CharAcc, ExprAcc) ->
  Result = case CharAcc of
    [] -> lists:reverse(ExprAcc);
    _  -> lists:reverse([#string{line=Line, characters=lists:reverse(CharAcc)}|ExprAcc])
  end,
  case Result of
    [#string{} = Res] -> Res;
    _ -> #dstring{line=Line, elements=Result}
  end;
interpolate_string("#{" ++ String, Line, CharAcc, ExprAcc) ->
  {String2, Expr} = extract_fragment([], String, Line),
  ExprAcc2 = case CharAcc of
    [] -> ExprAcc;
    _  -> [#string{line=Line, characters=lists:reverse(CharAcc)}|ExprAcc]
  end,
  interpolate_string(String2, Line, [], [Expr|ExprAcc2]);
interpolate_string([Char|Rest], Line, CharAcc, ExprAcc) ->
  interpolate_string(Rest, Line, [Char|CharAcc], ExprAcc).

extract_fragment(_Continuation, [], Line) ->
  throw({error, {Line, "unexpected end of interpolated string"}});
extract_fragment(_Continuation, [$"|_], Line) ->
  throw({error, {Line, "invalid quote within interpolated string"}});
extract_fragment(Continuation, [$}|String], Line) ->
  {more, Continuation2} = reia_scan:tokens(Continuation, [$}], Line),
  case Continuation2 of
    {tokens, _, _, _, _, [{'}', _}|Tokens], _, _} ->
      case reia_parse:parse(lists:reverse(Tokens)) of
        {ok, [Expr]} ->
          {String, Expr};
        %% Need more tokens
        {error, {999999, _}} ->
          extract_fragment(Continuation2, String, Line);
        Error ->
          throw(Error)
      end;
    {skip_tokens, _, _, _, _, {_, _, Error}, _, _} ->
      throw({error, {Line, Error}})
  end;
extract_fragment(Continuation, [Char|String], Line) ->
  {more, Continuation2} = reia_scan:tokens(Continuation, [Char], Line),
  extract_fragment(Continuation2, String, Line).