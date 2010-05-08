%
% reia_parse: Yecc grammar for the Reia language
% Copyright (C)2008-10 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for delist_tails.
%

Nonterminals
  grammar
  expr_list
  exprs
  inline_exprs
  expr
  inline_if_expr
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
  block
  block_args
  clauses
  clause
  case_expr
  if_expr
  if_clause
  elseif_clauses
  elseif_clause
  else_clause
  try_expr
  catch_clauses
  catch_clause
  function_identifier
  bound_var
  rebind_op
  bool_op
  comp_op
  add_op
  mult_op
  pow_op
  unary_op
  module_decl
  class_decl
  functions
  function
  pargs
  pargs_tail
  block_capture
  boolean
  class_inst
  call
  number
  list
  list_tail
  splat
  list_comprehension
  lc_exprs
  lc_expr
  binary
  bin_elements
  bin_element
  bit_size
  bit_type_list
  bit_type_elements
  bit_type
  lambda
  tuple
  dict
  dict_entries
  .
  
Terminals
  '(' ')' '[' ']' '{' '}' '<[' ']>' def eol
  float integer string atom regexp true false nil 
  module class module_name identifier punctuated_identifier erl 
  'case' 'when' 'end' 'if' 'unless' 'elseif' 'else' fun do
  'and' 'or' 'not' 'try' 'catch' for in
  '+' '-' '*' '/' '%' '**' ',' '.' '..' 
  '=' '=>' ':' '?' '!' '~' '&' '|' '^' '<<' '>>'
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
expr -> inline_if_expr : '$1'.

inline_if_expr -> match_expr 'if' match_expr :
  #'if'{line=?line('$2'), clauses=[
    #clause{line=?line('$2'), patterns=['$3'], exprs=['$1']}
  ]}.
inline_if_expr -> match_expr 'unless' match_expr :
  #'if'{line=?line('$2'), clauses=[
    #clause{
      line     = ?line('$2'), 
      patterns = [#unary_op{line=?line('$1'), type='not', expr='$3'}],
      exprs    = ['$1']
    }
  ]}.
inline_if_expr -> match_expr : '$1'.

match_expr -> ternary_expr '=' match_expr :
  #match{
    line  = ?line('$2'), 
    left  = '$1', 
    right = '$3'
  }.
match_expr -> ternary_expr rebind_op ternary_expr:
  #binary_op{
    line  = ?line('$1'), 
    type  = ?op('$2'), 
    left  = '$1', 
    right = '$3'
  }.
match_expr -> ternary_expr : '$1'.

ternary_expr -> bool_expr '?' bool_expr ':' ternary_expr :
  #'if'{line=?line('$1'), clauses=[
    #clause{line=?line('$1'), patterns=['$1'], exprs=['$3']},
    #clause{line=?line('$1'), patterns=[#true{line=?line('$1')}], exprs=['$5']}
  ]}.
  
ternary_expr -> bool_expr : '$1'.

bool_expr -> bool_expr bool_op comp_expr : 
  #binary_op{
    line  = ?line('$1'), 
    type  = ?op('$2'), 
    left  = '$1', 
    right = '$3'
  }.
bool_expr -> comp_expr : '$1'.

comp_expr -> range_expr comp_op range_expr : 
  #binary_op{
    line  = ?line('$1'), 
    type  = ?op('$2'), 
    left  = '$1', 
    right = '$3'
  }.
comp_expr -> range_expr : '$1'.

range_expr -> range_expr '..' add_expr :
  #range{
    line = ?line('$1'), 
    from = '$1', 
    to   = '$3'
  }.
range_expr -> add_expr : '$1'.

add_expr -> add_expr add_op mult_expr :
  #binary_op{
    line  = ?line('$1'),
    type  = ?op('$2'),
    left  = '$1',
    right = '$3'
  }.
add_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op pow_expr :
  #binary_op{
    line  = ?line('$1'),
    type  = ?op('$2'),
    left  = '$1',
    right = '$3'
  }.
mult_expr -> pow_expr : '$1'.

pow_expr -> pow_expr pow_op unary_expr :
  #binary_op{
    line  = ?line('$1'), 
    type  = ?op('$2'), 
    left  = '$1', 
    right = '$3'
  }.
pow_expr -> unary_expr : '$1'.

unary_expr -> unary_op unary_expr :
  #unary_op{
    line = ?line('$1'),
    type = ?op('$1'),
    expr = '$2'
  }.
unary_expr -> call_expr : '$1'.

call_expr -> class_inst : '$1'.
call_expr -> call       : '$1'.
call_expr -> max_expr   : '$1'.

max_expr -> number       : '$1'.
max_expr -> list         : '$1'.
max_expr -> binary       : '$1'.
max_expr -> lambda       : '$1'.
max_expr -> tuple        : '$1'.
max_expr -> dict         : '$1'.
max_expr -> atom         : '$1'.
max_expr -> boolean      : '$1'.
max_expr -> regexp       : '$1'.
max_expr -> case_expr    : '$1'.
max_expr -> if_expr      : '$1'.
max_expr -> try_expr     : '$1'.
max_expr -> module_name  : '$1'.
max_expr -> module_decl  : '$1'.
max_expr -> class_decl   : '$1'.
max_expr -> bound_var    : '$1'.
max_expr -> identifier   : #var{line=?line('$1'), name=?identifier_name('$1')}.
max_expr -> string       : interpolate_string('$1').
max_expr -> list_comprehension : '$1'.
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

%% Module declaration
module_decl -> module module_name eol 'end' : 
  #module{
    line      = ?line('$1'), 
    name      = element(3, '$2'), 
    functions = []
  }.
module_decl -> module module_name eol functions 'end' : 
  #module{
    line      = ?line('$1'), 
    name      = element(3, '$2'), 
    functions = '$4'
  }.
  
%% Class declaration
class_decl -> class module_name eol 'end' : 
  #class{
    line    = ?line('$1'), 
    name    = ?identifier_name('$2'), 
    methods = []
  }.
class_decl -> class module_name eol functions 'end' : 
  #class{
    line    = ?line('$1'), 
    name    = ?identifier_name('$2'), 
    methods = '$4'
  }.

%% Parenthesized arguments
pargs -> '(' ')'                 : #pargs{}.
pargs -> '(' eol ')'             : #pargs{}.
pargs -> '(' expr pargs_tail     : ?pargs_add('$2', '$3').
pargs -> '(' eol expr pargs_tail : ?pargs_add('$3', '$4').
pargs -> '(' block_capture ')'   : '$2'.

pargs_tail -> ',' expr pargs_tail         : ?pargs_add('$2', '$3').
pargs_tail -> ',' eol expr pargs_tail     : ?pargs_add('$3', '$4').
pargs_tail -> eol ',' expr pargs_tail     : ?pargs_add('$3', '$4').
pargs_tail -> eol ',' eol expr pargs_tail : ?pargs_add('$4', '$5').

pargs_tail -> ')'     : #pargs{}.
pargs_tail -> eol ')' : #pargs{}.
pargs_tail -> ',' block_capture ')'     : '$2'.
pargs_tail -> eol ',' block_capture ')' : '$3'.

block_capture -> '&' expr         : #pargs{block='$2'}.
block_capture -> '&' expr eol     : #pargs{block='$2'}.
block_capture -> eol '&' expr     : #pargs{block='$3'}.
block_capture -> eol '&' expr eol : #pargs{block='$3'}.
    
%% Functions
functions -> function : ['$1'].
functions -> function eol : ['$1'].
functions -> eol functions : '$2'.
functions -> function eol functions : ['$1'|'$3'].

%% Function identifiers
function_identifier -> identifier : '$1'.
function_identifier -> punctuated_identifier : '$1'.

%% Function definitions
function -> def function_identifier eol expr_list 'end' : 
  #function{
    line = ?line('$1'), 
    name = element(3, '$2'), 
    body = '$4'
  }.
function -> def function_identifier pargs eol expr_list 'end' :
  #function{
    line  = ?line('$1'), 
    name  = element(3, '$2'), 
    args  = '$3'#pargs.args,
    block = '$3'#pargs.block,
    body  = '$5'
  }.

%% Class instantiations
class_inst -> module_name pargs :
  #class_inst{
    line  = ?line('$1'),
    class = ?identifier_name('$1'),
    args  = '$2'#pargs.args,
    block = ?pargs_default_block(#nil{}, '$2')
  }.

%% Local function calls
call -> function_identifier pargs : 
  #local_call{
    line  = ?line('$1'), 
    name  = ?identifier_name('$1'),
    args  = '$2'#pargs.args,
    block = ?pargs_default_block(#nil{}, '$2')
  }.
  
%% Local function calls with blocks
call -> function_identifier block : 
  #local_call{
    line  = ?line('$2'), 
    name  = ?identifier_name('$1'), 
    block = '$2'
  }.
call -> function_identifier pargs block :
  case '$2'#pargs.block of
    #var{line=1, name='_'} -> % user didn't pass a &block
      #local_call{
        line  = ?line('$1'), 
        name  = ?identifier_name('$1'), 
        args  = '$2'#pargs.args, 
        block = '$3'
      };
    _ ->
      throw({error, {?line('$1'), "both block arg and actual block given"}})
  end.

%% Remote function calls
call -> call_expr '.' function_identifier '(' ')' :
  #remote_call{
    line     = ?line('$2'),
    receiver = '$1',
    name     = ?identifier_name('$3')
  }.
call -> call_expr '.' function_identifier '(' exprs ')' :
  #remote_call{
    line     = ?line('$2'),
    receiver = '$1',
    name     = ?identifier_name('$3'),
    args     = '$5'
  }.

%% Remote function calls with blocks
call -> call_expr '.' function_identifier block :
  #remote_call{
    line     = ?line('$2'), 
    receiver = '$1', 
    name     = ?identifier_name('$3'), 
    block    = '$4'
  }.
call -> call_expr '.' function_identifier '(' ')' block :
  #remote_call{
    line     = ?line('$2'), 
    receiver = '$1', 
    name     = ?identifier_name('$3'), 
    block    = '$6'
  }. 
call -> call_expr '.' function_identifier '(' exprs ')' block :
  #remote_call{
    line     = ?line('$2'), 
    receiver = '$1', 
    name     = ?identifier_name('$3'),
    args     = '$5', 
    block    = '$7'
  }.
  
%% Blocks
block -> '{' expr_list '}' : 
  #lambda{
    line=?line('$1'), 
    body='$2'
  }.
block -> '{' '|' block_args '|' expr_list '}' :
  #lambda{
    line=?line('$1'), 
    args='$3', 
    body='$5'
  }.
block -> do expr_list 'end' :
  #lambda{
    line=?line('$1'),
    body='$2'
  }. 
block -> do '|' block_args '|' expr_list 'end' :
  #lambda{
    line=?line('$1'), 
    args='$3',
    body='$5'
  }.

block_args -> max_expr : ['$1'].
block_args -> max_expr ',' block_args : ['$1'|'$3'].

%% Native Erlang function calls
call -> erl '.' identifier '(' ')' :
  #native_call{
    line      = ?line('$2'),
    module    = erlang,
    function  = ?identifier_name('$3')
  }.
call -> erl '.' identifier '(' exprs ')' :
  #native_call{
    line      = ?line('$2'),
    module    = 'erlang',
    function  = ?identifier_name('$3'),
    args      = '$5'
  }.
call -> erl '.' identifier '.' identifier '(' ')' :
  #native_call{
    line      = ?line('$2'),
    module    = ?identifier_name('$3'),
    function  = ?identifier_name('$5')
  }.
call -> erl '.' identifier '.' identifier '(' exprs ')' :
  #native_call{
    line      = ?line('$2'),
    module    = ?identifier_name('$3'),
    function  = ?identifier_name('$5'),
    args      = '$7'
  }.
  
%% Boolean values
boolean -> true  : '$1'.
boolean -> false : '$1'.
boolean -> nil   : '$1'.

%% Numbers
number -> float   : '$1'.
number -> integer : '$1'.

%% Lists
list -> '[' ']'                : #empty{line=?line('$1')}.
list -> '[' eol ']'            : #empty{line=?line('$1')}.
list -> '[' expr list_tail     : #cons{line=?line('$1'), expr='$2', tail='$3'}.
list -> '[' eol expr list_tail : #cons{line=?line('$1'), expr='$3', tail='$4'}.

list_tail -> ',' expr list_tail         : #cons{line=?line('$1'), expr='$2', tail='$3'}.
list_tail -> ',' eol expr list_tail     : #cons{line=?line('$1'), expr='$3', tail='$4'}.
list_tail -> eol ',' expr list_tail     : #cons{line=?line('$1'), expr='$3', tail='$4'}.
list_tail -> eol ',' eol expr list_tail : #cons{line=?line('$1'), expr='$4', tail='$5'}.

list_tail -> ']'     : #empty{line=?line('$1')}.
list_tail -> eol ']' : #empty{line=?line('$1')}.

list_tail -> ',' splat ']'     : '$2'.
list_tail -> eol ',' splat ']' : '$3'.

splat -> '*' expr         : '$2'.
splat -> '*' expr eol     : '$2'.
splat -> eol '*' expr     : '$3'.
splat -> eol '*' expr eol : '$3'.

%% List comprehensions
list_comprehension -> '[' expr for lc_exprs ']' : 
  #lc{
    line=?line('$1'), 
    expr='$2', 
    generators='$4'
  }.

lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs: ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr 'in' expr : 
  #generate{
    line=?line('$2'), 
    pattern='$1', 
    source='$3'
  }.

%% Binaries
binary -> '<[' ']>' : #binary{line=?line('$1'), elements=[]}.
binary -> '<[' bin_elements ']>' : #binary{line=?line('$1'), elements='$2'}.

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

%% Lambdas
lambda -> fun '{' expr_list '}' :
  #lambda{
    line = ?line('$1'), 
    body = '$3'
  }.
lambda -> fun '(' ')' '{' expr_list '}' : 
  #lambda{
    line = ?line('$1'),
    body = '$5'
  }.
lambda -> fun '(' exprs ')' '{' expr_list '}' :
  #lambda{
    line = ?line('$1'), 
    args = '$3',
    body = '$6'
  }.
lambda -> fun do expr_list 'end' :
  #lambda{
    line = ?line('$1'),
    body = '$3'
  }.
lambda -> fun '(' ')' do expr_list 'end' :
  #lambda{
    line = ?line('$1'), 
    body = '$5'
  }.
lambda -> fun '(' exprs ')' do expr_list 'end' :
  #lambda{
    line = ?line('$1'), 
    args = '$3', 
    body = '$6'
  }.

%% Tuples
tuple -> '(' ')' :               #tuple{line=?line('$1'), elements=[]}.
tuple -> '(' expr ',' ')' :      #tuple{line=?line('$1'), elements=['$2']}.
tuple -> '(' expr ',' exprs ')': #tuple{line=?line('$1'), elements=['$2'|'$4']}.

%% Dicts
dict -> '{' '}' :                #dict{line=?line('$1'), elements=[]}.
dict -> '{' dict_entries '}' :   #dict{line=?line('$1'), elements='$2'}.

dict_entries -> bool_expr '=>' expr : [{'$1','$3'}]. % FIXME: change add_expr to 1 below match
dict_entries -> bool_expr '=>' expr ',' dict_entries : [{'$1','$3'}|'$5'].

%% Bound variables
bound_var -> '^' identifier : #bound_var{line=?line('$1'), name=element(3, '$2')}.

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
    patterns=[#unary_op{line=?line('$1'), type='not', expr='$2'}], 
    exprs='$4'
  }.

elseif_clauses -> elseif_clause elseif_clauses : ['$1'|'$2'].
elseif_clauses -> elseif_clause : ['$1'].
elseif_clause  -> elseif expr eol expr_list : 
  #clause{line=?line('$1'), patterns=['$2'], exprs='$4'}.

else_clause    -> else expr_list : 
  #clause{line=?line('$1'), patterns=[#true{line=?line('$1')}], exprs='$2'}.
  
%% Try expressions
try_expr -> 'try' expr_list catch_clauses 'end' :
  #'try'{
    line    = ?line('$1'), 
    body    = '$2', 
    clauses = '$3'
  }.

catch_clauses -> catch_clause catch_clauses : ['$1'|'$2'].
catch_clauses -> catch_clause : ['$1'].

catch_clause -> 'catch' expr eol expr_list :
  #'catch'{
    line    = ?line('$1'), 
    pattern = '$2', 
    body    = '$4'
  }.

Erlang code.

-export([string/1]).
-include("reia_nodes.hrl").
-record(pargs, {args=[], block={var,1,'_'}}).
-define(line(Node), element(2, Node)).
-define(op(Node), element(1, Node)).
-define(identifier_name(Id), element(3, Id)).
-define(pargs_add(Arg, Pargs), Pargs#pargs{args=[Arg|Pargs#pargs.args]}).
-define(pargs_default_block(Block, Pargs), case (Pargs)#pargs.block of {var,1,'_'} -> Block; _ -> (Pargs)#pargs.block end).

%% Parse a given string with nicely formatted errors
string(String) ->
  try
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
    end
  catch {error, {_Line, _Message}} = Error ->
    Error
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
    [] -> #string{line=Line, characters=""};
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
        {error, {_, _, [_, []]}} ->
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
