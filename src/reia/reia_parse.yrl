Nonterminals
  grammar
  exprs
  expr
  expr_ending
  ending_token
  additive_expr
  multiplicative_expr
  exponential_expr
  unary_expr
  simple_expr
  parenthesized_expr
  number
%  atom
  list
  tail
  tuple
  elements
  .
  
Terminals
  true false nil float integer string regexp eol not
  '+' '-' '*' '**' '/' '%' '~' ';' '(' ')' '[' ']'  ','
  % '.' '&&' '===' '==' '<=' '>=' '<>' 
  % '&=' '^=' '|=' '=' '?' '<<' '>>' '<' '>' 
  % '{' '}' '&' '^' '||' '|' '||='
  % ':' '!' '*=' '/=' '%=' '+=' '-='
  .

Rootsymbol grammar.

grammar -> exprs : '$1'.

%% exprs
exprs -> expr : ['$1'].
exprs -> expr expr_ending : ['$1'].
exprs -> expr expr_ending exprs : ['$1'|'$3'].

%% expr endings
expr_ending -> ending_token : '$1'.
expr_ending -> expr_ending ending_token : '$1'.
ending_token -> ';' : '$1'.
ending_token -> eol : '$1'.

%% exprs
expr -> additive_expr : '$1'.

%% additive operators
additive_expr -> multiplicative_expr : '$1'.
additive_expr -> additive_expr '+' multiplicative_expr : {op, '$2', '$1', '$3'}.
additive_expr -> additive_expr '-' multiplicative_expr : {op, '$2', '$1', '$3'}.

%% multiplicative operators
multiplicative_expr -> exponential_expr : '$1'.
multiplicative_expr -> multiplicative_expr '*' exponential_expr : {op, '$2', '$1', '$3'}.
multiplicative_expr -> multiplicative_expr '/' exponential_expr : {op, '$2', '$1', '$3'}.
multiplicative_expr -> multiplicative_expr '%' exponential_expr : {op, '$2', '$1', '$3'}.

%% exponential operators
exponential_expr -> unary_expr : '$1'.
exponential_expr -> exponential_expr '**' unary_expr : {op, '$2', '$1', '$3'}.

%% unary operators
unary_expr -> simple_expr : '$1'.
unary_expr -> '+' unary_expr : {op, '$1', '$2'}.
unary_expr -> '-' unary_expr : {op, '$1', '$2'}.
unary_expr -> '~' unary_expr : {op, '$1', '$2'}.
unary_expr -> 'not' unary_expr : {op, '$1', '$2'}.

%% simple exprs 
simple_expr -> nil    : '$1'.
simple_expr -> true   : '$1'.
simple_expr -> false  : '$1'.
simple_expr -> number : '$1'.
%simple_expr -> atom   : '$1'.
simple_expr -> string : '$1'.
simple_expr -> regexp : '$1'.
simple_expr -> list   : '$1'.
simple_expr -> tuple  : '$1'.
simple_expr -> parenthesized_expr : '$1'.
parenthesized_expr -> '(' expr ')' : '$2'.

%% number
number -> float : '$1'.
number -> integer : '$1'.

%% atoms
%atom -> '$' identifier : {atom, line('$1'), $2}.
%atom -> '$' string     : {atom, line('$1'), $2}.

%% lists
list -> '[' ']' : {nil,line('$1')}.
list -> '[' expr tail : {cons, line('$1'), '$2', '$3'}.

tail -> ']' : {nil, line('$1')}.
tail -> ',' expr tail : {cons, line('$2'), '$2', '$3'}.

%% tuples
tuple -> '(' ')' : {tuple, line('$1'), []}.
tuple -> '(' expr ',' ')' : {tuple, line('$1'), ['$2']}.
tuple -> '(' expr ',' elements ')': {tuple, line('$1'), ['$2'|'$4']}.

elements -> expr : ['$1'].
elements -> expr ',' elements : ['$1' | '$3'].

Erlang code.

%% keep track of line info in tokens
line(Tup) -> element(2, Tup).