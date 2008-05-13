Nonterminals
  grammar
  statements
  statement
  statement_ending
  expression
  additive_expression
  multiplicative_expression
  exponential_expression
  unary_expression
  simple_expression
  parenthesized_expression
  number
  .
  
Terminals
  float integer not true false nil eol
  '+' '-' '*' '**' '/' '%' '~' % ':' '!' '*=' '/=' '%=' '+=' '-='
  ';' % '&=' '^=' '|=' '=' '?' '<<' '>>' '<' '>' 
  '(' ')' % '[' ']' '.' ',' '&&' '===' '==' '<=' '>=' '<>' 
%  '{' '}' '&' '^' '||' '|' '||='
  .

Rootsymbol grammar.

grammar -> statements : '$1'.

statements -> statement : ['$1'].
statements -> statement statement_ending : ['$1'].
statements -> statement statement_ending statements : ['$1'|'$3'].

%% statement endings
statement_ending -> ';' : '$1'.
statement_ending -> eol : '$1'.

%% statements
statement -> expression : '$1'.

%% expressions
expression -> additive_expression : '$1'.

%% additive operators
additive_expression -> multiplicative_expression : '$1'.
additive_expression -> additive_expression '+' multiplicative_expression : {op, '$2', '$1', '$3'}.
additive_expression -> additive_expression '-' multiplicative_expression : {op, '$2', '$1', '$3'}.

%% multiplicative operators
multiplicative_expression -> exponential_expression : '$1'.
multiplicative_expression -> multiplicative_expression '*' exponential_expression : {op, '$2', '$1', '$3'}.
multiplicative_expression -> multiplicative_expression '/' exponential_expression : {op, '$2', '$1', '$3'}.
multiplicative_expression -> multiplicative_expression '%' exponential_expression : {op, '$2', '$1', '$3'}.

%% exponential operators
exponential_expression -> unary_expression : '$1'.
exponential_expression -> exponential_expression '**' unary_expression : {op, '$2', '$1', '$3'}.

%% unary operators
unary_expression -> simple_expression : '$1'.
unary_expression -> '+' unary_expression : {op, '$1', '$2'}.
unary_expression -> '-' unary_expression : {op, '$1', '$2'}.
unary_expression -> '~' unary_expression : {op, '$1', '$2'}.
unary_expression -> 'not' unary_expression : {op, '$1', '$2'}.

%% simple expressions 
simple_expression -> nil : '$1'.
simple_expression -> true : '$1'.
simple_expression -> false : '$1'.
simple_expression -> number : '$1'.
simple_expression -> parenthesized_expression : '$1'.
parenthesized_expression -> '(' expression ')' : '$2'.

%% number
number -> float : '$1'.
number -> integer : '$1'.