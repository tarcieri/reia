Nonterminals
  Grammar
  Statements
  Statement
  StatementEnding
  EmptyStatement
  ExpressionStatement
  Expression
  AdditiveExpression
  MultiplicativeExpression
  ExponentialExpression
  UnaryExpression
  PrimaryExpression
  SimpleExpression
  ParenthesizedExpression
  Number
  .
  
Terminals
  float integer not true false nil eol
  '+' '-' '*' '**' '/' '%' '~' % ':' '!' '*=' '/=' '%=' '+=' '-='
  ';' % '&=' '^=' '|=' '=' '?' '<<' '>>' '<' '>' 
  '(' ')' % '[' ']' '.' ',' '&&' '===' '==' '<=' '>=' '<>' 
%  '{' '}' '&' '^' '||' '|' '||='
  .

Rootsymbol Grammar.

Grammar -> Statements : '$1'.
Statements -> '$empty' : [].
Statements -> Statements Statement: '$1' ++ ['$2'].

%% Statements
Statement -> EmptyStatement : '$1'.
Statement -> ExpressionStatement StatementEnding : '$1'.
StatementEnding -> 'eol' : '$1'.
StatementEnding -> ';' : '$1'.

%% Empty Statement 
EmptyStatement -> ';'. 

%% Expression Statement 
ExpressionStatement -> Expression : '$1'.

%% Expressions
Expression -> AdditiveExpression : '$1'.

%% Additive Operators
AdditiveExpression -> MultiplicativeExpression : '$1'.
AdditiveExpression -> AdditiveExpression '+' MultiplicativeExpression : {op, '$2', '$1', '$3'}.
AdditiveExpression -> AdditiveExpression '-' MultiplicativeExpression : {op, '$2', '$1', '$3'}.

%% Multiplicative Operators
MultiplicativeExpression -> ExponentialExpression : '$1'.
MultiplicativeExpression -> MultiplicativeExpression '*' ExponentialExpression : {op, '$2', '$1', '$3'}.
MultiplicativeExpression -> MultiplicativeExpression '/' ExponentialExpression : {op, '$2', '$1', '$3'}.
MultiplicativeExpression -> MultiplicativeExpression '%' ExponentialExpression : {op, '$2', '$1', '$3'}.

%% Exponential Operators
ExponentialExpression -> UnaryExpression : '$1'.
ExponentialExpression -> ExponentialExpression '**' UnaryExpression : {op, '$2', '$1', '$3'}.

%% Unary Operators
UnaryExpression -> PrimaryExpression : '$1'.
UnaryExpression -> '+' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> '-' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> '~' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> 'not' UnaryExpression : {op, '$1', '$2'}.

%% Primary Expressions 
PrimaryExpression -> SimpleExpression : '$1'.
SimpleExpression -> nil : '$1'.
SimpleExpression -> true : '$1'.
SimpleExpression -> false : '$1'.
SimpleExpression -> Number : '$1'.
SimpleExpression -> ParenthesizedExpression : '$1'.
ParenthesizedExpression -> '(' Expression ')' : '$2'.

%% Number
Number -> float : '$1'.
Number -> integer : '$1'.