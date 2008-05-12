Nonterminals
  Program
  Number
  PrimaryExpression
  SimpleExpression
  ParenthesizedExpression
  UnaryExpression
  ExponentialExpression
  MultiplicativeExpression
  AdditiveExpression
  Expression
  Statement
  OptionalSemicolon
  EmptyStatement
  ExpressionStatement
  Statements
  .
  
Terminals
  float integer not true false nil
  '+' '-' '*' '**' '/' '%' '~' % ':' '!' '*=' '/=' '%=' '+=' '-='
  ';' % '&=' '^=' '|=' '=' '?' '<<' '>>' '<' '>' 
  '(' ')' % '[' ']' '.' ',' '&&' '===' '==' '<=' '>=' '<>' 
%  '{' '}' '&' '^' '||' '|' '||='
  .
  
% Left 100 FunctionExpression.
% Left 100 LiteralField.
% Left 100 PostfixExpression.

Rootsymbol Program.

%% Number
Number -> float : '$1'.
Number -> integer : '$1'.

%% Primary Expressions 
PrimaryExpression -> SimpleExpression : '$1'.
SimpleExpression -> nil : '$1'.
SimpleExpression -> true : '$1'.
SimpleExpression -> false : '$1'.
SimpleExpression -> Number : '$1'.
SimpleExpression -> ParenthesizedExpression : '$1'.
ParenthesizedExpression -> '(' Expression ')' : '$2'.

%% Unary Operators
UnaryExpression -> PrimaryExpression : '$1'.
UnaryExpression -> '+' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> '-' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> '~' UnaryExpression : {op, '$1', '$2'}.
UnaryExpression -> 'not' UnaryExpression : {op, '$1', '$2'}.

%% Exponential Operators
ExponentialExpression -> UnaryExpression : '$1'.
ExponentialExpression -> ExponentialExpression '**' UnaryExpression : {op, '$2', '$1', '$3'}.

%% Multiplicative Operators
MultiplicativeExpression -> ExponentialExpression : '$1'.
MultiplicativeExpression -> MultiplicativeExpression '*' ExponentialExpression : {op, '$2', '$1', '$3'}.
MultiplicativeExpression -> MultiplicativeExpression '/' ExponentialExpression : {op, '$2', '$1', '$3'}.
MultiplicativeExpression -> MultiplicativeExpression '%' ExponentialExpression : {op, '$2', '$1', '$3'}.

%% Additive Operators
AdditiveExpression -> MultiplicativeExpression : '$1'.
AdditiveExpression -> AdditiveExpression '+' MultiplicativeExpression : {op, '$2', '$1', '$3'}.
AdditiveExpression -> AdditiveExpression '-' MultiplicativeExpression : {op, '$2', '$1', '$3'}.

%% Expressions
Expression -> AdditiveExpression : '$1'.

%% Statements
Statement -> EmptyStatement : '$1'.
Statement -> ExpressionStatement OptionalSemicolon : '$1'.
OptionalSemicolon -> ';' : '$1'. 
OptionalSemicolon -> '$empty' : [].

%% Empty Statement 
EmptyStatement -> ';'. 

%% Expression Statement 
ExpressionStatement -> Expression : '$1'.

%% Programs

Program -> Statements : '$1'.
Statements -> '$empty' : [].
Statements -> Statement Statements : ['$1'|'$2'].