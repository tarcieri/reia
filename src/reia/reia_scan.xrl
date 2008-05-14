Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
DoubleQuoted = "(\\\^.|\\.|[^\"])*"
SingleQuoted = '(\\\^.|\\.|[^\'])*'
Regexp = /(\\\^.|\\.|[^/])*/
Comment = #.*?\n

Rules.

%% Numbers
{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
{Digit}+ : build_integer(TokenChars, TokenLine).

%% Strings
{DoubleQuoted} : build_token(string, TokenChars, TokenLine, TokenLen).
{SingleQuoted} : build_token(string, TokenChars, TokenLine, TokenLen).

%% Regular expressions
{Regexp} : build_token(regexp, TokenChars, TokenLine, TokenLen).

%% Atoms
\$({UpperCase}|{LowerCase}|_)({UpperCase}|{Digit}|{LowerCase}|_)* : build_atom(TokenChars, TokenLine, TokenLen).
\${DoubleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).
\${SingleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).

%% Identifiers and constants
{UpperCase}({UpperCase}|{Digit}|{LowerCase}|_)* : build_constant(TokenChars, TokenLine).
{LowerCase}({UpperCase}|{Digit}|{LowerCase}|_)* : build_identifier(TokenChars, TokenLine).

%% Ignored
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% Special single-character tokens
\n :                  {token,{eol,TokenLine}}.
\* :                  {token,{'*',TokenLine}}.
/ :                   {token,{'/',TokenLine}}.
\+ :                  {token,{'+',TokenLine}}.
- :                   {token,{'-',TokenLine}}.
\^ :                  {token,{'^',TokenLine}}.
& :                   {token,{'&',TokenLine}}.
\| :                  {token,{'|',TokenLine}}.
\< :                  {token,{'<',TokenLine}}.
\> :                  {token,{'>',TokenLine}}.
= :                   {token,{'=',TokenLine}}.
\. :                  {token,{'.',TokenLine}}.
, :                   {token,{',',TokenLine}}.
: :                   {token,{':',TokenLine}}.
! :                   {token,{'!',TokenLine}}.
\? :                  {token,{'?',TokenLine}}.
; :                   {token,{';',TokenLine}}.
\( :                  {token,{'(',TokenLine}}.
\) :                  {token,{')',TokenLine}}.
\{ :                  {token,{'{',TokenLine}}.
} :                   {token,{'}',TokenLine}}.
\[ :                  {token,{'[',TokenLine}}.
\] :                  {token,{']',TokenLine}}.
\% :                  {token,{'%',TokenLine}}.
\~ :                  {token,{'~',TokenLine}}.

%% Special multi-character tokens
<< :                  {token,{'<<',TokenLine}}.
>> :                  {token,{'>>',TokenLine}}.
== :                  {token,{'==',TokenLine}}.
<> :                  {token,{'<>',TokenLine}}.
=== :                 {token,{'===',TokenLine}}.
&& :                  {token,{'&&',TokenLine}}.
\|\| :                {token,{'||',TokenLine}}.
\*= :                 {token,{'*=',TokenLine}}.
/= :                  {token,{'/=',TokenLine}}.
\%= :                 {token,{'%=',TokenLine}}.
\+= :                 {token,{'+=',TokenLine}}.
-= :                  {token,{'-=',TokenLine}}.
&= :                  {token,{'&=',TokenLine}}.
\^= :                 {token,{'^=',TokenLine}}.
\|= :                 {token,{'|=',TokenLine}}.
\|\|= :               {token,{'||=',TokenLine}}.
<= :                  {token,{'<=',TokenLine}}.
>= :                  {token,{'>=',TokenLine}}.
\*\* :                {token,{'**',TokenLine}}.

Erlang code.  

-export([build_integer/2, build_float/2]).

build_integer(Chars, Line) ->
  {token, {integer, Line, list_to_integer(Chars)}}.
  
build_float(Chars, Line) ->
  {token, {float, Line, list_to_float(Chars)}}.
    
build_token(Type, Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 2), 
  {token, {Type, Line, String}}.
  
build_atom(Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 1),
  {token, {atom, Line, list_to_atom(String)}}.
  
build_quoted_atom(Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 2),
  build_atom(String, Line, Len - 2).
  
build_constant(Chars, Line) ->
  Atom = list_to_atom(Chars),
  {token, {constant, Line, Atom}}.
  
build_identifier(Chars, Line) ->  
    Atom = list_to_atom(Chars),
    case reserved_word(Atom) of
        true -> {token, {Atom, Line}};
        false -> {token, {identifier, Line, Atom}}
    end.

reserved_word('nil') -> true;
reserved_word('true') -> true;
reserved_word('false') -> true;
reserved_word(_) -> false.