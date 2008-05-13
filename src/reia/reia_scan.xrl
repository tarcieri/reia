Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
Quote = '(\\\^.|\\.|[^\'])*'
String = "(\\\^.|\\.|[^\"])*"
Comment = #.*?\n

Rules.   

{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
{Digit}+ : build_integer(TokenChars, TokenLine).
{String} : build_string(TokenChars, TokenLine, TokenLen).
{Quote} : build_string(TokenChars, TokenLine, TokenLen).
({UpperCase}|{LowerCase}|_|\$)({UpperCase}|{Digit}|{LowerCase}|_|\$)* : build_identifier(TokenChars, TokenLine).
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% special characters and single character operators
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

%% multi character operators
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
  
build_string(Chars, Line, Len) ->
  S = lists:sublist(Chars, 2, Len - 2), 
  {token, {string, Line, S}}.
  
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