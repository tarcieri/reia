%
% reia_scan: Leex scanner for the Reia language
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
DoubleQuoted = "(\\\^.|\\.|[^\"])*"
SingleQuoted = '(\\\^.|\\.|[^\'])*'
Regexp = /(\\\^.|\\.|[^/])*/
Comment = #.*

Rules.

%% Numbers
-?{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
-?{Digit}+ : build_integer(TokenChars, TokenLine).

%% Strings
{DoubleQuoted} : build_string(string, TokenChars, TokenLine, TokenLen).
{SingleQuoted} : build_string(string, TokenChars, TokenLine, TokenLen).

%% Regular expressions
{Regexp} : build_string(regexp, TokenChars, TokenLine, TokenLen).

%% Atoms
\~({UpperCase}|{LowerCase}|_)({UpperCase}|{Digit}|{LowerCase}|_)* : build_atom(TokenChars, TokenLine, TokenLen).
\~{DoubleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).
\~{SingleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).

%% Identifiers and constants
{UpperCase}({UpperCase}|{LowerCase}|{Digit}|_)* : build_constant(TokenChars, TokenLine).
({LowerCase}|_)({UpperCase}|{LowerCase}|{Digit}|_)* : build_identifier(TokenChars, TokenLine).
({LowerCase}|_)({UpperCase}|{LowerCase}|{Digit}|_)*[?!] : build_punctuated_identifier(TokenChars, TokenLine).

%% Ignored
{Comment} : skip_token.
{Whitespace}+ : skip_token.

\n    : {token,{eol,TokenLine}}.
\(    : {token,{'(',TokenLine}}.
\)    : {token,{')',TokenLine}}.
\[    : {token,{'[',TokenLine}}.
\]    : {token,{']',TokenLine}}.
\{    : {token,{'{',TokenLine}}.
}     : {token,{'}',TokenLine}}.
<-    : {token,{'<-',TokenLine}}.
->    : {token,{'->',TokenLine}}.
<<    : {token,{'<<',TokenLine}}.
>>    : {token,{'>>',TokenLine}}.
\+    : {token,{'+',TokenLine}}.
-     : {token,{'-',TokenLine}}.
\*    : {token,{'*',TokenLine}}.
/     : {token,{'/',TokenLine}}.
\%    : {token,{'%',TokenLine}}.
\*\*  : {token,{'**',TokenLine}}.
\.    : {token,{'.',TokenLine}}.
,     : {token,{',',TokenLine}}.
:     : {token,{':',TokenLine}}.
::    : {token,{'::',TokenLine}}.
;     : {token,{';',TokenLine}}.
@     : {token,{'@',TokenLine}}.
=     : {token,{'=',TokenLine}}.
==    : {token,{'==',TokenLine}}.
===   : {token,{'===',TokenLine}}.
!=    : {token,{'!=',TokenLine}}.
<     : {token,{'<',TokenLine}}.
>     : {token,{'>',TokenLine}}.
<=    : {token,{'<=',TokenLine}}.
>=    : {token,{'>=',TokenLine}}.
\.\.  : {token,{'..',TokenLine}}.
\|    : {token,{'|',TokenLine}}.
\|\|  : {token,{'or',TokenLine}}.
&&    : {token,{'and',TokenLine}}.
!     : {token,{'!',TokenLine}}.

Erlang code.  

build_integer([$-|Chars], Line) ->
  {token, {integer, Line, -list_to_integer(Chars)}};
build_integer(Chars, Line) ->
  {token, {integer, Line, list_to_integer(Chars)}}.

build_float([$-|Chars], Line) ->
  {token, {float, Line, -list_to_float(Chars)}};
build_float(Chars, Line) ->
  {token, {float, Line, list_to_float(Chars)}}.
  
build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2)), 
  {token, {Type, Line, String}}.
  
unescape_string(String) -> unescape_string(String, []).

unescape_string([], Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $\" -> $\";
    $\' -> $\';
    $b  -> $\b;
    $d  -> $\d;
    $e  -> $\e;
    $f  -> $\f;
    $n  -> $\n;
    $r  -> $\r;
    $s  -> $\s;
    $t  -> $\t;
    $v  -> $\v;
    _   -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).
  
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
    
build_punctuated_identifier(Chars, Line) ->
  Atom = list_to_atom(Chars),
  {token, {punctuated_identifier, Line, Atom}}.

reserved_word('nil')     -> true;
reserved_word('true')    -> true;
reserved_word('false')   -> true;
reserved_word('module')  -> true;
reserved_word('class')   -> true;
reserved_word('def')     -> true;
reserved_word('fun')     -> true;
reserved_word('do')      -> true;
reserved_word('end')     -> true;
reserved_word('case')    -> true;
reserved_word('receive') -> true;
reserved_word('after')   -> true;
reserved_word('when')    -> true;
reserved_word('else')    -> true;
reserved_word('if')      -> true;
reserved_word('unless')  -> true;
reserved_word('and')     -> true;
reserved_word('or')      -> true;
reserved_word('not')     -> true;
reserved_word('for')     -> true;
reserved_word('in')      -> true;
reserved_word('try')     -> true;
reserved_word('catch')   -> true;
reserved_word('throw')   -> true;
reserved_word(_)         -> false.