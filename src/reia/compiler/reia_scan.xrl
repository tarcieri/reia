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
Comment = #.*?\n

Rules.

%% Indentation
\n\s+ : build_indentation(TokenLine, TokenLen).

%% Numbers
{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
{Digit}+ : build_integer(TokenChars, TokenLine).

%% Strings
{DoubleQuoted} : build_token(string, TokenChars, TokenLine, TokenLen).
{SingleQuoted} : build_token(string, TokenChars, TokenLine, TokenLen).

%% Regular expressions
{Regexp} : build_token(regexp, TokenChars, TokenLine, TokenLen).

%% Atoms
\~({UpperCase}|{LowerCase}|_)({UpperCase}|{Digit}|{LowerCase}|_)* : build_atom(TokenChars, TokenLine, TokenLen).
\~{DoubleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).
\~{SingleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).

%% Identifiers and constants
{UpperCase}({UpperCase}|{Digit}|{LowerCase}|_)* : build_constant(TokenChars, TokenLine).
({LowerCase}|_)({UpperCase}|{Digit}|{LowerCase}|_)* : build_identifier(TokenChars, TokenLine).

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
=     : {token,{'=',TokenLine}}.
==    : {token,{'==',TokenLine}}.
!=    : {token,{'!=',TokenLine}}.
<     : {token,{'<',TokenLine}}.
>     : {token,{'>',TokenLine}}.
<=    : {token,{'<=',TokenLine}}.
>=    : {token,{'>=',TokenLine}}.
\.\.  : {token,{'..',TokenLine}}.
\|    : {token,{'|',TokenLine}}.
\|\|  : {token,{'or',TokenLine}}.
&&    : {token,{'and',TokenLine}}.
!     : {token,{'not',TokenLine}}.

Erlang code.  

-export([scan/1, scan/2, process_indentation/2]).

%% Scan the string for tokens
scan(String) ->
  scan(String, 1).
  
%% Scan the string for tokens, providing a starting line
scan(String, Line) ->
  case string(String, Line) of
    {ok, Tokens, Lines} ->
      process_indentation(Tokens, Lines);
    Result ->
      Result
  end.

%% Process indentation tokens, converting them to indent/dedent      
process_indentation(Tokens, Lines) ->
  case process_indentation(Tokens, [], [0]) of
    {ok, NewTokens, State} -> 
      {ok, eof_dedents(NewTokens, Lines, State), Lines};
    Result ->
      Result
  end.
  
%% Generate dedents for all remaining indentation levels when EOF is reached
eof_dedents(Tokens, Lines, [0]) ->
  lists:reverse(Tokens);
eof_dedents(Tokens, Lines, [_|State]) ->
  eof_dedents([{dedent, Lines}|Tokens], Lines, State).

%% Indentation processing finished
process_indentation([], Tokens, State) -> 
  {ok, Tokens, State};

%% Ignore blank lines
process_indentation([{indentation, _, _}, Token = {indentation, _, _}|Rest], Tokens, State) ->
  process_indentation([Token|Rest], Tokens, State);
process_indentation([{indentation, Line, _}, Token = {eol, _}|Rest], Tokens, State) ->
  process_indentation([{eol, Line}|Rest], Tokens, State);
process_indentation([Token = {eol, _}, {eol, _}|Rest], Tokens, State) ->
  process_indentation([Token|Rest], Tokens, State);
      
%% Convert EOL tokens to indentation tokens
process_indentation([{eol, Line}|Rest], Tokens, State) ->
  process_indentation([{indentation, Line, 0}|Rest], Tokens, State);
  
%% Handle indents or dedents  
process_indentation([{indentation, Line, Amount}|Rest], Tokens, State = [Current|_]) ->
  if
    Amount == Current ->
      process_indentation(Rest, [{eol, Line - 1}|Tokens], State);
    Amount > Current ->
      process_indentation(Rest, [{indent, Line},{eol, Line - 1}|Tokens], [Amount|State]);
    Amount < Current ->
      case build_dedent([{eol, Line - 1}|Tokens], Amount, Line, State) of
        {ok, NewTokens, NewState} ->
          process_indentation(Rest, NewTokens, NewState);
        nomatch ->
          {error, {Line, ?MODULE,{illegal,dedent}},Line}
      end
  end;
  
%% Pass through all other tokens
process_indentation([Token|Rest], Tokens, State) ->
  process_indentation(Rest, [Token|Tokens], State).
  
build_dedent(Tokens, Amount, Line, [Current|State]) ->
  if
    Amount == Current ->
      {ok, Tokens, [Current|State]};
    Amount > Current ->
      nomatch;  
    true ->
      build_dedent([{dedent, Line}|Tokens], Amount, Line, State)
  end.
  
build_indentation(Line, Length) ->
  {token, {indentation, Line, Length - 1}}.

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

reserved_word('nil')    -> true;
reserved_word('true')   -> true;
reserved_word('false')  -> true;
reserved_word('module') -> true;
reserved_word('def')    -> true;
reserved_word('fun')    -> true;
reserved_word('do')     -> true;
reserved_word('case')   -> true;
reserved_word('else')   -> true;
reserved_word('if')     -> true;
reserved_word('unless') -> true;
reserved_word('and')    -> true;
reserved_word('or')     -> true;
reserved_word('not')    -> true;
reserved_word('in')     -> true;
reserved_word('try')    -> true;
reserved_word('catch')  -> true;
reserved_word(_)        -> false.