%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module(reia_scan).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.

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
    $/  -> $/; 
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
reserved_word('begin')   -> true;
reserved_word('end')     -> true;
reserved_word('case')    -> true;
reserved_word('receive') -> true;
reserved_word('after')   -> true;
reserved_word('when')    -> true;
reserved_word('if')      -> true;
reserved_word('elseif')  -> true;
reserved_word('else')    -> true;
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
format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%%    {ok,Tokens,Line} | {error,ErrorInfo,Line}.

string([], L, [], Ts) ->			%No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
	{A,Alen,Ics1,L1} ->			%Accepting end state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{A,Alen,Ics1,L1,_S1} ->		%After an accepting state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{reject,_Alen,Tlen,_Ics1,L1,_S1} ->
	    {error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
	{A,Alen,_Tlen,_Ics1,L1,_S1} ->
	    string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L1), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%%  Test for and remove the end token wrapper.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(Chars, Line, yystate(), Chars, 0, reject, 0);
token({Line,State,Tcs,Tlen,Action,Alen}, Chars, _) ->
    token(Chars, Line, State, Tcs ++ Chars, Tlen, Action, Alen).

%% token(InChars, Line, State, TokenChars, TokenLen, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

token(Ics0, L0, S0, Tcs, Tlen0, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{L1,S1,Tcs,Alen1,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{L1,S1,Tcs,Tlen1,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,{eof,L1},[]};
	{reject,_Alen1,Tlen1,Ics1,L1,_S1} ->
	    {done,{error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},L1},Ics1};
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1))
    end.

%% tokens_cont(RestChars, Line, Token)
%%  If we have a token or error then return done, else if we have a
%%  skip_token then continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, skip_token) ->
    token(Rest, Line, yystate(), Rest, 0, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(Chars, Line, yystate(), Chars, 0, [], reject, 0);
tokens({tokens,Line,State,Tcs,Tlen,Ts,Action,Alen}, Chars, _) ->
    tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Ts, Action, Alen);
tokens({skip_tokens,Line,State,Tcs,Tlen,Error,Action,Alen}, Chars, _) ->
    skip_tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Error, Action, Alen).

%% tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(Ics0, L0, S0, Tcs, Tlen0, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{tokens,L1,S1,Tcs,Alen1,Ts,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{tokens,L1,S1,Tcs,Tlen1,Ts,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,if Ts == [] -> {eof,L1};
		     true -> {ok,yyrev(Ts),L1} end,[]};
	{reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1+1), L1,
			{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}});
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    tokens_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%%  If we have a end_token or error then return done, else if we have
%%  a token then save it and continue, else if we have a skip_token
%%  just continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%%  Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(Ics, Line, yystate(), Ics, 0, Error, reject, 0).

%% skip_tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(Ics0, L0, S0, Tcs, Tlen0, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Alen1,Error,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Tlen1,Error,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,{error,Error,L1},[]};
	{reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1), L1, Error);
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    skip_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%%  Skip tokens until we have an end_token or error then reutrn done
%%  with the original rror.

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, TokenLen, AcceptAction, AcceptLen) ->
%%      {Action, AcceptLength, RestChars, Line} |         Accepting end state
%%      {Action, AcceptLength, RestChars, Line, State} |  Accepting state
%%      {Action, AcceptLength, TokLength, RestChars, Line, State} |
%%      {reject, AcceptLength, TokLength, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

yystate() -> 71.

yystate(74, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(70, Ics, Line, Tlen+1, 1, Tlen);
yystate(74, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(74, Ics, Line, Tlen+1, 1, Tlen);
yystate(74, Ics, Line, Tlen, _Action, _Alen) ->
    {1,Tlen,Ics,Line,74};
yystate(73, Ics, Line, Tlen, _Action, _Alen) ->
    {19,Tlen,Ics,Line};
yystate(72, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(72, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(56, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(60, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $. ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $[ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(72, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,72};
yystate(71, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(71, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(59, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$!|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(51, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$#|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(23, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$%|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(19, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$&|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(15, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$(|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$)|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(16, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$*|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(20, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$+|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(28, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$,|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(32, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$-|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(36, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(44, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(52, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(62, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(10, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$<|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(6, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(9, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$>|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(25, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$@|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(37, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$[|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(45, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(49, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(57, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [${|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(61, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$||Ics], Line, Tlen, _Action, _Alen) ->
    yystate(65, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [$}|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(73, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(74, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(41, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(57, Ics, Line, Tlen+1, _Action, _Alen);
yystate(71, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,71};
yystate(70, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(66, Ics, Line, Tlen+1, _Action, _Alen);
yystate(70, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,70};
yystate(69, Ics, Line, Tlen, _Action, _Alen) ->
    {46,Tlen,Ics,Line};
yystate(68, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(68, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(64, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(60, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $. ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $[ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(68, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,68};
yystate(67, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, 13, Tlen);
yystate(67, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line, Tlen+1, 13, Tlen);
yystate(67, [$#|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(63, Ics, Line, Tlen+1, 13, Tlen);
yystate(67, Ics, Line, Tlen, _Action, _Alen) ->
    {13,Tlen,Ics,Line,67};
yystate(66, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(66, Ics, Line, Tlen+1, 0, Tlen);
yystate(66, Ics, Line, Tlen, _Action, _Alen) ->
    {0,Tlen,Ics,Line,66};
yystate(65, [$||Ics], Line, Tlen, _Action, _Alen) ->
    yystate(69, Ics, Line, Tlen+1, 45, Tlen);
yystate(65, Ics, Line, Tlen, _Action, _Alen) ->
    {45,Tlen,Ics,Line,65};
yystate(64, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line+1, Tlen+1, 4, Tlen);
yystate(64, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(56, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(60, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(72, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $. ->
    yystate(72, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $[ ->
    yystate(72, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(72, Ics, Line, Tlen+1, 4, Tlen);
yystate(64, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,64};
yystate(63, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, 13, Tlen);
yystate(63, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(63, Ics, Line, Tlen+1, 13, Tlen);
yystate(63, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $ÿ ->
    yystate(63, Ics, Line, Tlen+1, 13, Tlen);
yystate(63, Ics, Line, Tlen, _Action, _Alen) ->
    {13,Tlen,Ics,Line,63};
yystate(62, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(18, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(14, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(14, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(14, Ics, Line, Tlen+1, _Action, _Alen);
yystate(62, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,62};
yystate(61, Ics, Line, Tlen, _Action, _Alen) ->
    {18,Tlen,Ics,Line};
yystate(60, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(60, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(64, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(60, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(68, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $. ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $[ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(72, Ics, Line, Tlen+1, _Action, _Alen);
yystate(60, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,60};
yystate(59, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, 12, Tlen);
yystate(59, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(59, Ics, Line, Tlen+1, 12, Tlen);
yystate(59, [$#|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(55, Ics, Line, Tlen+1, 12, Tlen);
yystate(59, Ics, Line, Tlen, _Action, _Alen) ->
    {12,Tlen,Ics,Line,59};
yystate(58, Ics, Line, Tlen, _Action, _Alen) ->
    {6,Tlen,Ics,Line};
yystate(57, [$!|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(53, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, [$?|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(53, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(57, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(57, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(57, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(57, Ics, Line, Tlen+1, 9, Tlen);
yystate(57, Ics, Line, Tlen, _Action, _Alen) ->
    {9,Tlen,Ics,Line,57};
yystate(56, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line};
yystate(55, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(55, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(55, Ics, Line, Tlen+1, _Action, _Alen);
yystate(55, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $ÿ ->
    yystate(55, Ics, Line, Tlen+1, _Action, _Alen);
yystate(55, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,55};
yystate(54, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(54, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(50, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(46, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(54, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,54};
yystate(53, Ics, Line, Tlen, _Action, _Alen) ->
    {10,Tlen,Ics,Line};
yystate(52, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(72, Ics, Line+1, Tlen+1, 27, Tlen);
yystate(52, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(56, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(60, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(72, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $. ->
    yystate(72, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $[ ->
    yystate(72, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(72, Ics, Line, Tlen+1, 27, Tlen);
yystate(52, Ics, Line, Tlen, _Action, _Alen) ->
    {27,Tlen,Ics,Line,52};
yystate(51, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(47, Ics, Line, Tlen+1, 49, Tlen);
yystate(51, Ics, Line, Tlen, _Action, _Alen) ->
    {49,Tlen,Ics,Line,51};
yystate(50, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line+1, Tlen+1, 6, Tlen);
yystate(50, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(58, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(46, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(42, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(42, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(42, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(42, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, Ics, Line, Tlen, _Action, _Alen) ->
    {6,Tlen,Ics,Line,50};
yystate(49, Ics, Line, Tlen, _Action, _Alen) ->
    {17,Tlen,Ics,Line};
yystate(48, Ics, Line, Tlen, _Action, _Alen) ->
    {44,Tlen,Ics,Line};
yystate(47, Ics, Line, Tlen, _Action, _Alen) ->
    {38,Tlen,Ics,Line};
yystate(46, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(46, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(50, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(46, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(54, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,46};
yystate(45, Ics, Line, Tlen, _Action, _Alen) ->
    {16,Tlen,Ics,Line};
yystate(44, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(48, Ics, Line, Tlen+1, 30, Tlen);
yystate(44, Ics, Line, Tlen, _Action, _Alen) ->
    {30,Tlen,Ics,Line,44};
yystate(43, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(43, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(39, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(35, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,43};
yystate(42, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(42, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(42, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(58, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(46, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(42, Ics, Line, Tlen+1, _Action, _Alen);
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,42};
yystate(41, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(41, Ics, Line, Tlen+1, 8, Tlen);
yystate(41, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(41, Ics, Line, Tlen+1, 8, Tlen);
yystate(41, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(41, Ics, Line, Tlen+1, 8, Tlen);
yystate(41, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(41, Ics, Line, Tlen+1, 8, Tlen);
yystate(41, Ics, Line, Tlen, _Action, _Alen) ->
    {8,Tlen,Ics,Line,41};
yystate(40, Ics, Line, Tlen, _Action, _Alen) ->
    {21,Tlen,Ics,Line};
yystate(39, Ics, Line, Tlen, _Action, _Alen) ->
    {2,Tlen,Ics,Line};
yystate(38, Ics, Line, Tlen, _Action, _Alen) ->
    {7,Tlen,Ics,Line};
yystate(37, Ics, Line, Tlen, _Action, _Alen) ->
    {34,Tlen,Ics,Line};
yystate(36, [$>|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(40, Ics, Line, Tlen+1, 25, Tlen);
yystate(36, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(74, Ics, Line, Tlen+1, 25, Tlen);
yystate(36, Ics, Line, Tlen, _Action, _Alen) ->
    {25,Tlen,Ics,Line,36};
yystate(35, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(35, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(31, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(35, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(27, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,35};
yystate(34, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(34, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,34};
yystate(33, Ics, Line, Tlen, _Action, _Alen) ->
    {23,Tlen,Ics,Line};
yystate(32, Ics, Line, Tlen, _Action, _Alen) ->
    {31,Tlen,Ics,Line};
yystate(31, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line+1, Tlen+1, 2, Tlen);
yystate(31, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(39, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(35, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(43, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(43, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(43, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(43, Ics, Line, Tlen+1, 2, Tlen);
yystate(31, Ics, Line, Tlen, _Action, _Alen) ->
    {2,Tlen,Ics,Line,31};
yystate(30, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line+1, Tlen+1, 7, Tlen);
yystate(30, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(38, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(22, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(22, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(22, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(22, Ics, Line, Tlen+1, 7, Tlen);
yystate(30, Ics, Line, Tlen, _Action, _Alen) ->
    {7,Tlen,Ics,Line,30};
yystate(29, Ics, Line, Tlen, _Action, _Alen) ->
    {43,Tlen,Ics,Line};
yystate(28, Ics, Line, Tlen, _Action, _Alen) ->
    {24,Tlen,Ics,Line};
yystate(27, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(43, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(27, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(31, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(35, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(43, Ics, Line, Tlen+1, _Action, _Alen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,27};
yystate(26, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(26, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(34, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,26};
yystate(25, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(29, Ics, Line, Tlen+1, 40, Tlen);
yystate(25, [$>|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(33, Ics, Line, Tlen+1, 40, Tlen);
yystate(25, Ics, Line, Tlen, _Action, _Alen) ->
    {40,Tlen,Ics,Line,25};
yystate(24, Ics, Line, Tlen, _Action, _Alen) ->
    {29,Tlen,Ics,Line};
yystate(23, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(67, Ics, Line+1, Tlen+1, 11, Tlen);
yystate(23, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(23, Ics, Line, Tlen+1, 11, Tlen);
yystate(23, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $ÿ ->
    yystate(23, Ics, Line, Tlen+1, 11, Tlen);
yystate(23, Ics, Line, Tlen, _Action, _Alen) ->
    {11,Tlen,Ics,Line,23};
yystate(22, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(22, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(38, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(22, Ics, Line, Tlen+1, _Action, _Alen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,22};
yystate(21, Ics, Line, Tlen, _Action, _Alen) ->
    {41,Tlen,Ics,Line};
yystate(20, [$*|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(24, Ics, Line, Tlen+1, 26, Tlen);
yystate(20, Ics, Line, Tlen, _Action, _Alen) ->
    {26,Tlen,Ics,Line,20};
yystate(19, Ics, Line, Tlen, _Action, _Alen) ->
    {28,Tlen,Ics,Line};
yystate(18, Ics, Line, Tlen, _Action, _Alen) ->
    {32,Tlen,Ics,Line};
yystate(17, Ics, Line, Tlen, _Action, _Alen) ->
    {37,Tlen,Ics,Line};
yystate(16, Ics, Line, Tlen, _Action, _Alen) ->
    {15,Tlen,Ics,Line};
yystate(15, [$&|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(11, Ics, Line, Tlen+1, 48, Tlen);
yystate(15, Ics, Line, Tlen, _Action, _Alen) ->
    {48,Tlen,Ics,Line,15};
yystate(14, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(14, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(14, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(14, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(14, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, Ics, Line, Tlen, _Action, _Alen) ->
    {5,Tlen,Ics,Line,14};
yystate(13, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(17, Ics, Line, Tlen+1, 36, Tlen);
yystate(13, Ics, Line, Tlen, _Action, _Alen) ->
    {36,Tlen,Ics,Line,13};
yystate(12, Ics, Line, Tlen, _Action, _Alen) ->
    {14,Tlen,Ics,Line};
yystate(11, Ics, Line, Tlen, _Action, _Alen) ->
    {47,Tlen,Ics,Line};
yystate(10, Ics, Line, Tlen, _Action, _Alen) ->
    {33,Tlen,Ics,Line};
yystate(9, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(13, Ics, Line, Tlen+1, 35, Tlen);
yystate(9, [$>|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(21, Ics, Line, Tlen+1, 35, Tlen);
yystate(9, Ics, Line, Tlen, _Action, _Alen) ->
    {35,Tlen,Ics,Line,9};
yystate(8, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(8, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,8};
yystate(7, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(7, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(3, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,7};
yystate(6, [$-|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(2, Ics, Line, Tlen+1, 39, Tlen);
yystate(6, [$<|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(1, Ics, Line, Tlen+1, 39, Tlen);
yystate(6, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(5, Ics, Line, Tlen+1, 39, Tlen);
yystate(6, Ics, Line, Tlen, _Action, _Alen) ->
    {39,Tlen,Ics,Line,6};
yystate(5, Ics, Line, Tlen, _Action, _Alen) ->
    {42,Tlen,Ics,Line};
yystate(4, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line+1, Tlen+1, 3, Tlen);
yystate(4, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(3, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(7, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(7, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, 3, Tlen);
yystate(4, Ics, Line, Tlen, _Action, _Alen) ->
    {3,Tlen,Ics,Line,4};
yystate(3, Ics, Line, Tlen, _Action, _Alen) ->
    {3,Tlen,Ics,Line};
yystate(2, Ics, Line, Tlen, _Action, _Alen) ->
    {20,Tlen,Ics,Line};
yystate(1, Ics, Line, Tlen, _Action, _Alen) ->
    {22,Tlen,Ics,Line};
yystate(0, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(0, [$'|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(8, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $& ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $(, C =< $[ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%%        {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_float(TokenChars, TokenLine);
yyaction(1, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_integer(TokenChars, TokenLine);
yyaction(2, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_string(string, TokenChars, TokenLine, TokenLen);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_string(string, TokenChars, TokenLine, TokenLen);
yyaction(4, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_string(regexp, TokenChars, TokenLine, TokenLen);
yyaction(5, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_atom(TokenChars, TokenLine, TokenLen);
yyaction(6, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_quoted_atom(TokenChars, TokenLine, TokenLen);
yyaction(7, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_quoted_atom(TokenChars, TokenLine, TokenLen);
yyaction(8, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_constant(TokenChars, TokenLine);
yyaction(9, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_identifier(TokenChars, TokenLine);
yyaction(10, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    build_punctuated_identifier(TokenChars, TokenLine);
yyaction(11, _, _, _) ->
    skip_token;
yyaction(12, _, _, _) ->
    skip_token;
yyaction(13, _, _, TokenLine) ->
    {token,{eol,TokenLine}};
yyaction(14, _, _, TokenLine) ->
    {token,{'(',TokenLine}};
yyaction(15, _, _, TokenLine) ->
    {token,{')',TokenLine}};
yyaction(16, _, _, TokenLine) ->
    {token,{'[',TokenLine}};
yyaction(17, _, _, TokenLine) ->
    {token,{']',TokenLine}};
yyaction(18, _, _, TokenLine) ->
    {token,{'{',TokenLine}};
yyaction(19, _, _, TokenLine) ->
    {token,{'}',TokenLine}};
yyaction(20, _, _, TokenLine) ->
    {token,{'<-',TokenLine}};
yyaction(21, _, _, TokenLine) ->
    {token,{'->',TokenLine}};
yyaction(22, _, _, TokenLine) ->
    {token,{'<<',TokenLine}};
yyaction(23, _, _, TokenLine) ->
    {token,{'>>',TokenLine}};
yyaction(24, _, _, TokenLine) ->
    {token,{'+',TokenLine}};
yyaction(25, _, _, TokenLine) ->
    {token,{'-',TokenLine}};
yyaction(26, _, _, TokenLine) ->
    {token,{'*',TokenLine}};
yyaction(27, _, _, TokenLine) ->
    {token,{'/',TokenLine}};
yyaction(28, _, _, TokenLine) ->
    {token,{'%',TokenLine}};
yyaction(29, _, _, TokenLine) ->
    {token,{'**',TokenLine}};
yyaction(30, _, _, TokenLine) ->
    {token,{'.',TokenLine}};
yyaction(31, _, _, TokenLine) ->
    {token,{',',TokenLine}};
yyaction(32, _, _, TokenLine) ->
    {token,{'::',TokenLine}};
yyaction(33, _, _, TokenLine) ->
    {token,{eol,TokenLine}};
yyaction(34, _, _, TokenLine) ->
    {token,{'@',TokenLine}};
yyaction(35, _, _, TokenLine) ->
    {token,{'=',TokenLine}};
yyaction(36, _, _, TokenLine) ->
    {token,{'==',TokenLine}};
yyaction(37, _, _, TokenLine) ->
    {token,{'===',TokenLine}};
yyaction(38, _, _, TokenLine) ->
    {token,{'!=',TokenLine}};
yyaction(39, _, _, TokenLine) ->
    {token,{'<',TokenLine}};
yyaction(40, _, _, TokenLine) ->
    {token,{'>',TokenLine}};
yyaction(41, _, _, TokenLine) ->
    {token,{'=>',TokenLine}};
yyaction(42, _, _, TokenLine) ->
    {token,{'<=',TokenLine}};
yyaction(43, _, _, TokenLine) ->
    {token,{'>=',TokenLine}};
yyaction(44, _, _, TokenLine) ->
    {token,{'..',TokenLine}};
yyaction(45, _, _, TokenLine) ->
    {token,{'|',TokenLine}};
yyaction(46, _, _, TokenLine) ->
    {token,{'or',TokenLine}};
yyaction(47, _, _, TokenLine) ->
    {token,{'and',TokenLine}};
yyaction(48, _, _, TokenLine) ->
    {token,{'&',TokenLine}};
yyaction(49, _, _, TokenLine) ->
    {token,{'!',TokenLine}};
yyaction(_, _, _, _) -> error.
