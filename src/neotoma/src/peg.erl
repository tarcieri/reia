-module(peg).
-author("Sean Cribbs <seancribbs@gmail.com>").

% Thanks to Jeffrey A. Meunier for the original parser.erl library from which I
% lifted many of these functions, which in turn was based on the Haskell
% "parsec" library by Erik Meijer.  I've renamed the functions to be more
% Erlang-y.

%% @type parse_fun() = function(Input::string(), Index::parse_index()) .
%% @type parse_index() = {{line, integer()},{column,integer()}} .
%% @type parse_result() = ({fail, Reason} | {Result::any(), Remainder::string(), NewIndex::parse_index()}) .

-export([p/4, p/5]).
-export([setup_memo/1, release_memo/0]).

-export([eof/0, optional/1,
         not_/1, assert/1, seq/1,
         and_/1, choose/1,
         zero_or_more/1, one_or_more/1,
         label/2,
         string/1, anything/0,
         charclass/1]).

%% @doc Memoizing parsing function wrapper.  This form does not transform the result of a successful parse.
%% @see p/5.
p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N) -> N end).

%% @doc Memoizing and transforming parsing function wrapper.
%% @spec p(Input::string(), StartIndex::parse_index(), Name::atom(), ParseFun::parse_fun(), TransformFun::transform_fun()) -> parse_result()
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, return the result
    {ok, Result} -> Result;
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp, StartIndex) of
        % If it fails, memoize the failure
        {fail,_} = Failure ->
          memoize(StartIndex, dict:store(Name, Failure, Memo)),
          Failure;
        % If it passes, transform and memoize the result.
        {Result, InpRem, NewIndex} ->
          Transformed = TransformFun(Result),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          {Transformed, InpRem, NewIndex}
      end
  end.

%% @doc Sets up the packrat memoization table for this parse. Used internally by generated parsers.
%% @spec setup_memo(Name::any()) -> any()
setup_memo(Name) ->
  TID = ets:new(Name, [set]),
  put(ets_table, TID).

%% @doc Cleans up the packrat memoization table.  Used internally by generated parsers.
release_memo() ->
  ets:delete(get(ets_table)),
  erase(ets_table).

memoize(Position, Struct) ->
  ets:insert(get(ets_table), {Position, Struct}).

get_memo(Position) ->
  case ets:lookup(get(ets_table), Position) of
    [] -> dict:new();
    [{Position, Dict}] -> Dict
  end.

%% @doc Generates a parse function that matches the end of the buffer.
%% @spec eof() -> parse_fun()
eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

%% @doc Generates a parse function that treats the passed parse function as optional.
%% @spec optional(parse_fun()) -> parse_fun()
optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

%% @doc Generates a parse function that ensures the passed function will not match without consuming any input, that is, negative lookahead.
%% @spec not_(parse_fun()) -> parse_fun()
not_(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

%% @doc Generates a parse function that ensures the passed function will match, without consuming any input, that is, positive lookahead.
%% @spec assert(parse_fun()) -> parse_fun()
assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

%% @doc Alias for seq/1.
%% @see seq/1.
and_(P) ->
  seq(P).

%% @doc Generates a parse function that will match the passed parse functions in order.
%% @spec seq([parse_fun()]) -> parse_fun()
seq(P) ->
  fun(Input, Index) ->
      all(P, Input, Index, [])
  end.

all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

%% @doc Generates a parse function that will match at least one of the passed parse functions (ordered choice).
%% @spec choose([parse_fun()]) -> parse_fun()
choose(Parsers) ->
  fun(Input, Index) ->
      attempt(Parsers, Input, Index, none)
  end.

attempt([], _Input, _Index, Failure) -> Failure;
attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> attempt(Parsers, Input, Index, Failure);
        _ -> attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

%% @doc Generates a parse function that will match any number of the passed parse function in sequence (optional greedy repetition).
%% @spec zero_or_more(parse_fun()) -> parse_fun()
zero_or_more(P) ->
  fun(Input, Index) ->
      scan(P, Input, Index, [])
  end.

%% @doc Generates a parse function that will match at least one of the passed parse function in sequence (greedy repetition).
%% @spec one_or_more(parse_fun()) -> parse_fun()
one_or_more(P) ->
  fun(Input, Index)->
      Result = scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

%% @doc Generates a parse function that will tag the result of the passed parse function with a label when it succeeds.  The tagged result will be a 2-tuple of {Tag, Result}.
%% @spec label(Tag::anything(), P::parse_fun()) -> parse_fun()
label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> scan(P, InpRem, NewIndex, [Result | Accum])
  end.

%% @doc Generates a parse function that will match the passed string on the head of the buffer.
%% @spec string(string()) -> parse_fun()
string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

%% @doc Generates a parse function that will match any single character.
%% @spec anything() -> parse_fun()
anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, advance_index(H, Index)}
  end.

%% @doc Generates a parse function that will match any single character from the passed "class".  The class should be a PCRE-compatible character class in a string.
%%   Examples:
%%     "[a-z]"
%%     "[0-9]"
%%     "[^z]" .
%% @spec charclass(string()) -> parse_fun()
charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
  lists:foldl(fun advance_index/2, Index, MatchedInput);
advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
