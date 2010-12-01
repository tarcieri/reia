-module(neotoma_peg).
-author("Sean Cribbs <seancribbs@gmail.com>").

% Thanks to Jeffrey A. Meunier for the original parser.erl library from which I
% lifted many of these functions, which in turn was based on the Haskell
% "parsec" library by Erik Meijer.  I've renamed the functions to be more
% Erlang-y.

%% @type parse_fun() = function(Input::string(), Index::parse_index()) .
%% @type parse_index() = {{line, integer()},{column,integer()}} .
%% @type parse_result() = ({fail, Reason} | {Result::any(), Remainder::string(), NewIndex::parse_index()}) .

-export([p/4, p/5]).
-export([setup_memo/0, release_memo/0]).

-export([p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]).

%% @doc Memoizing parsing function wrapper.  This form does not transform the result of a successful parse.
%% @see p/5.
p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

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
          Transformed = TransformFun(Result, StartIndex),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          {Transformed, InpRem, NewIndex}
      end
  end.

%% @doc Sets up the packrat memoization table for this parse. Used internally by generated parsers.
%% @spec setup_memo() -> any()
setup_memo() ->
  put(parse_memo_table, ets:new(?MODULE, [set])).

%% @doc Cleans up the packrat memoization table.  Used internally by generated parsers.
release_memo() ->
  ets:delete(memo_table_name()).

memoize(Position, Struct) ->
  ets:insert(memo_table_name(), {Position, Struct}).

get_memo(Position) ->
  case ets:lookup(memo_table_name(), Position) of
    [] -> dict:new();
    [{Position, Dict}] -> Dict
  end.

memo_table_name() ->
    get(parse_memo_table).

%% @doc Generates a parse function that matches the end of the buffer.
%% @spec p_eof() -> parse_fun()
p_eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

%% @doc Generates a parse function that treats the passed parse function as optional.
%% @spec p_optional(parse_fun()) -> parse_fun()
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

%% @doc Generates a parse function that ensures the passed function will not match without consuming any input, that is, negative lookahead.
%% @spec p_not(parse_fun()) -> parse_fun()
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

%% @doc Generates a parse function that ensures the passed function will match, without consuming any input, that is, positive lookahead.
%% @spec p_assert(parse_fun()) -> parse_fun()
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

%% @doc Alias for p_seq/1.
%% @see p_seq/1.
p_and(P) ->
  p_seq(P).

%% @doc Generates a parse function that will match the passed parse functions in order.
%% @spec p_seq([parse_fun()]) -> parse_fun()
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

%% @doc Generates a parse function that will match at least one of the passed parse functions (ordered choice).
%% @spec p_choose([parse_fun()]) -> parse_fun()
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

%% @doc Generates a parse function that will match any number of the passed parse function in sequence (optional greedy repetition).
%% @spec p_zero_or_more(parse_fun()) -> parse_fun()
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

%% @doc Generates a parse function that will match at least one of the passed parse function in sequence (greedy repetition).
%% @spec p_one_or_more(parse_fun()) -> parse_fun()
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

%% @doc Generates a parse function that will tag the result of the passed parse function with a label when it succeeds.  The tagged result will be a 2-tuple of {Tag, Result}.
%% @spec p_label(Tag::anything(), P::parse_fun()) -> parse_fun()
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

%% @doc Generates a parse function that will match the passed string on the head of the buffer.
%% @spec p_string(string()) -> parse_fun()
p_string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

%% @doc Generates a parse function that will match any single character.
%% @spec p_anything() -> parse_fun()
p_anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
  end.

%% @doc Generates a parse function that will match any single character from the passed "class".  The class should be a PCRE-compatible character class in a string.
%%   Examples:
%%     "[a-z]"
%%     "[0-9]"
%%     "[^z]" .
%% @spec p_charclass(string()) -> parse_fun()
p_charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

%% @doc Extracts the line number from the Idx tuple
%% @spec line(parse_index()) -> integer()
line({{line,L},_}) -> L;
line(_) -> undefined.

%% @doc Extracts the column number from the Idx tuple
%% @spec column(parse_index()) -> integer()
column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
  lists:foldl(fun p_advance_index/2, Index, MatchedInput);
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
