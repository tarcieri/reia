-module(csv).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->
  setup_memo(),
  Result = case 'rows'(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'rows'(Input, Index) ->
  p(Input, Index, 'rows', fun(I,D) -> (p_choose([p_seq([p_label('head', fun 'row'/2), p_label('tail', p_zero_or_more(p_seq([fun 'crlf'/2, fun 'row'/2])))]), p_string("")]))(I,D) end, fun(Node, Idx) -> 
case Node of
  [] -> [];
  [""] -> [];
  _ ->
    Head = proplists:get_value(head, Node),
    Tail = [R || [_,R] <- proplists:get_value(tail, Node)],
    [Head|Tail]
end
 end).

'row'(Input, Index) ->
  p(Input, Index, 'row', fun(I,D) -> (p_choose([p_seq([p_label('head', fun 'field'/2), p_label('tail', p_zero_or_more(p_seq([fun 'field_sep'/2, fun 'field'/2])))]), p_string("")]))(I,D) end, fun(Node, Idx) -> 
case Node of
  [] -> [];
  [""] -> [];
  _ ->
    Head = proplists:get_value(head, Node),
    Tail = [F || [_,F] <- proplists:get_value(tail, Node)],
    [Head|Tail]
end
 end).

'field'(Input, Index) ->
  p(Input, Index, 'field', fun(I,D) -> (p_choose([fun 'quoted_field'/2, p_zero_or_more(p_seq([p_not(p_choose([fun 'field_sep'/2, fun 'crlf'/2])), p_anything()]))]))(I,D) end, fun(Node, Idx) -> lists:flatten(Node) end).

'quoted_field'(Input, Index) ->
  p(Input, Index, 'quoted_field', fun(I,D) -> (p_seq([p_string("\""), p_label('string', p_zero_or_more(p_choose([p_string("\"\""), p_seq([p_not(p_string("\"")), p_anything()])]))), p_string("\"")]))(I,D) end, fun(Node, Idx) -> 
  String = proplists:get_value(string, Node),
  re:replace(String, "[\"]{2}", "\"",[global, {return, list}])
 end).

'field_sep'(Input, Index) ->
  p(Input, Index, 'field_sep', fun(I,D) -> (p_string(","))(I,D) end, fun(Node, Idx) -> Node end).

'crlf'(Input, Index) ->
  p(Input, Index, 'crlf', fun(I,D) -> (p_choose([p_string("\r\n"), p_string("\n")]))(I,D) end, fun(Node, Idx) -> Node end).






p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

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

setup_memo() ->
  put(parse_memo_table, ets:new(?MODULE, [set])).

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

p_eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

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

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

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

p_string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

p_anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
  end.

p_charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

line({{line,L},_}) -> L;
line(_) -> undefined.

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
