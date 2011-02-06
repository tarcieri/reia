-module(json).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(Bin).

parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
    setup_memo(),
    Result = case 'json_value'(Input,{{line,1},{column,1}}) of
                 {AST, <<>>, _Index} -> AST;
                 Any -> Any
             end,
    release_memo(), Result.

'json_value'(Input, Index) ->
    p(Input, Index, 'json_value', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), p_choose([fun 'object'/2, fun 'array'/2, fun 'string'/2, fun 'number'/2, fun 'true'/2, fun 'false'/2, fun 'null'/2]), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) -> lists:nth(2, Node) end).

'object'(Input, Index) ->
    p(Input, Index, 'object', fun(I,D) -> (p_choose([p_seq([p_string(<<"{">>), p_optional(fun 'space'/2), p_label('head', fun 'pair'/2), p_label('tail', p_zero_or_more(p_seq([p_optional(fun 'space'/2), p_string(<<",">>), p_optional(fun 'space'/2), fun 'pair'/2]))), p_optional(fun 'space'/2), p_string(<<"}">>)]), p_seq([p_string(<<"{">>), p_optional(fun 'space'/2), p_string(<<"}">>)])]))(I,D) end, fun(Node, Idx) ->
                                                                                                                                                                                                                                                                                                                                                                                                                        case length(Node) of
                                                                                                                                                                                                                                                                                                                                                                                                                            3 -> {struct, []};
                                                                                                                                                                                                                                                                                                                                                                                                                            _ ->
                                                                                                                                                                                                                                                                                                                                                                                                                                Head = proplists:get_value(head, Node),
                                                                                                                                                                                                                                                                                                                                                                                                                                Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
                                                                                                                                                                                                                                                                                                                                                                                                                                {struct, [Head|Rest]}
                                                                                                                                                                                                                                                                                                                                                                                                                        end
                                                                                                                                                                                                                                                                                                                                                                                                                end).

'pair'(Input, Index) ->
    p(Input, Index, 'pair', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), p_label('key', fun 'string'/2), p_optional(fun 'space'/2), p_string(<<":">>), p_optional(fun 'space'/2), p_label('value', fun 'json_value'/2), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) -> {proplists:get_value(key, Node), proplists:get_value(value, Node)} end).

'array'(Input, Index) ->
    p(Input, Index, 'array', fun(I,D) -> (p_choose([p_seq([p_string(<<"[">>), p_optional(fun 'space'/2), p_label('head', fun 'json_value'/2), p_label('tail', p_zero_or_more(p_seq([p_optional(fun 'space'/2), p_string(<<",">>), p_optional(fun 'space'/2), fun 'json_value'/2]))), p_optional(fun 'space'/2), p_string(<<"]">>)]), p_seq([p_string(<<"[">>), p_optional(fun 'space'/2), p_string(<<"]">>)])]))(I,D) end, fun(Node, Idx) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                   case length(Node) of
                                                                                                                                                                                                                                                                                                                                                                                                                                       3 -> [];
                                                                                                                                                                                                                                                                                                                                                                                                                                       _ ->
                                                                                                                                                                                                                                                                                                                                                                                                                                           Head = proplists:get_value(head, Node),
                                                                                                                                                                                                                                                                                                                                                                                                                                           Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
                                                                                                                                                                                                                                                                                                                                                                                                                                           [Head|Rest]
                                                                                                                                                                                                                                                                                                                                                                                                                                   end
                                                                                                                                                                                                                                                                                                                                                                                                                           end).

'string'(Input, Index) ->
    p(Input, Index, 'string', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('chars', p_zero_or_more(p_seq([p_not(p_string(<<"\"">>)), p_choose([p_string(<<"\\\\\\\\">>), p_string(<<"\\\\\"">>), p_anything()])]))), p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) -> iolist_to_binary(proplists:get_value(chars, Node)) end).

'number'(Input, Index) ->
    p(Input, Index, 'number', fun(I,D) -> (p_seq([fun 'int'/2, p_optional(fun 'frac'/2), p_optional(fun 'exp'/2)]))(I,D) end, fun(Node, Idx) ->
                                                                                                                                      case Node of
                                                                                                                                          [Int, [], []] -> list_to_integer(binary_to_list(iolist_to_binary(Int)));
                                                                                                                                          [Int, Frac, []] -> list_to_float(binary_to_list(iolist_to_binary([Int, Frac])));
                                                                                                                                          [Int, [], Exp] -> list_to_float(binary_to_list(iolist_to_binary([Int, ".0", Exp])));
                                                                                                                                          _ -> list_to_float(binary_to_list(iolist_to_binary(Node)))
                                                                                                                                      end
                                                                                                                              end).

'int'(Input, Index) ->
    p(Input, Index, 'int', fun(I,D) -> (p_choose([p_seq([p_optional(p_string(<<"-">>)), p_seq([fun 'non_zero_digit'/2, p_one_or_more(fun 'digit'/2)])]), fun 'digit'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'frac'(Input, Index) ->
    p(Input, Index, 'frac', fun(I,D) -> (p_seq([p_string(<<".">>), p_one_or_more(fun 'digit'/2)]))(I,D) end, fun(Node, Idx) -> Node end).

'exp'(Input, Index) ->
    p(Input, Index, 'exp', fun(I,D) -> (p_seq([fun 'e'/2, p_one_or_more(fun 'digit'/2)]))(I,D) end, fun(Node, Idx) -> Node end).

'e'(Input, Index) ->
    p(Input, Index, 'e', fun(I,D) -> (p_seq([p_charclass(<<"[eE]">>), p_optional(p_choose([p_string(<<"+">>), p_string(<<"-">>)]))]))(I,D) end, fun(Node, Idx) -> Node end).

'non_zero_digit'(Input, Index) ->
    p(Input, Index, 'non_zero_digit', fun(I,D) -> (p_charclass(<<"[1-9]">>))(I,D) end, fun(Node, Idx) -> Node end).

'digit'(Input, Index) ->
    p(Input, Index, 'digit', fun(I,D) -> (p_charclass(<<"[0-9]">>))(I,D) end, fun(Node, Idx) -> Node end).

'true'(Input, Index) ->
    p(Input, Index, 'true', fun(I,D) -> (p_string(<<"true">>))(I,D) end, fun(Node, Idx) -> true end).

'false'(Input, Index) ->
    p(Input, Index, 'false', fun(I,D) -> (p_string(<<"false">>))(I,D) end, fun(Node, Idx) -> false end).

'null'(Input, Index) ->
    p(Input, Index, 'null', fun(I,D) -> (p_string(<<"null">>))(I,D) end, fun(Node, Idx) -> null end).

'space'(Input, Index) ->
    p(Input, Index, 'space', fun(I,D) -> (p_zero_or_more(p_charclass(<<"[ \t\n\s\r]">>)))(I,D) end, fun(Node, Idx) -> Node end).








p(Inp, Index, Name, ParseFun) ->
    p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
    % Grab the memo table from ets
    Memo = get_memo(StartIndex),
    % See if the current reduction is memoized
    case proplists:lookup(Name, Memo) of
        % If it is, return the result
        {Name, Result} -> Result;
        % If not, attempt to parse
        _ ->
            case ParseFun(Inp, StartIndex) of
                % If it fails, memoize the failure
                {fail,_} = Failure ->
                    memoize(StartIndex, [{Name, Failure}|Memo]),
                    Failure;
                % If it passes, transform and memoize the result.
                {Result, InpRem, NewIndex} ->
                    Transformed = TransformFun(Result, StartIndex),
                    memoize(StartIndex, [{Name, {Transformed, InpRem, NewIndex}}|Memo]),
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
        [] -> [];
        [{Position, PList}] -> PList
    end.

memo_table_name() ->
    get(parse_memo_table).

p_eof() ->
    fun(<<>>, Index) -> {eof, [], Index};
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

p_string(S) when is_list(S) -> p_string(list_to_binary(S));
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
            try
                <<S:Length/binary, Rest/binary>> = Input,
                {S, Rest, p_advance_index(S, Index)}
            catch
                error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
            end
    end.

p_anything() ->
    fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
       (Input, Index) when is_binary(Input) ->
            <<C/utf8, Rest/binary>> = Input,
            {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
    end.

p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, Class}, Index}}
            end
    end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
    lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
    {{line, Line}, {column, Col}} = Index,
    case MatchedInput of
        $\n -> {{line, Line+1}, {column, 1}};
        _ -> {{line, Line}, {column, Col+1}}
    end.
