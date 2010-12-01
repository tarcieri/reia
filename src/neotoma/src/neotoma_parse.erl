-module(neotoma_parse).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



escape_quotes(String) ->
  {ok, RE} = re:compile("\""),
  re:replace(String, RE, "\\\\\"", [global, {return, list}]).

add_lhs(Symbol, Index) ->
  case ets:lookup(memo_table_name(), lhs) of
    [] ->
      ets:insert(memo_table_name(), {lhs, [{Symbol,Index}]});
    [{lhs, L}] when is_list(L) ->
      ets:insert(memo_table_name(), {lhs, [{Symbol,Index}|L]})
  end.

add_nt(Symbol, Index) ->
  case ets:lookup(memo_table_name(), nts) of
    [] ->
      ets:insert(memo_table_name(), {nts, [{Symbol,Index}]});
    [{nts, L}] when is_list(L) ->
      case proplists:is_defined(Symbol, L) of
        true ->
          ok;
        _ ->
          ets:insert(memo_table_name(), {nts, [{Symbol,Index}|L]})
      end
  end.

verify_rules() ->
  [{lhs, LHS}] = ets:lookup(memo_table_name(), lhs),
  [{nts, NTs}] = ets:lookup(memo_table_name(), nts),
  [Root|NonRoots] = lists:reverse(LHS),
  lists:foreach(fun({Sym,Idx}) ->
                    case proplists:is_defined(Sym, NTs) of
                      true ->
                        ok;
                      _ ->
                        io:format("neotoma warning: rule '~s' is unused. ~p~n", [Sym,Idx])
                    end
                end, NonRoots),
  lists:foreach(fun({S,I}) ->
                    case proplists:is_defined(S, LHS) of
                      true ->
                        ok;
                      _ ->
                        io:format("neotoma error: nonterminal '~s' has no reduction. (found at ~p) No parser will be generated!~n", [S,I]),
                        exit({neotoma, {no_reduction, list_to_atom(S)}})
                    end
                end, NTs),
    Root.

file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->
  setup_memo(),
  Result = case 'rules'(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'rules'(Input, Index) ->
  p(Input, Index, 'rules', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), fun 'declaration_sequence'/2, p_optional(fun 'space'/2), p_optional(fun 'code_block'/2), p_optional(fun 'space'/2)]))(I,D) end, fun(Node, Idx) -> 
  RootRule = verify_rules(),
  Rules = string:join(lists:nth(2, Node), "\n\n"),
  Code = case lists:nth(4, Node) of
             {code, Block} -> Block;
             _ -> []
         end,
  [{rules, Rules}, {code, Code}, {root, RootRule}, {transform, ets:lookup(memo_table_name(),gen_transform)}]
 end).

'declaration_sequence'(Input, Index) ->
  p(Input, Index, 'declaration_sequence', fun(I,D) -> (p_seq([p_label('head', fun 'declaration'/2), p_label('tail', p_zero_or_more(p_seq([fun 'space'/2, fun 'declaration'/2])))]))(I,D) end, fun(Node, Idx) -> 
  FirstRule = proplists:get_value(head, Node),
  OtherRules =  [lists:last(I) || I <- proplists:get_value(tail, Node, [])],
  [FirstRule|OtherRules]
 end).

'declaration'(Input, Index) ->
  p(Input, Index, 'declaration', fun(I,D) -> (p_seq([fun 'nonterminal'/2, fun 'space'/2, p_string("<-"), fun 'space'/2, fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_optional(fun 'code_block'/2), p_optional(fun 'space'/2), p_string(";")]))(I,D) end, fun(Node, Idx) -> 
  [{nonterminal,Symbol}|Tail] = Node,
  add_lhs(Symbol, Index),
  Transform = case lists:nth(6,Tail) of
                  {code, CodeBlock} -> CodeBlock;
                  _ ->
                      ets:insert_new(memo_table_name(),{gen_transform, true}),
                      "transform('"++Symbol++"', Node, Idx)"
                  end,
  "'"++Symbol++"'"++"(Input, Index) ->\n  " ++
        "p(Input, Index, '"++Symbol++"', fun(I,D) -> ("++
        lists:nth(4, Tail) ++
        ")(I,D) end, fun(Node, Idx) -> "++Transform++" end)."
 end).

'parsing_expression'(Input, Index) ->
  p(Input, Index, 'parsing_expression', fun(I,D) -> (p_choose([fun 'choice'/2, fun 'sequence'/2, fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'choice'(Input, Index) ->
  p(Input, Index, 'choice', fun(I,D) -> (p_seq([p_label('head', fun 'alternative'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, p_string("/"), fun 'space'/2, fun 'alternative'/2])))]))(I,D) end, fun(Node, Idx) ->   
  Tail = [lists:last(S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "p_choose([" ++ string:join(Statements, ", ") ++ "])"
 end).

'alternative'(Input, Index) ->
  p(Input, Index, 'alternative', fun(I,D) -> (p_choose([fun 'sequence'/2, fun 'labeled_primary'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'primary'(Input, Index) ->
  p(Input, Index, 'primary', fun(I,D) -> (p_choose([p_seq([fun 'prefix'/2, fun 'atomic'/2]), p_seq([fun 'atomic'/2, fun 'suffix'/2]), fun 'atomic'/2]))(I,D) end, fun(Node, Idx) -> 
case Node of
  [Atomic, one_or_more] -> "p_one_or_more("++Atomic++")";
  [Atomic, zero_or_more] -> "p_zero_or_more("++Atomic++")";
  [Atomic, optional] -> "p_optional("++Atomic++")";
  [assert, Atomic] -> "p_assert("++Atomic++")";
  [not_, Atomic] -> "p_not("++Atomic++")";
  _ -> Node
end
 end).

'sequence'(Input, Index) ->
  p(Input, Index, 'sequence', fun(I,D) -> (p_seq([p_label('head', fun 'labeled_primary'/2), p_label('tail', p_one_or_more(p_seq([fun 'space'/2, fun 'labeled_primary'/2])))]))(I,D) end, fun(Node, Idx) -> 
  Tail = [lists:nth(2, S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "p_seq(["++ string:join(Statements, ", ") ++ "])"
 end).

'labeled_primary'(Input, Index) ->
  p(Input, Index, 'labeled_primary', fun(I,D) -> (p_seq([p_optional(fun 'label'/2), fun 'primary'/2]))(I,D) end, fun(Node, Idx) -> 
  case hd(Node) of
    [] -> lists:nth(2, Node);
    Label -> "p_label('" ++ Label ++ "', "++lists:nth(2, Node)++")"
  end
 end).

'label'(Input, Index) ->
  p(Input, Index, 'label', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2), p_string(":")]))(I,D) end, fun(Node, Idx) -> 
  String = lists:flatten(Node),
  lists:sublist(String, length(String)-1)
 end).

'suffix'(Input, Index) ->
  p(Input, Index, 'suffix', fun(I,D) -> (p_choose([fun 'repetition_suffix'/2, fun 'optional_suffix'/2]))(I,D) end, fun(Node, Idx) -> 
  case Node of
    "*" -> zero_or_more;
    "+" -> one_or_more;
    "?" -> optional
  end
 end).

'optional_suffix'(Input, Index) ->
  p(Input, Index, 'optional_suffix', fun(I,D) -> (p_string("?"))(I,D) end, fun(Node, Idx) -> Node end).

'repetition_suffix'(Input, Index) ->
  p(Input, Index, 'repetition_suffix', fun(I,D) -> (p_choose([p_string("+"), p_string("*")]))(I,D) end, fun(Node, Idx) -> Node end).

'prefix'(Input, Index) ->
  p(Input, Index, 'prefix', fun(I,D) -> (p_choose([p_string("&"), p_string("!")]))(I,D) end, fun(Node, Idx) -> 
  case Node of
    "&" -> assert;
    "!" -> not_
  end
 end).

'atomic'(Input, Index) ->
  p(Input, Index, 'atomic', fun(I,D) -> (p_choose([fun 'terminal'/2, fun 'nonterminal'/2, fun 'parenthesized_expression'/2]))(I,D) end, fun(Node, Idx) -> 
case Node of
  {nonterminal, Symbol} ->
                add_nt(Symbol, Index), 
                "fun '" ++ Symbol ++ "'/2";
  _ -> Node
end
 end).

'parenthesized_expression'(Input, Index) ->
  p(Input, Index, 'parenthesized_expression', fun(I,D) -> (p_seq([p_string("("), p_optional(fun 'space'/2), fun 'parsing_expression'/2, p_optional(fun 'space'/2), p_string(")")]))(I,D) end, fun(Node, Idx) -> lists:nth(3, Node) end).

'nonterminal'(Input, Index) ->
  p(Input, Index, 'nonterminal', fun(I,D) -> (p_seq([fun 'alpha_char'/2, p_zero_or_more(fun 'alphanumeric_char'/2)]))(I,D) end, fun(Node, Idx) -> {nonterminal, lists:flatten(Node)} end).

'terminal'(Input, Index) ->
  p(Input, Index, 'terminal', fun(I,D) -> (p_choose([fun 'quoted_string'/2, fun 'character_class'/2, fun 'anything_symbol'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'quoted_string'(Input, Index) ->
  p(Input, Index, 'quoted_string', fun(I,D) -> (p_choose([fun 'single_quoted_string'/2, fun 'double_quoted_string'/2]))(I,D) end, fun(Node, Idx) -> "p_string(\""++escape_quotes(lists:flatten(proplists:get_value(string, Node)))++"\")" end).

'double_quoted_string'(Input, Index) ->
  p(Input, Index, 'double_quoted_string', fun(I,D) -> (p_seq([p_string("\""), p_label('string', p_zero_or_more(p_seq([p_not(p_string("\"")), p_choose([p_string("\\\\"), p_string("\\\""), p_anything()])]))), p_string("\"")]))(I,D) end, fun(Node, Idx) -> Node end).

'single_quoted_string'(Input, Index) ->
  p(Input, Index, 'single_quoted_string', fun(I,D) -> (p_seq([p_string("'"), p_label('string', p_zero_or_more(p_seq([p_not(p_string("'")), p_choose([p_string("\\\\"), p_string("\\'"), p_anything()])]))), p_string("'")]))(I,D) end, fun(Node, Idx) -> Node end).

'character_class'(Input, Index) ->
  p(Input, Index, 'character_class', fun(I,D) -> (p_seq([p_string("["), p_label('characters', p_one_or_more(p_seq([p_not(p_string("]")), p_choose([p_seq([p_string("\\\\"), p_anything()]), p_seq([p_not(p_string("\\\\")), p_anything()])])]))), p_string("]")]))(I,D) end, fun(Node, Idx) -> "p_charclass(\"[" ++ escape_quotes(lists:flatten(proplists:get_value(characters, Node))) ++ "]\")" end).

'anything_symbol'(Input, Index) ->
  p(Input, Index, 'anything_symbol', fun(I,D) -> (p_string("."))(I,D) end, fun(Node, Idx) ->  "p_anything()"  end).

'alpha_char'(Input, Index) ->
  p(Input, Index, 'alpha_char', fun(I,D) -> (p_charclass("[A-Za-z_]"))(I,D) end, fun(Node, Idx) -> Node end).

'alphanumeric_char'(Input, Index) ->
  p(Input, Index, 'alphanumeric_char', fun(I,D) -> (p_choose([fun 'alpha_char'/2, p_charclass("[0-9]")]))(I,D) end, fun(Node, Idx) -> Node end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_choose([fun 'white'/2, fun 'comment_to_eol'/2])))(I,D) end, fun(Node, Idx) -> Node end).

'comment_to_eol'(Input, Index) ->
  p(Input, Index, 'comment_to_eol', fun(I,D) -> (p_seq([p_string("%"), p_zero_or_more(p_seq([p_not(p_string("\n")), p_anything()]))]))(I,D) end, fun(Node, Idx) -> Node end).

'white'(Input, Index) ->
  p(Input, Index, 'white', fun(I,D) -> (p_charclass("[ \t\n\r]"))(I,D) end, fun(Node, Idx) -> Node end).

'code_block'(Input, Index) ->
  p(Input, Index, 'code_block', fun(I,D) -> (p_choose([p_seq([p_string("`"), p_label('code', p_one_or_more(p_choose([p_string("\\`"), p_string("$`"), p_seq([p_not(p_string("`")), p_anything()])]))), p_string("`")]), p_string("~")]))(I,D) end, fun(Node, Idx) -> 
   case Node of
       "~" -> {code, "Node"};
       _   -> {code, lists:flatten(proplists:get_value('code', Node))}
   end
 end).






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
