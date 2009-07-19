-module(peg_meta_gen).
-export([transform/2]).
-author("Sean Cribbs <seancribbs@gmail.com>").

transform(rules, Node) ->
  verify_rules(),
  Rules = string:join(lists:nth(2, Node), ";\n\n"),
  Rules ++ ".\n";
transform(declaration_sequence, Node) ->
  FirstRule = proplists:get_value(head, Node),
  OtherRules =  [lists:last(I) || I <- proplists:get_value(tail, Node, [])],
  [FirstRule|OtherRules];
transform(declaration, [{nonterminal,Symbol}|Node]) ->
  add_lhs(Symbol),
  "rule("++Symbol++") ->\n  " ++ lists:nth(4, Node);
transform(sequence, Node) ->
  Tail = [lists:nth(2, S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "peg:seq(["++ string:join(Statements, ", ") ++ "])";
transform(choice, Node) ->
  Tail = [lists:last(S) || S <- proplists:get_value(tail, Node)],
  Statements = [proplists:get_value(head, Node)|Tail],
  "peg:choose([" ++ string:join(Statements, ", ") ++ "])";
transform(label, Node) ->
  String = lists:flatten(Node),
  lists:sublist(String, length(String)-1);
transform(labeled_sequence_primary, Node) ->
  case hd(Node) of
    [] -> lists:nth(2, Node);
    Label -> "peg:label('" ++ Label ++ "', "++lists:nth(2, Node)++")"
  end;
transform(single_quoted_string, Node) ->
  transform(double_quoted_string, Node);
transform(double_quoted_string, Node) ->
  "peg:string(\""++escape_quotes(lists:flatten(proplists:get_value(string, Node)))++"\")";
transform(character_class, Node) ->
  "peg:charclass(\"[" ++ escape_quotes(lists:flatten(proplists:get_value(characters, Node))) ++ "]\")";
transform(parenthesized_expression, Node) ->
  lists:nth(3, Node);
transform(atomic, {nonterminal, Symbol}) ->
  add_nt(Symbol),
  "fun " ++ Symbol ++ "/2";
transform(primary, [Atomic, one_or_more]) ->
  "peg:one_or_more("++Atomic++")";
transform(primary, [Atomic, zero_or_more]) ->
  "peg:zero_or_more("++Atomic++")";
transform(primary, [Atomic, optional]) ->
  "peg:optional("++Atomic++")";
transform(primary, [assert, Atomic])->
  "peg:assert("++Atomic++")";
transform(primary, [not_, Atomic]) ->
  "peg:not_("++Atomic++")";
transform(nonterminal, Node) ->
  {nonterminal, lists:flatten(Node)};
transform(anything_symbol, _Node) ->
  "peg:anything()";
transform(suffix, Node) ->
  case Node of
    "*" -> zero_or_more;
    "+" -> one_or_more;
    "?" -> optional
  end;
transform(prefix, Node) ->
  case Node of
    "&" -> assert;
    "!" -> not_
  end;
transform(Rule, Node) when is_atom(Rule) ->
   Node.

escape_quotes(String) ->
  {ok, RE} = re:compile("\""),
  re:replace(String, RE, "\\\\\"", [global, {return, list}]).

add_lhs(Symbol) ->
  case get(lhs) of
    undefined ->
      put(lhs, [Symbol]);
    L when is_list(L) ->
      put(lhs, [Symbol|L])
  end.

add_nt(Symbol) ->
  case get(nts) of
    undefined ->
      put(nts, [Symbol]);
    L when is_list(L) ->
      case lists:member(Symbol, L) of
        true ->
          ok;
        _ ->
          put(nts, [Symbol|L])
      end
  end.

verify_rules() ->
  LHS = erase(lhs),
  NTs = erase(nts),
  NonRoots = tl(lists:reverse(LHS)),
  lists:foreach(fun(L) ->
                    case lists:member(L, NTs) of
                      true ->
                        ok;
                      _ ->
                        io:format("neotoma warning: rule '~s' is unused.~n", [L])
                    end
                end, NonRoots),
  lists:foreach(fun(S) ->
                    case lists:member(S, LHS) of
                      true ->
                        ok;
                      _ ->
                        io:format("neotoma error: symbol '~s' has no reduction. No parser will be generated!~n", [S]),
                        exit({neotoma, {no_reduction, list_to_atom(S)}})
                    end
                end, NTs).
