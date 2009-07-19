-module(peg_transform).
-author("Sean Cribbs <seancribbs@gmail.com>").
-export([parse_transform/2]).

% The parse transform for DRYing up parser files.
parse_transform(AST, _Options) ->
  ets:new(peg_transform, [named_table, set]),
  [{attribute, _, module, ModName}] = lists:filter(fun({attribute,_,module,_})
                                                          -> true;
                                                      (_) -> false
                                                       end,
                                                   AST),
  ets:insert(peg_transform, {module, ModName}),
  Result = transform_rules(AST),
  ets:delete(peg_transform),
  Result.

transform_rules(AST) ->
  transform_rules(AST, []).

transform_rules([], Accum) ->
  lists:reverse(Accum);
transform_rules([{function,_Line,rule,1,Clauses}|Rest], Accum) ->
  Rules = lists:reverse(build_rules(Clauses)),
  transform_rules(Rest, Rules++Accum);
transform_rules([H|Rest], Accum) ->
  transform_rules(Rest, [H|Accum]).

build_rules(Clauses) ->
  Rules = [build_rule(Rule) || Rule <- Clauses],
  [generate_file_function(),generate_parse_function()|Rules].

build_rule({clause,Line,[{atom,_,Name}],_,Stmt}) ->
  ets:insert_new(peg_transform, {root, Name, Line}),
  Wrapped = wrap_reductions(Stmt, Name),
  {function,Line,Name,2,
   [{clause,Line,[{var,Line,'Input'},{var,Line,'Index'}],[],Wrapped}]}.

wrap_reductions(Stmt,Name) ->
  Inner = hd(Stmt),
  Line = element(2, Inner), % [{call,Line,_,_}]
  Fun = wrap_fun(Inner, Line),
  Transform = semantic_fun(Name, Line),
  [{call,Line,
    {remote,Line,{atom,Line,peg},{atom,Line,p}},
    [
     {var,Line,'Input'},
     {var,Line,'Index'},
     {atom,Line,Name},
     Fun,
     Transform
    ]}].

wrap_fun(Stmts, Line) when is_tuple(Stmts) ->
  {'fun',Line,
   {clauses,
    [{clause,Line,
      [{var,Line,'I'},{var,Line,'D'}],
      [],
      [{call,Line,
        Stmts,
        [{var,Line,'I'},{var,Line,'D'}]}]}]}};
wrap_fun(Stmts, Line) ->
  io:format("peg rules must be double-arity functions or statements and cannot be sequences of statements!~n"),
  throw({parse_error, not_tuple, Line, Stmts}).

generate_parse_function() ->
  ModName = ets:lookup_element(peg_transform, module, 2),
  [{_, Rule, Line}] = ets:lookup(peg_transform, root),
  {function,Line,parse,1,
   [{clause,Line,
     [{var,Line,'Input'}],
     [],
     [{call,Line,
       {remote,Line,{atom,Line,peg},{atom,Line,setup_memo}},
       [{atom,Line,ModName}]},
      {match,Line,
       {var,Line,'Result'},
       {'case',Line,
        {call,Line,{atom,Line,Rule},[{var,Line,'Input'},{tuple,Line,
                            [{tuple,Line,[{atom,Line,line},{integer,Line,1}]},
                             {tuple,Line,[{atom,Line,column},{integer,Line,1}]}]}]},
        [{clause,Line,[{tuple,Line,[{var,Line,'AST'},{nil,Line},{var,Line, '_Index'}]}],[],[{var,Line,'AST'}]},
         {clause,Line,[{var,Line,'Any'}],[],[{var,Line,'Any'}]}]}},
      {call,Line,{remote,Line,{atom,Line,peg},{atom,Line,release_memo}},[]},
      {var,Line,'Result'}]}]}.

generate_file_function() ->
  [{_, _Rule, Line}] = ets:lookup(peg_transform, root),
  {function,Line,file,1,
   [{clause,Line,
     [{var,Line,'Filename'}],
     [],
     [{match,Line,
       {tuple,Line,[{atom,Line,ok},{var,Line,'Bin'}]},
       {call,Line,
        {remote,Line,{atom,Line,file},{atom,Line,read_file}},
        [{var,Line,'Filename'}]}},
      {call,Line,
       {atom,Line,parse},
       [{call,Line,{atom,Line,binary_to_list},[{var,Line,'Bin'}]}]}]}]}.


semantic_fun(Rule, Line) ->
  {'fun',Line,
   {clauses,
    [{clause,Line,
      [{var,Line,'Node'}],
      [],
      [{call,Line,
        {atom,Line,transform},
        [{atom,Line,Rule},{var,Line,'Node'}]}]}]}}.
