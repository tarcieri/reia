-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

parse(S) ->
    io:format("Parsing string '~s'~n",[S]),
    reia_parse:parse(S).

p_empty_test() ->                           parse("").
p_empty_parse_test() ->                     [] = parse("").
p_space_parse_test() ->                     [] = parse(" ").
p_two_space_parse_test() ->                 [] = parse("  ").
p_tab_parse_test() ->                       [] = parse("\t").
p_space_tab_parse_test() ->                 [] = parse(" \t").
p_space_tab_space_parse_test() ->           [] = parse(" \t ").
p_two_space_nl_parse_test() ->              [] = parse("  \n").
p_two_space_crlf_parse_test() ->            [] = parse("  \r\n").
p_two_space_nl_space_parse_test() ->        [] = parse("  \n ").
p_two_space_crlf_space_parse_test() ->      [] = parse("  \r\n ").
p_space_tab_space_nl_parse_test() ->        [] = parse(" \t \n").
p_space_tab_space_nl_tab_parse_test() ->    [] = parse(" \t \n\t").
p_one_comment_eof_parse_test() ->           [] = parse("# comment 1").
p_one_comment_parse_test() ->               [] = parse(" # comment 1\n").
p_two_comments_eof_parse_test() ->          [] = parse("\n# comment 1\n# comment 2").
p_two_comments_parse_test() ->              [] = parse(" \n # comment 1\n# comment 2\n").

root_test_() ->
  [ % 'grammar' and 'exp_list'
    % NOTE: Depends on 'integer' already working
    ?_assertEqual([],                       parse("")),
    ?_assertEqual([],                       parse(" ")),
    ?_assertEqual([],                       parse(" \n ")),
    ?_assertEqual([{integer,1,12}],         parse("12 ")),
    ?_assertEqual([{integer,2,12}],         parse(" \n12 ")),
    ?_assertEqual([{integer,1,12}],         parse("12 \n ")),
    ?_assertEqual([{integer,2,12}],         parse(" \n12\n ")),
    ?_assertEqual([{integer,1,12},{integer,3,23}],
                                            parse("12 \n\n 23 ")),
    ?_assertEqual([{integer,2,12},{integer,4,23}],
                                            parse("\n12\n \n 23\n ")),
    ?_assert(true)
  ].

expr_test_() ->    % NOTE: All depend on 'integer' already working
    % each group of tests consists of:
    % a precedence test checking that one of the group is of lower precedence than the group above
    % an associativity test (where associativity is defined) with a circular relationship of the elements
    
  [ % inline_if
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,7}],[{integer,1,1}]}]}],
                                            parse(" 1 if 7 ")),
    ?_assertEqual([{'if',1,[{clause,1,[{unary_op,1,'not',{integer,1,7}}],
                                      [{integer,1,1}] }] }],
                                            parse(" 1 unless 7 ")),
    % match
    ?_assertEqual([{match,1,{integer,1,1},
                            {match,1,{integer,1,2},
                                     {integer,1,3}}}],                  
                                            parse(" 1 = 2 = 3")),
    % rebind
    ?_assertEqual([{match,1,{integer,1,3},{binary_op,1,'+=',{integer,1,1},{integer,1,2}}}],  
                                            parse("3 = 1 += 2")),
    ?_assertEqual([{binary_op,1,'+=',{integer,1,1},{integer,1,2}}],  parse("1 += 2")),
    ?_assertEqual([{binary_op,1,'-=',{integer,1,1},{integer,1,2}}],  parse("1 -= 2")),
    ?_assertEqual([{binary_op,1,'*=',{integer,1,1},{integer,1,2}}],  parse("1 *= 2")),
    ?_assertEqual([{binary_op,1,'/=',{integer,1,1},{integer,1,2}}],  parse("1 /= 2")),
    ?_assertEqual([{binary_op,1,'**=',{integer,1,1},{integer,1,2}}], parse("1 **= 2")),
    ?_assertEqual([{binary_op,1,'&=',{integer,1,1},{integer,1,2}}],  parse("1 &= 2")),
    ?_assertEqual([{binary_op,1,'|=',{integer,1,1},{integer,1,2}}],  parse("1 |= 2")),
    ?_assertEqual([{binary_op,1,'^=',{integer,1,1},{integer,1,2}}],  parse("1 ^= 2")),
    ?_assertEqual([{binary_op,1,'<<=',{integer,1,1},{integer,1,2}}], parse("1 <<= 2")),
    ?_assertEqual([{binary_op,1,'>>=',{integer,1,1},{integer,1,2}}], parse("1 >>= 2")),
    ?_assertEqual([{binary_op,1,'>>=',{integer,1,1},{integer,2,2}}], parse("1 >>= \n 2")),

    % ternary
    ?_assertEqual([{match,1,
                    {'if',1,[{clause,1,[{integer,1,1}],
                                     [{integer,1,2}]},
                           {clause,1,[{true,1}],
                                     [{integer,1,3}]}
                          ]
                    },
                    {'if',1,[{clause,1,[{integer,1,4}],
                                     [{integer,1,5}]},
                           {clause,1,[{true,1}],
                                     [{integer,1,6}]}
                          ]
                     }
                   }],
                                                                    parse("1 ? 2 : 3 = 4 ? 5 : 6 ")),
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],
                                      [{integer,1,2}]},
                            {clause,1,[{true,1}
                           ],
                           [{'if',1,[{clause,1,[{integer,1,3}],
                                               [{integer,1,4}]},
                                     {clause,1,[{true,1}],
                                               [{integer,1,5}]}]}]}]}],
                                                                    parse("1 ? 2 : 3 ? 4 : 5 ")),
    % send
    ?_assertEqual([{'if',1,[{clause,1,[{send,1,{integer,1,1},{integer,1,2}}],
                                      [{send,1,{integer,1,3},{integer,1,4}}]},
                            {clause,1,[{true,1}],
                                      [{send,1,{integer,1,5},{integer,1,6}}]}
                           ]}],
                                                                    parse("1 ! 2 ? 3 ! 4 : 5 ! 6 ")),
    ?_assertEqual([{send,1,{integer,1,1},{send,1,{integer,1,2},{integer,1,3}}}],
                                                                    parse("1 ! 2 ! 3")),
    % or ||
    ?_assertEqual([{send,1,{binary_op,1,'or',{integer,1,1},{integer,1,2}},
                           {binary_op,1,'or',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 or 2 ! 3 or 4")),
    ?_assertEqual([{binary_op,1,'or',{binary_op,1,'or',{binary_op,1,'or',{integer,1,1},
                                                                         {integer,1,2}},
                                                       {integer,1,3}},
                                     {integer,1,4}}],
                                                                    parse("1 or 2 || 3 or 4")),
    % and
    ?_assertEqual([{binary_op,1,'or',{binary_op,1,'and',{integer,1,1},{integer,1,2}},
                                     {binary_op,1,'and',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 and 2 || 3 && 4")),
    ?_assertEqual([{binary_op,1,'and',{binary_op,1,'and',{binary_op,1,'and',{integer,1,1},
                                                                            {integer,1,2}},
                                                         {integer,1,3}},
                                      {integer,1,4}}],
                                                                    parse("1 and 2 && 3 and 4")),
    % orelse
    % andif
    % compares
    ?_assertEqual([{binary_op,1,'and',{binary_op,1,'==',{integer,1,1},{integer,1,2}},
                                      {binary_op,1,'==',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 == 2 and 3 == 4")),
    ?_assertEqual([{binary_op,1,'==',{binary_op,1,'===',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 === 2 == 3")),
    ?_assertEqual([{binary_op,1,'!=',{binary_op,1,'==',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 == 2 != 3")),
    ?_assertEqual([{binary_op,1,'>=',{binary_op,1,'!=',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 != 2 >= 3")),
    ?_assertEqual([{binary_op,1,'>',{binary_op,1,'>=',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 >= 2 > 3")),
    ?_assertEqual([{binary_op,1,'<=',{binary_op,1,'>',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 > 2 <= 3")),
    ?_assertEqual([{binary_op,1,'<',{binary_op,1,'<=',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 <= 2 < 3")),
    ?_assertEqual([{binary_op,1,'===',{binary_op,1,'<',{integer,1,1},{integer,1,2}},{integer,1,3}}],
                                                                    parse("1 < 2 === 3")),
    % range
    ?_assertEqual([{binary_op,1,'==',{range,1,{integer,1,1},{integer,1,2}},
                                     {range,1,{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 .. 2 == 3 .. 4")),
    % bitor
    ?_assertEqual([{range,1,{binary_op,1,'|',{integer,1,1},{integer,1,2}},
                            {binary_op,1,'^',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 | 2 .. 3 ^ 4")),
    ?_assertEqual([{binary_op,1,'|',{binary_op,1,'^',{binary_op,1,'|',{integer,1,1},
                                                                      {integer,1,2}},
                                                     {integer,1,3}},
                                    {integer,1,4}}],
                                                                    parse("1 | 2 ^ 3 | 4")),
    % bitand
    ?_assertEqual([{binary_op,1,'|',{binary_op,1,'&',{integer,1,1},{integer,1,2}},
                                    {binary_op,1,'&',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 & 2 | 3 & 4")),
    % shift
    ?_assertEqual([{binary_op,1,'&',{binary_op,1,'<<',{integer,1,1},{integer,1,2}},
                                    {binary_op,1,'>>',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 << 2 & 3 >> 4")),
    ?_assertEqual([{binary_op,1,'<<',{binary_op,1,'>>',{binary_op,1,'<<',{integer,1,1},
                                                                         {integer,1,2}},
                                                       {integer,1,3}},
                                     {integer,1,4}}],
                                                                    parse("1 << 2 >> 3 << 4")),
    % addititive
    ?_assertEqual([{binary_op,1,'<<',{binary_op,1,'+',{integer,1,1},{integer,1,2}},
                                     {binary_op,1,'-',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 + 2 << 3 - 4")),
    ?_assertEqual([{binary_op,1,'-',{binary_op,1,'-',{binary_op,1,'+',{integer,1,1},
                                                                         {integer,1,2}},
                                                       {integer,1,3}},
                                     {integer,1,4}}],
                                                                    parse("1 + 2 - 3 - 4")),
    % multiplicative
    ?_assertEqual([{binary_op,1,'+',{binary_op,1,'*',{integer,1,1},{integer,1,2}},
                                    {binary_op,1,'/',{integer,1,3},{integer,1,4}}}],
                                                                    parse("1 * 2 + 3 / 4")),
    ?_assertEqual([{binary_op,1,'*',{binary_op,1,'/',{binary_op,1,'%',{binary_op,1,'*',{integer,1,1},
                                                                                       {integer,1,2}},
                                                                      {integer,1,3}},
                                                     {integer,1,4}},
                                    {integer,1,5}}],
                                                                    parse("1 * 2 % 3 / 4 * 5")),
    % +- unary
    ?_assertEqual([{binary_op,1,'*',{unary_op,1,'-',{integer,1,2}},
                                    {unary_op,1,'+',{integer,1,4}}}],
                                                                    parse(" - 2 * + 4")),
    ?_assertEqual([{unary_op,1,'+',{unary_op,1,'-',{integer,1,3}}}],
                                                                    parse(" + - 3 ")),
    % power
    ?_assertEqual([{unary_op,1,'-',{binary_op,1,'**',{integer,1,2},
                                                     {unary_op,1,'+',{integer,1,4}}}}],
                                                                    parse(" - 2 ** + 4")),
    ?_assertEqual([{binary_op,1,'**',{integer,1,1},{binary_op,1,'**',{integer,1,2},{integer,1,3}}}],
                                                                    parse(" 1 ** 2 ** 3 ")),
    % ! ~ unary
    ?_assertEqual([{binary_op,1,'**',{unary_op,1,'!',{integer,1,2}},
                                     {unary_op,1,'+',{integer,1,4}}}],
                                                                    parse(" ! 2 ** + 4")),
    ?_assertEqual([{unary_op,1,'!',{unary_op,1,'~',{unary_op,1,'!',{integer,1,4}}}}],
                                                                    parse(" ! ~ ! 4 ")),

    % receive
    ?_assertEqual([{'receive',1,[{clause,2,[{integer,2,1},{integer,2,2}],[{integer,3,3}]}],[]}],
                                            parse(" receive \n when 1 , 2 \n 3 \n end")),
    ?_assertEqual([{'receive',1,[],{'after',2,{integer,2,1},[{integer,3,2}]}}],
                                            parse(" receive \n after 1 \n 2 \n end")),
    ?_assertEqual([{'receive',1,[{clause,2,[{integer,2,1}],[{integer,3,2},{integer,4,3}]}],{'after',5,{integer,5,4},[{integer,6,5}]}}],
                                            parse(" receive \n when 1 \n 2 \n 3 \n after 4 \n 5 \n end")),

    % throw
    ?_assertEqual([{'throw',1,'RuntimeError',{integer,1,1}}],
                                            parse(" throw ( 1 ) ")),
    ?_assertEqual([{'throw',1,'Modname',{integer,1,1}}],
                                            parse(" throw ( Modname , 1 ) ")),

    % try
    ?_assertEqual([{'try',1,[{integer,2,1}],
                            [{'catch',3,{integer,3,4},[{integer,4,5}]}]}],
                                            parse(" try \n 1 \n catch 4 \n 5 \n end ")),
    ?_assertEqual([{'try',1,[{integer,1,1},{integer,2,2}],
                            [{'catch',3,{integer,3,4},[{integer,4,5}]}]}],
                                            parse(" try 1 \n 2 \n catch 4 \n 5 \n end ")),
    ?_assertEqual([{'try',1,[{integer,1,1},{integer,2,2},{integer,3,3}],
                            [{'catch',4,{integer,4,4},[{integer,5,5},{integer,6,6}]}]}],
                                            parse(" try 1 \n 2 \n 3 \n catch 4 \n 5 \n 6 end ")),
    ?_assertEqual([{'try',1,[{integer,1,1},{integer,2,2}],
                            [{'catch',3,{integer,3,4},[{integer,4,5}]},
                             {'catch',5,{integer,5,6},[{integer,6,7}]}]}],
                                            parse(" try 1 \n 2 \n catch 4 \n 5 \n catch 6 \n 7 \n end ")),

    ?_assert(true)
  ].

structured_expr_test_() -> %==========================================
  [ % case and clauses
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,7}],[{nil,2}]}]}],
                                            parse(" case 1 \n when 7 \n end")),
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,7}],[{nil,2}]},
                                            {clause,3,[{var,1,'_'}],[{integer,3,8}]}]}],
                                            parse(" case 1 \n when 7 \n else 8 \n end")),
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,2},{integer,2,3},{integer,2,4}],
                                                      [{nil,2}]}]}],
                                            parse(" case 1 \n when 2 , 3 , 4 \n end")),
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,2}],
                                                      [{integer,3,3},{integer,4,4}]},
                                            {clause,5,[{integer,5,5}],
                                                      [{integer,6,6},{integer,7,7}]}]}],
                                            parse(" case 1 \n when 2 \n 3 \n 4 \n when 5 \n 6 \n 7 \n end")),
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,2}],
                                                      [{integer,3,3}]},
                                            {clause,4,[{integer,4,5}],
                                                      [{integer,5,6}]}]}],
                                            parse(" case 1 \n when 2 \n 3 \n when 5 \n 6 \n end")),
    ?_assertEqual([{'case',1,{integer,1,1},[{clause,2,[{integer,2,2}],
                                                      [{integer,3,3},{integer,4,4}]},
                                            {clause,5,[{integer,5,5}],
                                                      [{integer,6,6},{integer,7,7}]},
                                            {clause,8,[{var,1,'_'}],[{integer,8,8}]}]}],
                                            parse(" case 1 \n when 2 \n 3 \n 4 \n when 5 \n 6 \n 7 \n else 8\n end")),
    % if
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],[{integer,2,2}]}]}],
                                            parse(" if 1 \n 2 \n end")),
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],[{integer,2,2},{integer,3,3}]}]}],
                                            parse(" if 1 \n 2 \n 3 end")),
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],[{integer,2,2}]},
                            {clause,3,[{integer,3,3}],[{integer,4,4}]}]}],
                                            parse(" if 1 \n 2 \n elseif 3 \n 4 \n end")),
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],[{integer,2,2}]},
                            {clause,3,[{integer,3,3}],[{integer,4,4}]},
                            {clause,5,[{true,5}],[{integer,5,5}]}]}],
                                            parse(" if 1 \n 2 \n elseif 3 \n 4 \n else 5 \n end")),
    ?_assertEqual([{'if',1,[{clause,1,[{integer,1,1}],[{integer,2,2},{integer,3,22}]},
                            {clause,4,[{integer,4,3}],[{integer,5,4},{integer,6,44}]},
                            {clause,7,[{true,7}],[{integer,7,5},{integer,8,55}]}]}],
                                            parse(" if 1 \n 2 \n 22 \n elseif 3 \n 4 \n 44 \n else 5 \n 55 \n end")),

    ?_assert(true)
  ].

structure_test_() -> %==========================================
  [ % funref
    % class_inst and pargs
    ?_assertEqual([{class_inst,1,'Mod_name',[],{nil,1}}],
                                            parse("Mod_name ( ) ")),
    ?_assertEqual([{class_inst,1,'Mod_name',[{integer,1,1}],{nil,1}}],
                                            parse(" Mod_name ( 1 ) ")),
    ?_assertEqual([{class_inst,1,'Mod_name',[{integer,1,1},{integer,1,2}],{nil,1}}],
                                            parse(" Mod_name ( 1 , 2 ) ")),
    ?_assertEqual([{class_inst,1,'Mod_name',[{integer,1,1}],{integer,1,7}}],
                                            parse(" Mod_name ( 1 , &7 ) ")),
    ?_assertEqual([{class_inst,1,'Mod_name',[{integer,1,1},{integer,1,2}],{integer,1,7}}],
                                            parse(" Mod_name ( 1 , 2 , &7 ) ")),

    ?_assert(true)
  ].

basic_term_test_() -> %==========================================
  [ % integer
    ?_assertEqual([{integer,1,23}],         parse("23 ")),
    % float
    ?_assertEqual([{float,1,23.4}],         parse("23.4 ")),
    % atom
    ?_assertEqual([{atom,1,abc}],           parse(":abc ")),
    ?_assertEqual([{atom,1,abc}],           parse(":'abc' ")),
    ?_assertEqual([{atom,1,'a\'bc'}],       parse(":'a\\'bc' ")),
    ?_assertEqual([{atom,1,'a"bc'}],        parse(":\"a\\\"bc\" ")),
    % boolean values
    ?_assertEqual([{true,1}],               parse("true ")),
    ?_assertEqual([{false,1}],              parse("false ")),
    ?_assertEqual([{nil,1}],                parse("nil ")),
    % regexp
    ?_assertEqual([{regexp,1,""}],          parse("%r// ")),
    ?_assertEqual([{regexp,1,"a/bc"}],      parse("%r/a\\/bc/ ")),
    ?_assertEqual([{regexp,1,"abc"}],       parse("%r/abc/ ")),
    % self
    ?_assertEqual([{self,1}],               parse("self ")),

    % module name
    ?_assertEqual([{module_name,1,'Mod'}],  parse("Mod ")),
    ?_assertEqual([{module_name,1,'M0d_Ul3_'}],
                                            parse("M0d_Ul3_ ")),

    % instance variable
    ?_assertEqual([{ivar,1,'ident'}],       parse("@ident ")),
    % bound variable
    ?_assertEqual([{bound_var,1,'ident'}],  parse("^ident ")),
    % identifier
    ?_assertEqual([{var,1,ident}],          parse("ident ")),
    ?_assertEqual([{var,1,ident_name}],     parse("ident_name ")),
    ?_assertEqual([{var,1,ident_3}],        parse("ident_3 ")),
    ?_assertEqual([{var,1,iDeNt_4_}],       parse("iDeNt_4_")),
    ?_assertEqual([{var,1,'_ident_name'}],  parse("_ident_name")),
    % apostrophe string - simple
    ?_assertEqual([{string,1,""}],          parse("'' ")),
    ?_assertEqual([{string,1,"abc"}],       parse("'abc' ")),
    ?_assertEqual([{string,1,"a#bc"}],      parse("'a#bc'")),
    % quote string - simple
    ?_assertEqual([{string,1,""}],          parse("\"\" ")),
    ?_assertEqual([{string,1,"abc"}],       parse("\"abc\"")),
    ?_assertEqual([{string,1,"a#bc"}],      parse("\"a#bc\"")),
    ?_assert(true)
  ].


compound_term_test_() -> %==========================================
  [ % basic
    % apostrophe string - interpolated
    ?_assertEqual([{dstring,1,
                    [   {string,1,"ab"},
                        {binary_op,1,'+',{integer,1,1},{integer,1,2}},
                        {string,1,"cd"}]
                    }],       
                                            parse("'ab#{1+2}cd' ")),
    ?_assertEqual([{dstring,1,
                    [ {string,1,"ab"},
                      {dstring,1,
                        [ {string,1,"wx"},
                          {binary_op,1,'+',{integer,1,1},{integer,1,2}},
                          {string,1,"yz"}
                        ]
                      },
                      {string,1,"cd"}]
                    }],       
                                            parse("'ab#{'wx#{1+2}yz'}cd'")),
    % quote string - interpolated
    ?_assertEqual([{dstring,1,
                    [   {string,1,"ab"},
                        {binary_op,1,'+',{integer,1,1},{integer,1,2}},
                        {string,1,"cd"}]
                    }],       
                                            parse("\"ab#{1+2}cd\" \n\n")),
    ?_assertEqual([{dstring,1,
                    [ {string,1,"ab"},
                      {dstring,1,
                        [ {string,1,"wx"},
                          {binary_op,1,'+',{integer,1,1},{integer,1,2}},
                          {string,1,"yz"}
                        ]
                      },
                      {string,1,"cd"}]
                    }],       
                                            parse("\"ab#{\"wx#{1+2}yz\"}cd\"")),
    ?_assertEqual([{dstring,1,
                    [ {string,1,"This is an "},
                      {dstring,1,
                        [ {string,1,"embedded "},
                          {binary_op,1,'+',{integer,1,1},{integer,1,1}},
                          {string,1,"-level expression"}
                        ]
                      },
                      {string,1," string"}]
                    }],       
                                            parse("'This is an #{\"embedded #{1+1}-level expression\"} string' \n")),
    % tuple
    ?_assertEqual([{tuple,1,[]}],           parse(" ( ) ")),
    ?_assertEqual([{tuple,1,[{integer,1,1}]}],
                                            parse(" ( 1 , ) ")),
    ?_assertEqual([{tuple,1,[{integer,1,1},{integer,1,2},{integer,1,3}]}],
                                            parse(" ( 1 , 2 , 3 ) ")),
    % dictionary
    ?_assertEqual([{dict,1,[]}],           parse(" { } ")),
    ?_assertEqual([{dict,1,[{{integer,1,1},{integer,1,2}}]}],
                                            parse(" { 1 => 2} ")),
    ?_assertEqual([{dict,1,[{{integer,1,1},{integer,1,2}},{{integer,1,3},{integer,1,4}}]}],
                                            parse(" { 1 => 2 , 3 => 4 } ")),
    % list_comprehension
    ?_assertEqual([{lc,1,{integer,1,1},[{integer,1,2}]}],
                                            parse(" [ 1 for 2 ] ")),
    ?_assertEqual([{lc,1,{integer,1,1},[{integer,1,2},{integer,1,3},{integer,1,4}]}],
                                            parse(" [ 1 for 2 , 3 , 4 ] ")),
    ?_assertEqual([{lc,1,{integer,1,1},[{integer,1,2},{generate,1,{integer,1,3},{integer,1,4}}]}],
                                            parse(" [ 1 for 2 , 3 in 4 ] ")),
    % list
    ?_assertEqual([{empty,1}],
                                            parse(" [ ] ")),
    ?_assertEqual([{cons,1,{integer,1,1},{empty,1}}],
                                            parse(" [ 1 ] ")),
    ?_assertEqual([{cons,1,{integer,1,1},{cons,1,{integer,1,2},{empty,1}}}],
                                            parse(" [ 1 , 2 ] ")),
    ?_assertEqual([{cons,1,{integer,1,1},{integer,1,2}}],
                                            parse(" [ 1, * 2 ] ")),
    ?_assertEqual([{cons,1,{integer,1,1},{cons,1,{integer,1,2},{integer,1,3}}}],
                                            parse(" [ 1 , 2, * 3 ] ")),
    % binary
    ?_assertEqual([{binary,1,[]}],
                                            parse(" <[ ]> ")),
    ?_assertEqual([{binary,1,[{bin_element,1,{integer,1,3},default,default}]}],
                                            parse(" <[ 3 ]> ")),
    ?_assertEqual([{binary,1,[{bin_element,1,{integer,1,3},{integer,1,2},default}]}],
                                            parse(" <[ 3:2 ]> ")),
    ?_assertEqual([{binary,1,[{bin_element,1,{integer,1,3},{integer,1,2},[x]}]}],
                                            parse(" <[ 3:2/:x ]> ")),
    ?_assertEqual([{binary,1,[{bin_element,1,{integer,1,3},{integer,1,2},[{x,5}]}]}],
                                            parse(" <[ 3:2/:x:5 ]> ")),
    ?_assertEqual([{binary,1,[{bin_element,1,{integer,1,3},{integer,1,2},[{x,5}]},
                              {bin_element,1,{integer,1,7},{integer,1,4},[{y,6}]}
                             ]}],
                                            parse(" <[ 3:2/:x:5 , 7:4/:y:6 ]> ")),
    % bracketed
    ?_assertEqual([{binary_op,1,'*',{integer,1,1},{binary_op,1,'+',{integer,1,2},{integer,1,3}}}],
                                            parse(" 1 * ( 2 + 3 ) ")),

    ?_assert(true)
  ].


class_and_module_decl_test_() ->
  [ % Class Declarations
    ?_assertEqual([{class,1,'Class','Object',[]}],
                                            parse(" class Class \n end ")),
    ?_assertEqual([{class,1,'Class','Banjo',[]}],
                                            parse(" class Class < Banjo \n end ")),
    ?_assertEqual([{class,1,'Class','Winkle',[{function,2,scratch,[],{var,1,'_'},[{nil,2}]}]}],
                                            parse("class Class < Winkle
                                                            def scratch ()
                                                            end
                                                        end ")),
    ?_assertEqual([{class,1,'Class','Banjo',[{function,2,'[]',[{var,2,parm}],
                                                              {var,2,'_'},
                                                              [{nil,2}] }] }],
                                            parse("class Class < Banjo
                                                            def [] ( parm )
                                                            end
                                                        end ")),
    ?_assertEqual([{class,1,'Class','Object',[{function,2,'[]=',[{var,2,parm1},{var,2,parm2}],
                                                                {var,2,'_'},
                                                                [{integer,3,17}] },
                                             {function,5,'func',[],
                                                               {var,1,'_'}, % should be line 5?
                                                               [{integer,6,23},{integer,7,47}] }] }],
                                            parse("class Class
                                                            def []=  ( parm1 , parm2 ) 
                                                                17 
                                                            end
                                                            def func () 
                                                                23 
                                                                47 
                                                            end 
                                                        end ")),
    % Module Declarations
    ?_assertEqual([{module,1,'Mod',[]}],    parse(" module Mod \n end ")),
    ?_assertEqual([{module,1,'NewMod',[{function,2,method,[],{var,1,'_'},[{nil,2}]}]}],
                                            parse(" module NewMod
                                                             def method ()
                                                             end
                                                         end ")),
    ?_assert(true)
  ].



function_calls_test_() ->
  [ % Local Calls
    ?_assertEqual([{local_call,1,func1,[],{nil,1}}],
                                            parse(" func1 () ")),
    ?_assertEqual([{local_call,1,func1,[{integer,1,27}],{nil,1}}],
                                            parse(" func1( 27 ) ")),
    ?_assertEqual([{local_call,1,func1,[],{lambda,1,[],[]}}],
                                            parse(" func1 { } ")),
    ?_assertEqual([{local_call,1,func1,[],{lambda,1,[],[{integer,1,69}]}}],
                                            parse(" func1 { 69 } ")),
    ?_assertEqual([{local_call,1,func1,[{integer,1,27}],{lambda,1,[],[]}}],
                                            parse(" func1 ( 27 ) { } ")),
    ?_assertEqual([{local_call,1,func1,[{integer,1,27}],{lambda,1,[],[{integer,1,69}]}}],
                                            parse(" func1 ( 27 ) { 69 } ")),
    ?_assertEqual([{local_call,1,func1,[],{lambda,1,[],[{integer,1,77}]}}],
                                            parse(" func1 do 77 end ")),
    % Native Calls
    ?_assertEqual([{native_call,1,erlang,func1,[]}],
                                            parse(" erl.func1() ")),
    ?_assertEqual([{native_call,1,erlang,func1,[{integer,1,27}]}],
                                            parse(" erl.func1 ( 27 ) ")),
    ?_assertEqual([{native_call,1,mod1,func1,[]}],
                                            parse(" erl.mod1.func1 () ")),
    ?_assertEqual([{native_call,1,mod1,func1,[{integer,1,27}]}],
                                            parse(" erl.mod1.func1( 27 ) ")),
    % Remote Calls
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[],{nil,1}}],
                                            parse(" func1().func2() ")),
        % Based on a class
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[],{nil,1}}],
                                            parse(" Clas.func() ")),
%    ?_assertEqual(,
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[{integer,1,23}],{nil,1}}],
                                            parse(" Clas.func( 23 ) ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func {} ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func do end ")),
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" Clas.func {23} ")),
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" Clas.func do 23 end ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func () {} ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func () do end ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func (23) {} ")),
%    ?_assertEqual(,
%                                            parse(" Clas.func (23) do end ")),
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" Clas.func (23) {45} ")),
    ?_assertEqual([{remote_call,1,{module_name,1,'Clas'},func,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" Clas.func (23) do 45 end ")),

        % Based on a call
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[],{nil,1}}],
                                            parse(" func1().func2 () ")),
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[{integer,1,23}],{nil,1}}],
                                            parse(" func1().func2 (23) ")),
%                                            parse(" func1().func2 {} ")),
%                                            parse(" func1().func2 do end ")),
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" func1().func2 {23} ")),
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" func1().func2 do 23 end ")),
%                                            parse(" func1() {}.func2 () {} ")),
%                                            parse(" func1() do end.func2 () do end ")),
%                                            parse(" func1().func2 (23) {} ")),
%                                            parse(" func1().func2 (23) do end ")),
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" func1().func2 (23) {45} ")),
    ?_assertEqual([{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" func1().func2 (23) do 45 end ")),

        % Based on a max_expr
    ?_assertEqual([{remote_call,1,{var,1,ident},func,[],{nil,1}}],
                                            parse(" ident.func () ")),
    ?_assertEqual([{remote_call,1,{integer,1,23},func,[{integer,1,23}],{nil,1}}],
                                            parse(" 23.func (23) ")),
%                                            parse(" "abc".func {} ")),
%                                            parse(" (1,2,3).func do end ")),
    ?_assertEqual([{remote_call,1,{atom,1,abc},func,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" :abc.func {23} ")),
    ?_assertEqual([{remote_call,1,{ivar,1,ident},func,[],{lambda,1,[],[{integer,1,23}]}}],
                                            parse(" @ident.func do 23 end ")),
%                                            parse(" ^ident.func () {} ")),
%                                            parse(" %r/xyz/.func () do end ")),
%                                            parse(" [1,2,3].func (23) {} ")),
%                                            parse(" { }.func (23) do end ")),
    ?_assertEqual([{remote_call,1,{false,1},func,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" false.func (23) {45} ")),
    ?_assertEqual([{remote_call,1,{float,1,23.7},func,[{integer,1,23}],{lambda,1,[],[{integer,1,45}]}}],
                                            parse(" 23.7.func (23) do 45 end ")),

        % Multi-level calls
    ?_assertEqual([{remote_call,1,{remote_call,1,{local_call,1,func1,[],{nil,1}},func2,[],{nil,1}},func3,[],{nil,1}}],
                                            parse(" func1().func2().func3() ")),

    ?_assert(true)
  ].



