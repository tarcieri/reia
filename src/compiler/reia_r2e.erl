%
% reia_r2e: Reia to Erlang translation layer of the compiler
% Copyright (C)2009 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_r2e).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("../core/reia_types.hrl").

-define(call(Line, Module, Function, Args),
  {call, Line,
		{remote, Line, {atom, Line, Module}, {atom, Line, Function}},
		[transform(Arg) || Arg <- Args]
	}).
-define(dispatch(Receiver, Line, Method, Args, Block),
  ?call(Line, reia_dispatch, call, [
		Receiver,
		#atom{line=Line, name=Method},
		#tuple{line=Line, elements=Args},
		Block
	])).
-define(inst(Line, Class, Args, Block),
	?call(Line, reia, inst, [
		#atom{line=Line, name=Class},
		#tuple{line=Line, elements=Args},
		Block
	])).

% Lists of expressions
transform(Exprs, _Options) ->
  [transform(Expr) || Expr <- Exprs].

% Modules
transform(#module{line=Line, name=Name, exprs=Exprs}) ->
  % Modules need some postprocessing, so we leave them in a similar form but
  % with their subtrees converted
  #module{
    line  = Line,
    name  = Name,
    attrs = [{module_type, module}], 
    exprs = group_clauses([transform(Expr) || Expr <- Exprs])
  };
  
% Classes
transform(#class{line=Line, name=Name, exprs=Exprs}) ->
  % Modules need some postprocessing, so we leave them in a similar form but
  % with their subtrees converted
  #module{
    line  = Line,
    name  = Name,
    attrs = [{module_type, class}],
    exprs = group_clauses([transform(Expr) || Expr <- Exprs])
  };

% Functions
transform(#function{body=[]} = Function) ->
	% Functions with empty bodies implicitly return nil
	transform(Function#function{body=[#nil{}]});
transform(#function{line=Line, name=Name, args=Args, block=Block, body=Exprs}) ->
  {function, Line, Name, 2, [{clause, Line,
    [{tuple, Line, [transform(Arg) || Arg <- Args]}, transform(Block)],
    [],
    [transform(Expr) || Expr <- Exprs]
  }]};

% Simple literals
transform(#integer{} = Expr) -> Expr;
transform(#float{} = Expr)   -> Expr;
transform(#atom{} = Expr)    -> Expr;
transform(#true{line=Line})  -> {atom, Line, true};
transform(#false{line=Line}) -> {atom, Line, false};
transform(#nil{line=Line})   -> {atom, Line, nil};

% Variables
transform(#var{line=Line, name=Name}) -> {var, Line, Name};

% Strings
transform(#string{line=Line, characters=Chars}) ->
  {tuple, Line, [
    {atom, Line, reia_string},
    {cons, Line,
      {bin, Line, [{bin_element, Line, {string, Line, Chars}, default, default}]},
      {nil, Line}
    }
  ]};
  
% Interpolated strings
transform(#dstring{line=Line, elements=Elements}) ->
  transform(#remote_call{
    line=Line,
    receiver=explode_list(Elements, Line),
    name=join,
    args=[],
    block=#nil{line=Line}
  });
  
% Regexps
transform(#regexp{line=Line, pattern=Pattern}) ->
  {tuple, Line, [
    {atom, Line, reia_regexp}, 
    {bin, Line, [{bin_element, Line, {string, Line, Pattern}, default, default}]}
  ]};
  
% Binaries
transform(#binary{line=Line, elements=Elements}) ->
  {bin, Line, [binary_element(Element) || Element <- Elements]};

% Lists
transform(#cons{line=Line, expr=Expr, tail=Tail}) ->
  {cons, Line, transform(Expr), transform(Tail)};

transform(#empty{line=Line}) ->
  {nil, Line};

% Tuples
transform(#tuple{line=Line, elements=Exprs}) ->
  {tuple, Line, [transform(Expr) || Expr <- Exprs]};

% Dicts
transform(#dict{line=Line, elements=Elements}) ->
  {call, Line,
    {remote, Line, {atom, Line, dict}, {atom, Line, from_list}},
    [dict_elements(Elements, Line)]
  };
  
% Ranges
transform(#range{line=Line, from=From, to=To}) ->
  {tuple, Line, [
    {atom, Line, reia_range}, 
    transform(From),
    transform(To)
  ]};
  
% Lambdas
transform(#lambda{line=Line, args=Args, body=Exprs}) ->
  {'fun', Line, {clauses, [
    {clause, Line,
      [transform(Arg) || Arg <- Args], [], 
      [transform(Expr) || Expr <- Exprs]
    }
  ]}};

% Module references
transform(#module_name{line=Line, name=Name}) ->
  {tuple, Line, [{atom, Line, reia_module}, {atom, Line, Name}]};
  
%% List comprehensions
transform(#lc{line=Line, expr=Expr, generators=Generators}) ->
  {lc, Line, transform(Expr), [transform(Generator) || Generator <- Generators]};

%% List comprehension generators
transform(#generate{line=Line, pattern=Pattern, source=Source}) ->
  List = ?dispatch(Source, Line, to_list, [], transform(#nil{line=Line})),
  {generate, Line, transform(Pattern), List};

% Clauses
transform(#clause{line=Line, patterns=Patterns, exprs=Exprs}) ->
  Exprs2 = [transform(Expr) || Expr <- Exprs],
  [{clause, Line, [transform(Pattern)], [], Exprs2} || Pattern <- Patterns];

% Case expressions
transform(#'case'{line=Line, expr=Expr, clauses=Clauses}) ->
  Clauses2 = lists:flatten([transform(Clause) || Clause <- Clauses]),
  {'case', Line, transform(Expr), Clauses2};

% Throw expressions
transform(#throw{line=Line, type=Class, message=Message}) ->
	Exception = ?inst(Line, Class, [
		#integer{line=Line, value=Line},
		Message
	], #nil{line=Line}),
	{call, Line, {atom, Line, throw}, [Exception]};

% Try expressions
transform(#'try'{line=Line, body=Exprs, clauses=Clauses}) ->
  {'try', Line, 
    [transform(Expr) || Expr <- Exprs],
    [],
    [transform(Clause) || Clause <- Clauses],
    []
  };

% Catch clauses
transform(#'catch'{line=Line, pattern=Pattern, body=Exprs}) ->
  Underscore = {var, Line, '_'},
  {clause, Line, 
    [{tuple, Line, [Underscore, Pattern, Underscore]}], 
    [],
    [transform(Expr) || Expr <- Exprs]
  };

% Matches
transform(#match{line=Line, left=Left, right=Right}) ->
  {match, Line, transform(Left), transform(Right)};
    
% Unary operators
transform(#unary_op{line=Line, type='!', expr=Expr}) ->
  {op, Line, 'not', transform(Expr)};

transform(#unary_op{line=Line, type='~', expr=Expr}) ->
  {op, Line, 'bnot', transform(Expr)};
    
transform(#unary_op{line=Line, type=Type, expr=Expr}) ->
  {op, Line, Type, transform(Expr)};

% Binary operators
transform(#binary_op{line=Line, type='==', left=Left, right=Right}) ->
  {call, Line,
    {remote, Line, {atom, Line, reia_comparisons}, {atom, Line, compare}},
    [transform(Left), transform(Right)]
  };

transform(#binary_op{line=Line, type='&', left=Left, right=Right}) ->
  {op, Line, 'band', transform(Left), transform(Right)};
  
transform(#binary_op{line=Line, type='|', left=Left, right=Right}) ->
  {op, Line, 'bor', transform(Left), transform(Right)};

transform(#binary_op{line=Line, type='^', left=Left, right=Right}) ->
  {op, Line, 'bxor', transform(Left), transform(Right)};

transform(#binary_op{line=Line, type='<<', left=Left, right=Right}) ->
  {op, Line, 'bsl', transform(Left), transform(Right)};
    
transform(#binary_op{line=Line, type='>>', left=Left, right=Right}) ->
  {op, Line, 'bsr', transform(Left), transform(Right)};

transform(#binary_op{line=Line, type='%', left=Left, right=Right}) ->
  {op, Line, 'rem', transform(Left), transform(Right)};
  
transform(#binary_op{line=Line, type='**', left=Left, right=Right}) ->
  {call, Line,
    {remote, Line, {atom, Line, math}, {atom, Line, pow}},
    [transform(Left), transform(Right)]
  };
  
transform(#binary_op{line=Line, type='===', left=Left, right=Right}) ->
  {'try', Line,
    [{match, Line, transform(Left), transform(Right)}, {atom, Line, true}],
    [],
    [{clause, Line,
      [{tuple, Line,
      [{atom, Line, error},
        {tuple, Line, [{atom, Line, badmatch}, {var, Line, '_'}]},
        {var, Line, '_'}]}
      ],
      [],
      [{atom, Line, false}]}],
  []};
  
transform(#binary_op{line=Line, type='<=', left=Left, right=Right}) ->
  {op, Line, '=<', transform(Left), transform(Right)};
  
transform(#binary_op{line=Line, type='[]', left=Left, right=Right}) ->
  ?dispatch(Left, Line, '[]', [Right], transform(#nil{line=Line}));
  
transform(#binary_op{line=Line, type=Type, left=Left, right=Right}) ->
  {op, Line, Type, transform(Left), transform(Right)};
  
% Send expressions (i.e. async messaging)
transform(#send{line=Line, receiver=Receiver, message=Message}) ->
  {op, Line, '!', transform(Receiver), transform(Message)};
  
% Receive expressions
transform(#'receive'{line=Line, clauses=Clauses, after_clause=After}) ->
  Clauses2 = lists:flatten([transform(Clause) || Clause <- Clauses]),
  
  case After of
    void -> 
      {'receive', Line, Clauses2};
    #'after'{} -> 
      Timeout = transform(After#'after'.timeout),
      Exprs   = transform(After#'after'.exprs),
      {'receive', Line, Clauses2, Timeout, Exprs}
  end;
  
% Function references
transform(#funref{line=Line, receiver=Receiver, name=Name}) ->
  {tuple, Line, [
    {atom, Line, reia_funref}, 
    transform(Receiver),
    {atom, Line, Name}
  ]};

% Class instantiations
transform(#class_inst{
	line  = Line, 
	class = Class, 
	args  = Args,
	block = Block
}) -> ?inst(Line, Class, Args, Block);

% Function calls
transform(#local_call{
  line  = Line,
  name  = Name,
  args  = Args,
  block = Block
}) ->
  {call, Line, {atom, Line, Name}, [
    {tuple, Line, [transform(Arg) || Arg <- Args]}, 
    transform(Block)
  ]};

transform(#remote_call{
  line      = Line,
  receiver  = Receiver,
  name      = Name,
  args      = Args,
  block     = Block
}) -> ?dispatch(Receiver, Line, Name, Args, Block);

transform(#native_call{
  line      = Line,
  module    = Module,
  function  = Function,
  args      = Args
}) ->
  {call, Line,
    {remote, Line, {atom, Line, Module}, {atom, Line, Function}},
    [transform(Arg) || Arg <- Args]
  };
  
transform(#var_call{
  line     = Line,
  receiver = Receiver,
  args     = Args,
  block    = Block
}) ->
  Receiver2 = transform(Receiver),
  Args2 = [transform(Arg) || Arg <- Args],
  {'if', Line,
    [{clause, Line, [],
      [[{call, Line, {atom, Line, is_function}, [Receiver2]}]],
      [{call, Line, Receiver2, Args2}]},
    {clause, Line, [],
      [[{atom, Line, true}]],
      [{call, Line, {remote, Line,
         {atom, Line, reia_internal},
         {atom, Line, invoke_callable}},
           [Receiver2, {tuple, Line, Args2}, transform(Block)]}]}]};

% Code blocks (Erlang-style, not to be confused with Ruby-style block args)
transform(#block{line=Line, exprs=Exprs}) -> 
  {block, Line, [transform(Expr) || Expr <- Exprs]}.

% Group clauses of functions with the same name and arity
group_clauses(Functions) ->
  group_clauses(Functions, dict:new()).
group_clauses([], Functions) ->
  [
    {function, Line, Name, Arity, lists:reverse(Clauses)} ||
    {{Name, Arity},{Line, Clauses}} <- dict:to_list(Functions)
  ];
group_clauses([{function, Line, Name, Arity, [Clause]}|Rest], Functions) ->
  case dict:find({Name, Arity}, Functions) of
    {ok, {Line2, Clauses}} ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line2, [Clause|Clauses]}, Functions));
    error ->
      group_clauses(Rest, dict:store({Name, Arity}, {Line, [Clause]}, Functions))
  end.

% Transform the elements of Dicts
dict_elements([], Line) ->
  {nil, Line};
dict_elements([{Key,Value}|Rest], Line) ->
  {cons, Line, {tuple, Line, [transform(Key), transform(Value)]}, dict_elements(Rest, Line)}.
 
%% Transform an element of a binary
binary_element(Element) -> Element.

% Explode a list of expressions into cons nodes
explode_list([], Line) ->
  #empty{line=Line};
explode_list([Expr|Tail], Line) ->
  #cons{line=Line, expr=Expr, tail=explode_list(Tail, Line)}.