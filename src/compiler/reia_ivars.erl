%
% reia_ivars: Convert Reia implicit state instance variables to explicit state
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ivars).
-export([mutable_method/1, immutable_method/1]).
-include("reia_nodes.hrl").
-include("reia_object.hrl").

% Transform for methods that are allowed to alter instance variables
mutable_method(Method) ->
	Line = Method#function.line,
	
	% Extract instance variable dictionary
	Ivars = #native_call{
		line     = Line, 
		module   = erlang, 
		function = element,
		args     = [#integer{line=Line, value=3}, ?self(Line)]
	},
	
	BindIvars = #match{line=Line, left=?ivars(Line), right=Ivars},
	Body = reia_syntax:map_subtrees(fun mutable_method_ivars/1, Method#function.body),
	Method#function{body = [BindIvars|Body]}.

mutable_method_ivars(#ivar{} = Ivar) -> remap_ivar(Ivar);
		
mutable_method_ivars(Expr) ->
	reia_syntax:map_subtrees(fun mutable_method_ivars/1, Expr).
	
% Methods which are not allowed to make changes to instance variables
immutable_method(Method) ->
	Line = Method#function.line,
	
	IvarPattern = #tuple{line=Line, elements=[
		#atom{line=Line, name=reia_object},
		#var{line=Line, name='_'},
		?ivars(Line)
	]},
	
	Ivars = #'case'{line=Line, expr=?self(Line), clauses=[
		#clause{line=Line, patterns=[IvarPattern], exprs=[#nil{}]},
		#clause{line=Line, patterns=[#var{line=Line, name='_'}], exprs=[
		  #match{line=Line, left=?ivars(Line), right=#nil{}}
		]}
	]},
	
	Body = reia_syntax:map_subtrees(fun immutable_method_ivars/1, Method#function.body),
	Method#function{body=[Ivars|Body]}.
	
immutable_method_ivars(#match{} = Expr) ->
	[Left]  = reia_syntax:map_subtrees(fun immutable_match_context/1, [Expr#match.left]),
	[Right] = reia_syntax:map_subtrees(fun immutable_method_ivars/1,  [Expr#match.right]),
	Expr#match{left=Left, right=Right}; 

immutable_method_ivars(#ivar{} = Ivar) -> remap_ivar(Ivar);
			
immutable_method_ivars(Expr) ->
	reia_syntax:map_subtrees(fun immutable_method_ivars/1, Expr).
		
immutable_match_context(#ivar{line=Line}) ->
	throw({error, {Line, "illegal usage of an instance variable in match context"}});
	
immutable_match_context(Expr) ->
	reia_syntax:map_subtrees(fun immutable_match_context/1, Expr).
		
remap_ivar(#ivar{line=Line, name=Name}) ->
	#binary_op{
		line  = Line,
		type  = '[]',
		left  = ?ivars(Line),
		right = #atom{line=Line, name=Name}
	}.