%
% reia_ivars: Convert Reia implicit state instance variables to explicit state
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ivars).
-export([mutable_method/1, immutable_method/1]).
-include("reia_nodes.hrl").
-define(self(Line),  (#var{line=Line, name='__reia_self'})).
-define(ivars(Line), (#var{line=Line, name='__reia_ivars'})).

% Transform for methods that are allowed to alter instance variables
mutable_method(Method) ->
	Line = Method#function.line,
	
	% Generate instance variable dictionary
	% FIXME: this really shouldn't be in a generic "mutable_method" transform.
	% This should get factored elsewhere when the object model is actually working.
	Ivars = #native_call{
		line     = Line, 
		module   = dict, 
		function = new
	},
	
	BindIvars = #match{line=Line, left=?ivars(Line), right=Ivars},
	Body = reia_syntax:map_subtrees(fun mutable_method_ivars/1, Method#function.body),
	Method2 = Method#function{body = [BindIvars|Body]},
	{Method2, ?ivars(Line)}.

mutable_method_ivars(#ivar{} = Ivar) -> remap_ivar(Ivar);
		
mutable_method_ivars(Expr) ->
	reia_syntax:map_subtrees(fun mutable_method_ivars/1, Expr).
	
% Methods which are not allowed to make changes to instance variables
immutable_method(Method) ->
	Line = Method#function.line,
	
	Ivars = #native_call{
		line     = Line,
		module   = erlang,
		function = element,
		args     = [#integer{line=Line, value=3}, ?self(Line)]
	},
	
	BindIvars = #match{line=Line, left=?ivars(Line), right=Ivars},
	Body = reia_syntax:map_subtrees(fun immutable_method_ivars/1, Method#function.body),
	Method#function{body=[BindIvars|Body]}.
	
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