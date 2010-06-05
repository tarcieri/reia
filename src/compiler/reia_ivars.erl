%
% reia_ivars: Convert Reia implicit state instance variables to explicit state
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ivars).
-export([initialize/1]).
-include("reia_nodes.hrl").
-define(ivars(Line), (#var{line=Line, name='__reia_ivars'})).

initialize(Method) ->
	Line = Method#function.line,
	
	% Generate instance variable dictionary
	Ivars = #native_call{
		line     = Line, 
		module   = dict, 
		function = new, 
		args=[]
	},
	
	BindIvars = #match{line=Line, left=?ivars(Line), right=Ivars},
	Body = reia_syntax:map_subtrees(fun initialize_ivars/1, Method#function.body),
	Initialize = Method#function{body = [BindIvars|Body]},
	{Initialize, ?ivars(Line)}.
	
initialize_ivars(Expr) ->
	Expr.
	