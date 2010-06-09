%
% reia_classes: Convert OOP features to Reia code
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_classes).
-export([transform/2]).
-include("reia_nodes.hrl").
-define(self(Line), (#var{line=Line, name='__reia_self'})).

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform/1, Exprs).

transform(#class{} = Expr) ->
	reia_syntax:map_subtrees(fun transform/1, transform_class(Expr));
      
transform(Expr) ->
  reia_syntax:map_subtrees(fun transform/1, Expr).
    
transform_class(#class{line=Line, name=Name, superclass=Ancestor, methods=Methods}) ->
	Superclass = case Name of
		'Object' -> undefined;
		_        -> Ancestor
	end,
	
	MethodTable = build_method_table(Methods, Superclass),
	
	Initialize = transform_initialize_method(Name, MethodTable),
	MethodMissing = transform_method_missing(Ancestor, MethodTable),
	
	MethodTable2 = dict:erase(initialize, MethodTable),
	MethodTable3 = dict:store(method_missing, MethodMissing, MethodTable2),
	MethodTable4 = dict:store(inspect, inspect_method(Line, Name), MethodTable3),
	
	Methods2 = [prepare_method(Method) || {_, Method} <- dict:to_list(MethodTable4)],
	Methods3 = [callify_method(Initialize)|Methods2],
		
  #class{line=Line, name=Name, methods=Methods3}.

% Create a dict of methods by name
build_method_table(Methods, Superclass) ->
	build_method_table(dict:new(), Methods, Superclass).
	
build_method_table(Dict, [], _Superclass) ->
	Dict;
build_method_table(Dict, [Method|Rest], Superclass) ->
	Dict2 = dict:store(Method#function.name, Method, Dict),
	build_method_table(Dict2, Rest, Superclass).
	
% Transform the initialize method to return a new object instance
transform_initialize_method(Name, MethodTable) ->
	Method = case dict:find(initialize, MethodTable) of
		{ok, Function} -> Function;
		error -> % Use default initialize method if one isn't defined
			#function{
				line=1, 
				name=initialize,
				body=[]
			}
	end,

	{Initialize, Ivars} = reia_ivars:mutable_method(Method),

	Line = Initialize#function.line,	
	Result = #tuple{
		line     = Line, 
		elements = [
		  #atom{line=Line, name=reia_object},
			#atom{line=Line, name=Name},
			Ivars
		]
	},
	
	Initialize#function{body = Initialize#function.body ++ [Result]}.
	
% Transform the method_missing method or create it if it wasn't defined
transform_method_missing(_Ancestor, MethodTable) ->
	MethodMissing = case dict:find(method_missing, MethodTable) of
		{ok, Function} -> Function;
		error -> % Use default (call super) if the method doesn't exist
			#function{
				line=1, 
				name=method_missing,
				body=[#nil{}] % FIXME: yeah this should try to call the superclass
			}
	end,
	
	% FIXME: This should really do something
	MethodMissing.

% Prepare the finalized form of a method
prepare_method(Method) ->
	Method2 = reia_ivars:immutable_method(Method),
	Method3 = transform_method(Method2),
	callify_method(Method3).
	
% Change the method into a clause of the call function
callify_method(Method) ->
	Line = Method#function.line,
	
	Args = [
	  ?self(Line),
		#atom{line = Line, name = Method#function.name},
		#tuple{line = Line, elements = Method#function.args}
	],
	
	Method#function{
	  line = Line,
	  name = call,
	  args = Args
	}.
	
% Generate the "inspect" method
% FIXME: this should be inherited from 'Object'
inspect_method(Line, Name) ->
	#function{
		line = Line, 
		name = inspect, 
		body = [#dstring{
		  line = Line,
		  elements = [
		    #string{line = Line, characters = "#<"},
		    #atom{line = Line, name = Name},
		    #string{line = Line, characters = ">"}
		  ]
	}]}.
	
% Transform local calls to the OO "call" signature
transform_method(#local_call{line=Line, name=Name, args=Args, block=Block}) ->
  Expr = #native_call{
		line     = Line,
		module   = reia_dispatch,
		function = call,
		args     = [
		  ?self(Line),
		  #atom{line=Line, name=Name},
		  #tuple{line=Line, elements=Args},
		  Block
		]
	},

	reia_syntax:map_subtrees(fun transform_method/1, Expr);
		
% Transform references to self
transform_method(#self{line=Line}) ->
	?self(Line);
	
transform_method(Expr) ->
  reia_syntax:map_subtrees(fun transform_method/1, Expr).