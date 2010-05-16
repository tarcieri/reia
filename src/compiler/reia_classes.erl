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

transform(#class{} = Node) ->
  reia_syntax:map_subtrees(fun transform/1, transform_class(Node));
      
transform(Node) ->
  reia_syntax:map_subtrees(fun transform/1, Node).
    
transform_class(#class{line=Line, name=Name, superclass=Ancestor, methods=Methods}) ->
	Superclass = case Name of
		'Object' -> undefined;
		_        -> Ancestor
	end,
	
	MethodTable = build_method_table(Methods, Superclass),
	Initialize = transform_initialize_method(Name, dict:find(initialize, MethodTable)),
	MethodTable2 = dict:store(initialize, Initialize, MethodTable),
	MethodTable3 = dict:store(inspect, inspect_method(Line, Name), MethodTable2),
	Methods2 = [callify_method(Method) || {_, Method} <- dict:to_list(MethodTable3)],
	
  #class{line=Line, name=Name, methods=Methods2}.

% Create a dict of methods by name
build_method_table(Methods, Superclass) ->
	build_method_table(dict:new(), Methods, Superclass).
	
build_method_table(Dict, [], _Superclass) ->
	Dict;
build_method_table(Dict, [Func|Rest], Superclass) ->
	Dict2 = dict:store(Func#function.name, transform_method(Func, Superclass), Dict),
	build_method_table(Dict2, Rest).
	
% Transform the initialize method to return a new object instance
transform_initialize_method(Name, error) ->
	% Use default initialize method if one isn't defined
	transform_initialize_method(Name, #function{
		line=1, 
		name=initialize,
		body=[]
	});
transform_initialize_method(Name, #function{body = Body} = Method) ->
	Line = 1,
	
  Ivars = #native_call{
		line     = Line, 
		module   = dict, 
		function = new, 
		args=[]
	},
	
	Result = #tuple{
		line     = Line, 
		elements = [
		  #atom{line=Line, name=reia_object},
			#atom{line=Line, name=Name},
			Ivars
		]
	},
	
	Method#function{body = Body ++ [Result]}.
	
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
	
% Transform OO features into functional ones
transform_method(Method, _Superclass) ->
	Method.
	
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