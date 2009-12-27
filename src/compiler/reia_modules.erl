%
% reia_modules: Extract submodules, invoking a fun to replace them
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_modules).
-export([replace/2]).
-include("reia_nodes.hrl").

% Internal state of the module transform
-record(state, {'fun', modules=[]}).
  
replace(Exprs, Fun) ->
  {Exprs2, State} = lists:mapfoldl(
    fun transform/2,
    #state{'fun'=Fun},
    Exprs
  ),
  {ok, Exprs2, State#state.modules}.

transform(#module{} = Node, #state{'fun'=Fun} = State) ->
  {Node2, State2} = reia_syntax:mapfold_subtrees(fun transform/2, State, Node),
  Modules = State2#state.modules,
  {Fun(Node2), State2#state{modules=[Node2|Modules]}};
  
transform(Node, State) when is_integer(Node) or is_float(Node) or is_atom(Node) ->
  {Node, State};

transform(Node, State) ->
  reia_syntax:mapfold_subtrees(fun transform/2, State, Node).