%
% reia_ssa: Convert destructive assignments into Static Single Assignment form
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_ssa).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_compile_options.hrl").
-include("reia_bindings.hrl").

% Lists of expressions
transform(Exprs, Options) ->
  {ok, BAExprs} = reia_bindings:transform(Exprs, Options#compile_options.scope),
  reia_syntax:map_subtrees(fun transform_node/1, BAExprs).

% Bind unsafe variables of case clauses
transform_node(#bindings{
  node=#clause{line=Line, patterns=Patterns, exprs=BindingExprs}, 
  final=FinalBinding, 
  output=OutputBinding
}) ->  
  Patterns2 = reia_syntax:map_subtrees(fun transform_node/1, Patterns),
  Exprs = reia_syntax:map_subtrees(fun transform_node/1, BindingExprs),
  
  UnsafeVariables = enumerate_unsafe_variables(FinalBinding, OutputBinding),
  Exprs2 = case UnsafeVariables of
    [] -> Exprs;
    _  -> annotate_return_value(Exprs, UnsafeVariables)
  end,
  
  #clause{line=Line, patterns=Patterns2, exprs=Exprs2};

transform_node(#bindings{
  node=#var{line=Line, name=Name}=Node, 
  entries=Bindings
}) ->
  case dict:find(Name, Bindings) of
    {ok, Version} ->
      #var{line=Line, name=ssa_name(Name, Version)};
    error ->
      case Name of
        '_' -> 
          Node;
        _   ->
          throw({error, {Line, lists:flatten(io_lib:format("unbound variable: '~s'", [Name]))}})
      end
  end;
transform_node(#bindings{node=#bound_var{line=Line, name=Name}}=Bindings) ->
  transform_node(Bindings#bindings{node=#var{line=Line, name=Name}});
transform_node(#bindings{node=Node}) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node);
transform_node(Node) ->
  reia_syntax:map_subtrees(fun transform_node/1, Node).

% Generate the SSA name for a given variable, which takes the form name_version
ssa_name(Name, Version) ->
  Name2 = lists:flatten(io_lib:format("~s_~w", [Name, Version])),
  list_to_atom(Name2).
  
% Enumerate all variables which are unsafe at the end of a given clause
enumerate_unsafe_variables(FinalBinding, OutputBinding) ->
  enumerate_unsafe_variables([], dict:to_list(FinalBinding), OutputBinding). 
  
enumerate_unsafe_variables(Unsafe, [], _) ->
  Unsafe;
enumerate_unsafe_variables(Unsafe, [{Name, Version}|Rest], Output) ->
  {ok, NewVersion} = dict:find(Name, Output),
  if
    NewVersion > Version ->
      enumerate_unsafe_variables([{Name,Version,NewVersion}|Unsafe], Rest, Output);
    true ->
      enumerate_unsafe_variables(Unsafe, Rest, Output)
  end.
  
% Annotate the return value, binding all unsafe variables to their newest versions
annotate_return_value(Exprs, UnsafeVariables) ->
  [LastExpr|Rest] = lists:reverse(Exprs),
  OutputVariables = dict:from_list([{Name, {InputVer, OutputVer}} || {Name, InputVer, OutputVer} <- UnsafeVariables]),
  {LastExpr2, BoundVariables} = update_final_expression(LastExpr, OutputVariables),
  FinalVariables = bind_unsafe_variables(filter_safe_variables(UnsafeVariables, BoundVariables)),
  
  lists:reverse([LastExpr2|FinalVariables] ++ Rest).

% Update the version numbers of any variables bound in the final expression
update_final_expression(Expr, OutputVariables) ->
  {[Expr2], {_, BoundVariables}} = reia_syntax:mapfold_subtrees(
    fun transform_final/2,
    {OutputVariables, []},
    [Expr]
  ),
  {Expr2, sets:from_list(BoundVariables)}.
  
% Look for outdated version references in the final expression and update them
transform_final(#var{line=Line, name=SSAName}=Node, {OutputVariables, Bound}=State) ->
  case decode_ssa(SSAName) of
    {NameString, Version} ->
      Name = list_to_atom(NameString),
      case dict:find(Name, OutputVariables) of
        {ok, {Input, Output}} ->
          if
            Version == Input -> 
              Var = #var{line=Line, name=ssa_name(Name, Output)},
              State2 = {OutputVariables, [Name|Bound]},
              {Var, State2};
            true -> 
              {Node, State}
          end;
        _ ->
          {Node, State}
      end;
    _ ->
      {Node, State}
  end;
transform_final(Node, State) when is_integer(Node) or is_float(Node) or is_atom(Node) ->
  {Node, State};

transform_final(Node, State) ->
  reia_syntax:mapfold_subtrees(fun transform_final/2, State, Node).
    
% Decode variable from its SSA form
decode_ssa(Name) ->
  decode_ssa([], lists:reverse(atom_to_list(Name))).
  
decode_ssa(_, []) ->
  undefined;
decode_ssa(Version, [$_|Name]) ->
  try
    {lists:reverse(Name), list_to_integer(lists:reverse(Version))}
  catch 
    error:badarg -> undefined
  end;
decode_ssa(Version, [Char|Rest]) ->
  decode_ssa([Char|Version], Rest).
  
% Filter out variables that are safely bound in the last expression
filter_safe_variables(UnsafeVariables, BoundVariables) ->
  lists:filter(
    fun({Name, _, _}) -> not sets:is_element(Name, BoundVariables) end, 
    UnsafeVariables
  ).

% Bind all unsafe variables which aren't bound by the last clause
bind_unsafe_variables(UnsafeVariables) ->
  [bind_expression(Name, Input, Output) || {Name, Input, Output} <- UnsafeVariables].
  
bind_expression(Name, Input, Output) ->
  #match{
    line  = 1,
    left  = #var{line=1, name=ssa_name(Name, Output)},
    right = #var{line=1, name=ssa_name(Name, Input)}
  }.