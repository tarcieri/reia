%
% reia_classes: Convert OOP features to Reia code
% Copyright (C)2010 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_classes).
-export([transform/2]).
-include("reia_nodes.hrl").
-include("reia_object.hrl").
-include("reia_bindings.hrl").

transform(Exprs, _Options) ->
  reia_syntax:map_subtrees(fun transform/1, Exprs).
  
transform(#class{} = Expr) ->
  reia_syntax:map_subtrees(fun transform/1, transform_class(Expr));
      
transform(Expr) ->
  reia_syntax:map_subtrees(fun transform/1, Expr).
    
transform_class(#class{line=Line, name=Name, parent=Parent, exprs=Methods}) ->
  MethodTable = build_method_table(Methods),
  
  Initializers = transform_initialize_methods(Name, MethodTable),
  MethodMissings = transform_method_missings(Parent, MethodTable),
  
  MethodTable2 = orddict:erase(initialize, MethodTable),
  MethodTable3 = orddict:store(method_missing, MethodMissings, MethodTable2),
  
  Methods2 = [Meths || {_, Meths} <- orddict:to_list(MethodTable3)],
  Methods3 = [prepare_method(Method) || Method <- lists:flatten(Methods2)],
  Methods4 = Initializers ++ Methods3 ++ [method_missing_thunk()],
  
  #class{line=Line, name=Name, exprs=Methods4}.

% Create a orddict of methods by name
build_method_table(Methods) ->
  build_method_table(orddict:new(), Methods).
  
build_method_table(Dict, []) ->
  Dict;
build_method_table(Dict, [Method|Rest]) ->
  Name = Method#function.name,
  Dict2 = case orddict:find(Name, Dict) of
    {ok, List} ->
      orddict:store(Name, lists:reverse([Method|List]), Dict);
    error ->
      orddict:store(Name, [Method], Dict)
  end,
  build_method_table(Dict2, Rest).
    
% Transform the initialize method to return a new object instance
transform_initialize_methods(_Name, MethodTable) ->
  Methods = case orddict:find(initialize, MethodTable) of
    {ok, Res} -> Res;
    error -> % Use default initialize method if one isn't defined
      [#function{
        line=1, 
        name=initialize,
        body=[]
      }]
  end,

  lists:map(fun(Method) ->
    Initialize = prepare_initialize_method(Method),

    Line = Initialize#function.line,  
    Result = #native_call{
      line     = Line,
      module   = erlang,
      function = setelement,
      args = [
        #integer{line=Line, value=3},
        ?self(Line),
        ?ivars(Line)
      ] 
    },
      
    Initialize#function{body = Initialize#function.body ++ [Result]}
  end, Methods).
  
% Transform the method_missing method or create it if it wasn't defined
transform_method_missings(Parent, MethodTable) ->
  case orddict:find(method_missing, MethodTable) of
    {ok, Methods} -> Methods;
    error -> % Use default (call super) if the method doesn't exist
      [#function{
        line  = 1, 
        name  = method_missing,
        args  = [#var{line=1, name=method}, #var{line=1, name=args}],
        block = #var{line=1, name=block},
        body  = [
          #native_call{
            line     = 1,
            module   = Parent,
            function = call,
            args     = [
              #tuple{
                line = 1,
                elements = [
                  ?self(1),
                  #var{line=1, name=method},
                  #var{line=1, name=args}
                ]
              },
              #var{line=1, name=block}
            ]
          }
        ]
      }]
  end.
  
% Create a catchall thunk which calls method_missing
method_missing_thunk() ->
  #function{
    line = 1,
    name = call,
    args = [
      #var{line=1, name=self},
      #var{line=1, name=method},
      #var{line=1, name=args}
    ],
    block = #var{line=1, name=block},
    body = [
      #local_call{
        line=1,
        name=call,
        args=[
          #var{line=1, name=self},
          #atom{line=1, name=method_missing},
          #tuple{line=1, elements=[
            #var{line=1, name=method}, 
            #var{line=1, name=args}
          ]}
        ],
        block=#var{line=1, name=block}
      }
    ]
  }.
  
% Prepare the finalized form of an initialize method
prepare_initialize_method(Method) ->
  Method2 = reia_ivars:mutable_method(Method),
  Method3 = transform_method(Method2),
  callify_method(Method3).
    
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

transform_method(Method) ->
  {ok, AnnotatedMethod} = reia_bindings:transform([Method]),
  [Method2] = reia_syntax:map_subtrees(fun transform_method_expr/1, AnnotatedMethod),
  Method2.
  
% Transform local calls to the OO "call" signature
transform_method_expr(#bindings{
  node=#local_call{name=Name}=Call, 
  entries=Bindings
}) ->
  Call2 = case dict:find(Name, Bindings) of
    {ok, _Version} ->
      Call; % Pass through calls to bound variables verbatim
    error ->
      transform_local_call(Call)
  end,

  reia_syntax:map_subtrees(fun transform_method_expr/1, Call2);
    
% Transform references to self
transform_method_expr(#bindings{node=#self{line=Line}}) ->
  ?self(Line);
  
transform_method_expr(#bindings{node=Expr}) ->
  reia_syntax:map_subtrees(fun transform_method_expr/1, Expr);
  
transform_method_expr(Expr) ->
  reia_syntax:map_subtrees(fun transform_method_expr/1, Expr).

% Transform local calls to dispatch to self
transform_local_call(#local_call{line=Line, name=Name, args=Args, block=Block}) ->
  #native_call{
    line     = Line,
    module   = reia_dispatch,
    function = call,
    args     = [
      ?self(Line),
      #atom{line=Line, name=Name},
      #tuple{line=Line, elements=Args},
      Block
    ]
  }.