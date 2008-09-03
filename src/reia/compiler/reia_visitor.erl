-module(reia_visitor).
-export([transform/3, transformer/2]).

transform(Expressions, State, Fun) when is_list(Expressions) ->
  {Expressions2, {State2, _Fun}} = lists:mapfoldl(fun reia_visitor:transformer/2, {State, Fun}, Expressions),
  {ok, State2, Expressions2};
  
transform(Node, State, Fun) ->
  case Fun(State, Node) of
    {walk, State2, Node2} ->
      walk(Node2, State2, Fun);
    {stop, State2, Node2} ->
      {ok, State2, Node2};
    Value ->
      throw({error, {"invalid_transform result", Value}})
  end.
  
transformer(Node, {State, Fun}) ->
  {ok, State2, Node2} = transform(Node, State, Fun),
  {Node2, {State2, Fun}}.

walk({module, Line, Name, Expressions}, State, Fun) ->
  {ok, State2, Name2} = transform(Name, State, Fun),
  {ok, State3, Expressions2} = transform(Expressions, State2, Fun),
  {ok, State3, {module, Line, Name2, Expressions2}};
walk({function, Line, Name, Arguments, Expressions}, State, Fun) ->
  {ok, State2, Name2} = transform(Name, State, Fun),
  {ok, State3, Arguments2} = transform(Arguments, State2, Fun),
  {ok, State4, Expressions2} = transform(Expressions, State3, Fun),  
  {ok, State4, {function, Line, Name2, Arguments2, Expressions2}};
walk({funcall, Line, Name, Expressions}, State, Fun) ->
  {ok, State2, Name2} = transform(Name, State, Fun),
  {ok, State3, Expressions2} = transform(Expressions, State2, Fun),
  {ok, State3, {funcall, Line, Name2, Expressions2}};
walk({funcall, Line, Module, Name, Expressions}, State, Fun) ->
  {ok, State2, Module2} = transform(Module, State, Fun),
  {ok, State3, Name2} = transform(Name, State2, Fun),
  {ok, State4, Expressions2} = transform(Expressions, State3, Fun),
  {ok, State4, {funcall, Line, Module2, Name2, Expressions2}};
walk({constant, _Line, _Name} = Node, State, _Fun) ->
  {ok, State, Node};
walk({identifier, _Line, _Name} = Node, State, _Fun) ->
  {ok, State, Node};
walk({string, _Line, _Value} = Node, State, _Fun) ->
  {ok, State, Node};
walk(Ast, State, _Fun) ->
  % io:format("warning: unrecognized AST node: ~p~n", [Ast]),
  {ok, State, Ast}.