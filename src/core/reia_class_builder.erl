%
% reia_class_builder: Builds classes at runtime with support for inheritance.
% Intended to eventually replace reia_class.erl
%
% Copyright (C)2008 Tony Arcieri
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_class_builder).
-export([new/2]).

-record(class, {name, ancestor, methods}).

%% Create a new class with the given ancestor
new(Name, Ancestor) ->
  case code:ensure_loaded(Ancestor) of
    {module, _} -> void;
    Error -> throw(Error)
  end,
  
  {class, _Line, _Name, Methods} = case [Code || {code, Code} <- Ancestor:module_info(attributes)] of
    [[Class]] -> Class;
    _ -> throw({error, {Ancestor, "lacks a code attribute (not a Reia module?)"}})
  end,
  
  #class{name=Name, ancestor=Ancestor, methods=merge_methods(Ancestor, Methods, dict:new())}.
  
%% Merge a new set of methods into
merge_methods(Source, Methods, MethodDict) -> 
  lists:foldr(
    fun(Function, Dict) -> 
      {function, Line, {identifier, _, Name}, Arity, Clauses} = Function,
      Method = {method, Line, {Source, Name}, Arity, Clauses},
      dict:store(Name, Method, Dict)
    end,
    MethodDict,
    Methods
  ).