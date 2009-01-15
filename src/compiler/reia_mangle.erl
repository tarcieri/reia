%
% reia_mangle: Reversable name mangling routines
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%
-module(reia_mangle).
-export([method/1, method/2, unmangle/2]).
-define(prefix, "__re_").

method(Name) when is_atom(Name) ->
  method(atom_to_list(Name));
method(Name) ->
  list_to_atom(lists:concat([?prefix, "method_", Name])).
  
method(Class, Name) when is_atom(Name) ->
  method(Class, atom_to_list(Name));
method(Class, Name) ->
  list_to_atom(lists:concat([?prefix, "extmethod_", Class, "_", Name])).
  
unmangle(Module, Name) when is_atom(Name) ->
  unmangle(Module, atom_to_list(Name));
unmangle(Module, ?prefix ++ "method_" ++ Name) ->
  {method, Module, list_to_atom(Name)};
unmangle(_Module, ?prefix ++ "extmethod_" ++ Name) ->
  {Class, Method} = extract_class_and_method(Name),
  {method, list_to_atom(Class), list_to_atom(Method)};
unmangle(_, _) ->
  nil.
  
extract_class_and_method(Name) ->
  extract_class_and_method(Name, []).
  
extract_class_and_method([$_|Name], Klass) ->
  {lists:reverse(Klass), Name};
extract_class_and_method([X|Name], Klass) ->
  extract_class_and_method(Name, [X|Klass]).