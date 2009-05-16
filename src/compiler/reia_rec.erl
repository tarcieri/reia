%
% reia_rec: Internal format of compiled Reia scripts
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_rec).
-export([string/1, compile/1, compile/2]).
-record(recfile, {filename, version=0, base_module, submodules}).

string(Str) ->
  case reia_parse:string(Str) of
    {ok, Expressions} ->
      compile(Expressions);
    Error ->
      Error
  end.

compile(Expressions) ->
  compile(nonce_filename(), Expressions).
  
compile(Filename, Expressions) ->
  #recfile{filename=Filename, base_module=Expressions, submodules=[]}.
  
nonce_filename() ->
  ["#Ref" ++ Ref] = io_lib:format("~p", [make_ref()]),
  "reia_eval#" ++ Ref.
  