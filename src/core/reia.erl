%
% reia: Interface between Erlang and Reia environments
% Copyright (C)2008-10 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module(reia).
-export([
	init/0, 
	load/1,
	parse/1
]).
-include("reia_types.hrl").

%
% Public functions
%

% Initialize the Reia environment
init() -> reia_internal:load_core(), reia_internal:load_stdlib().

load(Filename) ->
  SourcePath = filename:absname(Filename),
  BinPath = filename:rootname(SourcePath) ++ ".reb",
  
  case file:read_file_info(SourcePath) of
    {ok, SourceInfo} ->      
      case file:read_file_info(BinPath) of
        % If the binary already exists, load it
        {ok, BinInfo} ->
          SourceMtime = element(6, SourceInfo),
          BinMtime = element(6, BinInfo),
          
          % Ensure changes haven't been made to the sources
          if
            BinMtime > SourceMtime ->
              void;
            true ->
              reia_internal:compile(SourcePath, BinPath)
          end;
          
        % Otherwise compile the source code
        {error, _} ->
          reia_internal:compile(SourcePath, BinPath)
      end,
      reia_bytecode:load_file(BinPath);
    {error, _} = Error ->
      Error
  end.

% Parse the given string of Reia source code
parse(String) ->
	reia_compiler:parse(String).