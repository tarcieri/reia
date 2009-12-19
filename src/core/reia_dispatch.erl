%
% reia_dispatch: Dispatch logic for all Reia method invocations
% Copyright (C)2008 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_dispatch).
-export([call/4]).
-include("../compiler/reia_types.hrl").

% Dispatch incoming calls
call(Receiver, Method, Arguments, Block) when is_integer(Receiver) or is_float(Receiver) ->
  'Numeric':call(Receiver, Method, Arguments, Block);
call(#reia_string{} = Receiver, Method, Arguments, Block) ->
  'String':call(Receiver, Method, Arguments, Block);
call(Receiver, Method, Arguments, Block) when is_atom(Receiver) ->
  'Atom':call(Receiver, Method, Arguments, Block);
call(Receiver, Method, Arguments, Block) when is_list(Receiver) ->
  'List':call(Receiver, Method, Arguments, Block);
call({dict,_,_,_,_,_,_,_,_} = Receiver, Method, Arguments, Block) ->
  'Map':call(Receiver, Method, Arguments, Block);
call(Receiver, Method, Arguments, Block) when is_tuple(Receiver) ->
  'Tuple':call(Receiver, Method, Arguments, Block);
call(Receiver, Method, Arguments, Block) when is_binary(Receiver) ->
  'Binary':call(Receiver, Method, Arguments, Block);
call(Receiver, Method, Arguments, Block) when is_function(Receiver) ->
  'Fun':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_pid(Receiver) ->
  'Process':call(Receiver, Method, Arguments, Block);
call(Receiver, _, _, _) ->
  throw({error, unknown_receiver, Receiver}).