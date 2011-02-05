%
% reia_dispatch: Dispatch logic for all Reia method invocations
% Copyright (C)2008-10 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_dispatch).
-export([call/4]).
-include("reia_types.hrl").

% Dispatch incoming calls
call(Receiver, Method, Arguments, Block) when is_integer(Receiver) ->
  'Integer':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_float(Receiver) ->
  'Float':call({Receiver, Method, Arguments}, Block);  
call(Receiver, Method, Arguments, Block) when is_list(Receiver) ->
  'List':call({Receiver, Method, Arguments}, Block);
call(#reia_object{class = Class} = Receiver, Method, Arguments, Block) ->
	Class:call({Receiver, Method, Arguments}, Block);
call({dict,_,_,_,_,_,_,_,_} = Receiver, Method, Arguments, Block) ->
  'Dict':call({Receiver, Method, Arguments}, Block);
call(#reia_string{} = Receiver, Method, Arguments, Block) ->
  'String':call({Receiver, Method, Arguments}, Block);
call(#reia_regexp{} = Receiver, Method, Arguments, Block) ->
  'Regexp':call({Receiver, Method, Arguments}, Block);
call(#reia_range{} = Receiver, Method, Arguments, Block) ->
  'Range':call({Receiver, Method, Arguments}, Block);
call(#reia_module{} = Receiver, Method, Arguments, Block) ->
  dispatch_module_call(Receiver, Method, Arguments, Block);
call(#reia_funref{} = Receiver, Method, Arguments, Block) ->
  'Funref':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_tuple(Receiver) ->
  'Tuple':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_binary(Receiver) ->
  'Binary':call({Receiver, Method, Arguments}, Block);
call(true, Method, Arguments, Block) ->
  'Boolean':call({true, Method, Arguments}, Block);
call(false, Method, Arguments, Block) ->
  'Boolean':call({false, Method, Arguments}, Block);
call(nil, Method, Arguments, Block) ->
  'Boolean':call({nil, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_atom(Receiver) ->
  'Atom':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_function(Receiver) ->
  'Fun':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_pid(Receiver) ->
  'Pid':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_port(Receiver) ->
  'Channel':call({Receiver, Method, Arguments}, Block);
call(Receiver, Method, Arguments, Block) when is_reference(Receiver) ->
  'UUID':call({Receiver, Method, Arguments}, Block);
call(Receiver, _, _, _) ->
  throw({error, unknown_receiver, Receiver}).
  
dispatch_module_call(#reia_module{name=Name} = Receiver, Method, Arguments, Block) ->
  case code:ensure_loaded(Name) of
    {module, Name} ->
      Attributes = Name:module_info(attributes),
      case proplists:get_value(module_type, Attributes) of
        [module] ->
          'Module':call({Receiver, Method, Arguments, Block}, nil);
        [class] ->
          % FIXME: to_s and inspect shouldn't be implemented here!
          case Method of
            to_s    -> reia:list_to_string(atom_to_list(Name));
            inspect -> reia:list_to_string(atom_to_list(Name));
            _ ->
              reia:throw('RuntimeError', "class methods not implemented yet, sorry!")
          end;
        undefined ->
          Message = lists:flatten(io_lib:format("missing module_type attribute in ~s", [Name])),
          reia:throw('RuntimeError', Message)
      end;
    _ ->
      Message = lists:flatten(io_lib:format("undefined module ~s", [Name])),
      reia:throw('NameError', Message)
  end.