%
% CodeServer: Reia code loading and management service
% Copyright (C)2010 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module('CodeServer'). 
-behaviour(gen_server).
-record(state, {debug=true}).
-define(DEBUG(Msg, Args), if State#state.debug -> io:format(Msg, Args); true -> void end).
-export([
  % Public API
  start/0, call/2,
  
  % gen_server callbacks
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
  
call(Request, _Block) ->
  gen_server:call('CodeServer', Request).
  
init([]) ->
  {ok, #state{}}.
  
handle_call(Request, _From, State) ->
  ?DEBUG("*** CodeServer: Handling call: ~p~n", [Request]),
  {reply, ok, State}.
  
handle_cast(Message, State) ->
  ?DEBUG("*** CodeServer: Handling cast: ~p~n", [Message]),
  {noreply, State}.
  
handle_info(Info, State) ->
  ?DEBUG("*** CodeServer: Handling info: ~p~n", [Info]),
  {noreply, State}.
  
terminate(Reason, State) ->
  ?DEBUG("*** CodeServer: Terminating: ~p~n", [Reason]),
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  ?DEBUG("*** CodeServer: Code change~n", []),
  {ok, State}.