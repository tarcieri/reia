%
% CodeServer: Reia code loading and management service
% Copyright (C)2010 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module('CodeServer'). 
-behaviour(gen_server).
-export([
  % Public API
  start/0, 
  
  % gen_server callbacks
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
  {ok, []}.
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.
  
handle_cast(_Msg, State) ->
  {noreply, State}.
  
handle_info(_Info, State) ->
  {noreply, State}.
  
terminate(_Reason, _State) ->
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.