%
% CodeServer: Reia code loading and management service
% Copyright (C)2010 Tony Arcieri
%
% Redistribution is permitted under the MIT license. See LICENSE for details.
%

-module('CodeServer'). 
-behaviour(gen_server).
-record(state, {debug=true, paths=[]}).
-define(DEBUG(Msg, Args), (_={void, Msg, Args})).
%-define(DEBUG(Msg, Args), if State#state.debug -> io:format(Msg, Args); true -> void end).
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

handle_call({paths}, {From, _Ref}, State) ->
  ?DEBUG("*** CodeServer: got paths() request from ~p~n", [From]),
  {reply, State#state.paths, State};
handle_call({unshift_path, Path}, {From, _Ref}, State) ->
  ?DEBUG("*** CodeServer: got unshift_path(~p) request from ~p~n", [Path, From]),
  AbsPath = filename:absname(Path),
  State2 = State#state{paths = [AbsPath|State#state.paths]},
  {reply, State2#state.paths, State2};
handle_call({push_path, Path}, {From, _Ref}, State) ->
  ?DEBUG("*** CodeServer: got push_path(~p) request from ~p~n", [Path, From]),
  AbsPath = filename:absname(Path),
  State2 = State#state{paths = State#state.paths ++ [AbsPath]},
  {reply, State2#state.paths, State2};
handle_call({set_paths, Paths}, {From, _Ref}, State) ->
  ?DEBUG("*** CodeServer: got set_paths(~p) request from ~p~n", [Paths, From]),
  State2 = State#state{paths = [filename:absname(Path) || Path <- Paths]},
  {reply, State2#state.paths, State2};
handle_call(Request, _From, State) ->
  ?DEBUG("*** CodeServer: Unknown call: ~p~n", [Request]),
  {reply, {error, undef}, State}.
  
handle_cast(Message, State) ->
  ?DEBUG("*** CodeServer: Handling cast: ~p~n", [Message]),
  {noreply, State}.
  
handle_info(Info, State) ->
  ?DEBUG("*** CodeServer: Handling info: ~p~n", [Info]),
  {noreply, State}.
  
terminate(Reason, State) ->
  ?DEBUG("!!! CodeServer CRASH: ~p~n", [Reason]),
  ?DEBUG("    State: ~p~n", [State]),
  
  % If the Reia code server crashes, we're horked. Fuck the system. 
  init:stop(1).
  
code_change(_OldVsn, State, _Extra) ->
  ?DEBUG("*** CodeServer: Code change~n", []),
  {ok, State}.