%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 2003 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%%
%%			   SMART EXCEPTIONS
%%
%% Author: Thomas Lindgren (030414-)
%%
%% These are the runtime handler functions for smart_exceptions.

-module(smart_exc_rt).
-author('thomasl_erlang@yahoo.com').
-behaviour(smart_handler_b).
-export([
	 function_clause/3,
	 if_clause/3,
	 case_clause/3,
	 switch/4,
	 match/5, 
	 binop/6, 
	 unop/5, 
	 bif/5, 
	 exit/3
	]).

function_clause(MFA, Line, Args) ->
    switch(MFA, Line, function_clause, Args).

if_clause(MFA, Line, Args) ->
    switch(MFA, Line, if_clause, Args).

case_clause(MFA, Line, Args) ->
    switch(MFA, Line, case_clause, Args).

%% perhaps not to be exported?
switch(MFA, Line, Rsn, Args) ->
    exit({MFA, 
	  {line, Line}, 
	  {reason, Rsn}, 
	  Args}).

match(MFA, Line, Reason, Pattern, Value) ->
    %% very simplistic use of Pattern: it's a syntax tree ...
    exit({MFA, 
	  {line, Line}, 
	  {reason, Reason}, 
	  {match, Pattern, Value}}).

binop(MFA, Line, Rsn, Op, X1, X2) ->
    exit({MFA, 
	  {line, Line}, 
	  {reason, Rsn}, 
	  {Op, op_type_args(Op), [X1,X2]}}).

unop(MFA, Line, Rsn, Op, X1) ->
    exit({MFA, 
	  {line, Line}, 
	  {reason, Rsn}, 
	  {Op, op_type_arg(Op), [X1]}}).

bif(MFA, Line, Rsn, F, Xs) ->
    exit({MFA, 
	  {line, Line}, 
	  {reason, Rsn}, 
	  {bif, F, bif_type_args(F, length(Xs)), Xs}}).

exit(MFA, Line, Rsn) ->
    exit({MFA, 
	  {line, Line}, 
	  {reason, Rsn}, 
	  []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% unary operators

op_type_arg('+') ->
    [number];
op_type_arg('-') ->
    [number];
op_type_arg('bnot') ->
    [integer];
op_type_arg(Op) ->
    warning("unary operator ~p not recognized~n", [Op]),
    [unknown].

%% binary operators

op_type_args('+') ->
    [number,number];
op_type_args('-') ->
    [number,number];
op_type_args('*') ->
    [number,number];
op_type_args('/') ->
    [number,non_zero];
op_type_args('band') ->
    [integer,integer];
op_type_args('bor') ->
    [integer,integer];
op_type_args('bxor') ->
    [integer,integer];
op_type_args('++') ->
    %% actually, 2nd arg can be anything
    [list,list];
op_type_args('--') ->
    [list,list];
op_type_args(Op) ->
    warning("binary operator ~p not recognized~n", [Op]),
    [unknown, unknown].

%% BIFs from OTP R8B0
%%
%% - the types given here are very ad hoc
%% - the set of BIFs varies with implementation too
%%
%% Here is an interesting thought:
%% - currently, we return [T],[A] and let the user puzzle out what went
%%   wrong
%% - instead, for each argument A with type T, run T(A) to see whether the
%%   type is satisfied or not.
%%   [unable to check records, however]

bif_type_args(abs,1) ->
    [number];
bif_type_args(append_element,2) ->
    [tuple,any];
bif_type_args(apply,2) ->
    [{module,function},args];
bif_type_args(apply,3) ->
    [module,function,args];
bif_type_args(atom_to_list,1) ->
    [atom];
bif_type_args(binary_to_float,1) ->
    [binary];
bif_type_args(binary_to_list,1) ->
    [binary];
bif_type_args(binary_to_list,3) ->
    %% actually, nat =< nat ...
    [binary, integer, integer];
bif_type_args(binary_to_term,1) ->
    [binary];
bif_type_args(bump_reductions,1) ->
    [integer];
bif_type_args(cancel_timer,1) ->
    [ref];
bif_type_args(check_process_code,2) ->
    [pid,module];
bif_type_args(concat_binary,1) ->
    [[binary]];
bif_type_args(date,0) ->
    [];
bif_type_args(delete_module,1) ->
    [module];
bif_type_args(demonitor,1) ->
    [ref];
bif_type_args(disconnect_node,1) ->
    [node];
bif_type_args(display,1) ->
    [any];
bif_type_args(element,2) ->
    [integer,tuple];
bif_type_args(erase,0) ->
    [];
bif_type_args(erase,1) ->
    [any];
bif_type_args(exit,1) ->
    [any];
bif_type_args(exit,2) ->
    [pid,any];
bif_type_args(failt,1) ->
    [any];
bif_type_args(fault,2) ->
    [any,[any]];
bif_type_args(float,1) ->
    [number];
bif_type_args(float_to_binary,1) ->
    [float,size];
bif_type_args(float_to_list,1) ->
    [float];
bif_type_args(fun_info,1) ->
    ['fun'];
bif_type_args(function_exported,3) ->
    [module,function,arity];
bif_type_args(fun_info,2) ->
    ['fun',any];
bif_type_args(fun_to_list,1) ->
    ['fun'];
bif_type_args(garbage_collect,0) ->
    [];
bif_type_args(garbage_collect,1) ->
    [pid];
bif_type_args(get,0) ->
    [];
bif_type_args(get,1) ->
    [any];
bif_type_args(get_cookie,0) ->
    [];
bif_type_args(get_keys,1) ->
    [any];
bif_type_args(group_leader,0) ->
    [];
bif_type_args(group_leader,2) ->
    [pid,pid];
bif_type_args(halt,0) ->
    [];
bif_type_args(halt,1) ->
    [integer_or_string];
bif_type_args(hash,2) ->
    [any,integer];
bif_type_args(hd,1) ->
    [list];
bif_type_args(info,1) ->
    [any];
bif_type_args(integer_to_list,1) ->
    [integer];
bif_type_args(is_alive,0) ->
    [];
bif_type_args(is_builtin,3) ->
    [module,function,arity];
bif_type_args(is_process_alive,1) ->
    [pid];
bif_type_args(length,1) ->
    [list];
bif_type_args(link,1) ->
    [pid];
bif_type_args(list_to_atom,1) ->
    [string];
bif_type_args(list_to_binary,1) ->
    %% non-flat byte list
    [io_list];
bif_type_args(list_to_float,1) ->
    [string];
bif_type_args(list_to_integer,1) ->
    [byte_list];
bif_type_args(list_to_pid,1) ->
    [string];
bif_type_args(list_to_tuple,1) ->
    [list];
bif_type_args(load_module,2) ->
    [module,binary];
bif_type_args(loaded,0) ->
    [];
bif_type_args(localtime,0) ->
    [];
bif_type_args(localtime_to_universaltime,1) ->
    [datetime];
bif_type_args(make_ref,0) ->
    [];
bif_type_args(make_tuple,2) ->
    [integer,any];
bif_type_args(md5, 1) ->
    [any];
bif_type_args(md5_init,0) ->
    [];
bif_type_args(md5_update,2) ->
    [binary, io_list];
bif_type_args(md5_final,1) ->
    [binary];
bif_type_args(module_loaded,1) ->
    [module];
bif_type_args(monitor,2) ->
    [monitor_type, monitor_item];
bif_type_args(monitor_node,2) ->
    [node, bool];
bif_type_args(node,0) ->
    [];
bif_type_args(node,1) ->
    [pid_ref_port];
bif_type_args(nodes,0) ->
    [];
bif_type_args(now,0) ->
    [];
bif_type_args(open_port,2) ->
    [port_name, port_settings];
bif_type_args(phash,2) ->
    [any, integer];
bif_type_args(pid_to_list,1) ->
    [pid];
bif_type_args(port_close,2) ->
    %% this is probably an error in the 'man erlang' docs
    [port,any];
bif_type_args(port_close,1) ->
    [port];
bif_type_args(port_command,2) ->
    [port, io_list];
bif_type_args(port_connect,2) ->
    [port, pid];
bif_type_args(port_control,3) ->
    [port, int_32, io_list];
bif_type_args(port_info,2) ->
    [port, any];
bif_type_args(ports,0) ->
    [];
bif_type_args(port_to_list,1) ->
    [port];
bif_type_args(pre_loaded,0) ->
    [];
bif_type_args(process_display,2) ->
    [pid, any];
bif_type_args(process_flag, 2) ->
    [atom, any];
bif_type_args(process_flag, 3) ->
    [pid, atom, any];
bif_type_args(process_info,1) ->
    [pid];
bif_type_args(process_info,2) ->
    [pid,any];
bif_type_args(processes,0) ->
    [];
bif_type_args(purge_module,1) ->
    [module];
bif_type_args(put,2) ->
    [any,any];
bif_type_args(read_timer,1) ->
    %% badly named in man page
    [ref];
bif_type_args(ref_to_list,1) ->
    [ref];
bif_type_args(register,2) ->
    [any,pid];
bif_type_args(registered,0) ->
    [];
bif_type_args(resume_process,1) ->
    [pid];
bif_type_args(round,1) ->
    [number];
bif_type_args(self,0) ->
    [];
bif_type_args(send_after,3) ->
    [time,pid,any];
bif_type_args(set_cookie,2) ->
    [node, cookie];
bif_type_args(setelement,3) ->
    [integer,tuple,any];
bif_type_args(size,1) ->
    [tuple_or_binary];
bif_type_args(spawn,1) ->
    ['fun'];
bif_type_args(spawn,2) ->
    [node, 'fun'];
bif_type_args(spawn,3) ->
    [module,function,[any]];
bif_type_args(spawn,4) ->
    [node, module, function, [any]];
bif_type_args(spawn_link,1) ->
    ['fun'];
bif_type_args(spawn_link,2) ->
    [node, 'fun'];
bif_type_args(spawn_link,3) ->
    [module, function, [any]];
bif_type_args(spawn_link,4) ->
    [node, module, function, [any]];
bif_type_args(spawn_opt,4) ->
    [module, function, [any], options];
bif_type_args(split_binary,2) ->
    [binary,integer];
bif_type_args(start_timer,3) ->
    [integer, pid, any];
bif_type_args(statistics, 1) ->
    %% actually atom
    [any];
bif_type_args(suspend_process,1) ->
    [pid];
bif_type_args(system_flag,2) ->
    [any,any];
bif_type_args(system_info,1) ->
    [any];
bif_type_args(term_to_binary,1) ->
    [any];
bif_type_args(term_to_binary,2) ->
    [any,[any]];
bif_type_args(throw,1) ->
    [any];
bif_type_args(time,0) ->
    [];
bif_type_args(tl,1) ->
    [list];
bif_type_args(trace,3) ->
    [pid_spec, bool, [flag]];
bif_type_args(trace_info,2) ->
    [pid_or_new, any];
bif_type_args(trace_pattern, 2) ->
    [mfa, matchspec];
bif_type_args(trace_pattern, 3) ->
    [mfa, matchspec, [flag]];
bif_type_args(trunc,1) ->
    [number];
bif_type_args(tuple_to_list,1) ->
    [tuple];
bif_type_args(universaltime,0) ->
    [];
bif_type_args(universaltime_to_localtime,1) ->
    [datetime];
bif_type_args(unlink,1) ->
    [pid];
bif_type_args(unregister,1) ->
    [any];
bif_type_args(whereis,1) ->
    [any];
bif_type_args(yield,0) ->
    [];
bif_type_args(F,N) ->
    warning("bif ~p/~p not recognized~n", [F, N]),
    lists:duplicate(N, unknown).

warning(Str, Xs) ->
    io:format("WARNING" ++ Str, Xs).
