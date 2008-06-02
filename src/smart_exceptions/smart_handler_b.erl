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
%%		     BEHAVIOUR FOR SMART HANDLER
%%
%% Author: Thomas Lindgren (021118-)
%%
%% This is what a 'smart exception' handler must satisfy. Note that we
%% do not define/require any return value; the simplest handler just throws an
%% elaborated exception.
%%
%%   switch(MFA, Line, Rsn, Args) -> ...
%%        invoked if no switch clause matches
%%    match(MFA, Line, Rsn, Pattern, Val) -> ...
%%        invoked if "P = E" fails
%%    binop(MFA, Line, Rsn, Op, X1, X2) -> ...
%%        invoked if "X1 op X2" fails
%%     unop(MFA, Line, Rsn, Op, X1) -> ...
%%        invoked if "op X1" fails
%%      bif(MFA, Line, Rsn, Fn, Xs) -> ...
%%        invoked if "f(X1,...,Xn)" fails
%%     exit(MFA, Line, Rsn) -> ...
%%        invoked at "exit(Rsn)"
%%
%% Note: Args for switch is a list of the switch arguments, which is fixed
%%  for all except one.
%%  case:     [X]
%%  if:       []
%%  (receive:  [])
%%  function: [X1,...,Xn]
%%
%% Note: Pattern is the syntax tree of the pattern.

-module(smart_handler_b).
-author('thomasl_erlang@yahoo.com').
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{switch,4},
     {match, 5},
     {binop, 6},
     {unop, 5},
     {bif, 5},
     {exit, 3}];
behaviour_info(_) ->
    undefined.
