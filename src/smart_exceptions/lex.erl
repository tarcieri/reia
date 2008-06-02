%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @copyright Copyright(C) 2004-2005 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% @license
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
%%			   LEXICAL ANALYSIS
%%
%% Author: Thomas Lindgren (040422-040425; 041125; 051007-051016)
%%
%% Lexical analysis of strings: convert a sequence into a list of tokens
%% according to regexp rules.
%%
%% USAGE:
%%   lex:with([Regexp_rule], Sequence) -> [Token]
%%     Regexp_rule is same as for regexps_to_table/2
%%   lex:file_with([Regexp_rule], File) -> [Token]
%%     As above but lexes a whole file.
%%
%% A sequence is a list of characters, a binary, or a pair of binary
%% and position. Positions are an abstract datatype, see start_pos/0
%% and its friends below. (You normally SHOULD NOT need to generate positions
%% yourself.)
%%
%% EXAMPLES
%%     lex:with(lex:real_erlang_lex(), "foo() -> zap().")
%%     lex:file_with(lex:real_erlang_lex(), "file.erl")
%%
%%   For writing your own rules, see real_erlang_lex() and below. The functions
%%  it uses to emit tokens and count lines can often be reused.
%%
%% MORE API:
%%   regexps_to_table([{regexp, [Prio,] Regexp_string, Action [,Prio]}]) -> 
%%                    Lex_table
%%     where Action(AccToken) -> {token, Token} | no_token
%%           Regexp_string is a regexp according to the regexp module
%%           Prio is an integer rule priority (default: 0)
%%            that chooses the rule to accept if there are several possible
%%   longest(Lex_table, String) -> [Token]
%%      Use a generated lex table to convert a string into a list of tokens.
%%
%%   regexps_to_nfa([{regexp, Regexp_string, Action}]) -> NFA
%%     where Action(AccToken) -> {token, Token} | no_token
%%   nfa_to_dfa(NFA) -> DFA
%%   dfa_to_table(DFA) -> Lex_table
%%
%%   See also below for help in specifying your lexers.
%%
%% 1. Specify (regexp -> action) rules
%% 2. Translate regexp -> NFA
%% 3. Translate NFA -> DFA (+ check for uniqueness)
%% 4. Generate table representing DFA
%%
%% This is a simple subset of lex/flex. When there are two matching tokens,
%% the longest is always chosen. (I.e., if you are in an accepting state and
%% more characters arrive, you continue.)
%%
%% We also provide a library for keeping track of the number of lines
%% seen so far.
%%   no_lines()                      reset line count to zero
%%   inc_lines(), inc_lines(N)       increment line count by 1 or N
%%   current_line() -> N             returns current line number
%%   count_lines(Str) -> N           count newlines in string
%%
%% The available lex driver sets no_lines() before lexing.
%% Note that the line count is global per-process at this
%% time.
%%
%% Usage in your lexer:
%%    {regexp, "\n", fun(_) -> lex:inc_lines(), no_token end}
%%    {regexp, "...", fun(Acc) -> lex:count_lines(Acc), no_token end}
%%    {regexp, "<", fun(_) -> {token, {op, lex:current_line(), '<'}} end}
%% see also erlang_lex() and real_erlang_lex() below.
%%
%% YECC COMPATIBILITY
%%
%% Yecc expects the tokenizer to return tuples on the form
%%   {Category, LineNumber, Symbol} or
%%   {Category, LineNumber}
%% if there is just one member of the Category. For example, all operators
%% are the same while variables normally are different, so we would return
%% something like
%%   {var, 42, "VarName"}
%% or
%%   {'+', 17}
%%
%% Furthermore, the list of tokens should end with
%%   {End_symbol, LineNumber}
%% (where End_symbol is declared in the yecc file)
%%
%% It is up to you to return tokens suitable for yecc, but as you can see
%% it's not too hard. Line numbers can 
%%
%% STATUS:
%% A lex table for erlang_lex() becomes 142 DFA states and is generated
%% (from regexps to ready to use) in 54.8 msec on my Athlon 1300+. This
%% is approximately as complex as a programming language lexer gets.
%%
%% No known bugs.
%%
%% To understand the code below, you should know about (deterministic
%% and nondeterministic) finite automata.
%%
%% LEXING PERFORMANCE NOTE:
%%
%% The driver tries to match a maximally long token (aka "maximal munch")
%% which means there can be _backtracking_ if a long match ultimately fails.
%% In that case, the driver emits the longest known match and continues from
%% the saved point. This means characters can be traversed several times.
%% Consider the regexp (a|a*b), which matches a single 'a' or any number of
%% 'a' followed by a 'b'. 
%%
%% For the string "aaaaa", the driver will traverse it all to try to
%% match a*b, then see there is no match and emit 'a'. Then the same
%% thing is done with the remaining "aaaa", "aaa", "aa" and "a". So
%% lexing can be slow in this respect (quadratic at worst, I think, as
%% shown, since you need to match at least one character every time and
%% each match takes linear time).
%%
%% I'm not sure what is done in flex etc. for the corresponding situation.
%% Note that this sort of overlap is not so common in real use (you need
%% nasty regexps and nasty input), but be aware that it _may_ happen.
%% 
%% RANDOM NOTES:
%% - An application: composing collections of regexps is simple, you
%%   just set their priorities and concatenate => you can easily extend
%%   your tokenizer dynamically! (just regenerate the lexer)
%% - lex generation is fast, real_erlang_lex() is generated in 66-68
%%   milliseconds on my 1.6 GHz laptop
%%
%% *** UNFINISHED ***
%% - only handles UTF-8 (or rather, "8-bit ASCII"), will require
%%   a rewrite to handle larger charsets
%%   * curr. table requires about (s * c) words, where s is the number
%%     of states and c the number of characters
%%     = for 100 states and 65K characters, we get a table of about 26 MB
%%       which is normally impractical
%%   * curr. lex drivers work one byte at a time -- they will have to
%%     be rewritten to munch 1 character at a time (this is probably
%%     straightforward)
%%   * probably other issues as well
%% - permit more generous regexps? I don't know what are suitable ones
%%   * flex has, e.g., match beginning of line
%% - the regexp ".*" should probably be replaced by "[^\n]*", which
%%   only matches up to end of line (I think the latter is what flex
%%   uses)
%% - extend lex drivers:
%%   * incremental 1: if sequence ends, return a resumable state
%%   * incremental 2: ("yacc compatible") when a token has been
%%       matched, return it and a resumable state
%%   * GENERATE CODE rather than just tables
%%     = suitable when you want something really standalone
%%     = simple version: emit a suitable driver + the table into a module
%%       * actions must be emitted as text, somehow, rather than as
%%         #fun<..>, obviously
%%     = more complex: PE of driver wrt table
%%   * variable handling of non-match
%%     = simply fail (as now)
%%     = emit non-matching character(s) (as lex/flex does)
%%     = log failed stuff, etc
%%     = pass a closure to handle non-matching?
%%
%% some finer details also remain to be addressed:
%% - what if {seq, []}? (never happens, normally)
%% - what if {disj, []}? (dito)
%% - unicode vs 7b ascii vs 8b ascii vs ...? hmm
%%   * "lexing chinese"?
%%   * the current table structure probably is insufficient for large
%%     character sets; it should be possible to move to something less
%%     direct

-module(lex).
-author('thomasl_erlang@yahoo.com').
-export([with/2,
	 regexps_to_table/1,
	 regexps_to_nfa/1,
	 nfa_to_dfa/1,
	 dfa_to_table/2
	]).

%% library functions for using the generated lexer
-export([longest/2]).

%% library functions for counting the number of lines
-export([no_lines/0,
	 inc_lines/0,
	 inc_lines/1,
	 current_line/0,
	 count_lines/1
	]).

%% Examples
-export([test_lexer/0,
	 erlang_lex/0,
	 real_erlang_lex/0,
	 test/1,
	 test/2,
	 test_all/1,
	 test_exp/1
	]).

%% Used when lexing a binary:
-define(least_pos, 0).

%% Wrapped io:format
-define(log(Str, Xs), ok).
%% -define(log(Str, Xs), io:format(Str, Xs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Quick entrypoint

with(Regexp_rules, String) ->
    Tab = regexps_to_table(Regexp_rules),
    longest(Tab, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regexps_to_table(Regexp_rules0) ->
    Regexp_rules = regexps_to_internal(Regexp_rules0),
    NFA = regexps_to_nfa(Regexp_rules),
    DFA = nfa_to_dfa(NFA),
    Lex_table = dfa_to_table(DFA, Regexp_rules),
    Lex_table.

%% Convert all regexp rules to internal form

regexps_to_internal(Regexp_rules0) ->
    [ regexp_to_internal(N, Rule)
      || {N, Rule} <- zip(lists:seq(1,length(Regexp_rules0)), Regexp_rules0) ].

%% Convert a single regexp rule to internal form

regexp_to_internal(N, {regexp, Regexp_str, Action}) ->
    Prio = 0,
    {regexp, N, parse_regexp(Regexp_str), Action, Prio};
regexp_to_internal(N, {regexp, Prio, Regexp_str, Action}) 
  when integer(Prio), Prio >= 0 ->
    {regexp, N, parse_regexp(Regexp_str), Action, Prio}.
    
%% ?max_char is currently used to decide the "charset size"
%% we permit characters 0-?maxchar (excepting ?no_match,)

-define(max_char, 255).

%% Possibly not needed, but for the sake of symmetry
%%
%% NOTE: currently,the code generator relies on doing "element" on
%%  tuples, so we _can't_ handle  ?min_char = 0 ...

-define(min_char, 1).

%% ?no_match is a reserved state indicating error/no match possible
%%
%% - when the table is represented as a tuple of tuples, we can have
%%   ?no_match = -1
%% - if the table is a binary, we would like to fit all characters into
%%   an unsigned byte ... in that case, ?no_match can't be -1. Not sure
%%   what it should be. (255?)

-define(no_match, -1).

%% For compact tables, the value must be unsigned (which is a drawback).

-define(compact_no_match, 255).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% We currently handle a SUBSET of the regexps of Erlang/OTP:s regexp
%% matching (regexp(3)), a subset of AWKs.
%%
%% c       char c
%% \c      escaped c
%% .       any char
%% ^       beginning of string
%% $       end of string
%% [abc]   characters abc
%% [^abc]  complement
%% r*      zero or more r
%% r+      one or more r
%% r?      zero or one r
%% r1r2    sequencing
%%
%% {optional, RE}        r?
%% {comp_class, Chars}   complementation of chars
%% {pclosure, RE}        r+
%% bos                   "beginning of string", ^
%% eos                   "end of string", $

parse_regexp(Regexp) ->
    case regexp:parse(Regexp) of
	{ok, RE} ->
	    internal_form(RE);
	Err ->
	    exit(Err)
    end.

%% Convert to internal form
%%
%% *** UNFINISHED ***
%% - does not handle beginning/end of string

internal_form({kclosure, RE}) ->
    {iter, internal_form(RE)};
internal_form({'or', RE1, RE2}) ->
    flatten_disj([internal_form(RE1), internal_form(RE2)]);
internal_form({optional, RE}) ->
    {disj, [{seq, ""}, internal_form(RE)]};
internal_form({char_class, Char_ranges}) ->
    {disj, [ internal_range(Range) || Range <- Char_ranges ]};
internal_form({comp_class, Char_ranges}) ->
    %% how do we handle this? depends on charset, right?
    %% (or build a more complex range merger...)
    flatten_disj(complement_char_ranges(?min_char, ?max_char, Char_ranges));
internal_form({concat, RE1, RE2}) ->
    flatten_seq([internal_form(RE1), internal_form(RE2)]);
internal_form({From, To}) when integer(From), integer(To), From =< To ->
    {range, From, To};
internal_form(N) when integer(N) ->
    {range, N, N};
internal_form({pclosure, RE}) ->
    RegExp = internal_form(RE),
    {seq, [RegExp, {iter, RegExp}]};
internal_form(Other) ->
    exit({regexp_not_handled, Other}).

internal_range({From, To}) when integer(From), integer(To), From =< To ->
    {range, From, To};
internal_range(N) when integer(N) ->
    {range, N, N}.

%% Convert a complemented range into a non-complemented one

complement_char_ranges(MinChar, MaxChar, Char_ranges) ->
    complement(MinChar, MaxChar, [ internal_range(R) || R <- Char_ranges ]).

complement(MinChar, MaxChar, Ranges) ->
    compl(MinChar, MaxChar, sort_distinct_ranges(Ranges)).

%%

sort_distinct_ranges(Ranges) ->
    lists:sort(
      fun({range, From0, To0}, {range, From1, To1}) ->
	      From0 < From1
      end,
      Ranges
     ).

%% compl/3 generates new ranges spanning M..N but not the parameter Ranges
%%
%% E.g., compl(1, 255, [{range, 10, 10}]) -> [{range, 1, 9}, {range, 11, 255}]

compl(M, N, [{range, From, To}|Ranges]) when M =< N, From =< N ->
    if
	M < From ->
	    [{range, M, From-1}|compl(To+1, N, Ranges)];
	true ->
	    compl(To+1, N, Ranges)
    end;
compl(M, N, []) ->
    if
	M =< N ->
	    [{range, M, N}];
	true ->
	    []
    end.

%% Flatten a disjunction of disjunctions

flatten_disj(Xs) ->
    {disj, collect_disj(Xs)}.

collect_disj([{disj, Xs}|Rest]) ->
    collect_disj(Xs) ++ collect_disj(Rest);
collect_disj([X|Xs]) ->
    [X|collect_disj(Xs)];
collect_disj([]) ->
    [].

%% Flatten a sequence of sequences

flatten_seq(Xs) ->
    {seq, collect_seq(Xs)}.
    
collect_seq([{seq, Xs}|Rest]) ->
    collect_seq(Xs) ++ collect_seq(Rest);
collect_seq([X|Xs]) ->
    [X|collect_seq(Xs)];
collect_seq([]) ->
    [].

%% The abstract syntax tree of regexps:
%%
%%  {seq, [R]} | {disj, [R]} | {range, From, To} | {iter, R}
%%
%% Other regexps can (probably) be compiled into these. Check what flex is
%% capable of.

%% A finite automaton is represented as
%%  [{State, Accept, [Arc]}]
%%
%%  where State, NextState are integers
%%        Accept is non_accept | {accept, ID}
%%        Arc is an arc {range, From, To, NextState} | {epsilon, NextState}
%%  such that no two non-epsilon Ranges overlap
%%
%% A character range is {range, From, To} | Char | epsilon
%%  (possibly also {not, From, To}?)
%%
%% An automaton without epsilon-transitions is called deterministic (DFA).
%% One with epsilon-transitions is an NFA.
%%

%% Convert a collection of regexps into an NFA
%%
%% Translation system:
%%
%%  r1 ... rn => -> (r1) -> (r2) -> ... -> (rn) -> (accept)
%%   where (r) is the automaton for r and -> is an epsilon transition
%%   from the accepting state of automaton A to the entry state of B.
%%
%%  [range] => -> () -R-> (accept)
%%    where () is a new state and -R-> is an arc labelled with range R
%%
%%  (r1 | ... | rn) =>  () -> (ri) -> (accept)
%%    that is, an empty start state with epsilon transitions to ALL the
%%    automata for the regexps, which then epsilon-transit to the accept
%%    state
%%    * note: the toplevel NFAs must be handled differently, since each
%%      must end up in a different accepting state!
%%
%%  (r)* => -> (r)<- -> (accept)
%%    where <- is an arc from (r) back to (r) and -> (accept) is an
%%    epsilon arc to the accepting state
%%
%%
%% The automata are built up from end to start, so that every automaton
%% always has an existing "next state" to go to, the "accept state" which
%% is the parameter.
%%
%% NOTE: we assume that each NFA state has non-overlapping outgoing ranges.
%%  If you want to express c:(A -> B | A -> C), that we transition from
%%  A to B or C when c is found, then you MUST use epsilon transitions:
%%   (A eps-> S1, A eps-> S2, S1 -c-> B, S2 -c-> C)
%%
%% This is _equivalent_to_ having overlapping ranges, but is less terse
%% (for good or bad).

%% Regexps = [{regexp, RuleID, Regexp_syntree, Action, Prio}]
%%
%% The automaton for a regexp is
%%   () -> (ri) -> (accepti)
%% where
%% () is the starting state, (ri) the automaton for ri and (accepti)
%% the accepting state for ri (invoking its action)
%% and -> here is an epsilon transition.
%%
%% For all regexps, we have a single start state that (conceptually
%% at least) epsilon-transitions to each of the individual start states.
%%
%% NOTE: we ensure/assume that the accepting states are low-numbered.
%%  This is implicit in calling init_accepting_states/1 to get the FA.

regexps_to_nfa(Regexps) ->
    {AccStates, FA1} = init_accepting_states(Regexps),
    {StartStates, FA2} = nfa_states(Regexps, AccStates, FA1),
    {St, FA3} = add_epsilon_transitions(StartStates, FA2),
    set_fa_start_state(St, FA3).

%% Add initial accepting states, returns acc.states + FA

init_accepting_states(Regexps) ->
    lists:mapfoldl(
      fun({regexp, AccID, Regexp, Action, Prio}, FA0) ->
	      add_accepting_state(AccID, Prio, [], FA0)
      end,
      empty_fa(),
      Regexps
     ).

%% Add NFAs ending in accepting states, returns start states + FA

nfa_states(Regexps, AccStates, FA1) ->
    lists:mapfoldl(
      fun({{regexp, AccID, Regexp, Action, Prio}, AccState}, FA_in) ->
	      regexp_to_nfa(Regexp, AccState, FA_in)
      end,
      FA1,
      zip(Regexps, AccStates)
     ).

%% Add epsilon transition to all the start states

add_epsilon_transitions(StartStates, FA2) ->
    add_state([ {epsilon, Start} || Start <- StartStates ], FA2).

%% takes (Regexp, ExitState, FA) as arguments
%% returns {EntryState, NewFA}

regexp_to_nfa({seq, Rs}, Accept, FA) ->
    lists:foldr(
      fun(Regexp, {AccState, FA0}) ->
	      regexp_to_nfa(Regexp, AccState, FA0)
      end,
      { Accept, FA},
      Rs
     );
regexp_to_nfa({disj, Rs}, Accept, FA0) ->
    {StartStates, FA1} =
	lists:mapfoldl(
	  fun(Regexp, FA_in) ->
		  regexp_to_nfa(Regexp, Accept, FA_in)
	  end,
	  FA0,
	  Rs
	 ),
    add_state([ {epsilon, Start} || Start <- StartStates ], FA1);
regexp_to_nfa({range, From, To}, Accept, FA) ->
    add_state([{range, From, To, Accept}], FA);
regexp_to_nfa({iter, R}, Accept, FA0) ->
    {Last, FA1} = add_state([], FA0),
    {StartState, FA2} = regexp_to_nfa(R, Last, FA1),
    FA3 = add_epsilon(Last, StartState, FA2),
    {StartState, add_epsilon(StartState, Accept, FA3)}.

%% regexp_to_nfa({iter, R}, Accept, FA0) ->
%%  {StartState, FA1} = regexp_to_nfa(R, Accept, FA0)
%%  {StartState, add_epsilon(Accept, StartState, FA1)}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Converting an NFA to a DFA:
%%
%% There are two main issues
%% 1. getting rid of epsilon-transitions
%% 2. ensuring that an accepting state has a unique rule associated with it
%%    (or rather, the transition out of it)
%%
%% Getting rid of epsilon: (starting with start state)
%% - merge states accessible by epsilon transitions
%% - outgoing args are merged wrt their ranges
%% ... until none remain
%%
%% *** UNFINISHED ***
%% - the accepting states should be named 1..maxaccept to simplify
%%   checking if we are in one
%%   * this should probably be done by renaming them ...

nfa_to_dfa(NFA) ->
    Start = start_state(NFA),
    {NewStateName, Pre_DFA} = merge_nfa_states([Start], NFA, empty_pre_dfa()),
    DFA = pre_dfa_to_dfa(NewStateName, Pre_DFA),
    ?log("NFA states: ~w~nDFA states: ~w~n", 
	 [length(states_of(NFA)), length(states_of(DFA))]),
    DFA.

%% list of States to be merged and inserted into Done
%% - if already done, do nothing
%% - otherwise, merge it, then merge successors
%%
%% return {MergedStateName, Done_states}

merge_nfa_states(States0, NFA, Done0) ->
    States = epsilon_reachable_all(States0, NFA),
    %% io:format("merged states = ~w~n", [States]),
    case states_present(States, Done0) of
	{found, Name} ->
	    {Name, Done0};
	not_found ->
	    {DFA_name, Done1} = name_dfa_state(States, Done0),
	    {state, Acc, Succs} = merge_epsilon_reachable_states(States, NFA),
	    {NewSuccs, Done2} = merge_nfa_succ_list(Succs, NFA, Done1),
	    {DFA_name, 
	     set_dfa_state(
	       DFA_name,
	       {state, Acc, NewSuccs},
	       Done2
	      )
	    }
    end.

merge_nfa_succ_list(Ranges, NFA, Done0) ->
    lists:mapfoldl(
      fun(Range, Done) ->
	      merge_nfa_succ(Range, NFA, Done)
      end,
      Done0,
      Ranges
     ).

merge_nfa_succ({From, To, Next}, NFA, Done0) ->
    {NewNext, Done1} = merge_nfa_states(Next, NFA, Done0),
    {{range, From, To, NewNext}, Done1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% How do we merge two (or more) states into one?
%% (a) epsilon transitions: ignore, we assume we will merge all
%%     epsilon-reachable states
%% (b) non-overlapping ranges: just keep
%% (c) overlapping ranges:
%%     - overlapping range to states s1,...,sn => {s1,...,sn} is a new state
%%       to be merged too
%%     (note: there may furthermore be PARTIAL and MULTIPLE overlaps.)

%% Merge epsilon-reachable states by merging their ranges
%%
%% Returns a new semi-state such that:
%% - name is union of states' names (= not a proper state name)
%% - accepting if precisely one of the states is accepting
%% - successors according to unions taken (these are pseudo-names, since
%%   the states as such DO NOT EXIST yet)

merge_epsilon_reachable_states(StateIDs, NFA) ->
    StatesInfo = [ state(StateID, NFA) || StateID <- StateIDs ],
    Acceptance =
	lists:foldl(
	  fun({state, Acc0, _Succs0}, Acc1) ->
		  lub_acceptance(Acc0, Acc1)
	  end,
	  non_accepting,
	  StatesInfo
	 ),
    %% should printout StatesInfo, Succs_list, Acceptance for debugging ...
    Succs_list = 
	[ ranges_to_set_form(Succs) || {state, _Acc0, Succs} <- StatesInfo ],
    case Succs_list of
	[] ->
	    {state, Acceptance, []};
	_ ->
	    MergedSuccs = lists:foldl(
			    fun merge_ranges/2,
			    hd(Succs_list),
			    tl(Succs_list)
			   ),
	    {state, Acceptance, MergedSuccs}
    end.

%% Two rules can match in the same accepting state. This function chooses
%% which rule is the actual match:
%% 1. best priority wins
%% 2. if same priority, compare rule IDs (least-numbered rule wins)
%%    and warn that this happened
%%
%% The warning is emitted because the rule selection may not have been
%% intended by the user.

lub_acceptance(non_accepting, X) ->
    X;
lub_acceptance(X, non_accepting) ->
    X;
lub_acceptance({accepting, Rule1, Prio1}, {accepting, Rule2, Prio2}) 
  when Rule1 == Rule2 ->
    {accepting, Rule1, Prio1};
lub_acceptance({accepting, Rule1, Prio1}, {accepting, Rule2, Prio2}) 
  when Rule1 =/= Rule2 ->
    if
	Prio1 > Prio2 ->
	    {accepting, Rule1, Prio1};
	Prio1 < Prio2 ->
	    {accepting, Rule2, Prio2};
	Prio1 == Prio2 ->
	    %% in this case, disambiguate by comparing rule IDs
	    %% (arbitrary)
	    {Rule, Prio, ShadowedRule} =
		if
		    Rule1 < Rule2 ->
			{Rule1, Prio1, Rule2};
		    Rule1 > Rule2 ->
			{Rule2, Prio2, Rule1};
		    Rule1 == Rule2 ->
			%% can never happen (handled by previous clauses)
			{Rule1, Prio1, Rule2}
		end,
	    warning(
	      "both ~w and ~w can be accepted at once,"
	      " ~w shadowed~n", 
	      [Rule1, Rule2, ShadowedRule]),
	    {accepting, Rule, Prio}
    end.

-record(ranges, {r0, r1}).

%% This is the entry point: we have two states with successor-ranges
%% Ranges0 and Ranges1, merge them.
%%
%% Note: successor ranges must have LISTS of states in successor position.

merge_ranges(Ranges0, Ranges1) ->
    merge_ranges_with_ranges(Ranges0, Ranges1).

%%
      
ranges_to_set_form(Ranges0) ->
    lists:flatten([ convert_to_set_form(R0) || R0 <- Ranges0 ]).

%% Convert a range arc into suitable form for range-merging

convert_to_set_form({range, From, To, Next}) ->
    {From, To, singleton(Next)};
convert_to_set_form({epsilon, Next}) ->
    %% discarded when list flattened
    [].

%% Merge the active range (From0, To0) with a list of "passive" ranges
%%
%% returns a new list of ranges

merge_range_with_ranges(From0, To0, S0, []) ->
    [{From0, To0, S0}];
merge_range_with_ranges(From0, To0, S0, [{From1, To1, S1}|Ranges]) ->
    #ranges{r0=Actives, r1=Passives} = 
	range_overlap(From0, To0, S0, From1, To1, S1),
    Passives ++ merge_ranges_with_ranges(Actives, Ranges).

%% Merge a ("active") list of ranges into a list of ("passive")
%% non-overlapping ranges.
%%
%% note: this is inefficient, since we insert Active into the list
%% then traverse it -- yet we are guaranteed that the Actives NEVER overlap

merge_ranges_with_ranges(Actives, Ranges0) ->
    lists:foldl(
      fun({From0, To0, S0}, Ranges) ->
	      merge_range_with_ranges(From0, To0, S0, Ranges)
      end,
      Ranges0,
      Actives
     ).

%% The relationship between two ranges R0=(From0, To0) and R1=(From1, To1)
%% where R0 is an _active_ range (being merged with a list of passive ranges)
%% and R1 is a passive range.
%%
%% We have a large number of cases:
%%   R0 == R1
%%   R0 and R1 do not overlap
%%   R0 includes R1 (on both sides, or one side) = 3 cases
%%   R1 includes R0 (on both sides, or one side) = 3 cases
%%   R0 and R1 partially overlap = 2 cases
%% sum: 10 cases
%%
%% We return #ranges{r0, r1}
%%  where r0 = new active ranges
%%        r1 = new passive ranges 

range_overlap(From0, To0, S0, From1, To1, S1) ->
    if
	From0 == From1,
	To0 == To1 ->
	    %% intervals are the same, so r0 is covered and the
	    %% new range in r1 leads to s0+s1
	    #ranges{r0 = [],
		    r1 = [{From0, To0, union(S0, S1)}]};
	To0 < From1 ; To1 < From0 ->
	    %% no overlap, just return both ranges
	    #ranges{r0=[{From0, To0, S0}],
		    r1=[{From1, To1, S1}]};
	From0 == From1,
	To0 < To1 ->
	    %% R0 inside R1, lower bound the same
	    %% - r0 range entirely covered (since inside r1)
	    %% - the r1 range splits into two, one leading to s0+s1
	    %%   the other just to s1
	    #ranges{r0=[],
		    r1=[{From0, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From0 == From1,
	To0 > To1 ->
	    %% R1 inside R0, lower bound the same
	    %% - r0: the uncovered part returned, gets s0
	    %% - r1: add s0+s1 as states
	    #ranges{r0=[{To1+1, To0, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 > From1,
	To0 == To1 ->
	    %% R0 inside R1, upper bound the same
	    %% - r0 entirely covered
	    %% - r1 splits into two ranges, one covering r0, one not
	    %%   the covering one gets s0+s1, the other just s1
	    #ranges{r0=[],
		    r1=[{From1, From0-1, S1},
			{From0, To0, union(S0, S1)}]};
	From1 > From0,
	To0 == To1 ->
	    %% R1 inside R0, upper bound the same
	    %% - uncovered part of r0 gets s0
	    %% - overlapping part of r0 and r1 get s0+s1
	    #ranges{r0=[{From0, From1-1, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 > From1, To0 < To1 ->
	    %% R0 included in R1
	    %% - r0 entirely covered
	    %% - r1 splits into three intervals, middle one s0+s1, others s1
	    #ranges{r0=[],
		    r1=[{From1, From0-1, S1},
			{From0, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From0 < From1, To0 > To1 ->
	    %% R1 included in R0
	    %% - r0 returns two intervals, states s0
	    %% - r1 gets middle interval, states s0+s1
	    #ranges{r0=[{From0, From1-1, S0},
			{To1+1, To0, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 < From1,
	To0 < To1 ->
	    %% R0 overlaps R1 (in lower range)
	    %% - R0 alone gets s0
	    %% - R0, R1 overlap gets s0+s1
	    %% - R1 alone gets s1
	    #ranges{r0=[{From0, From1-1, S0}],
		    r1=[{From1, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From1 < From0,
	To1 < To0 ->
	    %% R1 overlaps R0 (in lower range)
	    %% - R1 alone gets s1 => r1
	    %% - R1, R0 overlap gets s0+s1 => r1
	    %% - R0 alone gets s0 => r0
	    #ranges{r0=[{To1+1, To0, S0}],
		    r1=[{From1, From0-1, S1},
			{From0, To1, union(S0, S1)}]};
	true ->
	    %% error case -- should never happen, right?
	    exit({range_compare_bug, {From0, To0}, {From1, To1}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% State = integer()
%% FA = finite_automaton()

epsilon_neighbours(State, FA) ->
    [ NextState || {epsilon, NextState} <- successors(State, FA) ].

epsilon_reachable(State, FA) ->
    All_seen = epsilon_reach(State, FA, none_seen()),
    seen_list(All_seen).

epsilon_reachable_all(States, FA) ->
    All_seen = epsilon_reach_all(States, FA, none_seen()),
    seen_list(All_seen).

%% depth-first search for all epsilon-reachable states;
%% keep track of already seen states
%%
%% returns Seen

epsilon_reach(State, FA, Seen) ->
    case seen(State, Seen) of
	false ->
	    %% visit all epsilon-neighbours depth-first
	    epsilon_reach_all(
	      epsilon_neighbours(State, FA), 
	      FA, 
	      see(State, Seen)
	     );
	true ->
	    Seen
    end.

%% collect all epsilon-reachable states beginning from States

epsilon_reach_all(States, FA, Seen) ->
    lists:foldl(
      fun(State0, Seen0) ->
	      epsilon_reach(State0, FA, Seen0)
      end,
      Seen,
      States
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Converting a DFA to a suitable table + using the table for parsing.
%%
%% The code must do two things:
%% 1. generate a suitable structure for lexing
%% 2. invoke the correct action when a match is found
%%
%% NOTES:
%% We can represent the DFA as an array with rows = states and columns
%% = chars (e.g., 128 columns for 7-bit ASCII). When in state k and getting
%% character c, the next state is array[k,c].
%%
%% (In Erlang, the best representation of the array when states are
%% few, e.g., 16-bit or less, may be a binary.)
%%
%% *** UNFINISHED ***
%% * A number of possible methods:
%% - generate direct code
%% - generate a table (array)
%% - generate a binary, index into it (faster?)
%% - generate a decision-tree per state?
%% - generate an ets-table (?)

dfa_to_table(DFA, Regexp_rules) ->
    {AcceptLimit, NewDFA} = group_accepting_states(DFA),
    ?log("~p accepting states~n", [AcceptLimit]),
    Actions = action_table(Regexp_rules, NewDFA, AcceptLimit),
    generate_code(NewDFA, AcceptLimit, Actions).

generate_code(DFA, AcceptLimit, Actions) ->
    Table = generate_table(DFA),
    Start = start_state(DFA),
    {lex, Start, AcceptLimit, Table, Actions}.

%% The table is a tuple of one row/element per state
%%
%% (Other approach: index as element(This_state*?chars + Char, Table)
%% - one extra multiply-add (2 BIFs)
%% - current approach: extra load + extra element/3
%% so, is it worth it?)
%%
%% *** UNFINISHED ***
%% I think we should check that states are contiguous and starting at 1
%% as well, since we kind of rely on this.

generate_table(DFA) ->
    States = lists:keysort(
	       1,
	       [ {ID, state(ID, DFA)} || ID <- states_of(DFA) ]),
    list_to_tuple([ generate_row(State) || State <- States ]).

%% each row is the transition function for the corresponding state,
%% a tuple of ?maxchar elements

generate_row({ID, {state, Acc, Succs}}) ->
    list_to_tuple(ranges_to_list(sort_successors(Succs))).

sort_successors(Succs) ->
    Sorts =
	lists:sort(
	  fun({range, From0, To0, St0}, {range, From1, To1, St1}) ->
		  From0 < From1
	  end,
	  Succs
	 ),
    %% io:format("sorted successors: ~w~n",[Sorts]),
    Sorts.

%% Generate a list of [St1, St2, ..., StN] where each Sti is the next state
%% to use

ranges_to_list(Ranges) ->
    lists:flatten(
      [ lists:duplicate(To-(From-1), NxtSt) 
	|| {From, To, NxtSt} <- rtol(?min_char, ?max_char, ?no_match, Ranges) ]
     ).

%% rtol "ranges to list of ranges"
%%
%% M: current "index"/Mth element of final list
%% N: max index
%% St: current state to emit
%% third argument is the ranges to emit

rtol(M, N, St, [{range, From, To, NxtSt}|Rs]) when M =< N ->
    [{M, From-1, St}, {From, To, NxtSt} | rtol(To+1, N, St, Rs)];
rtol(M, N, St, []) ->
    if M =< N ->
	    [{M, N, St}];
       true ->
	    []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% In order to quickly check that a state is accepting, ensure that the
%% accepting states are 1..maxaccept. If so, the test is a simple compare.
%%
%% This is not guaranteed by our NFA->DFA conversion (and is difficult
%% to do, since the accepting states may be many). We do it in a separate
%% pass instead.

group_accepting_states(FA) ->
    State_info =
	[ {ID, accepting_state(ID, FA)} || ID <- states_of(FA) ],
    Acc = [ ID || {ID, true} <- State_info ],
    N = length(Acc),
    Non_Acc = [ ID || {ID, false} <- State_info ],
    NumStates =
	length(State_info),
    Map = rename_map(Acc ++ Non_Acc, 1),
    NewStart = rename_state(start_state(FA), Map),
    NewFA = set_next_id(
	      NumStates+1,
	      set_start_state(
		NewStart, 
		rename_states(Map, FA))),
    {N, NewFA}.

rename_map([ID|IDs], NewID) ->
    [{ID, NewID} | rename_map(IDs, NewID+1)];
rename_map([], NewID) ->
    [].

%% create a new FA with renamed states
%%
%% Note: we explicitly set the state NewID, though I think we could just
%% add a new state. But just adding states means we do not _ensure_ that
%% the NewID is correct, we just assume it is. So I avoid that.

rename_states(Map, FA) ->
    lists:foldl(
      fun({ID, NewID}, NewFA) ->
	      {state, Acc, Succ} = state(ID, FA),
	      NewSucc = [ {range, From, To, rename_state(St, Map)} 
			  || {range, From, To, St} <- Succ ],
	      NewState = {state, Acc, NewSucc},
	      set_state(NewID, NewState, NewFA)
      end,
      empty_fa(),
      Map
     ).

rename_state(X, Map) ->
    rename_state(X, Map, Map).

rename_state(X, [{X, Y}|_], Map) ->
    Y;
rename_state(X, [_|Xs], Map) ->
    rename_state(X, Xs);
rename_state(X, [], Map) ->
    exit({state_not_found, Map}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Driving the lexical analysis
%%
%% We provide functions that performs tokenization over a string or a
%% binary given a table to work with.
%%
%% There are three kinds of states:
%% - accepting
%% - non-accepting
%% - error (this means no match possible)
%%
%% Longest match: emit latest token when transiting into error state;
%%  save token when transiting from accepting to non-accepting state
%%  since we may reenter an accepting state later
%%  (this means more work and some backtracking, but is probably what
%%  is intended)

%% longest(Lex_table, Sequence) -> [Token]
%%
%% A sequence is a string (list of char) or a binary

longest(Lex, Seq) when binary(Seq) ->
    longest_bin(Lex, Seq, start_pos());
longest(Lex, {Bin, Pos}) when binary(Bin), integer(Pos), Pos >= ?least_pos ->
    longest_bin(Lex, Bin, Pos);
longest(Lex, Seq) when list(Seq) ->
    longest_string(Lex, Seq).

%% Longest match applied to string (list of chars)

longest_string({lex, Start, AcceptLimit, Table, Actions}, String) 
  when list(String) -> 
    no_lines(),
    longest_string(Start, Table, AcceptLimit, Actions, String).

longest_string(Start, Table, AcceptLimit, Actions, String) ->
    Row = transition_table(Start, Table),
    pre_accept(String, none_acc(), Start, Row, 
	       Table, AcceptLimit, Start, Actions).

%% [C|Cs] string to be lexed
%% Acc = accumulated possible match so far
%% State = current state
%% Nxt = state transition vector
%% Table = state transition table
%% AcceptLimit = max. # for accepting state (up to this are accepting)
%% Start = starting state
%%
%% If we reach an error state or end of string, stop with error. No match
%%  was possible.
%%
%% If we enter an accepting state, then go to accept
%%
%% Otherwise, keep going in pre_acc.

pre_accept([C|Cs], Acc, State, Nxt, Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no match possible, fail
	    {no_match, lists:reverse(Acc), [C|Cs]};
        N when N =< AcceptLimit ->
	    %% move into accepting state N
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), none_acc(), N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    NewNxt = transition_table(N, Table),
	    pre_accept(Cs, acc(C, Acc), N, NewNxt, 
		    Table, AcceptLimit, Start, Actions)
    end;
pre_accept("", Acc, State, Nxt, Table, AcceptLimit, Start, Actions) ->
    %% no match
    {no_match, lists:reverse(Acc), ""}.

%% [C|Cs] = lexed string
%% Acc = accumulated token
%% AccSt = previous accepted token
%% N = state
%% Nxt = state transition
%% Table = lex table
%% AcceptLimit = all states less than this are accepting
%%
%% If we enter an error state or the string ends, emit the current token
%%  and match next token (if possible)
%%
%% If we stay in an accepting state, go on.
%%
%% If we get into a non-accepting state, save the current match and
%%  go into post_accept

accept([C|Cs]=Lst, Acc, AccSt, State, Nxt, 
       Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% error state, so no further matching possible
	    %% - emit current token, reset
	    %% (note that AccSt is forgotten, since we now have a better match)
	    %%
	    case emit_token(Actions, State, Acc) of
		no_token ->
		    longest_string(Start, Table, AcceptLimit, Actions, Lst);
		{token, T} ->
		    [T | longest_string(Start, Table, AcceptLimit, 
					Actions, Lst) ]
	    end;
	N when N =< AcceptLimit ->
	    %% still in accepting state, keep accumulating
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), AccSt, N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    %% leaving accepting state, so save current token
	    NewNxt = transition_table(N, Table),
	    post_accept(Cs, acc(C, Acc), save_match(State, Acc, Lst), 
			N, NewNxt,
			Table, AcceptLimit, Start, Actions)
    end;
accept("", Acc, AccSt, State, Nxt, Table, AcceptLimit, Start, Actions) ->
    %% no more to lex and we are in an accepting state => emit token
    %% and end
    case emit_token(Actions, State, Acc) of
	no_token ->
	    [];
	{token, T} ->
	    [T]
    end.

%% We have been in an accepting state (stored in AccSt) and are
%% now in a non-accepting state.
%%
%% If we enter an error state, or the string ends, emit the saved state 
%% and continue from there. 
%%
%% If we enter an accepting state, jolly good. Go back to accept.
%%
%% Otherwise, keep going.

post_accept([C|Cs]=Lst, Acc, AccSt, State, Nxt, 
	    Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no more matching possible, reset to saved token
	    {State0, Acc0, Cs0} = reset_match(AccSt),
	    case emit_token(Actions, State0, Acc0) of
		no_token ->
		    longest_string(Start, Table, AcceptLimit, Actions, Cs0);
		{token, T} ->
		    [ T | longest_string(Start, Table, AcceptLimit, 
					 Actions, Cs0) ]
	    end;
	N when N =< AcceptLimit ->
	    %% we re-enter an accepting state
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
		   Table, AcceptLimit, Start, Actions);
	N ->
	    %% still in non-accepting state, keep searching
	    NewNxt = transition_table(N, Table),
	    post_accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
			Table, AcceptLimit, Start, Actions)
    end;
post_accept("", Acc, AccSt, State, Nxt, Table, AcceptLimit, Start, Actions) ->
    %% no more matching, reset to saved token and continue from there
    {State0, Acc0, Cs0} = reset_match(AccSt),
    case emit_token(Actions, State0, Acc0) of
	no_token ->
	    longest_string(Start, Table, AcceptLimit, Actions, Cs0);
	{token, T} ->
	    [ T | longest_string(Start, Table, AcceptLimit, Actions, Cs0) ]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_state(Char, Trans_table) ->
    element(Char, Trans_table).

%%

transition_table(State, Table) ->
    element(State, Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Accumulated characters so far

none_acc() ->
    [].

acc(C, Cs) ->
    [C|Cs].

%%

emit_token(Actions, State, Acc) ->
    Action = lookup_action(Actions, State),
    Action(Acc).

%% Save a longest match (so far)

save_match(State, Acc, Cs) ->
    {match, State, Acc, Cs}.

%% Reset to a previously saved longest match

reset_match({match, State, Acc, Cs}) ->
    {State, Acc, Cs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Longest match on sequence (binary() or {binary(), position()})
%%
%% *** UNFINISHED ***
%% - does not permit incremental lexing

%% not used, except for testing:
longest_bin(Lex, Bin) when binary(Bin) ->
    Pos = start_pos(),
    longest_bin(Lex, Bin, Pos).

%% Used when matching invoked from longest/2

longest_bin({lex, Start, AcceptLimit, Table, Actions}, Bin, Pos) ->
    no_lines(),
    longest_bin(Start, Table, AcceptLimit, Actions, Bin, Pos).

%% the following is used when matching restarts from a saved state {Bin, Pos}.

longest_bin(Start, Table, AcceptLimit, Actions, {Bin, Pos}) ->
    longest_bin(Start, Table, AcceptLimit, Actions, Bin, Pos).

%% Here is the "moral entrypoint".

longest_bin(Start, Table, AcceptLimit, Actions, Bin, Pos) ->
    Row = transition_table(Start, Table),
    pre_accept_bin(Bin, Pos, none_acc(), Start, Row,
		   Table, AcceptLimit, Start, Actions).

%% @see pre_accept/8 for most parameters (except Bin, Pos) and for
%% how this is supposed to work.
%%
%% Bin: the binary being lexed
%% Pos: the current position in the binary
%%
%% - if we reach end of sequence, stop
%% - if we reach error state, match fails
%% - if we enter accepting state, switch to accept_bin
%% - otherwise, keep going
%%
%% *** UNFINISHED ***
%% - we accumulate the chars into a list (which works)
%%   but we should also permit just positions to be stored
%%   for extra speed
%% - incremental lexing not handled (curr_char not_found => "partial state")

pre_accept_bin(Bin, Pos, Acc, State, Nxt, 
	       Table, AcceptLimit, Start, Actions) ->
    case curr_char(Bin, Pos) of
	not_found ->
	    %% sequence ended
	    {no_match, lists:reverse(Acc), {Bin, Pos}};
	{found, C} ->
	    case next_state(C, Nxt) of
		?no_match ->
		    {no_match, lists:reverse(Acc), {Bin, Pos}};
		N ->
		    NewNxt = transition_table(N, Table),
		    NxtPos = inc_pos(Pos),
		    if
			N =< AcceptLimit ->
			    %% accepting state
			    accept_bin(Bin, NxtPos, acc(C, Acc), none_acc(),
				       N, NewNxt, Table, AcceptLimit,
				       Start, Actions);
			true ->
			    %% not accepting
			    pre_accept_bin(Bin, NxtPos, acc(C,Acc), N, NewNxt,
					   Table, AcceptLimit, Start, Actions)
		    end
	    end
    end.

%% @see accept/9 for most parameters. Bin is the incoming binary, Pos
%% is the current position in the binary.

accept_bin(Bin, Pos, Acc, AccSt, State, Nxt, 
	   Table, AcceptLimit, Start, Actions) ->
    case curr_char(Bin, Pos) of
	not_found ->
	    %% sequence ended, we have a match
	    case emit_token(Actions, State, Acc) of
		no_token ->
		    [];
		{token, T} ->
		    [T]
	    end;
	{found, C} ->
	    case next_state(C, Nxt) of
		?no_match ->
		    case emit_token(Actions, State, Acc) of
			no_token ->
			    %% discard the acc.token, continue matching
			    longest_bin(Start, Table, AcceptLimit, 
					Actions, Bin, Pos);
			{token, T} ->
			    %% emit the token, continue matching
			    [ T | longest_bin(Start, Table, AcceptLimit,
					      Actions, Bin, Pos) ]
	 	    end;
		N ->
		    NxtPos = inc_pos(Pos),
		    NewNxt = transition_table(N, Table),
		    if
			N =< AcceptLimit ->
			    %% remain in accepting state
			    accept_bin(Bin, NxtPos, acc(C, Acc), AccSt, N,
				       NewNxt, Table, AcceptLimit, 
				       Start, Actions);
			true ->
			    %% leave accepting state, save what has
			    %% matched so far
			    post_accept_bin(Bin, NxtPos, acc(C, Acc),
					    save_match(State, Acc, {Bin, Pos}),
					    N, NewNxt, Table,
					    AcceptLimit, Start, Actions)
		    end
	    end
    end.

%% @see post_accept/9 for most arguments; Bin and Pos is the binary
%% and the position in the binary

post_accept_bin(Bin, Pos, Acc, AccSt, State, Nxt,
		Table, AcceptLimit, Start, Actions) ->
    case curr_char(Bin, Pos) of
	not_found ->
	    %% no match possible, reset to saved token and continue from
	    %% there
	    {State0, Acc0, Seq0} = reset_match(AccSt),
	    case emit_token(Actions, State0, Acc0) of
		no_token ->
		    longest_bin(Start, Table, AcceptLimit, 
				Actions, Seq0);
		{token, T} ->
		    [ T | longest_bin(Start, Table, AcceptLimit, 
				      Actions, Seq0) ]
	    end;
	{found, C} ->
	    case next_state(C, Nxt) of
		?no_match ->
		    %% reset to saved token
		    {State0, Acc0, Seq0} = reset_match(AccSt),
		    case emit_token(Actions, State0, Acc0) of
			no_token ->
			    longest_bin(Start, Table, AcceptLimit, 
					Actions, Seq0);
			{token, T} ->
			    [T | longest_bin(Start, Table, AcceptLimit,
					     Actions, Seq0) ]
		    end;
		N ->
		    NxtPos = inc_pos(Pos),
		    NewNxt = transition_table(N, Table),
		    if
			N =< AcceptLimit ->
			    %% reenter accepting state
			    accept_bin(Bin, NxtPos, acc(C, Acc), AccSt, 
				       N, NewNxt,
				       Table, AcceptLimit, Start, Actions);
			true ->
			    %% still non-accepting
			    post_accept_bin(Bin, NxtPos, acc(C, Acc), AccSt,
					    N, NewNxt, Table, AcceptLimit,
					    Start, Actions)
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The character to be read is indicated by the "current position" in
%% the binary. This position is an abstract value, only to be used with
%% curr_char/2 and inc_pos/1.
%%
%% - see the match expression in curr_char/2 for an explanation why
%%   we really store CurrPos-1 instead of CurrPos

%% This is the abstract "start of sequence" position.
%%
%% if start_pos() is changed, also change ?least_pos above

start_pos() ->
    0.

%% Increment position by one.
%%
%% = should be inlined

inc_pos(Pos) ->
    Pos+1.

%% Select the current character. Note that since binaries are 1-indexed,
%% and we use PrevPos as 0-indexed, we get a relatively simple match 
%% expression (should be compilable into nice code).
%%
%% = should be inlined

curr_char(Bin, PrevPos) ->
    case Bin of
	<<_:PrevPos/binary, C, _/binary>> ->
	    {found, C};
	_ ->
	    not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Action table ADT
%%
%% We map (AccStateID -> Action)

action_table(Regexp_rules, DFA, AcceptLimit) ->
    Acts0 = regexp_actions(Regexp_rules),
    state_actions(AcceptLimit, DFA, Acts0).

regexp_actions(Regexp_rules) ->
    dict:from_list(
      [ {ID, the_action(Action)} 
	|| {regexp, ID, Regexp_str, Action, Prio} <- Regexp_rules ]
     ).

%% for accepting states (1..AcceptLimit), get the corresponding Action
%%
%% returns a tuple -- index into this tuple using St to get the Action

state_actions(AcceptLimit, DFA, Actions) ->
    list_to_tuple(
      [ begin
	    {state, {accepting, R, Prio}, Succ} = state(St, DFA),
	    rule_action(Actions, R)
	end || St <- lists:seq(1, AcceptLimit) ]
     ).

%%

the_action(Fun) when function(Fun) ->
    Fun;
the_action(A) ->
    fun(Acc) ->
	    io:format("rule ~w: ~s~n", [A, lists:reverse(Acc)]),
	    no_token
    end.

%%

rule_action(Actions, RuleID) ->
    case dict:find(RuleID, Actions) of
	{ok, Act} ->
	    Act;
	_ ->
	    exit({action_not_found, RuleID})
    end.

%% This is done while lexing. Note that Actions is a tuple of closures,
%%  { Action(Acc) -> {token, T} | no_token }

lookup_action(Actions, St) ->
    element(St, Actions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Library ADT for keeping track of the number of lines seen.
%%
%%  usage
%%    {regexp, "\n", fun(_) -> lex:inc_lines(), no_token end}
%%    {regexp, "...", fun(Acc) -> lex:count_lines(Acc), no_token end}
%%    {regexp, "<", fun(_) -> {token, {op, lex:current_line(), '<'}} end}
%%
%% The middle rule "..." should be read as a regexp that matches a string
%% w. any number of actual newlines (a quoted atom in Erlang, perhaps). 
%%
%% Prior to starting the lexer, you must reset lex_lines to 0 (or undefined),
%% otherwise you will get the wrong number of lines. Reset is done by
%%   lex:no_lines().
%%
%% (Perhaps the drivers should call implicitly?)

no_lines() ->
    put(lex_lines, 1).

inc_lines() ->
    inc_lines(1).

inc_lines(N) when N >= 0 ->
    K = get(lex_lines),
    put(lex_lines, K+N).

current_line() ->
    get(lex_lines).

count_lines(Str) ->
    count_lines(Str, 0).

count_lines("\n" ++ Cs, Acc) ->
    count_lines(Cs, Acc+1);
count_lines([_|Cs], Acc) ->
    count_lines(Cs, Acc);
count_lines("", Acc) ->
    Acc.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Finite automaton ADT

%%

add_state(Succ, FA) ->
    add_fa_state({state, non_accepting, Succ}, FA).

%%

add_accepting_state(AccRule, Prio, Succ, FA) ->
    add_fa_state({state, {accepting, AccRule, Prio}, Succ}, FA).

%%

add_epsilon(From, To, FA) ->
    Succ = successors(From, FA),
    update_fa_successors(From, Succ ++ [{epsilon, To}], FA).

%%

set_state(ID, State, FA) ->
    set_fa_state(ID, State, FA).

%%

state(ID, FA) ->
    fa_state(ID, FA).

%%

successors(ID, FA) ->
    {state, _, Succs} = state(ID, FA),
    Succs.

%%

accepting_state(ID, FA) ->
    {state, Acc, Succs} = state(ID, FA),
    case Acc of
	non_accepting ->
	    false;
	{accepting, Rule, Prio} ->
	    true
    end.

%%

set_start_state(ID, FA) ->
    set_fa_start_state(ID, FA).

%%

start_state(FA) ->
    fa_start_state(FA).

%%

states_of(FA) ->
    fa_states_of(FA).

%%

num_states(FA) ->
    fa_num_states(FA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% next=1 since least element in tuple is 1
%% use dict to represent dictionaries (or gb_tree?)

-record(fa, 
	{next=1, 
	 start_state, 
	 states=dict:new()
	}
       ).

%%

empty_fa() ->
    #fa{}.

%%

add_fa_state(State, FA) ->
    ID = FA#fa.next,
    {ID, FA#fa{next=ID+1, states=dict:store(ID, State, FA#fa.states)}}.

%% NOTE: the resulting FA does _not_ have a correct "next" field.

set_fa_state(ID, State, FA) ->
    FA#fa{states=dict:store(ID, State, FA#fa.states)}.

%% Update successors of state to new list

update_fa_successors(ID, NewSuccs, FA) ->
    {state, Acc, OldSuccs} = fa_state(ID, FA),
    NewState = {state, Acc, NewSuccs},
    FA#fa{states=dict:store(ID, NewState, FA#fa.states)}.

%% Enumerate the states of the FA
%%
%% (the convoluted code needed because lists:seq/2 is buggy)

fa_states_of(FA) ->
    N = FA#fa.next-1,
    if
	N < 1 ->
	    [];
	true ->
	    lists:seq(1, N)
    end.

fa_num_states(FA) ->
    FA#fa.next-1.

%%

fa_state(ID, FA) ->
    case dict:find(ID, FA#fa.states) of
	{ok, State} ->
	    State;
	Err ->
	    exit({invalid_state, Err, {state, ID}, fa_to_list(FA)})
    end.

%%

set_fa_start_state(ID, FA) ->
    FA#fa{start_state=ID}.

%% NOTE: use only with care

set_next_id(ID, FA) ->    
    FA#fa{next=ID}.

%%

fa_start_state(FA) ->
    FA#fa.start_state.

%% (generate a pseudo-FA for output)

fa_to_list(FA) ->
    FA#fa{next={next, FA#fa.next},
	  start_state={start, FA#fa.start_state},
	  states=dict:to_list(FA#fa.states)}.

%% (a pseudo-FA used as input)

fa_from_list(FA) ->
    {next, Nxt} = FA#fa.next,
    {start, Start} = FA#fa.start_state,
    FA#fa{next=Nxt, start_state=Start, states=dict:from_list(FA#fa.states)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ADT for handling what states have been visited already
%% - using a list is suboptimal ...

none_seen() ->
    [].

seen(X, Xs) ->
    lists:member(X, Xs).

see(X, Xs) ->
    [X|Xs].

seen_list(Xs) ->
    Xs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple set handling (might be unconscionably slow!)

singleton(X) ->
    [X].

union(Xs, Ys) ->
    lists:sort(union0(Xs, Ys)).

union0([X|Xs], Ys) ->
    case lists:member(X, Ys) of
	true ->
	    union0(Xs, Ys);
	false ->
	    [X|union0(Xs,Ys)]
    end;
union0([], Ys) ->
    Ys.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** UNFINISHED ***
%% - which is start state?
%% - which are accepting states?
%%   * low numbering of acc.states?

empty_pre_dfa() ->
    { 1, dict:new(), dict:new() }.

name_dfa_state(StateSet, {Next, State_names, DFA_states}) ->
    {Next, {Next+1, dict:store(StateSet, Next, State_names), DFA_states}}.

states_present(StateSet, {Next, State_names, DFA_states}) ->
    case dict:find(StateSet, State_names) of
	{ok, Name} ->
	    {found, Name};
	_ ->
	    not_found
    end.

set_dfa_state(ID, State, {Next, State_names, DFA_states}) ->
    {Next, State_names, dict:store(ID, State, DFA_states)}.

pre_dfa_to_list({Next, State_names, DFA_states}) ->
    {Next, dict:to_list(State_names), dict:to_list(DFA_states)}.

pre_dfa_state_names_list(Pre_DFA) ->
    dict:to_list(pre_dfa_state_names(Pre_DFA)).

pre_dfa_state_names({Next, State_names, DFA_states}) ->
    State_names.

pre_dfa_to_dfa(Start, {Next, State_names, DFA_states}=Pre_DFA) ->
    #fa{next=Next,
	start_state=Start,
	states=DFA_states}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Library stuff

zip([X|Xs], [Y|Ys]) ->
    [ {X,Y} | zip(Xs, Ys) ];
zip([], []) ->
    [].

%% perhaps also record # of warnings?

warning(Str, Xs) ->
    io:format("Warning: " ++ Str, Xs).

log(Str, Xs) ->
    io:format(Str, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Testing stuff beyond this

test_exp(1) ->
    "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?";
test_exp(2) ->
    "[a-z][0-9A-Za-z_]*";
test_exp(3) ->
    "[A-Z][0-9A-Za-z_]*";
test_exp(N) ->
    exit({no_such_test, N}).

test_all(N) ->
    [ {regexp, test_exp(R), {rule, R}} || R <- lists:seq(1,N) ].

test(Regexp) ->
    Regexp_rules = [ {regexp, parse_regexp(Regexp), {rule, 1}} ],
    regexps_to_table(Regexp_rules).

insert(Item, [X|Xs]) ->
    [X|insert_before(Item, Xs)].

insert_before(Item, [X|Xs]) ->
    [Item,X|insert_before(Item,Xs)];
insert_before(Item, []) ->
    [].

test(1, String) ->
    Lex = lex:regexps_to_table(
	    [{regexp, "[a-z][A-Za-z0-9_]*", atom}, 
	     {regexp, "[A-Z][A-Za-z0-9_]*", var}, 
	     {regexp, "[1-9][0-9]*", int}, 
	     {regexp, "[\n\t\ ]*", fun(_) -> no_token end}]
	   ),
    longest(Lex, String).

%% this is a reasonable lexing grammar, somewhat sparse with  reserved words
%%
%% the lexer seems to handle it quite well. Note: quoted atoms are sort of
%% handled, but I haven't really thought through what policy should be used for
%% quoted characters
%%
%% (currently: 112 NFA states, 36 DFA states with 28 accepting states)

test_lexer() ->
    [{regexp, ",", comma},
     {regexp, "\\(", lpar},     
     {regexp, "\\)", rpar},     
     {regexp, "\\[", lbrack},
     {regexp, "\\]", lbrack},
     {regexp, "&&", op_andand},
     {regexp, "&", op_and},
     {regexp, "\\|\\|", op_oror},
     {regexp, "\\|", op_or},
     {regexp, "\\+", plus},
     {regexp, "\\+\\+", plusplus},
     {regexp, "-", minus},
     {regexp, "--", minusminus},
     {regexp, "\\*", times},
     {regexp, "/", divide},
     {regexp, "[A-Z][A-Za-z0-9_]*", var},
     {regexp, "[a-z][A-Za-z0-9_]*", atom}, 
     {regexp, "'([^\n']|\\')+'", atom},     %% quoted atom
     {regexp, "(-|\\+)?[1-9][0-9]*", int},
     {regexp, "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?", float},
     {regexp, "[\ \n\t]*", fun(_) -> no_token end}
    ].

%% this one inspired by the terminals of erl_parse.yrl
%%
%% - conflicts between atoms and reserved words (currently, reserved
%%   words by some quirk win; we should be able to handle that explicitly)
%% - NFA has 296 states, becomes a DFA with 142 states (129 accepting)
%%
%% Size is 142*255 + 129 words (+ closures, and some extra stuff)
%% or 36339 words (<146 KB) for this lexer table. Not too awful.

erlang_lex() ->
    [{regexp, "\\$.", char},
     {regexp, "[A-Z][A-Za-z0-9_]*", var},
     {regexp, "[a-z][A-Za-z0-9_]*", atom}, 
     {regexp, "'([^\n']|\\')+'", atom},     %% quoted atom
     {regexp, "(-|\\+)?[1-9][0-9]*", int},
     {regexp, "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?", float},

     {regexp, "\"[^\"\n]\"", string},
     {regexp, "\\(", '(' },
     {regexp, "\\)", ')' },
     {regexp, "\\[", '[' },
     {regexp, "\\]", ']' },
     {regexp, "\\{", '{' },
     {regexp, "\\}", '}' },
     {regexp, ",", ',' },
     {regexp, "->", '->' },
     {regexp, ":-", ':-' },
     {regexp, "\\|", '|' },
     {regexp, "\\|\\|", '||' },
     {regexp, ";", ';' },
     {regexp, ":", ':' },
     {regexp, "#", '#' },
     {regexp, "\\.", '.' },

     {regexp, "after", 'after' },
     {regexp, "begin", 'begin' },
     {regexp, "case", 'case' },
     {regexp, "try", 'try' },
     {regexp, "catch", 'catch' },
     {regexp, "end", 'end' },
     {regexp, "fun", 'fun' },
     {regexp, "if", 'if' },
     {regexp, "of", 'of' },
     {regexp, "receive", 'receive' },
     {regexp, "when", 'when' },
     {regexp, "andalso", 'andalso' },
     {regexp, "orelse", 'orelse' },
     {regexp, "query", 'query' },
     {regexp, "bnot", 'bnot' },
     {regexp, "not", 'not' },

     {regexp, "\\*", '*' },
     {regexp, "/", '/' },
     {regexp, "div", 'div' },
     {regexp, "rem", 'rem' },
     {regexp, "band", 'band' },
     {regexp, "and", 'and' },

     {regexp, "\\+", '+' },
     {regexp, "-", '-' },
     {regexp, "bor", 'bor' },
     {regexp, "bxor", 'bxor' },
     {regexp, "bsl", 'bsl' },
     {regexp, "bsr", 'bsr' },
     {regexp, "or", 'or' },
     {regexp, "xor", 'xor' },

     {regexp, "\\+\\+", '++' },
     {regexp, "--", '--' },

     {regexp, "==", '==' },
     {regexp, "/=", '/=' },
     {regexp, "=<", '=<' },
     {regexp, "<", '<' },
     {regexp, ">=", '>=' },
     {regexp, ">", '>' },
     {regexp, "=:=", '=:=' },
     {regexp, "=/=", '=/=' },

     {regexp, "<<", '<<' },
     {regexp, ">>", '>>' },

     {regexp, "!", '!' },
     {regexp, "=", '=' },

     {regexp, "%[^\n]*", fun(_) -> no_token end},
     {regexp, "[\ \n\t]*", fun(_) -> no_token end}
     ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% A more realistic erlang lexer, which returns "actual tokens". Some
%% tokens are just returned as atoms, while others are wrapped into
%% 'items'/tuples.
%%
%% Note: not quite compatible with the standard erlang lexer:
%% - emits a lot of non-operators as operators (easily fixed)
%% - I couldn't find a good way for quoted atoms to contain newlines
%%   (how to specify? "'[^']*'" ?)
%% - strings currently cannot contain " or \n, which is a weakness
%%   (also, should we convert control chars into character codes?)
%%   (how to respecify?)
%% - quoted characters inside quoted atoms and strings not handled
%%   e.g., \', \"
%%
%% All of these should be fixable with more involved regexps.

real_erlang_lex() ->
    [{regexp, "\\$.", char()},
     {regexp, 0, "[A-Z_][A-Za-z0-9_]*", item(var)},
     {regexp, 0, "[a-z][A-Za-z0-9_]*", item(atom)},
     {regexp, 0, "\\?[A-Za-z][A-Za-z0-9_]*", item(macro)},
     {regexp, 0, "'(\\\'|[^'\n])*'", quoted_atom()},
     {regexp, 0, "(-|\\+)?[0-9][0-9]*", item(integer)},
     {regexp, 0, 
      "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?",item(float)},

     {regexp, 0, "\"(\\\"|[^\"\n])*\"", string()},
     %% {regexp, 0, "\"[^\"\n]*\"", item(string)},
     {regexp, "\\(", token('(') },
     {regexp, "\\)", token(')') },
     {regexp, "\\[", token('[') },
     {regexp, "\\]", token(']') },
     {regexp, "\\{", token('{') },
     {regexp, "\\}", token('}') },
     {regexp, ",", token(',') },
     {regexp, "->", token('->') },
     {regexp, ":-", token(':-') },
     {regexp, "\\|", token('|') },
     {regexp, "\\|\\|", token('||') },
     {regexp, ";", token(';') },
     {regexp, ":", token(':') },
     {regexp, "#", token('#') },
     {regexp, "\\.", token('.') },

     {regexp, 1, "after", token('after') },
     {regexp, 1, "begin", token('begin') },
     {regexp, 1, "case", token('case') },
     {regexp, 1, "try", token('try') },
     {regexp, 1, "catch", token('catch') },
     {regexp, 1, "end", token('end') },
     {regexp, 1, "fun", token('fun') },
     {regexp, 1, "if", token('if') },    % rule 30
     {regexp, 1, "of", token('of') },
     {regexp, 1, "receive", token('receive') },
     {regexp, 1, "when", token('when') },
     {regexp, 1, "andalso", token('andalso') },
     {regexp, 1, "orelse", token('orelse') },
     {regexp, 1, "query", token('query') },
     {regexp, 1, "bnot", token('bnot') },
     {regexp, 1, "not", token('not') },

     {regexp, 1, "\\*", op('*') },
     {regexp, 1, "/", op('/') },
     {regexp, 1, "div", op('div') },
     {regexp, 1, "rem", op('rem') },
     {regexp, 1, "band", op('band') },
     {regexp, 1, "and", op('and') },

     {regexp, 1, "\\+", op('+') },
     {regexp, 1, "-", op('-') },
     {regexp, 1, "bor", op('bor') },
     {regexp, 1, "bxor", op('bxor') },
     {regexp, 1, "bsl", op('bsl') },
     {regexp, 1, "bsr", op('bsr') }, % rule 50
     {regexp, 1, "or", op('or') },
     {regexp, 1, "xor", op('xor') },

     {regexp, 1, "\\+\\+", op('++') },
     {regexp, 1, "--", op('--') },

     {regexp, 1, "==", op('==') },
     {regexp, 1, "/=", op('/=') },
     {regexp, 1, "=<", op('=<') },
     {regexp, 1, "<", op('<') },
     {regexp, 1, ">=", op('>=') },
     {regexp, 1, ">", op('>') },
     {regexp, 1, "=:=", op('=:=') },
     {regexp, 1, "=/=", op('=/=') },

     {regexp, 1, "<<", op('<<') },
     {regexp, 1, ">>", op('>>') },

     {regexp, 1, "!", op('!') },
     {regexp, 1, "=", op('=') },

     {regexp, 1, "%[^\n]*", comment()},
     {regexp, 2, "\n", fun(_) -> %% io:format("inc lines~n"),
				 inc_lines(), 
				 no_token 
		       end},
     {regexp, 1, "[\ \t]*", whitespace()}
     ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These functions emit action functions for most tokens (better than
%% writing them explicitly in the spec above)

item(Key) ->
    fun(Acc) ->
	    {token, {Key, lex:current_line(), lists:reverse(Acc)}}
    end.

%%

op(Key) ->
    fun(Acc) ->
	    {token, {op, lex:current_line(), Key}}
    end.

%% Note: with current definition, no newlines inside atom

quoted_atom() ->
    fun([$'|Acc]) ->
	    [$'|Atm] = lists:reverse(Acc),
	    Line = lex:current_line(),
	    N = lex:count_lines(Atm),
	    lex:inc_lines(N),
	    {token, {atom, Line, Atm}}
    end.

%% Note: with current definition, no newlines inside string

string() ->
    fun([$"|Acc]) ->
	    [$"|Str] = lists:reverse(Acc),
	    Line = lex:current_line(),
	    N = lex:count_lines(Str),
	    lex:inc_lines(N),
	    {token, {string, Line, Str}}
    end.

%% recall that accumulator is reversed when passed to the action

char() ->
    fun([C,$$]) ->
	    {token, {char, lex:current_line(), C}}
    end.

%% perhaps {token, {Key, current_line()}}?
	
token(Key) ->    
    fun(Acc) ->
	    {token, Key}
    end.

%%

no_token() ->
    fun(_) ->
	    no_token
    end.

%%

comment() ->
    fun(Acc) ->
	    %% io:format("comment '~s'~n", [lists:reverse(Acc)]),
	    no_token
    end.

%%

whitespace() ->
    fun(Acc) ->
	    %% io:format("whitespace '~s'~n", [lists:reverse(Acc)]),
	    no_token
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here is a rule for floats that also handles various IEEE special numbers

float_rules() ->
    [{regexp, 0, 
      "((\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?|NAN|NaN|(\\+|-)?(INF|Inf|inf))",item(float)}
    ].

