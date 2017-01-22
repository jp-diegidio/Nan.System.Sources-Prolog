%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	Nan.System.Sources
	Nan.System.Sources/Prolog 1.2.0-beta
	Answer Sources in Prolog
	Copyright 2015-2017 Julio P. Di Egidio
	<mailto:julio@diegidio.name>
	<http://julio.diegidio.name/Projects/Nan.System.Sources/>
	
	This file is part of Nan.System.Sources.
	
	Nan.System.Sources is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	Nan.System.Sources is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with Nan.System.Sources.  If not, see <http://www.gnu.org/licenses/>.
*/

% (SWI-Prolog 7.3.25)
%	Main SWI-Prolog specifics:
%	- Threads
%	- Thread message queues
%	- Global cuts

:- module(sources,
	[	using_source/4,         % @TSol, @GExe, -Src, :GUse
		using_source_com/4,     % +Srcs, :GCom, -Src, :GUse
		source_create/3,        % @TSol, @GExe, -Src
		source_create_com/5,    % +Srcs, @GRst, @GNxt, @Sta0, -Src
		source_destroy/1,       % +Src
		source_destroy_all/0,   % 
		source_reset/1,         % +Src
		source_next/2,          % +Src, ?Ans
		source_next_begin/1,    % +Src
		source_next_end/2,      % +Src, ?Ans
		source_exists/1         % +Src
	]).

:- use_module(sources_types).
:- use_module(sources_system).

/** <module> Answer Sources

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Module =sources= (nan/system/sources.pl) provides
the predicates that implement Answer Sources in Prolog.

For code docs syntax and meaning see sources_docs.txt.

*NOTE*:
  - Predicates in this module are not synchronised.
  - Predicates in this module do not validate their input.

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3

@tbd  Extend from fluents to interactors (implement yield/1).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(apply)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PUBLIC interface: using
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
using_source(@TSol:any, @GExe:callable, -Src:source, :GUse:callable) is nondet.

Creates a source, calls a goal, finally destroys the source.

Ensures destruction of the source even if goal throws an error.

  - GExe invoked as =|GExe() is nondet|=.
  - GUse invoked as =|GUse() is nondet|=.

Example:
==
?- using_source(I, between(1, 2, I), _S,
   (   repeat,
       source_next(_S, answer(_Det, the(I))),
       (_Det == last -> !; true)
   )).
I = 1 ;
I = 2.
==

@arg  TSol  The source's solution template (copied).
@arg  GExe  The source's goal to execute (copied).
@arg  Src   The new source.
@arg  GUse  The goal to execute.
*/

:- meta_predicate
	using_source(+, 0, -, 0).

using_source(TSol, GExe, Src, GUse) :-
	setup_call_cleanup(
		source_create(TSol, GExe, Src),
		call(GUse),
		source_destroy(Src)
	).

/**
using_source_com(+Srcs:list(source), :GCom:callable, -Src:source, :GUse:callable) is nondet.

Creates a combined source, calls a goal, finally destroys the source.

Ensures destruction of the source even if goal throws an error.

  - GCom invoked as =|GCom(Srcs, Src) is det|=.
  - GUse invoked as =|GUse() is nondet|=.

Example:
==
?- [user].
parallel_com(Srcs, Src) :-
    GRst = parallel_com__rst,
    GNxt = parallel_com__nxt,
    source_create_com(Srcs, GRst, GNxt, _, Src).

parallel_com__rst(Srcs, _, _) :-
    maplist(source_reset, Srcs).

parallel_com__nxt(Srcs, _, _, Ans) :-
    maplist(source_next_begin, Srcs),
    foldl(parallel_com__nxt__do, Srcs, [], As),
    Ans = answer(more, the(As)).

parallel_com__nxt__do(Src, As0, As) :-
    source_next_end(Src, A),
    append(As0, [A], As).
^Z

?- using_source(1, sleep(1), _S1,
   using_source(2, sleep(1), _S2,
   using_source_com([_S1, _S2], parallel_com, _S,
   (   time(source_next(_S, answer(more, the(Anss))))
   )))).  % Warm run
% 188 inferences, 0.000 CPU in 1.000 seconds (0% CPU, Infinite Lips)
Anss = [answer(last, the(1)), answer(last, the(2))].
==

@arg  Srcs  The sources to combine.
@arg  GCom  The combination constructor.
@arg  Src   The new combined source.
@arg  GUse  The goal to execute.
*/

:- meta_predicate
	using_source_com(+, 2, -, 0).

using_source_com(Srcs, GCom, Src, GUse) :-
	setup_call_cleanup(
		call(GCom, Srcs, Src),
		call(GUse),
		source_destroy(Src)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PUBLIC interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	source_create(@TSol:any, @GExe:callable, -Src:source) is det.
%
%	Creates a source.
%	
%	  - GExe invoked as =|GExe() is nondet|=.
%	
%	@arg  TSol  The solution template (copied).
%	@arg  GExe  The goal to execute (copied).
%	@arg  Src   The new source.

:- meta_predicate
	source_create(+, 0, -).

source_create(TSol, GExe, Src) :-
	source_create_(TSol, GExe, Sid),
	source_typ_sid(Src, t0, Sid).

%!	source_create_com(+Srcs:list(source), @GRst:callable, @GNxt:callable, @Sta0:any, -Src:source) is det.
%
%	Creates a combined source.
%	
%	  - GRst invoked as =|GRst(Srcs, Sta0, Sta1) is det|=.
%	  - GNxt invoked as =|GNxt(Srcs, Sta0, Sta1, Ans) is det|=.
%	
%	Enforces determinism on GRst and GNxt.
%	
%	For an example, see using_source_com/4.
%	
%	@arg  Srcs  The sources to combine.
%	@arg  GRst  The goal to reset the combinator to its initial state (copied).
%	@arg  GNxt  The goal to get the next answer from the combinator (copied).
%	@arg  Sta0  The initial state of the combinator (copied).
%	@arg  Src   The new combined source.

:- meta_predicate
	source_create_com(+, 3, 4, +, -).

source_create_com(Srcs, GRst, GNxt, Sta0, Src) :-
	source_create_com_(Srcs, GRst, GNxt, Sta0, Sid),
	source_typ_sid(Src, t1, Sid).

%!	source_destroy(+Src:source) is det.
%
%	Destroys a source.
%	
%	@arg  Src  The source.
%	
%	@throws  source_error(does_not_exist(Src))

source_destroy(Src) :-
	source_typ_sid(Src, Typ, Sid),
	source_assert_(Src, Sid),
	source_destroy_(Typ, Sid).

%!	source_destroy_all is semidet.
%
%	Destroys all existing sources.
%	
%	Fails if no source exists.

source_destroy_all :-
	source_destroy_all_.

%!	source_reset(+Src:source) is det.
%
%	Resets a source to its initial state.
%	
%	@arg  Src  The source.
%	
%	@throws  source_error(does_not_exist(Src))

source_reset(Src) :-
	source_typ_sid(Src, Typ, Sid),
	source_assert_(Src, Sid),
	source_reset_(Typ, Sid).

%!	source_next(+Src:source, ?Ans:answer) is det.
%
%	Gets the next answer from a source.
%	
%	@arg  Src  The source.
%	@arg  Ans  The answer.
%	
%	@throws  source_error(does_not_exist(Src))

source_next(Src, Ans) :-
	source_typ_sid(Src, Typ, Sid),
	source_assert_(Src, Sid),
	source_next_(Typ, Sid, Ans).

%!	source_next_begin(+Src:source) is det.
%
%	Begins getting the next answer from a source (async).
%	
%	@arg  Src  The source.
%	
%	@throws  source_error(does_not_exist(Src))

source_next_begin(Src) :-
	source_typ_sid(Src, Typ, Sid),
	source_assert_(Src, Sid),
	source_next_b_(Typ, Sid).

%!	source_next_end(+Src:source, ?Ans:answer) is det.
%
%	Ends getting the next answer from a source (async).
%	
%	@arg  Src  The source.
%	@arg  Ans  The answer.
%	
%	@throws  source_error(does_not_exist(Src))

source_next_end(Src, Ans) :-
	source_typ_sid(Src, Typ, Sid),
	source_assert_(Src, Sid),
	source_next_e_(Typ, Sid, Ans).

%!	source_exists(+Src:source) is semidet.
%
%	Tests that a source exists.
%	
%	@arg  Src  The source to test for.

source_exists(Src) :-
	source_typ_sid(Src, _, Sid),
	source_db_exists(Sid).

source_assert_(_, Sid) :-
	source_db_exists(Sid), !.
source_assert_(Src, _) :-
	source_throw(does_not_exist(Src)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PRIVATE implementation: create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	source_create_com_ (+Srcs, @GRst, @GNxt, @Sta0, -Sid) is det

:- meta_predicate
	source_create_com_(+, 3, 4, +, -).

source_create_com_(Srcs, GRst, GNxt, Sta0, Sid) :-
	source_new_sid(t1, Sid),
	source_call_(Sid, 'CREATE_1',
	(	copy_term([GRst, GNxt, Sta0], [GRst1, GNxt1, Sta01]),
		source_db_add(Sid, 'CREATE_1', t1(Srcs, GRst1, GNxt1, Sta01))
	)).

%	source_create_ (@TSol, @GExe, -Sid) is det

:- meta_predicate
	source_create_(+, 0, -).

source_create_(TSol, GExe, Sid) :-
	source_new_sid(t0, Sid),
	source_call_(Sid, 'CREATE_0',
	(	source_create__do(Sid, TSol, GExe, Ress)
	)), source_rethrow(Ress).

:- meta_predicate
	source_create__do(+, +, 0, -).

source_create__do(Sid, TSol, GExe, Ress) :-
	source_create__pre(Sid, TSol, GExe, [Pid, Tid, GEXEC]),
	source_create__all(Sid, Pid, Tid, GEXEC, Res1),
	(	source_has_err(Res1)
	->	source_create__abort(Sid, Res2),
		Ress = [Res1, Res2]
	;	Ress = [Res1]
	).

:- meta_predicate
	source_create__pre(+, +, 0, -).

source_create__pre(Sid, TSol, GExe, [Pid, Tid, GEXEC]) :-
	copy_term([TSol, GExe], [TSol1, GExe1]),
	atom_concat(Sid, '_p', Pid),
	atom_concat(Sid, '', Tid),
	GEXEC = source_exec_(Sid, TSol1, GExe1).

:- meta_predicate
	source_create__all(+, +, +, 0, -).

source_create__all(Sid, Pid, Tid, GEXEC, Res) :-
	source_catch(
	(	source_db_add(Sid, 'CREATE_0', t0(Pid, Tid)),
		message_queue_create(_, [alias(Pid)]),
		thread_create(GEXEC, _, [alias(Tid)])
	), Res).

source_create__abort(Sid, Res) :-
	source_catch(
	(	source_destroy_(t0, Sid)
	), Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PRIVATE implementation: destroy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	source_destroy_all_ () is semidet

source_destroy_all_ :-
	bagof(Sid, Term^source_db_enum(Sid, Term), Sids),
	foldl(source_destroy_all__do, Sids, [], Ress),
	source_rethrow(Ress).

source_destroy_all__do(Sid, Ress0, Ress) :-
	source_catch(
	(	source_typ_sid(_, Typ, Sid),
		source_destroy_(Typ, Sid)
	), Res), append(Ress0, [Res], Ress).

%	source_destroy_ (+Typ, +Sid) is det

source_destroy_(t1, Sid) :-
	source_call_(Sid, 'DESTROY_1',
	(	source_db_del(Sid, 'DESTROY_1')
	)).

source_destroy_(t0, Sid) :-
	source_db_get(Sid, 'DESTROY_0', t0(Pid, Tid)),
	source_call_(Sid, 'DESTROY_0',
	(	source_destroy__do(Sid, Pid, Tid, Ress)
	)), source_rethrow(Ress).

source_destroy__do(Sid, Pid, Tid, Ress) :-
	source_destroy__db(Sid, Res1),
	source_destroy__queue(Pid, Res2),
	source_destroy__thread(Sid, Tid, Res3),
	Ress = [Res1, Res2, Res3].

source_destroy__db(Sid, Res) :-
	source_catch(
	(	source_db_del(Sid, 'DESTROY_0')
	), Res).

source_destroy__queue(Qid, Res) :-
	source_catch(
	(	message_queue_destroy(Qid)
	), Res).

source_destroy__thread(Sid, Tid, Res) :-
	source_catch(
	(	(	thread_property(Tid, status(running))
		->	source_msg_send_(Sid, 'DESTROY_0', Tid, close)
		;	true
		), thread_join(Tid, _)
	), Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PRIVATE implementation: call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	source_reset_ (+Typ, +Sid) is det

source_reset_(t1, Sid) :-
	source_db_get(Sid, 'RESET_1', t1(Srcs, GRst, _, Sta0)),
	source_call_(Sid, 'RESET_1',
	(	source_reset__t1(Srcs, GRst, Sta0, Sta1),
		source_next__t1_state(Sid, 'RESET_1', Sta1)
	)).

source_reset_(t0, Sid) :-
	source_db_get(Sid, 'RESET_0', t0(_, Tid)),
	source_call_(Sid, 'RESET_0',
	(	source_reset__t0(Sid, 'RESET_0', Tid)
	)).

%	source_next_ (+Typ, +Sid, ?Ans) is semidet

source_next_(t1, Sid, Ans) :-
	source_db_get(Sid, 'NEXT_1', t1(Srcs, _, GNxt, Sta0)),
	source_call_(Sid, 'NEXT_1',
	(	source_next__t1(Srcs, GNxt, Sta0, Sta1, Ans),
		source_next__t1_state(Sid, 'NEXT_1', Sta1)
	)).

source_next_(t0, Sid, Ans) :-
	source_db_get(Sid, 'NEXT_0', t0(Pid, Tid)),
	source_call_(Sid, 'NEXT_0',
	(	source_next__t0_b(Sid, 'NEXT_0', Tid),
		source_next__t0_e(Sid, 'NEXT_0', Pid, Ans)
	)).

%	source_next_b_ (+Typ, +Sid) is det

source_next_b_(t1, Sid) :-
	source_db_get(Sid, 'NEXT_1_B', _),
	source_call_(Sid, 'NEXT_1_B',
	(	true
	)).

source_next_b_(t0, Sid) :-
	source_db_get(Sid, 'NEXT_0_B', t0(_, Tid)),
	source_call_(Sid, 'NEXT_0_B',
	(	source_next__t0_b(Sid, 'NEXT_0_B', Tid)
	)).

%	source_next_e_ (+Typ, +Sid, ?Ans) is semidet

source_next_e_(t1, Sid, Ans) :-
	source_db_get(Sid, 'NEXT_1_E', t1(Srcs, _, GNxt, Sta0)),
	source_call_(Sid, 'NEXT_1_E',
	(	source_next__t1(Srcs, GNxt, Sta0, Sta1, Ans),
		source_next__t1_state(Sid, 'NEXT_1_E', Sta1)
	)).

source_next_e_(t0, Sid, Ans) :-
	source_db_get(Sid, 'NEXT_0_E', t0(Pid, _)),
	source_call_(Sid, 'NEXT_0_E',
	(	source_next__t0_e(Sid, 'NEXT_0_E', Pid, Ans)
	)).

:- meta_predicate
	source_reset__t1(+, 3, +, -),
	source_next__t1(+, 4, +, -, ?).

source_reset__t1(Srcs, GRst, Sta0, Sta1) :-
	call(GRst, Srcs, Sta0, Sta1), !.					% NOTE: Enforce determinism!

source_next__t1(Srcs, GNxt, Sta0, Sta1, Ans) :-
	call(GNxt, Srcs, Sta0, Sta1, Ans_), !, Ans = Ans_.	% NOTE: Enforce determinism!

source_next__t1_state(Sid, Cat, Sta1) :-
	source_db_get(Sid, Cat, t1(Srcs, GRst, GNxt, Sta0)),
	(	Sta1 \== Sta0
	->	source_db_del(Sid, Cat),
		source_db_add(Sid, Cat, t1(Srcs, GRst, GNxt, Sta1))
	;	true
	).

source_reset__t0(Sid, Cat, Tid) :-
	source_msg_send_(Sid, Cat, Tid, reset).

source_next__t0_b(Sid, Cat, Tid) :-
	source_msg_send_(Sid, Cat, Tid, next).

source_next__t0_e(Sid, Cat, Pid, Ans) :-
	source_msg_recv_(Sid, Cat, Pid, Msg),
	(	\+ callable(Msg)  ->
			source_throw(invalid_message(Sid, Cat, Msg))
	;	Msg = fail        -> Ans = answer(fail, no)
	;	Msg = last(Sol)  -> Ans = answer(last, the(Sol))
	;	Msg = more(Sol)  -> Ans = answer(more, the(Sol))
	;	Msg = except(Err) ->
			source_throw(error_message(Sid, Cat, Err))
	;	source_throw(unknown_message(Sid, Cat, Msg))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PRIVATE implementation: exec
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	source_exec_ (+Sid, ?TSol, :GExe) is det

:- meta_predicate
	source_exec_(+, ?, 0).

source_exec_(Sid, TSol, GExe) :-
	source_db_get(Sid, 'EXEC', t0(Pid, Tid)),
	source_call_(Sid, 'EXEC',
	(	call_cleanup(
			source_exec__loop_0(Sid, Pid, Tid, TSol, GExe),
			exception(Err),
			source_msg_send_(Sid, 'EXEC', Pid, except(Err))
		)
	)).

:- meta_predicate
	source_exec__loop_0(+, +, +, ?, 0),
	source_exec__loop_1(+, +, +, ?, 0),
	source_exec__loop_2(+, +, ?, 0).

source_exec__loop_0(Sid, Pid, Tid, TSol, GExe) :-
	repeat,
	source_msg_recv_(Sid, 'EXEC', Tid, Msg),
	(	\+ callable(Msg) ->
			source_throw(invalid_message(Sid, 'EXEC', Msg))
	;	Msg = reset      -> fail
	;	Msg = close      -> !
	;	Msg = next       -> !,
			source_exec__loop_1(Sid, Pid, Tid, TSol, GExe)
	;	source_throw(unknown_message(Sid, 'EXEC', Msg))
	).

source_exec__loop_1(Sid, Pid, Tid, TSol, GExe) :-
	prolog_current_choice(Loop0),
	repeat,
	prolog_current_choice(Loop1),
	source_exec__loop_2(Sid, Pid, TSol, GExe),
	source_exec__recv(Sid, Tid, Loop0, Loop1).

source_exec__loop_2(Sid, Pid, TSol, GExe) :-
	(	call_cleanup(GExe, Det = true),
		(	Det == true
		->	source_msg_send_(Sid, 'EXEC', Pid, last(TSol))
		;	source_msg_send_(Sid, 'EXEC', Pid, more(TSol))
		)
	;	repeat,
		source_msg_send_(Sid, 'EXEC', Pid, fail)
	).

source_exec__recv(Sid, Tid, Loop0, Loop1) :-
	source_msg_recv_(Sid, 'EXEC', Tid, Msg),
	(	\+ callable(Msg) ->
			source_throw(invalid_message(Sid, 'EXEC', Msg))
	;	Msg = next       -> fail
	;	Msg = close      -> prolog_cut_to(Loop0)
	;	Msg = reset      -> prolog_cut_to(Loop1),
			source_exec__recv(Sid, Tid, Loop0, Loop1)
	;	source_throw(unknown_message(Sid, 'EXEC', Msg))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PRIVATE helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	source_call_ (+Sid, +Cat, :GAct) is det

:- meta_predicate
	source_call_(+, +, 0).

source_call_(Sid, Cat, GAct) :-
	setup_call_cleanup(
		source_log_call(Sid, Cat, start),
		call(GAct),
		source_log_call(Sid, Cat, end)
	).

%	source_msg_send_ (+Sid, +Cat, +Qid, +Msg) is det
%	source_msg_recv_ (+Sid, +Cat, +Qid, -Msg) is det

source_msg_send_(Sid, Cat, Qid, Msg) :-
	thread_send_message(Qid, Msg),
	source_log_msg(Sid, Cat, send, Msg).

source_msg_recv_(Sid, Cat, Qid, Msg) :-
	thread_get_message(Qid, Msg),
	source_log_msg(Sid, Cat, recv, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
