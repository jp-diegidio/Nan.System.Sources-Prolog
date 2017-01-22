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

:- module(sources_system_test, []).

/** <module> Answer Sources :: Sources system tests

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Tests for module =sources_system= (nan/system/sources_system.pl).

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization
	use_module(loader),
	module_path('sources_system.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_typ_sid').

t__source_typ_sid__t(t0, id, sources__t0_id).
t__source_typ_sid__t(t1, id, sources__t1_id).

test(source_typ_sid__t_pm,
[	forall(t__source_typ_sid__t(T0, I0, S0)),
	true((T, S) == (T0, S0))
]) :-
	source_typ_sid(source(T0, I0), T, S).

test(source_typ_sid__t_mp,
[	forall(t__source_typ_sid__t(T0, I0, S0)),
	true((Src, T) == (source(T0, I0), T0))
]) :-
	source_typ_sid(Src, T, S0).

:- end_tests('sources_system:source_typ_sid').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_new_sid',
[	setup(flag(sources__id_max, _, 0))
]).

t__source_new_sid__t(t0, sources__t0_1).
t__source_new_sid__t(t1, sources__t1_2).
t__source_new_sid__t(t0, sources__t0_3).
t__source_new_sid__t(t1, sources__t1_4).
t__source_new_sid__t(t1, sources__t1_5).

test(source_new_sid__t,
[	forall(t__source_new_sid__t(T0, S0)),
	true(Sid == S0)
]) :-
	source_new_sid(T0, Sid).

:- end_tests('sources_system:source_new_sid').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_db',
[	setup(forall(recorded(sources__db, sid-term, Ref), erase(Ref)))
]).

t__source_db__t(Sid, Cat, Term, Err_v, Err_n) :-
	Sid = sid,
	Cat = 'TEST',
	Term = term,
	Err_v = source_error(db_does_not_exist(Sid, Cat), _),
	Err_n = source_error(db_exists_already(Sid, Cat), _).

t__source_db__t_e(Sid, Cat, Err1, Err2) :-
	\+ source_db_exists(Sid),
	\+ source_db_enum(Sid, _),
	catch(source_db_get(Sid, Cat, _), Err1, true),
	catch(source_db_del(Sid, Cat), Err2, true).

t__source_db__t_n(Sid, Cat, T1s, T2, Err1) :-
	source_db_exists(Sid),
	findall(T1, source_db_enum(Sid, T1), T1s),
	source_db_get(Sid, Cat, T2),
	catch(source_db_add(Sid, Cat, _), Err1, true).

test(source_db__t_1,
[	setup((t__source_db__t(Sid, Cat, _, Ev1, _), copy_term(Ev1, Ev2))),
	true((Err1, Err2) =@= (Ev1, Ev2))
]) :-
	%% 1: empty
	t__source_db__t_e(Sid, Cat, Err1, Err2).

test(source_db__t_2,
[	setup(t__source_db__t(Sid, Cat, Term, _, En1)),
	true((T1s, T2, Err1) =@= ([Term], Term, En1))
]) :-
	%% 2: add
	source_db_add(Sid, Cat, Term),
	t__source_db__t_n(Sid, Cat, T1s, T2, Err1).

test(source_db__t_3,
[	setup((t__source_db__t(Sid, Cat, _, Ev1, _), copy_term(Ev1, Ev2))),
	true((Err1, Err2) =@= (Ev1, Ev2))
]) :-
	%% 3: del
	source_db_del(Sid, Cat),
	t__source_db__t_e(Sid, Cat, Err1, Err2).

test(source_db__t_4,
[	setup((t__source_db__t(Sid, Cat, _, Ev1, _), copy_term(Ev1, Ev2))),
	true((Err1, Err2) =@= (Ev1, Ev2))
]) :-
	%% 4: empty
	t__source_db__t_e(Sid, Cat, Err1, Err2).

:- end_tests('sources_system:source_db').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_catch').

test(source_catch__t_t,
[	true(SRes =@= source_res(false, _))
]) :-
	source_catch(true, SRes).

test(source_catch__t_e,
[	true(SRes =@= source_res(true, err))
]) :-
	source_catch(throw(err), SRes).

:- end_tests('sources_system:source_catch').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_throw').

test(source_throw__t,
[	throws(source_error(smsg, _))
]) :-
	source_throw(smsg).

:- end_tests('sources_system:source_throw').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_rethrow').

t__source_rethrow__t_0([]).
t__source_rethrow__t_0([source_res(false, _)]).
t__source_rethrow__t_0([source_res(false, _), source_res(false, _)]).

t__source_rethrow__t_1([source_res(true, err)]).
t__source_rethrow__t_1([source_res(true, err), source_res(false, _)]).

t__source_rethrow__t_n([source_res(true, err), source_res(true, err)]).
t__source_rethrow__t_n([source_res(true, err), source_res(true, err), source_res(false, _)]).

test(source_rethrow__t_0,
[	forall(t__source_rethrow__t_0(SRess)),
	true
]) :-
	source_rethrow(SRess).

test(source_rethrow__t_1,
[	forall(t__source_rethrow__t_1(SRess)),
	throws(err)
]) :-
	source_rethrow(SRess).

test(source_rethrow__t_n,
[	forall(t__source_rethrow__t_n(SRess)),
	throws(source_error(many_errors([err, err]), _))
]) :-
	source_rethrow(SRess).

:- end_tests('sources_system:source_rethrow').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_system:source_has_err').

test(source_has_err__t_f, [fail]) :- source_has_err(source_res(false, _)).
test(source_has_err__t_t, [true]) :- source_has_err(source_res(true, err)).

:- end_tests('sources_system:source_has_err').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
