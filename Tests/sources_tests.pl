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

:- module(sources_tests, []).

/** <module> Answer Sources :: Sources tests

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Tests for module =sources= (nan/system/sources.pl).

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization
	use_module(loader),
	module_path('sources.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t__setup :-
	catch(ignore(source_destroy_all), E, print_message(error, E)),
	flag(sources__id_max, _, 0).

:- meta_predicate
	t__com(3, 4, +, +, -).

t__com(GRst, GNxt, Sta0, Srcs, Src) :-
	source_create_com(Srcs, GRst, GNxt, Sta0, Src).

t__src(t0, source(t0, '1'), source_create(I, between(1, 2, I))).
t__src(t0, source(t0, '2'), source_create(I, between(3, 4, I))).
t__src(t1, source(t1, '3'), source_create_com(Srcs, GRst, GNxt, 1)) :-
	Srcs = [source(t0, '1'), source(t0, '2')],
	GRst = t__src__t1_rst,
	GNxt = t__src__t1_nxt.

t__src__t1_rst([Src1, Src2], _, 1) :-
	source_reset(Src1),
	source_reset(Src2).

t__src__t1_nxt([Src1, _], 1, 2, Ans) :- !,
	source_next(Src1, Ans).
t__src__t1_nxt([_, Src2], 2, 1, Ans) :-
	source_next(Src2, Ans).

t__src__t1_ans(answer(more, the(1))).
t__src__t1_ans(answer(more, the(3))).
t__src__t1_ans(answer(last, the(2))).
t__src__t1_ans(answer(last, the(4))).
t__src__t1_ans(answer(fail, no)).
t__src__t1_ans(answer(fail, no)).

t__err(Src, Es0, Errs) :-
	t__err__e0(Src, Es0),
	\+ source_exists(Src),
	catch(source_destroy(Src), Err1, true),
	catch(source_reset(Src), Err2, true),
	catch(source_next(Src, _), Err3, true),
	catch(source_next_begin(Src), Err4, true),
	catch(source_next_end(Src, _), Err5, true),
	Errs = [Err1, Err2, Err3, Err4, Err5].

t__err__e0(Src, Es0) :-
	E1 = source_error(does_not_exist(Src), _),
	copy_term(E1, E2),
	copy_term(E1, E3),
	copy_term(E1, E4),
	copy_term(E1, E5),
	Es0 = [E1, E2, E3, E4, E5].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources:basic', [setup(t__setup)]).

test(basic__t_1,                        %% 1: empty
[	forall(t__src(_, Src, _)),
	true(Errs =@= Es0)
]) :-
	\+ source_destroy_all,
	t__err(Src, Es0, Errs).

test(basic__t_2,                        %% 2: create
[	forall(t__src(_, Src0, GNew)),
	true(Src == Src0)
]) :-
	call(GNew, Src),
	source_exists(Src).

test(basic__t_3,                        %% 3: next b-e
[	forall((
		t__src(t1, Src, _),
		t__src__t1_ans(Ans0)
	)),
	true(Ans == Ans0)
]) :-
	source_next_begin(Src),
	source_next_end(Src, Ans).

test(basic__t_4,                        %% 4: reset
[	forall(t__src(t1, Src, _)),
	true
]) :-
	source_reset(Src).

test(basic__t_5,                        %% 5: next
[	forall((
		t__src(t1, Src, _),
		t__src__t1_ans(Ans0)
	)),
	true(Ans == Ans0)
]) :-
	source_next(Src, Ans).

test(basic__t_6,                        %% 6: destroy
[	forall(t__src(_, Src, _)),
	true(Errs =@= Es0)
]) :-
	source_destroy(Src),
	t__err(Src, Es0, Errs).

test(basic__t_7,                        %% 7: destroy_all
[	true
]) :-
	\+ source_destroy_all.

:- end_tests('sources:basic').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources:using', [setup(t__setup)]).

test(using__t_1,                        %% 1: using_source
[	forall(t__src(t0, _, source_create(TSol, GExe))),
	true(Errs =@= Es0)
]) :-
	using_source(TSol, GExe, Src, source_exists(Src)),
	t__err(Src, Es0, Errs).

test(using__t_2,                        %% 2: using_source_com
[	forall(t__src(t1, _, source_create_com(Srcs, GRst, GNxt, Sta0))),
	true(Errs =@= Es0)
]) :-
	GCom = t__com(GRst, GNxt, Sta0),
	using_source_com(Srcs, GCom, Src, source_exists(Src)),
	t__err(Src, Es0, Errs).

test(using__t_3,                       %% 3: post-using
[	true
]) :-
	\+ source_destroy_all.

:- end_tests('sources:using').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
