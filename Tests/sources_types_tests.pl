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

:- module(sources_types_tests, []).

/** <module> Answer Sources :: Sources types tests

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Tests for module =sources_types= (nan/system/sources_types.pl).

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization
	use_module(loader),
	module_path('sources_types.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_types:is_source').

%	  s(source(+Typ:(s(t0); s(t1)), +Id:atom))

t__is_source__f_as(T) :- member(T, [_, 1, source]).
t__is_source__f_ts(T) :- member(T, [_, 1, s]).
t__is_source__f_is(T) :- member(T, [_, 1]).

test(is_source__f_as,
[	forall(t__is_source__f_as(T)),
	fail
]) :-
	is_source(T).

test(is_source__f_ts,
[	forall(t__is_source__f_ts(T)),
	fail
]) :-
	is_source(source(T, id)).

test(is_source__f_is,
[	forall((
		member(Typ, [t0, t1]),
		t__is_source__f_is(T)
	)),
	fail
]) :-
	is_source(source(Typ, T)).

test(is_source__t_t0, [true]) :- is_source(source(t0, id)).
test(is_source__t_t1, [true]) :- is_source(source(t1, id)).

:- end_tests('sources_types:is_source').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sources_types:is_answer').

%	  s(answer(+Det:(s(more); s(last)), +Ret:s(the(?Sol:any))))
%	; s(answer(+Det:s(fail),            +Ret:s(no)))

t__is_answer__f_as(T) :- member(T, [_, 1, answer]).
t__is_answer__f_ds(T) :- member(T, [_, 1, a]).
t__is_answer__f_rs(T) :- member(T, [_, 1, the]).

test(is_answer__f_as,
[	forall(t__is_answer__f_as(T)),
	fail
]) :-
	is_answer(T).

test(is_answer__f_ds,
[	forall((
		t__is_answer__f_ds(T),
		member(Ret, [no, the(_)])
	)),
	fail
]) :-
	is_answer(answer(T, Ret)).

test(is_answer__f_rs,
[	forall((
		member(Det, [more, last, fail]),
		t__is_answer__f_rs(T)
	)),
	fail
]) :-
	is_answer(answer(Det, T)).

test(is_answer__t_mt, [true]) :- is_answer(answer(more, the(_))).
test(is_answer__t_lt, [true]) :- is_answer(answer(last, the(_))).
test(is_answer__t_fn, [true]) :- is_answer(answer(fail, no)).

:- end_tests('sources_types:is_answer').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
