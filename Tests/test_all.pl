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

:- module(test_all,			% TODO: Check integration with SWI. #####
	[	test_all/0,  % 
		test_all/1   % +Spec
	]).

/** <module> Answer Sources :: All tests

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Predicates to run all library tests.

(Integrated with SWI-Prolog's pack system.)

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(plunit)).

:- use_module(sources_types_tests).
:- use_module(sources_system_tests).
:- use_module(sources_tests).

:- set_test_options(		% TODO: Check integration with SWI. #####
	[	load(always),
		run(manual),
		silent(false),
		sto(false),
		cleanup(false)
	]).

t__rnd_seed(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_all is det.
%
%	Prepares the test context then calls time/1 around run_tests/0.

test_all :-
	test__do(run_tests).

%!	test_all(+Spec:atom) is det.
%
%	Prepares the test context then calls time/1 around run_tests/1.

test_all(Spec) :-
	test__do(run_tests(Spec)).

%	test__do (+GRun) is det

:- meta_predicate
	test__do(0).

test__do(GRun) :-
	test__rnd,
	time(GRun).

test__rnd :-
	t__rnd_seed(Seed),
	set_random(seed(Seed)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
