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

:- module(sources_types,
	[	is_source/1,  % @Term
		is_answer/1   % @Term
	]).

/** <module> Answer Sources :: Types

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Module =sources_types= (nan/system/sources_types.pl) provides
type testing predicates to validate arguments in user code.

For code docs syntax and meaning see sources_docs.txt.

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PUBLIC interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	is_source(@Term:any) is semidet.
%
%	Tests that a term is a =source=.
%	
%	A =source= term has the form:
%	==
%	  s(source(+Typ:(s(t0); s(t1)), +Id:atom))
%	==
%	
%	Examples:
%	==
%	source(t0, Id)
%	source(t1, Id)
%	==
%	
%	@arg  Term  The term to test.

is_source(Src) :-
	callable(Src),
	Src = source(Typ, Id),
	is_source__typ(Typ),
	is_source__id(Id).

is_source__typ(Typ) :-
	atom(Typ),
	memberchk(Typ, [t0, t1]).

is_source__id(Id) :-
	atom(Id).

%!	is_answer(@Term:any) is semidet.
%
%	Tests that a term is a =answer=.
%	
%	A =answer= term has the form:
%	==
%	  s(answer(+Det:(s(more); s(last)), +Ret:s(the(?Sol:any))))
%	; s(answer(+Det:s(fail),            +Ret:s(no)))
%	==
%	
%	Examples:
%	==
%	answer(more, the(Sol))
%	answer(last, the(Sol))
%	answer(fail, no)
%	==
%	
%	@arg  Term  The term to test.

is_answer(Ans) :-
	callable(Ans),
	Ans = answer(Det, Ret),
	is_answer__det(Det),
	is_answer__ret(Det, Ret).

is_answer__det(Det) :-
	atom(Det),
	memberchk(Det, [fail, more, last]).

is_answer__ret(fail, Ret) :- !,
	Ret == no.
is_answer__ret(_, Ret) :-
	subsumes_term(the(_), Ret).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
