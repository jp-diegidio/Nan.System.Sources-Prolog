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
%	- Flags
%	- Recorded database

:- module(sources_system,
	[	%%	Sid generation
		source_typ_sid/3,    % +Src, -Typ, -Sid
		                     % -Src, -Typ, +Sid
		source_new_sid/2,    % +Typ, -Sid
		%%	Database records
		source_db_exists/1,  % +Sid
		source_db_enum/2,    % ?Sid, ?Term
		source_db_get/3,     % +Sid, +Cat, -Term
		source_db_add/3,     % +Sid, +Cat, +Term
		source_db_del/2,     % +Sid, +Cat
		%%	Error handling
		source_catch/2,      % :Goal, -Res
		source_throw/1,      % +EMsg
		source_rethrow/1,    % +Ress
		source_has_err/1,    % +Res
		%%	Debug logging
		source_log_call/3,   % +Sid, +Cat, +Act
		source_log_msg/4,    % +Sid, +Cat, +Act, +Msg
		source_log/4         % +Sid, +Cat, +Fmt, +Args
	]).

:- use_module(sources_types).

:- multifile
	prolog:message//1.

/** <module> Answer Sources :: System

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Module =sources_system= (nan/system/sources_system.pl) provides
system-level support predicates.

For code docs syntax and meaning see sources_docs.txt.

*NOTE*:
  - This module is meant for *|internal use only|*.

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3

@tbd  Add stack trace in debug mode.
@tbd  Remove logging calls with source_debug(false).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(debug)).
:- use_module(library(apply)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Sid generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	source_typ_sid(+Src:source, -Typ:source_typ, -Sid:source_sid) is det.
%!	source_typ_sid(-Src:source, -Typ:source_typ, +Sid:source_sid) is det.
%
%	Converts between a source reference and an internal sid.
%	
%	@arg  Src  The source reference.
%	@arg  Typ  The source type (one of =t0= or =t1=).
%	@arg  Sid  The source internal sid.

source_typ_sid(Src, Typ, Sid) :-
	Src = source(Typ, Id),
	source_sid__do(Typ, Id, Sid).

%!	source_new_sid(+Typ:source_typ, -Sid:source_sid) is det.
%
%	Generates a new internal sid given a source type.
%	
%	@arg  Typ  The source type (one of =t0= or =t1=).
%	@arg  Sid  The source internal sid.

source_new_sid(Typ, Sid) :-
	% 1-based (current flag is last id)
	flag(sources__id_max, I0, I0 + 1),
	succ(I0, I),
	atom_number(Id, I),
	source_sid__do(Typ, Id, Sid).

%	source_sid__do (+Typ, +Id, -Sid) is det
%	source_sid__do (-Typ, -Id, +Sid) is det

source_sid__do(Typ, Id, Sid) :-
	var(Sid), !,
	SCs = [sources__, Typ, '_', Id],
	atomic_list_concat(SCs, Sid).
source_sid__do(Typ, Id, Sid) :-
	atom_concat(sources__, Typ_Id, Sid),
	sub_atom(Typ_Id, 0, 2, _, Typ),
	sub_atom(Typ_Id, 3, _, 0, Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Database records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	source_db_exists(+Sid:source_sid) is semidet.
%
%	Tests that a record exists for a source.
%	
%	@arg  Sid  The source internal sid.

source_db_exists(Sid) :-
	source_db__recorded(Sid, _, _), !.

%!	source_db_enum(?Sid:source_sid, ?Term:any) is nondet.
%
%	Enumerates all recorded sources and corresponding terms.
%	
%	@arg  Sid   The source internal sid.
%	@arg  Term  The source's recorded term.

source_db_enum(Sid, Term) :-
	source_db__recorded(Sid, Term, _).

%!	source_db_get(+Sid:source_sid, +Cat:atom, -Term:any) is det.
%
%	Gets the recorded term of a source.
%	
%	@arg  Sid   The source internal sid.
%	@arg  Cat   The caller's category.
%	@arg  Term  The source's recorded term.
%	
%	@throws  source_error(db_does_not_exist(Sid, Cat))

source_db_get(Sid, Cat, Term) :-
	source_db__val_or_throw(Sid, Cat, Term, _).

%!	source_db_add(+Sid:source_sid, +Cat:atom, +Term:any) is det.
%
%	Adds the recorded term for a source.
%	
%	@arg  Sid   The source internal sid.
%	@arg  Cat   The caller's category.
%	@arg  Term  The source's recorded term.
%	
%	@throws  source_error(db_exists_already(Sid, Cat))

source_db_add(Sid, Cat, Term) :-
	source_db__noval_or_throw(Sid, Cat),
	source_db__recordz(Sid, Term).

%!	source_db_del(+Sid:source_sid, +Cat:atom) is det.
%
%	Deletes the recorded term of a source.
%	
%	@arg  Sid  The source internal sid.
%	@arg  Cat  The caller's category.
%	
%	@throws  source_error(db_does_not_exist(Sid, Cat))

source_db_del(Sid, Cat) :-
	source_db__val_or_throw(Sid, Cat, _, Ref),
	source_db__erase(Ref).

%	source_db__val_or_throw   (+Sid, +Cat, -Term, -Ref) is det
%	source_db__noval_or_throw (+Sid, +Cat)              is det

source_db__val_or_throw(Sid, _, Term, Ref) :-
	source_db__recorded(Sid, Term, Ref), !.
source_db__val_or_throw(Sid, Cat, _, _) :-
	source_throw(db_does_not_exist(Sid, Cat)).

source_db__noval_or_throw(Sid, _) :-
	\+ source_db__recorded(Sid, _, _), !.
source_db__noval_or_throw(Sid, Cat) :-
	source_throw(db_exists_already(Sid, Cat)).

%	source_db__recorded (+Sid, -Term, -Ref) is nondet
%	source_db__recordz  (+Sid, +Term)       is det
%	source_db__erase    (+Ref)              is semidet

source_db__recorded(Sid, Term, Ref) :-
	recorded(sources__db, Sid-Term, Ref).

source_db__recordz(Sid, Term) :-
	recordz(sources__db, Sid-Term).

source_db__erase(Ref) :-
	erase(Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Error handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	source_catch(:Goal:callable, -Res) is det.
%
%	Calls a goal with catch/3 and returns a result term.
%	
%	  - Goal invoked as =|Goal() is nondet|=.
%	
%	@arg  Goal  The goal to catch.
%	@arg  Res   The result of the call.

:- meta_predicate
	source_catch(0, -).

source_catch(Goal, Res) :-
	catch(
	(	call(Goal), HasErr = false
	), Err, HasErr = true),
	source_err__res(Res, HasErr, Err).

%!	source_throw(+EMsg:callable) is det.
%
%	Throws a source error for a message term.
%	
%	The error term has the form =|source_error(EMsg, _)|=.
%	
%	@arg  EMsg  The error message.

source_throw(EMsg) :-
	throw(source_error(EMsg, _)).

%!	source_rethrow(+Ress:list) is det.
%
%	Possibly throws a combined source error for a list of goal results.
%	
%	Depending on the list of results, the possible cases are:
%	  - No errors: does not trow (succeeds).
%	  - A single error: rethrows the error.
%	  - Multiple errors: throws a combined source error.
%	
%	A combined source error term has the form 
%	=|source_error(many_errors(Errs), _)|=
%	where _Errs_ is a list of all error terms found in the results.
%	
%	@arg  Ress  The list of goal results.

source_rethrow(Ress) :-
	foldl(source_rethrow__err, Ress, Errs, []),
	(	Errs = []    -> true
	;	Errs = [Err] -> throw(Err)
	;	source_throw(many_errors(Errs))
	).

source_rethrow__err(Res, Errs, ErrsT) :-
	source_err__res(Res, HasErr, Err),
	(	HasErr == true
	->	Errs = [Err| ErrsT]
	;	Errs = ErrsT
	).

%!	source_has_err(+Res) is semidet.
%
%	Tests if a source result indicates an error.
%	
%	@arg  Res  The goal result.

source_has_err(Res) :-
	source_err__res(Res, true, _).

%	source_err__res (?Res, ?HasErr, ?Err) is semidet

source_err__res(Res, HasErr, Err) :-
	Res = source_res(HasErr, Err).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Debug logging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	source_log_call(+Sid:source_sid, +Cat:atom, +Act:atom) is det.
%
%	Prints an =informational= message before or after a call.
%	
%	The message term has the form
%	=|sources__log(Tm, Sid, Cat, Fmt, Args)|=.
%	
%	@arg  Sid  The source internal sid.
%	@arg  Cat  The caller's category.
%	@arg  Act  The caller's action (one of =begin= or =end=).

source_log_call(Sid, Cat, Act) :-
	(	debugging(sources)
	->	source_log_call__do(Act, Sid, Cat)
	;	true
	).

source_log_call__do(begin, Sid, Cat) :- !,
	source_log__do(Sid, Cat, 'Started...', []).
source_log_call__do(end, Sid, Cat) :- !,
	source_log__do(Sid, Cat, 'Done.', []).

%!	source_log_msg(+Sid:source_sid, +Cat:atom, +Act:atom, +Msg:text) is det.
%
%	Prints an =informational= message about a source messaging.
%	
%	The message term has the form
%	=|sources__log(Tm, Sid, Cat, Fmt, Args)|=.
%	
%	@arg  Sid  The source internal sid.
%	@arg  Cat  The caller's category.
%	@arg  Act  The caller's action (one of =send= or =recv=).
%	@arg  Msg  The caller's message.

source_log_msg(Sid, Cat, Act, Msg) :-
	(	debugging(sources)
	->	source_log_msg__do(Act, Sid, Cat, Msg)
	;	true
	).

source_log_msg__do(send, Sid, Cat, Msg) :- !,
	source_log__do(Sid, Cat, '->>- ~w', [Msg]).
source_log_msg__do(recv, Sid, Cat, Msg) :- !,
	source_log__do(Sid, Cat, '-<<- ~w', [Msg]).

%!	source_log(+Sid:source_sid, +Cat:atom, +Fmt:text, +Args:list) is det.
%
%	Prints an =informational= message.
%	
%	The message term has the form
%	=|sources__log(Tm, Sid, Cat, Fmt, Args)|=.
%	
%	@arg  Sid   The source internal sid.
%	@arg  Cat   The caller's category.
%	@arg  Fmt   The caller's format.
%	@arg  Args  The caller's arguments.

source_log(Sid, Cat, Fmt, Args) :-
	(	debugging(sources)
	->	source_log__do(Sid, Cat, Fmt, Args)
	;	true
	).

%	source_log__do (+Sid, +Cat, +Fmt, +Args) is det
%	source_log__tm (+Ts, -Tm)                is det

source_log__do(Sid, Cat, Fmt, Args) :-
	get_time(Ts),
	PTerm = sources__log(Ts, Sid, Cat, Fmt, Args),
	print_message(informational, PTerm).

source_log__tm(Ts, Tm) :-
	Tf is floor(float_fractional_part(round(Ts * 1000) / 1000) * 1000),
	format_time(atom(Tm0), '%H%M%S', Ts),
	format(atom(Tm), '~a.~|~48t~d~3+', [Tm0, Tf]).

prolog:message(sources__log(Ts, Sid, Cat, Fmt, Args)) -->
	{	source_log__tm(Ts, Tm),
		source_typ_sid(Src, _, Sid),
		format(atom(Msg), Fmt, Args)
	}, ['~a : ~w : ~|~a~9+ : ~a'-[Tm, Src, Cat, Msg]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
