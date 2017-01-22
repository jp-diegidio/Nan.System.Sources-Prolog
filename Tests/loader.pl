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

:- module(loader,
	[	module_sdir/1,  % -SDir
		module_path/2,  % +File, -Path
		module_path/3   % +Dir, +File, -Path
	]).

/** <module> Answer Sources :: Module loader

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Predicates for locating module code files.

(Integrated with SWI-Prolog's pack system.)

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	module_sdir(-SDir:atom) is nondet.
%
%	Enumerates the search directories.

module_sdir('../Code/').
module_sdir('../prolog/nan/system/').

%!	module_path(+File:atom, -Path:atom) is det.
%
%	Returns the full Path for File in any of the search directories.
%	
%	It is equivalent to:
%	==
%	module_path(File, Path) :-
%		module_sdir(Dir),
%		module_path(Dir, File, Path), !.
%	==
%	
%	Throws if file File does not exist in any _Dir_:
%	the error term is =|loader_error(file_not_found(any, File), _)|=.

module_path(File, Path) :-
	module_sdir(Dir),
	module_path__do(Dir, File, Path), !.
module_path(File, _) :-
	module_path__throw(any, File).

%!	module_path(+Dir:atom, +File:atom, -Path:atom) is det.
%
%	Concatenates Dir and File into file Path.
%	
%	Throws if File does not exist in Dir:
%	the error term is =|loader_error(file_not_found(Dir, File), _)|=.

module_path(Dir, File, Path) :-
	module_path__do(Dir, File, Path), !.
module_path(Dir, File, _) :-
	module_path__throw(Dir, File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_path__do(Dir, File, Path) :-
	atom_concat(Dir, File, Path),
	exists_file(Path).

module_path__throw(Dir, File) :-
	throw(loader_error(file_not_found(Dir, File), _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
