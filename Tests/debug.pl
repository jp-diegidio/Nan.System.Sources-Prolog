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

:- module(debug, []).

:- multifile
	prolog:message//1.

/** <module> Answer Sources :: Docs

Part of *|Nan.System.Sources|* (nan/system/sources.pl)

Starts the documentation server on port =8001= with editing disabled.

Use doc_browser/0 to open the default browser.

(Integrated with SWI-Prolog's documentation system.)

@author		Julio P. Di Egidio
@version	1.2.0-beta
@copyright	2015-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(doc_http)).

doc_server_port_(8001).

:- consult(test_all).

:- initialization
	doc_server_port_(Port),
	portray_text(true),
	doc_server(Port, [edit(false)]),
	print_message(information, debug__browser_help).

prolog:message(debug__browser_help) -->
	['Use doc_browser/0 to open the default browser.'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
