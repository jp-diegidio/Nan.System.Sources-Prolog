:	Nan.System.Sources
:	Nan.System.Sources/Prolog 1.2.0-beta
:	Answer Sources in Prolog
:	Copyright 2015-2017 Julio P. Di Egidio
:	<mailto:julio@diegidio.name>
:	<http://julio.diegidio.name/Projects/Nan.System.Sources/>
:	
:	This file is part of Nan.System.Sources.
:	
:	Nan.System.Sources is free software: you can redistribute it and/or modify
:	it under the terms of the GNU General Public License as published by
:	the Free Software Foundation, either version 3 of the License, or
:	(at your option) any later version.
:	
:	Nan.System.Sources is distributed in the hope that it will be useful,
:	but WITHOUT ANY WARRANTY; without even the implied warranty of
:	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:	GNU General Public License for more details.
:	
:	You should have received a copy of the GNU General Public License
:	along with Nan.System.Sources.  If not, see <http://www.gnu.org/licenses/>.

@rem Nan.System.Sources::pack.bat (1.2.0-beta)
@rem Author: Julio P. Di Egidio (julio@diegidio.name)
@rem Requires PowerShell 4.0 and .NET 4.5

@echo off

set name=nan_system_sources
set proPath=nan\system
set /P ver=Nan.System.Sources Version ? 

set infoDir=..
set codeDir=..\Code
set testDir=..\Tests
set workDir=.\.work
set targetFile=..\Publish\%name%-%ver%.zip

if exist "%workDir%" (
	rmdir /S /Q "%workDir%"
)

echo Copying info...

xcopy /Q "%infoDir%\COPYING" "%workDir%\"
xcopy /Q "%infoDir%\HISTORY.md" "%workDir%\"
xcopy /Q "%infoDir%\README.md" "%workDir%\"
xcopy /Q "%infoDir%\pack.pl" "%workDir%\"

echo Copying prolog...

xcopy /Q "%codeDir%\*.*" "%workDir%\prolog\%proPath%\"

echo Copying test...

xcopy /Q "%testDir%\*.*" "%workDir%\test\"

echo Generating target...

if exist "%targetFile%" (
	del "%targetFile%"
)

if exist "%targetFile%" (
	pause
) else (
	PowerShell ^
		-ExecutionPolicy Bypass ^
		-NoLogo -NoProfile ^
		-File ".\zipDir.ps1" "%workDir%" "%targetFile%"
)

echo Cleaning up...

rmdir /S /Q "%workDir%"

echo Done.

pause
