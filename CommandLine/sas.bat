::===============================================================================
::
:: This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
::
:: See LICENSE for full details.
::
::===============================================================================

:: RUN_SAS.BAT
::
@echo off

set inpname=%1
set sasname=sas-5.1-Cygwin.exe
set hh=%time:~0,2%

if "%time:~0,1%"==" " set hh=0%hh:~1,1%

set rundate=%date:~10,4%-%date:~4,2%-%date:~7,2%__%hh%.%time:~3,2%.%time:~6,2%

set dirname=%inpname%_%rundate%
md %dirname%

copy %inpname% .\%dirname%\%inpname%
copy %sasname% .\%dirname%\%sasname%

cd %dirname%
.\%sasname% < .\%inpname% > .\%inpname%.out
cd ..


echo on
