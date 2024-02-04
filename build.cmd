@echo off

set LAZARUS_HOME=C:\Lazarus
set LAZBUILD=%LAZARUS_HOME%\lazbuild.exe

set "BASEDIR=%~dp0"
if "%BASEDIR:~-1%"=="\" set "BASEDIR=%BASEDIR:~0,-1%"

%LAZBUILD% --bm=Release -B %BASEDIR%\src\pascal\sling4j_alpha0\sling4j_alpha0.lpi
%LAZBUILD% --bm=Debug -B %BASEDIR%\src\pascal\sling4j_alpha0\sling4j_alpha0.lpi

