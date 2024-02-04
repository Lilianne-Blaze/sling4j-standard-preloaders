@echo off

set LAZARUS_HOME=C:\Lazarus
set LAZBUILD=%LAZARUS_HOME%\lazbuild.exe

set "BASEDIR=%~dp0"
if "%BASEDIR:~-1%"=="\" set "BASEDIR=%BASEDIR:~0,-1%"

%LAZBUILD% --bm=Release -B %BASEDIR%\src\main\pascal\sling4j_alpha0\sling4j_alpha0.lpi
%LAZBUILD% --bm=Debug -B %BASEDIR%\src\main\pascal\sling4j_alpha0\sling4j_alpha0.lpi

mkdir %BASEDIR%\target
mkdir %BASEDIR%\target\classes
mkdir %BASEDIR%\target\classes\META-INF
mkdir %BASEDIR%\target\classes\META-INF\sling4j
mkdir %BASEDIR%\target\classes\META-INF\sling4j\standard-preloaders

set NATBINDIR=%BASEDIR%\target\classes\META-INF\sling4j\standard-preloaders

copy %BASEDIR%\src\main\pascal\sling4j_alpha0\*.exe %NATBINDIR%
dir /b %NATBINDIR%\*.exe > %NATBINDIR%\filelist.windirb

