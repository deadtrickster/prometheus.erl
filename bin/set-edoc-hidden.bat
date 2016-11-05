@echo off

set FILE=%~f1

if not exist %FILE% (
    call :USAGE
)

set /p FIRST_LINE=<%FILE%
set "HIDDEN=%%%% @hidden"

if not %FIRST_LINE% == %HIDDEN% (
    call :GETTEMPNAME
    echo %TMPFILE%
    echo %HIDDEN% > %TMPFILE%
    type %FILE% >> %TMPFILE%
    type %TMPFILE% > %FILE%
    del %TMPFILE%
    exit /B 0 
) else (
    exit /B 0
)

:USAGE
echo "Must specify file"
exit /B 1

:GETTEMPNAME
set TMPFILE=%TMP%\set-edoc-hidden-%RANDOM%-%TIME:~6,5%.tmp
if exist "%TMPFILE%" GOTO :GETTEMPNAME
