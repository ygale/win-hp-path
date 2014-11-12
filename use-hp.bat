@echo off
call find-hp.bat %1
if "%hppath%" == "" goto :oops
use-hppath %hppath%
goto :end

:oops
echo use-hp.bat: No Haskell Platform was found for: %1

:end
