@echo off
call find-hp.bat %1
if "%hppath%" == "" goto :oops
activate-hppath %hppath%
goto :end

:oops
echo activate-hp.bat: No Haskell Platform was found for: %1

:end
