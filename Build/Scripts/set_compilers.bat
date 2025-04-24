@echo off

:: Initialize compiler flags
set set_COMP_FC=0

:: Check and set Fortran compiler
if defined FIREMODELS_FC (
    set COMP_FC=%FIREMODELS_FC%
    where /q %COMP_FC% && (
        set set_COMP_FC=1
    ) || (
        echo Warning: %FIREMODELS_FC% is not available. Searching for an alternative.
    )
)
if %set_COMP_FC%==0 (
    for %%F in (ifx ifort) do (
        where /q %%F && set COMP_FC=%%F && set set_COMP_FC=1 && goto :found_fc
    )
    echo Error: Neither ifx nor ifort is available. & exit /b 1
)
:found_fc

:: Display selected compilers
echo.
echo Firemodels and Third-party libs Fortran Compiler: %COMP_FC%
echo.



