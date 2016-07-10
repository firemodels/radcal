@echo off
Title Building debug radcal for 64 bit Windows

call ..\Scripts\setup_intel_compilers.bat

erase *.obj *.mod
make VPATH="../../Source" -f ..\makefile2 intel_win_64_db
pause
