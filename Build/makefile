# Makefile for RADCAL

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 64 bit parallel version (MPI) of RADCAL under Linux using the Intel compilers type:
#
# make mpi_intel_linux_64

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

ifeq ($(shell echo "check_quotes"),"check_quotes")
# windows
  BUILD_DATE := $(shell powershell -Command "(Get-Date).ToString('ddd-MM/dd/yyyy-hh:mm-tt')")
  GIT_STAT := $(shell ..\Scripts\gitstat)
  GIT_HASH := $(shell ..\Scripts\githash)
  GIT_BRANCH := $(shell ..\Scripts\gitbranch)
  GIT_DATE := $(shell ..\Scripts\gitlog)
else
# linux/osx
  SHELL:=/bin/bash
  GIT_HASH := $(shell git describe --long --abbrev=7)
  ifeq ("$(shell git diff --shortstat $(VPATH)/*.f90)","")
    GIT_STAT :=
  else
    GIT_STAT := -dirty
  endif
  GIT_DATE := $(shell git log -1 --format=%cd)
  GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
  BUILD_DATE := $(shell date "+%b %d, %Y  %T")
# The preprocessor used by xlf uses comma as separation between preprocessing commands.
# Commas needs to be escaped if they are to be used as part of a value in a preprocessing variable.
  BUILD_DATE_XLF := $(shell date "+%b %d\, %Y  %T")
  CLT_VERSION := $(shell ../Scripts/osx_clt.sh)
endif

GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)$(GIT_STAT)-$(GIT_BRANCH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
GITINFO=-fpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
GITINFOGNU=-cpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
GITINFOXLF=-qsuffix=cpp=f90 '-WF,-DGITHASH_PP=\"$(GIT_HASH)\",-DGITDATE_PP=\"$(GIT_DATE)\",-DBUILDDATE_PP=\"$(BUILD_DATE_XLF)\"'
GITINFOPGI=-Mpreprocess $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""


# Definition of the non-MPI object variables

obj_serial = rmod.o rcal.o main.o

objwin_serial = $(obj_serial:.o=.obj)

# General Purpose Rules

no_target:
	@echo \******** You did not specify a make target \********
	@echo Please read the comments at the top of the makefile

setup:
%.o : %.mod

setup_win:
%.obj : %.mod

.SUFFIXES: .f90 .o .obj

.f90.o:
	$(FCOMPL) -c $(FFLAGS) $<
.f90.obj:
	$(FCOMPL) -c $(FFLAGS) $<

# Windows Intel Fortran Compiler
#*** Note: the script, Build\Scripts\setup_intel_compilers.bat, must be run before running make with this target

intel_win_64 : FFLAGS = -D_WIN32 /Qipo /O2 /I$(MPIINCLUDE) /wrap-margin- $(GITINFO) /Qdiag-disable:10448
intel_win_64 : FCOMPL = $(COMP_FC)
intel_win_64 : obj = radcal_win_64
intel_win_64 : setup_win $(objwin_serial)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin_serial)

intel_win_64_db : FLAGS = -D_WIN32 /Od /nologo /debug:all /Z7 /extend-source:132 /warn:unused /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f18 /wrap-margin- $(GITINFO)  /Qdiag-disable:10448
intel_win_64_db : FCOMPL = $(COMP_FC)
intel_win_64_db : obj = radcal_win_64_db
intel_win_64_db : setup_win $(objwin_serial)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin_serial)

# Intel Compiler for Linux

intel_linux_64 : FFLAGS = -m64 -O2 -ipo -no-wrap-margin $(GITINFO) -DUSE_IFPORT  -diag-disable=10448
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = $(COMP_FC)
intel_linux_64 : obj = radcal_intel_linux_64
intel_linux_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) $(FOPENMPFLAGS) -o $(obj) $(obj_serial)

intel_linux_64_db : FFLAGS = -m64 -check all -warn all -diag-error=remark,warn,error -O0 -g -traceback -fpe0 -nofltconsistency -stand:f18 -no-wrap-margin $(GITINFO) -DUSE_IFPORT -diag-disable=10448_INTEL
intel_linux_64_db : LFLAGS = -static-intel
intel_linux_64_db : FCOMPL = $(COMP_FC)
intel_linux_64_db : obj = radcal_intel_linux_64_db
intel_linux_64_db : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# OSX

intel_osx_64 : FFLAGS = -m64 -O2 -stand:f18 -no-wrap-margin $(GITINFO)
intel_osx_64 : LFLAGS = -static-intel
intel_osx_64 : FCOMPL = $(COMP_FC)
intel_osx_64 : obj = radcal_intel_osx_64
intel_osx_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

intel_osx_64_db : FFLAGS = -m64 -check all -fp-stack-check -warn unused -O0 -auto -WB -g -traceback -fpe0 -nofltconsistency -stand:f18 -no-wrap-margin $(GITINFO)
intel_osx_64_db : LFLAGS = -static-intel
intel_osx_64_db : FCOMPL = $(COMP_FC)
intel_osx_64_db : obj = radcal_intel_osx_64_db
intel_osx_64_db : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# *** Object Dependencies ***

rcal.o : rmod.o
main.o : rmod.o rcal.o

rcal.obj : rmod.obj
main.obj : rmod.obj rcal.obj

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj

.PHONY : winclean
winclean:
	-erase *.mod *.obj

