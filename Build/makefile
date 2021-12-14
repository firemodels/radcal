# Makefile for RADCAL

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 64 bit parallel version (MPI) of RADCAL under Linux using the Intel compilers type:
#
# make mpi_intel_linux_64

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

ifeq ($(shell echo "check_quotes"),"check_quotes")
# windows
  BUILD_DATE := $(shell date /t) $(shell time /t)
  GIT_HASH := $(shell ..\Scripts\githash)
  GIT_DATE := $(shell ..\Scripts\gitlog)
else
# linux/osx
  GIT_HASH := $(shell git describe --long --dirty)
  GIT_DATE := $(shell git log -1 --format=%cd)
  BUILD_DATE := $(shell date "+%b %d, %Y  %T")
# The preprocessor used by xlf uses comma as separation between preprocessing commands.
# Commas needs to be escaped if they are to be used as part of a value in a preprocessing variable.
  BUILD_DATE_XLF := $(shell date "+%b %d\, %Y  %T")  
endif

GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
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

# Windows Intel Compiler

#*** note: the following scripts (located in ..\Scripts directory) must be run before running make with this target
#    ..\Scripts\ifortvars intel64

intel_win_64 : FFLAGS = /O2 /Qipo /traceback $(GITINFO) 
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = radcal_win_64
intel_win_64 : setup_win $(objwin_serial)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin_serial)

intel_win_64_db : FFLAGS = /Od /nologo /debug:full /check /warn /extend_source:132 /traceback /stand:f18 /Z7 /MDd $(GITINFO) 
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : obj = radcal_win_64_db
intel_win_64_db : setup_win $(objwin_serial)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin_serial)

# Intel Compiler for Linux

intel_linux_64 : FFLAGS = -m64 -O2 -ipo -traceback $(GITINFO) 
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = radcal_intel_linux_64
intel_linux_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -stand:f18 $(GITINFO) 
intel_linux_64_db : LFLAGS = -static-intel
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : obj = radcal_intel_linux_64_db
intel_linux_64_db : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# OSX   

intel_osx_64 : FFLAGS = -m64 -O2 $(GITINFO) 
intel_osx_64 : LFLAGS = -static-intel
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = radcal_intel_osx_64
intel_osx_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

intel_osx_64_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -stand:f18 $(GITINFO) 
intel_osx_64_db : LFLAGS = -static-intel
intel_osx_64_db : FCOMPL = ifort
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

