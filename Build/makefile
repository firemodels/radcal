# Make file for RADCAL
# Works for the Intel Fortran Compiler
# Tested on version: ifort (IFORT) 12.1.3 20120212
# Build RADCAL.x with static library
# For other coompilers: Make sure to provide path to libraries or remove flag -static-intel
# make sure your fortran compiler accepts C preprocessor macro
# 
# To use:
# typing "make" will create the instrumented, debugging version of RADCAL with run-time printing
# typing "make DEBUG=0" creates the production version of RADCAL with optimization flags
#                       This version is faster than debug
# typing "make DEBUG=1 PARALL=1" creates the debugging open mp version
# typing "make DEBUG=0 PARALL=1" creates the production open mp version. Does not improves
#                                much the performances.
#
#========================================================================================

DEBUG ?= 1
PARALL ?= 2

today_date = `date`
compile_date = $(shell date)

ifeq ($(PARALL),1)
    PARA_FLAG = -openmp -Dopenmp
else
    PARA_FLAG = 
endif

ifeq ($(DEBUG), 1)
    COMP_FLAGS = -fpp -Ddebug -Ddate='"$(compile_date)"' $(PARA_FLAG) -warn unused -debug full  -pg -check all -stand:f08 -r8 -traceback -ftrapuv -c -o  $@
    LINK_FLAGS = -v  $(PARA_FLAG) -cxxlib -debug -check all -traceback -r8 -pg
else
    COMP_FLAGS = -fpp -O2 $(PARA_FLAG) -Ddate='"$(compile_date)"' -r8 -c -o $@
    LINK_FLAGS = -v $(PARA_FLAG) -cxxlib -r8
    endif

COMPILE_F90 = ifort $(COMP_FLAGS)
LINK        = ifort $(LINK_FLAGS)

#========================================================================================

PROG = RADCAL.exe

#========================================================================================

#TARGETS:
all: executable

clean :; @rm -f *.obj *.mod $(PROG)

#-----------------------------------------------------------------------------------------

OBJ_MAIN =             \
        RADCAL.obj

#----------------------------------------------------------------------------------------
#--------------------------------------  Main Routine  -- -------------------------------
#----------------------------------------------------------------------------------------

RADCAL.obj : RADCAL.f90 
	$(COMPILE_F90) RADCAL.f90

#========================================================================================

executable:  $(OBJ_MAIN)
	$(LINK) $(OBJ_MAIN) -o $(PROG)




