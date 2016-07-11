#!/bin/bash

source $IFORT_COMPILER/bin/compilervars.sh intel64

rm *.o *.mod
make VPATH="../../Source" -f ../makefile2 intel_osx_64_db
