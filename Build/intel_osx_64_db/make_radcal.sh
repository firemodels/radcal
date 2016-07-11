#!/bin/bash

source $IFORT_COMPILER/bin/compilervars.sh intel64

make VPATH="../../Source" -f ../makefile intel_osx_64_db
