#!/bin/bash
platform=intel
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh $platform
source ../Scripts/set_compilers.sh

echo Building $target
make -j4 DEBUG=1 VPATH="../../Source" -f ../makefile $target
