#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

source ../Scripts/set_compilers.sh

echo Building $target
`ifort -v`
make -j4 DEBUG=1 VPATH="../../Source" -f ../makefile $target
