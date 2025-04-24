#!/bin/bash

dir=`pwd`
target=${dir##*/}

source ../Scripts/set_compilers.sh

echo Building $target
make -j4 DEBUG=1 VPATH="../../Source" -f ../makefile $target
