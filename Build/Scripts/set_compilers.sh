#!/bin/bash

echo "FDS build target = $FDS_BUILD_TARGET"

# Initialize variables and check environment variables
set_compiler_from_env_var() {
    local var_name=$1    # Variable name to set (e.g., COMP_CC, COMP_CXX, COMP_FC)
    local env_var=$2     # Environment variable to check (e.g., FIREMODELS_CC)
    local set_flag_var=$3 # Flag variable name (e.g., set_COMP_CC, set_COMP_CXX, set_COMP_FC)

    if [ -n "${!env_var}" ]; then
        eval "$var_name=${!env_var}"
        if command -v "${!var_name}" &> /dev/null; then
            eval "$set_flag_var=1"
        else
            echo "Warning: ${!env_var} specified by $env_var is not available. Searching for an alternative."
        fi
    fi
}


# Set compilers based on the build target
select_compiler_from_system() {
    local var_name=$1       # Variable to set (COMP_CC, COMP_CXX, COMP_FC)
    shift
    local compilers=("$@")  # List of compilers to check in order
    local set_flag_var="set_$var_name"

    # Only proceed if the compiler flag is not set
    if [ "$(eval echo \$$set_flag_var)" -eq 0 ]; then
        for compiler in "${compilers[@]}"; do
            if command -v "$compiler" &> /dev/null; then
                # Set the compiler variable
                eval "$var_name=$compiler"
                # Set the flag variable to 1 (indicating the compiler was found)
                eval "$set_flag_var=1"
                return
            fi
        done
        echo "Error: None of the specified compilers (${compilers[*]}) are available for $var_name on this system."
        exit 1
    fi
}


# Following variables indicate if compilers are set using environment variables FIREMODELS_XXX.
set_COMP_FC=0

# Check environment variables for compilers
set_compiler_from_env_var COMP_FC FIREMODELS_FC set_COMP_FC

# Determine compiler list based on build target
if [[ "$FDS_BUILD_TARGET" == *"osx"* ]]; then
    select_compiler_from_system COMP_FC mpifort
elif [[ "$FDS_BUILD_TARGET" == *"intel"* ]]; then
    select_compiler_from_system COMP_FC mpiifx mpiifort
else  # Default to GNU compilers
    select_compiler_from_system COMP_FC mpifort
fi

echo "Firemodels and Thirdparty libs Fortran compiler COMP_FC=$COMP_FC"

export COMP_FC=$COMP_FC
