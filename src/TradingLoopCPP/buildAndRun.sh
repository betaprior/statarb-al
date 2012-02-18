#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Enter the name of the C++ file"
    echo "(can use optional switches like --clean; it all will be passed to R CMD SHLIB)"
    exit 1
fi

# build the shared library for the C++ variant
# we have to let R know where the Rcpp header and library are
export PKG_CPPFLAGS="-I. "$(r -e "Rcpp:::CxxFlags()" )
export PKG_LIBS=$(r -e "Rcpp:::LdFlags()" )
#export CLINK_CPPFLAGS=$(r -e "Rcpp:::Cxx0xFlags()" )

R CMD SHLIB "$@"

