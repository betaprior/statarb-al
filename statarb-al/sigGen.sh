#!/bin/bash

if [ $# -lt 2 ]; then
    echo "$0 [ret_mtx_file] [signal_file_name] [[-yearsBack 7 -offsetYear 2009, etc.]]"
    exit 1
fi

RM_FN=$1
SIG_FN=$2
shift
shift

RCmd tr_test_generic_batch.r -saveSigFile TRUE -retMtxFilename $RM_FN -filename $SIG_FN "$@"