#!/bin/bash

SCRIPT_FILE='tr_test_generic_batch.r'

if [ $# -lt 2 ]; then
    echo "   $0 [ret_mtx_file] [signal_file_name] [[-yearsBack 7 -offsetYear 2009, etc.]]"
    echo "   default signal script is $SCRIPT_FILE; override it by supplying an -f option:"
    echo "   $0 -f MySript.r [ret_mtx_file] [signal_file_name] [[-yearsBack 7 -offsetYear 2009, etc.]]"
    exit 1
fi

while getopts ":f:" Option
do
    case $Option in
        f    ) SCRIPT_FILE=$OPTARG;;
        *    ) echo ""
               echo "Unknown option $Option supplied"
    esac
done
shift $(($OPTIND - 1)) #  Decrements the argument pointer so it points to next argument.

RM_FN=$1
SIG_FN=$2
shift
shift

RCmd $SCRIPT_FILE -saveSigFile TRUE -retMtxFilename $RM_FN -filename $SIG_FN "$@"

