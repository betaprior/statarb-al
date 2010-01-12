#!/bin/bash

if [ $# -lt 1 ]
then
echo "Enter R script name"
exit 1
fi




scriptPath=$1
shift
R --vanilla --slave --args -batch T $* < $scriptPath
