#!/bin/sh 
## Copyright 2000--2004 Bluespec, Inc.  All rights reserved.
##


usage() 
{
    echo "Usage: $0  -R | -all | -f <filename>"
    echo "" 
    echo "summary: Unconverts Maybe = Valid | Invalid  --> Maybe = Just | Nothing"
    echo "One and only one argument must be specified"
    echo "   -all          converts all bsv files in current directory to new type names"
    echo "   -R            converts all bsv file in directory tree to use new type names"
    echo "   -f <filename> converts <filename> to new type names."
}

## function to convert one file
convert_file ()
{
    echo "Updating types to bsc-3.8.30 in file: $1" ;
    
    cp $1 $1.origX && \
    sed \
        -es/\\\<Valid\\\>/Just/g \
        -es/\\\<Invalid\\\>/Nothing/g \
            $1.origX > $1
}


recurse=false
if [ "$1" == "-R" ] ; then
    recurse=true
    shift
elif [ "$1" == "-f" ] ; then
    convert_file $2
    exit 
elif [ "$1" == "-all" ] ; then 
    shift
else
    usage
    exit 
fi
if [ "$1" != "" ] ; then
    usage 
    exit 
fi


## Normal path -- convert all files.

##  We want to change files which contain these patterns 
patterns="Valid|Invalid"

files=`ls | egrep ".bsv$|.bs$" `
if [ "" != "$files" ] ; then
    targetFiles=`egrep -l $patterns $files`

    for f in ${targetFiles}
    do
        convert_file $f
    done
fi

## recurse down the directory tree
if [ true == $recurse ] ; then
    directories=`ls -l | awk '/^d/ {print $NF}' `
    for d in ${directories} 
    do
        echo "Converting files in directory: $d"
        cd $d;
        $0 -R
        cd .. 
    done
fi
