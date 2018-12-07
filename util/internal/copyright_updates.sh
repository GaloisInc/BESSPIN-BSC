#!/bin/sh 
## Copyright 2006 Bluespec, Inc.  All rights reserved.
##
## Updates the copyrights dates in various kinds of files

usage() 
{
    echo "Usage: $0  -R | -all | -f <filename>"
    echo " primitive utility to update copyright dates in a tree." 
    echo "   -all          converts all files in current directory to new date"
    echo "   -R            converts all file in directory tree to new date"
    echo "   -f <filename> converts <filename> tocopyright date."
}

## function to convert one file
convert_file ()
{
    echo "Updating copyright in file: $1" ;
    #  e.g. 
    # // Copyright 2000--2005 Bluespec, Inc.  All rights reserved.
    cp $1 $1.orig && \
    sed \
        -e/opyright.*Bluespec/s/\\\<2008\\\>/2009/g \
            $1.orig > $1
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
patterns="Copyright|copyright"

files=`ls | egrep ".bsv$|.v$|.tcl$|.c$|.h$" `
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
