#!/bin/sh 
## Copyright 2000--2004 Bluespec, Inc.  All rights reserved.
##
## summary: converts files to new type names found in bcs version 3.8.30 and later."
## One and only one argument must be specified"
##    -all          converts all bsv files in current directory to new type names"
##    -R            converts all bsv file in directory tree to use new type names"
##    -f <filename> converts <filename> to new type names."

usage() 
{
    echo "Usage: $0  -R | -all | -f <filename>"
    echo "" 
    echo "summary: converts files to new type names found in bcs version 3.8.30 and later."
    echo "One and only one argument must be specified"
    echo "   -all          converts all bsv files in current directory to new type names"
    echo "   -R            converts all bsv file in directory tree to use new type names"
    echo "   -f <filename> converts <filename> to new type names."
}

## function to convert one file
convert_file ()
{
    echo "Updating types to bsc-3.8.30 in file: $1" ;
    
    cp $1 $1.orig && \
    sed \
        -es/\\\<isJust\\\>/isValid/g \
        -es/\\\<unJust\\\>/validValue/g \
        -es/\\\<Just\\\>/Valid/g \
        -es/\\\<Nothing\\\>/Invalid/g \
        -es/^import.\*ArrayFile/import\ RegFile/ \
        -es/\\\<mkArrayFile\\\>/mkRegFileLoad/g \
        -es/\\\<mkArrayFullFile\\\>/mkRegFileFullLoad/g \
        -es/\\\<mkArrayWCFFile\\\>/mkRegFileWCFLoad/g \
        -es/\\\<mkArray\\\>/mkRegFile/g \
        -es/\\\<mkArrayFull\\\>/mkRegFileFull/g \
        -es/\\\<mkArrayWCF\\\>/mkRegFileWCF/g \
        -es/\\\<Array\\\>/RegFile/g \
        -es/\\\<toListN\\\>/toVector/g \
        -es/\\\<newListN\\\>/newVector/g \
        -es/\\\<ListN\\\>/Vector/g \
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
patterns="Just|Nothing|isJust|unJust|Array|ListN"

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
