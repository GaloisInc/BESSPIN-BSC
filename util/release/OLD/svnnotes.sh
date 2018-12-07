#!/bin/sh
## pull out the svn log notes from the $1 to $2

SVNURL=https://svn.bluespec.com:8080/bs/bsc/trunk


if [ -z $1 ] ; then
    echo "Usage: $0 revNum [revNum]"
    echo "E.g., $0 4145 4072"
    exit -1 
fi
V1=$1
shift 1

if [ -z $1 ] ; then
    V2=head
else
    V2=$1
fi

svn log -v -r $V1:$V2 $SVNURL
