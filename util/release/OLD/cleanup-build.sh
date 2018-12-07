#! /bin/bash
VERSION=${1##/tmp/build-bsc-}
VERSION=${VERSION%/}
if [ -z $VERSION ] ; then
    echo "Usage: $0 VERSION"
    echo ""
    echo "cleans up the various build directories, and umounts the chroots"
    exit -1
fi
shift

set -x
if [ -z $SARGEBUILD_MK ]
then SARGEBUILD_MK=`dirname $0`/sargebuild.mk
fi

make -f $SARGEBUILD_MK -r VERSION=$VERSION clean
