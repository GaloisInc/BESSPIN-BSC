#!/bin/bash
set -x

# First difference will cause "make" to abort with an error.
set -e

if [ -z "$1" ]
then set +x
echo Need to specify build dir.  Aborting.
exit 1
fi
normal=`find "$1" -maxdepth 1 -name 'Bluespec-*.tar.gz' -not -name 'Bluespec-*-DEBUG.tar.gz' -not -name 'Bluespec-*-PROF.tar.gz'`
debug=`find "$1" -maxdepth 1 -name 'Bluespec-*-DEBUG.tar.gz'`
D=$1/build/compare-files
mkdir -p $D
tar tf "$normal" | sort | perl -plwe 's,^Bluespec-.*?/,,' > $D/normal
tar tf "$debug" | sort | perl -plwe 's,^Bluespec-.*?/,,' > $D/debug
diff -u $D/normal $D/debug

prof=`find "$1" -maxdepth 1 -name 'Bluespec-*-PROF.tar.gz'`
tar tf "$prof" | sort | perl -plwe 's,^Bluespec-.*?/,,' > $D/prof
diff -u $D/normal $D/prof
