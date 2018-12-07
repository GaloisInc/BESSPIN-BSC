#!/bin/bash
set -x
set -e
set -o pipefail
if [ -z "$1" ]
then exit 1
fi
dir=temp-extract
mkdir $dir

p=etc/pki/rpm-gpg/RPM-GPG-KEY-atrpms
# cannot do this all in one pipeline because rpm2cpio
# expects to slurp the whole file at once, probably a bug.
wget -O $dir/at.rpm "$1"
rpm2cpio $dir/at.rpm | ( cd $dir && cpio -idv ./$p )
cmp $dir/$p keys/RPM-GPG-KEY.atrpms 1>&2
