#!/bin/bash
set -x
set -e
if [ -z "$1" ]
then echo "need target"
exit 1
fi
BASETARGET="$1"
mkdir -p $BASETARGET
CHROOT_UID=1061
CHROOT_USERNAME=tinderbox

for bit in 32 64
do
TARGET=$BASETARGET/$bit
LINUXWRAP=linux$bit

if [ -e "$TARGET" ]
then echo TARGET $TARGET already exists.  Aborting.
exit 1
fi

if [ $bit = 32 ]
then arch=i386
url=http://packages:9999/archive
elif [ $bit = 64 ]
then arch=amd64
url=http://packages:9999/sarge64-archive
else
echo Something went wrong with BIT
fi

sudo debootstrap --arch="$arch" sarge "$TARGET" "$url"
for GHC_VERSION in 6.12.3 7.4.2
do tar xjf /raid/tools/ghc/tarballs/ghc-$GHC_VERSION-sarge$bit-tmp-install.tar.bz2 -C $TARGET
done
ln -s 6.12.3a $TARGET/tmp/6.12.3

sudo bash -c "echo deb http://packages:9999/backports-archive sarge-backports main contrib non-free >> ""$TARGET""/etc/apt/sources.list"
LANG=C $LINUXWRAP sudo chroot "$TARGET" apt-get update
LANG=C $LINUXWRAP sudo chroot "$TARGET" apt-get -t sarge-backports install -y bash findutils make patch subversion
LANG=C $LINUXWRAP sudo chroot "$TARGET" apt-get install -y bison bzip2 flex libfontconfig1-dev less gperf libx11-dev libxft-dev perl python2.3 tcl8.4 tcsh verilog libgmp3 libgmp3-dev libncurses5-dev libreadline5 libreadline5-dev g++-3.4 gcc-3.4 sudo
sudo ln -s ../../bin/gcc-3.4 "$TARGET"/usr/local/bin/gcc
sudo ln -s ../../bin/g++-3.4 "$TARGET"/usr/local/bin/g++
sudo ln -s gcc "$TARGET"/usr/local/bin/cc
sudo ln -s g++ "$TARGET"/usr/local/bin/c++

# It used to be that ghc (the one in /raid/tools/ghc/6.12.3/x*_sarge)
# has /usr/bin/gcc hardcoded, but now we use a tarball-version in /tmp
# that has /usr/local/bin hardcoded.

# sudo ln -s gcc-3.4 "$TARGET"/usr/bin/gcc

# Note well that the arguments to useradd change after sarge
LANG=C $LINUXWRAP sudo chroot "$TARGET" useradd -m -u $CHROOT_UID $CHROOT_USERNAME

#verific needs subversion in the middle of a compile
#this includes kens subversion password
sudo cat /raid/home/ken/centos/dot-subversion-for-release.tar.gz | LANG=C $LINUXWRAP sudo chroot "$TARGET" sudo -u $CHROOT_USERNAME -H bash -c 'tar xzf - -C $HOME'

done
