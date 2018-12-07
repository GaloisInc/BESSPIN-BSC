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

for bit in 64 32
do
TARGET=$BASETARGET/$bit
LINUXWRAP=linux$bit

if [ -e "$TARGET" ]
then echo TARGET $TARGET already exists.  Aborting.
exit 1
fi

url=http://packages:9999/archive
if [ $bit = 32 ]
then arch=i386
elif [ $bit = 64 ]
then arch=amd64
else
echo Something went wrong with BIT
fi

sudo debootstrap --arch="$arch" etch "$TARGET" "$url"
for GHC_VERSION in 6.12.3
do tar xjf /raid/tools/ghc/tarballs/ghc-$GHC_VERSION-etch$bit-tmp-install-with-cabal-prof.tar.bz2 -C $TARGET
done

LANG=C $LINUXWRAP sudo chroot "$TARGET" apt-get update
LANG=C $LINUXWRAP sudo chroot "$TARGET" apt-get install -y make patch subversion bison bzip2 flex libfontconfig1-dev less gperf libx11-dev libxft-dev perl python2.5 tcl8.4 tcsh verilog libgmp3c2 libgmp3-dev libncurses5-dev libreadline5 libreadline5-dev g++ gcc sudo libusb-dev

# It used to be that ghc (the one in /raid/tools/ghc/6.12.3/x*_sarge)
# has /usr/bin/gcc hardcoded, but now we use a tarball-version in /tmp
# that has /usr/local/bin hardcoded.

# sudo ln -s gcc-3.4 "$TARGET"/usr/bin/gcc

LANG=C $LINUXWRAP sudo chroot "$TARGET" useradd -m -u $CHROOT_UID $CHROOT_USERNAME

#verific needs subversion in the middle of a compile
#this includes kens subversion password
sudo cat /raid/home/ken/centos/dot-subversion-for-release.tar.gz | LANG=C $LINUXWRAP sudo chroot "$TARGET" sudo -u $CHROOT_USERNAME -H bash -c 'tar xzf - -C $HOME'

done
