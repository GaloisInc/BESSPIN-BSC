#! /bin/bash
# this is a script for testing a set of packages to see if "rpm" will unpack them smoothly.
set -e
set -x
CHROOT=/tmp/kenredhat
mkdir $CHROOT
mkdir $CHROOT/dev
#sudo mount --bind /dev /tmp/kenredhat/dev
rpm --root $CHROOT --import RPM-GPG-KEY
cd RPMS
for dir in *
do if [ -e ../flags/$dir ]
then FLAGS=`cat ../flags/$dir`
else FLAGS=
fi
sudo rpm -Uvh --root $CHROOT $FLAGS $dir/*rpm
done
sudo umount /tmp/kenredhat/dev
