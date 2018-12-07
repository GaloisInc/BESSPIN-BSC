#!/bin/bash
#this should NOT be run with sudo
#clean up after a "beta" build, meaning no RedHat testing chroots,
#just some compilation directories in /tmp and the sarge chroot /tmp

if [ -z "$1" ]
then echo need full version directory, e.g., /tmp/build-bsc-2013.01.01
fi
if [ -z "$CHROOT_BASE" ]
then CHROOT_BASE=/tmp/noraid
fi
set -x
mount | grep /tmp | grep /dev
sleep 10
#this is should fail if there are redhat chroots
rm -fr "$1"
echo ${CHROOT_BASE}/32$1 ${CHROOT_BASE}/64$1
sleep 10
sudo -u tinderbox rm -fr ${CHROOT_BASE}/32$1
sudo -u tinderbox rm -fr ${CHROOT_BASE}/64$1
