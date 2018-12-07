#!/bin/bash
#Delete the chroots created with make-chroots.sh
set -x
if [ -z "$1" ]
then echo "need target"
exit 1
fi
TARGET=$1

#Nowadays we do not mount /raid in the sarge chroot at all, so this
#just a precaution.  We would not want to sudo rm -fr FOO, where FOO
#has /raid mounted, which would delete the entirety of the raid

#done twice each in case it had been double mounted
sudo umount $TARGET/32/raid
sudo umount $TARGET/64/raid
sudo umount $TARGET/32/raid
sudo umount $TARGET/64/raid

#on the assumption that target will usually by /tmp something
mount | grep /tmp
mount | grep raid
sleep 20
sudo rm -fr $TARGET
