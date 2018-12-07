#!/bin/bash
# Description: The following script downloads, compiles, and installs into \texttt{/usr/local/lib} the libusb driver shared library that Xilinx \texttt{impact} needs via LD_PRELOAD.  The script is dependent on the website being available.  If not, retry.

# USB Driver from http://www.rmdir.de/~michael/xilinx/

set -e
TARGET_DIR=/usr/local/lib
NAME=libusb-driver.so
if [ -e $TARGET_DIR/$NAME ]
then echo "$TARGET_DIR/$NAME already exists.  Aborting." 1>&2
exit 1
fi

if [ -z "$XILINX" ]
then echo "ERROR: XILINX variable not set to Xilinx installation location.  Please source the Xilinx's settings script for your architecture and shell.  Aborting." 1>&2
exit 1
fi

set -x
D=`mktemp -d`
cd $D
curl 'http://git.zerfleddert.de/cgi-bin/gitweb.cgi/usb-driver?a=snapshot;h=HEAD;sf=tgz' | tar xzvf -
cd usb-driver-HEAD*
make
sudo install -m 755 libusb-driver.so $TARGET_DIR
sudo ./setup_pcusb $XILINX
