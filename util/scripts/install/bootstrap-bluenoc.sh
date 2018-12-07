#!/bin/bash
# Description: The following script loads the BlueNoC driver endpoint onto the FPGA so that the BlueNoC kernel driver can see it and properly initialize.  Specify the board model, e.g., \texttt{10gk7ll} or \texttt{kc705} or  \texttt{ml605} or \texttt{vc707}, as the argument to the script.  Prior to running this script, ensure that your board is installed and both PCIe and USB are connected.
set -e
if [ -z "$1" ]
then echo "ERROR: Specify the board model, e.g., 10gk7ll, as the argument to the script." 1>&2
echo "Available boards are:" 1>&2
ls bootstrap 1>&2
exit 1
fi

BOARD=$1
shift
#cannot use $1 because xilinx settings uses $1 in some weird way.
if [ ! -z "$1" ]
then echo "ERROR: Too many arguments to script.  Aborting." 1>&2
exit 1
fi

set -x

cd bootstrap
if [ -d $BOARD ]
then
# run "impact" in a temporary directory because it will fail if the current directory is read-only.
DIR=`mktemp -d`
cp -r $BOARD $DIR
pushd $DIR/$BOARD
chmod -R u+w .
else
set +x
echo "ERROR: Unsupported board $BOARD.  Supported boards are" 1>&2
ls 1>&2
exit 1
fi

if [ -z "$XIL_CSE_PLUGIN_DIR" ]
then set +x
echo "Warning: XIL_CSE_PLUGIN_DIR not set.  The CSE plugin is required for some boards, e.g., 10gk7ll.  Continuing."
set -x
fi

if [ ! -e ${USB_DRIVER_FOR_BLUESPEC:=/usr/local/lib/libusb-driver.so} ]
then
  set +x
  echo "ERROR: libusb-driver.so at $USB_DRIVER_FOR_BLUESPEC not found." 1>&2
  exit 1
fi

LD_PRELOAD=$USB_DRIVER_FOR_BLUESPEC impact -batch $BOARD.program.cmd
# If "impact" is not found, you need to source Xilinx's settings script for your architecture and shell.
popd

