#!/bin/bash
# Description: The following script installs the Xilinx Digilent driver for communicating over USB.  During installation, it will ask for locations to install various things.  Generally use the defaults, but when asked, ``\texttt{In which directory should plugins be installed?}'' the CSE plugin should be installed in \{your home directory path\}\texttt{/.cse}, not \texttt{/root/.cse} which is the given default.  You must type the full path to your home directory; you may not use a tilde shortcut.

if [[ "$EUID" -eq 0 ]]
then echo "This script is generally not run as root."
echo  "Type \"yes\" if you really want to run as root."
echo "Really run as root (yes/no)?"
read answer
if [ "$answer" != "yes" ]
then
echo "Aborted."
exit 1
fi
fi

set -e

if [ -z "$XILINX" ]
then echo "XILINX variable not set to Xilinx installation location.  Please source the Xilinx's settings script for your architecture and shell." 1>&2
exit 1
fi
if [ -e $HOME/.cse ]
then echo "ERROR: Digilent CSE plugin already exists at $HOME/.cse ." 1>&2
echo "Please remove it first." 1>&2
exit 1
fi

uname=`uname -m`
if [ "$uname" = x86_64 ]
then archdir=lin64
elif [ "$uname" = i686 ]
then archdir=lin
else echo "ERROR: Unable to recognize machine architecture: $uname." 1>&2
exit 1
fi

set -x
cd $XILINX
cd ..
cd common
cd bin
cd $archdir
cd digilent
set +x
echo ""
echo "*** When asked:"
echo "In which directory should plugins be installed? [/root/.cse]"
echo "*** You must type:"
echo $HOME/.cse
echo "*** Please type the full path as above; do NOT use a tilde (~) shortcut. Press ENTER to begin Digilent installation."
read
set -x
sudo ./install_digilent.sh
