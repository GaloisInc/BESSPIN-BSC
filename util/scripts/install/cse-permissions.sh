#!/bin/bash
# Description: The following script restores the ownership of \texttt{$HOME/.cse} (the CSE plugins for Xilinx Digilent) to the current user.
set -e
set -x
if [ -e $HOME/.cse ]
then
sudo chown -R $USER $HOME/.cse
else
set +x
echo "ERROR: No CSE plugin found in $HOME/.cse ." 1>&2
exit 1
fi
