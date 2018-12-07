#!/bin/bash
# Description: The following script creates the \texttt{plugdev} group and adds the current user to it.  The \texttt{plugdev} group will have access to the USB device connecting to the FPGA board, as controlled by \texttt{udev} scripts.  After running the following script, log out and log back in for the group membership change to take effect.
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
set -x
set -e
# Do not use a pipe in order to detect if "groups" fails.
temp=`mktemp`
groups >| $temp
if perl -nlwae 'for(@F){exit 0 if $_ eq "plugdev"}exit 1' $temp
then set +x
echo "Already a member of the plugdev group."
exit 0
fi
if perl -nlwa -F: -e 'for($F[0]){exit 1 if $_ eq "plugdev"}' /etc/group
then sudo groupadd plugdev
fi
sudo usermod -a -G plugdev $USER
set +x
echo "User $USER has been added to plugdev."
echo "Please log out and log back in for the changes to take effect."
