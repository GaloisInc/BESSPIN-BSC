#!/bin/bash
# Description: The following script blacklists the \texttt{usbserial} device.  This step is not required on for some computers.
set -x
set -e
if ! grep -q '^blacklist  *usbserial$' /etc/modprobe.d/blacklist.conf
then
sudo bash -c 'echo "blacklist usbserial" >> /etc/modprobe.d/blacklist.conf'
else
set +x
echo "usbserial is already blacklisted in /etc/modprobe.d/blacklist.conf."
fi
