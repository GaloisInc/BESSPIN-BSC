#!/bin/bash
# Description: The following script blacklists ftdi_sio.  This step is not required for some computers.

set -e
set -x
if ! grep -q '^blacklist  *ftdi_sio$' /etc/modprobe.d/blacklist.conf
then
sudo bash -c 'echo "blacklist ftdi_sio" >> /etc/modprobe.d/blacklist.conf'
else
set +x
echo "ftdi_sio is already blacklisted in /etc/modprobe.d/blacklist.conf.  Done."
fi
