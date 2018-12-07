#!/bin/bash
# Description: The following script reboots the computer so that the changes to \texttt{/etc} can take effect.
set -e
echo Press ENTER to reboot.
read
set -x
sudo reboot

