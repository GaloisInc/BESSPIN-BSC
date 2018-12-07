#!/bin/bash
# Description: The following script reboots the computer so that the computer can detect the PCIe endpoint on the FPGA.
set -e
echo Press ENTER to reboot.
read
set -x
sudo reboot
