#!/bin/bash
# Description: The following script checks that BlueNoC has been installed correctly.

check_functions_library=check-functions.sh
if ! [ -r $check_functions_library ]
then echo "Error: Failed to open '$check_functions_library'."
exit 1
fi

. $check_functions_library


check_bluespecdir
check_path
check_ldpath
check_cse
check_xilinx
check_impact
check_xilinx_license
check_bluespec_license

check_libusb
check_plugdev
check_driverrc
check_dkms
check_cse_perm
#check_digilent
check_rpms
check_kernel
check_blacklist_usb
check_blacklist_ftdi
check_dmesg
check_lspci
check_lsmod

#done
