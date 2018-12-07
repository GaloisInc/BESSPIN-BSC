#!/bin/bash
# This is a collection of functions imported by prequisite.sh and post-install.sh for checking various things.

function check_rpms_dir {
    if [ -d rpms ]
    then echo "check_rpms_dir:		OK"
    else echo "check_rpms_dir:		FAILED: 'rpms' subdirectory does not exist."
    fi
}

function check_bluespecdir {
    if [ -z "$BLUESPECDIR" ]
    then echo "check_bluespecdir:	FAILED: BLUESPECDIR environment variable not set."
    else echo "check_bluespecdir:	OK"
    fi
}

function check_path {
    if which bsc > /dev/null 2>&1
    then echo "check_path:		OK"
    else echo "check_path:		FAILED: unable to find 'bsc' in PATH."
    fi
}

function check_version {
    if ! [ -r version.dat ]
    then echo "check_version:		FAILED: unable to read 'version.dat'.  Are you in the 'install' directory?"
         return 1
    fi
    if diff -u version.dat $BLUESPECDIR/../install/bluenoc/version.dat
    then echo "check_version:		OK"
    else echo "check_version:		FAILED: BLUESPECDIR does not point to current version."
    fi
}

function check_ldpath {
    perl -le 'for($ENV{LD_LIBRARY_PATH}){s/(.*?):.*/$1/; exit 2 if m(/ISE/)}'
    return_value=$?
    if [ $return_value == 0 ]
    then echo "check_ldpath:		OK"
    elif [ $return_value == 2 ]
    then echo "check_ldpath:		FAILED: Xilinx directories found as first entry of LD_LIBRARY_PATH."
    else echo "check_ldpath:		FAILED: checking the value of LD_LIBRARY_PATH failed."
    fi
}

function check_cse {
    if [ "$XIL_CSE_PLUGIN_DIR" = $HOME/.cse ]
    then echo "check_cse:		OK"
    else echo "check_cse:		FAILED: XIL_CSE_PLUGIN_DIR not set to '$HOME/.cse'."
    fi
}

function check_xilinx {
    if [ -z "$XILINX" ]
    then echo "check_xilinx:		FAILED: XILINX environment variable not set."
    else echo "check_xilinx:		OK"
    fi
}

function check_impact {
    if which impact > /dev/null 2>&1
    then echo "check_impact:		OK"
    else echo "check_impact:		FAILED: unable to find 'impact' in PATH."
    fi
}

function check_xilinx_license {
    if [ -z "$LM_LICENSE_FILE" ]
    then echo "check_xilinx_license:	FAILED: LM_LICENSE_FILE not set."
    else echo "check_xilinx_license:	OK"
    fi
}

function check_bluespec_license {
    if [ -z "$LM_LICENSE_FILE" -a -z "$BLUESPEC_LICENSE_FILE" ]
    then echo "check_bluespec_license:	FAILED: neither LM_LICENSE_FILE nor BLUESPEC_LICENSE_FILE are set."
    else echo "check_bluespec_license:	OK"
    fi
}

function check_sudo {
    echo "check_sudo: 		checking if you have sudo privileges..."
    if sudo -v
    then echo "check_sudo:		OK"
    else echo "check_sudo:		FAILED: You will need 'sudo' to perform the installation."
    fi
}

## postrequisites

function check_libusb {
    check_LIBUSB=/usr/local/lib/libusb-driver.so
    if [ -e $check_LIBUSB ]
    then echo "check_libusb:		OK"
    else echo "check_libusb:		FAILED: cannot find '$check_LIBUSB'."
    fi
}

function check_plugdev {
    # Do not use a pipe in order to detect if "groups" fails.
    temp=`mktemp`
    groups >| $temp
    if perl -nlwae 'for(@F){exit 0 if $_ eq "plugdev"}exit 1' $temp
    then echo "check_plugdev:		OK"
    else echo "check_plugdev:		FAILED: not a member of the 'plugdev' group."
    fi
}

function check_driverrc {
    if diff -u data/libusb-driverrc $HOME/.libusb-driverrc
    then echo "check_driverrc:		OK"
    else echo "check_driverrc:		FAILED: problem with file '$HOME/.libusb-driverrc'."
    fi
}

function check_dkms {
    output=`mktemp`
    if ! /usr/sbin/dkms status > $output
    then echo "check_dkms:		FAILED: running 'dkms status' failed."
         return 1
    fi

    if perl dkms-check.pl $output
    then echo "check_dkms:		OK"
    else echo "check_dkms:		FAILED: DKMS installation of BlueNoC did not succeed.  See 'dkms status'."
    fi
}

function check_cse_perm {
    if ! [ -e $HOME/.cse ]
    then echo "check_cse_perm:		FAILED: No CSE plugin found in '$HOME/.cse'."
         return 1
    fi

    if ! [ -w $HOME/.cse ]
    then echo "check_cse_perm:		FAILED: Incorrect permissions or ownership on '$HOME/.cse'."
         return 1
    fi
    echo "check_cse_perm:		OK"
}

function check_rpms {
    if ! OUT=`mktemp -d`
    then echo "check_rpms:		FAILED: unable to execute 'mktemp'."
         return 1
    fi
    perl -lwe 'for(sort <rpms/*.rpm>){s(^rpms/)();s/\.rpm$//;print}' > $OUT/expected
    rpm -q -a bluespec-bluenoc bluespec-external-dependencies bluespec-redhat-dependencies | sort > $OUT/actual
    if diff -u $OUT/expected $OUT/actual
    then echo "check_rpms:		OK"
    else echo "check_rpms:		FAILED: Bluespec RPMs are not installed correctly."
    fi
}

function check_kernel {
    output=`mktemp`
    yum check-update >| $output
    return_value=$?
    if [ $return_value == 0 ]
    then echo "check_kernel: 		OK"
         return 0
    elif [ $return_value == 1 ]
    then echo "check_kernel:		FAILED: An error occurred during 'yum check-update'."
         return 1
    elif [ $return_value == 100 ]
    then if grep '^kernel.x86_64 ' $output
         then echo "check_kernel:		FAILED: Not running newest kernel."
         else echo "check_kernel:		OK"
         fi
    else echo "check_kernel:		FAILED: Unknown return value from 'yum check-update'."
    fi
}

function check_blacklist_usb {
    blacklist_file=/etc/modprobe.d/blacklist.conf
    if grep -q '^blacklist  *usbserial$' $blacklist_file
    then echo "check_blacklist_usb:	OK"
    else echo "check_blacklist_usb:	FAILED: 'usbserial' not blacklisted in '$blacklist_file'."
    fi
}

function check_blacklist_ftdi {
    blacklist_file=/etc/modprobe.d/blacklist.conf
    if grep -q '^blacklist  *ftdi_sio$' $blacklist_file
    then echo "check_blacklist_ftdi:	OK"
    else echo "check_blacklist_ftdi:	FAILED: 'ftdi_sio' not blacklisted in '$blacklist_file'."
    fi
}

function check_dmesg {
    if dmesg | grep -q '^bluenoc: Registered Bluespec BlueNoC driver'
    then echo "check_dmesg:		OK"
    else echo "check_dmesg:		FAILED: No BlueNoC driver found in 'dmesg'."
    fi
}

function check_lspci {
    if /sbin/lspci | grep -q 'Device 1be7:b100'
    then echo "check_lspci:		OK"
    else echo "check_lspci:		FAILED: Unable to find 'Device 1be7:b100' in lspci."
    fi
}

function check_lsmod {
    if /sbin/lsmod | grep -q '^bluenoc '
    then echo "check_lsmod:		OK"
    else echo "check_lsmod:		FAILED: Unable to find 'bluenoc' in 'lsmod'."
    fi
}
