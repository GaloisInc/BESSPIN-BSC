#!/bin/bash
# Description: The following script checks that the prerequisites for BlueNoC installation have been met.

check_functions_library=check-functions.sh
if ! [ -r $check_functions_library ]
then echo "Error: Failed to open '$check_functions_library'."
exit 1
fi

. $check_functions_library

check_rpms_dir
check_bluespecdir
check_path
check_version
check_ldpath
check_cse
check_xilinx
check_impact
check_xilinx_license
check_bluespec_license
check_sudo

#done
