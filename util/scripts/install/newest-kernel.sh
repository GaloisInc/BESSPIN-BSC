#!/bin/bash
# Description: The following script ensures that the newest kernel is installed.  The BlueNoC driver will be installed in a later step using the Dynamic Kernel Module Support (DKMS) framework to maintain that the driver binary matches the current and and future installed kernels.  DKMS requires the \texttt{kernel-devel} package version to match the \texttt{kernel} package version.  A reboot will be required if a new kernel gets installed.
set -x
output=`mktemp`

# note cannot use "set -e" here
yum check-update >| $output
return_value=$?
set -e
if [ $return_value == 0 ]
then set +x
     echo "You are already running the latest kernel.  Done."
     exit 0
elif [ $return_value == 1 ]
then set +x
     echo "ERROR: An error occurred during \"yum check-update\".  Aborting." 1>&2
     exit 1
elif [ $return_value == 100 ]
then if grep '^kernel.x86_64 ' $output
     then sudo yum install -y kernel kernel-devel kernel-headers
          set +x
          echo "Press ENTER to reboot into the new kernel."
          read
          set -x
          sudo reboot
     else set +x
          echo "You are already running the latest kernel.  Done."
          exit 0
     fi
else echo "ERROR: Unknown return value from \"yum check-update\".  Aborting." 1>&2
fi
