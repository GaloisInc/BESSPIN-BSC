#!/bin/bash
# Description: The following script creates a \texttt{.libusb-driverrc} file in the user's home directory.
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

set -e
set -x

ORIGINAL=data/libusb-driverrc
TARGET=$HOME/.libusb-driverrc
if [ -e $TARGET ]
then      if ! diff -u $ORIGINAL $TARGET
     then set +x
          echo "ERROR: $TARGET already exists with different information than expected.  Aborting." 1>&2
          exit 1
     else set +x
          echo "$TARGET already exists with the correct information.  Done."
          exit 0
     fi
else cp $ORIGINAL $TARGET
     set +x
     echo "Wrote file $TARGET with contents:"
     cat $TARGET
fi
