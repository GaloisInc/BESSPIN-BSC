#!/bin/bash
# Description: The following script checks that the BlueNoC kernel module installed correctly.  If it fails, try \texttt{sudo yum erase bluespec-bluenoc}, then run the previous step again.  Be sure you are have rebooted into the latest kernel that was installed in the first step.
set -x

output=`mktemp`
if ! /usr/sbin/dkms status > $output
then set +x
echo "ERROR: 'dkms status' fails with error.  Aborting." 1>&2
exit 1
fi

if perl dkms-check.pl $output
then set +x
echo "DKMS installation of BlueNoC looks good."
else set +x
echo "ERROR: DKMS installation of BlueNoC did not succeed.  See 'dkms status'." 1>&2
exit 1
fi
