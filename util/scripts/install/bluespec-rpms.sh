#!/bin/bash
# Description: The following script installs the Bluespec BlueNoC kernel module and the other packages upon which Bluespec tools depend.  These RPMs are designed for Redhat Enterprise Linux 6.  Some packages pulled in as dependencies will come from external RPM repositories Atrpms and EPEL.  The BlueNoC kernel module is installed using DKMS, which requires that the machine be running the latest kernel installed in step 1.  If the BlueNoC RPM does not work, (e.g., you are on an operating system other than Redhat Enterprise Linux 6) please see \\ $BLUESPECDIR/board_support/bluenoc/drivers for source code for a manual installation of the BlueNoC kernel module.
set -x
set -e

REDHAT_RELEASE=/etc/redhat-release

if grep -i "release 6" $REDHAT_RELEASE
then release=6
elif grep -i "release 5" $REDHAT_RELEASE
then release=5
else set +x
echo "ERROR: Unable to determine Redhat release version in $REDHAT_RELEASE.  Aborting." 1>&2
exit 1
fi

# check if the external repository RPMs are already enabled
rpmq=`mktemp`
rpm -q -a >| $rpmq

for REPO in atrpms-repo epel-release
do if ! grep '^'$REPO $rpmq
   then sudo yum install --nogpgcheck -y repository-rpms/$release/${REPO}-*.rpm
   fi
done

# check if the external repository RPMs need updating
check_update=`mktemp`
set +e
yum check-update >| $check_update
set -e
return_value=$?
if [ return_value == 1 ]
then echo "ERROR: An error occurred during \"yum check-update\".  Aborting." 1>&2
     exit 1
fi
for REPO in atrpms-repo epel-release
do if grep '^'$REPO $check_update
   then sudo yum update -y $REPO
   fi
done

# Install Bluespec and its dependencies
sudo yum install --nogpgcheck -y rpms/*.rpm
