#!/bin/bash
# Description: The following script checks that the user has successfully been added to the \texttt{plugdev} group.
set -x
set -e

# Do not use a pipe in order to detect if "groups" fails.
temp=`mktemp`
groups >| $temp

if perl -nlwae 'for(@F){exit 0 if $_ eq "plugdev"}exit 1' $temp
then set +x
echo "Membership of user '$USER' in 'plugdev' group looks good."
else
set +x
echo "ERROR: '$USER' is not a member of the 'plugdev' group!" 1>&2
echo "Did you logout and log back in?" 1>&2
exit 1
fi
