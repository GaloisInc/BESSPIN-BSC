#! /bin/bash
set -x
if [ -z $DEBUG_NOVERSION ] ; then
VERSION=$1
shift
if [ -z $VERSION ] ; then
cat <<EOF
Usage: $0 VERSION [additional arguments]
Frequently used additional arguments:
  SVNTAG=12345
  SVNURL=https://svn.bluespec.com:8080/bs/bsc/trunk

  BSCTARBALL=/path/to/bsc-svn-co.tar.gz (to avoid SVN checkout)
  NORMAL_TARBALL=/path/to/Bluespec-xxx.tar.gz
  TESTSUITE_TARBALL=/path/to/testsuite-xxx.tar.gz
     (to test an already-built tarball)
EOF
    exit -1
fi
else
# RPM version numbers cannot have dashes, or else rpmbuild fails
VERSION=`date +"%Y.%m.alpha%d.%H%M%S"`
fi

set -e
if [ -z $SARGEBUILD_MK ]
then SARGEBUILD_MK=`dirname $0`/sargebuild.mk
fi

#get sudo privileges now.  because of parallel make,
#we cannot accept console input during the make process.

#Note that /etc/sudoers has:
#Defaults        timestamp_timeout=60
#to increase the length that sudo may be used after getting
#privileges
cat <<EOF
Now getting for sudo privileges for chroots...
EOF
sudo -v

#set SARGEBUILD_THREADS to 1 to avoid parallelization of the
#build process,
#or a number larger than 1 to limit the degree of parallelization.
#Default is the empty string, which results
#in unlimited parallelization -- beware, you need a lot of RAM!

set +e

time make -f $SARGEBUILD_MK \
    -j $SARGEBUILD_THREADS \
    --warn-undefined \
    -k -r VERSION=$VERSION "$@"

if [ -z $DEBUG_NOUMOUNT_BSC ] ; then
#it's dangerous to leave these mounted (an errant but well-meaning
#"rm -fr" might wipe away the actual (non-chroot) /dev or .X11-unix.
cat <<EOF
Now un-mounting /dev and /tmp/.X11-unix in the chroots...
EOF
make -f $SARGEBUILD_MK -k -r VERSION=$VERSION umount > /dev/null 2>&1
else
cat <<EOF
WARNING: NOT UNMOUNTING CHROOT
EOF
fi

set +x
echo "The tarballs are in the build directory (by default /tmp/build-bsc-*)."
echo ""
echo "Do the following if everything is OK:"
echo "  *  svn cp -r $SVNTAG \
https://svn.bluespec.com:8080/bs/bsc/trunk \
https://svn.bluespec.com:8080/bs/bsc/tags/$VERSION \
-m \"Create a release tag for Version $VERSION\""
echo "  *  cp TARBALL and TESTSUITE_TARBALL /raid/releases/Bluespec"
echo "  *  emacs /raid/releases/Bluespec/Release_List.html"
echo "  *  scp -l 500  TARBALL    jbluesp@www.bluespec.com:public_html/downloads"
echo "  *  Do release note"
echo "  *  ./cleanup-build.sh $VERSION"
echo " "
echo "All done."
