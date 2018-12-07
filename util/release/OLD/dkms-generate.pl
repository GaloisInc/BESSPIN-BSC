#!perl -w
die unless defined($RPM_NAME=$ARGV[0]);
die unless defined($VERSION=$ARGV[1]);
print << "EOF";
PACKAGE_VERSION=$VERSION
# Items below here should not have to change with each driver version
PACKAGE_NAME="$RPM_NAME"
EOF
    ;
print << 'EOF';
MAKE[0]="make -C ${kernel_source_dir} SUBDIRS=${dkms_tree}/${PACKAGE_NAME}/${PACKAGE_VERSION}/build modules"
CLEAN="make -C ${kernel_source_dir} SUBDIRS=${dkms_tree}/${PACKAGE_NAME}/${PACKAGE_VERSION}/build clean"

BUILT_MODULE_NAME[0]="bluenoc"
DEST_MODULE_LOCATION[0]="/kernel/bluespec"
EOF
    ;
