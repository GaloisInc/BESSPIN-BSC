#!/bin/sh

case $ACTION in
    add)
	/bin/chown root.staff /sys$DEVPATH/resource[0124]
	/bin/chmod 0666 /sys$DEVPATH/resource[0124]
	/bin/chown root.staff /sys$DEVPATH/config
	/bin/chmod 0666 /sys$DEVPATH/config
	;;
esac

exit 0
