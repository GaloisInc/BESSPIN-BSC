#!/bin/sh
### -*- Tcl -*- ##############################################################
###
### (c) Copyright 2004 - 2008, Bluespec Incorporated and Donald G. Baltus.
###
##############################################################################
# \
exec wish "$0" -- "$@" 


global env

lappend auto_path $env(BLUESPECDIR)/Blueview/lib


package require blueview

################################################################################

init $argc $argv

Window show .
Window show .top

#puts stdout [format "ARGC %s.\n" $argc]
#puts stdout [format "ARGV %s.\n" $argv]

main $argc $argv

################################################################################


