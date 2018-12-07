#!/bin/sh
# Copyright 2009--2010 Bluespec, Inc.  All rights reserved.
#
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

# top-level user script to invoke hdl editing commands
package require Bluetcl
package require RtlEdit


set boolOptions [list -- --gui --batch  -usage --usage --partition -help --help -h]
set valOptions  [list]

# Option scanning
if { [catch [list ::utils::scanOptions $boolOptions $valOptions false OPT "$argv"] opts] } {
    puts stderr "Error: $opts"
    puts [RtlEdit::usage]
    exit 1
}

if { [info exists OPT(-usage)] || [info exists OPT(--usage)] || [info exists OPT(-help)] || [info exists OPT(--help)] || [info exists OPT(-h)] } {
    puts [RtlEdit::usage]
    exit
}


# option checking
if { ![info exists OPT(--batch)] } { set OPT(--gui) --gui }

# invokation
if { [info exists OPT(--gui)] } {
    eval ::RtlEdit::guiStart $argv
    exit
} else {
    if { [catch "::RtlEdit::scriptEntry $argv" err] } {
        puts stderr $err
        exit 1
    }
    exit 0
}
