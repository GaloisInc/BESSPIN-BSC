#!/bin/sh



# NOTE: THIS FILE IS NO LONGER IN USE.  IT REQUIRES VERIFIC.  THE INSTALLER
# NOW INSTALLS listFiles.tcl INSTEAD UNDER THIS NAME.




# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

package require BhdlEdit

proc usage {} {
    puts ""
    puts "usage: $::argv0 <options> top_package_name"
    puts "Options:"
    puts "  -h                  Help"
    puts "  -v    <file>        Verilog file to analyze"
    puts "  -y    <dir>         Verilog search directory"
    puts ""
    puts " e.g: -v mkBridge.v -ydir $::env(BLUESPECDIR)/Verilog"
}

set BoolOptions [list -- -h]
set ValOptions [list -v]
set BscPath "+libext+.v -y $::env(BLUESPECDIR)/Verilog -y $::env(BLUESPECDIR)/Libraries -y $::env(BLUESPECDIR)/Libraries/BlueNoC"

if { [catch [list ::utils::scanOptions $BoolOptions $ValOptions false OPT "$argv"] opts] } {
    puts stderr $opts
    usage
    exit 1
}

if {[llength $opts] == 0} {
    puts stderr "A top package name argument is required"
    usage
    exit 1
}

if { [info exists OPT(-v)] } {
    set vfile $OPT(-v)
}

set slash [string last "/" $vfile]
if { $slash == -1 } {
    set BscPath "-y . $BscPath"
} else {
    set dir [string range $vfile 0 $slash]
    set BscPath "-y $dir $BscPath"
}

set size [llength $opts]-1
set top_mod [lindex $opts $size]
set opts [lreplace $opts $size $size]

eval BhdlEdit::netlist analyze $vfile $opts $BscPath


puts "# List of verilog files"

set files [BhdlEdit::netlist getfiles $top_mod]

foreach file $files {
    puts $file
}

exit
