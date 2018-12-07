#!/bin/sh
# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

package require BhdlEdit

proc usage {} {
    puts ""
    puts "usage: $::argv0 <options> top_module_name"
    puts "Options: (required -v, optional -p -c -y -o)"
    puts "  -h                  Help"
    puts "  -v    <file>        Verilog file to analyze"
    puts "  -p    <file>        pin file (default: top_module_name.pin)"
    puts "  -c    <file>        project.cfg file"
    puts "  -y    <dir>         Verilog search directory"
    puts "  -o    <dir>         Testbench output directory (default: '.')"
    puts ""
    puts " e.g: -v mkBridge.v -c project.cfg mkBridge"
}

set BoolOptions [list -- -h]
set ValOptions [list -v -y -c -p -o]
set BscPath "+libext+.v -y $::env(BLUESPECDIR)/Verilog -y $::env(BLUESPECDIR)/Libraries -y $::env(BLUESPECDIR)/Libraries/BlueNoC"

if { [catch [list ::utils::scanOptions $BoolOptions $ValOptions false OPT "$argv"] opts] } {
    puts stderr $opts
    usage
    exit 1
}

if {[llength $opts] == 0} {
    puts stderr "A top module name argument is required"
    usage
    exit 1
}

if { [info exists OPT(-h)] } {
    usage
    exit 1
}

if { [info exists OPT(-v)] } {
    set vfile $OPT(-v)
} else {
    usage
    exit 1
}

if { [info exists OPT(-y)] } {
    set vpath $OPT(-y)
}

if { [info exists OPT(-c)] } {
    set cfgfile $OPT(-c)
} else {
    set cfgfile ".none"
}

if { [info exists OPT(-o)] } {
    set outdir $OPT(-o)
} else {
    set outdir "."
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

if { [info exists OPT(-p)] } {
    set pinfile $OPT(-p)
} else {
    set pinfile "$top_mod.pin"
}

if { ![file exists $pinfile] } {
	
    puts "Error: pin file $pinfile not found."
    usage
    exit 0
}

set opts [lreplace $opts $size $size]

eval BhdlEdit::netlist analyze $vfile $opts $BscPath

set filename "$top_mod.pin"

eval BhdlEdit::netlist gen_simtbtemplate $top_mod $cfgfile $pinfile $outdir

exit 1
