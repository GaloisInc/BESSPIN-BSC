#!/bin/sh
# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

package require BhdlEdit

proc usage {} {
    puts ""
    puts "usage: $::argv0 <options> top_module_name"
    puts "Options:"
    puts "  -h                  Help"
    puts "  -v    <file>        Verilog file to analyze"
    puts "  -c    <file>        project.cfg file"
    puts "  -p    <file>        pin file"
    puts "  -y    <dir>         Verilog search directory"
    puts "  -o    <dir>         Testbench output directory"
    puts "  -a    <dir>         Optional c-api output directory"
    puts ""
    puts " e.g: -v mkBridge.v -ydir $::env(BLUESPECDIR)/Verilog"
}

set BoolOptions [list -- -h]
set ValOptions [list -v -y -c -p -o -a]
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
}

if { [info exists OPT(-v)] } {
    set vfile $OPT(-v)
}

if { [info exists OPT(-y)] } {
    set vpath $OPT(-y)
}

if { [info exists OPT(-c)] } {
    set cfgfile $OPT(-c)
} else {
    set cfgfile "project.cfg"
}

if { [info exists OPT(-o)] } {
    set outdir $OPT(-o)
} else {
    set outdir "."
}

if { [info exists OPT(-a)] } {
    set capi_outdir $OPT(-a)
} else {
    set capi_outdir ".none"
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

if { [info exists OPT(-p] } {
    set pinfile $OPT(-p)
} else {
    set pinfile "$top_mod.pin"
}

set opts [lreplace $opts $size $size]

eval BhdlEdit::netlist analyze $vfile $opts $BscPath

set filename "$top_mod.pin"

eval BhdlEdit::netlist gen_tbtemplate $top_mod $cfgfile $pinfile $outdir $capi_outdir

exit 1
