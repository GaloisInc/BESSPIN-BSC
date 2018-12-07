#!/bin/sh
# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

package require utils
package require SceMiParams

proc usage {} {
    puts "" 
    puts "usage: $::argv0 <options> params_file"
    puts "Options:"
    puts "  -package str       Top package name"
    puts "  -p <path>          Bluespec search path"
    puts "  -bdir <dir>        Bluespec bdir path"
    puts "  -outdir <dir>      Directory for generated files"
    puts "  -memberPrefix str  Prefix for class member names; default: \"m_\""
    puts "  -enumPrefix str    Prefix for enum type; default: \"e_\""
    puts "  -inputs            Generate header files for all input types"
    puts "  -outputs           Generate header files for all output types"
    puts "  -probes            Generate header files for all probe types"
    puts "  -probe-code        Generate an include files which adds all probes"
    puts "  -all               Generate all header/include files"
    puts "  -aliases           Generate header files for any relevant aliases as well"
    puts "  -vcd <path>        VCD file name for probes"
    puts "" 
    puts " e.g: -p bdir:+ foo.params"
}

set boolOptions [list -- -inputs -outputs -probes -probe-code -all -aliases -scemi-classic]
set valOptions [list -package -p -bdir -outdir -memberPrefix -enumPrefix -vcd]

set OPT(-outdir) ""
set OPT(-vcd) probe_dump.vcd
if { [catch [list ::utils::scanOptions $boolOptions $valOptions true OPT "$argv"] opts] } {
    puts stderr $opts
    usage
    exit 1
}

if {[llength $opts] == 0} {
    puts stderr "A params file argument is required"
    usage
    exit 1
}

if {[llength $opts] > 1} {
    puts stderr "Only a single SceMi params file may be specified"
    usage
    exit 1
}

if { [info exists OPT(-p)] } {
    Bluetcl::flags set -p $OPT(-p)
}
if { [info exists OPT(-bdir)] } {
    Bluetcl::flags set -bdir $OPT(-bdir)
}
if { [info exists OPT(-scemi-classic)] } {
    Bluetcl::flags set -scemi-classic
}

if { [info exists OPT(-all)] } {
	set OPT(-inputs) 1
	set OPT(-outputs) 1
	set OPT(-probe-code) 1

}

if {![info exists OPT(-inputs)] && ![info exists OPT(-outputs)] && ![info exists OPT(-probes)] && ![info exists OPT(-probe-code)] } {
	puts stderr "That was easy! No files to generate, missing -all, -inputs, -outputs, -probes -probe-code?"
	exit 2

}

if { [catch "SceMiParams::generateAllSceMiFiles OPT $opts" err] } {
    puts stderr "Error in generateSceMiHeaders: $err"
    exit 1
}
exit 0



