#!/bin/sh
# Copyright 2017 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

#
# Script to create the skeleton file for the "ptm_bridge" BlackBox.
# It gets the port names from the source "ptm_bridge.v" and the LUT
# and BRAM usage numbers from the Vivado synth utilization report.
# The location of the netlist, the location of the constraints, and
# the step clock name (all to be declared in pragams) are arguments
# to the script.
#

if { $argc != 5 } {
    set cmd [file tail $argv0]
    puts "usage: $cmd <source_vlog> <netlist> <xdc> <util_rpt> <outname>"
    exit 1
}

set bb_source_name        [lindex $argv 0]
set bb_netlist_name       [lindex $argv 1]
set bb_constraints_name   [lindex $argv 2]
set synth_utilization_rpt [lindex $argv 3]
set bb_skeleton_name      [lindex $argv 4]

set fi [open $bb_source_name "r"]
set fo [open $bb_skeleton_name "w"]

proc pragmas {fo} {
    global bb_netlist_name
    global bb_constraints_name
    global luts
    global bmem

    puts $fo "// pragma CVASTRPROP MODULE HDLICE BLACKBOX_SYNTHESIS_FILE \"$bb_netlist_name\""
    puts $fo "// pragma CVASTRPROP MODULE HDLICE BLACKBOX_CONSTRAINTS_FILE \"$bb_constraints_name\""
    puts $fo "// pragma CVASTRPROP MODULE HDLICE CONNECT_STEP_CLK \"CLK_step_clk\""

    # CDN MK values from xilinx/ptm_bridge_utilization_synth.rpt
    puts $fo "// pragma CVASTRPROP MODULE HDLICE RESERVE_LUTS \"$luts\""
    puts $fo "// pragma CVASTRPROP MODULE HDLICE RESERVE_BMEM \"$bmem\""
}

set fh [open $synth_utilization_rpt "r"]

set luts 0
set bmem 0

while { [gets $fh line] != -1} {
    switch -regexp -matchvar ms -- $line {
	{\mLUTs\*\s*\|\s*([^ ]+)} {
	    set val [lindex $ms 1]
	    set luts [expr $val]
	    puts "luts = $luts"
	}
	{\mBlock RAM Tile\s*\|\s*([^ ]+)} {
	    set val [lindex $ms 1]
	    set bmem [expr $val]
	    puts "bmem = $bmem"
	}
    }
}
close $fh

set inmodule 0
set scanning 0

# Kill comments
while { [gets $fi line] != -1} {
    set line [regsub {//.*$} $line ""]
    switch -regexp -matchvar ms -- $line {
	{^\s*$} {
	}
	{\mmodule\s+} {
	    set inmodule 1
	    puts $fo $line
	}
	{\mendmodule} {
	    puts $fo $line
	    set inmodule 0
	}
	{\m(input|output|inout)\s+}  {
	    if { $inmodule == 1 } {
		if {$scanning == 1} {
		    puts $fo $line
		}
	    }
	}
	{[;]} {
	    if { $inmodule == 1 } {
		if { $scanning == 0 } {
		    puts $fo $line
		    pragmas $fo
		    set scanning 1
		}
	    }
	}
	{default} {
	    if { $inmodule == 1 } {
		if { $scanning == 0 } {
		    puts $fo $line
		} else {
		    set scanning 2
		}
	    }
	}
    }
}
close $fi
close $fo
