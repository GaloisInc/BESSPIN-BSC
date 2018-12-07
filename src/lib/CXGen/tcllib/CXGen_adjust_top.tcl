#!/bin/sh
# Copyright 2017 Bluespec, Inc.  All rights reserved.
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

#
# Script to remove "CLK_step_clk" and "CLK_<clk_name>" from the port list
# of "ptm_top", to create an undriven wire declaration for "CLK_<clk_name>",
# and to remove the connection of "CLK_step_clk" in the instantiation of
# "ptm_bridge".
#

if { $argc != 3 } {
    puts "usage: $argv0 <clkname> <infile> <outfile>"
    exit 1
}

set clk_name [lindex $argv 0]
set in_name  [lindex $argv 1]
set out_name [lindex $argv 2]

#puts $clk_name
#puts $in_name
#puts $out_name

set fi [open $in_name r]
set fo [open $out_name w]

set in_module      0
set in_portlist    0
set in_bridge_inst 0
set in_dut_inst    0

set last_port     {}
set num_changes    0

while { [gets $fi line] != -1 } {

    switch -regexp -matchvar ms -- $line {
	{^\s*$} {
	    # ignore empty lines
	    puts $fo $line
	}
	{^\s*\/\/} {
	    # ignore empty lines with comments
	    puts $fo $line
	}
	{\mmodule\s+} {
	    set in_module      1
	    set in_portlist    1
	    set in_bridge_inst 0
	    set in_dut_inst    0
	    puts $fo $line
	}
	{\mendmodule} {
	    puts $fo $line
	    set in_module      0
	    set in_portlist    0
	    set in_bridge_inst 0
	    set in_dut_inst    0
	}
	{^\s*CLK_\S+\s*[,)]} {
	    if { ($in_module == 1) } {
		if {[ regexp {\mCLK_step_clk\M} $line ]} {
		    # omit step_clk
		    incr num_changes
		} elseif {[ regexp "\\mCLK_$clk_name\\M" $line ]} {
		    # keep xactor clock
		    puts $fo $line
		    ## omit the xactor clock
		    #incr num_changes
		} else {
		    puts $fo $line
		}
	    } else {
		puts $fo $line
	    }		
	}
	{^\s*input\s+CLK_\S+\s*} {
	    if { $in_module == 1 } {
		if {[ regexp {\mCLK_step_clk\M} $line ]} {
		    # omit step_clk
		    incr num_changes
		} elseif {[ regexp "\\mCLK_$clk_name\\M" $line ]} {
		    # keep xactor clock
		    puts $fo $line
		    ## omit the xactor clock
		    #incr num_changes
		} else {
		    puts $fo $line
		}
	    } else {
		puts $fo $line
	    }
	}
    {^\s*(input|output|inout)\s+(\[.*\]\s+)?(\S+)\s*;\s*$}  {
	    puts $fo $line
	    if {[ regexp {^\s*(input|output|inout)\s+(\[.*\]\s+)?(\S+)\s*;\s*$} $line -> var1 var2 var3 ]} {
		if { $var3 == $last_port } {
		    puts $fo {}
		    puts $fo {  // undriven signal for the xactor clock}
		    puts $fo "  wire CLK_$clk_name;"
		    incr num_changes
		}
	    } else {
		puts "ERROR in pattern for last port"
		exit 1
	    }
	}
        {^\s*(\S+)\s+(\S+)\s*\(\s*\.\S*\s*\(} {
	    # submodule instance
	    if {[ regexp {^\s*ptm_bridge\s+} $line ]} {
		set in_bridge 1
		# omit the step_clk argument if it's in this line
		incr num_changes [ regsub -- {\(CLK_step_clk\)} $line {(1'b0)} line ]
	    } else {
		set in_dut 1
		# change step_clk argument to $clkname if it's in this line
		incr num_changes [ regsub -- {CLK_step_clk} $line "CLK_$clk_name" line ]
	    }
	    puts $fo $line
	}
        {^\s*\.\S+\s*\(\s*\S+\s*\)\s*(,|\)\s*;)\s*$} {
	    if { $in_bridge == 1 } {
		# omit the step_clk argument if it's in this line
		incr num_changes [ regsub -- {\(CLK_step_clk\)} $line {(1'b0)} line ]
	    } elseif { $in_dut == 1 } {
		# change step_clk argument to $clkname if it's in this line
		incr num_changes [ regsub -- {CLK_step_clk} $line "CLK_$clk_name" line ]
	    }
	    puts $fo $line
	}
	{default} {
	    puts $fo $line
	}
    }
    # end: switch

    # if this line has a semicolon, then it's no longer a module instance
    if {[ regexp {;} $line ]} {
	set in_bridge 0
	set in_dut    0
    }

    # if this is the last port in the portlist,
    # record the port name and that we've exited the portlist
    if {[ regexp {^\s*(\S+)\s*\);\s*$} $line -> var1]} {
	if { $in_portlist == 1 } {
	    set in_portlist  0
	    set last_port $var1
	}
    }

}
# end: while { [gets $fi line] != -1 }

close $fi
close $fo

#puts "Num changes: $num_changes"
exit [ expr "$num_changes != 5" ]
