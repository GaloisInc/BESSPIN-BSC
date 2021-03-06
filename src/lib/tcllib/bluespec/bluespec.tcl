# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# $Id$

#  name spaces for bluetcl commands and for this command
# facility for calling procedures on exit

namespace eval ::Bluetcl {
    variable w32or64 [expr 8*$::tcl_platform(pointerSize)]
    variable kernelname [string tolower $::tcl_platform(os)]
}

namespace eval AtExit {
    variable atExitScripts [list]

    proc atExit script {
        variable atExitScripts
        lappend atExitScripts \
                [uplevel 1 [list namespace code $script]]
    }

    namespace export atExit
}

rename exit AtExit::ExitOrig
proc exit {{code 0}} {
    variable AtExit::atExitScripts
    set n [llength $atExitScripts]
    while {$n} {
        catch [lindex $atExitScripts [incr n -1]]
    }
    rename exit {}
    rename AtExit::ExitOrig exit
    namespace delete AtExit
    exit $code
}

# namespace import AtExit::atExit


# Procedure called during initialization
proc Bluetcl::initBluespec {} {
    global argv
    global env
    global tcl_rcFileName

    if {[file readable $tcl_rcFileName]} {
        if { [catch "uplevel #0 source $tcl_rcFileName" err] } {
            global errorInfo
            puts stderr "Error in startup script file: $tcl_rcFileName"
            puts stderr $errorInfo
            exit 1
        }
    }

    if { ! [info exists argv] } { return }

    set rest [::utils::scanOptions {-quiet} {-exec} false OPT $argv]
    set argv $rest
    set argc [llength $rest]

    # unload Bluesim model on exit
    AtExit::atExit [list ::Bluetcl::sim unload]

    if { [info exists OPT(-exec)] } {
        # look for a script to execute
        set found false
        foreach dir [linsert $::auto_path 0 ""] {
            set fn [file join $dir $OPT(-exec)]
            if { [file exists $fn] } {
                set found true
                set ::argv0 "$::argv0 -exec $OPT(-exec)"
                if { [catch "uplevel #0 source $fn" res] } {
                    puts stderr $res
                    puts stderr $::errorInfo
                    exit 2
                }
                break
            } elseif { [file exists $fn.tcl] } {
                set found true
                set ::argv0 "$::argv0 -exec $OPT(-exec)"
                if { [catch "uplevel #0 source $fn.tcl" res] } {
                    puts stderr $res
                    puts stderr $::errorInfo
                    exit 2
                }
                break
            }
        }
        if { ! $found } {
            puts stderr "Error:  script `$OPT(-exec)' was not found."
            exit -1
        }
    }
}

# Force the loading of the utils package.
utils::donothing
lappend auto_path /usr/share
lappend auto_path /usr/lib

if { [catch Bluetcl::initBluespec err] } {
    puts "Error in initialization file bluespec.tcl: $err" 
    error $err
}

namespace eval ::Bluetcl {
    namespace export bpackage
    namespace export browsemodule
    namespace export browseinst
    namespace export browsepackage
    namespace export browsetype
    namespace export defs
    namespace export depend
    namespace export flags
    namespace export help
    namespace export module
    namespace export rule    
    namespace export schedule
    namespace export sim
    namespace export submodule
    namespace export type
    namespace export version

}
