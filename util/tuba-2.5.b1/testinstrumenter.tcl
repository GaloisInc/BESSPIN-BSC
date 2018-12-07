#!/usr/bin/tclsh

#
# testinstrumenter - simple interface to the instrumenter to test it
#
#  Copyright (C) 1997,1998 John E. Stump
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba


# syntax: testinstrumenter <options> source_file
#  <options>:
#  -C : use the C++ parser
#  -T : use the Tcl parser
#  -X : use the experimental parser
#  -c <dir> : use <dir> as the cache dir
#  -purge : purge cache dir
#  -x : execute the code (dumps code to stdout if this option is not used)
#  actions can be a 2 digit number using digits 0, 1, 2, and 3
#  -ca <action> set command action
#  -pa <action> set proc action
#  -coa <action> set comment action
#  -rca <action> set runtime command action
#  -rpa <action> set runtime proc action
#  -rcoa <action> set runtime comment action
#  -pna 0|1 set proc entry action
#  -pxa 0|1 set proc exit action

# set up defaults
array set options {
  commandaction 22
  commentaction 00
  procaction 22
  RTcommandaction 22
  RTcommentaction 00
  RTprocaction 22
  procentryaction 1
  procexitaction 1
  parser C
  cachedir ""
}

set execute 0
set purge 0
set experimental 0

# these are some hard coded callbacks
proc foundCommand {file line commandvar} {
}

proc foundComment {file line commentvar} {
}

proc foundProc {file line commandvar procnamevar dynaprocflag} {
}

proc statement {file line stmt} {
  uplevel $stmt
}

proc procEntry {procname} {
}

proc procExit {procname} {
}


# parse the command line options
for {set argc 0} {[string match -* [lindex $argv $argc]]} {incr argc} {
  switch -exact -- [lindex $argv $argc] {
    -V |
    -version {
      puts "testinstrumenter version %%VERSION%%"
      exit 0
    }
    
    -C {
      set options(parser) C
    }
    
    -T {
      set options(parser) T
    }
     
    -X {
      set experimental 1
    }
  
    -c {
      incr argc
      set options(cachedir) [lindex $argv $argc]
    }

    -purge {
      set purge 1
    }
    
    -x {
      set execute 1
    }
    
    -ca {
      incr argc
      set options(commandaction) [lindex $argv $argc]
    }
    
    -pa {
      incr argc
      set options(procaction) [lindex $argv $argc]
    }
    
    -coa {
      incr argc
      set options(commentaction) [lindex $argv $argc]
    }
    
    -rca {
      incr argc
      set options(RTcommandaction) [lindex $argv $argc]
    }
    
    -rpa {
      incr argc
      set options(RTprocaction) [lindex $argv $argc]
    }
    
    -rcoa {
      incr argc
      set options(RTcommentaction) [lindex $argv $argc]
    }
    
    -pna {
      incr argc
      set options(procentryaction) [lindex $argv $argc]
    }
    
    -pxa {
      incr argc
      set options(procexitaction) [lindex $argv $argc]
    }
    
    default {
      puts stderr "unknown option: '[lindex $argv $argc]'"
      exit 1
    }
  }
}

if { $experimental } {
  append options(parser) X
}

if { [lindex $argv $argc] == "" } {
  puts stderr "syntax: testinstrumenter <options> source_file
<options>:
-C : use the C++ parser
-T : use the Tcl parser
-X : use the experimental parser
-c <dir> : use <dir> as the cache dir
-purge : purge cache dir
-x : execute the code (dumps code to stdout if this option is not used)
actions can be a 2 digit number using digits 0, 1, 2, and 3
-ca <action> set command action
-pa <action> set proc action
-coa <action> set comment action
-rca <action> set runtime command action
-rpa <action> set runtime proc action
-rcoa <action> set runtime comment action
-pna 0|1 set proc entry action
-pxa 0|1 set proc exit action"
  exit 1
}

# load in the instrumenter
set dir [file dirname $argv0]
source $dir/Tclparser_instrumenter.tcl

# initialize it
instrumenter::init $dir {} [array get options]

if { $purge } {
  instrumenter::purgeCache
  if { [lindex $argv $argc] == "" } {
    exit 0
  }
}

set icode [instrumenter::instrument [lindex $argv $argc]]

if { $execute } {
  eval $icode
} else {
  puts $icode
}
