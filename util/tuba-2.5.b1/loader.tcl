# loader.tcl - provides program loading services for tuba debugger
#
#  Copyright (C) 1997,1998 John E. Stump
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba

namespace eval ::tuba {
  variable tubalibdir 
  variable Ginterp
  variable startdir
  variable appl_pid 
  variable timeoutid
  
  proc parse_args {argv} {
    variable options
    variable startdir
    variable tubalibdir
    variable port
    variable debug
    variable Ginterp
    variable purgeflag
    variable Gworkingdir
    variable appl_pid
    variable exclusions
    variable sessionfile

    # force it to be an absolute path
    set startdir [pwd]
    cd $tubalibdir
    set tubalibdir [pwd]
    cd $startdir

    # set the default parser to C if there is a .so file there
    if { [file exists [file join $tubalibdir Tclparser[info shared]]] } {
      set options(parser) C
    } else {
      set options(parser) T
    }

    # set the socket port
    set port 1090

    set debug 0

    set experimental 0

    # parse the tuba options
    for {set argc 0} {[string match -* [lindex $argv $argc]]} {incr argc} {
      switch -exact -- [lindex $argv $argc] {
        -C {
          set options(parser) C
        }

        -T {
          set options(parser) T
        }

        -X {
          set experimental 1
        }

        -i {
          incr argc
          set Ginterp [lindex $argv $argc]
        }

        -p {
          incr argc
          set port [lindex $argv $argc]
        }

        -D {
          set debug 1
        }

        -R {
          set options(instrumenting) RT
        }

        -nc {
          set options(cacheflag) 0
        }

        -c {
          incr argc
          set options(cachedir) [lindex $argv $argc]
        }

        -purge {
          set purgeflag 1
        }

        -w {
          incr argc
          set Gworkingdir [lindex $argv $argc]
          cd $Gworkingdir
          set Gworkingdir [pwd]
          cd $startdir
        }
        
        -e {
          incr argc
          set file [lindex $argv $argc]
          set f [open $file r]
          set exclusions [string trim [read $f]]
          close $f
        }
        
        -s {
          incr argc
          set sessionfile [lindex $argv $argc]
        }

        -version -
        -V {
          puts "Tuba version %%VERSION%%"
          exit 0
        }

        -help {
          puts "
      -version | -V 
           Prints out the interpreter version and exits. 

       -T
           Use the Tcl-based parser.

       -C
           Use the C++-based parser if available. If not, use the Tcl-based parser. 
           This is the default.

       -X
           Use the experimental parser. Use this in conjunction with the -C or -T 
           options. The experimental parser is slightly faster and will eventually 
           replace the default parser.

       -R
           Specifies run-time instrumentation. Although slower, run-time 
           instrumenting is very handy for Tcl/Tk code that uses a lot of dynamic 
           code creation, code that cannot easily be parsed at load-time. If tuba 
           croaks on your application due to a parsing error, then try the -R 
           option.

       -i <interp>
           Sets the interpreter to <interp>. Default is tclsh8.0 (wish80 on 
           Windows). This does not have to have a path if the command is in 
           your PATH environment.

       -p <port>
           Use port <port> for the socket. The default is 1090.

       -c <cachedir>
           Sets the directory for caching instrumented source files. The default is 
           <tubadir>/cache, where <tubadir> is the directory tuba is installed in.

       -nc
           No caching. This turns off reading and writing caches instrument files.

       -purge
           Purges the cache directory before continuing.

       -w <workingdir>
           Sets the working directory to be in when debugging your application. 
           If <workingdir> is set to \"\", then it will default to the same 
           directory the application resides in.
           
       -e <exclusionfile>
           Specifies a file that contains proc exclusion expressions.
           
       -s <sessionfile>
           Opens the specied session file.

       -help 
           Prints out brief help on options and exits.
          "
          exit 0
        }

        default {
          unknown_option argv argc
          exit 1
        }
      }
    }

    if { $experimental } {
      append options(parser) X
    }

    set appl_pid ""
    
    return $argc
  }
  
  proc startapp {cwd app interp args} {
    variable port 
    variable tubalibdir 
    variable appl_pid
    variable Gprogram
    variable Ginterp

    if { ! [file readable $app] } {
      internal_error "application '$app' is not found or is not readable"
      return
    }

    # every is either absolute or relative to the directory we started in
    variable startdir
    cd $startdir

    # get the Gprogram as an absolute path
    cd [file dirname $app]
    set Gprogram [pwd]/[file tail $app]
    set Ginterp $interp
    cd $startdir

    if { $cwd == "" } {
      # if the user leaves the working directory blank, then set it to the 
      # same as the program
      cd [file dirname $app]
    } else {
      if { ! [file isdirectory $cwd] } {
        internal_error "the directory '$cwd' is not a directory"
        return
      }
      cd $cwd
    }

    # start up the application
    set appl_pid [eval exec [list $interp] [list $tubalibdir/tuba_lib.tcl] $port [list $tubalibdir] [list $app] $args &]

    # start the timeout timer
    variable timeoutid
    set timeoutid [after 10000 [namespace code timeout]    ]
  }

  proc killapp {} {
    variable appl_pid 
    variable appl_ch
    # remove the file event on the socket
    if { $appl_ch != "" } {
      sendcmd_wr closeapp
      sendmsg EXIT
      fileevent $appl_ch readable {}
    }
  }
}
