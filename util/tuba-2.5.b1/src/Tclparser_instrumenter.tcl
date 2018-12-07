#  Copyright (C) 1998 John E. Stump
#
#  This software is covered by the Starving Artist Artistic License. Please
#  see the file LICENSE in the toplevel directory of the distribution.
#
#  contact author: johnstump@iname.com
#       home page: http://www.geocities.com/SiliconValley/Ridge/2549/tuba
#
# This loader implements the generic interface for tuba to use the
# Tclparser package. Depending on options given to init, this will use
# the Tcl or C++ version.

namespace eval instrumenter {
  set Gfile ""
  set Gclientns ""
  set Glibdir "."
  set Gcommentaction 00
  set Gcommandaction 00
  set Gprocaction 00
  set GRTcommentaction 00
  set GRTcommandaction 00
  set GRTprocaction 00
  set Gprocentryaction 0
  set Gprocexitaction 0
  set Gcachedir ""
  set Gcacheflag 0
  
  # itcl support
  set insideClass ""
 
  proc init {libdir clientns options} {
    variable Glibdir
    set Glibdir $libdir
    
    variable Gclientns
    set Gclientns $clientns
    
    set parser T
    
    array set opts $options
    # parse the options
    foreach opt [array names opts] {
      switch $opt {
        parser {
          set parser $opts($opt)
        }
        
        cachedir -
        cacheflag -
        commentaction -
        commandaction -
        procaction -
        RTcommentaction -
        RTcommandaction -
        RTprocaction -
        procentryaction -
        procexitaction {
          variable G$opt
          set G$opt $opts($opt)
        }
      }
    }

    # remove any pre-existing parser namespace
    set ns [namespace current]
    if { [namespace children $ns Tclparser] == "${ns}::Tclparser" } {
      namespace delete Tclparser
    }
    
    # load the actual parser package
    switch $parser {
      T {
        source [file join $Glibdir Tclparser.tcl]
      }
      
      C {
        if { [catch {load $Glibdir/Tclparser[info shared] Tclparser} rc] } {
  #         puts "can't seem to load the Tclparser shared library because of:"
  #         puts $rc
  #         puts "Loading Tcl parser"
          source $Glibdir/Tclparser.tcl
        }
      }
      
      TX {
        source [file join $Glibdir TclparserX.tcl]
      }
      
      CX {
        if { [catch {load $Glibdir/TclparserX[info shared] Tclparser} rc] } {
  #         puts "can't seem to load the TclparserX shared library because of:"
  #         puts $rc
  #         puts "Loading Tcl parser"
          source [file join $Glibdir TclparserX.tcl]
        }
      }
    }
  }

  proc getabsolute {file} {
    # get absolute filename of file
    set cwd [pwd]
    cd [file dirname $file]
    set file [file join [pwd] [file tail $file]]
    cd $cwd
    
    return $file
  }
  
  proc instrument {file {wascached& ""} {fromcache& ""}} {
    variable Gfile
    
    set file [getabsolute $file]
    set Gfile $file
    
    # is this file cached?
    set cachedfile [isCached $file]
    
    if { $cachedfile != "" } {
      # load in the cached file instead
      set f [open $cachedfile r]
      set icode [read $f]
      close $f

      # set the cached flags if the user passed one in
      if { ${wascached&} != "" } {
        # wascached means did we cache this file
        upvar ${wascached&} wascached
        set wascached 0
      }

      if { ${fromcache&} != "" } {
        # fromcache means did we get this file from the cache
        upvar ${fromcache&} fromcache
        set fromcache 1
      }
    } else {
      set f [open $file r]
      set code [read $f]
      close $f

      set icode [instrumentcode $file 1 $code]

      if { ${fromcache&} != "" } {
        # fromcache means did we get this file from the cache
        upvar ${fromcache&} fromcache
        set fromcache 0
      }
    
      # do we want to cache this?
      variable Gcachedir
      if { $Gcachedir != "" } {
        set cachefile [cacheFilename $file]
        file mkdir [file dirname $cachefile]
        set f [open $cachefile w]
        puts $f $icode
        close $f

        # set the cached flag if the user passed one in
        if { ${wascached&} != "" } {
          upvar ${wascached&} wascached
          set wascached 1
        }
      } else {
        # set the cached flag if the user passed one in
        if { ${wascached&} != "" } {
          upvar ${wascached&} wascached
          set wascached 0
        }
      }
    }
    
    return $icode
  }

  proc cacheFilename {file} {
    global tcl_platform
    variable Gcachedir
    
    if { $tcl_platform(platform) == "windows" } {
      if { [string match ?:* $file] } {
        # remove any drive letters
        set file [string range $file 2 end]
      }
    }

    return ${Gcachedir}$file
  }
  
  proc isCached {file} {
    variable Gcachedir
    variable Gcacheflag
    variable Gclientns
    
    if { $Gcacheflag == 0 || $Gcachedir == "" } {
      return ""
    }
    
    set cachefile [cacheFilename $file]
    
    # is the file in our cache directory?
    if { ! [file exists $cachefile] } {
      return ""
    }
    
    # is the file newer than our cached file?
    if { [file mtime $file] > [file mtime $cachefile] } {
      return ""
    }
    
    # the cached file is good
    if { $Gcacheflag == "?" } {
      # now let's ask the client
      if { [${Gclientns}::useCache $file] == 0 } {
        return ""
      }
    }
    
    return $cachefile
  }
  
  proc setClientData {file data} {
    variable Gcachedir
    
    if { $Gcachedir == "" } {
      error "No cachedir set"
    }
    
    set file [getabsolute $file]
    set cachefile [cacheFilename $file]
    set f [open $cachefile.client w]
    puts $f $data
    close $f
  }
  
  proc getClientData {file} {
    variable Gcachedir
    
    if { $Gcachedir == "" } {
      error "No cachedir set"
    }

    set file [getabsolute $file]
    set cachefile [cacheFilename $file]
    
    if { ! [file exists $cachefile.client] } {
      return ""
    }
    
    set f [open $cachefile.client r]
    set data [read $f]
    close $f
    return $data
  }
  
  proc purgeCache {} {
    variable Gcachedir
    
    if { $Gcachedir == "" } {
      return
    }
    
    file delete -force $Gcachedir
  }
  
  proc getLevel {} {
    variable blocklevel
    return $blocklevel
  }
  
  # this will instrument a single statement or a block of code
  proc instrumentcode {file line code} {
    variable Gfile
    set saveGfile $Gfile
    set Gfile $file
    
    # create the parser object for the block of code
    set tp [new_Tclparser $code $line]
    set code [parseblock $tp]

    # delete the parser object
    delete_Tclparser $tp

    set Gfile $saveGfile
    
    return $code
  }
  
  proc RTinstrumentcode {file line args} {
    # need to reset the action options
    variable Gprocaction
    variable Gcommandaction
    variable Gcommentaction
    
    set procaction $Gprocaction
    set commandaction $Gcommandaction
    set commentaction $Gcommentaction
    
    variable GRTprocaction
    variable GRTcommandaction
    variable GRTcommentaction
    
    set Gprocaction $GRTprocaction
    set Gcommandaction $GRTcommandaction
    set Gcommentaction $GRTcommentaction
    
#puts "code to instrument is '$args'"
    # now instrument the code
    set code [instrumentcode $file $line $args]
#puts "runtime instrumented code : $code"
    # reset globals
    set Gprocaction $procaction
    set Gcommandaction $commandaction
    set Gcommentaction $commentaction

    set rc [catch {uplevel $code} msg]
    return -code $rc $msg
  }
  
  proc vartracehandler {procname args} {
    variable Gclientns
    ${Gclientns}::procExit $procname
  }
  
  # this handles the load time instrumentation
  proc dbg2 {file line stmt} {
    variable Gclientns
    
    set icode "${Gclientns}::statement [list $file] $line {$stmt}\n"
    return $icode
  }
  
  # this handles the run time instrumentation
  proc dbg3 {file line stmt} {
    set ns [namespace current]
#     if { [string index $stmt 0] != "\x7b" } {
#       set stmt "{$stmt}"
#     }

    # need to handle escaped newlines
    set stmt [handle_escaped_newlines $stmt]
    
    set icode "${ns}::RTinstrumentcode [list $file] $line $stmt\n"
#puts "code to instrument at runtime: $icode"
    return $icode
  }
  

  proc handle_escaped_newlines {stmt} {
    set rc ""
    foreach line [split $stmt \n] {
      set count [backslashcount $line]
      if { [expr {$count % 2}] != 0 } {
        set l [string length $line]
        incr l -2
        # remove the last backslash and put a space there
        append rc "[string range $line 0 $l] \\\x7f\n"
      } else {
        append rc $line\n
      }
    }
    
    return $rc
  }

  proc add_extra_backslashes {stmt} {
    set rc ""
    # put an extra backslash on every one
    regsub -all {\\} $stmt {&\\} rc
    return $rc
  }
  
  # returns the number of backslashes on the end of the line, skipping the 
  # last character which is either a newline or semi
  proc backslashcount {line} {
    set i [string length $line]
    incr i -1
    set count 0
    while { [string index $line $i] == "\\" } {
      incr count
      incr i -1
    }

    return $count
  }

  # flush the remaining tokens until reached the end of statement
  proc flush2eos {tp} {
    set icode ""

    for {set token [$tp gettok]} \
        {$token != [geteos] && $token != [geteof]} \
        {set token [$tp gettok]} {
      append icode "$token "
    }

    return $icode
  }

  # parse a statement (which may span multiple lines)
  #
  proc parse {cmd tp} {
    variable Gfile

    # notify the client about the statement, who may decide he doesn't
    # want it
    variable Gcommandaction
    variable Gclientns
    
    # if the command is proc, then we treat it differently (we let the 
    # proc handler deal with it)
    if { $cmd == "proc" || 
         $cmd == "body" || 
         $cmd == "configbody" || 
         $cmd == "method" } {
      set commandaction 22
    } elseif { $Gcommandaction == "?" } {
      set commandaction [${Gclientns}::foundCommand $Gfile [$tp getlineno] cmd]
    } else {
      set commandaction $Gcommandaction
    }
    
    switch -glob $commandaction {
      0? {
        # ignore this command
        flush2eos $tp
        set code ""
      }
      
      11 {
        # save it as is, uninstrumented
        set code "$cmd [flush2eos $tp]"
      }
      
      21 {
        # instrument the toplevel, but not any lower levels
        set code [dbg2 $Gfile [$tp getlineno] [list $cmd [flush2eos $tp]]]
      }
      
      31 {
        # rt instrument the toplevel only
        set code [dbg3 $Gfile [$tp getlineno] [list $cmd [flush2eos $tp]]]
      }
      
      12 -
      13 -
      22 -
      23 -
      32 -
      33 {
        # various forms of instrumentation
        # do we have a handler for this command?
        set ns [namespace current]
        if { [info command ${ns}::Tclparser_${cmd}] == "${ns}::Tclparser_${cmd}" } {
          # by all means run it
          set code [Tclparser_${cmd} $tp $commandaction]
        } else {
          # if we don't have a handler, the only thing we can do is the toplevel
          # and ignore any code bodies.
          set line [$tp getlineno]
          set stmt "$cmd [flush2eos $tp]"
          switch [string index $commandaction 0] {
            1 {
              set code $stmt
            }
            
            2 {
              set code [dbg2 $Gfile $line $stmt]
            }
            
            3 {
              set code [dbg3 $Gfile $line $stmt]
            }
          }
        }
        
      }
      
      default {
        puts stderr "Unknown command action: '$commandaction'"
        exit 1
      }
    }

    return $code\n
  }

  proc parseblock {tp} {
    variable Gclientns
    variable Gfile
    
    set icode ""

    while { [set cmd [$tp gettok]] != [geteof] } {
      if { $cmd == [geteos] } continue
      if { [string index $cmd 0] == "#" } {
        variable Gcommentaction
        if { $Gcommentaction == "?" } {
          set commentaction [${Gclientns}::foundComment $Gfile [$tp getlineno] cmd]
        } else {
          set commentaction $Gcommentaction
        }
        
        switch -glob $commentaction {
          0? {
            # ignore the comment
          }
          
          1? {
            # save the (possibly modified) comment, uninstrumented
            append icode $cmd\n
          }
          
          2? {
            # save the (possibly modified) comment, instrumented
            append icode "${Gclientns}::statement [list $Gfile]\
              [$tp getlineno] {$cmd}\n"
          }
          
          3? {
            error "rt instrumenting not ready yet"
          }
          
          default {
            error "Unknown commentaction: '$commentaction'"
          }
        }
      } else {
        append icode [parse $cmd $tp]
      }
    }
#puts $icode

    return $icode
  }

  # how many levels deep we are
  variable blocklevel 0
  
  # this parses a block. It assumes there should be a block. If there isn't
  # then this will create one using {}
  #
  proc doblock {block lineno dotrailer {extra ""}} {
    variable Gfile
    variable blocklevel

    set code ""
    incr blocklevel

    # create a new parser
    set newtp [new_Tclparser $block $lineno]
    set bs [$newtp getBlockStart]
    set be [$newtp getBlockEnd]

    # even if it wasn't blocked before (because it was only a single cmd)
    # we have to now because we're adding the debug callback
    if { $bs == " " } {
      set bs "{"
      set be "}"
    }

    append code "$bs $extra\n"

    append code [parseblock $newtp]

    if { $dotrailer == 1 } {
      append code [dbg2 $Gfile [$newtp getlineno] ""]
    }

    append code "$be "

    delete_Tclparser $newtp
    
    incr blocklevel -1

    return $code
  }

  # this is essentially dbg3, but it handles blocks. It retains the original
  # blocking characters, be they quotes or braces.
  proc doblock3 {lineno block {extra ""}} {
    variable Gfile

    set code ""

    # any blocking here?
    set bs [string index $block 0]
    switch $bs {
      \x7b -
      \x5b -
      \" {
        set len [string length $block]
        incr len -1
        # get the end character
        set be [string index $block $len]
        
        # get the code inside the block
        incr len -1
        set block [string range $block 1 $len]
      }
      
      default {
        set bs "\""
        set be "\""
      }
    }

    append code "$bs [dbg3 $Gfile $lineno $block] $be"
    return $code
  }


  proc Tclparser_if {tp action} {
    variable Gfile
    set code ""

    set line [$tp getlineno]
    append code "if "

    set expected expr
    while { [set arg [$tp gettok]] != [geteos] && $arg != [geteof] } {
      switch $expected {
        expr {
          append code "$arg "
          set expected body
        }

        body {
          if { $arg == "then" } {
            continue
          } else {
            if { [string match ?3 $action] } {
              append code "[doblock3 [$tp getlineno] $arg] "
            } else {
              append code [doblock $arg [$tp getlineno] 0]
            }
            set expected else
          }
        }

        else {
          if { $arg == "elseif" } {
            append code "elseif "
            set expected expr
          } elseif { $arg == "else" } {
            continue
          } else {
            if { [string match ?3 $action] } {
              append code "[doblock3 [$tp getlineno] $arg] "
            } else {
              append code [doblock $arg [$tp getlineno] 0]
            }
            set expected error
          }
        }

        error {
          puts "malformed if statement: line [$tp getlineno]"
          exit 1
        }

        default {
          puts "unknown state in if handler: '$expected'"
          exit 1
        }
      }
    }

    if { [string match 1? $action] } {
      return $code
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $code]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $code]
    }
  }

  proc Tclparser_while {tp action} {
    variable Gfile
    set code ""

    set begline [$tp getlineno]
    append code "while [$tp gettok] "
    set block [$tp gettok]
    set blockline [$tp getlineno]
    
    if { [string match ?3 $action] } {
      append code "[doblock3 $blockline $block]"
    } else {
      append code [doblock $block $blockline 1]
    }

    if { [string match 1? $action] } {
      return $code
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $begline $code]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $begline $code]
    }
  }

  # cmdlist: foreach <var> <list> <body>, or
  #          foreach <varlist> <list> [<varlist> <list> ...] <body>
  #
  proc Tclparser_foreach {tp action} {
    variable Gfile
    set code ""

    set line [$tp getlineno]
    append code "foreach "


    while { 1 } {
      # get the var list and the list
      set varlist [$tp gettok]
      set bodylineno [$tp getlineno]
      set vallist [$tp gettok]

      # if this is EOS, then varlist is actually the code
      if { $vallist == [geteos] || $vallist == [geteof] } break

      append code "$varlist $vallist "
    }

    # parse the foreach code block
    if { [string match ?3 $action] } {
      append code "[doblock3 $bodylineno $varlist]"
    } else {
      append code [doblock $varlist $bodylineno 0]
    }

    if { [string match 1? $action] } {
      return $code
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $code]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $code]
    }
  }


  # cmdlist: for <init> <test> <next> <body>
  #
  proc Tclparser_for {tp action} {
    variable Gfile
    set code ""

    set line [$tp getlineno]
    
    append code "for "

    set state init
    while { [set arg [$tp gettok]] != [geteos] && $arg != [geteof] } {
      switch $state {
        init {
          append code "$arg "
          set state test
        }

        test {
          append code "$arg "
          set state next
        }

        next {
          if { [string match ?3 $action] } {
            append code "[doblock3 [$tp getlineno] $arg] "
          } else {
            append code "[doblock $arg [$tp getlineno] 0]"
          }
          set state body
        }

        body {
          if { [string match ?3 $action] } {
            append code "[doblock3 [$tp getlineno] $arg] "
          } else {
            append code "[doblock $arg [$tp getlineno] 0]"
          }
          set state error
        }

        error {
          puts "malformed for statement, line [$tp getlineno]"
          exit 1
        }

        default {
          puts "internal error: unknown state: '$state'"
          exit 1
        }
      }
    }

    if { [string match 1? $action] } {
      return $code
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $code]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $code]
    }
  }


  # cmdlist: switch [options] <string> <pattern> <body> [<pattern> <body>...]
  #          switch [options] <string> {<pattern> <body> [<pattern> <body>...]}
  #
  proc Tclparser_switch {tp action} {
    variable Gfile
    set icode ""
    
    set line [$tp getlineno]
    
    append icode "switch "

    # get the string (but it may be an option)
    while { 1 } {
      set string [$tp gettok]
      append icode "$string "
      if { ! [string match -* $string] } {
        break
      }
    }

    # read the pattern body stuff
    set pattern [$tp gettok]
    set patternlineno [$tp getlineno]
    set body [$tp gettok]

    if { $body == [geteos] || $body == [geteof] } {
      # we're doing the second switch version
      append icode [parse_internal_switch $pattern $patternlineno $action]
    } else {
      # we're doing the 1st switch version
      while { 1 } {
        append icode "$pattern "
        if { $body == "-" } {
          append icode "- "
        } else {
          if { [string match ?3 $action] } {
            append icode "[doblock3 [$tp getlineno] $body] "
          } {
            append icode [doblock $body [$tp getlineno] 0]
          }
        }

        set pattern [$tp gettok]
        if { $pattern == [geteos] || $pattern == [geteof] } break
        set body [$tp gettok]
      }
    }

    if { [string match 1? $action] } {
      return $icode
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $icode]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $icode]
    }
  }


  # this proc parses the {pattern body} blocks in switch and case commands
  #
  proc parse_internal_switch {code lineno action} {
    variable Gfile

    set icode ""

    # create a new tclparser object
    set tp [new_Tclparser $code $lineno]
    set bs [$tp getBlockStart]
    set be [$tp getBlockEnd]

    # even if it wasn't blocked before (because it was only a single cmd)
    # we have to now because we're adding the debug callback
    if { $bs == " " } {
      set bs "{"
      set be "}"
    }

    append icode "$bs\n"

    set pattern [$tp gettok]
    while { $pattern != [geteof] } {
      if { $pattern != [geteos] } {
        set body [$tp gettok]

        append icode "$pattern "
        if { $body == "-" } {
          append icode "- "
        } else {
          if { [string match ?3 $action] } {
            append icode "[doblock3 [$tp getlineno] $body] "
          } {
            append icode [doblock $body [$tp getlineno] 0]
          }
        }
      }
      set pattern [$tp gettok]
    }

    append icode "$be "
    delete_Tclparser $tp
    return $icode
  }


  # catch <body> [var]
  #
  proc Tclparser_catch {tp action} {
    variable Gfile
    set icode ""

    #set icode [dbg $Gfile [$tp getlineno]]
    
    set body [$tp gettok]
    set line [$tp getlineno]
    if { [string match ?3 $action] } {
      append icode "catch [doblock3 $line $body] "
    } {
      append icode "catch [doblock $body $line 0] "
    }

    # get the optional varname
    if { [set ov [$tp gettok]] != [geteos] && $ov != [geteof] } {
      append icode "$ov\n"
    }

    if { [string match 1? $action] } {
      return $icode
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $icode]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $icode]
    }
  }

  # proc <name> <args> <body>
  #
  proc Tclparser_proc {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set icode ""

    set begline [$tp getlineno]
    set procname [$tp gettok]
    set arguments [$tp gettok]
    if { $arguments == [geteos] && $insideClass == 1 } {
      # part of a itcl class definition
      return "proc $procname"
    }
    
    set body [$tp gettok]
    if { $body == [geteos] && $insideClass == 1 } {
      # part of a itcl class definition
      return "proc $procname $arguments"
    }
    
    set endline [$tp getlineno]

    set dynaproc 0
    set dynaname 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 1. the proc name has expansion stuff in it
    if { [string first "\$" $procname] != -1 || 
         [string first "\[" $procname] != -1 } {
      set dynaproc 1
      set dynaname 1
    } 

    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }
    
    # stick the namespace on it
    if { [lookns] != "" && [string range $procname 0 1] != "::" } {
      set procname [lookns]::$procname
    }

    set proccmd "proc"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd procname $dynaproc]
    } else {
      set procaction $Gprocaction
    }
    
    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }
      
      11 {
        # save the proc definition as is
        set icode "$proccmd $procname $arguments $body\n"
      }
      
      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $procname $arguments $body"]
      }
      
      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $procname $arguments $body"]
      }
      
      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd $procname $arguments "
        set extra ""
        
        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $Gprocentryaction == 1 } {
              if { $dynaname } {
                if { $dynabody } {
                  # we don't know the real proc name until runtime
                  append extra "  ${Gclientns}::procEntry \\\[lindex \\\[info level 0] 0]\n"
                } else {
                  append extra "  ${Gclientns}::procEntry \[lindex \[info level 0] 0]\n"
                }
              } else {
                append extra "  ${Gclientns}::procEntry $procname\n"
              }
            }

            if { $Gprocexitaction == 1 } {
              if { $dynaname } {
                if { $dynabody } {
                  # we don't know the real proc name until runtime
                  append extra "  set __.__ {};trace variable __.__ u \\\"[namespace current]::vartracehandler \\\[lindex \\\[info level 0] 0]\\\"\n"
                } else {
                  # we don't know the real proc name until runtime
                  append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler \[lindex \[info level 0] 0]}\n"
                }
              } {
                append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $procname}\n"
              }
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            append icode [doblock3 $begline $body]
          }
        }
        
        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }
      
      default {
        error "unknown procaction: '$procaction'"
      }
    }

    return $icode
  }

  # namespace support.
  # we need to keep track of the current namespace nesting so our procs
  # defined in them can have a fully-qualified name
  variable nsstack ""
  
  # push a new namespace on the stack. If it isn't fully qualified already,
  # then append it to the current ns
  proc pushns {ns} {
    variable nsstack
    if { [string range $ns 0 1] != "::" } {
      lappend nsstack [lookns]::$ns
    } else {
      lappend nsstack $ns
    }
  }
  
  proc popns {} {
    variable nsstack
    # remove the top of the stack
    set nsstack [lreplace $nsstack end end]
  }
  
  proc lookns {} {
    variable nsstack
    return [lindex $nsstack end]
  }
  

  # namespace eval name <body>
  # all other namespace variants we aren't interested in
  #
  proc Tclparser_namespace {tp action} {
    variable Gfile

    set line [$tp getlineno]
    set icode ""

    # is this an "eval" namespace command?
    set tok [$tp gettok]
    if { $tok != "eval" } {
      append icode "namespace $tok [flush2eos $tp]\n"
    } else {
      set nsname [$tp gettok]
      append icode "namespace eval $nsname "
      pushns $nsname

      # things get a little hairy here because the syntax of the namespace
      # eval is : namespace eval NS arg ?arg? ?arg? ...
      # and we treat arg differently if it's a block as opposed to a simple
      # command.
      set arg [$tp gettok]
      set lineno [$tp getlineno]
      
      if { [string index $arg 0] == "\x7b" || [string index $arg 0] == "\"" } {
        # it's a block of code
        if { [string match ?3 $action] } {
          append icode "[doblock3 $lineno $arg]"
        } {
          append icode "[doblock $arg $lineno 0]\n"
        }
      } {
        append icode "$arg [flush2eos $tp]\n"
      }
      
      popns
    }

    if { [string match 1? $action] } {
      return $icode
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $icode]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $icode]
    }
  }

  #
  # Itcl support stuff
  #
  
  # class classname <body>
  #
  proc Tclparser_class {tp action} {
    variable Gfile
    variable insideClass

    set line [$tp getlineno]
    set icode ""

    set insideClass [$tp gettok]
    
    append icode "class $insideClass "
    
    # get the block of code
    set block [$tp gettok]
    
    if { [string match ?3 $action] } {
      append icode "[doblock3 $line $block]"
    } {
      append icode "[doblock $block $line 0]\n"
    }
    
    set insideClass 0

    if { [string match 1? $action] } {
      return $icode
    } elseif { [string match 2? $action] } {
      return [dbg2 $Gfile $line $icode]
    } elseif { [string match 3? $action] } {
      return [dbg3 $Gfile $line $icode]
    }
  }
  
  # constructor args ?init? body
  #
  proc Tclparser_constructor {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set icode ""

    set begline [$tp getlineno]
    
    set args [$tp gettok]
    
    # constructors can have an init block as well as a code block, so
    # try to get them both
    set init [$tp gettok]
    set body [$tp gettok]
    
    if { $body == [geteos] } {
      set body $init
      set init ""
    }
    
    set dynaproc 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }

    # stick the namespace on it
    set procname ${insideClass}::constructor

    set proccmd "constructor"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd procname $dynaproc]
    } else {
      set procaction $Gprocaction
    }

    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }

      11 {
        # save the proc definition as is
        set icode "$proccmd $args $init $body\n"
      }

      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $args $init $body"]
      }

      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $args $init $body"]
      }

      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd $args "
        set extra ""

        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $Gprocentryaction == 1 } {
              append extra "  ${Gclientns}::procEntry $procname\n"
            }

            if { $Gprocexitaction == 1 } {
              append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $procname}\n"
            }

            if { $init != "" } {
              append icode [doblock $init $begline 0 ""]
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            if { $init != "" } {
              append icode [doblock3 $begline $init]
            }
            append icode [doblock3 $begline $body]
          }
        }

        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }

      default {
        error "unknown procaction: '$procaction'"
      }
    }
    
    return $icode
  }
  
  # destructor body
  #
  proc Tclparser_destructor {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set icode ""

    set begline [$tp getlineno]
    
    set body [$tp gettok]
    
    set dynaproc 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }

    # stick the namespace on it
    set procname ${insideClass}::destructor

    set proccmd "destructor"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd procname $dynaproc]
    } else {
      set procaction $Gprocaction
    }

    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }

      11 {
        # save the proc definition as is
        set icode "$proccmd $body\n"
      }

      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $body"]
      }

      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $body"]
      }

      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd "
        set extra ""

        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $Gprocentryaction == 1 } {
              append extra "  ${Gclientns}::procEntry $procname\n"
            }

            if { $Gprocexitaction == 1 } {
              append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $procname}\n"
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            append icode [doblock3 $begline $body]
          }
        }

        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }

      default {
        error "unknown procaction: '$procaction'"
      }
    }
    
    return $icode
  }
  
  proc Tclparser_private {tp action} {
    # things get a little hairy here because the syntax of this 
    # is : private arg
    # and we treat arg differently if it's a block as opposed to a simple
    # statement.
    set arg [$tp gettok]
    set lineno [$tp getlineno]
    set icode "private "

    if { [string index $arg 0] == "\x7b" || [string index $arg 0] == "\"" } {
      # it's a block of code
      if { [string match ?3 $action] } {
        append icode "[doblock3 $lineno $arg]"
      } {
        append icode "[doblock $arg $lineno 0]\n"
      }
    } {
      append icode "[parse $arg $tp]\n"
    }
    
    return $icode
  }
  
  proc Tclparser_public {tp action} {
    # things get a little hairy here because the syntax of this 
    # is : public arg
    # and we treat arg differently if it's a block as opposed to a simple
    # statement.
    set arg [$tp gettok]
    set lineno [$tp getlineno]
    set icode "public "

    if { [string index $arg 0] == "\x7b" || [string index $arg 0] == "\"" } {
      # it's a block of code
      if { [string match ?3 $action] } {
        append icode "[doblock3 $lineno $arg]"
      } {
        append icode "[doblock $arg $lineno 0]\n"
      }
    } {
      append icode "[parse $arg $tp]\n"
    }
    
    return $icode
  }
  
  proc Tclparser_protected {tp action} {
    # things get a little hairy here because the syntax of this 
    # is : protected arg
    # and we treat arg differently if it's a block as opposed to a simple
    # statement.
    set arg [$tp gettok]
    set lineno [$tp getlineno]
    set icode "protected "

    if { [string index $arg 0] == "\x7b" || [string index $arg 0] == "\"" } {
      # it's a block of code
      if { [string match ?3 $action] } {
        append icode "[doblock3 $lineno $arg]"
      } {
        append icode "[doblock $arg $lineno 0]\n"
      }
    } {
      append icode "[parse $arg $tp]\n"
    }
    
    return $icode
  }
  
  # method name ?args? ?body?
  #
  proc Tclparser_method {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set procentryaction $Gprocentryaction
    set procexitaction $Gprocexitaction
    
    set icode ""
    set body ""

    set begline [$tp getlineno]
    
    set procname [$tp gettok]
    set args [$tp gettok]
    if { $args != [geteos] } {
      set body [$tp gettok]
      if { $body == [geteos] } {
        set body ""
        set procentryaction 0
        set procexitaction 0
      }
    } else {
      set args "{}"
      set procentryaction 0
      set procexitaction 0
    }
    
    set dynaproc 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }

    set proccmd "method"
    set fullname "${insideClass}::$procname"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd fullname $dynaproc]
    } else {
      set procaction $Gprocaction
    }

    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }

      11 {
        # save the proc definition as is
        set icode "$proccmd $procname $args $body\n"
      }

      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $procname $args $body"]
      }

      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $procname $args $body"]
      }

      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd $procname $args "
        set extra ""

        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $procentryaction == 1 } {
              append extra "  ${Gclientns}::procEntry $fullname\n"
            }

            if { $procexitaction == 1 } {
              append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $fullname}\n"
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            append icode [doblock3 $begline $body]
          }
        }

        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }

      default {
        error "unknown procaction: '$procaction'"
      }
    }
    
    return $icode
  }
  
  # body classname::function args body
  #
  proc Tclparser_body {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set icode ""

    set begline [$tp getlineno]
    
    set procname [$tp gettok]
    set args [$tp gettok]
    if { $args != [geteos] } {
      set body [$tp gettok]
    }
    
    set dynaproc 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }

    set proccmd "body"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd procname $dynaproc]
    } else {
      set procaction $Gprocaction
    }

    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }

      11 {
        # save the proc definition as is
        set icode "$proccmd $procname $args $body\n"
      }

      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $procname $args $body"]
      }

      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $procname $args $body"]
      }

      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd $procname $args "
        set extra ""

        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $Gprocentryaction == 1 } {
              append extra "  ${Gclientns}::procEntry $procname\n"
            }

            if { $Gprocexitaction == 1 } {
              append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $procname}\n"
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            append icode [doblock3 $begline $body]
          }
        }

        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }

      default {
        error "unknown procaction: '$procaction'"
      }
    }
    
    return $icode
  }
  
  proc Tclparser_configbody {tp action} {
    variable Gfile
    variable blocklevel
    variable Gclientns
    variable Gprocaction
    variable Gprocentryaction
    variable Gprocexitaction
    variable insideClass
    
    set icode ""

    set begline [$tp getlineno]
    
    set procname [$tp gettok]
    set body [$tp gettok]
    
    set dynaproc 0
    set dynabody 0

    # look to see if this is a dynamic proc
    # 2. the body is not in braces
    set p [string index $body 0]
    if { $p == "\$" || $p == "\[" || $p == "\"" } {
      set dynaproc 1
      set dynabody 1
    }

    set proccmd "configbody"

    # see what the client wants to do with this thing
    if { $Gprocaction == "?" } {
      set procaction [${Gclientns}::foundProc $Gfile $begline proccmd procname $dynaproc]
    } else {
      set procaction $Gprocaction
    }

    switch -glob $procaction {
      0? {
        # ignore the proc definition
        flush2eos $tp
        set icode ""
      }

      11 {
        # save the proc definition as is
        set icode "$proccmd $procname $body\n"
      }

      21 {
        # instrument top level only
        set icode [dbg2 $Gfile $begline "$proccmd $procname $body"]
      }

      31 {
        # run time instrumentation of toplevel only
        set icode [dbg3 $Gfile $begline "$proccmd $procname $body"]
      }

      12 -
      22 -
      32 -
      13 -
      23 -
      33 {
        # instrument body at least
        set icode "$proccmd $procname "
        set extra ""

        if { [string match @* $body] } {
          # more itcl support stuff
          append icode " $body"
        } else {
          if { ! [string match ?3 $procaction] } {
            if { $Gprocentryaction == 1 } {
              append extra "  ${Gclientns}::procEntry $procname\n"
            }

            if { $Gprocexitaction == 1 } {
              append extra "  set __.__ {};trace variable __.__ u {[namespace current]::vartracehandler $procname}\n"
            }

            append icode [doblock $body $begline 0 $extra]
          } else {
            append icode [doblock3 $begline $body]
          }
        }

        # how about the toplevel?
        if { [string match 2? $procaction] } {
          set icode [dbg2 $Gfile $begline $icode]
        } elseif { [string match 3? $procaction] } {
          set icode [dbg3 $Gfile $begline $icode]
        }
      }

      default {
        error "unknown procaction: '$procaction'"
      }
    }
    
    return $icode
  }
}
