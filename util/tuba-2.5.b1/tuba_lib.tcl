#  tuba_lib.tcl - debugger core functionality
#  Copyright (C) 1997, 1998 John E. Stump
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba

set VERSION 2.4.p1

# make sure we have Tcl 8
if { [catch {package require Tcl 8} rc] } {
  puts stderr $rc
  exit 99
}
unset rc

namespace eval ::tuba {
  set debug 0

  set tubalibdir .
  
  variable error_action ""
  
  # states are stop step run nodebugger
  set tuba(state) stop
  set tuba(systemfiles) 1
  set tuba(curfile) ""
  set tuba(curline) 0
  set tuba(stack) ""
  set tuba(level) -1
  set tuba(sourcelevel) 0
  set tuba(needstacknotify) 0
  set tuba(animate) 0
  set tuba(scriptfile) ""
  set tuba(dumpflag) 0
  set tuba(interrs) 1
  set tuba(intcaughterrs) 0
  set tuba(dynprocs) ask
  set tuba(coreglobals) 1
  set tuba(instrumenting) LT
  set tuba(parser) T
  set tuba(exclusions) ""
  
  variable cont 0
  
  # the home array identifies current debugger position stuff
  set home(file) ""
  set home(line) ""
  set home(proc) ""
  set home(level) 0
  set home(sourcelevel) 0
  
  variable sourcelevel 0
  variable catchlevel 0
  
  variable level_stack ""
  variable tuba_stack ""
  
  variable windows 0
  
  # array to hold breakpoints
  variable bps
  
  # breakpoints per line (includes procs too)
  variable bpsperline

  # array to hold proc breakpoints
  variable pbps
  
  # this is an array of possible breakpoint targets. Entries of {} are simple 
  # line BPs, whereas a name is a proc at that file,line
  set bptargets() ""
  
  # lists of available variables
  variable visvars ""
  variable glovars ""
  
  # list of globals defined in the interpreter core
  variable coreglobals ""
  
  # array of available procs
  variable procarray
  
  # return code for the completed program. If this is set to something, then
  # you know the program has completed (or in an event loop. use progdone to
  # know when the program is really done.).
  variable rccode "0"
  
  variable progdone 0
  
  # current program
  variable Gprogram ""
  
  proc debug {msg} {
    variable debug
    if { $debug } {
      puts $msg
    }
  }

  proc procheader {} {
    uplevel {
      ::tuba::push [lindex [info level 0] 0]
      set __tuba__ {}
      trace variable ___tuba___ u ::tuba::pop
    }
  }
  
  variable cmdqueue {}
  proc dequeue {} {
    variable cmdqueue
    set copy $cmdqueue
    set cmdqueue ""
    foreach msg $copy {
      debug "APPL processing message '$msg'"
      switch [lindex $msg 0] {
        STACKCMD {
          # command to execute in the debugger core
          if { [::original_catch {uplevel #[lindex $msg 1] [lrange $msg 2 end]} rc] } {
            sendcmd internal_error $rc
          }
        }

        STACKCMD_WR {
          set id [lindex $msg 1]
          set level [lindex $msg 2]
          if { [::original_catch {set rc [uplevel #$level [lindex $msg 3] [lrange $msg 4 end]]} err] } {
            sendcmd internal_error $err
          } else {
            sendmsg RETURN $id $rc
          }
        }
      }
    }
  }
  
  # sends a generic message to the ui server
  proc sendmsg {msg args} {
    variable ui_sock
    debug "APPL-> UI: '$msg $args'"; flush stdout

    puts $ui_sock [string length "$msg $args"]
    puts -nonewline $ui_sock "$msg $args"
    flush $ui_sock
  }
  
  # sends a CMD message to the ui server
  proc sendcmd {cmd args} {
    eval sendmsg CMD $cmd $args
    vwait ::tuba::ACK
  }

  # this sendcmd expects a return from the remote command
  proc sendcmd_wr {cmd args} {
    set varname [getuniqvarname]

    eval sendmsg CMD_WR $varname $cmd $args
    vwait $varname

    upvar #0 $varname v
    return $v
  }
  
  variable cmd_wr_rc_index 0
  proc getuniqvarname {} {
    variable cmd_wr_rc_index
    incr cmd_wr_rc_index
    upvar #0 ::tuba::cmd_wr_$cmd_wr_rc_index v
    set v ""
    return ::tuba::cmd_wr_$cmd_wr_rc_index
  }
  
  # OBSERVER CODE
  # This allows code to register itself with various components of the tuba
  # core library to observe various events. The following components are
  # observable: bps, pbps, vars, procs, and stack.
  # The events we notify the observers with are: update and destroy. Update
  # means that something has changed in the data and the observer should
  # update himself.
  # Destroy means that the observation is being removed and the observer
  # should do whatever he has to do in light of that.
  #
  # First an observer must register himself.
  # Component identifies which component of the core does it want to observe
  # Command is the Tcl code to execute to pass the event.
  # Returns an id to be used to unregister.
  #
  # observers data structure. Each array index is an identifier to a specific
  # observer. Indexes would be bps, pbps, vars, procs, ...
  variable observers
  array set observers {bps {} pbps {} vars {} procs {} stack {} var {} home {}}
  variable observerids
  array set observerids {bps 0 pbps 0 vars 0 procs 0 stack 0 var 0 home 0}
  
  proc register_observer {component command} {
    variable observers
    variable observerids
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata
    
    set index $observerids($component)
    
    set comp($index) $command
    incr observerids($component)

    set observers($component) [array get comp]
    
    return $index
  }
  
  # this is used to unregister yourself as an observer of the given
  # component. Use the id passed back from register_observer
  #
  proc unregister_observer {component id} {
    variable observers
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata
    
    ::original_catch {unset comp($id)}
    
    set observers($component) [array get comp]
  }
  
  # notify all the observers of the given component with the given event
  #
  proc notify_observers {component event args} {
    variable observers
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata

    foreach observer [array names comp] {
      if { [lindex $comp($observer) 0] == "pause" } continue
      eval sendcmd $comp($observer) $event $args
    }

  }
  
  # remove all observers from the given component. this will send the destroy
  # event to them first
  #
  proc remove_observers {component} {
    variable observers
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata
    
    foreach observerid [array names comp] {
      # send them all the destroy
      sendcmd $comp($observerid) destroy
      
      # and remove them from the list
      #unregister_observer $component $observerid
    }
  }
  
  # pause the notification for the given observer
  proc pause_observer {component id} {
    variable observers
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata
    
    # add the "pause" element
    set comp($id) [list "pause" $comp($id)]
    set observers($component) [array get comp]
  }
  
  proc unpause_observer {component id} {
    variable observers
    
    # get the array get info
    set arraydata $observers($component)
    array set comp $arraydata
    
    # remove the "pause" element
    set comp($id) [lindex $comp($id) 1]
    set observers($component) [array get comp]
  }
  
  # this is a special register that observes a variable. Events are u for
  # unset and w for write.
  #
  proc register_var_observer {var level cmd} {
    variable observers
        
    # get the array get info
    set arraydata $observers(var)
    array set comp $arraydata
    
    # next find out if it's an array
    set arrflag [uplevel #$level ::tuba::isarray $var]
    
    # setup the trace
    set id [vtrace $var $level uw [list ::tuba::notify_var_observers $arrflag $level $cmd]]
    
    # add the id to our list
    set comp($id) $cmd
    set observers(var) [array get comp]

    # return the id. The id will be used to remove the trace later
    return $id
  }
  
  proc unregister_var_observer {id} {
    variable observers
        
    # get the array get info
    array set comp $observers(var)
    # remove the variable trace (catch it because the observer may have
    # already unregistered)
    ::original_catch {vdelete $id} rc
    debug $rc
    # remove the id from our list
    if { [::original_catch {unset comp($id)} rc] } {
      puts "Internal Error: Error in unregistering var observer with id: '$id'\n$rc"
      return
    }
    set observers(var) [array get comp]
  }
  
  proc remove_var_observers {} {
    variable observers

    # get the array get info
    set arraydata $observers(var)
    array set comp $arraydata
    
    foreach observerid [array names comp] {
      # send them all the destroy
      sendcmd $comp($observerid) $observerid 0 0 "destroy"
      
      # and remove them from the list
      unregister_var_observer $observerid
    }
    set observers(var) [array get comp]
  }
  
  # this is called by Tcl because it was registered via a trace variable cmd
  proc notify_var_observers {arrflag level cmd var1 var2 op} {
    # call the supplied notification command passing the variable, the array
    # flag, and the event
    if { $op != "u" } {
      if { $arrflag } {
        sendcmd $cmd $var1 $level $arrflag $op [uplevel #$level array get $var1]
      } {
        sendcmd $cmd $var1 $level $arrflag $op [uplevel #$level set $var1]
      }
    } {
      sendcmd $cmd $var1 $level $arrflag $op
    }
  }
  
  proc pause_var_observer {id} {
    vpause $id
  }
  
  proc unpause_var_observer {id} {
    vunpause $id
  }
  
  # END OBSERVER CODE
  
  # this code simplifies the Tcl trace facilities
  variable traceids
  variable traceid 0
  # create a variable trace on the given var at the given level.
  # returns an id to be used later with vdelete and vinfo
  proc vtrace {var level ops cmd} {
    variable traceids
    variable traceid
    set id $traceid
    incr traceid
    set traceids($id) [list $var $level $cmd [list $var $ops $cmd]]
    uplevel #$level trace variable $var $ops [list $cmd]
    return $id
  }
  
  # delete the variable trace identified by the id
  proc vdelete {id} {
    variable traceids
    
    if { ! [info exists traceids($id)] } return

    uplevel #[lindex $traceids($id) 1] trace vdelete [lindex $traceids($id) 3]
    unset traceids($id)
  }
  
  # returns the trace info for the given id:
  # var level cmd {vdelete stuff}
  proc vinfo {id} {
    variable traceids
    
    if { ! [info exists traceids($id)] } return
    return $traceids($id)
  }
  
  # pauses the tracing of a variable
  proc vpause {id} {
    variable traceids
    if { ! [info exists traceids($id)] } return
    
    # remove the trace
    uplevel #[lindex $traceids($id) 1] trace vdelete [lindex $traceids($id) 3]
  }
  
  # starts up tracing a variable again
  proc vunpause {id} {
    variable traceids
    if { ! [info exists traceids($id)] } return
    
    # crank up the variable trace again
    uplevel #[lindex $traceids($id) 1] trace variable [lindex $traceids($id) 3]
  }
  
  proc infosourcelevel {} {
    variable sourcelevel
    return $sourcelevel
  }

  # this maintains the call stack
  proc push {procname} {
    variable tuba
    variable pbps
    
    lappend tuba(stack) "$tuba(curfile) $tuba(curline) $procname"
    
    # this is the best place for determining if we hit a proc breakpoint
    if { [info exists pbps($procname)] } {
      # this will force a stop the next time around
      set tuba(state) step
      ui_setmsg "hit proc breakpoint in $procname"
    }
    
    # set the need stack change notification sent
    set tuba(needstacknotify) 1
  }

  # this maintains the call stack
  proc pop {args} {
    variable tuba
    set tuba(stack) [lreplace $tuba(stack) end end]

    # set the need stack change notification sent
    set tuba(needstacknotify) 1
  }

  # when an exception is thrown, we will catch it in the same proc (or
  # the nearest instrumented proc). We'll block in this proc awaiting for
  # a command to either abort or continue. In the meantime, we can acccept
  # requests for stack info, variable values, etc.
  proc handle_error {rc msg} {
    variable tuba
    variable home
    variable catchlevel
    global errorCode errorInfo
    
    if { $rc != 1 } { return -code $rc $msg }
    
    if { [string first "handle_error \$rc \$msg" $errorInfo] != -1 || 
         $tuba(interrs) == 0 } {
      # let it go, we've already caught this or the app has a catch
      return -code error -errorinfo $errorInfo $msg
    }

    # is the app going to catch this?
    if { $tuba(intcaughterrs) == 0 && $catchlevel > 0 } {
      return -code error -errorinfo $errorInfo $msg
    }
    
    # get all the variables the debugger could watch
    grabvars
    
    # do we need to notify any stack observers?
    if { $tuba(needstacknotify) } {
      variable level_stack
      variable tuba_stack
      build_level_stack
      build_tuba_stack
      notify_observers stack update $level_stack $tuba_stack
      
      set tuba(needstacknotify) 0
    }

    # the last line we ran is in tuba()
    set file $tuba(curfile)
    set line $tuba(curline)

    set home(proc) [lindex [lindex [lrange $tuba(stack) end end] 0] 2]
    set home(file) $file
    set home(line) $line

    # now we can update the stat bar
    notify_observers home update [array get home]

    sendcmd getErrorAction $file $line $msg

    eventloop
    
    variable error_action
    if { $error_action == "cont" } {
      # throw the error back
      return -code error -errorinfo $errorInfo $msg 
    } elseif { $error_action == "ign" } {
      return -code return
    } else {
      # what do we do here? some kind of internal error
    }
  }
  
  # callbacks for the instrumenter
  proc foundCommand {file line cmdvar} {
    # we need to register the line number as a possible break point target
    variable bptargets
    if { [info exists bptargets($file,$line)] } {
      if { [lsearch -exact $bptargets($file,$line) {}] == -1} {
        lappend bptargets($file,$line) {}
      }
    } else {
      lappend bptargets($file,$line) {}
    }
    
    variable bpsperline
    set bpsperline($file,$line) 0
    
    # determine the instrumentation method
    variable tuba
    if { $tuba(instrumenting) == "RT" } {
      return 23
    } {
      return 22
    }
  }
  
  proc isProcExcluded {proc} {
    variable tuba
    
    foreach ex $tuba(exclusions) {
      if { [string match $ex $proc] } {
        return 1
      }
    }
    
    return 0
  }
  
  proc foundProc {file line cmdvar namevar dynaprocflag} {
    variable procarray
    variable bptargets
    variable bpsperline
    variable tuba
    upvar $namevar name
    upvar $cmdvar cmd
 
    # are we exclusing this proc?
    if { [isProcExcluded $name] } {
      return 11
    }

    # add the proc to our array of known procs
    if { $dynaprocflag == 0 } {
      set procarray($name) [list $file $line]
      
      # also save the proc by file,line
      lappend bptargets($file,$line) $name
      set bpsperline($file,$line) 0
      
      # JES notify an observer here?
      # JES no, probably would create too much communication
    } else {
      set action $tuba(dynprocs)
      if { $action == "ask" } {
        # ask the user what to do
        set ans [sendcmd_wr getDynaprocAction $file $line]
        switch $ans {
          loadtime {
            set action loadtime
          }
          LOADTIME {
            set action loadtime
            set tuba(dynprocs) loadtime
          }
          runtime {
            set action runtime
          }
          RUNTIME {
            set action runtime
            set tuba(dynprocs) runtime
          }
          default {
            sendcmd internal_error "unknown answer from getDynaprocAction: '$ans'"
          }
        }
      }
      
      if { $action == "loadtime" } {
        lappend bptargets($file,$line) $name
        set bpsperline($file,$line) 0
        return 22
      } else {
        return 23
      }
    }
    
    # determine the instrumentation method
    if { $tuba(instrumenting) == "RT" } {
      return 23
    } {
      # if the proc is being defined at a level > 0 then put a callback there
      if { [instrumenter::getLevel] > 0  && $cmd == "proc" } {
        return 22
      } else {
        return 12
      }
    }
  }
  
  proc useCache {file} {
    # get the client data for this file
    array set data [instrumenter::getClientData $file]
    
    # compare with our current settings
    variable tuba
    global VERSION
    
    if { ! [info exists data(dynprocs)] || $data(dynprocs) != $tuba(dynprocs) } {
      return 0
    }
    
    if { ! [info exists data(parser)] || $data(parser) != $tuba(parser) } {
      return 0
    }
    
    if { ! [info exists data(instrumenting)] || $data(instrumenting) != $tuba(instrumenting) } {
      return 0
    }
    
    if { ! [info exists data(version)] || $data(version) != $VERSION } {
      return 0
    }
    
    if { ! [info exists data(exclusions)] || [lsort $data(exclusions)] != [lsort $tuba(exclusions)] } {
      return 0
    }
    
    return 1
  }
  
  proc procEntry {procname} {
    #uplevel {::tuba::push [lindex [info level 0] 0]}
    push $procname
  }
  
  proc procExit {procname} {
    ::tuba::pop
  }
  
  proc statement {file line stmt} {
    variable tuba 
    variable bps 
    variable bold
    variable unbold
    variable home

    set home(level) [uplevel info level]
    set home(sourcelevel) [infosourcelevel]
  
    # push needs this information
    set tuba(curline) $line
    set tuba(curfile) $file
    
    if { $tuba(animate) } {
      # update the stat line
      set home(proc) [lindex [lindex [lrange $tuba(stack) end end] 0] 2]
      set home(file) $file
      set home(line) $line
      notify_observers home update [array get home]
      
      # update the source code display
      ui_showline $file $line
    } 

    # handle any pending events. This is strictly to see if the user has 
    # hit the stop button
    #adv_update file 0
    # fix for defect #12. We can't use adv_update because we may be
    # debugging a file event handler, and adv_update will cause an
    # infinite loop.
    variable stop_sock
    if { [gets $stop_sock] == "STOP" } {
      stop
    }

    if { $tuba(state) == "run" } {
      while {1} {
        # any breakpoints?
        if { [info exists bps($file,$line)] } {
          ui_setmsg "Hit breakpoint on $file:$line"
          break
        }

        # any level breakpoints?
        if { $home(level) <= $tuba(level) &&
             $home(sourcelevel) <= $tuba(sourcelevel) }   break

        # if we got here, there's no reason to stop
        if { $stmt != "" } {
          set rc [::original_catch {uplevel $stmt} msg]
          if { $rc != 0 } {
            if { $rc != 1 } {
              return -code $rc $msg
            } elseif { $tuba(interrs) } {
                handle_error $rc $msg
            } else {
              # rethrow it
              return -code $rc $msg
            }
          } else {
              return $msg
          }
        } else {
          return
        }
      }
    }

    # get the rest of the home information
    set home(proc) [lindex [lindex [lrange $tuba(stack) end end] 0] 2]
    set home(file) $file
    set home(line) $line

    # now we can update the stat bar
    notify_observers home update [array get home]
    
    set tuba(level) -1
    set tuba(sourcelevel) 0

    # get all the variables the debugger could watch
    grabvars
    
    # do we need to notify any stack observers?
    if { $tuba(needstacknotify) } {
      variable level_stack
      variable tuba_stack
      build_level_stack
      build_tuba_stack
      notify_observers stack update $level_stack $tuba_stack
      
      set tuba(needstacknotify) 0
    }

    ui_showline $file $line

    eventloop
    
    if { $stmt != "" } {
      set rc [::original_catch {uplevel $stmt} msg]
      if { $rc != 0 } {
        if { $rc != 1 } {
          return -code $rc $msg
        } elseif { $tuba(interrs) } {
            handle_error $rc $msg
        } else {
          # rethrow it
          return -code $rc $msg
        }
      } else {
          return $msg
      }
    } else {
      return
    }
  }
  
  # wait until the variable cont is set
  proc eventloop {} {
    variable ui_sock
    
    # we'll block here while waiting for something from the ui
    # (we have to handle the message in the scope of the debugger callback
    # because we may need to look at local variables or execute some code
    # in this scope. Letting the fileevent handle the message won't work
    # because he executes at a different scope.)
    variable cont
    
    set oldcont $cont
    
    while {1} {
      # process any commands that may have come in since we left
      dequeue
      
      # wait for new commands
      vwait ::tuba::msg
      
      # now process those
      dequeue
      
      # if we've come across a command that causes the debugger to continue
      # this will break
      if { $oldcont != $cont } break
    }
  }
  
  proc grabvars {} {
    variable glovars
    variable visvars
    variable tuba
    variable home
    variable coreglobals

    # save old vars to compare with new to see if any changes
    set oldglovars $glovars; set glovars ""
    
    foreach var [uplevel #$home(level) info globals] {
      if { $var != "" && $var != "__tuba__" } {
        # do we want to see core globals?
        if { $tuba(coreglobals) == 0 } {
          if { [lsearch -exact $coreglobals $var] != -1 } continue
        }
        
        lappend glovars $var
      }
    }
    

    set oldvisvars $visvars; set visvars ""
    foreach var [ uplevel #$home(level) info vars] {
      if { $var != "" && $var != "__.__" } {
        # do we want to see core globals?
        if { $home(level) == 0 && $tuba(coreglobals) == 0 } {
          if { [lsearch -exact $coreglobals $var] != -1 } continue
        }
        
        lappend visvars $var
      }
    }

    # only notify the observers if there is a change in vars
    if { $oldglovars != $glovars || $oldvisvars != $visvars } {
      notify_observers vars update $visvars $glovars
    }
  }

  proc get_visvars {} {
    variable visvars
    return $visvars
  }
  proc get_glovars {} {
    variable glovars
    return $glovars
  }
  
  # looks to see if the variable is an array
  # this is expected to be run with an uplevel
  proc isarray {varname} {
    return [uplevel array exists $varname]
  }
  
  proc get_home {} {
    variable home
    return [array get home]
  }
  
  proc get_level {} {
    variable home
    return $home(level)
  }
  
  # debug routine
  proc showstack {} {
    puts "STACK:"
    uplevel {
    for { set l [info level]} {$l > 0} {incr l -1} {
      puts "$l: [info level $l]"
    }
    }
    puts "END STACK"
  }

  proc get_var {varname} {
    return [uplevel set $varname]
  }
  
  proc error_abort {} {
    exit 99
  }
  proc error_cont {} {
    variable error_action
    
    set error_action "cont"
    cont_debugging
  }
  proc error_ign {} {
    variable error_action
    
    set error_action "ign"
    cont_debugging
  }
  
  proc cont_debugging {} {
    variable cont
    
    # this modifies the cont variable, which breaks the vwait command
    # in the eventloop, so the debugger can continue
    incr cont
  }
  
  proc breakpoint {} {
    variable tuba
    set tuba(state) step
  }
  
  proc step {} {
    variable tuba
    set tuba(state) step
    sendcmd removecurrline
    #ui_setmsg "step"
    cont_debugging
  }

  proc step_over {} {
    variable tuba
    variable home
    set tuba(state) run
    set tuba(level) $home(level)
    set tuba(sourcelevel) [infosourcelevel]
    sendcmd removecurrline
    #ui_setmsg "step over"
    cont_debugging
  }

  proc stop {} {
    variable tuba
    set tuba(state) step
    #ui_setmsg "stop"
  }
  
  proc finish_proc {} {
    variable tuba
    variable home
    # finish current level (continue until end of proc)
    set tuba(state) run
    set tuba(level) [expr {$home(level) - 1}]
    set tuba(sourcelevel) [infosourcelevel]
    sendcmd removecurrline
    #ui_setmsg "finish proc"
    cont_debugging
  }

  proc finish_source {} {
    variable tuba
    variable
    set tuba(state) run
    set tuba(level) 10000
    set tuba(sourcelevel) [expr {[infosourcelevel] - 1}]
    sendcmd removecurrline
    #ui_setmsg "finish source command"
    cont_debugging
  }

  proc cont {} {
    variable tuba
    set tuba(state) run
    sendcmd removecurrline
    #ui_setmsg continue
    cont_debugging
  }

  proc finish {} {
    variable tuba
    
    # remove debugger and run full speed (almost)
    # do this by redefining the debugger callback routine
    proc ::tuba::statement {file line stmt} {
      uplevel $stmt
    }

    set tuba(state) nodebug
    
    # remove any variable traces
    remove_var_observers
    sendcmd removecurrline
    cont_debugging
  }

  # returns 1 if we set a bp at this point, 0 if we removed one
  proc toggle_bp {file line {force 0}} {
    variable bps
    variable bpsperline
    variable tuba

    # first look to see if we are allowed to do anything on this line
    
    if { [info exists bps($file,$line)] } {
      # remove the bp
      unset bps($file,$line)
      ui_setmsg "removing breakpoint from $file:$line"
      
      # decrement the number of BPs on this line
      incr bpsperline($file,$line) -1
      
      # any observers?
      notify_observers bps update [array names bps]
      
      return 0
    } else {
      # it's either not a loaded file or a valid breakpoint spot, so
      # create the bp
      set bps($file,$line) ""
      ui_setmsg "setting breakpoint at $file:$line"
      
      # increment the number of BPs on this line
      if { [info exists bpsperline($file,$line)] } {
        incr bpsperline($file,$line)
      } else {
        set bpsperline($file,$line) 1
      }
      
      # any observers?
      notify_observers bps update [array names bps]
      
      return 1
    }
  }
  
  proc get_bptargets {file line} {
    variable bptargets
    if { [info exists bptargets($file,$line)] } {
      return $bptargets($file,$line)
    } else {
      return ""
    }
  }
  
  proc get_bps {} {
    variable bps
    return [array names bps]
  }
  
  proc get_pbps {} {
    variable pbps
    return [array get pbps]
  }
  
  proc is_pbp {proc} {
    variable pbps
    return [info exists pbps($proc)]
  }
  
  proc get_procs {} {
    variable procarray
    return [array names procarray]
  }
  
  proc get_procinfo {proc} {
    variable procarray
    if { [info exists procarray($proc)] } {
      return $procarray($proc)
    } else {
      return ""
    }
  }
  
  # returns 1 if we set a bp at this point, 0 if we removed one
  proc toggle_pbp {proc file line} {
    variable pbps
    variable bpsperline

    # is this or will this proc be excluded?
    if { [isProcExcluded $proc] } {
      ui_setmsg "proc $proc is excluded; no breakpoint set!"
      return 0
    }
    
    if { [info exists pbps($proc)] } {
      # remove the bp
      unset pbps($proc)
      ui_setmsg "Removed breakpoint from procedure $proc"
      
      incr bpsperline($file,$line) -1
      
      # any observers?
      notify_observers pbps update [array get pbps]
      
      return 0
    } else {
      # create the bp
      set pbps($proc) [list $file $line]
      ui_setmsg "Set breakpoint in procedure $proc"

      if { [info exists bpsperline($file,$line)] } {
        incr bpsperline($file,$line)
      } else {
        set bpsperline($file,$line) 1
      }
      
      # any observers?
      notify_observers pbps update [array get pbps]
      
      return 1
    }
  }
  
  proc getBPCount {file line} {
    variable bpsperline
    if { [info exists bpsperline($file,$line)] } {
      return $bpsperline($file,$line)
    } else {
      return 0
    }
  }
  

  proc turnOnTubaSource {} {
    if { [info commands ::original_source] == "" } {
      rename ::source ::original_source
      interp alias {} ::source {} ::tuba::tubasource
    }
  }
  
  proc turnOffTubaSource {} {
    if { [info commands ::original_source] != "" } {
      rename ::source {}
      rename ::original_source ::source
    }
  }
  
  proc saveClientData {file} {
    # save the data for this instrumentation run
    variable procarray
    variable bptargets
    variable bpsperline
    variable tuba
    global VERSION
    
    set data(procarray) [array get procarray]
    set data(bptargets) [array get bptargets]
    set data(bpsperline) [array get bpsperline]
    set data(parser) $tuba(parser)
    set data(instrumenting) $tuba(instrumenting)
    set data(dynprocs) $tuba(dynprocs)
    set data(exclusions) $tuba(exclusions)
    set data(version) $VERSION

    instrumenter::setClientData $file [array get data]
  }
  
  proc getClientData {file} {
    variable procarray
    variable bptargets
    variable bpsperline

    array set data [instrumenter::getClientData $file]
    array set procarray $data(procarray)
    array set bptargets $data(bptargets)
    array set bpsperline $data(bpsperline)
  }
  
  proc load_program {prog arglist cwd {keepbps 0}} {
    variable ui_sock
    variable rccode
    variable src
    variable currline
    variable tuba
    variable Gprogram
    
    rename exit original_exit
    interp alias {} exit {} ::tuba::tubaexit
    
    # this is to properly return info script info
    rename info ::original_info
    interp alias {} info {} ::tuba::tubainfo
    
    # this is so we can keep track of if we're inside a catch or not
    rename catch ::original_catch
    interp alias {} catch {} ::tuba::tubacatch
    
    # use our own source wrapper
    turnOnTubaSource

    # get the full path name of the program
    set oldcd [pwd]
    cd [file dirname $prog]
    set prog [pwd]/[file tail $prog]
    cd $oldcd
    
    set Gprogram $prog
    set tuba(scriptfile) $prog

    # we're given a tcl file to debug, so let's instrument it first
    ui_setmsg "Instrumenting code... "
    set wascached 0
    set fromcache 0
    set icode [instrumenter::instrument $prog wascached fromcache]

    # was this file cached?
    if { $wascached } {
      saveClientData $prog
    }
    
    # did we get this file from the cache?
    if { $fromcache } {
      # retrieve the client data
      getClientData $prog
    }
    
    ui_setmsg "Instrumenting Done!"
    #puts $icode

    # reset the argv list and argc
    global argv0 argv argc
    set argv0 [list $prog]
    set argv $arglist
    set argc [llength $argv]

    if { $cwd != "" } {
      cd $cwd
    }
    
    set tuba(state) step
    
    set rccode ""
    ui_setmsg "Ready"
    sendmsg READY
    
    # now crank it up
    if { [set rccode [::original_catch {uplevel #0 $icode} r]] } {
      sendcmd internal_error "\nApplication terminated with uncaught error:\n\n'$r'"
      exit 99
    }

    # if this is tk then there might be an eventloop waiting for . to go away
    if { [info commands .] == "." } {
      sendmsg ENDED $rccode loop
      # now we'l wait too
      tkwait window .
      exit 0
    } else {
      sendmsg ENDED $rccode noloop
      variable progdone 1
    }
  }
  
  proc get_level_stack {} {
    variable level_stack
    return $level_stack
  }
  proc build_level_stack {} {
    variable home
    variable level_stack

    set level_stack ""
    for {set level $home(level)} {$level > 0} {incr level -1} {
      set stack [info level $level]
      append level_stack "$stack\n"
    }
  }

  proc get_tuba_stack {} {
    variable tuba_stack
    return $tuba_stack
  }
  proc build_tuba_stack {} {
    variable tuba
    variable tuba_stack

    set tuba_stack ""
    set i [expr {[llength $tuba(stack)] - 1}]
    for {} {$i >= 0} {incr i -1} {
      append tuba_stack "[lindex $tuba(stack) $i]\n"
    }
  }
    
  # reimplement the source command so we can instrument sourced files.
  # source runs at the same level, so we need to maintain a separate source
  # level so that we can "n" (step over) source and package require commands.
  proc tubasource {file} {
    variable debug
    variable sourcelevel
    variable tuba
    global tcl_pkgPath

    # get the full path name of the program
    set oldcd [pwd]
    cd [file dirname $file]
    set file [pwd]/[file tail $file]
    cd $oldcd

    # are we running full speed (via the "C" command) or is this a pkgIndex?
    if { $tuba(state) == "nodebug" || [file tail $file] == "pkgIndex.tcl" } {
      uplevel original_source $file
      return
    }

    # we usually want to skip instrumenting system files, unless user specified
    # the -S option
    if { $tuba(systemfiles) == "0" } {
      # we want to skip system file instrumenting
      foreach libvar [concat [uplevel #0 info vars *_library] tcl_pkgPath] {
        foreach path [uplevel #0 set $libvar] {
          if { [string match $path/* $file] } {
            uplevel original_source $file
            return
          }
        }
      }
    }
    
    set oldscriptfile $tuba(scriptfile)
    set tuba(scriptfile) $file
    ui_setmsg "Instrumenting sourced file $file; Please wait..."

    # save the old procarray so we can compare after the source is done
    variable procarray
    set oldprocarray [array get procarray]
    
    set wascached 0
    set fromcache 0
    set icode [instrumenter::instrument $file wascached fromcache]

    # was the file cached?
    if { $wascached } {
      saveClientData $file
    }
    
    # did we get this file from the cache?
    if { $fromcache } {
      # retrieve the client data
      getClientData $file
    }

    # take a look at the new procarray and see if any procs have been
    # added
    # JES this doesn't work because we're nto guaranteed the order will
    # JES be the same!
    if { $oldprocarray != [array get procarray] } {
      # do we have any observers to inform?
      notify_observers procs update [array names procarray]
    }
    
    ui_setmsg "Running..."
    if { $debug == "TRUE" } {
      puts $icode
      set tuba(scriptfile) $oldscriptfile
    } else {
      incr sourcelevel
      set rc [uplevel $icode]
      incr sourcelevel -1
      set tuba(scriptfile) $oldscriptfile
      return $rc
    }
  }

  proc tubaexit {{rc 0}} {
    variable rccode

    sendmsg EXIT $rc
    set rccode $rc
    
    # should do something here to wait???
    while {1} {
      vwait ::tuba::msg
    }
  }
  
  proc tubainfo {cmd args} {
    variable tuba
    
    if { $cmd == "script" } {
      return $tuba(scriptfile)
    } else {
      return [uplevel ::original_info $cmd $args]
    }
  }
  
  proc tubacatch {script {var ""}} {
    variable catchlevel
    incr catchlevel
    set rc [uplevel ::original_catch [list $script] $var]
    incr catchlevel -1
    return $rc
  }

  proc closeapp {} {
    # remove all the observers
    foreach component [list bps pbps vars procs stack home] {
      remove_observers $component
    }
    
    remove_var_observers
  }
  
  variable ui_sock ""
  variable stop_sock ""
  variable msg ""
  variable ACK 0
  
  proc msgfromui {} {
    variable ui_sock 
    variable msg
    
    if { [eof $ui_sock] } {
      puts "Socket between application and Tuba GUI terminated"
      original_exit
    }
    
    set bytes [gets $ui_sock]
    if { $bytes == "" } return
    set msg [read $ui_sock $bytes]
    
    handle_msg $msg
  }
  
  proc handle_msg {msg} {
    variable ACK
    variable cmdqueue
    
    debug "APPL <- UI: '$msg'"

    switch [lindex $msg 0] {
      ACK {
        incr ACK
      }
      
      CMD {
        # command to execute in the debugger core
        if { [::original_catch {eval [lindex $msg 1] [lrange $msg 2 end]} rc] } {
          sendcmd internal_error $rc
        }
        # send an acknowledgement
        sendmsg ACK

      }

      CMD_WR {
        set id [lindex $msg 1]
        set rc [eval [lindex $msg 2] [lrange $msg 3 end]]
        sendmsg RETURN $id $rc
      }
      
      STACKCMD {
        variable progdone
        # send an acknowledgement
        sendmsg ACK
        
        # have we ended?
        if { $progdone } {
          # our app has ended, so don't queue up commands, execute them now
          if { [::original_catch {uplevel #[lindex $msg 1] [lrange $msg 2 end]} rc] } {
            sendcmd internal_error $rc
          }
        } else {
          debug "(queueing)"

          # command to execute in the debugger core. queue it up so it
          # can be properly executed in the eventloop scope
          lappend cmdqueue $msg
        }
      }
      
      STACKCMD_WR {
        variable progdone
        
        # have we ended?
        if { $progdone } {
          set id [lindex $msg 1]
          set level [lindex $msg 2]
          set rc [uplevel #$level [lindex $msg 3] [lrange $msg 4 end]]
          sendmsg RETURN $id $rc
        } else {
          debug "(queueing)"
          lappend cmdqueue $msg
        }
      }

      CONFIG {
        newconfig [lindex $msg 1]
      }
      
      RETURN {
        # this is a message specifying a return code for a previous
        # CMD_WR message
        set varname [lindex $msg 1]
        upvar #0 $varname v
        set v [lindex [lrange $msg 2 end] 0]
      }
          
      STOP {
        # special stop message to halt at the next debugger callback
        variable tuba
        set tuba(state) stop
      }
      
      EXIT {
        original_exit
      }
      default {
        puts "unknown msg from application to UI: '$msg'"
      }
    }
  }
  
  proc ui_setmsg {mesg} {
    sendcmd setmsg $mesg
  }
  
  proc ui_showline {file line {makecurrent 1}} {
    # we'll wait for the UI to return because we may get way ahead of him.
    sendcmd_wr showline $file $line $makecurrent
  }

  proc getErrorAction {msg} {
    variable tuba
    return [sendcmd_wr getErrorAction $tuba(curfile) $tuba(curline) $msg]
  }
  
  proc newconfig {arraydata} {
    variable tuba 
    variable tubalibdir
    variable debug
    variable windows
    
    array set conf $arraydata
    
    set purgeflag 0
    
    foreach index [array names conf] {
      switch $index {
        tubalibdir {
        }
        
        windows {
          set windows $conf(windows)
        }
        
        parser {
          switch $conf(parser) {
            Tcl {
              set tuba(parser) T
            }
            
            T -
            C -
            TX -
            CX {
              set tuba(parser) $conf(parser)
            }
            
            default {
              sendcmd internal_error "I don't understand the parser specified: '$conf(parser)'"
              return
            }
          }
          
          # now we have to instrument
          set instoptions(parser) $tuba(parser)
        }
        
        instrumenting {
          if { $conf(instrumenting) == "LT" } {
            # load time instrumenting
            set tuba(instrumenting) LT
          } elseif { $conf(instrumenting) == "RT" } {
            # run time instrumenting
            set tuba(instrumenting) RT
          } else {
            sendcmd internal_error "Unknown instrumenting option: '$conf(instrumenting)'"
            return
          }
        }
        
        cachedir {
          set instoptions(cachedir) $conf($index)
        }
        
        cacheflag {
          if { $conf($index) == 1 } {
            set instoptions(cacheflag) ?
          } else {
            set instoptions(cacheflag) 0
          }
        }
        
        purgeflag {
          set purgeflag $conf($index)
        }
        
        homeobservercmd {
          # register the home position observer
          register_observer home $conf(homeobservercmd)
        }
        
        debug {
          set debug $conf($index)
        }
        
        coreglobals -
        systemfiles -
        interrs -
        intcaughterrs -
        exclusions -
        dynprocs -
        animate {
          set tuba($index) $conf($index)
        }
        
        default {
          puts "don't know what the config option '$index' is"
        }
      }
    }
    
    if { [array exists instoptions] } {
      # turn off the tuba source wrapper in case the instrumenter uses
      # the source command
      turnOffTubaSource
      instrumenter::init $tubalibdir [namespace current] [array get instoptions]
      turnOnTubaSource
      
      if { $purgeflag } {
        instrumenter::purgeCache
      }
    }
  }
  
  proc reconfig {animate systemfiles} {
    variable tuba
    
    set tuba(animate) $animate
    set tuba(systemfiles) $systemfiles
  }

  proc main {argv} {
    variable ui_sock
    variable stop_sock
    variable tubalibdir
    variable coreglobals
    
    set argc 0
    set port [lindex $argv $argc]
    set stopport [expr {$port + 1}]
    incr argc
    
    set tubalibdir [lindex $argv $argc]
    global auto_path
    lappend auto_path $tubalibdir
    incr argc

    # get the core globals
    # (auto_load forces auto_index to be defined so it is on the core globals
    # list)
    catch {auto_load}
    set coreglobals [uplevel #0 info globals]
    
    # create the socket to the UI server
    set ui_sock [socket localhost $port]

    # create the socket for catching STOP commands
    set stop_sock [socket localhost $stopport]
    fconfigure $stop_sock -blocking 0
    
    # setup a fileevent handler
    proc accept_events {} "fileevent $ui_sock readable ::tuba::msgfromui"
    proc no_events {} "fileevent $ui_sock readable {}"

    accept_events

    # load in the instrumenter package
    source $tubalibdir/Tclparser_instrumenter.tcl
    
    # initialize instrumenter
    variable instrumenting
    array set parseropts {
      parser T 
      commentaction 00
      commandaction ?
      procaction ?
      RTcommentaction 00
      RTcommandaction ?
      RTprocaction ?
      procentryaction 1
      procexitaction 1
      cacheflag ?
    }
    instrumenter::init $tubalibdir [namespace current] [array get parseropts]
    
    # send an INIT message
    sendmsg INIT

    # wait for the CONFIG comand message to come back
    vwait ::tuba::msg

    load_program [lindex $argv $argc] [lrange $argv [incr argc] end] .
  }
}

if { [info commands bgerror] != "bgerror" } {
  proc bgerror {msg} {
    puts "Appl has detected a background error:\n"
    global errorInfo
    puts $errorInfo
  }
}

::tuba::main $argv
