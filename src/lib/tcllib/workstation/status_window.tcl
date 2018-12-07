package require Bluetcl
package require Iwidgets 4.0
package require Unique

itcl::class status_window {

    inherit base::messagebox
    common ChanInfo
    common fifoID
    common PROC
    common xtermExec "xterm"
    common xtermOptions "-bg white -fg black"
    common xtermHeight "600"
    common xtermWidth  "800"

    constructor {redirectputs args} {
        eval itk_initialize $args
        add_type info white
        add_type INFO white blue
        add_type ERROR white red
        add_type WARNING white red
        add_type RESULT white black
        [gettext] tag raise sel

        array set ChanInfo [list]
        array set PROC [list]

        if { $redirectputs } {
            # redirect stdout to this window
            if { [info command __puts ] == "" } {
                rename puts __puts
            }
            # the main interp redirects puts to the status window
            interp alias {} puts  {} [itcl::code $this puts_alias] {}
        }
    }

    destructor { }

    method display_message {msg {with_jump true}} {
        if {$with_jump} {
            issue "$msg" INFO
	} else {
            issue_without_jump "$msg" INFO
	}
    }

    method puts_alias {s args} {
        	if {[llength $args] > 3} {
		error "invalid arguments"
	}
	set newline 0
        # note that the messagebox widget always inserts a newline so only
        # the __puts paths support this option
	if {[string match "-nonewline" [lindex $args 0]]} {
		set newline 1
		set args [lrange $args 1 end]
	}
	if {[llength $args] == 1} {
		set chan stdout
		set string [lindex $args 0]
	} elseif {[llength $args] == 2} {
		set chan [lindex $args 0]
		set string [lindex $args 1]
	} else {
		error "invalid arguments"
	}

	if [regexp (stdout|stderr) $chan] {
            if { $chan == "stderr" } { set ty ERROR } else { set ty RESULT }
            interp eval $s __puts $chan [list $string]
            
            issue "$string" $ty
        } else {
            if { $newline } {
                interp eval $s __puts -nonewline $chan [list $string]
            } else {
                interp eval $s __puts $chan [list $string]
            }
	}
    }



# return a writable file opend via a named pipe
# When the channel is written, the results are presented to the status window
# When the channel is closed, the closeProc is called

    method open_writable_pipe_out_err { closeProc typeOut typeErr} {
        set fstdout [open_fifo RDONLY]
        set fstderr [open_fifo RDONLY]
        set stdoutChan  [lindex $fstdout 0]
        set stdoutName  [lindex $fstdout 1]
        set stderrChan  [lindex $fstderr 0]
        set stderrName  [lindex $fstderr 1]
        fileevent $stdoutChan readable [itcl::code $this readable_hook_dual $stdoutChan $stderrChan $typeOut $typeErr]
        fileevent $stderrChan readable [itcl::code $this readable_hook_dual $stdoutChan $stderrChan $typeOut $typeErr]

        set ChanInfo($stdoutChan,"NAME") $stdoutName
        set ChanInfo($stderrChan,"NAME") $stderrName
        set ChanInfo($stderrChan,"CLOSE") $closeProc
        return [list $stdoutName $stderrName $stderrChan]
     }

    method open_fifo {mode} {
         if {! [info exists fifoID] } { set fifoID 0 }
         incr fifoID

         set tmp $::Unique::Tmpdir
         set fifoName [file join $tmp [format "fifo_%d_%d" [pid] $fifoID]]

         if {[catch "exec mkfifo -m 700 $fifoName" err] } {
             puts stderr $err
             set readFIFO [open /dev/null "$mode NONBLOCK"]
         } else {
             set readFIFO [open $fifoName "$mode NONBLOCK"]
             fconfigure $readFIFO -blocking 0 -buffering line
         }
         return [list $readFIFO $fifoName]
    }

     method readable_hook_dual { stdoutChan stderrChan typeOut typeErr } {
         set lc1 0
         set more true
         while { $more } {
                 set more false
                 # empty stderr channel
                 while { [gets $stderrChan line] >= 0 } {
                     set more true
                     issue_without_jump "$line" $typeErr
                     incr lc1
                     if { $lc1 > 1000 } {
                         set lc1 0
                         update idletasks
                     }
                 }
             if { [gets $stdoutChan line] >= 0 } {
                 set more true
                 issue_without_jump "$line" $typeOut
                         incr lc1
                         if { $lc1 > 1000 } {
                                 set lc1 0
                                 return
                         }
                 }
         }

         if { [eof $stderrChan] && [eof $stdoutChan] } {
                 set closeProc $ChanInfo($stderrChan,"CLOSE")
                 if { $closeProc != "" } {
                         catch "$closeProc"
                 }
                 close $stderrChan
                 close $stdoutChan
                 set fileName $ChanInfo($stdoutChan,"NAME")
                 if { $fileName != "" } {
                         catch "exec rm -f \"$fileName\"" unused
                 }
                 set fileName $ChanInfo($stderrChan,"NAME")
                 if { $fileName != "" } {
                         catch "exec rm -f \"$fileName\"" unused
                 }
                 unregisterPid $fileName
                 #Unset the hooks
                 unset ChanInfo($stderrChan,"CLOSE")
                 unset ChanInfo($stderrChan,"NAME")
                 unset ChanInfo($stdoutChan,"NAME")
         }
     }
 #  Executes the specified command
 #
 # @param cmd the command to be executed
 # @param closeProc the procedure to be executed when the command
 # execution has finished
 #
     method execute_in_shell { cmd closeProc } {
         if { $cmd == "" } { return }
         set chans [open_writable_pipe_out_err $closeProc RESULT WARNING]
         set outStd [lindex $chans 0]
         set outErr [lindex $chans 1]
         if { [catch "exec > $outStd 2> $outErr sh -x -e -c { $cmd } &" err] } {
                 puts stderr $err
                 set err ""
         } else {
                 set PROC(BUILDPID) $err
                 registerPid $outErr $err
         }
         return $err
     }

 # Executes the command NOT in the background
     method execute_in_shell_wait { cmd closeProc } {
         if { $cmd == "" } { return }
         set chans [open_writable_pipe_out_err $closeProc RESULT WARNING]
         set outStd [lindex $chans 0]
         set outErr [lindex $chans 1]
         set stderrChan [lindex $chans 2]
         if { [catch "exec > $outStd 2> $outErr sh -x -e -c { $cmd } &" err] } {
             puts stderr $err
             set err ""
         } else {
             set PROC(BUILDPID) $err
             registerPid $outErr $err
             vwait [itcl::scope ChanInfo($stderrChan,"CLOSE")]
        }
        return $err
    }

    method registerPid { filename pd } {
        set PROC(CPID,$filename) $pd
    }
    method unregisterPid { fileName } {
        if { [info exists ::PROC(CPID,$fileName)] } {
                set pid $::PROC(CPID,$fileName)
                unset ::PROC(CPID,$fileName)
        }
    }

    proc getChildPids { p } {
        set result [list]
        if { [catch "exec ps h $p" res] } {
                return $result
        }
        while { [llength $p] != 0 } {
            set thisp [lindex $p 0]
            set p [lrange $p 1 end]
            if { $thisp == "" } { continue }
            lappend result $thisp
            if { [catch "exec pgrep -P $thisp" children] } {
                set children [list]
            }
            foreach child $children {
                lappend p $child
            }
        }
        return $result
    }

# @brief Stops build of current project
#
# PIDs are tracked in BSPEC(CPID,filename) array
#
    proc stop {{just_check false}} {
        set procs [list]
        foreach x [array names PROC CPID,*] {
	    lappend procs $PROC($x)
	    if {!$just_check} {
                unset PROC($x)
	    }
        }
        set procs [getChildPids $procs]
	if {$just_check} {
	    if {$procs == ""} {
		return false
	    }
	    return true
	}
        if { $procs != "" } {
                set cmd "kill $procs"
                catch "exec $cmd"
        }
        set PROC(BUILDPID) "killed"
     #   change_menu_toolbar_state
    }


    # start an xterm with cmd as a separate non-tcl window
    # args are valid xterm args
    proc run_in_xterm { cmd args } {
        set xt [format "%s -hold %s %s -e %s &" \
                    $xtermExec \
                    $xtermOptions  $args  $cmd]

        if { [catch "exec $xt" err] } {
            puts stderr "Cannot start xterm.\n$err"
            set pid 0
        } else {
            set pid $err
        }
        return $pid
    }

    # start an xterm with cmd as a top window
    # args -height -width -title
    proc run_in_xterm_top { cmd args } {
        set w [eval iwidgets::shell .xterm_#auto \
                   -height $xtermHeight -width $xtermWidth -title Xterm $args]
        $w activate

        set cs [$w childsite]
        set hi [expr [$w cget -height] - 25]
        set wi [expr [$w cget -width]  - 25]
        set sf [iwidgets::scrolledframe $cs.sf -vscrollmode none -hscrollmode none]
        pack $sf -expand 1 -fill both

        set sub [$sf childsite]
        set cf [frame $sub.f -container 1  -height $hi -width $wi -borderwidth 50]
        pack $cf  -expand 0 -fill none

        set xt [format "%s -hold -into %s -geometry %dx%d %s -e %s &" \
                    $xtermExec [scan [winfo id $cf] %x] \
                    $wi $hi $xtermOptions $cmd]

        if { [catch "exec $xt" err] } {
            puts stderr "Cannot start xterm.\n$err"
            set pid 0
        } else {
            set pid $err
        }

        # reenable scrollbars
        $sf configure -vscrollmode dynamic -hscrollmode dynamic


        # bind window destroy to also kill process
        bind $cf <Destroy> +[itcl::code killxterm  $pid]
        wm protocol $w WM_DELETE_WINDOW "destroy $w"

        check_if_running $pid $w
        update
        return [list $w $pid]
    }


    # start an xterm with cmd as a top window
    # args -height -width
    proc run_in_xterm_win { winname cmd args } {
        set sf [eval iwidgets::scrolledframe $winname#auto -vscrollmode none -hscrollmode none \
                    -height $xtermHeight -width $xtermWidth \
                    $args]
        pack $sf -expand 1 -fill both

        set hi [expr [$sf cget -height] - 25]
        set wi [expr [$sf cget -width]  - 25]

        set sub [$sf childsite]
        set cf [frame $sub.f -container 1  -height $hi -width $wi -borderwidth 50]
        pack $cf  -expand 0 -fill none

        set xt [format "%s -hold -into %s -geometry %dx%d %s -e %s &" \
                    $xtermExec [scan [winfo id $cf] %x] \
                    $wi $hi $xtermOptions $cmd]

        if { [catch "exec $xt" err] } {
            puts stderr "Cannot start xterm.\n$err"
            set pid 0
        } else {
            set pid $err
        }

        # reenable scrollbars
        $sf configure -vscrollmode dynamic -hscrollmode dynamic


        # bind window destroy to also kill process
        bind $cf <Destroy> +[itcl::code killxterm  $pid]
        return [list $sf $pid]
    }


    proc check_if_running {pid win} {
        if { [getChildPids $pid] == [list] } {
            catch "destroy $win"
            return
        }

        after 5000 [itcl::code check_if_running $pid $win]
    }

    proc killxterm { pid } {
        set procs [getChildPids $pid]
        if { $procs != "" } {
            set cmd "kill $procs"
            catch "exec $cmd"
        }
    }

}
