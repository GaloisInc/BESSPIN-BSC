#  Copyright Bluespec Inc. 2009-2011


# ----------
# The emulator control panel graphical interface

package require BSDebug
package require Iwidgets 4.0
package require Redirect

itcl::class emu_viewer {
    inherit itk::Widget
    
    variable status "Initializing"
    variable isRunning false
    variable cyclesRemaining 0
    variable lastCyclesRemaining 0
    variable gui_status "NOT STARTED"
    variable total_cycles_str "0"
    variable total_cycles_enqueued 0
    variable topview ""
    variable loop_count 0
    variable total_cycles_run 0
    variable hw_time 0
    variable is_rdback_on false
    variable is_rdback_on_new false
    variable deadcycles 0
    variable deadreport 2
    variable frame_count_str "0"
    variable bit_count_str "0"
    variable run_cycles 1000000
    variable how_far 0
    variable how_far_incr 2
    variable updatetime 200
    variable emu_cmd "redirect::rdbk"
    variable emu_cmd2 "redirect::emu"
    variable forever_cycles [expr int(pow (2,29))]
    variable this_run 1
    variable tr_elapsed 0
    variable tr_running false
    variable tr_start 0
    variable tr_cycles 0
    variable tr_hz "0 Hz"
    variable speed_change false
    variable frame_addr 0
    variable event ""
    variable _mode 
    variable _no_dbg
    # True if using a non-manual test bench, false for manual or no TB
    variable _tb
    # True if using a manual test bench
    variable _mantb
    # For manual test-bench, we tell the TB controller to run forever,
    # but we need to send the command only once
    variable _has_run_mantb false

    # Top level public interface
    # mkEmulationControlPanels -- builds, but does not pack the simulation control interface
    # statusLoop -- starts running the status loop, must be called after the window is built

    constructor {frame mode no_dbg args} {
    }
    
       #  #mkEmulationFrame $frame $pane $do_readback $top
       #  puts "frame is $frame"
       #  puts "do_readback is $do_readback"
       #  puts "top is $top"
       #  puts "pane constructor is $pane"
       
       #  #statusLoop
       # # return $itk_component(emu_frame)


    destructor {
        set updatetime 0
        catch "after cancel $event"
    } 

    proc create {frame mode no_dbg args} {
        uplevel #0 emu_viewer $frame $mode $no_dbg $args
    }


    method setTb {tb} {
	if { $tb == "none" } {
	    # support non-Semu use of the GUI
	    set _tb false
	    set _mantb false
	} elseif { $tb == "man" } {
	    set _tb false
	    set _mantb true
	} else {
	    set _tb true
	    set _mantb false
	}
    }
    
        
    method packEmu {} {
        pack $itk_component(emu_frame)  -fill x -side top
    } 

    method mkEmulationFrame {frame mode no_dbg} {
        # set _do_readback $do_readback

	# if { $do_readback } {
	#     set emu_cmd "redirect::rdbk"
	# }

        set _mode $mode 
        set _no_dbg $no_dbg 
        
        itk_component add emu_frame {
            ttk::frame $frame.emu_frame 
        } {}
        
        mkEmulationWin
        rdback_start
        return $itk_component(emu_frame)               
    }

    method mkEmulationWin {} {

        set emu_frame $itk_component(emu_frame) 

        itk_component add lframe {
            ttk::frame $emu_frame.lframe
        } {}
        set lframe $itk_component(lframe) 

        itk_component add logo {
            button $lframe.logo -image  [getBSCIncImage]
        } {}
        itk_component add speed {
            ttk::label $lframe.speed  -textvariable [itcl::scope tr_hz]
        } {}

        # ----------
        # Status and count frame
        itk_component add scframe {
            ttk::frame $emu_frame.scframe
        } {}
        set scframe $itk_component(scframe)

        itk_component add  status_label {
            ttk::label $scframe.status-label -textvariable [itcl::scope gui_status]
        } {}
        itk_component add cycle_label {
            ttk::label $scframe.cycle_lable -textvariable [itcl::scope total_cycles_str] \
                -width 18 -anchor e
        } {}

        # ----------
        # Run/Continue buttons
        itk_component add  button_frame {
            ttk::frame $emu_frame.button_frame
        } {}
        set button_frame $itk_component(button_frame)
        itk_component add run_button {
            ttk::button $button_frame.run_button -text "Run <N>" -command "[itcl::code $this do_run]"
        } {}
        itk_component add forever_button {
            ttk::button $button_frame.forever_button -text "Run" -command "[itcl::code $this do_forever]"
        } {}
        itk_component add  stop_button {
            ttk::button $button_frame.stop_button -text "Stop" -command "[itcl::code $this  do_stop]"
        } {}
        itk_component add continue_button {
            ttk::button $button_frame.continue_button -text "Continue" -command "[itcl::code $this do_continue]" -width 10
        } {}

        # itk_component add reset_button {
        #     ttk::button $button_frame.reset -text "Reset" -command "[itcl::code $this reset]"
        # } {}



        $itk_component(stop_button) state disabled
        $itk_component(continue_button) state disabled

        # ----------
        # Readback buttons
        itk_component add  readback_frame {
            ttk::frame $emu_frame.readback_frame
        } {}
        itk_component add inst_frame {
            ttk::frame $emu_frame.inst_frame
        } {}
        set readback_frame $itk_component(readback_frame)
        set inst_frame $itk_component(inst_frame)

        itk_component add readback_on_button {
            ttk::button $readback_frame.readback_on -text "Start Readback" -command "[itcl::code $this  rdback_start]"
        } {}
        itk_component add readback_off_button {
            ttk::button $readback_frame.readback_off -text "Stop Readback" -command "[itcl::code $this rdback_stop]"
        } {}
        itk_component add  frame_count_prefix {
            ttk::label $readback_frame.count_prefix -text "Frames in use:  "
        } {}
        itk_component add frame_count_label {
            ttk::label $readback_frame.count -textvariable [itcl::scope frame_count_str] -width 18 -anchor e
        } {}
        itk_component add bit_count_prefix {
            ttk::label $readback_frame.bcount_prefix -text "Bits in use:  "
        } {}
        itk_component add bit_count_label {
            ttk::label $readback_frame.bcount -textvariable [itcl::scope bit_count_str] -width 18 -anchor e
        } {}
       
        # itk_component add readback_select_button {
        #     ttk::button $emu_frame.readback_select -text "Select Signals" -command "[itcl::code $this rdback_select]"
        # } {}

        $itk_component(readback_off_button) state disabled

        # ----------
        # Run interval entry box
        itk_component add  cycle_frame {
            ttk::frame $emu_frame.cycle_frame 
        } {}
        set cycle_frame $itk_component(cycle_frame)

        itk_component add  cycle_entry_frame {
            ttk::frame $cycle_frame.cycle_entry_frame
        } {}
        set cycle_entry_frame $itk_component(cycle_entry_frame)
        itk_component add cycle_entry {
            ttk::entry $cycle_entry_frame.cycle_entry -textvariable [itcl::scope run_cycles] -width 14 \
                -validate focusout -validatecommand "[itcl::code $this chk_num 1 [expr pow (2,29)-2] %W %P false]"
        } {}
        itk_component add cycle_prompt {
            ttk::label $cycle_entry_frame.cycle_prompt -text "<N>="
        } {}

        # ----------
        # Progress bar
        itk_component add progress {
            ttk::progressbar $cycle_frame.progress -orient horizontal -length 180 \
                -mode determinate -variable [itcl::scope how_far]
        } {}
        pack $itk_component(logo)   -anchor ne
        pack $itk_component(speed) -anchor n
        pack $itk_component(lframe) -anchor ne -side right


        pack $itk_component(status_label) $itk_component(cycle_label) -side left -padx 5 -pady 3
        pack $itk_component(scframe) -side top -anchor c

        pack $itk_component(forever_button) $itk_component(stop_button) $itk_component(run_button) $itk_component(continue_button) \
            -side left -padx 5
  
#        pack $itk_component(reset_button) -side right -padx 50 -pady 5
   
        if {!$_no_dbg} {
            pack $itk_component(readback_on_button) $itk_component(readback_off_button) \
                $itk_component(frame_count_prefix) $itk_component(frame_count_label) \
                -side left -padx 5 -pady 5
        }
        
        pack $itk_component(cycle_prompt) $itk_component(cycle_entry) -side left
        pack $itk_component(progress)           -side left -padx 5 -pady 5 
        pack $itk_component(cycle_entry_frame) -side right -padx 5 -pady 5 

        pack $itk_component(button_frame) -padx 5 -pady 5
        # pack $itk_component(cycle_frame) -padx 5 -pady 5
        # pack $itk_component(readback_frame) -padx 5 -pady 5 -side left  
        if {!$_no_dbg} {
            pack  $itk_component(cycle_frame) $itk_component(readback_frame) -side left -padx 40 -pady 5  
        } else {
            pack  $itk_component(cycle_frame) $itk_component(readback_frame)  -padx 40 -pady 5  
        }
#        pack $itk_component(readback_select_button) -side right -padx 5 -pady 5

        trace add variable total_cycles_run write [itcl::code $this update_cycles]

       # bind $itk_component(emu_frame) <Destroy>    +[itcl::code $this destroy]
        bind $itk_component(cycle_entry) <Return>   +[itcl::code $this do_cycle_entry]
        bind $itk_component(cycle_entry) <KP_Enter> +[itcl::code $this do_cycle_entry]

#        pack $itk_component(emu_frame)
#        return $emu_frame

    }
 
    method get_emu_status {} {
        set emu_resp [$emu_cmd query]
        #puts "get_emu_status $emu_resp"
        
	if { $loop_count == 50 } {
	    set loop_count 0
	    redirect::netlist flush
	} else {
	    set loop_count [expr $loop_count + 1]
	}

        set noResponse [lindex $emu_resp 3]
        if { ! $noResponse } {
            set isRunning [lindex $emu_resp 0]
            set cyclesRemaining [lindex $emu_resp 1]
            set total_cycles_run [expr $total_cycles_enqueued - $cyclesRemaining]
            set free [lindex $emu_resp 2]
            set hw_time [lindex $emu_resp 4]
            set is_rdback_on_new [lindex $emu_resp 5]
            
	    if {($is_rdback_on_new != $is_rdback_on) || !$isRunning} {
		set speed_change true
		redirect::netlist flush
	    }
            set is_rdback_on $is_rdback_on_new
            set deadcycles 0
        } else {
            incr deadcycles 
        }

        if { $noResponse } {
            # Only report NOT responding if dead for more than 10 requests
            if { $deadcycles >= $deadreport } {
                set status "NOT RESPONDING"
            }
        } elseif { $free } {
            if { $isRunning } {
                set status "FREE"
                if { $cyclesRemaining > $lastCyclesRemaining } {
                    # counter roll over....
                    incr total_cycles_enqueued $forever_cycles
                    # do not calculate speed since the message may have been sent earlier
                    #calcSpeed update xxxx
                } else {
                    calcSpeed update $total_cycles_run
                }
                set lastCyclesRemaining $cyclesRemaining
            } else {
                set status "WAITING"
                set total_cycles_enqueued [expr $total_cycles_enqueued  - $cyclesRemaining]
            }
        } else {
            if { ! $isRunning } {
                if {  $cyclesRemaining > 0 } {
                    set status "STOPPED"
                } else {
                    set status "WAITING"
                }
            } else {
                if { $cyclesRemaining > 0 } {
                    set status "RUNNING"
                    calcSpeed update $total_cycles_run
                } else {
                    set status "WAITING"
                }
            }
        }
    }
    # get the status, and update various counters



    method do_cycle_entry {} {
        if {[$itk_component(cycle_entry) validate]} {
            update
            $itk_component(run_button) invoke
        }
    }
        
    # method destroy {} {
    #     set updatetime 0
    # }

    method mkWavePanel { frame } {
        semu_waves::create_viewer
        
        itk_component add wave_frame {
            ttk::labelframe $frame.wave_frame -text "Waveform Viewer"
        } {}

        itk_component add wave_start {
            ttk::button $wave_frame.wave_start -text "Start" -command "semu_waves::start_viewer"
        } {}
        itk_component add wave_send {
            ttk::button $wave_frame.wave_send -text "Send Enabled" -command [itcl::code wave_send]
        } {}
        itk_component add wave_reload {
            ttk::button $wave_frame.wave_reload  -text "Reload VCD" -command "semu_waves::reload_vcd"
        } {}
        itk_component add wave_xhost {
            ttk::button $wave_frame.wave_xhost -text "Allow XServer\nconnections" -command Waves::open_xhost
        } {}

        pack $itk_component(wave_starta) -padx 10 -pady 10 -side left -fill both
        pack $itk_component(wave_send) -padx 10 -pady 10 -side left -fill both
        pack $itk_component(wave_reload) -padx 10 -pady 10 -side left -fill both
        pack $itk_component(wave_xhost) -padx 10 -pady 10 -side left -fill both
                        
        return $itk_component(wave_frame)
    }


    # ----------
    # Functionality methodedures
    method rdback_start {} {
        set stat [$emu_cmd rdback_on]
    }

    method rdback_stop {} {
        set stat [$emu_cmd rdback_off]
    }

    method rdback_clr {} {
        set stat [$emu_cmd rdback_clr]
    }

    method reset {} {
        set answer [tk_messageBox -message "Are you sure you want to reset?" -icon question \
                        -type yesno -title "Reset"]
        if {$answer == "no"} {return}
	set loop_count 0
	set tr_running false
	set tr_elapsed 0
        if {$_mode == "emu"} {
            redirect::netlist flush
            redirect::netlist reset
            emu_control::execute_in_shell "bluenoc reset" [itcl::code runDone "Reset done"]
           # puts "reset sim complete"
        } else {
            redirect::netlist flush
           # puts "reset sim complete"
        }
	if {$_tb} {

            GuiDut::do_reset
	    #puts "reset tb complete"
	}	    
    }


    method rdback_select {} {
        if {[winfo exists .v]} {
            raise .v . 
        } else {
            ::instrument_viewer::create .v $topview
        }
    }

    method wave_start {} {
	semu_waves::start_viewer
    }

    method wave_send {} {
        if {[winfo exists .v]} {
            [itcl::code .v sendAllEnabledSignals]   
        } else {
            tk_messageBox -message "No Signals are enabled. " -icon error 
        }
    } 


    method rdback_add {} {
	set stat [$emu_cmd rdback_add [expr 0x$frame_addr]]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Readback Add command was not sent, message time out"
            return
        }
    }

    method do_forever {} {

       # puts "do_forever _tb  $_tb"

	# First start the tb simulation control
        if { $_mantb && ! $_has_run_mantb } {

            #puts "in do_forever starting tb simulation control"
	    set stat [$emu_cmd2 free_running]
	    if { $stat == "TIMEOUT" } {
		report_message_fail "Run command for tb simulation control was not sent, message time out"
		return
	    }
	    set _has_run_mantb true
	}

        set stat [$emu_cmd free_running]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Run command was not sent, message time out"
            return
        }

        if { $_tb } {
	    
            #puts "in do_forever starting test"
            GuiDut::do_test
	}

        # Add to the total cycles run, not enqueued, because a Run command can
        # be issued during Stop, in which case the remaining cycles are ignored
        set total_cycles_enqueued [expr $total_cycles_run + $forever_cycles]
        set lastCyclesRemaining [expr 10 + $forever_cycles]
        calcSpeed start $total_cycles_run
        set gui_status "FREE RUNNING"
        $itk_component(progress) configure -mode indeterminate
        set how_far 0
    }

    method do_run {} {

	# First start the tb simulation control
        if { $_mantb && ! $_has_run_mantb } {

            #puts "in do_run starting tb simulation control"
	    set stat [$emu_cmd2 free_running]
	    if { $stat == "TIMEOUT" } {
		report_message_fail "Run command for tb simulation control was not sent, message time out"
		return
	    }
	    set _has_run_mantb true
	}

        set stat [$emu_cmd run [expr $run_cycles]]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Run command was not sent, message time out"
            return
        }
        
        if { $_tb } {
	    
            GuiDut::do_test
	}

        $itk_component(run_button) state disabled
        $itk_component(forever_button) state disabled
        $itk_component(stop_button) state !disabled
        $itk_component(continue_button) state disabled
        $itk_component(progress) configure -mode determinate
        set this_run [expr $run_cycles]

        # Add to the total cycles run, not enqueued, because a Run command can
        # be issued during Stop, in which case the remaining cycles are ignored
        set total_cycles_enqueued [expr $total_cycles_run + $run_cycles]

        set gui_status "RUNNING"
        calcSpeed start $total_cycles_run
        update
    }

    method do_stop {} {
        set stat [$emu_cmd stop]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Stop command was not sent, message time out"
            return
        }

        $itk_component(stop_button) state disabled
        set gui_status "STOPPED"

        calcSpeed stop
        update
	redirect::netlist flush
    }

    method do_continue {} {

        set stat [$emu_cmd continue]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Continue command was not sent, message time out"
            return
        }

        $itk_component(run_button) state disabled
        $itk_component(forever_button) state disabled
        $itk_component(stop_button) state !disabled
        $itk_component(continue_button) state disabled
        calcSpeed continued
        set gui_status "RUNNING"
    }

    # print the cycle counter
    method update_cycles {varname key op} {
         set triples [list]
         set i $hw_time
         while {$i > 0} {
             set n [expr $i % 1000]
             if {$i < 1000} {
                 lappend triples $n
             } elseif {$n < 10} {
                 lappend triples [join [list "00" $n] ""]
             } elseif {$n < 100} {
                 lappend triples [join [list "0" $n] ""]
             } else {
                 lappend triples $n
             }
             set i [expr $i / 1000]
         }

         if {[llength $triples] == 0} {
             set total_cycles_str 0
         } else {
             set total_cycles_str [join [lreverse $triples] ","]
         }

         set frame_count_new [redirect::netlist framecount]
 #	set bit_count_new   [redirect::netlist bitcount]
         if {$frame_count_new != $frame_count_str} {
             set speed_change true
             redirect::netlist flush
         }
        set frame_count_str $frame_count_new
        #	set bit_count_str    $bit_count_new
     }


     method chk_num {mn mx win val_str {is_hex false}} {
         set res 0
         if { $is_hex } {
             if { [catch "expr 0x$val_str" val] } {
                 set res 0
             } elseif {[string is integer -strict $val] && [expr $val >= $mn ] && [expr $val <= $mx]} {
                 set res 1
             }
         } else {
             if { [catch "expr $val_str" val] } {
                 set res 0
             } elseif {[string is integer -strict $val] && [expr $val >= $mn ] && [expr $val <= $mx]} {
                 set res 1
             }
         }
         if { $res } {
             $win configure -foreground black
             $win state !invalid
             $itk_component(run_button) state !disabled
         } else {
             set tv [$win cget -textvariable]
             if { [info exists $tv] } { set $tv $val_str }
             $win configure -foreground red
             $win state invalid
             $itk_component(run_button) state disabled
         }
         # puts "Validate: $val  $mn $mx $win -- > $res"
         return $res
    }

    method update_progress { mode } {
        #puts "update_progess $mode $how_far"
        if { $mode == "FREE" } {
            incr how_far $how_far_incr
            if { $how_far > 100 } { set how_far 100 ; set how_far_incr [expr -1 * $how_far_incr] }
            if { $how_far < 0   } { set how_far 0   ; set how_far_incr [expr -1 * $how_far_incr] }
        } else {
            set cycles [expr $this_run - $cyclesRemaining]
            set how_far [expr (100 * $cycles) / $this_run]
        }
    }

    method statusLoop {} {
        #puts "in Status Loop"
        if { [catch statusLoopInternal err] } {
            puts stderr "Status loop failed,  interface will not respond"
            puts stderr $err
            puts stderr $::errorInfo
        }
        if { $updatetime == 0} {
            status_window::stop
        }
    }
    
    # Loop watching status
    method statusLoopInternal {} {
        set emu_frame $itk_component(emu_frame)
        if { $updatetime == 0} { return }
        if { ! [winfo exists $emu_frame] } { return }
        get_emu_status
        #puts "statusLoopInternal status: $status"
        
	if { $is_rdback_on }  {
	    $itk_component(readback_on_button) state disabled
	    $itk_component(readback_off_button) state !disabled
	} else {
	    $itk_component(readback_on_button) state !disabled
	    $itk_component(readback_off_button) state disabled
	}
        switch -exact $status {
            "WAITING" { set gui_status "WAITING"
                $itk_component(run_button) state !disabled
                $itk_component(forever_button) state !disabled
                $itk_component(stop_button) state disabled
                $itk_component(continue_button) state disabled
                set how_far 0
            }
            "RUNNING" { set gui_status "RUNNING"
                $itk_component(run_button) state disabled
                $itk_component(forever_button) state disabled
                $itk_component(stop_button) state !disabled
                $itk_component(continue_button) state disabled
            }
            "STOPPED" { set gui_status "STOPPED"
                $itk_component(run_button) state !disabled
                $itk_component(stop_button) state disabled
                $itk_component(continue_button) state !disabled
                $itk_component(forever_button) state !disabled
            }
            "FREE" {
                set gui_status "FREE RUNNING"
                $itk_component(stop_button) state !disabled
                $itk_component(run_button) state disabled
                $itk_component(continue_button) state disabled
                $itk_component(forever_button) state disabled
            }
            "NOT RESPONDING" {
                set gui_status "NOT RESPONDING"
                $itk_component(stop_button) state !disabled
                $itk_component(run_button) state !disabled
                $itk_component(continue_button) state !disabled
                $itk_component(forever_button) state !disabled
            }
            default   { set gui_status $status }
        }
        
        # progress bar
        update_progress $status

        # if the count state is invalid disable the run button
        if { [$itk_component(cycle_entry) instate invalid] } {
            $itk_component(run_button) state disabled
        }
        set event [after $updatetime [itcl::code $this statusLoop]]
        update
    }

    method getBSCIncImage {} {
        image create photo -data [getBSCIncLogo]
    }
    
    method getBSCIncLogo {} {
        return {
            R0lGODlhjAArAIQAABMJRf///05HdHp1l4mEojEoXCIYUcTC0bWyxWxli+Lg6FxVftPR3fHx
            9JeTrqejuj83aP//////////////////////////////////////////////////////////
            /yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAAIwAKwAABf4gII5kaZ5oqq5s675wLM90
            bd94ru987//AoHBILBqPyKRyyWwCEyTIyGEKAKwAgkCEJQQCh4BA3IA6z7+tSE0wEwptAgFN
            V0lFBdJ8LTqM/FeAYmBfBAh1iCt5fAICcACACWFhY32FYomZLQUGW2oCBAcOBxCAoweSEIcx
            X19qVa1YdIsoi7RQr5AjCVpWvl5cY2NhAXuYMLG5JLGymj8Fn52dEI2LjqytyiPMziPafSN7
            e1dZWlzkgYPDl8LNLMko3N1BAtTSn9D3Bi/JY18D22KVk6MGjpxxAAwQC4BgX4kEClopWBDu
            YMKIDAFsoeVAIsVdABJAYNDqgAJdBP5lFftCaRAXL+0a8GtFUh63WHv8fRmBkRmDZcxacckW
            9FbQLw6HHi3hSSOoNiLexNEiFY6ZFkdj/bzZKqdAAA2yBpB5RexJsUIBLEDrEK0VNX7iegET
            jNDKdoCwui0rFOcagToD7DMAD14BgUEXfoES64GIsF9kzv3iuOYXao7mAJrbcl3dYTMjixiQ
            jGuhv0IhB/gpomNfZg/uKN05u0FgoDvl8c3IqanGUKNKlRrVR7io0Md2T87dFTXzvZaZndxN
            dncA12gDOw6pSwSvSr+wqLMLc1Ded9kCXkLcXCP7vZDE7p5uXXHW5QhhCLiHOd/+eshttxtp
            7wWw3QMFsv6mQgHYtQKBbsws5x0sX9A3iYEpbcNFZ+KVB5oLzEhh2nOFNGgFAl8BgMABLCJw
            22yBBaCAAKpdN+AaLLIIQE+ChRQLQCnU89801azRyTfx7DViVrMdlUcsM/Jo3VIG1BjUSYSh
            FZUa4RGC12lXjCEgemI9CVhQNYrwYFYHiuWYW1dp6Z1Yd/Sm0RrAFScKi6mQch6ZvsSyiDwJ
            aLUbCfYpkBQAa8bSwKCxNCjbnMxcNUJ0q2Xh1CObsVQXMR6OOY9eadEwKR7hbJqXIBy+NEh+
            owLqDj2pZsELHI9kAUqusSI3Ky3ecDfCLXdyl1eG4X1GnhiW9spCjjo6MaSQjj4g6awNwCJ5
            7DK1+hIVS+OBeu24eDRiz38JAEnuLMFqxCsqLAZHgAGLrmvvvfjmq+++/Pbr778AByzwwImE
            AAA7
        }
    }


    # calculate and display the "speed" of emulation
    # act == action
    # ticks -- current cycle count
    method calcSpeed {act {ticks 1}} {
        # # bool
        # variable tr_running
        # # elapsed time to capture starts and stops
        # variable tr_elapsed
        # # start time
        # variable tr_start
        # # cycles at start of run
        # variable tr_cycles
	# variable speed_change

	if {$speed_change && $act == "update"} {
	    set speed_change false
	    set act start
	}

        switch $act {
            start {
                set tr_running true
                set tr_elapsed 0
                set tr_start [clock millisecond]
                set tr_cycles $ticks
            }
            finished {
                set now [clock millisecond]
                set tr_elapsed [expr $tr_elapsed + ($now - $tr_start)]
                reportSpeed $tr_cycles $tr_elapsed
                set tr_running false
            }
            stop {
                set now [clock millisecond]
                set tr_elapsed [ expr $tr_elapsed + ($now - $tr_start) ]
                set tr_running false
            }
            continued {
                set tr_start [clock millisecond]
                set tr_running true
            }
            update {
                if { $tr_running } {
                    set now [clock millisecond]
                    set tmp [expr  $tr_elapsed + ($now - $tr_start) ]
                    set cyclesDone [expr $ticks - $tr_cycles]
                    reportSpeed $cyclesDone $tmp
                    if { $ticks == 0 } {
                        set tr_running false
                    }
                }
            }
        }
    }
    method reportSpeed { cycles timems } {
        set prefixes [list "" "K" "M" "G" "T"]
        if { [expr $timems >= 0] } {
            set hz [expr  ($cycles * 1000.0) / $timems ]
            set newhz [format "%0.3g HzXXX" [expr $hz]]
            for { set i 0 } { $i < [llength $prefixes] } { incr i} {
                if { $hz < 1000 } {
                    set newhz [format "%0.3g %sHz" $hz [lindex $prefixes $i]]
                    break;
                } else {
                    set hz [expr $hz / 1000]
                }
            }
            if { $newhz != $tr_hz } {
                set tr_hz $newhz
            }
        }
    }

    method report_message_fail {msg} {
        tk_messageBox -message $msg \
            -title "Emulation Control Command failed" \
            -parent $itk_component(emu_frame) \
            -type ok \
            -icon warning
        update idletasks
    }
}
