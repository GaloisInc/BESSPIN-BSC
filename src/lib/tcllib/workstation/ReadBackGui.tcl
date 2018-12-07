#  Copyright Bluespec Inc. 2009-2011


# ----------
# The emulator control panel graphical interface
namespace eval ::ReadBackGui {
    package require BSDebug
    package require Iwidgets 4.0
    package require Redirect


    # Top level public interface
    # mkEmulationControlPanels -- builds, but does not pack the simulation control interface
    # statusLoop -- starts running the status loop, must be called after the window is built

    variable status "Initializing"
    variable isRunning false
    variable cyclesRemaining 0
    variable lastCyclesRemaining 0

    variable gui_status "NOT STARTED"
    variable status "NOT STARTED"
    variable total_cycles_str "0"
    variable total_cycles_enqueued 0
    variable run_button
    variable forever_button
    variable stop_button
    variable continue_button
    variable readback_on_button
    variable readback_off_button
    variable readback_clr_button
    variable readback_add_button
    variable frame_addr 0
    variable emu_frame
    variable total_cycles_run 0
    variable hw_time 0
    variable is_rdback_on false
    variable frame_count_str "0"
    variable bit_count_str "0"
    variable run_cycles 1000000
    variable forever_cycles [expr  int(pow(2,29))]
    variable this_run 1
    variable how_far 0
    variable how_far_incr 2
    variable cycle_entry
    variable cycle_entry_frame
    variable updatetime 200
    variable deadcycles 0
    variable deadreport 2
    variable readback_select_button

    variable wave_frame
    variable wave_start
    variable wave_send
    variable wave_reload
    variable wave_xhost

    variable tr_running false
    variable tr_elapsed 0
    variable tr_start 0
    variable tr_cycles 0
    variable tr_hz "0 Hz"
    variable emu_cmd "redirect::emu"
    variable speed_change false
    variable loop_count 0


    variable topview ""
#    variable topview "mkBridge/scemi_dut_dutIfc/ifc_raw"


    # get the status, and update various counters
    proc get_emu_status {} {
	variable emu_cmd
        variable isRunning
        variable cyclesRemaining
        variable lastCyclesRemaining
        variable total_cycles_enqueued
        variable forever_cycles
        variable total_cycles_run
        variable status
        variable hw_time
        variable is_rdback_on
        variable deadcycles
        variable deadreport
	variable speed_change
	variable loop_count
        set emu_resp [$emu_cmd query]

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

    proc mkEmulationControlPanels {frame {do_readback false} top} {
        variable topview
	variable emu_cmd
        variable emu_frame
        variable gui_status
        variable status
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
	variable readback_on_button
	variable readback_off_button
	variable readback_clr_button
	variable readback_add_button
        variable cycle_entry
        variable progress
        variable tz_hz
        variable readback_select_button
        variable wave_frame
        variable wave_start
        variable wave_send
        variable wave_reload
        variable wave_xhost
        
        if {$top != ""} {
            set topview $top
            puts "topview is $topview top is $top"
        }

	if { $do_readback } {
	    set emu_cmd "redirect::rdbk"
	}

        set emu_frame [ttk::labelframe $frame.emu_panel -relief ridge -text "Emulation Control" ]

        set lframe    [ttk::frame $emu_frame.lframe]
        set logo      [button $lframe.logo -image  [getBSCIncImage]]
        set speed     [ttk::label $lframe.speed  -textvariable ReadBackGui::tr_hz]

        # ----------
        # Status and count frame
        set scframe    [ttk::frame $emu_frame.scframe]
        set status_label [ttk::label $scframe.status -textvariable ReadBackGui::gui_status]
        set cycle_label  [ttk::label $scframe.total_cycles -textvariable ReadBackGui::total_cycles_str -width 18 -anchor e]

        # ----------
        # Run/Continue buttons
        set button_frame [ttk::frame $emu_frame.button_frame]
        set run_button [ttk::button $button_frame.run -text "Run <N>" -command [namespace code do_run]]
        set forever_button [ttk::button $button_frame.forever -text "Run" -command [namespace code do_forever]]
        set stop_button [ttk::button $button_frame.stop -text "Stop" -command [namespace code do_stop]]
        set continue_button [ttk::button $button_frame.continue -text "Continue" -command [namespace code do_continue] -width 10]

        $stop_button state disabled
        $continue_button state disabled

        # ----------
        # Readback buttons
        set readback_frame [ttk::frame $emu_frame.readback_frame]
        set inst_frame [ttk::frame $emu_frame.inst_frame]
        # set readback_frame [ttk::labelframe $emu_frame.readback_frame -text "Readback"]
        # set inst_frame [ttk::labelframe $emu_frame.inst_frame -text "Signals"]
        
        set readback_on_button [ttk::button $readback_frame.readback_on -text "Start Readback" -command [namespace code rdback_start]]
        set readback_off_button [ttk::button $readback_frame.readback_off -text "Stop Readback" -command [namespace code rdback_stop]]

	set frame_count_prefix   [ttk::label $readback_frame.count_prefix -text "Frames in use:  "]
        set frame_count_label  [ttk::label $readback_frame.count -textvariable ReadBackGui::frame_count_str -width 18 -anchor e]
	set bit_count_prefix   [ttk::label $readback_frame.bcount_prefix -text "Bits in use:  "]
        set bit_count_label  [ttk::label $readback_frame.bcount -textvariable ReadBackGui::bit_count_str -width 18 -anchor e]
        set readback_select_button [ttk::button $emu_frame.readback_select -text "Select Signals" -command [namespace code rdback_select]]


        
        # set readback_clr_button [ttk::button $readback_frame.readback_clr -text "Clear Frames" -command [namespace code rdback_clr]]
        # set readback_add_button [ttk::button $readback_frame.readback_add -text "Add Frame" -command [namespace code rdback_add]]
        # set frame_prompt [ttk::label $readback_frame.frame_prompt -text "<ADDR (HEX)>="]
        # set frame_sep    [ttk::label $readback_frame.frame_sep -text "  |  "]
        # set frame_entry  [ttk::entry $readback_frame.frame_entry -textvariable ReadBackGui::frame_addr -width 8 -validate focusout \
        #                      -validatecommand [namespace code "chk_num 0 [expr pow (2,24)-2] %W %P true"]]

        $readback_off_button state disabled

        # ----------
        # Run interval entry box
        set cycle_frame [ttk::frame $emu_frame.cycle_frame]
        set cycle_entry_frame [ttk::frame $cycle_frame.cycle_entry_frame]
        set cycle_entry [ttk::entry $cycle_entry_frame.cycle_entry -textvariable ReadBackGui::run_cycles -width 14 -validate focusout \
                             -validatecommand [namespace code "chk_num 1 [expr pow (2,30)-2] %W %P false"]]
        set cycle_prompt [ttk::label $cycle_entry_frame.cycle_prompt -text "<N>="]

        # ----------
        # Progress bar
        set progress [ttk::progressbar $cycle_frame.progress -orient horizontal -length 180 -mode determinate -variable ReadBackGui::how_far]

        pack $logo   -anchor ne
        pack $speed -anchor n
        pack $lframe -anchor ne -side right


        pack $status_label $cycle_label -side left -padx 5 -pady 3
        pack $scframe -side top -anchor c

        pack $forever_button $stop_button $run_button $continue_button \
            -side left -padx 5

#        pack $readback_on_button $readback_off_button $frame_sep $readback_clr_button $readback_add_button $frame_prompt $frame_entry \
#            -side left -padx 5

#        pack  $readback_select_button $readback_on_button $readback_off_button $frame_count_prefix $frame_count_label \
            -side left -padx 5
        # pack $readback_on_button $readback_off_button \
	#     $frame_count_prefix $frame_count_label \
	#     $bit_count_prefix $bit_count_label \
        #     -side left -padx 5 -pady 5

	pack $readback_on_button $readback_off_button \
	    $frame_count_prefix $frame_count_label \
            -side left -padx 5 -pady 5
       
        pack $cycle_prompt $cycle_entry -side left
        pack $progress           -side left -padx 5 -pady 5 
        pack $cycle_entry_frame -side right -padx 5 -pady 5 

        pack $button_frame -padx 5 -pady 5
        pack $cycle_frame -padx 5 -pady 5
        pack $readback_frame -padx 5 -pady 5 -side left  
        pack $readback_select_button -side right -padx 5 -pady 5


#        pack $inst_frame  -padx 10 -pady 5 -side right 

        # if {$mode == "sim"} {
        #     $wave_button configure -state disabled
        #     $readback_select_button configure -state disable
        #     $readback_on_button configure -state disabled
        #     $readback_off_button configure -state disabled
        #     $frame_count_prefix configure -state disabled
        #     $frame_count_label configure -state disabled
        # } 


        trace add variable ReadBackGui::total_cycles_run write [namespace code update_cycles]

        bind $emu_frame <Destroy>    +[namespace code destroy]
        bind $cycle_entry <Return>   +[namespace code do_cycle_entry]
        bind $cycle_entry <KP_Enter> +[namespace code do_cycle_entry]

        return $emu_frame
    }

    proc do_cycle_entry {} {
        variable run_button
        variable cycle_entry
        if {[$cycle_entry validate]} {
            update
            $run_button invoke
        }
    }
        
    proc destroy {} {
        variable updatetime
        set updatetime 0
    }

    proc mkWavePanel { frame } {
        variable wave_frame
        variable wave_bb
        variable wave_start
        variable wave_send
        variable wave_reload
        variable wave_xhost

        semu_waves::create_viewer
        
        set wave_frame [ttk::labelframe $frame.wave_frame -text "Waveform Viewer"]

        set wave_start [ttk::button $wave_frame.wave_start -text "Start" -command "semu_waves::start_viewer"]
        set wave_send [ttk::button $wave_frame.wave_send -text "Send Enabled" -command [namespace code wave_send]]
        set wave_reload [ttk::button $wave_frame.wave_reload  -text "Reload VCD" -command "semu_waves::reload_vcd"]
        set wave_xhost [ttk::button $wave_frame.wave_xhost -text "Allow XServer\nconnections" -command Waves::open_xhost]

        pack $wave_start -padx 10 -pady 10 -side left -fill both
        pack $wave_send -padx 10 -pady 10 -side left -fill both
        pack $wave_reload -padx 10 -pady 10 -side left -fill both
        pack $wave_xhost -padx 10 -pady 10 -side left -fill both
                        
        # set wave_bb [iwidgets::buttonbox $wave_frame.wave_bb -padx 5 -pady 5]

        # $wave_bb add wave_start -text "Start" -command [namespace code wave_start]
        # $wave_bb add wave_send  -text "Send Enabled" -command [namespace code wave_send]
        # $wave_bb add wave_reload  -text "Reload VCD" -command "semu_waves::reload_vcd"

#        pack $wave_bb -fill both -expand yes 

        return $wave_frame
    }


    # ----------
    # Functionality procedures
    proc rdback_start {} {
	variable emu_cmd

        set stat [$emu_cmd rdback_on]
    }

    proc rdback_stop {} {
	variable emu_cmd

        set stat [$emu_cmd rdback_off]
    }

    proc rdback_clr {} {
	variable emu_cmd

        set stat [$emu_cmd rdback_clr]
    }

    proc rdback_select {} {
        variable topview
        puts "topview is $topview"
        if {[winfo exists .v]} {
            raise .v . 
        } else {
            puts "in rdback_select else"
            ::inst_old::create .v none $topview
            # inst_old::mkMenuBar none
            # inst_old::packMenu emu
            # inst_old::mkInstrumentWin none
            # inst_old::packInst
        }
    }

    proc wave_start {} {
	semu_waves::start_viewer
    }

    proc wave_send {} {
        if {[winfo exists .v]} {
            [itcl::code .v sendAllEnabledSignals]   
        } else {
            tk_messageBox -message "No Signals are enabled. " -icon error 
        }
    } 


    proc rdback_add {} {
	variable emu_cmd
	variable frame_addr

	set stat [$emu_cmd rdback_add [expr 0x$frame_addr]]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Readback Add command was not sent, message time out"
            return
        }

    }

    proc do_forever {} {
	variable emu_cmd
        variable status
        variable total_cycles_enqueued
        variable total_cycles_run
        variable lastCyclesRemaining
        variable forever_cycles
        variable progress
        variable how_far
        variable gui_status

	set stat [$emu_cmd free_running]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Run command was not sent, message time out"
            return
        }

        # Add to the total cycles run, not enqueued, because a Run command can
        # be issued during Stop, in which case the remaining cycles are ignored
        set total_cycles_enqueued [expr $total_cycles_run + $forever_cycles]
        set lastCyclesRemaining [expr 10 + $forever_cycles]
        calcSpeed start $total_cycles_run
        set gui_status "FREE RUNNING"
        $progress configure -mode indeterminate
        set how_far 0
    }

    proc do_run {} {
	variable emu_cmd
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
        variable progress
        variable run_cycles
        variable this_run
        variable total_cycles_enqueued
        variable total_cycles_run
        variable gui_status

        set stat [$emu_cmd run [expr $run_cycles]]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Run command was not sent, message time out"
            return
        }

        $run_button state disabled
        $forever_button state disabled
        $stop_button state !disabled
        $continue_button state disabled
        $progress configure -mode determinate
        set this_run [expr $run_cycles]

        # Add to the total cycles run, not enqueued, because a Run command can
        # be issued during Stop, in which case the remaining cycles are ignored
        set total_cycles_enqueued [expr $total_cycles_run + $run_cycles]

        set gui_status "RUNNING"
        calcSpeed start $total_cycles_run
        update
    }

    proc do_stop {} {
	variable emu_cmd
        variable stop_button
        variable gui_status

        set stat [$emu_cmd stop]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Stop command was not sent, message time out"
            return
        }

        $stop_button state disabled
        set gui_status "STOPPED"

        calcSpeed stop
        update
	redirect::netlist flush
    }

    proc do_continue {} {
	variable emu_cmd
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
        variable gui_status

        set stat [$emu_cmd continue]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Continue command was not sent, message time out"
            return
        }

        $run_button state disabled
        $forever_button state disabled
        $stop_button state !disabled
        $continue_button state disabled
        calcSpeed continued
        set gui_status "RUNNING"
    }

    # print the cycle counter
    proc update_cycles {varname key op} {
        variable total_cycles_run
        variable total_cycles_str
        variable hw_time
	variable frame_count_str
	variable bit_count_str
	variable speed_change

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


    proc chk_num {mn mx win val_str {is_hex false}} {
        variable run_button
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
            $run_button state !disabled
        } else {
            set tv [$win cget -textvariable]
            if { [info exists $tv] } { set $tv $val_str }
            $win configure -foreground red
            $win state invalid
            $run_button state disabled
        }
        # puts "Validate: $val  $mn $mx $win -- > $res"
        return $res
    }

    proc update_progress { mode } {
        variable this_run
        variable cyclesRemaining
        variable how_far
        variable how_far_incr

        if { $mode == "FREE" } {
            incr how_far $how_far_incr
            if { $how_far > 100 } { set how_far 100 ; set how_far_incr [expr -1 * $how_far_incr] }
            if { $how_far < 0   } { set how_far 0   ; set how_far_incr [expr -1 * $how_far_incr] }
        } else {
            set cycles [expr $this_run - $cyclesRemaining]
            set how_far [expr (100 * $cycles) / $this_run]
        }
    }

    proc statusLoop {} {
        if { [catch statusLoopInternal err] } {
            puts stderr "Status loop failed,  interface will not respond"
            puts stderr $err
            puts stderr $::errorInfo
        }
    }
    # Loop watching status
    proc statusLoopInternal {} {
        variable status
        variable gui_status
        variable total_cycles_run
        variable total_cycles_enqueued
        variable cyclesRemaining
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
        variable how_far
        variable cycle_entry
        variable updatetime
        variable emu_frame
	variable is_rdback_on
	variable readback_on_button
	variable readback_off_button

        if { $updatetime == 0} { return }
        if { ! [winfo exists $emu_frame] } { return }
        get_emu_status

	if { $is_rdback_on }  {
	    $readback_on_button state disabled
	    $readback_off_button state !disabled
	} else {
	    $readback_on_button state !disabled
	    $readback_off_button state disabled
	}

            switch -exact $status {
                "WAITING" { set gui_status "WAITING"
                    $run_button state !disabled
                    $forever_button state !disabled
                    $stop_button state disabled
                    $continue_button state disabled
                    set how_far 0
                }
                "RUNNING" { set gui_status "RUNNING"
                    $run_button state disabled
                    $forever_button state disabled
                    $stop_button state !disabled
                    $continue_button state disabled
                }
                "STOPPED" { set gui_status "STOPPED"
                    $run_button state !disabled
                    $stop_button state disabled
                    $continue_button state !disabled
                    $forever_button state !disabled
                }
                "FREE" {
                    set gui_status "FREE RUNNING"
                    $stop_button state !disabled
                    $run_button state disabled
                    $continue_button state disabled
                    $forever_button state disabled
                }
                "NOT RESPONDING" {
                    set gui_status "NOT RESPONDING"
                    $stop_button state !disabled
                    $run_button state !disabled
                    $continue_button state !disabled
                    $forever_button state !disabled
                }
                default   { set gui_status $status }
            }

        # progress bar
        update_progress $status

        # if the count state is invalid disable the run button
        if { [$cycle_entry instate invalid] } {
            $run_button state disabled
        }
        after $updatetime [namespace code statusLoop]
        update
    }

    proc getBSCIncImage {} {
        image create photo -data [getBSCIncLogo]
    }
    proc getBSCIncLogo {} {
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
    proc calcSpeed {act {ticks 1}} {
        # bool
        variable tr_running
        # elapsed time to capture starts and stops
        variable tr_elapsed
        # start time
        variable tr_start
        # cycles at start of run
        variable tr_cycles
	variable speed_change

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
    proc reportSpeed { cycles timems } {
        variable tr_hz
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

    proc report_message_fail {msg} {
        variable emu_frame
        tk_messageBox -message $msg \
            -title "Emulation Control Command failed" \
            -parent $emu_frame \
            -type ok \
            -icon warning
        update idletasks
    }


}
package provide ReadBackGui 1.0



# ----------
# ControlGui::mkEmulationControlPanels .
#proc tester {} {
#    pack [ControlGui::mkEmulationControlPanels .] -expand 1 -fill x
#    pack  [button .b -text "Exit" -command "exit"]
#   
#}
# tester
