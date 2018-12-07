#  Copyright Bluespec Inc. 2009-2011


# ----------
# The emulator control panel graphical interface
namespace eval ::ControlGui {
    package require BSDebug
    package require Iwidgets 4.0

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
    variable emu_frame
    variable total_cycles_run 0
    variable hw_time 0
    variable run_cycles 1000000
    variable forever_cycles [expr  int(pow(2,30))]
    variable this_run 1
    variable how_far 0
    variable how_far_incr 2
    variable cycle_entry
    variable cycle_entry_frame
    variable updatetime 200
    variable deadcycles 0
    variable deadreport 2

    variable tr_running false
    variable tr_elapsed 0
    variable tr_start 0
    variable tr_cycles 0
    variable tr_hz "0 Hz"

    # get the status, and update various counters
    proc get_emu_status {} {
        variable isRunning
        variable cyclesRemaining
        variable lastCyclesRemaining
        variable total_cycles_enqueued
        variable forever_cycles
        variable total_cycles_run
        variable status
        variable hw_time
        variable deadcycles
        variable deadreport
        set emu_resp [bsdebug::emu query]

        set noResponse [lindex $emu_resp 3]
        if { ! $noResponse } {
            set isRunning [lindex $emu_resp 0]
            set cyclesRemaining [lindex $emu_resp 1]
            set total_cycles_run [expr $total_cycles_enqueued - $cyclesRemaining]
            set free [lindex $emu_resp 2]
            set hw_time [lindex $emu_resp 4]
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

    proc mkEmulationControlPanels {frame} {
        variable emu_frame
        variable gui_status
        variable status
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
        variable cycle_entry
        variable progress
        variable tz_hz

        set emu_frame [ttk::labelframe $frame.emu_panel -relief ridge -text "Emulation Control" ]

        set lframe    [ttk::frame $emu_frame.lframe]
        set logo      [button $lframe.logo -image  [getBSCIncImage]]
        set speed     [ttk::label $lframe.speed  -textvariable ControlGui::tr_hz]

        # ----------
        # Status and count frame
        set scframe    [ttk::frame $emu_frame.scframe]
        set status_label [ttk::label $scframe.status -textvariable ControlGui::gui_status]
        set cycle_label  [ttk::label $scframe.total_cycles -textvariable ControlGui::total_cycles_str -width 18 -anchor e]

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
        # Run interval entry box
        set cycle_frame [ttk::frame $emu_frame.cycle_frame]
        set cycle_entry_frame [ttk::frame $cycle_frame.cycle_entry_frame]
        set cycle_entry [ttk::entry $cycle_entry_frame.cycle_entry -textvariable ControlGui::run_cycles -width 14 -validate focusout \
                             -validatecommand [namespace code "chk_num 1 [expr pow (2,30)-2] %W %P"]]
        set cycle_prompt [ttk::label $cycle_entry_frame.cycle_prompt -text "<N>="]

        # ----------
        # Progress bar
        set progress [ttk::progressbar $cycle_frame.progress -orient horizontal -length 180 -mode determinate -variable ControlGui::how_far]

        pack $logo   -anchor ne
        pack $speed -anchor n
        pack $lframe -anchor ne -side right

        pack $status_label $cycle_label -side left -padx 5 -pady 3
        pack $scframe -side top -anchor c

        pack $forever_button $stop_button $run_button $continue_button \
            -side left -padx 5

        pack $cycle_prompt $cycle_entry -side left
        pack $progress           -side left -padx 5 -pady 5 
        pack $cycle_entry_frame -side right -padx 5 -pady 5 

        pack $button_frame -padx 5 -pady 5
        pack $cycle_frame -padx 5 -pady 5

        trace add variable ControlGui::total_cycles_run write [namespace code update_cycles]

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

    # ----------
    # Functionality procedures
    proc do_forever {} {
        variable status
        variable total_cycles_enqueued
        variable total_cycles_run
        variable lastCyclesRemaining
        variable forever_cycles
        variable progress
        variable how_far
        variable gui_status

	set stat [bsdebug::emu free_running]
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

        set stat [bsdebug::emu run [expr $run_cycles]]
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
        variable stop_button
        variable gui_status

        set stat [bsdebug::emu stop]
        if { $stat == "TIMEOUT" } {
            report_message_fail "Stop command was not sent, message time out"
            return
        }

        $stop_button state disabled
        set gui_status "STOPPED"

        calcSpeed stop
        update
    }

    proc do_continue {} {
        variable run_button
        variable forever_button
        variable stop_button
        variable continue_button
        variable gui_status

        set stat [bsdebug::emu continue]
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
    }


    proc chk_num {mn mx win val_str} {
        variable run_button
        set res 0
        if { [catch "expr $val_str" val] } {
            set res 0
        } elseif {[string is integer -strict $val] && [expr $val >= $mn ] && [expr $val <= $mx]} {
            set res 1
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

        if { $updatetime == 0} { return }
        if { ! [winfo exists $emu_frame] } { return }
        get_emu_status

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
package provide ControlGui 1.0



# ----------
# ControlGui::mkEmulationControlPanels .
#proc tester {} {
#    pack [ControlGui::mkEmulationControlPanels .] -expand 1 -fill x
#    pack  [button .b -text "Exit" -command "exit"]
#   
#}
# tester
