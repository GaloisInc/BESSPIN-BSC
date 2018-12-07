package require Tk
package require Redirect

::redirect::send {lappend auto_path $::env(BLUESPECDIR)/tcllib/simtb}
::redirect::send {package require Interp}

namespace eval GuiDut {

    proc mkDutControl { frame } {
        variable top

        set top $frame

        set top [ttk::labelframe $frame.dut_pane -text "Dut Control" -relief flat]

        ## Button Frame
        set button_frame [ttk::frame $top.button_frame]
        set usertb_label [ttk::labelframe $top.button_frame.label]
        set usertb_text [ttk::label $top.blabel -justify center -text "TestBench Controls"]
        pack $usertb_text
        set testbut [ttk::button $button_frame.but1 -text "Verbose On" -command "redirect::send_cmd 0 5001"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but2 -text "Verbose Off" -command "redirect::send_cmd 0 5000"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but3 -text "Run Test 0" -command "redirect::send_cmd 0 0"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but4 -text "Run Test 1" -command "redirect::send_cmd 0 1"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but5 -text "Run Test 2" -command "redirect::send_cmd 0 2"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but6 -text "Run Test 3" -command "redirect::send_cmd 0 3"]
        pack $testbut -anchor n -side left -padx 5
        set testbut [ttk::button $button_frame.but7 -text "Abort Test" -command "redirect::send_cmd 0 6000"]
        pack $testbut -anchor n -side left -padx 5
	pack $button_frame -anchor n -side top


        return $top
    }

    proc chk_num {s} {
        string is integer $s
    }

    proc do_test {} {
    }
    proc do_reset {} {
    }

}
