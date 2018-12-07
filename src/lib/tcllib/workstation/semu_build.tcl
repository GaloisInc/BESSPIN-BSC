package require types
package require Iwidgets 4.0
package require Itk
package require SemuInit
package require semu_testbench
package require sim_testbench

package provide semu_build 1.0

global env

catch "itcl::delete class semu_build"
itcl::class semu_build {
    inherit iwidgets::Dialog
    common _Options
    common _Answer
    common _Build
    common _InputMode ""
    common _Projvals
    common _Mode run
    common _Projname
    common _Projfile
    common _Buildfile "project.blt"
    common sim_rundut "rundut"
    common emu_rundut "rundut"
    common _tbname
    common __pids [list]
    common _LoadDone False

    common _ctb [list \
                     ]

    common _stb [list \
                     ]

    common design  [list \
                        top-file \
                        top-module \
                        design-clock-frequency \
                        ]

    common fields [list \
                        tb-mode \
                        ctb-file \
                        stb-file \
                      ]

#                        simulator \

                    


    # common emtargets [list \
    #                       kc705  \
    #                       vc707  \
    #                       10gk7ll \
    #                       dh2000tq \
    #                       ml605 \
    #                      ]

    common __just_check false


    common _TbSettings ""


                     
    constructor {args} {
        lappend -args -modality application -title "Build" 

        eval itk_initialize $args
        array set _Answer [list]
        array set _Build [list]
        array set _Projvals [list]
        
        itk_component add browse_dia {
            file_dialog::create .#auto
        } {}
        
        # puts "creating askSpeed"

        # itk_component add askSpeed {
        #     askSpeed::create .#auto
        # } {}

        # puts "askSpeed created"
        # itk_component add semu_ctb {
        #    semu_ctb::create .#auto
        # } {}

        #puts "add semu_testbench"
        itk_component add semu_testbench {
            semu_testbench::create .#auto
        } {}

        itk_component add sim_testbench {
            sim_testbench::create .#auto
        } {}

        wm title $itk_component(hull) "Configure Testbench"

        mkBuildWindow

        $itk_component(hull) hide Help
        $itk_component(hull) hide Apply
        buttonconfigure OK  -text "OK" -underline 0 -command [itcl::code $this finish]
        buttonconfigure Cancel -underline 0
    }
    
    destructor {}

    proc first_line {str} {
	set l [split $str "\n"]
	return [lindex $l 0]
    }

    proc unavailable_option {msg} {
	set index [string first "Error in startup script:" $msg]
	if {$index == 0} {
	    set msg [string range $msg 25 end]

	}
	tk_messageBox -title "Option Unavailable!" -icon error -message [first_line $msg]
    }


    proc getTbSettings {} {
        if {$_TbSettings == ""} {
            uplevel #0 semu_build .build -modality application      
            set _TbSettings .build
        }
        return $_TbSettings
    }

    #  proc create {pathname args} {
    #     uplevel #0 semu_build $pathname -modality application $args
    # }

    proc resetArrays {} {
        array unset _Build
        array unset _Answer
        array unset _Projvals
    }

    proc readBuild {{no_error false}} {
        set _Buildfile project.blt
        if {[file exists $_Buildfile]} {
            set bf [open $_Buildfile "r+"] 
            while {[gets $bf bline] >= 0} {
                if  {[regexp {(.*): (.*)} $bline fulline key value] } {
                    set _Build($key) $value
                }
            }
	    close $bf
	    foreach {directive dflt} {emu-stb-file "" sim-stb-file ""} {
		if {![info exists _Build($directive)]} {
		    set _Build($directive) $dflt
		}
	    }
            return true
        } else {
	    if {!$no_error} {
		tk_messageBox -title "Error" -icon error \
		    -message "Project must be built before it can be run"
	    }
            return false
        }
    }
        
    proc readOptions {} {
        # set _Projfile $projfile
	 set projfile project.cfg
         set goodread true

         if {[catch {set pf [open $projfile "r+"] } ]} {
             tk_messageBox -title "Error" -icon error -message "Configuration file does not exist "
             set goodread false
             return $goodread
         } 
         while {[gets $pf line] >= 0 } {
             if  {[regexp {(.*): (.*)} $line fulline key value] } {
                 set _Projvals($key) $value
             }
         }
         set _Buildfile project.blt
         #puts "pwd [pwd]"
         if {[file exists $_Buildfile]} {
             set bf [open $_Buildfile "r+"] 
             while {[gets $bf bline] >= 0} {
                 if  {[regexp {(.*): (.*)} $bline fulline key value] } {
                     set _Answer($key) $value
                     set _Build($key) $value
                 }
             }
	     foreach {directive dflt} {emu-stb-file "" sim-stb-file "" boardbuilt ""} {
		 if {![info exists _Build($directive)]} {
		     set _Build($directive) $dflt
		     set _Answer($directive) $dflt

		 }
	     }
             #puts "Buildfile exists"
             #parray _Build
             #parray _Answer
	     close $bf
         } else {
             array unset _Answer 
             set f "tb-mode"
             set _Build(emu-$f) 0
             set _Answer(emu-$f) 0
             set _Build(sim-$f) 0
             set _Answer(sim-$f) 0

             set f "ctb-file"
             set _Build(emu-$f) ""
             set _Answer(emu-$f) ""
             set _Build(sim-$f) ""
             set _Answer(sim-$f) ""

             set f "stb-file"
             set _Build(emu-$f) ""
             set _Answer(emu-$f) ""
             set _Build(sim-$f) ""
             set _Answer(sim-$f) ""


             set _Answer(board) ""
             set _Build(board) ""
             set _Answer(boardbuilt) ""
             set _Built(boardbuilt) ""
         }
         #parray _Build
         #parray _Answer
	 return $goodread
     }

    proc getBitInfo {} {
        return $_Answer(boardbuilt)
    }

     method get_ctbs {} {
         set _ctb ""
         #puts "current directory is [pwd]"
         if {[file exists "./testbench.bld"]} {
             set tbfile [open "testbench.bld" "r"]
             while {[gets $tbfile line] >= 0} {
                 if {[regexp {^\[emu(.*)\]} $line tbfull]} {
                     regsub -all {\[|\]} $tbfull "" tbname
                     regsub  {^emu} $tbname "" tbnamefinal
                     lappend _ctb $tbnamefinal
                 }
             }
             close $tbfile
         }
	 set _ctb [lsort -unique $_ctb]
	 return $_ctb
     }

    method get_stbs {} {
	set _stb ""
	#puts "current directory is [pwd]"
	if {[file exists "./sim_tb.bld"]} {
	    set tbfile [open "sim_tb.bld" "r"]
	    while {[gets $tbfile line] >= 0} {
		if {[regexp {^\[emu(.*)\]} $line tbfull]} {
		    regsub -all {\[|\]} $tbfull "" tbname
		    regsub  {^emu} $tbname "" tbnamefinal
		    lappend _stb $tbnamefinal
		}
	    }
	    close $tbfile
	}
	set _stb [lsort -unique $_stb]
	return $_stb
    }
    
    method fill_ctb {} {
        $itk_component(cfield)  delete list 0 end 
        $itk_component(scfield) delete list 0 end
	
        foreach t [get_ctbs] {
            eval $itk_component(cfield) insert list end $t
            eval $itk_component(scfield) insert list end $t
        }

        if {$_Build(emu-ctb-file) != ""} {
            $itk_component(cfield) insert entry 0 $_Build(emu-ctb-file)
        }
        if {$_Build(sim-ctb-file) != ""} {
            $itk_component(scfield) insert entry 0 $_Build(sim-ctb-file)
        }
    } 

    method fill_stb {} {
        $itk_component(sfield)  delete list 0 end 
        $itk_component(ssfield) delete list 0 end
	
        foreach t [get_stbs] {
            eval $itk_component(sfield) insert list end $t
            eval $itk_component(ssfield) insert list end $t
        }

        if {$_Build(emu-stb-file) != ""} {
            $itk_component(sfield) insert entry 0 $_Build(emu-stb-file)
        }
        if {$_Build(sim-stb-file) != ""} {
            $itk_component(ssfield) insert entry 0 $_Build(sim-stb-file)
        }
    } 
    
    method settb {runmode tb} {
        if {$runmode == "sim"} {
            set sim_rundut run$tb
        } else {
            set emu_rundut run$tb
        }
    }

    method mkBuildWindow {} {
        itk_component add top {
            frame $itk_interior.top
        }
        #addProject
       
        addEmulation
        addSimulation
        #alignFields

        # pack $itk_component(pframe) -expand true -fill x
        pack $itk_component(ew) -expand true -fill x -anchor w
        pack $itk_component(sw) -expand true -fill x -anchor w
        pack $itk_component(top) -expand true -fill x
    }


    proc getprojFields {} {return $design}
    proc getemFields {} {return $emtargets}
    proc  getFields {} {return $fields}

    method addProject {} {
        itk_component add pframe {
            iwidgets::labeledframe $itk_component(top).pframe -labelpos nw -labeltext "Project" -borderwidth 3
        } {}

        set pframe [$itk_component(pframe) childsite]
        
        foreach {f} [getprojFields] {
            addEntryField $pframe.$f $f $pframe 
            
        }
    }

    method addEntryField {pw f w} {
        itk_component add $f {
            iwidgets::entryfield $pw -highlightthickness 0 -labelpos w \
                -labeltext $f -textvariable [itcl::scope _Projvals($f)] \
                 -borderwidth 0 -labelmargin 20 -state disabled
        } {}

        pack $itk_component($f) -pady 5 -anchor w -padx 10
    } 
    
    private method alignFields {} {
        set lab {}
        foreach {f} [getprojFields] {
            lappend lab $itk_component($f)
        }
        eval iwidgets::Labeledwidget::alignlabels $lab
    }
 
    method addEmulation {} {
        itk_component add ew {
            iwidgets::labeledframe $itk_component(top).ew -labelpos nw \
                -labeltext "DUT Emulation" -borderwidth 3
        }
        set ewt [$itk_component(ew) childsite]

        # itk_component add ewt {
        #     iwidgets::labeledframe $ew.ewt -labelpos nw \
        #         -labeltext "Testbench"
        # }
        # set ewt [$itk_component(ewt) childsite]

        itk_component add em {
            checkbutton $ewt.em -text "Manually apply inputs" -variable [itcl::scope _Answer(emu-tb-mode)] \
              -onvalue manual
        }
        itk_component add ectb {
            checkbutton $ewt.ectb -text "C++ Testbench" -variable [itcl::scope _Answer(emu-tb-mode)] -onvalue ctb
        }        
    
        itk_component add cfield {
            iwidgets::combobox $ewt.cfield -textvariable [itcl::scope _Answer(emu-ctb-file)] 
        } {}
        

        # foreach t [get_ctbs] {
        #     eval $itk_component(cfield) insert list end $t
        # }

        itk_component add cbb { 
            ttk::button $ewt.cbb -text "Add" -width 8 \
                -command "$this mkTb emu"
        } {}     
        itk_component add cbb2 { 
            ttk::button $ewt.cbb2 -text "Edit" -width 8 \
                -command "$this editTb emu"
        } {}     
        itk_component add cbb3 {
            ttk::button $ewt.cbb3 -text "Delete" -width 8 \
                -command "$this deleteTb emu"
        } {}

	itk_component add simcheck {
            checkbutton $ewt.simcheck -text "Simulation Testbench" -variable [itcl::scope _Answer(emu-tb-mode)] -onvalue stb
        }        
    
        itk_component add sfield {
            iwidgets::combobox $ewt.sfield -textvariable [itcl::scope _Answer(emu-stb-file)] 
        } {}
        
        itk_component add sbb { 
            ttk::button $ewt.sbb -text "Add" -width 8 \
                -command "$this mkSTb emu"
        } {}     
        itk_component add sbb2 { 
            ttk::button $ewt.sbb2 -text "Edit" -width 8 \
                -command "$this editSTb emu"
        } {}     
        itk_component add sbb3 {
            ttk::button $ewt.sbb3 -text "Delete" -width 8 \
                -command "$this deleteSTb emu"
        } {}

        

        grid $itk_component(em) -row 0 -column 0 -sticky w

        grid $itk_component(ectb) -row 1 -column 0 -sticky w
        grid $itk_component(cfield) -row 1 -column 1 -padx 5
        grid $itk_component(cbb) -row 1 -column 2  -padx 5 -pady 5
        grid $itk_component(cbb2) -row 1 -column 3  -padx 2 -pady 5
        grid $itk_component(cbb3) -row 1 -column 4 -padx 15 -pady 5

        # grid $itk_component(simcheck) -row 2 -column 0 -sticky w
        # grid $itk_component(simfield) -row 2 -column 1 
        # grid $itk_component(simulator) -row 2 -column 2

        grid $itk_component(simcheck) -row 2 -column 0 -sticky w
        grid $itk_component(sfield) -row 2 -column 1 -padx 5
        grid $itk_component(sbb) -row 2 -column 2  -padx 5 -pady 5
        grid $itk_component(sbb2) -row 2 -column 3  -padx 2 -pady 5
        grid $itk_component(sbb3) -row 2 -column 4 -padx 15 -pady 5


      #  pack $itk_component(ewt) -side left  -expand true -fill x -padx 20
        
        # itk_component add bwt {        
        #     iwidgets::labeledframe $ew.bwt -labelpos nw \
        #         -labeltext "Board"
        # }
        # set bwt [$itk_component(bwt) childsite]
        
        # foreach {f} [getemFields] {
        #     addCheckBox $bwt.$f $f $bwt
        # }

        # $itk_component(ml605) configure -state normal
        # $itk_component(kc705) configure -state normal
        # $itk_component(vc707) configure -state normal
        # $itk_component(none) configure -state normal
        

#        pack $itk_component(bwt) -fill x -expand true -padx 20
#        pack $itk_component(ewt) -after $itk_component(bwt)
#         pack $itk_component(ew) -fill x -expand true
    }

    method addCheckBox {pw f w} {
        # itk_component add $f {
        #     checkbutton $pw -text $f -variable [itcl::scope _Answer($f)] \
        #         -state disabled
        # }
        itk_component add $f {
            radiobutton $pw -text $f -variable [itcl::scope _Answer(board)] \
                -state normal -value $f 
        }
      
        pack $itk_component($f) -pady 5 -side left -fill x -expand true
    } 
 
    method addSimulation {} {
        itk_component add sw {
            iwidgets::labeledframe $itk_component(top).sw -labelpos nw \
                -labeltext "DUT Simulation" -borderwidth 3
        }
        set swt [$itk_component(sw) childsite]

        # itk_component add swt {
        #     iwidgets::labeledframe $sw.swt -labelpos nw \
        #         -labeltext "Testbench"
        # }

        #set swt [$itk_component(swt) childsite]
        itk_component add sm {
            checkbutton $swt.sm -text "Manually apply inputs" -variable [itcl::scope _Answer(sim-tb-mode)] \
              -onvalue manual
        }
        itk_component add sctb {
            checkbutton $swt.sctb -text "C++ Testbench" -variable [itcl::scope _Answer(sim-tb-mode)] -onvalue ctb
        }        

        itk_component add scfield {
            iwidgets::combobox $swt.scfield -textvariable [itcl::scope _Answer(sim-ctb-file)] 
        } {}
        

        itk_component add scbb { 
            ttk::button $swt.scbb -text "Add" -width 8 \
                -command "$this mkTb sim"
        } {}     

        itk_component add scbb2 { 
            ttk::button $swt.scbb2 -text "Edit" -width 8 \
                -command "$this editTb sim"
        } {}     

        itk_component add scbb3 { 
            ttk::button $swt.scbb3 -text "Delete" -width 8 \
                -command "$this deleteTb sim"
        } {}     

	itk_component add ssimcheck {
	    checkbutton $swt.ssimcheck -text "Simulation Testbench" -variable [itcl::scope _Answer(sim-tb-mode)] -onvalue stb
	}        
    
        itk_component add ssfield {
            iwidgets::combobox $swt.ssfield -textvariable [itcl::scope _Answer(sim-stb-file)] 
        } {}
        
        itk_component add ssbb { 
            ttk::button $swt.ssbb -text "Add" -width 8 \
                -command "$this mkSTb sim"
        } {}     
        itk_component add ssbb2 { 
            ttk::button $swt.ssbb2 -text "Edit" -width 8 \
                -command "$this editSTb sim"
        } {}     
        itk_component add ssbb3 {
            ttk::button $swt.ssbb3 -text "Delete" -width 8 \
                -command "$this deleteSTb sim"
        } {}


        grid $itk_component(sm) -row 0 -column 0 -sticky w

        grid $itk_component(sctb) -row 1 -column 0 -sticky w
        grid $itk_component(scfield) -row 1 -column 1 -padx 5
        grid $itk_component(scbb) -row 1 -column 2  -padx 5 -pady 5
        grid $itk_component(scbb2) -row 1 -column 3  -padx 2 -pady 5
        grid $itk_component(scbb3) -row 1 -column 4  -padx 15 -pady 5

	# grid $itk_component(ssimcheck) -row 2 -column 0 -sticky w
        # grid $itk_component(ssimfield) -row 2 -column 1 
        # grid $itk_component(ssimulator) -row 2 -column 2

	grid $itk_component(ssimcheck) -row 2 -column 0 -sticky w
        grid $itk_component(ssfield) -row 2 -column 1 -padx 5
        grid $itk_component(ssbb) -row 2 -column 2  -padx 5 -pady 5
        grid $itk_component(ssbb2) -row 2 -column 3  -padx 2 -pady 5
        grid $itk_component(ssbb3) -row 2 -column 4 -padx 15 -pady 5

    #    pack $itk_component(sw) -side left  -expand true 

	# code to add simulator selection here (in case we ever want to move it)
	# itk_component add xwt {
        #     iwidgets::labeledframe $sw.xwt -labelpos nw \
        #         -labeltext "Simulator"
        # }

        # set xwt [$itk_component(xwt) childsite]

	# set zz "A"
	# itk_component add zzfield {
        #     iwidgets::combobox $xwt.zzfield -textvariable zz
        # } {}

	# pack $itk_component(xwt) -fill x -expand true -padx 20
	# pack $itk_component(zzfield) -anchor w -pady 5
	# pack $itk_component(swt) -after $itk_component(xwt)
    }

    
    proc startCheck {} {
	set __just_check true
    }

    proc endCheck {} {
	set __just_check false
    }
   
    proc loadEm {} {
	#parray _Answer
        set board [semu_viewer::whichBoard]
        if {$board != $_Answer(boardbuilt) && $board != "None"} {
            set answer [tk_messageBox -title "Load Bit File" -type okcancel -icon warning \
                            -message "The current bit file was built for the $_Answer(boardbuilt) board which doesn't match the connected $board board.  Would you like to proceed anyway?"]
            if {$answer == "cancel"} {
                return
            }
        }

        set canrunemu [lindex [.view checkCanRun] 0]

        #puts "canrunemu $canrunemu"

        # if {!$canrunemu} {
        #     set answer [tk_messageBox -title "Load Emulation" -type okcancel \
        #                     -message "The built project is out of date with the configuration files.  Press OK to load the bit file."]
        #     switch -- $answer {
        #         cancel {
        #             return
        #         }
        #     }
        # }


        set xpath "build/emu/xilinx"
        if { [info exists env(SEMU_XILINX_DIR)] } {
            set xpath $env(SEMU_XILINX_DIR)
        }

        if {[file exists $xpath/mkBridge_jtag.cmd]} {
            set needed_files  [list $xpath/mkBridge_jtag.cmd $xpath/mkBridge.bit]
        } else {
            set needed_files  [list $xpath/program.cmd $xpath/fpga.bit]
        }

	set missing_files [list]
	foreach ff $needed_files {
	    if {![file readable $ff]} {
               # puts "xpath $xpath missing file $ff"
		lappend missing_files $ff
	    }
	}

        if {[catch "exec which impact" result]} {
	    if {$__just_check} {
		return false
	    } else {
		unavailable_option "Xilinx tools are not in the path."
		return
	    }
        }

	if {[llength $missing_files] != 0} {
	    if {$__just_check} {
		return false
	    }
	    unavailable_option "The following file(s) are unavailable: $missing_files ."
	    return
	}

	if {$__just_check} {
	    return true
	}

        cd $xpath
        if {[file exists mkBridge_jtag.cmd]} {
            semu_viewer::execute_in_shell_wait "impact -batch mkBridge_jtag.cmd" [itcl::code runDone "Programming completed"]
        } else {
            semu_viewer::execute_in_shell_wait "impact -batch program.cmd" [itcl::code runDone "Programming completed"]
        } 

        cd ../../..
        checkLoad

        if {$_LoadDone} {
            if {![file exists /sys/bus/pci/rescan]} {
                tk_messageBox -title "Info" -icon info -message "A warm reboot is required before running emulation"
                return
            } else {
                if {$_LoadDone} {
                    hotswap
                }
            }
        }
    }

    proc hotswap {} {
        if {[catch "exec groups" user]} {
            puts stderr $user
        } 
        
        # if {[lsearch -regexp $user plugdev] <  0} {
        #     tk_messageBox -title "Info" -icon info -message "User must be in group plugdev"
        #     return
        # }

        set hotswapcode [semu_viewer::execute_in_shell_wait "bluenoc_hotswap" [itcl::code runDone "hotswap attempted"]]
        if {$hotswapcode == "" || ![file exists /dev/bluenoc_1]} {
            puts "Unable to hotswap"
            tk_messageBox -title "Info" -icon info -message "A warm reboot is required before running emulation"
            return
        } else {
            puts "hotswap successful"
        }
        
        tk_messageBox -title "Info" -icon info -message "Board is ready to run emulation"
    }
        
            
            

    proc resetEm {} {
	if {$__just_check} {
	    return true
	}
        semu_viewer::execute_in_shell_wait "bluenoc reset" [itcl::code runDone "Reset done"]
    }

    proc stop_shells {} {
	set procs_to_stop [status_window::stop true]
	if {$__just_check} {
	    return $procs_to_stop
	}
	if {!$procs_to_stop} {
	    unavailable_option "There are no processes to stop."
	    return
	}
	status_window::stop
        semu_viewer::display_message "Process stopped" false
    } 


    # proc changeSpeed {} {
    #     puts "change speed selected"
    #     parray _Projvals
        
    #     $itk_component(askSpeed) activate

    #     # $itk_component(askSpeed) sendSpeed $_Projvals(design-clock-frequency)
        
    #     # set newspeed [$itk_component(askSpeed) getSpeed]
    #     puts "new speed $newspeed"

    # }

    method stop_xterms {} {
	set pids [list]

	foreach pid $__pids {
	    if {[processExists $pid]} {
		lappend pids $pid
	    }
	}
	set __pids $pids

	if {$__just_check} {
	    if {$pids == ""} {
		return false
	    }
	    return true
	}

	if {$pids == ""} {
	    unavailable_option "There are no processes to stop."
	    return
	}
	
	set cmd "kill $pids"
	catch "exec $cmd"
	set __pids [list]
    }

    ################################################################################
    ###
    ################################################################################

    proc startEm {{no_dbg false}} {

        set canrunemu [lindex [.view checkCanRun] 0]
        #puts "canrunemu $canrunemu"

        #  if {!$canrunemu} {
        #     set answer [tk_messageBox -title "Run Emulation" -type okcancel \
        #                     -message "The built project is out of date with the configuration files.  Press OK to run with the out of date build."]
        #     switch -- $answer {
        #         cancel {
        #             return
        #         }
        #     }
        # }

	set tb_mode $_Build(emu-tb-mode) 
	if {$__just_check && $tb_mode == 0} {
	    return false
	}

	if {$tb_mode == 0} {
	    unavailable_option "No emulation testbench has been specified."
	    return
	}

	set no_dbg_flag ""
	if {$no_dbg} {
	    set no_dbg_flag "-no_dbg"
	}

	set gui_exe $::env(BLUESPECDIR)/tcllib/semu/dbg-gui

        if {![readBuild $__just_check]} {return false}
	global env


        set path "build/emu/scripts"
        if { [info exists env(SEMU_EMU_SCRIPTS_DIR)] } {
            set path $env(SEMU_EMU_SCRIPTS_DIR)
        }

        set module "mkBridge"
        set top  "/mkBridge/scemi_dut_dutIfc/ifc_raw"
        
        if {[array names _Build multi-fpga] != ""} {
            if {$_Build(multi-fpga)} {
                set module "fpga_a"
                set top "/fpga_a/top/scemi_dut_dutIfc/ifc_raw"
            }
        }
        if { [info exists env(SEMU_XILINX_MODULE)] } {
            set module $env(SEMU_XILINX_MODULE)
        }
        
        set xpath "build/emu/xilinx"
        if { [info exists env(SEMU_XILINX_DIR)] } {
            set xpath $env(SEMU_XILINX_DIR)
        }
        
        set paramfile "scemi.params"
        if { [info exists env(SEMU_SCEMI_PARAM_FILE)] } {
            set paramfile $env(SEMU_SCEMI_PARAM_FILE)
        }
        
#        set top "/mkBridge/scemi_dut_dutIfc/ifc_raw"
        if {[info exists env(SEMU_TOP_MOD)] } {
            set top $env(SEMU_TOP_MOD)
        }

	set sed_cmd [concat {sed -n {s/Link .* TCPPort *\([0-9]*\)$/\1/p} } "$path/$paramfile"]
	if {[catch "exec $sed_cmd" tcp_port]} {
	    puts stderr $tcp_port
	}
	if {! [string is integer "$tcp_port"]} {
	    set tcp_port 5566
	}
        
	if {$tb_mode == "manual"} {
            #puts "tbmode manual module $module"
            set cmd "$gui_exe $path/$paramfile $no_dbg_flag -xrf $xpath/$module.xrf -mode emu -top $top"
            #puts "cmd $cmd"
            set r [catch "exec $cmd -missing" msg]
	    if {$__just_check } {
		if {$r} { return false }
		return true
	    }
	    if {$r} {
		unavailable_option $msg
		return
	    }

	    resetEm
	    #puts "gui cmd is: $cmd"
	    set pid [status_window::run_in_xterm "sh  -e -c \{ $cmd ; echo 'Testbench Completed.\n\n\nUser must close this window'; \}" -title "Testbench Console"]
	    lappend __pids $pid
	}
	if {$tb_mode == "stb"} {
	    set tb $_Build(emu-stb-file)
	    if {$tb == "" && !$__just_check} {
		unavailable_option "No emulation testbench has been specified."
		return
	    }
	    set gui [read_stbfile $tb]
	    set cmd "$gui_exe $path/$paramfile $no_dbg_flag -xrf $xpath/$module.xrf -mode emu -port $tcp_port -simtb $path/$tb -gui $gui -top $top"
	    set r [catch "exec $cmd -missing" msg]
	    if {$__just_check } {
		if {$r} { return false }
		return true
	    }
	    if {$r} {
		unavailable_option $msg
		return
	    }

	    resetEm
	    #puts "gui cmd is: $cmd"
	    set pid [status_window::run_in_xterm "sh  -e -c \{ $cmd ; echo 'Testbench Completed.\n\n\nUser must close this window'; \}" -title "Testbench Console"]
	    lappend __pids $pid
	}
	if {$tb_mode == "ctb"} {
            set tb $_Build(emu-ctb-file)
	    if {$tb == "" && !$__just_check} {
		unavailable_option "No emulation testbench has been specified."
		return
	    }
            set gui [readTbfile $tb]

	    set cmd "$gui_exe $path/$paramfile $no_dbg_flag -xrf $xpath/$module.xrf -mode emu -gui $gui -top $top"
            #puts "cmd is $cmd"
	    set r [catch "exec $cmd -missing" msg]
	    if {$__just_check } {
		if {$r} { return false }
		return true
	    }
	    if {$r} {
		unavailable_option $msg
		return
	    }

	    resetEm
	    #puts "gui cmd is: $cmd"
	    set pid [status_window::run_in_xterm "sh  -e -c \{ $cmd ; echo 'Testbench Completed.\n\n\nUser must close this window' ;\}" -title "Testbench Console"]
	    lappend __pids $pid
	}
    }
    
    proc startSim {} {

	set tb_mode $_Build(sim-tb-mode) 
	if {$__just_check && $tb_mode == 0} {
	    return false
	}
	if {$tb_mode == 0} {
	    unavailable_option "No simulation testbench has been specified."
	    return
	}

	set gui_exe $::env(BLUESPECDIR)/tcllib/semu/dbg-gui

	if {![readBuild $__just_check]} {return false}

        set canrunsim [lindex [.view checkCanRun] 1]
        #puts "canrunsim $canrunsim"

        # if {!$canrunsim} {
        #     set answer [tk_messageBox -title "Run Simulation" -type okcancel \
        #                     -message "The built project is out of date with the configuration files.  Press OK to run with the out of date build."]
        #     switch -- $answer {
        #         cancel {
        #             return
        #         }
        #     }
        # }

	global env

        set path "build/sim/scripts"
        if { [info exists env(SEMU_SIM_SCRIPTS_DIR)] } {
            set path $env(SEMU_SIM_SCRIPTS_DIR)
        }
        
        set module "mkBridge"
        if { [info exists env(SEMU_XILINX_MODULE)] } {
            set module $env(SEMU_XILINX_MODULE)
        }
        
        #set xpath "build/sim/xilinx"
        set xpath "build/emu/xilinx"
        if { [info exists env(SEMU_XILINX_DIR)] } {
            set xpath $env(SEMU_XILINX_DIR)
        }
        
        set paramfile "scemi.params"
        if { [info exists env(SEMU_SCEMI_PARAM_FILE)] } {
            set paramfile $env(SEMU_SCEMI_PARAM_FILE)
        }
        
#        set top ""
        set top "/mkBridge/scemi_dut_dutIfc/ifc_raw"
        if {[info exists env(SEMU_TOP_MOD)] } {
            set top $env(SEMU_TOP_MOD)
        }

        if {$tb_mode == "manual"}  {
            return [runSim man $path $paramfile]
        } 

	if {$tb_mode == "stb"} {
	    set tb $_Build(sim-stb-file)
	    if {$tb == "" && !$__just_check} {
		unavailable_option "No simulation testbench has been specified."
		return
	    }
	    set tbdir [read_stbfile $tb] 
            return [runSim $tbdir $path $paramfile $tb]
	} else {
            set tb $_Build(sim-ctb-file) 
	    if {$tb == "" && !$__just_check} {
		unavailable_option "No simulation testbench has been specified."
		return
	    }
            set tbdir [readTbfile $tb]
            return [runSim $tbdir $path $paramfile]
        }
    }

	

    proc runSim {tbdir path paramfile {simtb ""}} {

	#puts "RUNSIM $tbdir $path $paramfile"

	set sed_cmd [concat {sed -n {s/Link .* TCPPort *\([0-9]*\)$/\1/p} } "$path/$paramfile"]
	if {[catch "exec $sed_cmd" tcp_port]} {
	    puts stderr $tcp_port
	}
	if {! [string is integer "$tcp_port"]} {
	    set tcp_port 5566
	}

	set gui_exe $::env(BLUESPECDIR)/tcllib/semu/dbg-gui
	set cmd "$gui_exe $path/$paramfile -no_dbg -mode sim -gui $tbdir"
	if {$simtb != ""} {
	    set cmd "$cmd -port $tcp_port -simtb $path/$simtb"
	}
	set r [catch "exec $cmd -missing" msg]
	set exe_exists [file exists $path/rundut]
	if {$__just_check } {
	    if {$r} { return false }
	    return $exe_exists
	}
	if {$r} {
	    unavailable_option $msg
	    return
	}
	if {!$exe_exists} {
	    unavailable_option "The following file(s) are unavailable: $path/rundut ."
	    return
	}

        .view configure -cursor watch
        update
        set port [extractPort $path/$paramfile]

	#puts "simulation cmd is: $path/rundut"
        set pid_0 [status_window::run_in_xterm "sh  -e -c \{ $path/rundut; echo 'Simulation Completed.\n\n\nUser must close this window'; \}" -title "Simulation Console"]
	lappend __pids $pid_0

	waitForPort $port

        .view configure -cursor arrow
        update

	#puts "gui cmd is: $cmd"
        set pid_1 [status_window::run_in_xterm "sh  -e -c \{ $cmd; echo 'Testbench completed.\n\nUser must close this window';\}" -title "Testbench Console"]
	lappend __pids $pid_1
    }

    proc extractPort {param} {
        set pfile [open $param "r"]
        while {[gets $pfile line ] >= 0} {
            if {[regexp {(.*)TCPPort (.*)} $line portfull key value]} {
                #puts " $value"
		close $pfile
                return [string trim $value]
            }
        }
        close $pfile
    }

    proc waitForPort {port} {
	set i 0;
	while {$i < 20} {
	    if {![catch [list exec lsof -n -P -i :$port] result]} {
		break
	    }
	    after 100
	}
    }

    proc processExists {pid} {
        if {$pid == -1} { return 0 }
        set value [expr ![catch {exec kill -s 0 $pid} msg]]
        return $value
    }


    proc createSimTbBldCmd {target} {
	set cmd "build -p sim_tb.bld -v $target --from link_for_verilog tcl_tb$target;"
	return $cmd
    }

    proc createXrf {} {

	set module "mkBridge"
        if { [info exists env(SEMU_XILINX_MODULE)] } {
            set module $env(SEMU_XILINX_MODULE)
        }

	set xpath "build/emu/xilinx"
        if { [info exists env(SEMU_XILINX_DIR)] } {
            set xpath $env(SEMU_XILINX_DIR)
        }

	set xrf_exe $::env(BLUESPECDIR)/tcllib/semu/create_xrf

	set cmd "$xrf_exe -missing -pre $xpath/$module foo.xrf"

	set r [catch "exec $cmd" msg]

	if {$__just_check } {
	    if {$r} { 
		return -code error $msg
	    }
	    return true
	}

	if {$r} {
	    unavailable_option $msg
	    return
	}

        if {$_Answer(board) == 0} {
            tk_messageBox -title "Error" -icon error -message "Board must be selected in Settings before building"
            return
        }

	set build_cmd "build -v emu_$_Answer(board) --from create_xrf --to create_xrf"
	semu_viewer::execute_in_shell $build_cmd [itcl::code runDone "Xrf build finished"]
    }
   
    proc buildEMProject {} {
        if {![readOptions]} {
            return
        }

        if {$_Answer(emu-tb-mode) == 0 || $_Answer(board) == ""} {
            tk_messageBox -title "Error" -icon error -message "Board and testbench must be selected in Settings before building"
            return
        }
        set build_cmd "build -v"
        #puts "board: $_Answer(board) buildtargets $buildtargets"
        if {$_Answer(board) != ""} {
            if {$_Answer(board) != "none"} {
                if {[catch "exec which impact" result]} {
                    tk_messageBox -title "Error" -icon error -message "Xilinx tools are not in the path"
                    return
                }
		set build_cmd "$build_cmd emu_$_Answer(board);"
            }
        }
        
        set attboard [semu_viewer::whichBoard]
        if {$attboard != "None" && $attboard != $_Answer(board)} {
            set answer [tk_messageBox -title "Board Mismatch Warning" -icon warning \
                            -message "The attached board ($attboard) does not match the board specified in Configure Hardware ($_Answer(board)).  Do you want to continue to build?" \
                            -type okcancel]
            if {$answer == "cancel"} {
                return
            }
        }

        if {$_Answer(emu-tb-mode) == "ctb"} {
            set emtarget emu$_Answer(emu-ctb-file)
	    set build_cmd "$build_cmd build -v $emtarget;"
        } 
        if {$_Answer(emu-tb-mode) == "stb"} {
            set emtarget emu$_Answer(emu-stb-file)
	    set build_cmd "$build_cmd [createSimTbBldCmd $emtarget]"
        } 
	if {$_Answer(emu-tb-mode) == "manual"} {
	    set build_cmd "$build_cmd build -v emu_manualtb;"
        }

        if {[file exists build/emu/xilinx/fpga.bit] || [file exists build/emu/xilinx/mkBridge.bit]} {
            if {![warnBuild]} {
                return
            }             
        }                                                

       # puts "CMD: $build_cmd"
        set _Answer(boardbuilt) $_Answer(board)
        writeBuild $_Answer(board)
        semu_viewer::execute_in_shell $build_cmd [itcl::code runDone "Emulation build finished"]
    }

    proc warnBuild {} {
        set answer [tk_messageBox -title "Build Emulation" -type yesno \
                        -message "A bit file already exists. Do you want to build a new one?"]
        if {$answer == "no"} {
            set build False
        } else {
            set build True
            semu_viewer::execute_in_shell_wait "rm -rf build/oldemu"  [itcl::code msgDone "Saving previous build"]
            semu_viewer::execute_in_shell_wait "mv build/emu build/oldemu" [itcl::code msgDone "Previous build saved"]
        }
        return $build
    }


    proc buildSimProject {} {
        if {![readOptions]} {
            return
        }
        if {$_Answer(sim-tb-mode) == 0} {
            tk_messageBox -title "Error" -icon error -message "Testbench must be selected in Settings before building"
            return
        }
        set build_cmd "build -v sim_dut;"
        #parray _Answer
        if {$_Answer(sim-tb-mode) == "ctb"} {
            set simtarget sim$_Answer(sim-ctb-file)
	    set build_cmd "$build_cmd build -v $simtarget;"
        } 
        if {$_Answer(sim-tb-mode) == "stb"} {
            set simtarget sim$_Answer(sim-stb-file)
	    set build_cmd "$build_cmd [createSimTbBldCmd $simtarget]"
        } 
	if {$_Answer(sim-tb-mode) == "manual"} { 
	    set build_cmd "build -v sim"
        }
            
#	puts "CMD: $build_cmd"
        semu_viewer::execute_in_shell $build_cmd [itcl::code runDone "Simulation build finished"]
       
    }

    
    proc buildBothTb {mode} {
        set fulldone False
        if {![readOptions]} {
            return
        }

        if {$mode == "emu"} {
            if [file exists build/emu] {
                if {[file exists build/emu/xilinx/fpga.bit] || [file exists build/emu/xilinx/mkBridge.bit]} {
                    set fulldone True
                } else {
                    set fulldone False
                }                
            } else {
                set fulldone False 
            }
        } else {
            if [file exists build/sim] {
                set fulldone True
            }
        }
        if {!$fulldone} {
            tk_messageBox -title "Error" -icon error -message "Full build must be completed before building testbench" 
            return
            
        } 

        set build_cmd "build -v"

        if {$_Answer($mode-tb-mode) == "manual"} {
	    set build_cmd "$build_cmd $mode\_manualtb"
	}
        if {$_Answer($mode-tb-mode) == "ctb"} {
            set build_cmd "$build_cmd $mode$_Answer($mode-ctb-file)"
        }
	if {$_Answer($mode-tb-mode) == "stb"} {
            set build_cmd [createSimTbBldCmd $mode$_Answer($mode-stb-file)]
        } 
            
        semu_viewer::execute_in_shell $build_cmd [itcl::code runDone "Testbench build complete"]
    }   

    proc readTbfile {editname} {
        set tbfile [open "testbench.bld" "r"]
        while {[gets $tbfile line] >= 0} {
            if {[regexp {^\[emu(.*)\]} $line tbfull]} {
                regsub -all {\[|\]} $tbfull "" tbname
                regsub {^emu} $tbname "" tbnamefinal
                if {$editname == $tbnamefinal} {
                    while {$line != "" && $line >=0} {
                        gets $tbfile line
                        regexp {(.*): (.*)} $line fulline key value
                        if {$key == "#sourcedir"} {
                            set tbgui $value
                            
                        }
                    }
                }
            }
        }
        close $tbfile
        return $tbgui
    }

    proc read_stbfile {editname} {
        set tbfile [open "sim_tb.bld" "r"]
        while {[gets $tbfile line] >= 0} {
            if {[regexp {^\[emu(.*)\]} $line tbfull]} {
                regsub -all {\[|\]} $tbfull "" tbname
                regsub {^emu} $tbname "" tbnamefinal
                if {$editname == $tbnamefinal} {
                    while {$line != "" && $line >=0} {
                        gets $tbfile line
                        regexp {(.*): (.*)} $line fulline key value
                        if {$key == "#sourcedir"} {
                            set tbgui $value
                            
                        }
                    }
                }
            }
        }
        close $tbfile
        return $tbgui
    } 
    
   method finish {} {
       writeBuild $_Answer(board)
       .view checkCanBuild
       [itcl::code $this deactivate 1]
   }

    proc msgDone {msg} {
        semu_viewer::display_message $msg false
    }
    
    proc checkLoad {} {
        set success "Programmed successfully"
        if {[catch {exec grep $success build/emu/xilinx/_impactbatch.log} err]} {
            #puts "error $err"
            tk_messageBox -title "Error" -icon error -message "Bit File not Loaded"
            set _LoadDone False
        } else {
           # puts "no error $err"
            set _LoadDone True
        }
    }
 
    proc runDone {msg} {
#        .view menuChange
        semu_viewer::display_message $msg false
        .view checkCanBuild
        return
    }
    
    proc writeBuild {board} {
        set isfile [file exists $_Buildfile]
        if {!$isfile} {
            readOptions
        } else {
            if {[checkBChanges $board] || $board != ""} {
                file delete -force  $_Buildfile
                #puts "delete Buildfile"
          } else {
                return
            }
        }
        
        set fh [open $_Buildfile "w"]
        set pln ""
        foreach {f} [getFields] {
            append pln "sim-$f: $_Answer(sim-$f)" \n
            append pln "emu-$f: $_Answer(emu-$f)" \n
        }
        #append pln "board: $_Answer(board)" \n
        append pln "board: $board" \n
        if {$board == "b2000t"} {
            append pln "multi-fpga: True" \n
        } else {
            append pln "multi-fpga: False" \n
        }
        #puts "board: $board"
        append pln "boardbuilt: $_Answer(boardbuilt)" \n
        puts $fh $pln
        close $fh
    }

    proc checkBChanges {board} {
        #puts " $board $_Build(board) $_Answer(board)"
        set changed False
        foreach {f} [getFields] {
            if {$_Answer(sim-$f) != $_Build(sim-$f) || $_Answer(emu-$f) != $_Build(emu-$f) || $_Build(board) != $board || $_Answer(boardbuilt) != ""} {
                set changed True
            }
        }
        #puts "board changed $changed"
        return $changed
    } 

    method mkSTb {target} {
        if {$target == "sim"} {
            set name $_Answer(sim-stb-file)
            set oname $_Answer(emu-stb-file)
        } else {
            set name $_Answer(emu-stb-file)
            set oname $_Answer(sim-stb-file)
        }
	set name2 $name

        if {[lsearch -exact [get_stbs] $name] > -1} {
            set name ""
        }
        
        $itk_component(sim_testbench) getTopmod $_Projvals(top-module)
        $itk_component(sim_testbench) setmode "tb" $target $name new $_Projvals(top-file) $name2 $_Projvals(verilog-simulator)
        $itk_component(sim_testbench) activate

        $itk_component(sfield)  delete list 0 end 
        $itk_component(ssfield) delete list 0 end

        foreach t [get_stbs] {
            eval $itk_component(sfield) insert list end $t
            eval $itk_component(ssfield) insert list end $t
        }

        set tbname [$itk_component(sim_testbench) getname]
        if {$target == "sim"} {
            $itk_component(ssfield) insert entry 0 $tbname
            $itk_component(sfield) insert entry 0 $oname
        } else {
            $itk_component(sfield) insert entry 0 $tbname
            $itk_component(ssfield) insert entry 0 $oname
        }
    }

    method mkTb {target} {
        if {$target == "sim"} {
            set name $_Answer(sim-ctb-file)
            set oname $_Answer(emu-ctb-file)
        } else {
            set name $_Answer(emu-ctb-file)
            set oname $_Answer(sim-ctb-file)
        }
        if {[lsearch -exact [get_ctbs] $name] > -1} {
            set name ""
        } 
        
        $itk_component(semu_testbench) getTopmod $_Projvals(top-module)
        $itk_component(semu_testbench) setmode "tb" $target $name new $_Projvals(top-file)
        $itk_component(semu_testbench) activate

        $itk_component(cfield)  delete list 0 end 
        $itk_component(scfield) delete list 0 end

        foreach t [get_ctbs] {
            eval $itk_component(cfield) insert list end $t
            eval $itk_component(scfield) insert list end $t
        }

        set tbname [$itk_component(semu_testbench) getname]
        if {$target == "sim"} {
            $itk_component(scfield) insert entry 0 $tbname
            $itk_component(cfield) insert entry 0 $oname
        } else {
            $itk_component(cfield) insert entry 0 $tbname
            $itk_component(scfield) insert entry 0 $oname
        }
    }
    
    method editSTb {target} {
        #puts "in editTb"
        if {$target == "sim"} {
            set name $_Answer(sim-stb-file)
        } else {
            set name $_Answer(emu-stb-file)
        }
        if {$name == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The Simulation Testbench field cannot be blank" \
                -parent $itk_interior
            return
        }
        if {[lsearch -exact $_stb $name] >= 0} {
            #puts "readfile from semu_build"
            $itk_component(sim_testbench) setmode "tb" $target $name edit $_Projvals(top-file) $name ""
            $itk_component(sim_testbench) readfile $name
            $itk_component(sim_testbench) getTopmod $_Projvals(top-module)
            $itk_component(sim_testbench) activate
        } else {
             tk_messageBox -title "Error" -icon error \
                 -message "The Simulation Testbench must already exist" \
                 -parent $itk_interior
            return
        }
    }

 method editTb {target} {
        #puts "in editTb"
        if {$target == "sim"} {
            set name $_Answer(sim-ctb-file)
        } else {
            set name $_Answer(emu-ctb-file)
        }
        if {$name == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The C++ Testbench field cannot be blank" \
                -parent $itk_interior
            return
        }
        if {[lsearch -exact $_ctb $name] >= 0} {
            #puts "readfile from semu_build"
            $itk_component(semu_testbench) setmode "tb" $target $name edit $_Projvals(top-file)
            $itk_component(semu_testbench) readfile $name
            $itk_component(semu_testbench) getTopmod $_Projvals(top-module)
            $itk_component(semu_testbench) activate
        } else {
             tk_messageBox -title "Error" -icon error \
                 -message "The C++ Testbench must already exist" \
                 -parent $itk_interior
            return
        }
    }

       
    method deleteSTb {target} {
        if {$target == "sim"} {
            set name $_Answer(sim-stb-file)
            set oname $_Answer(emu-stb-file)
        } else {
            set name $_Answer(emu-stb-file)
            set oname $_Answer(sim-stb-file)
        }

        if {$name == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The Simulation Testbench field cannot be blank" \
                -parent $itk_interior
            return
        }

        $itk_component(sim_testbench) deletetb $name

        $itk_component(sfield)  clear
        $itk_component(ssfield) clear
      
        foreach t [get_stbs] {
            eval $itk_component(sfield) insert list end $t
            eval $itk_component(ssfield) insert list end $t
        }

        if {$oname != $name} {
            if {$target == "sim"} {
                $itk_component(sfield) insert entry 0 $oname
            } else {
                $itk_component(ssfield) insert entry 0 $oname
            }
        }
    }

    method deleteTb {target} {
        if {$target == "sim"} {
            set name $_Answer(sim-ctb-file)
            set oname $_Answer(emu-ctb-file)
        } else {
            set name $_Answer(emu-ctb-file)
            set oname $_Answer(sim-ctb-file)
        }

        if {$name == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The C++ Testbench field cannot be blank" \
                -parent $itk_interior
            return
        }

        $itk_component(semu_testbench) deletetb $name

        $itk_component(cfield)  clear
        $itk_component(scfield) clear
      
        foreach t [get_ctbs] {
            eval $itk_component(cfield) insert list end $t
            eval $itk_component(scfield) insert list end $t
        }

        if {$oname != $name} {
            if {$target == "sim"} {
                $itk_component(cfield) insert entry 0 $oname
            } else {
                $itk_component(scfield) insert entry 0 $oname
            }
        }
    }
    
    method mkBrowse {field file_type} {
        $itk_component(browse_dia) setMask $file_type
        if {[$itk_component(browse_dia) activate]} {
            $itk_component($field) insert 0 [$itk_component(browse_dia) getSelection]
        } else {
            $itk_component($field) insert 0 ""
        }
    }    

}

