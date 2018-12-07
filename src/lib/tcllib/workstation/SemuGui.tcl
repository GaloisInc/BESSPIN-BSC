#  Copyright Bluespec Inc. 2009-2012

# Tcl/Tk package for building a graphical interface for semu

#package require BhdlEdit
package require Bluetcl
package require Itk
package require Iwidgets 4.0
package require SemuInit
package require Tablelist
package require Waves 2.0
package require semu_build
package require semu_config
package require types

package provide SemuGui 1.0

fonts::set_colours
fonts::initialize

#puts "Sourcing SemuGui.tcl"
#catch "itcl::delete class semu_viewer"

wm title . "Bluespec semu"
wm minsize . 750 600

itcl::class semu_viewer {
    inherit itk::Widget
    common _vcd_file
    common _Viewer ""
    variable _emucontrol
    variable _dutcontrol
    common _Projfile project.cfg
    common _Projdir ""
    common _Projname ""
    variable  _Project
    common _Window ""
    common _build_menu_stale true
    common _run_menu_stale true
    common _build_posted false
    common _run_posted   false
    variable _simbuild False
    common _emubuild False
    common _canbuild True

    constructor {args} {
       # puts "add semu_config"
        itk_component add semu_in {
            semu_config::create .setup
        } {}

        #puts "add semu_build"
        itk_component add semu_build {
            semu_build::getTbSettings
        } {}
        
        #puts "add proj_dialog"
        itk_component add proj_dialog {
            project_dialog::create .proj
        } {}

        itk_component add askSpeed {
            askSpeed::create .#auto
        } {}


	# Get the initial top module
	set top [lindex $args 0]
	set cfg [lindex $args 1]

	set args [lrange $args 2 end]

	# if {$top != ""} {
	#     set args [lreplace $args 0 0]
	# }

	# if {$cfg != ""} {
	#     set args [lreplace $args 0 0]
	# }

        eval itk_initialize $args
        set _Window [winfo name $itk_component(hull)]
        #puts "mkMenubar"
        mkMenuBar $top
        
        #puts "menuInit"
        menuInit
        mkMessageWindow yes
        
        pack $itk_component(mb) -fill x
        pack $itk_component(mtext) -fill both -expand yes

	if {$cfg != ""} {
           # puts "cfg is $cfg"
	    set cfg [file normalize $cfg]
	    set _Projdir [file dirname $cfg]
            set _Projname [file tail   $cfg]   
	    doOpen
	}


    }

    destructor {exit} 

    method mkMessageWindow {redirect} {
        
        itk_component add mtext {
            status_window $itk_interior.mtext $redirect
        } {}
    }

    proc execute_in_shell_wait {cmd closeProc} {
        .view component mtext execute_in_shell_wait $cmd $closeProc
    }

    proc execute_in_shell { cmd closeProc} {
        .view component mtext execute_in_shell $cmd $closeProc
    }

    proc display_message {msg {with_jump true}} {
        .view component mtext display_message $msg $with_jump
    }

    method mkMenuBar {args} {
        #puts "mkMenuBar"
        itk_component add mb {
            base::menubar $itk_interior.mb -menubuttons {
                menubutton project -text "Project"  -underline 0 -menu {
                    options -tearoff false  
                    command new -label "New" -underline 0
                    command open -label "Open"  -underline 0 
                    separator project1
                    command config -label "Configure Hardware..." -underline 10
                    command pin -label "Configure RTL Pins..." -underline 10
                    command define -label "Configure Testbench..." -underline 10    
                    separator project2
                    command saveas -label "Save As..."  -underline 0 
                    command close -label "Close"  -underline 0 
                    command quit -label "Quit"  -underline 0  
                }
                menubutton build -text "Build" -underline 0 -menu {
                    #options -tearoff false -postcommand  "semu_viewer::build_post_cmd $this"
                    options -tearoff false
                    command buildem -label "Build Full Emulation"  -underline 11
                    command buildtb -label "Build Emulation Testbench" -underline 16
                    command bitinfo -label "Bit File Info" -underline 4
                    separator build1
                    command buildsim -label "Build Full Simulation"  -underline 11
                    command buildsimtb -label "Build Simulation Testbench" 
                    separator build2     
                   # command buildxrf -label "Create Xrf File" -underline 7
                   # separator build3
                    command stop -label "Stop" -underline 1
                } 
                menubutton board -text "Board" -underline 0 -menu {
                    options -tearoff false
                    command load -label "Load Bit File"   -underline 0 
                    command reset -label "Reset FPGA"  -underline 0 
                    command speed -label "Change Emulation Speed" -underline 0
                    separator board1
                    command info -label "Board Info" -underline 0
                }
                menubutton run -text "Run"  -underline 0  -menu {
                    #options -tearoff false -postcommand  "semu_viewer::run_post_cmd $this"
                    options -tearoff false
                    # command load -label "Load Bit File"   -underline 0 
                    # command reset -label "Reset FPGA"  -underline 0 
                    # command speed -label "Change Emulation Speed" -underline 0
                    # separator run1
                    command startem -label "Start Emulation"   -underline 6 
               #     command startem_no_dbg -label "Start Emulation (no debug)" -underline 17 
                    separator run2
                    command startsim -label "Start Simulation (no debug)"  -underline 6 
		    separator run3
                    command stop -label "Stop" -underline 1
                }
            } 
        } {}


        add_menu_commands
    }
    
    method add_menu_commands {} {
        #puts "in add_menu_commands"
        set menu $itk_component(mb)
        $menu menuconfigure project.new -command "[itcl::code $this newProject]"
        $menu menuconfigure project.open -command "[itcl::code $this openProject]"
        $menu menuconfigure project.saveas -command "[itcl::code $this saveasProject]"
        $menu menuconfigure project.config -command "[itcl::code $this doConfigure init]"
        $menu menuconfigure project.close -command "[itcl::code $this closeProject]"
        $menu menuconfigure project.quit -command exit
        $menu menuconfigure project.define -command "[itcl::code $this buildProject]"

        $menu menuconfigure project.pin -command "[itcl::code $this editPin]"

        $menu menuconfigure build.buildem -command semu_build::buildEMProject
     #   $menu menuconfigure build.buildxrf -command "semu_build::createXrf"
        $menu menuconfigure build.buildtb -command "semu_build::buildBothTb emu"
        $menu menuconfigure build.buildsim -command semu_build::buildSimProject 
        $menu menuconfigure build.buildsimtb -command "semu_build::buildBothTb sim"
        $menu menuconfigure build.stop -command semu_build::stop_shells
        $menu menuconfigure build.bitinfo -command semu_viewer::bitInfo      
        
        $menu menuconfigure board.load -command semu_build::loadEm
        $menu menuconfigure board.reset -command semu_build::resetEm
        $menu menuconfigure board.speed -command "[itcl::code $this changeSpeed]"
        $menu menuconfigure board.info -command semu_viewer::boardInfo      



        $menu menuconfigure run.startem -command semu_build::startEm
      #  $menu menuconfigure run.startem_no_dbg -command "semu_build::startEm true"
        $menu menuconfigure run.startsim -command semu_build::startSim
	$menu menuconfigure run.stop -command semu_build::stop_xterms

	# bind  $itk_component(mb).menubar.build <Enter> \
	#     "semu_viewer::update_build_menu $itk_component(mb)"

	# bind  $itk_component(mb).menubar.build <Leave> \
	#     "semu_viewer::build_leave_cmd"

	# bind  $itk_component(mb).menubar.run <Enter> \
	#     "semu_viewer::update_run_menu $itk_component(mb)"

	# bind  $itk_component(mb).menubar.run <Leave> \
	#     "semu_viewer::run_leave_cmd"

	# bind  $itk_component(mb).menubar.build.menu <Unmap> \
	#     "semu_viewer::build_unpost_cmd"

         # bind  $itk_component(mb).menubar.run.menu <Unmap> \
         #     "semu_viewer::run_unpost_cmd"

         # base::setTooltip $itk_component(mb).menubar.build.menu \
         #     "Selecting a greyed out option\nwill explain why it is unavailable."

         # base::setTooltip $itk_component(mb).menubar.run.menu \
         #     "Selecting a greyed out option\nwill explain why it is unavailable."
     } 


     method change_menu_status {m n s} {
         #puts "change status: $m $n $s"
         set menu $itk_component(mb).menubar.$m.menu
         for {set i 0} {$i < $n} {incr i} {
             if {[$menu type $i] == "command" || [$menu type $i] == "cascade" } {
                 $menu entryconfigure $i -state $s
             }
         }
     }

     method set_menu_item {m i s} {
         #puts "set_menu_item $m $i $s"
         set menu $itk_component(mb).menubar.$m.menu
         $menu entryconfigure $i -state $s
     }

     method menuInit {} {
         #puts "in MenuInit"
         set menu $itk_component(mb).menubar
         set pmenu $menu.project.menu

         $pmenu entryconfigure "Save As..."  -state disabled
         $pmenu entryconfigure "Configure Hardware..." -state disabled
         $pmenu entryconfigure "Configure Testbench..." -state disabled
         $pmenu entryconfigure "Configure RTL Pins..." -state disabled        
         $pmenu entryconfigure Close -state disabled

         foreach mlist {build run board} {
             set items [expr [$menu.$mlist.menu index end] +1]
             for {set i 0} {$i <= $items} {incr i} {
                 change_menu_status $mlist $i disabled
             }
         }
         set_menu_item board 4 normal
         set_menu_item build 2 normal
     }

     method menuOpen {} {
         set menu $itk_component(mb).menubar
         #puts "menu1 $menu"
         # enable all menu options
         foreach mlist {project} {
             set n [expr [$menu.$mlist.menu index end] +1]
             for {set i 0} {$i <= $n} {incr i} {
                 change_menu_status $mlist $i normal
             }
         }

         #$menu.project.menu entryconfigure "Save As..."  -state disabled
     }

     method checkCanBuild {} {
         #puts "checkCanBuild projectfile $_Projfile"
         set _canbuild True
         set complete true
         if {![file exists project.bld]} {
            # puts "no project.bld"
             set _canbuild False
         } else {
             set pfile [open $_Projfile "r+"]

             while {[gets $pfile line] >= 0} {
                 if {[regexp {(^top-module:) (.*)} $line fulline key value] } {
                     set topmodule $value
                 }
             }
             close $pfile

             if {[file exists $topmodule.pin]} {
                 set pinfile [open $topmodule.pin "r+"]
                 gets $pinfile line
                 if {![regexp {^//complete} $line]} {
                     #puts "pinfile not complete"
                     set complete False
                     set _canbuild False
                     close $pfile
                 } else {
                     set complete True
                 }

             } else {
                 #puts "no pin file"
                 set _canbuild False
                 set complete False
             }
         }

         # if {$_canbuild} {
         #     #puts "canbuild $_canbuild going to checkWhat"
         #     checkWhatBuild
         # }
         #puts "before updatemenuscall _canbuild $_canbuild"

         if {$_canbuild} {
             set _emubuild True
             set _simbuild True
         } else {
             set _emubuild False
             set _simbuild False
         }            
         updateMenus
     }

     method checkWhatBuild {} {
         #puts "checkWhatBuild"
         set _emubuild True
         set _simbuild True
         if {[file exists project.blt]} {
             set bfile [open project.blt "r+"]
             while {[gets $bfile line] > 0} {
                 if {[regexp {(.*): (.*)} $line fulline key value] } {
                     if {$key == "board"} {
                         if {$value == ""} {
                             puts "board $value"
                             set _emubuild False
                         }
                     } elseif {$key == "sim-tb-mode"} {
                         if {$value == 0} {
                             set _simbuild False
                         } else {
                             set _simbuild True
                         }                      
                     } elseif {$key == "emu-tb-mode"} {
                         if {$value == 0} {
                             set _emubuild False
                         } else {
                             set _emubuild True
                         }
                     }
                 }
             }
         }
        # puts "exit checkWhat emu $_emubuild sim $_simbuild"
     }

     method updateMenus {} {
         #puts "updatemenus canbuild $_canbuild emubuild $_emubuild simbuild $_simbuild"
         if {!$_canbuild} {
             set _emubuild False
             set _simbuild False
         }

         if {$_emubuild} {
           #  puts "can build emu"
             set_menu_item build 0 normal
             set_menu_item build 1 normal
           #  set_menu_item build 7 normal
         } else {
           #  puts "can't build emu"
             set_menu_item build 0 disabled
             set_menu_item build 1 disabled
            # set_menu_item build 7 disabled
         }

         if {$_simbuild} {
          #   puts "can build sim"
             set_menu_item build 4 normal
             set_menu_item build 5 normal
         } else {
             #puts "can't build sim"
             set_menu_item build 4 disabled
             set_menu_item build 5 disabled
         }

         if {!$_emubuild && !$_simbuild} {
             set_menu_item build 7 disabled
             set _canbuild False
         } else { 
             set_menu_item build 7 normal
             set _canbuild True
             checkCanRun
         }        
     }

     method checkCanRun {} {
         #puts "checkCanRun"
         set canrun ""
         set canrunemu False
         set canrunsim False
         set emubuilt False
         set simbuilt False

         if {!$_emubuild && !$_simbuild} {
             lappend canrun $canrunemu $canrunsim
             return $canrun
         }
         if {[file exists project.bld] && [file exists project.blt]} {
             set projtime [file mtime project.bld]
             set buildtime [file mtime project.blt]
         } else {
	     set projtime 0
	     set buildtime 0
	 }
         if {[file exists  build/emu/scripts/scemi.params]} {
             set emubtime [file mtime build/emu/scripts/scemi.params]
             set emubuilt True
             if {$emubtime > $projtime && $emubtime > $buildtime} { 
                 set canrunemu True
             }
         } 
         if {[file exists  build/sim/scripts/scemi.params]} {
             set simbtime [file mtime build/sim/scripts/scemi.params]
             set simbuilt True
             if {$simbtime > $projtime && $simbtime > $buildtime} { 
                 set canrunsim True
             }
         }
         
         set board [semu_viewer::whichBoard]
        #  if {$board == "None"} {
        #     set canrunemu False
        #     set emubuilt False
        # } 


        if {$emubuilt} {
            set_menu_item board 0 normal
            set_menu_item board 1 normal
            set_menu_item board 2 normal   
            
            set_menu_item run 0 normal
           # set_menu_item run 1 normal

        }  else {
            set_menu_item board 0 disabled
            set_menu_item board 1 disabled
            set_menu_item board 2 disabled

            set_menu_item run 0 disabled
          #  set_menu_item run 1 disabled
        }
       
        if {$simbuilt} {
            set_menu_item run 2 normal
        } else {
            set_menu_item run 2 disabled
        }

        if {$emubuilt || $simbuilt} {
            set_menu_item run 4 normal
        } else {
            set_menu_item run 4 disabled
        }

        if {$board == "pdv72kr2"} {
            set_menu_item board 0 disabled
        }

        lappend canrun $canrunemu $canrunsim
        return $canrun
    }

    method updateBoardMenu {} {
    }
    

    private method doConfigure {mode} {
        set _Projfile [file join $_Projdir project.cfg]
        #puts "doConfigure Projfile $_Projfile"
        $itk_component(semu_in) getFile $_Projfile
        $itk_component(semu_in) openInFile 
        $itk_component(semu_in) activate
    }

    method openProject {} {
        if {$_Projname != ""} {
            set answer [tk_messageBox -parent $itk_interior -title "Open Project" -type okcancel \
                        -message "A project is already open. Press OK to close current project and open a different project. "] 
            switch -- $answer {
                cancel {
                    return
                }
                ok {
                   # puts "Yes selected - saveas"
                    itcl::code $this closeProject
                }
            }
        }
        set types [list {{Project files} {.cfg}} ]
        if {[set _Projfile [tk_getOpenFile -title "Semu Project File" -parent $itk_interior \
                                -filetypes $types -initialfile "project.cfg"]] != ""} {
            if {[file tail $_Projfile] != "project.cfg"} {
                tk_messageBox -parent $itk_interior -icon error -title "Error" \
                    -type ok -message "Configuration file must be named project.cfg"
                return
            }
            set _Projdir [file dirname $_Projfile]
            set _Projname [file tail $_Projfile]   
            #puts "projname $_Projname"
            doOpen
        }
    }
    
    method doOpen {} {
        #initVariables
        cd $_Projdir
        set continue [semu_build::readOptions]
        #puts "doOpen dir is [pwd]"
        set name [file tail $_Projdir]
        wm title . "Bluespec semu - $name"
        menuOpen
        # set ctb [$itk_component(semu_build) readtb]
        # add_ctb $ctb
        semu_viewer::display_message "Project $_Projfile opened" false
        checkCanBuild
        # if {$_canbuild} {
        #     checkCanRun
        # }
    }

    method closeProject {} {
        set _Projname ""
        wm title . "Bluespec semu"
        menuInit
        semu_build::resetArrays
        semu_viewer::display_message "Project $_Projfile closed" false
        reset

    }

    method reset {} {
        set _Projfile ""
        set _Projdir ""
        set _Projname ""
    }

    method saveProject {} {
        if {[file exists $_Projfile]} {
        set answer [tk_messageBox -parent $itk_interior -title "Save Project" -type yesnocancel \
                        -message "Project $_Projfile already exists.  Saving will require you to rebuild the project.  Do you want to save under a new name? Press Yes to SaveAs, No to overwrite "] 
            switch -- $answer {
                cancel {
                    return
                }
                yes {
                   # puts "Yes selected - saveas"
                    itcl::code $this saveasProject
                }
            }
        }
    }




    method saveasProject {} {
        $itk_component(proj_dialog) setTitle "Save As" 
        set answer [tk_messageBox -title "Save As" -icon info \
            -message "Save As will save your entire project directory to a new directory.  This action may take a few minutes.  Press OK to continue." \
                        -type okcancel]
        if {$answer == "cancel" } {return}
        set contSave [$itk_component(proj_dialog) activate]
        if { $contSave == 0 } {
            return
        }       
        set pname [$itk_component(proj_dialog) component pname get]
        set newdir  [$itk_component(proj_dialog) component pdir get]
        set newproj [file join $newdir $pname]
        #puts "saveasProject new directory: $newproj"
        if {[catch [file copy $_Projdir $newproj]]} {
            tk_messageBox -title "Error" -icon error -message "Save As unsuccessful"
        } else {
            tk_messageBox -title "Save As" -icon info -message "Save As complete"
            #puts "saveas newproj $newproj old directory $_Projdir"
            closeProject
            set _Projdir $newproj
            #puts "saveasProject directory $_Projdir"
            doOpen
        }
    }


    method newProject {} {
        if {$_Projname != ""} {
            set answer [tk_messageBox -parent $itk_interior -title "New Project" -type okcancel \
                        -message "A project is already open. Press OK to close current project and open a new project. "] 
            switch -- $answer {
                cancel {
                    return
                }
                ok {
                   # puts "Yes selected - saveas"
                    initVariables
                    itcl::code $this closeProject
                }
            }
        }

        $itk_component(proj_dialog) setTitle "New Project" 
        
        set contNew [$itk_component(proj_dialog) activate]
        if { $contNew == 0 } {
            return
        }
        set _Projname [$itk_component(proj_dialog) component pname get]
        set _Projdir [$itk_component(proj_dialog) component pdir get]
        set _Projfile project.cfg
        #puts "newProject _Projname: $_Projname _Projdir: $_Projdir"
      
        if {$_Projname == [file tail $_Projdir]} {
            
            set answer [tk_messageBox -title "Create Project" -icon question \
                            -message "Directory already exists. \n 
Select yes to create a new subdirectory \n no to use the current directory \n or cancel to exit" \
                        -type yesnocancel]

            switch -- $answer {
                yes {mkNewDir}
                no {
                    cd $_Projdir
                    if {[file exists project.cfg]}  {
                        set a [tk_messageBox -title "Error" -icon error \
                                   -message "Project already exists" -type ok]
                        return
                    }
                }
                cancel {return}
            }
        } else {
            mkNewDir
        }
        newProjectInit $_Projname $_Projdir
    }

    method mkNewDir {} {
        #puts "switch yes"
        set _Projdir [file join $_Projdir $_Projname]
        if {[catch [file mkdir $_Projdir]]} {
            tk_messageBox -title "Error" -icon error -message "Can not create the directory $_Projdir "
            return
        } else {
            cd $_Projdir
            #puts "New directory $_Projdir"
        }
    }

    method initVariables {} {
        if {[array exists semu_config::_Initvals]} {
            array unset semu_config::_Initvals 
        }
        if {[array exists semu_config::_Oldvals]} {
             array unset semu_config::_Oldvals 
        }
        if {[array exists semu_advanced::_Initvals]} {
            array unset semu_advanced::_Initvals 
        }
        if {[array exists semu_build::_Answer]} {
            array unset semu_build::_Answer 
        }
        if {[array exists semu_build::_Build]} {
            array unset semu_build::_Build 
        }
        if {[array exists semu_build::_Projvals]} {
             array unset  semu_build::_Projvals
        }
    }

    method newProjectInit {n l} {
       # puts "newProjectInit file: $n directory $l"
        $itk_component(semu_in) newFile $n $l
        $itk_component(semu_in) activate
#        set name [file tail $_Projdir]
        wm title . "Bluespec semu - $n"
        menuOpen
    }       

    method buildProject  { } {
        set pfile [file join $_Projdir project.cfg]
        set continue [semu_build::readOptions]
        if {$continue == "false" } {
            return
        }
        set continue [semu_build::readBuild true]
	#  if {$continue == "false" } {
        #      return
        # }
        $itk_component(semu_build) fill_ctb
        $itk_component(semu_build) fill_stb
        $itk_component(semu_build) activate
        #set ctb [$itk_component(semu_build) readtb]
        #add_ctb $ctb
        checkCanBuild
    }

    method editPin {} {
        set needread [$itk_component(semu_in) checkStatus]
        if {$needread} {
            $itk_component(semu_in) getFile $_Projfile
            $itk_component(semu_in) openInFile 
        }
        $itk_component(semu_in) editPin      
    }

   method changeSpeed {} {
       set needread [$itk_component(semu_in) checkStatus]
       if {$needread} {
           $itk_component(semu_in) getFile $_Projfile
           $itk_component(semu_in) openInFile 
       }
       set oldspeed [$itk_component(semu_in) configSpeed]
       $itk_component(askSpeed) sendSpeed $oldspeed
       $itk_component(askSpeed) activate
       set newspeed [$itk_component(askSpeed) getSpeed]
  
         
    }

    proc bitInfo {} {
        puts "Bit File Info"
        if {![file exists build/emu/xilinx/fpga.bit]} {
            puts "  No current bit file"
        } else {
            puts "  Target board: [semu_build::getBitInfo]"
            set time [file mtime build/emu/xilinx/fpga.bit]
            set goodtime [clock format $time -format "%D %r"]
            puts "  Created on:   $goodtime"
        }
    }

    proc boardInfo {} {
        whichBoard
        semu_viewer::execute_in_shell_wait "bluenoc" \
            [itcl::code execDone ""]
     
    }

    proc whichBoard {} {
        if {[exec bluenoc] == "" } {
            return "None"
        }
        set board [exec bluenoc board]
#        puts "board $board"
        if {$board == "" || [regexp {^No BlueNoC targets} $board]} {
#            puts "board isn't connected"
            return "None"
        } else {
            return $board
        }
    }

   proc execDone {msg} {
       semu_viewer::display_message $msg false
       return
   }


}

catch "itcl::delete class askSpeed"
itcl::class askSpeed {
    inherit iwidgets::Dialog
    variable nspeed ""

    constructor {args} {
        lappend args -modality application
        
        eval itk_initialize $args
        wm geometry $itk_component(hull) 350x130+200+200
        wm title $itk_component(hull) "Change Speed"

        itk_component add sframe {
            frame $itk_interior.sframe
        } {}

        itk_component add nspeed {
            iwidgets::entryfield $itk_component(sframe).nspeed -labeltext "New Emulation Speed" 
        } {}

        # itk_component add nspeed {
        #     entry $itk_component(sframe).nspeed -validate focus \
        #         -vcmd "[itcl::code $this checkSpeed %P]"
        # } {}

        itk_component add units {
            ttk::label $itk_component(sframe).units -text "Mhz"
        } {}
        
        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help
        buttonconfigure OK -command [itcl::code $this doFinish] 
        
        pack $itk_component(nspeed) -side left -padx 5 -fill x -expand true
        pack $itk_component(units) -side top -fill x -expand true
        pack $itk_component(sframe) -fill x -expand true
    }
    
    proc create {pathname args} {
        uplevel #0 askSpeed $pathname -modality application $args
    }
    
    method doFinish {} {
       if {[checkSpeed]} {
           set speed [expr 2 * $nspeed]
            semu_viewer::execute_in_shell_wait "bluenoc clock $speed" \
                [itcl::code execDone "Speed reset done"]
            semu_viewer::execute_in_shell_wait "bluenoc reset" \
                [itcl::code execDone "Board reset done"]
            [itcl::code $this deactivate 1]
        } else {
            return
        }
    }
    
        proc execDone {msg} {
            semu_viewer::display_message $msg false
return
        }

    method checkSpeed {} {
        set nspeed [$itk_component(nspeed) get]
        if {$nspeed < 5 || $nspeed > 500} {
            tk_messageBox -title "Invalid speed" -type ok \
                -icon error -message "Frequency must be between 5 and 500 Mhz"\
                -parent $itk_interior
            
            return False
        } else {
            return True
        }
    }
                       
    method sendSpeed {cspeed} {
        $itk_component(nspeed) clear
        $itk_component(nspeed) insert 0 $cspeed
    }
    
    method getSpeed {} {
        set newspeed [$itk_component(nspeed) get]
        return $newspeed
    }
    
}




    


