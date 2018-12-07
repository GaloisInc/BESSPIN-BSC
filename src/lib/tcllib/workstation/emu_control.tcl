package require BSDebug
package require Iwidgets 4.0
package require Itcl
package require Instrument 1.0

package provide emu_control 1.0

catch "itcl::delete class emu_control"

itcl::class emu_control {
    inherit itk::Widget

    variable viewHierVal 1
    variable viewCollVal 1
    variable _mode
    variable _no_dbg
    variable _lfile "proj.lyt"
    variable _tb
    variable _viewer ""
    variable viewDutVal 0
    variable viewMsgVal 1
    variable dutpane false
    variable _Layout
    variable _dispviews 0
    variable _Saved 


    constructor {top mode dut no_dbg args} {
       # puts "in constructor"
        set pane 0
        if {$top == ""} {
	    ## empty string works fine.
#            set top "/mkBridge"
        } 
        set _mode $mode
        set _no_dbg $no_dbg
        set _tb $dut
        array set _Layout [list]
        array set _Saved [list]

        if {$_mode == "emu"} {
            wm title . "Emulation Control Panel"
            wm minsize . 1080 600
        } else {
            wm title . "Simulation Control Panel"
            wm minsize . 1080 600
        }

        uplevel #0 source $::env(BLUESPECDIR)/tcllib/iwidgets4.0/generic/panedwindow.itk
        #puts "add paned"
                
        itk_component add paned {
            base::panedwindow $itk_interior.paned -orient horizontal
        } {}
        pack $itk_component(paned) -fill both -expand true -pady 0 -side bottom 
        #puts "add emu_win"
        itk_component add emu_win {
            emu_viewer::create .emuwin $itk_interior $_mode $no_dbg
        } {}

        #puts "add inst_win"

        itk_component add inst_win {
            instrument_viewer::create .instwin $itk_interior $top $_mode $no_dbg full
        } {}
        
        #puts "add prompt name"
        itk_component add promptName {
            promptName::create .prompt
        } {}
       # puts "prompt name added"

        mkMenuBar 

        #puts "menu bar added"

        $itk_component(paned) add eframe
        set eframe [$itk_component(emu_win) mkEmulationFrame [$itk_component(paned) childsite] $_mode $no_dbg]
        $itk_component(emu_win) setTb $_tb
        pack $eframe -fill x -expand true -pady 0

        #puts "eframe added"

        set dutpane true
        $itk_component(paned) add dut
        incr pane
        set frame [lindex [$itk_component(paned) childsite] $pane]
        set dut  [GuiDut::mkDutControl $frame]
        pack $dut -fill x -expand true -pady 0
        
        if {$_tb == "man"} {
            set viewDutVal 1
        } else {
            set viewDutVal 0
        }


        packMenu $no_dbg

        #puts "packMenu done"
        
        $itk_component(paned) add msg
        incr pane
        set mframe [lindex [$itk_component(paned) childsite] $pane]
        itk_component add mtext {
            status_window $mframe.mtext yes
        } {}

        #puts "ready to pack mtext"

        pack $itk_component(mtext) -fill both -expand yes -pady 0
        
       # puts "adding instsel"

        if {!$no_dbg} {
            $itk_component(paned) add instsel
            incr pane
            set iframe [lindex [$itk_component(paned) childsite] $pane]
            set inst [$itk_component(inst_win) mkPanedWin $iframe "full"]

            $itk_component(paned) add sellist
            incr pane
            set selframe [lindex [$itk_component(paned) childsite] $pane]
            set sel [$itk_component(inst_win) mkSelectionWin $selframe]

            #set sel [$itk_component(inst_win) mkSelectionWin $sigframe]

            $itk_component(inst_win) packInst 
        } else {
            set viewCollVal 0
            set viewHierVal 0
        } 

        #puts "setting dispviews"

        set _dispviews [expr $pane + 1]

        pack $itk_interior -fill both -expand true -pady 0 -side bottom
        
        #puts "going to readDefault"


        if {[file exists $_lfile]} {
            readDefault
            loadFirstLayout
        } else {
            defaultLayout
        }
        
	after 500 $itk_component(emu_win) statusLoop
        
    }
    
    destructor {exit}

     proc execute_in_shell { cmd closeProc} {
         .control component mtext execute_in_shell $cmd $closeProc
     }

     proc display_message {msg {with_jump true}} {
         .control component mtext display_message $msg $with_jump
     }


     method mkMenuBar {} {

         itk_component add mf {
             ttk::frame $itk_interior.mf
         } {}

         set menutop $itk_component(mf)

         itk_component add menubar {
             base::menubar $menutop.mb -helpvariable helpVar -menubuttons {
                 menubutton file -text "Session" -underline 0 -menu {
                      options -tearoff false
                     command closewin -label "Close"  
                     command reset -label "Reset Emulation" -underline 0
                 }
                 menubutton sig -text "Hierarchy" -underline 0 -menu {
                     options -tearoff false
                     command collapse -label "Collapse All" -underline 9
                     command expand -label "Expand All" -underline 0
                     command promote -label "Promote To Top" -underline 0
                     command original -label "Restore Original Hierarchy" -underline 0
                     #separator seps1
                     #command collect -label "Collect" -underline 0
                 }
                 menubutton collection -text "Collection" -underline 0 -menu {
                     options -tearoff false
                     command loadcol -label "Load Collection" -underline 0
                     command savecol -label "Save Collection" -underline 0
                     command saveascol -label "Save Collection As..." -underline 16
                     separator sep2
                     command enable_col -label "Enable All" -underline 0
                     command disable_col -label "Disable All" -underline 0
                    command clear_col -label "Clear" -underline 0
                }
                menubutton wave -text "Wave Viewer" -underline 0 -menu {
                    options -tearoff false
                    command start -label "Start Wave Viewer" -underline 0
                    cascade attach -label "Attach" -underline 0 \
                        -menu {
                            options -tearoff false
                        }
                    command load -label "Load Dump File..." -underline 0
                    command send_all_enabled -label "Send Enabled Signals" -underline 5 
                    command reload_vcd -label "Reload VCD" -underline 0
                    command xhost -label "Allow XServer connections" -underline 6 -state normal 
                }
                menubutton view -text "View" -underline 0 -menu {
                    options -tearoff false
                    checkbutton dut -label "Testbench" -underline 0
                    checkbutton msg -label "Messages" -underline 0
                    checkbutton hier -label "Design Hierarchy" -underline 0 
                    checkbutton coll -label "Signal Collection" -underline 0 
                    separator view1
                    cascade load -label "Load View" -underline 0 \
                        -menu {
                            options -tearoff false
                        }
                    command save -label "Save View" -underline 5
                    command saveas -label "Save View As..." -underline 10
#                    command savedef -label "Set as Default Layout" -underline 7  
                    command orig -label "Reset View" -underline 0
                }
            }
        } {}
        
        # File related buttons
        
        $itk_component(menubar) menuconfigure file.closewin -command exit
        $itk_component(menubar) menuconfigure file.reset    -command "[itcl::code $this doReset]"
        
        # Signals related buttons
        $itk_component(menubar) menuconfigure sig.collapse -command "$itk_component(inst_win) collapseHierarchyAll"
        $itk_component(menubar) menuconfigure sig.expand -command "$itk_component(inst_win) expandHierarchyAll"
        $itk_component(menubar) menuconfigure sig.original -command "$itk_component(inst_win) restoreOriginalHierarchy"
        $itk_component(menubar) menuconfigure sig.promote -command "$itk_component(inst_win) promoteToTop"
        #$itk_component(menubar) menuconfigure sig.collect -command "$itk_component(inst_win) edit_action signal collect"
        
        # Collection related buttons
        $itk_component(menubar) menuconfigure collection.loadcol -command "$itk_component(inst_win) loadCollection"
        $itk_component(menubar) menuconfigure collection.savecol -command "$itk_component(inst_win) saveCollection"
        $itk_component(menubar) menuconfigure collection.saveascol -command "$itk_component(inst_win) saveCollectionAs"
        $itk_component(menubar) menuconfigure collection.clear_col -command "$itk_component(inst_win) clearCollection"
        $itk_component(menubar) menuconfigure collection.enable_col -command "$itk_component(inst_win) enableCollection"
        $itk_component(menubar) menuconfigure collection.disable_col -command "$itk_component(inst_win) disableCollection"
        
        # Wave viewer related buttons
        $itk_component(menubar) menuconfigure wave.start -command "[itcl::code $this startWaveViewer]"
        $itk_component(menubar) menuconfigure wave.load -command "[itcl::code $this viewer_load_dump_file]"
        $itk_component(menubar) menuconfigure wave.send_all_enabled -command "$itk_component(inst_win) sendAllEnabledSignals"
        $itk_component(menubar) menuconfigure wave.reload_vcd -command "[itcl::code $this reloadVCD]"
        $itk_component(menubar) menuconfigure wave.xhost -command Waves::open_xhost
        
        $itk_component(menubar).menubar.wave.menu configure -postcommand "[itcl::code $this open_wave_menu]"

        $itk_component(menubar) menuconfigure view.dut -command  "[itcl::code $this changeView dut]"
        $itk_component(menubar) menuconfigure view.dut -variable "[itcl::scope viewDutVal]"       
        $itk_component(menubar) menuconfigure view.msg -command  "[itcl::code $this changeView msg]"
        $itk_component(menubar) menuconfigure view.msg -variable "[itcl::scope viewMsgVal]"
        $itk_component(menubar) menuconfigure view.hier -command  "[itcl::code $this changeView hier]"
        $itk_component(menubar) menuconfigure view.hier -variable "[itcl::scope viewHierVal]"
        $itk_component(menubar) menuconfigure view.coll -command  "[itcl::code $this changeView coll]"
        $itk_component(menubar) menuconfigure view.coll -variable "[itcl::scope viewCollVal]"
        $itk_component(menubar) menuconfigure view.save -command  "[itcl::code $this saveLayout]"
#         $itk_component(menubar) menuconfigure view.savedef -command "[itcl::code $this saveDefault]"
        $itk_component(menubar) menuconfigure view.saveas -command  "[itcl::code $this saveAsLayout]"
        $itk_component(menubar).menubar.view.menu configure -postcommand  "[itcl::code $this buildViewMenu]"
        $itk_component(menubar) menuconfigure view.orig -command  "[itcl::code $this defaultLayout]"
        

        pack $itk_component(menubar) -fill x  -side top  -pady 0
    }
    
    method packMenu {no_dbg} {
        set menu $itk_component(menubar).menubar
        if {!$no_dbg} {
            $menu.sig configure -state normal
            $menu.collection configure -state normal
            $menu.view configure -state normal
        } else {
            $menu.sig configure -state disabled
            $menu.collection configure -state disabled
            $menu.view.menu entryconfigure 2 -state disabled
            $menu.view.menu entryconfigure 3 -state disabled
            $menu.wave.menu entryconfigure 3 -state disabled
            set viewCollVal 0
            set viewHierVal 0
        }
        # if {!$dutpane} {
        #     set viewDutVal 0
        #     $menu.view.menu entryconfigure 0 -state disabled
        # }

        pack $itk_component(mf) -fill x -side top -pady 0
    }

    method changeView {win} {
        if {$win == "hier"} {
            $itk_component(inst_win) viewHier $viewHierVal
        } elseif {$win == "coll"} {
            $itk_component(inst_win) viewColl $viewCollVal
        } elseif {$win == "dut"} {
            viewDut $viewDutVal
        } elseif {$win == "msg"} {
            viewMsg $viewMsgVal
         }
     }

    method viewDut {val} {
        if {$val}  {
            $itk_component(paned) show 1
        } else {
            $itk_component(paned) hide 1
        }
    }

    method viewMsg {val} {
        #puts "in viewMsg val $val"
        set pane 2
        if {$val} {
            $itk_component(paned) show $pane
        } else {
            $itk_component(paned) hide $pane
        }
    }

    method saveDefault {} {
        if {![file exists $_lfile]} {
            return
        }
        if {$_Layout(name) == ""} {
            saveAsLayout 
        } elseif {$_Layout(default) == 1} {
            modLayout $_Layout(name) $_Layout(default)
        } else {
            changeDefault $_Layout(name)
        }
    }

    method changeDefault {newDefault} {
        set tmpfile [open "temp.layout" "w+"]
        set layfile [open $_lfile "r"]
        set pln ""
        while {[gets $layfile fline] >= 0} {
            if {[regexp {(.*):(.*)} $fline fullline key value]} {
                if {$key == "name"} {
                    set cname [lindex $value 0]
                    set cdefault [lindex $value 1]
                    if {$cname == $newDefault} {
                        saveCurrent $tmpfile $cname 1
                    } else {
                        saveCopy $fline $tmpfile $layfile $cname 0
                    }
                }
            }
        }
        
        close $layfile
        close $tmpfile
        file copy -force "temp.layout" "proj.lyt"
    }
    
    method saveLayout {} {
        # if {$_Layout(name) == ""} {
        #     saveAsLayout
        # }
     
        if {[file exists $_lfile]} {
            modLayout $_Layout(name) $_Layout(default)
        } else {
            saveAsLayout
        }
       
    }
    
    method saveAsLayout {} {
        $itk_component(promptName) activate
        set _Layout(name) [$itk_component(promptName) getNewName]
       # set values [$itk_component(promptName) getNewName]
       # set _Layout(name) [lindex $values 0]
        #set _Layout(default) [lindex $values 1]
        set _Layout(default) 1
        if {$_Layout(name) == ""} {
            return
        }
        
         if {[file exists $_lfile]} {
             modLayout $_Layout(name) $_Layout(default)
         } else {
             set lfile [open "proj.lyt" "w"]
             saveCurrent $lfile $_Layout(name) 1
             close $lfile
         }
     }

     method modLayout {lname set_default} {
         #puts "in modLayout"
         set modfound 0
         set tmpfile [open "temp.layout" "w+"]
         set layfile [open $_lfile "r"]
         set pln ""
         while {[gets $layfile fline] >= 0} {
             if {[regexp {(.*):(.*)} $fline fullline key value]} {
                 if {$key == "name"} {
                     set fname [lindex $value 0]
                     set read_default [lindex $value 1]

                     if {$fname == $lname} {
                         set modfound 1
                         saveCurrent $tmpfile $lname $set_default
                     } else {
                         if {$set_default == 1} {
                             set defvalue 0
                         } else {
                             set defvalue $read_default
                         }
                         saveCopy $fline $tmpfile $layfile $fname $defvalue
                     }
                 }
             }
         }
         if {!$modfound} {
             saveCurrent $tmpfile $lname $set_default
         } 

         close $layfile
         close $tmpfile
         file copy -force "temp.layout" "proj.lyt"
     }

     method saveCurrent {tmpfile name set_default} {
         set pln ""
         append pln "name:$name $set_default" \n
         append pln "geom:[winfo geometry .]" \n
         append pln "view:$viewDutVal $viewMsgVal $viewHierVal $viewCollVal" \n
         append pln "fract:[$itk_component(paned) fraction]" \n
         puts $tmpfile $pln
     }

     method saveCopy {line tmpfile layfile name set_default} {
         set pln ""
         append pln "name:$name $set_default" \n
         while {$line != "" && $line >= 0} {
             gets $layfile line
             append pln $line \n
         }
         puts $tmpfile $pln
     }


     method loadNewLayout {} {
         #buildViewMenu
         readDefault
         loadLayout
         # set types [list {{Layout files} {.lyt}}  {{All files} {*}}]
         # set f [tk_getOpenFile -title "Load Layout"  -parent "$itk_interior" -filetypes $types -initialfile $_lfile]
         # if { $f == "" } {
         #     tk_messageBox -title "Error" -icon error -message "file name must be given." \
         #         -parent $itk_interior
         #     return
         # }
         # set _lfile $f
         # loadLayout
     }

     method readLayout {sname} {
         #puts "readLayout $sname"
         set lfile [open proj.lyt "r"]
         while {[gets $lfile line] >= 0} {
             regexp {(.*):(.*)} $line fulline key value
             if {$key == "name"} {
                 set rname [lindex $value 0]
                 if {$rname == $sname} {
                     set _Layout(name) $rname
                     set _Layout(default) [lindex $value 1]
                     foreach newkey {geom view fract} {
                         gets $lfile line
                         regexp {(.*):(.*)} $line fulline key value
                         set _Layout($key) $value
                     }
                 }
             }
         }
         #parray _Layout
         loadLayout 
     }

     method readDefault {} {
         if {[file exists $_lfile] == 0} {
             #puts "loading Default - no lyt"
             defaultLayout
         }
         set lfile [open proj.lyt "r"]
         while {[gets $lfile line] >= 0} {
             regexp {(.*):(.*)} $line fulline key value
             if {$key == "name"} {
                 set is_default [lindex $value 1]
                 if {$is_default} {
                     set _Layout(name) [lindex $value 0]
                     set _Layout(default) 1
                     foreach newkey {geom view fract} {
                         gets $lfile line
                         regexp {(.*):(.*)} $line fulline key value
                         set _Layout($key) $value
                     }
                 }
             }
         }
     }

    method loadFirstLayout {} {
        #puts "in first loadLayout"
        wm geometry . $_Layout(geom)
        setView $_Layout(view)
        set fract [llength $_Layout(fract)]
        
        if {$_no_dbg} {
            if {$fract < 4} {
                eval $itk_component(paned) fraction $_Layout(fract)
            } else {
                defaultLayout
            }
        } elseif {$fract > 3} {
            eval $itk_component(paned) fraction $_Layout(fract)
        } else {
            defaultLayout
        }
    }
    

    method loadLayout {} {
        #puts "in loadLayout"
        set fract [llength $_Layout(fract)]
        
        if {$_no_dbg} {
            if {$fract < 4} {
                wm geometry . $_Layout(geom)
                setView $_Layout(view)
                eval $itk_component(paned) fraction $_Layout(fract)
                saveDefault
            } else {
                tk_messageBox -title "Cannot display selected view" -icon info -type ok\
                    -message "Selected view for debug but currently running in no debug mode. " \
                    -parent $itk_interior
            }
        } elseif {$fract > 3} {
            wm geometry . $_Layout(geom)
            setView $_Layout(view)
            eval $itk_component(paned) fraction $_Layout(fract)
            saveDefault
        } else {
            tk_messageBox -title "Cannot display selected view" -icon info -type ok\
                -message "Selected view for no debug but currently running in debug mode." \
                -parent $itk_interior
        }
     }
    

    method setView {value} {
        set viewDutVal [lindex $value 0]
        set viewMsgVal [lindex $value 1]
        set viewHierVal [lindex $value 2]
        set viewCollVal [lindex $value 3]
        if {!$_no_dbg} {
            $itk_component(inst_win) viewColl $viewCollVal
            $itk_component(inst_win) viewHier $viewHierVal
        }
        viewDut $viewDutVal
        viewMsg $viewMsgVal
    }


     method defaultLayout {} {
         #puts "in default Layout"
         set dispviews 2
         if {!$_no_dbg} {
             set viewHierVal 1
             incr dispviews            
             set viewCollVal 1
             incr dispviews
             $itk_component(inst_win) viewColl $viewCollVal
             $itk_component(inst_win) viewHier $viewHierVal
         } else {
             set viewHierVal 0
             set viewCollVal 0
         }
         set viewMsgVal 1
         if {$_tb == "man"} {
             set viewDutVal 1
             incr dispviews
         } else {
             set viewDutVal 0
         }

         viewDut $viewDutVal
         viewMsg $viewMsgVal

         switch $dispviews {
             3 {
                 $itk_component(paned) fraction 25 30 45
             }
             4 {
                 $itk_component(paned) fraction 22 10 33 35
             }
             5 {
                 $itk_component(paned) fraction 22 9 9 34 26
             }
         }
     }    

     method buildViewMenu {} {
         #puts "in buildView"
         set m $itk_component(menubar)
         catch "$m delete .view.load.0 .view.load.end"

         if {[file exists $_lfile]} {
             foreach {l d} [$itk_component(promptName) loadNames] {
                 $m add checkbutton .view.load.$l -label $l -variable [itcl::scope _Saved($l)] \
                     -command [itcl::code $this readLayout $l] 
                 set _Saved($l) $d

             }
         }
     }

    method startWaveViewer {} {
        set _viewer [semu_waves::start_viewer]
    }
    
    method viewer_load_dump_file {} {
        set type {}
        set at {}
        set dir [pwd]
        foreach t [Waves::get_dump_file_extensions] {
            lappend at ".$t"
        }
        lappend type "{All dump files} {$at}"
        foreach t [Waves::get_dump_file_extensions] {
            lappend type "{$t files} {.$t}"
        }
        lappend type "{All files} {*}"

        set d [tk_getOpenFile  -title "Load Dump File" \
               -filetypes $type  -parent $itk_component(hull)]
        if { $d != ""} {
            $_viewer load_dump_file $d
        }
    }

    method reloadVCD {} {
        semu_waves::reload_vcd
    }


    method open_wave_menu {} {
        if {$_viewer != ""} {
            if {[$semu_waves::_Viewer attach] != ""} {
                #  change_menu_status viewer attach normal
                add_attach_submenu
            }
            if {[$semu_waves::_Viewer isRunning]} {
                # change_menu_status viewer {load close} normal
                # change_menu_status viewer start disabled
            }
        }    
    }
     
 
    method add_attach_submenu {} {
        set m $itk_component(menubar)
        catch "$m delete .wave.attach.0 .wave.attach.end"
        foreach v [$semu_waves::_Viewer attach] {
            regsub -all {[ ]+} $v "" win
                if {[$m index .wave.attach.$win] == -1} {
                        if {[$m index .wave.attach.detach] != -1} {
                                $m delete .wave.attach.detach
                        }
                        $m add_command .wave.attach.$win "$v" 0 \
                            "Attaches to the $v viewer" \
                            [itcl::code semu_waves::attach_viewer $v]  "normal"
                }
        }
        $m add_command .wave.attach.detach "Detach" 0 \
            "Detaches the viewer" [itcl::code semu_waves::attach_viewer {}] "normal"
    }

    method doReset {} {
	if {$_mode == "emu"} {
	    $itk_component(inst_win) disableCollection
	}
	$itk_component(emu_win) reset
    }
}

catch "itcl::delete class promptName"        
itcl::class promptName {
    inherit iwidgets::Dialog
    
    variable default_name ""
    variable defcheck 0
    variable _laylist 

    constructor {args} {
        lappend args -modality application
        
        eval itk_initialize $args
        wm title $itk_component(hull) "Select Layout Name"

        array set _laylist [list]
        
        itk_component add nframe {
            frame $itk_interior.nframe
        }
        
        itk_component add nfield {
            iwidgets::combobox $itk_component(nframe).nfield \
                -labeltext "Layout Name" 
        } {}
        
        #puts "before loading names"
        if {[file exists "proj.lyt"] == 1} {
            loadNames
            set names [array get _laylist]

            foreach {n d} $names {
                eval $itk_component(nfield) insert list end $n
            }
        }
         #puts "after loading names"

        # itk_component add dcheck {
        #     checkbutton $itk_component(nframe).dcheck -text "Default?" \
        #         -variable [itcl::scope defcheck] -state normal
        # } {}

        
        $itk_component(hull) hide Apply
        $itk_component(hull) hide Help

        pack $itk_component(nfield)  -padx 10 -pady 10 -fill both
#        pack $itk_component(dcheck) -padx 10 -pady 10 -fill both
        pack $itk_component(nframe)  -padx 10 -pady 10 -fill both
        
    } 
        
    proc create {pathname args} {
        uplevel #0 promptName $pathname -modality application $args
    }

    method loadNames {} {
     #   puts "loadNames"
        set names ""
        set lfile [open proj.lyt "r"]
        while {[gets $lfile line] >= 0} {
            regexp {(.*):(.*)} $line fulline key value
            if {$key == "name"} {
                set name [lindex $value 0]
                set _laylist($name) [lindex $value 1]
            }
        }
        #parray _laylist
        set names [array get _laylist]
        return $names
    }
    
    method getNewName {} {
        set values ""
        set newname [$itk_component(nfield) get]
        return $newname
    }

}
    

