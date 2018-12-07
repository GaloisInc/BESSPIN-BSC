package require BSDebug
package require Iwidgets 4.0
package require Itcl
package require Instlite

package provide emulite_control 1.0



catch "itcl::delete class emulite_control"

itcl::class emulite_control {
    inherit itk::Widget

    variable viewHierVal 1
    variable viewCollVal 1
    variable _mode
    variable _no_dbg
    variable _lfile "lite.lyt"
    variable _tb
    variable _viewer ""
    variable viewDutVal 0
    variable viewMsgVal 1
    variable dutpane false
    variable _Layout
    variable _dispviews 0
    variable _Loaded 
    variable collFile "sig.col"

    constructor {top  dut no_dbg args} {
        #puts "in emulite_control"

        set pane 0
        if {$top == ""} {
	    ## empty string works fine.
#            set top "/mkBridge"
        } 
        set _mode emu
        set _no_dbg ""
        set _tb $dut
        array set _Layout [list]
        array set _Loaded [list]

        wm title . "Lumina"
        wm minsize . 1080 600
        wm protocol . WM_DELETE_WINDOW {exit}

        uplevel #0 source $::env(BLUESPECDIR)/tcllib/iwidgets4.0/generic/panedwindow.itk
                
        #puts "add paned"

        itk_component add paned {
            base::panedwindow $itk_interior.paned -orient horizontal
        } {}
        pack $itk_component(paned) -fill both -expand true -pady 0 -side bottom 
        #puts "paned window created"

        # itk_component add emu_win {
        #     emu_viewer::create .emuwin $itk_interior $_mode $no_dbg
        # } {}

        #puts "add inst_win"

        itk_component add inst_win {
            instlite_viewer::create .instwin $itk_interior $top 
        } {}
        
        #puts "add prompt name"
        itk_component add promptName {
            promptName::create .prompt
        } {}
        #puts "prompt name added"

        # puts "add prompt coll"
        # itk_component add promptColl {
        #     promptColl::create .pcoll "sig.col"
        # } {}
        # puts "prompt name added"

        mkMenuBar 

        #puts "menu bar added"
   
        packMenu $no_dbg

        #puts "adding instsel"

        $itk_component(paned) add instsel
        set iframe [lindex [$itk_component(paned) childsite] $pane]
        set inst [$itk_component(inst_win) mkPanedWin $iframe lite]

        $itk_component(paned) add sellist
        incr pane
        set selframe [lindex [$itk_component(paned) childsite] $pane]
        set sel [$itk_component(inst_win) mkSelectLite $selframe]
        

        # itk_component add bbox {
        #     iwidgets::buttonbox $sel.bbox
        # } {}

        #puts "packing instrument window"
        $itk_component(inst_win) packInst
       
        # pack $itk_component(bbox) -side bottom -fill both -expand true

        # $itk_component(bbox) add read -text "Read State" -underline 0 -command "[itcl::code $this doRead]"


        #puts "setting dispviews"

        pack $itk_interior -fill both -expand true -pady 0 -side bottom
        
        
        #defaultLayout
        #puts "going to read layout"


        if {[file exists $_lfile]} {
            loadLayout
        } else {
            defaultLayout
        }


        
    }
    
    # destructor {}

    destructor {}

    # method exit_main_window {} {
    #     puts "in exit_main_window"
    #     #saveLayout
    #     exit
    # }
    
    method doFinish {} {
        saveLayout
        exit
    }

    method mkMenuBar {} {
        #puts "in mkMenuBar"
        itk_component add mf {
             ttk::frame $itk_interior.mf
        } {}

         set menutop $itk_component(mf)

         itk_component add menubar {
             base::menubar $menutop.mb -helpvariable helpVar -menubuttons {
                 menubutton file -text "Session" -underline 0 -menu {
                     options -tearoff false
                     command closewin -label "Close"  
                 }
            
                 menubutton collection -text "Collection" -underline 0 -menu {
                     options -tearoff false
                     cascade loadcol -label "Load Collection" -underline 0 \
                         -menu {
                             options -tearoff false
                         }
                     command savecol -label "Save Collection" -underline 0
                     command saveascol -label "Save Collection As..." -underline 16
                     command clear -label "Clear Collection" -underline 0
                 }
                 menubutton wave -text "Wave Viewer" -underline 0 -menu {
                     options -tearoff false
                     command start -label "Start Wave Viewer" -underline 0
                     cascade attach -label "Attach" -underline 0 \
                         -menu {
                             options -tearoff false
                         }
                     command load -label "Load Dump File..." -underline 0
                     command send_all_enabled -label "Send Signals" -underline 5 
                     command reload_vcd -label "Reload VCD" -underline 0
                     command xhost -label "Allow XServer connections" -underline 6 -state normal 
                 }
             }
         } {}
        
        # File related buttons
        #puts "menubar options"
        $itk_component(menubar) menuconfigure file.closewin -command "[itcl::code $this doFinish]"
        # Signals related buttons
        # $itk_component(menubar) menuconfigure sig.collapse -command "$itk_component(inst_win) collapseHierarchyAll"
        # $itk_component(menubar) menuconfigure sig.expand -command "$itk_component(inst_win) expandHierarchyAll"
        # $itk_component(menubar) menuconfigure sig.original -command "$itk_component(inst_win) restoreOriginalHierarchy"
        # $itk_component(menubar) menuconfigure sig.promote -command "$itk_component(inst_win) promoteToTop"
        # #$itk_component(menubar) menuconfigure sig.collect -command "$itk_component(inst_win) edit_action signal collect"
        
        # Collection related buttons
        # $itk_component(menubar) menuconfigure collection.loadcol -command "$itk_component(inst_win) loadLiteCollection"
        $itk_component(menubar) menuconfigure collection.savecol -command "$itk_component(inst_win) saveLiteCollection"
      #  $itk_component(menubar) menuconfigure collection.clear -command "$itk_component(inst_win) clearCollection"
        $itk_component(menubar) menuconfigure collection.clear -command "[itcl::code $this clearColl]"
        $itk_component(menubar) menuconfigure collection.saveascol -command "$itk_component(inst_win) saveLiteCollectionAs"
        $itk_component(menubar).menubar.collection.menu configure -postcommand "[itcl::code $this buildCollMenu]"
       

        # Wave viewer related buttons
        $itk_component(menubar) menuconfigure wave.start -command "[itcl::code $this startWaveViewer]"
        $itk_component(menubar) menuconfigure wave.load -command "[itcl::code $this viewer_load_dump_file]"
        $itk_component(menubar) menuconfigure wave.send_all_enabled -command "$itk_component(inst_win) sendAllSignals"
        $itk_component(menubar) menuconfigure wave.reload_vcd -command "[itcl::code $this reloadVCD]"
        $itk_component(menubar) menuconfigure wave.xhost -command Waves::open_xhost
        
        $itk_component(menubar).menubar.wave.menu configure -postcommand "[itcl::code $this open_wave_menu]"
        pack $itk_component(menubar) -fill x  -side top  -pady 0
    }
    
    method packMenu {no_dbg} {
        set menu $itk_component(menubar).menubar
       # $menu.sig configure -state normal
        $menu.collection configure -state normal
       # $menu.view configure -state normal
    
        pack $itk_component(mf) -fill x -side top -pady 0
    }


    method doRead {} {
        #puts "emulite_control calling doRead"
        $itk_component(inst_win) doRead
    }

    
    method saveLayout {} {
        #puts "in savelayout"
        set layfile [open $_lfile "w"]
        set pln ""
        append pln "geom:[winfo geometry .]" \n
        append pln "fract:[$itk_component(paned) fraction]" \n
        #puts "pln $pln"
        puts $layfile $pln
        close $layfile
    }

     method loadLayout {} {
         #puts "loadLayout "
         set lfile [open $_lfile "r"]
         while {[gets $lfile line] >= 0} {
             regexp {(.*):(.*)} $line fulline key value
             if {$key == "geom"} {
                 wm geometry . $value
             } elseif {$key == "fract"} {
                 eval $itk_component(paned) fraction $value
             }                 
         }
     }
    
     method defaultLayout {} {
         #puts "in default Layout"
         $itk_component(paned) fraction 40 60
     }    


    method buildCollMenu {} {
        set m $itk_component(menubar)
        catch "$m delete .collection.loadcol.0 .collection.loadcol.end"

        if {[file exists $collFile]} {
            foreach {l} [promptColl::loadNames $collFile] {
                $m add checkbutton .collection.loadcol.$l -label $l  \
                    -variable [itcl::scope _Loaded($l)] \
                    -command "$itk_component(inst_win) loadLiteCollection $l"
            }
        }
    }

    method clearColl {} {
        foreach {l} [promptColl::loadNames $collFile] {
            set _Loaded($l) 0
        }
        $itk_component(inst_win) clearCollection
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
        $itk_component(inst_win) disableCollection
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
        #puts "newname $newname defcheck $defcheck"
       # lappend values $newname $defcheck
       # return $values
    }

}
    

