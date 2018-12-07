#NOT USED BY SEMU OR SEMULITE ANYMORE
# top-level user script to invoke hdl editing commands
#lappend auto_path $env(BLUESPECDIR)/tcllib/workstation
#lappend auto_path $env(BLUESPECDIR)/tcllib/bwidget
lappend auto_path $env(BLUESPECDIR)/tcllib/tablelist

package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist
package require Redirect


# standard Bluespec colours and fonts
fonts::set_colours
fonts::initialize

#catch "itcl::delete class instrument_viewer"

itcl::class inst_old {
    inherit itk::Toplevel

    protected variable _lasthier ""
    protected variable _collectionfile "signals.col"
    protected variable _viewer ""
    protected variable _vcdfile "dump1.vcd"
    protected variable _current_top "mkBridge"
    protected variable _original_top "mkBridge"
    variable viewHierVal 1
    variable viewCollVal 1
    variable _Frame


    ## number of break expressions displayed
    common breakExprCount 3
    ## previous break expression button state
    common exprSelPrev 0
    ## currently selected break expression (if any)
    common currentBreakExpr ""
    ## current break expression button state
    variable breakpt
    for {set num 0} {$num < $breakExprCount} {incr num} {
	variable expr_$num
    }
  
    common bkfields [list \
                            bkA \
                            bkB \
                            bkC \
                            bkD \
                            ]

    common checkimg   [::fonts::get_image checked]
    common uncheckimg [::fonts::get_image unchecked]
    common deleteimg  [::fonts::get_image delete]
    common waveimg    [::fonts::get_image gtkwave_24x24]

    constructor {frame top args} {
        set frame $itk_interior
        set _Frame $itk_interior
        wm title $itk_interior "Signal Collection"
        wm minsize $itk_interior 800 600
	set nl [lindex [redirect::netlist lsinst] 0]
	set _original_top [lindex [split $nl " "] 0]
	set _current_top $_original_top
	# if {$args != "" && $args != "{}" } {
	#     set _current_top $args
	# }
        if {$top != ""} {
            set _current_top $top
        }

        #semu_waves::create_viewer
       
        puts "in constructor"
        mkMenuBar $frame
        packMenu emu
        mkInstrumentWin $frame
        packInst
        #mkMessageWin $frame
    }


    destructor {
        status_window::stop
       # destroy $_Frame
    }

    # proc create { args} {
    #     uplevel #0 instrument_viewer $args
    # }

    # proc mkInstrument {frame} {
    #     set mb [::instrument_viewer::create .signals $frame]
    #     return $mb
    # } 

   proc create {frame top args} {
        uplevel #0 inst_old $frame $top $args
    }
    
    method mkInstrumentWin {frame} {
        if {$frame == "none"} {
            set frame $itk_interior
        }

        # itk_component add iwf {
        #     ttk::frame $top.iwf 
        # } {}


        # itk_component add iwf {
        #     ttk::frame $win.iwf
        # } {}

        # itk_component add ipw {
        #     base::panedwindow $itk_component(iwf).ipw -orient horizontal 
        # }

       # $pane add ipw
       # puts "inst pane is $pane"
       # set pane [$pane childsite]

        puts "frame is $frame"
        itk_component add ipw {
            base::panedwindow $frame.ipw -orient horizontal 
        }

#        mkMenuBar
        mkPanedWin 
        mkSelectionWin 
        mkBreakExprs
       
       return $itk_component(ipw)

    }

    # method mkInstrumentWin {win} {
    #   itk_component add ipw {
    #         base::panedwindow $itk_interior.ipw -width 850p -height 600p \
    #             -orient horizontal
    #     }
    # }
 
    method packInst {} {
        pack $itk_component(pw)  -fill both -expand true -pady 0 -side bottom
        pack $itk_component(sw) -fill both -expand true -pady 0  -side bottom
        pack $itk_component(ipw)  -fill both -expand true -pady 0 -side bottom
#        pack $itk_component(iwf)  -fill both -expand true -pady 0 -side bottom
    }


    method viewHier {} {
        if {$viewHierVal} {
            $itk_component(ipw) show 0
        } else {
            $itk_component(ipw) hide 0
        }
    }

    method viewColl {} {
        if {$viewCollVal} {
            $itk_component(ipw) show 1
        } else {
            $itk_component(ipw) hide 1
        }
    }

    method showHier {} {
        $itk_component(ipw) show 0
    }

    method mkMessageWin {top} {
        itk_component add mframe {
            base::messagebox $top.mframe -labeltext "Messages" -labelpos nw
        } {}

        return $itk_component(mframe)
    }

    method packMessage {} {
        pack $itk_component(mframe) -fill both -expand true -pady 0 -side bottom
    }

    method mkMenuBar {top} {
        puts "top is $top"
        if {$top == "none"} {
            set top $itk_interior
        }
        
        itk_component add mf {
            ttk::frame $top.mf
        } {}

        set menutop $itk_component(mf)

        itk_component add menubar {
            base::menubar $menutop.mb -helpvariable helpVar -menubuttons {
                menubutton file -text "Session" -underline 0 -menu {
                    options -tearoff false
                    # command loadcol -label "Load Collection" 
                    # command savecol -label "Save Collection" 
                    # command saveascol -label "Save Collection As..." 
                    # separator sep2
                    command closewin -label "Close"  
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
                    checkbutton hier -label "Design Hierarchy" -underline 0 
                    checkbutton coll -label "Signal Collection" -underline 0 
                }
            }
        } {}

	# File related buttons
        #$itk_component(menubar) menuconfigure file.open -command "[itcl::code $this loadFile]"
        # $itk_component(menubar) menuconfigure file.loadcol -command "[itcl::code $this loadCollection]"
        # $itk_component(menubar) menuconfigure file.savecol -command "[itcl::code $this saveCollection]"
        # $itk_component(menubar) menuconfigure file.saveascol -command "[itcl::code $this saveCollectionAs]"
        #$itk_component(menubar) menuconfigure file.closewin -command "[itcl::code $this closeWindow]"
        $itk_component(menubar) menuconfigure file.closewin -command exit
        
	# Signals related buttons
        $itk_component(menubar) menuconfigure sig.collapse -command "[itcl::code $this collapseHierarchyAll]"
        $itk_component(menubar) menuconfigure sig.expand -command "[itcl::code $this expandHierarchyAll]"
        $itk_component(menubar) menuconfigure sig.original -command "[itcl::code $this restoreOriginalHierarchy]"
        $itk_component(menubar) menuconfigure sig.promote -command "[itcl::code $this promoteToTop]"
        #$itk_component(menubar) menuconfigure sig.collect -command "[itcl::code $this edit_action signal collect]"

	# Collection related buttons
        $itk_component(menubar) menuconfigure collection.loadcol -command "[itcl::code $this loadCollection]"
        $itk_component(menubar) menuconfigure collection.savecol -command "[itcl::code $this saveCollection]"
        $itk_component(menubar) menuconfigure collection.saveascol -command "[itcl::code $this saveCollectionAs]"
        $itk_component(menubar) menuconfigure collection.clear_col -command "[itcl::code $this clearCollection]"
        $itk_component(menubar) menuconfigure collection.enable_col -command "[itcl::code $this enableCollection]"
        $itk_component(menubar) menuconfigure collection.disable_col -command "[itcl::code $this disableCollection]"

	# Wave viewer related buttons
        $itk_component(menubar) menuconfigure wave.start -command "[itcl::code $this startWaveViewer]"
        $itk_component(menubar) menuconfigure wave.load -command "[itcl::code $this viewer_load_dump_file]"
        $itk_component(menubar) menuconfigure wave.send_all_enabled -command "[itcl::code $this sendAllEnabledSignals]"
        $itk_component(menubar) menuconfigure wave.reload_vcd -command "[itcl::code $this reloadVCD]"
        $itk_component(menubar) menuconfigure wave.xhost -command Waves::open_xhost

        $itk_component(menubar).menubar.wave.menu configure -postcommand "[itcl::code $this open_wave_menu]"
        $itk_component(menubar) menuconfigure view.hier -command "[itcl::code $this viewHier]"
        $itk_component(menubar) menuconfigure view.hier -variable "[itcl::scope viewHierVal]"
        $itk_component(menubar) menuconfigure view.coll -command "[itcl::code $this viewColl]"
        $itk_component(menubar) menuconfigure view.coll -variable "[itcl::scope viewCollVal]"        

        pack $itk_component(menubar) -fill x  -side top  
    }
    
    method packMenu {mode} {
        set menu $itk_component(menubar).menubar
        if {$mode == "emu"} {
            $menu.sig configure -state normal
            $menu.collection configure -state normal
            $menu.view configure -state normal
        } else {
            $menu.sig configure -state disabled
            $menu.collection configure -state disabled
            $menu.view configure -state disabled
        }
             
        pack $itk_component(mf) -fill x -side top
    }

    method open_wave_menu {} {
        if {[$semu_waves::_Viewer attach] != ""} {
          #  change_menu_status viewer attach normal
            add_attach_submenu
        }
        if {[$semu_waves::_Viewer isRunning]} {
           # change_menu_status viewer {load close} normal
           # change_menu_status viewer start disabled
        }
    }    
     
   # if {[$_Viewer dump_file_loaded]} {
        #     change_menu_status viewer reload normal
        #     if {[Bluetcl::module list] != ""} {
        #         change_menu_status viewer replay normal
        #     }
        # }
   

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

    # method attach_viewer {viewer} {
    #     $_Viewer attach $viewer
    #     #update_menus
    # }


    method mkPanedWin {} {
        $itk_component(ipw) add pw
        itk_component add pw {
            base::panedwindow [$itk_component(ipw) childsite].pw  -orient vertical 
        }
        
        ##  Hierarchy pane #######################################################
        $itk_component(pw) add hier
        set cs [$itk_component(pw) childsite hier]

        set texttags [list {bsv {-foreground #100842}} \
                          {rule {-foreground blue}} \
                          {synth {-foreground #100842}} \
                          {hilite {-foreground white -background #100842}} \
                          {lolite {-foreground black -background white}} \
                     ]

        itk_component add hier {
            base::hierarchy $cs.h \
                -labeltext "Design Hierarchy" -labelpos nw \
                -querycommand "$itk_interior lsinst %n" -visibleitems 5x5 \
                -selectcommand "$itk_interior select_node %n %s" -alwaysquery false \
                -texttags $texttags -filter 0
        } { }

        configure_hierviewer hier
        pack $itk_component(hier) -fill both -expand 1
        #####   Signal pane ##########################################
        $itk_component(pw) add signal
        set cs [$itk_component(pw) childsite signal]
        itk_component add filter {
            iwidgets::combobox  $cs.filter \
                -labeltext {Filter} -command "$itk_interior filterSignalUpdate" \
                -completion false
        } {}
        itk_component add signal {
            base::hierarchy $cs.h -labeltext "Signals" -labelpos nw \
                -alwaysquery true  -visibleitems 5x5 \
                -querycommand "$itk_interior listSignals %n" \
                -selectcommand "$itk_interior selectSignal {%n} %s" \
                -selectonemorecommand "$itk_interior selectAnotherSignal {%n} %s" \
		-texttags [list {enabled {-foreground green}} {unavailable {-foreground red}} {pruned {-foreground purple}} {net {-foreground gray}} ]
        } {}
        $itk_component(filter) insert list end * WILL_FIRE_*
        pack $itk_component(signal) -fill both -expand 1  -pady 0
        #pack $itk_component(filter) -fill x -expand 0 -pady 0 

        configure_hierviewer signal

      #  pack $itk_component(signal) -fill both -expand 1  -pady 0
        
        $itk_component(pw) fraction 50 50
    }
    
    method mkBreakExprs {} {

        set selectcmd [itcl::code $this doSelExpr]
        #set cs [$itk_component(pw) childsite signal]

        set cs [$itk_component(ipw) childsite spw]

        
        itk_component add bf {
           iwidgets::Labeledframe $cs.bw -labeltext "Enable Breakpoints" -labelpos ne 
        } {}
        set bf [$itk_component(bf) childsite]

	for {set num 0} {$num < $breakExprCount} {incr num} {

	    itk_component add expr$num {
		base::buttonfield $bf.expr$num
	    } {}
	
	    $itk_component(expr$num) setVariables [itcl::scope breakpt] $num $selectcmd [itcl::scope expr_$num]
	    pack $itk_component(expr$num)  -side right -padx 2

	}
        
        itk_component add bkexpr {
            ttk::label $bf.bkexpr -textvariable [itcl::scope currentBreakExpr] -anchor w
        } {}
        puts "bkexpr is $currentBreakExpr"

        # itk_component add bkexpr {
        #     text $bf.bkexpr -height 1 -state disabled
        # } {}

        
        pack $itk_component(bkexpr) -side left
	pack $itk_component(bf) -fill x -expand 0 -pady 0 -side top       


    }

    method doSelExpr {} {

#	puts "PUSH $breakpt"

	set is_on [regsub -all {__ON}  $breakpt "" label]

	if {!$is_on} {
	    regsub -all {__OFF}  $breakpt "" label
	}

	if {$is_on} {

	    set textvar "expr_$label"
	    if {[info exists $textvar]} {
		set is_valid [base::is_valid_expr [set $textvar]]
		if {!$is_valid} {
		    set cp [set $textvar]
		    regsub  -all { } $cp "" cp
		    if {$cp == ""} {
			set msg "Breakpoint expression cannot be empty"
		    } else {
			set msg "ERROR: Use of unsupported syntax in expression '[set $textvar]'"
		    }
                    tk_messageBox -title "Error" -icon error \
                        -message $msg -type ok -parent $itk_interior
                    
		    ## uncheck the box
		    set breakpt $exprSelPrev
		    #error $msg
		}
		if {$is_valid} {
		    set code [base::create_lut_code [set $textvar]]
		    puts "CODE: $code"
		    redirect::netlist addcode $code
		    sendBreakpoints
		    redirect::netlist refreshhw $code
		    for {set button 0} {$button < $breakExprCount} {incr button} {
			if {$label == $button} {
			    $itk_component(expr$button) lockField
			} else {
			    $itk_component(expr$button) unlockField
			}
		    }
		    set currentBreakExpr [set $textvar]
		    refreshEditable true
                   
		}
	    } else {
		## first time
		set $textvar ""
                tk_messageBox -title "Error" -icon error \
                    -message "Breakpoint expression cannot be empty" \
                    -type ok -parent $itk_interior
		set msg "Breakpoint expression is empty!"
		## uncheck the box
		set breakpt $exprSelPrev
		#error $msg
	    }
	} else {
	    for {set button 0} {$button < $breakExprCount} {incr button} {
		$itk_component(expr$button) unlockField
	    }
	    set currentBreakExpr ""
	    ## clear all the breakpoint terms
	    redirect::netlist addcode 0
	    redirect::netlist removeterm 0
	    refreshEditable
	}
	set exprSelPrev $breakpt
    }

            
    method mkSelectionWin {} {
        $itk_component(ipw) add spw
        set scs [$itk_component(ipw) childsite spw]

        itk_component add sw {
                 iwidgets::Labeledframe $scs.sw  -labeltext "Signal Collection"
        } {}

        set csw [$itk_component(sw) childsite]
        
        set vsb $csw.vsb
        set hsb $csw.hsb

            

        itk_component add slist {
            tablelist::tablelist $csw.slist  \
                -columns { 0 "Name"  left \
                               0 "Hierarchy" left \
                               0 "Nickname" left \
                               0 "Enable" center \
                               0 "Add to\nWaves" center \
                               0 "Breakpoint\nBit Mask" left \
                               0 "Breakpoint\nValue" left \
                               0 "A" center \
                               0 "B" center \
                               0 "C" center \
                               0 "D" center \
                               0 "Del" center } \
                -labelcommand tablelist::sortByColumn \
                -sortcommand instrument_viewer::compareAsSet \
                -height 15 -width 100 -stretch all \
                -xscrollcommand [list $hsb set] -yscrollcommand [list $vsb set] \
                -background white -stripebg #e4e8ec \
                -movablecolumns true -movablerows true -resizablecolumns true \
                -editstartcommand [itcl::code $this editStartCmd] \
                -editendcommand [itcl::code $this editEndCmd] \
        } {}

        set slist $itk_component(slist)

        $slist columnconfigure 0 -name name
        $slist columnconfigure 1 -name hierarchy
        $slist columnconfigure 2 -name nickname -editable yes  \
            -sortmode dictionary 
        $slist columnconfigure 3 -name capture -editable yes -editwindow checkbutton \
           -formatcommand "instrument_viewer::emptyStr"
        $slist columnconfigure 4 -name waves 

        $slist columnconfigure 5 -name mask -editable yes \
            -formatcommand "instrument_viewer::formatHex"
        $slist columnconfigure 6 -name value -editable yes  \
            -formatcommand "instrument_viewer::formatHex"
        $slist columnconfigure 7 -name bkA  -editable yes -editwindow checkbutton \
            -formatcommand "instrument_viewer::emptyStr" 
        $slist columnconfigure 8 -name bkB  -editable yes -editwindow checkbutton \
            -formatcommand "instrument_viewer::emptyStr" 
        $slist columnconfigure 9 -name bkC  -editable yes -editwindow checkbutton \
            -formatcommand "instrument_viewer::emptyStr" 
        $slist columnconfigure 10 -name bkD  -editable yes -editwindow checkbutton \
            -formatcommand "instrument_viewer::emptyStr" 
        $slist columnconfigure 11 -name del  -editable no 

        set bodyTag [$slist bodytag]
        bind $bodyTag <Double-1> [itcl::code $this selectCmd]

        scrollbar $vsb -orient vertical -command [list $csw.slist yview]
        scrollbar $hsb -orient horizontal -command [list $csw.slist xview]
      
        grid $slist  -row 0 -rowspan 2 -column 0 -sticky news
        
        grid $vsb -row 1 -column 1 -sticky ns
        grid $hsb -row 2 -column 0 -sticky ew
        grid rowconfigure $csw 1 -weight 1
        grid columnconfigure $csw 0 -weight 1 
        
        $itk_component(ipw) fraction 40 60
    }    

    proc emptyStr val { return "" }
    proc formatHex val {
        if {$val == ""} {
            set val 0
        }
        format "0x%08x" $val 
    }
    # proc formatHexOne val {
    #     if {$val == ""} {
    #         set val -1
    #     }
    #     format "0x%08x" $val 
    # }

    proc backslash_dollars {name} {
	regsub -all {\$} $name "\\$" new
	return $new
    }

    method createButton {tbl row col w} {
	set net [$tbl getcells $row,hierarchy]
	set net [backslash_dollars $net]
        button $w -text "W"  -image $waveimg -command "$itk_interior sendSignalToViewer $net" ; #-state disabled
    }

    method createDelButton {tbl row col w} {
        set ind [$tbl getfullkeys $row]
        button $w  -text "D" -image $deleteimg  -command [itcl::code $this signalDelete $ind]
    }

    method expandHierarchyAll {} {
        $itk_component(hier) configure -expanded true
        set cmd [$itk_component(hier) cget -querycommand]
        $itk_component(hier) configure -querycommand $cmd
    }
    
    method collapseHierarchyAll {} {
        $itk_component(hier) configure -expanded false
        set cmd [$itk_component(hier) cget -querycommand]
        $itk_component(hier) configure -querycommand $cmd
    }
    
    method promoteToTop {} {
	#puts "Promote To Top $_lasthier"
	set _current_top $_lasthier
        $itk_component(hier) configure -expanded false
        set cmd [$itk_component(hier) cget -querycommand]
        $itk_component(hier) configure -querycommand $cmd
    }
    
    method restoreOriginalHierarchy {} {
	set _current_top $_original_top
        $itk_component(hier) configure -expanded true
        set cmd [$itk_component(hier) cget -querycommand]
        $itk_component(hier) configure -querycommand $cmd
    }
     
    method isSubPathOf {hierarchy path} {
	
	set len [expr [llength $path] - 1]
	set subpath [string range $hierarchy 0 $len] 
	if {$path == $subpath} {
	    return true
	} else {
	    return false
	}
    }
    method getBkFields {} { return $bkfields }

    method selectCmd { } {
        $itk_component(hier) selection clear
        array set path [list]
        set curRow [$itk_component(slist) curselection]
        set hierRow [$itk_component(slist) getcells $curRow,hierarchy]
        set hier [split $hierRow /] 
        set nodecount [llength $hier]
	if {[lindex $hier 0] == ""} { # path has a leading /
	    set first [lindex $hier 1]
	    set hier [lrange $hier 1 end]
	    set hier [lreplace $hier 0 0 "/$first"]
	    set nodecount [expr $nodecount-1]
	}
        set path(0) [lindex $hier 0]
	#puts "PATH path(0): $path(0) hier: $hier"
	if {[isSubPathOf $_current_top $path(0)]} {
	    $itk_component(hier) expand $path(0)
	}
        for {set n 1} {$n < [expr $nodecount-1]} {incr n} {
            set path($n) [join $path([expr $n-1])/[lindex $hier $n]]
	    if {[isSubPathOf $_current_top $path($n)]} {
		$itk_component(hier) expand $path($n)
	    } else {
		continue
	    }
        }
        $itk_component(hier) selection add $path([expr $n-1])
        $itk_component(signal) selection add $hier 
        set _lasthier $path([expr $n-1])
	#puts "lasthier1 $_lasthier"
        
        refreshSignalList        
    }

    method updateSelectionGivenCollectionSignal {curRow} {
        $itk_component(hier) selection clear
        array set path [list]
        set hierRow [$itk_component(slist) getcells $curRow,hierarchy]
        set hier [split $hierRow /] 
        set nodecount [llength $hier]
	if {[lindex $hier 0] == ""} { # path has a leading /
	    set first [lindex $hier 1]
	    set hier [lrange $hier 1 end]
	    set hier [lreplace $hier 0 0 "/$first"]
	    set nodecount [expr $nodecount-1]
	}
        set path(0) [lindex $hier 0]
	if {[isSubPathOf $_current_top $path(0)]} {
	    $itk_component(hier) expand $path(0)
	}

        for {set n 1} {$n < [expr $nodecount-1]} {incr n} {
            set path($n) [join $path([expr $n-1])/[lindex $hier $n]]
	    if {[isSubPathOf $_current_top $path($n)]} {
		$itk_component(hier) expand $path($n)
	    } else {
		continue
	    }
        }
        $itk_component(hier) selection add $path([expr $n-1])
        $itk_component(signal) selection add $hier 
        set _lasthier $path([expr $n-1])
	#puts "lasthier2 $_lasthier"
        
#        refreshSignalList        
    }



    method editEndCmd {tbl row col text} {
        #puts "in editEndcmd $tbl $row $col $text"
	switch [$tbl columncget $col -name] {
	    # capture {
            #     puts "end switch is capture  "
	    #     set ind [$tbl getfullkeys $row]
	    #     set nets [$tbl getcells $ind,hierarchy]
	    #     if {$text} {
	    #         #puts "goto action_enable $nets"
	    #         instrument_viewer::action_enable $nets
	    #         $tbl cellconfigure $row,$col -image $checkimg
	    #     } else {
	    #         #puts "goto action_disable $nets"
	    #         instrument_viewer::action_disable $nets
	    #         $tbl cellconfigure $row,$col -image $uncheckimg
	    #     }
	    #     return $text
	    # } 
#	    breakpoint {
	        # puts "end breakpoint text: $text"
	        # set ind [$tbl getfullkeys $row]
	        # if {$text} {
	        #     puts "endEditCmd breakpoint $row $text"
	        #     #instrument_viewer::action_setbreakpoint $row
	        #     $tbl cellconfigure $row,$col -image $checkimg
	        # } else {
	        #     puts "endEditCmd breakpoint $row $text"
	        #     #instrument_viewer::action_unsetbreakpoint $row
	        #     $tbl cellconfigure $row,$col -image $uncheckimg
	        # }
	        # return $text
#	    } 
	    nickname {
		return $text
	    }
            mask {
                return $text
            }
            value {
                return $text
            }
        }
    }
    
    method editStartCmd {tbl row col text} {
	puts "in editStartcmd $tbl $row $col $text"
        set s [$tbl columncget $col -name]
        puts "switch is $s"
	switch -glob [$tbl columncget $col -name] {
            capture {
                puts "switch is capture"
		set ind [$tbl getfullkeys $row]
		set nets [$tbl getcells $ind,hierarchy]
		if {$text} {
		    instrument_viewer::action_disable $nets
		} else {
		    instrument_viewer::action_enable $nets
		}
		updateSelectionGivenCollectionSignal $row
		$tbl cancelediting
		return $text
	    }
            bk? {
                puts "switch is bk*"
                if {$text} {
                    instrument_viewer::action_unsetbreakpoint $row $col 
                } else {
                    instrument_viewer::action_setbreakpoint $row $col
                }
                $tbl cancelediting
                return $text
            }
        }
    }
    
    

     method configure_hierviewer { name }  {
         set h $itk_component($name)

         if { $name == "hier" } {
             $h component itemMenu add command -label "Expand" \
                 -command "$itk_interior action_on_node hier expand"
             $h component itemMenu add command -label "Collapse" \
                 -command "$itk_interior action_on_node hier collapse"
             $h component itemMenu add command -label "Promote To Top" \
                 -command "$itk_interior promoteToTop"
             $h component itemMenu add command -label "Collapse All" \
                 -command "$itk_interior collapseHierarchyAll"
             $h component itemMenu add separator

             ### Back ground menu #####################################
             $h component bgMenu add command -label "Expand All" \
                 -command "$itk_interior expandHierarchyAll"
             $h component bgMenu add command -label "Collapse All" \
                 -command "$itk_interior collapseHierarchyAll"
         }

          if { $name == "signal" } {
             $h component itemMenu add command -label "Add and Enable" \
                 -command "$itk_interior edit_action signal addand"
             $h component itemMenu add command -label "Add" \
                 -command "$itk_interior edit_action signal add"
             $h component itemMenu add command -label "Enable" \
                  -command "$itk_interior edit_action signal enable"
             $h component itemMenu add command -label "Disable" \
                 -command "$itk_interior action_menu_disable signal disable"
             $h component itemMenu add separator

             $h component itemMenu add command -label "Clear Selections" \
                 -command "$h selection clear"
         }
     }


    # Signal in signal list --  list of children signals
    method listSignals {node} {
        set res [list]
        if {$node == ""} {
            set res [redirect::netlist lsnet $_lasthier]
            set f [$itk_component(filter) get]
            set res [lsearch -not -glob -nocase -inline -all -index 2 $res  unavailable]
            #puts "res is $res"
            if { $f != "" } {
                set res [lsearch -glob -nocase -inline -all -index 1 $res $f]
                #puts "if res is $res"
            }
        }
        return $res
    }

    # Signal in signal list
    method selectSignal {uid status} {
	#puts "selectSignal $uid $status"
        set win $itk_component(signal)
        $win selection clear
        if { $status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
         }
    }
    method selectAnotherSignal {uid status} {
        #puts "selectAnother $uid $status"
        set win $itk_component(signal)
        if { $status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
        }
    }

    method action_on_node {comp action} {
        set c [lindex [$itk_component($comp) current] 0]
        $itk_component($comp) $action $c
    }

    # Node in hierarchy
    method select_node {uid status } {
        set win $itk_component(hier)
        set _lasthier $uid
        $win selection clear
        if {$status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
            refreshSignalList
            #testSignalList
      }
    }

    # reload the signal list either via a new hier node or filter.
    method refreshSignalList {} {
        $itk_component(signal) configure -querycommand [$itk_component(signal) cget -querycommand]
	refreshEnabled
	refreshEditable
    }

    # reload the signal list either via a new hier node or filter.
    method testSignalList {} {
        set res [redirect::netlist lsinst $_lasthier]
        set res [lsearch -not -glob -nocase -inline -all -index 2 $res "unavailable"]
    }

    # reload the hierarchy  list
    #not currently called
    method refreshHierList {} {
        $itk_component(hier) configure -querycommand [$itk_component(hier) cget -querycommand]

    }

    method filterSignalUpdate {} {
        set pat [string trim [$itk_component(filter) get]]
        $itk_component(filter) clear entry
        $itk_component(filter) insert entry 0 $pat
        if { [lsearch -exact [$itk_component(filter) component list get 0 end] $pat] == -1 } {
            $itk_component(filter) insert list 0 $pat
        }
        refreshSignalList
    }

    method selection_clear {} {
        set win $itk_component(hier)
        $win selection clear
    }

    method lsinst {node} {
	
	if {$node == ""} {
	    return [list $_current_top]
	} else {
	    return [redirect::netlist lsinst $node]
	}
    }

    method edit_action {name action} {
        #puts "In edit_action $name $action"
        set nets ""
        if { $name == "signal" } { 
            set nets [$itk_component($name) selection get]
            $itk_component($name) selection clear
        }
        edit_action_do $action $nets
    }

    method edit_action_do { action nets } {
        #puts "edit_action_do $action $nets"
        switch -exact $action {
	    "addand"         { action_add $nets 1}
	    "disable"        { action_disable $nets }
            "add"            { action_add $nets 0}
            "enable"         { action_enable $nets  }
            default   {error "Unknown action -- $action"}
        }
    }

    method refreshEnabled {} {

	set size [$itk_component(slist) size]
#	puts "SIZE $size"
        for {set row 0} {$row < $size} {incr row} {
	    set net [$itk_component(slist) getcells $row,hierarchy]
#	    puts "$row NET $net"
	    if {[check_enabled $net]} {
#		puts "ENABLED"
		$itk_component(slist) cellconfigure $row,capture -image $checkimg
		$itk_component(slist) cellconfigure $row,capture -text 1
	    } else {
#		puts "NOT ENABLED"
		$itk_component(slist) cellconfigure $row,capture -image $uncheckimg
		$itk_component(slist) cellconfigure $row,capture -text 0
	    }
	}
    }

    method refreshEditable {{do_warn false}} {
	enableAllEdits
	foreach {var} [list A B C D] {
	    if {[regsub -all $var $currentBreakExpr "" ignore]} {
		disableEditsFor $var $do_warn
	    }
	}
    }

    method disableEditsFor {var do_warn} {
	
	set bk bk$var

	## light blue
	set color_even #add8e6
	## sky blue
	set color_odd  #87ceeb
	## royal blue
#	set color_odd  #4169e1 

	set size [$itk_component(slist) size]
        $itk_component(slist) cellselection clear anchor end
	set used false;
        for {set row 0} {$row < $size} {incr row} {
	    set color $color_even
	    if {[expr $row % 2]} {
		set color $color_odd
	    }
	    $itk_component(slist) cellconfigure $row,$bk -editable 0 -bg $color
	    if {[$itk_component(slist) getcells $row,$bk] == 1} {
		set used true
		$itk_component(slist) cellconfigure $row,mask -editable 0 -bg $color
		$itk_component(slist) cellconfigure $row,value -editable 0 -bg $color
		$itk_component(slist) cellconfigure $row,del -bg $color
	    }
	}

	if {!$used && $do_warn} {
            set ignore [tk_messageBox -parent $itk_component(hier) -icon warning -title "Unused Variable" -type ok -message "The breakpoint variable '$var' includes no condition terms and thus will always be asserted true."]
	}
    }

    method enableAllEdits {} {
        set size [$itk_component(slist) size] 
        for {set row 0} {$row < $size} {incr row} {
            foreach {bk} [getBkFields] {
		$itk_component(slist) cellconfigure $row,$bk -editable 1 -bg ""

            }
	    $itk_component(slist) cellconfigure $row,mask -editable 1 -bg ""
	    $itk_component(slist) cellconfigure $row,value -editable 1 -bg ""
	    $itk_component(slist) cellconfigure $row,del  -bg ""

        }
    }
        
    
    
    method enableEdit {} {
        $itk_component(slist) columnconfigure mask -editable yes
        $itk_component(slist) columnconfigure value -editable yes
        foreach {bk} [getBk] {
            $itk_component(slist) columnconfigure $bk -editable yes
        }
    }

   
    method sendBreakpoints {} {

        set size [$itk_component(slist) size] 
        for {set row 0} {$row < $size} {incr row} {
            set col 7
            foreach {bk} [getBkFields] {
                if {[$itk_component(slist) getcells $row,$bk] == 1} {
                    sendBreakpoint $row $col
                }
                incr col
            }
        }
    }

    method sendBreakpoint {bkrow col} {
        set net [$itk_component(slist) getcells $bkrow,hierarchy]

	set value [$itk_component(slist) getcells $bkrow,value]
	set cmd   [$itk_component(slist) columncget value -formatcommand]
	set value [$cmd $value]

	set mask [$itk_component(slist) getcells $bkrow,mask]
	set cmd   [$itk_component(slist) columncget mask -formatcommand]
	set mask [$cmd $mask]

        set track [expr $col - 7]
	# puts "EXPR: $net == $value"
	puts "addterm $net $value $mask $track"
        set term_id [redirect::netlist addterm $net $value $mask $track]
    }

    method disable_editing {} {
    }
     

    method action_setbreakpoint {bkrow col} {
        $itk_component(slist) cellconfigure $bkrow,$col -image $checkimg 
        $itk_component(slist) cellconfigure $bkrow,$col -text 1
    }

    method action_unsetbreakpoint {row col} {
        
        $itk_component(slist) cellconfigure $row,$col -image $uncheckimg 
        $itk_component(slist) cellconfigure $row,$col -text 0
    }

    method action_menu_disable {name action} {
        set nets [$itk_component($name) selection get]
        action_disable $nets
    }

    method action_enable { net }  {
	puts "action enable $net"
	redirect::netlist enable $net
	while {![redirect::netlist isenabled $net]} {}
        refreshSignalList
    }

    method action_disable { net }  {
	puts "action disable $net"
	redirect::netlist disable $net
	while {[redirect::netlist isenabled $net]} {}
        refreshSignalList
    }

    # method disable_items {nets} {
    #     set size [$itk_component(slist) size]
    #     for {set row 0} {$row < $size} {incr row} {
    #         if { [$itk_component(slist) getcells $row,hierarchy] == $nets } {
    #             $itk_component(slist) cellconfigure $row,capture -image $uncheckimg
    #          } 
    #     }
    # }

    method action_add {net en} {
        #puts "action_add nets: $nets"
	add_selection $net 0 ""
	if {$en} {
	    action_enable $net
	}
        refreshSignalList
    }

    method check_available {nets} {
        set res [redirect::netlist lsnet $_lasthier]
        set res [lsearch -glob -nocase -inline -all -index 2 $res "unavailable"]
        if {[lsearch -index 0 $res $nets] == -1 } {
            return true
        } else {
            return false
        }
    }

    method check_enabled {net} {
	# set value [array get enabled_set $net]
	# if {$value == ""} {
	#     return 0
	# } else {
	#     return [lindex $value 1]
	# }
	return  [redirect::netlist isenabled $net]
        # set res [redirect::netlist lsnet $_lasthier]
        # set res [lsearch -glob -nocase -inline -all -index 2 $res "enabled"]
        # if {[lsearch -index 0 $res nets] == -1 } {
        #     return false
        # } else {
        #     return true
        # }
    }

    method add_collection {netdata ignore_breaks} {
        set en 0
        foreach signal $netdata {
	    set net [lindex $signal 0]
            set sname [lindex [split $net /] end]
	    set nick [lindex $signal 1]
            set value [lindex $signal 2]
            set mask [lindex $signal 3]

#            if {[check_duplicate_col $net $value $mask]} {
                if {[check_available $net]} {
                    set rindex [$itk_component(slist) insert end ""]
                    $itk_component(slist) configcells $rindex,hierarchy -text $net
                    $itk_component(slist) configcells $rindex,name -text $sname
                    $itk_component(slist) configcells $rindex,nickname -text $nick
                    $itk_component(slist) cellconfigure $rindex,del -window [itcl::code $this createDelButton]
                    $itk_component(slist) cellconfigure $rindex,waves -window [itcl::code $this createButton]
                    $itk_component(slist) cellconfigure $rindex,waves -image $deleteimg
                    $itk_component(slist) cellconfigure $rindex,capture -text 0
                    $itk_component(slist) cellconfigure $rindex,capture -image $uncheckimg
                    $itk_component(slist) cellconfigure $rindex,del -image $waveimg
                    $itk_component(slist) cellconfigure $rindex,mask -text $mask
                    $itk_component(slist) cellconfigure $rindex,value -text $value
                    set col 4
		    foreach {bk} [getBkFields] {
			set bkvalue [lindex $signal $col]
			if {$ignore_breaks} {set bkvalue 0}
			puts "bk: $bk bkvalue: $bkvalue"
			$itk_component(slist) cellconfigure $rindex,$bk -text $bkvalue
			set img [expr {$bkvalue ? "$checkimg" : "$uncheckimg"}]  
			$itk_component(slist) cellconfigure $rindex,$bk -image $img
			incr col
		    }
		}
#	    }
	}    
        refreshSignalList
    }
    
    method check_duplicate_col { net value mask} {
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
	    #puts "check_duplicate on net $net"
            if {[$itk_component(slist) getcells $row,hierarchy] == $net } {                
                if {[$itk_component(slist) getcells $row,value] == $value } {    
                    if {[$itk_component(slist) getcells $row,mask] == $mask } {  
                        return false
                    }
                }
            }
        } 
        return true
    }

    method check_duplicate { net en nick} {
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
	    #puts "check_duplicate on net $net"
            if { [$itk_component(slist) getcells $row,hierarchy] == $net } {
               return
            }
        } 
        add_selection $net $en $nick
    }


    method add_selection {net en nick} {     
        set amask 0
        set avalue 0
        set sname [lindex [split $net /] end]
        set rindex [$itk_component(slist) insert end ""]
	
	#puts "add selection net: $net en: $en nick: $nick"
        $itk_component(slist) cellconfigure $rindex,del -window [itcl::code $this createDelButton]
        $itk_component(slist) configcells $rindex,name -text $sname
	if { $nick == "" } {
	    $itk_component(slist) configcells $rindex,nickname -text $sname
	} else {
	    $itk_component(slist) configcells $rindex,nickname -text $nick
	}
        $itk_component(slist) configcells $rindex,hierarchy -text $net
        $itk_component(slist) configcells $rindex,capture -text $en
        set img [expr {$en ? "$checkimg" : "$uncheckimg"}]  
        $itk_component(slist) cellconfigure $rindex,capture -image $img
        $itk_component(slist) cellconfigure $rindex,del -image $deleteimg
        $itk_component(slist) cellconfigure $rindex,waves -window [itcl::code $this createButton]
        $itk_component(slist) cellconfigure $rindex,waves -image $waveimg
        $itk_component(slist) cellconfigure $rindex,mask -text $amask
        $itk_component(slist) cellconfigure $rindex,value -text $avalue
        foreach {bk} [getBkFields] {
            $itk_component(slist) cellconfigure $rindex,$bk -image $uncheckimg
            $itk_component(slist) cellconfigure $rindex,$bk -text 0
        }
    }


#        $itk_component(slist) configcells $rindex,del -text 0

    method signalDelete {row} {
	set is_editable [$itk_component(slist) cellcget $row,value -editable]
	if {$is_editable} {
	    set net [$itk_component(slist) getcells $row,hierarchy]
	    instrument_viewer::action_disable $net
	    $itk_component(slist) delete $row
	}
    }


    method doToAllEdits {action} {
	set size [$itk_component(slist) size]

	for {set row 0} {$row < $size} {incr row} {
	    $itk_component($comp) $action $row
	}
    }

    method loadCollection {} {
        set types [list {{Collection files} {.col}}  {{All files} {*}}]
        set f [tk_getOpenFile -title "Load Collection"  -parent "$itk_interior" -filetypes $types -initialfile $_collectionfile]
        if { $f == "" } {
            tk_messageBox -title "Error" -icon error -message "file name must be given." \
                -parent $itk_interior
            return
        }

	set ignore_breaks false
	if {$currentBreakExpr != ""} {
	    set answer [tk_messageBox -parent $itk_component(hier)  -title "Breakpoints in Use" -icon warning -type yesno -message "A breakpoint expression is currrently enabled. Any breakpoint information in the collection file will be ignored. Proceed loading?"]
	    if {$answer == no} { return }
	    set ignore_breaks true
	}

	set colfile [open $f "r"]
	set file_data [read $colfile]
        puts "file_data $file_data"
	add_collection $file_data $ignore_breaks
	catch [close $colfile]

        $itk_component(hier) expand $_current_top

        #refreshHierList        
    }

    method saveCollection {} {

        set size [$itk_component(slist) size]
	if {$size == 0} {
            set answer [tk_messageBox -parent $itk_component(hier) -icon warning -title "Save Collection" -type yesno -message "No signals collected. Do you want to write $_collectionfile with empty collection?"]
	    switch -- $answer {
		no {
		    return
		}
	    }
        } elseif {[file exists $_collectionfile]} {
            set answer [tk_messageBox -parent $itk_component(hier) -title "Save Collection" -type yesno -message "File $_collectionfile already exists. Do you want to overwrite it?"]
	    switch -- $answer {
		no {
		    return
		}
	    }
	}
        doSave
    }

    
    method doSave {} {
	set colfile [open $_collectionfile "w"]
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
            set nick [$itk_component(slist) getcells $row,nickname]
	    set value [$itk_component(slist) getcells $row,value]
	    set mask [$itk_component(slist) getcells $row,mask]
            set col 7
            foreach {bk} [getBkFields] {
                set $bk [$itk_component(slist) getcells $row,$col]
                incr col
            }
#            puts "$net $nick value: $value mask: $mask bkA:$bkA bkB:$bkB bkC:$bkC bkD:$bkD"
            puts $colfile "{$net $nick $value $mask $bkA $bkB $bkC $bkD}"
	}
	catch [close $colfile]
    }
    
    method saveCollectionAs {} {
        set types [list {{Collection files} {.col}}  {{All files} {*}}]
        set f [tk_getSaveFile -title "Save Collection"  -parent "$itk_interior" -filetypes $types -initialfile $_collectionfile]
        if { $f == "" } { return}
        set _collectionfile $f
        doSave
    }
        
    
    method clearCollection {} {

	disableCollection
	$itk_component(slist) delete 0 end
        
    }

    method enableCollection {} {

        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
	    $itk_component(slist) cellconfigure $row,capture -text 1
	    $itk_component(slist) cellconfigure $row,capture -image "$checkimg"
	    action_enable $net
	}
	refreshSignalList
    }

    method disableCollection {} {

        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
	    $itk_component(slist) cellconfigure $row,capture -text 0
	    $itk_component(slist) cellconfigure $row,capture -image "$uncheckimg"
            for {set col 7} {$col < 11} {incr col} {
                if {[$itk_component(slist) getcells $row,$col]} {
                    action_unsetbreakpoint $row $col
                }
            }
            action_disable $net
	}
	refreshSignalList
    }
    
    method startWaveViewer {} {
        semu_waves::start_viewer
	#set size [$itk_component(slist) size]
	#$itk_component(slist) columnconfigure waves -window -state !disabled
    }

    method viewer_load_dump_file {} {
        set type {}
        set at {}
        set dir [pwd]
        puts "dir is $dir"
        foreach t [Waves::get_dump_file_extensions] {
            lappend at ".$t"
        }
        lappend type "{All dump files} {$at}"
        foreach t [Waves::get_dump_file_extensions] {
            lappend type "{$t files} {.$t}"
        }
        lappend type "{All files} {*}"

        set d [tk_getOpenFile -minsize "350 200" -title "Load Dump File" \
               -filetypes $type  -parent $itk_component(hull)]
        if { $d != ""} {
            $_viewer load_dump_file $d
        }
    }

    method reloadVCD {} {
        semu_waves::reload_vcd
    }
    
    method sendSignalToViewer {net} {
	set nbits [getSignalWidth $net]
	set net [backslash_dollars $net]
	#puts "SENDING Signal $net $nbits"
        semu_waves::send_to_viewer $net $nbits
    }
    
    method sendAllEnabledSignals {} {
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
	    set text [$itk_component(slist) cellcget $row,capture -text]
	    #puts "send all found row $row $text"
	    if {$text == 1} {
		set net [$itk_component(slist) getcells $row,hierarchy]
		set nbits [getSignalWidth $net]
		#puts " send net $net $nbits"
		semu_waves::send_to_viewer $net $nbits
	    }
	}
    }
    
    method getSignalWidth {net} {

	set lastslash [string last "/" $net]
	if {$lastslash <= 0} {
	    return 1
	}
	incr lastslash -1
	set hierarchy [string range $net 0 $lastslash]
	
	set netlist [redirect::netlist lsnet $hierarchy]

        set i [lsearch -exact -index 0 $netlist $net]
	if {$i == -1} {
	    return 1
	}
	set rec [lindex $netlist $i]

	set bracketfield [lindex $rec 1]
	if {[llength $bracketfield] == 1} {
	    return 1
	}
	set bracket [lindex $bracketfield 1]

	set openbracket [string last "\[" $bracket]
	set closebracket [string last "\]" $bracket]
	if {$openbracket < 0 || $closebracket < 0} {
	    return 1
	}
	incr openbracket
	incr closebracket -1
	set nbits [string range $bracket $openbracket $closebracket]

	return [expr $nbits]
    }

    method closeWindow {} {
        status_window::stop
        exit
#        destroy $_Frame
    }
    
    # method signalDelete {} {
    #  method to delete a group of selected rows
    
    #     $itk_component(slist) finishediting
    
    #     set size [$itk_component(slist) size]
    #     set del_list {}
    #     for {set row 0} {$row < $size} {incr row} {
    #         if [$itk_component(slist) getcells $row,0] {
    #             puts "delete row number: $row"
    #             lappend del_list $row
    #             if [$itk_component(slist) getcells $row,4] {
    #                 action_disable [$itk_component(slist) getcells $row,2]
    #             }
    #         }
    #     }
    #     $itk_component(slist) delete $del_list
    # }
}

            

        #wm title $itk_interior "Semu Instrumentation Panel"
        #wm minsize $itk_interior 1080 600
        
        #mkInstrumentWin
         #mkMenuBar
#         mkPanedWin
#         mkSelectionWin
#         mkBreakExprs
       

#         pack $itk_component(menubar) -fill x  -side top  
#         pack $itk_component(pw)  -fill both -expand true -pady 0 -side top
#         pack $itk_component(sw) -fill both -expand true -pady 0  -side top
#         pack $itk_component(ipw) -fill both -expand true -pady 0 -side top

#  #       pack $itk_component(bbox) -fill x -expand true -side bottom -pady 0


