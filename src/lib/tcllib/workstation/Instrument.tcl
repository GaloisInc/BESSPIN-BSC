package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist
package require Redirect

package provide Instrument 1.0

# standard Bluespec colours and fonts
# fonts::set_colours
# fonts::initialize


catch "itcl::delete class instrument_viewer"

itcl::class instrument_viewer {
    inherit itk::Widget

    tablelist::addIncrCombobox

    #puts "in class instrument_viewer"
    protected variable _lasthier ""
    protected variable _collectionfile "signals.col"
    variable _litefile "sig.col"
    protected variable _viewer ""
    protected variable _vcdfile "dump1.vcd"
    protected variable _current_top "mkBridge"
    protected variable _original_top "mkBridge"
#    variable viewHierVal 1
#    variable viewCollVal 1
    variable _Frame
    variable _Product "full"

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
    

    constructor {frame top mode no_dbg product args} {
        if {$frame == "none"} {
            set frame $itk_interior
        }

        set _Product $product
        #puts "instrument constructor product $_Product"
        set _Frame $frame
	if {!$no_dbg} {
            #puts "nodebug"
            set nl [lindex [redirect::netlist lsinst] 0]
            #puts "after first netlist call"
	    set _original_top [lindex [split $nl " "] 0]
	    set _current_top $_original_top
	    # if {$args != "" && $args != "{}" } {
	    #     set _current_top $args
	    # }
	}
	if {$top != ""} {
            set _current_top $top
        }

        set pfile $_collectionfile        
        
        # puts "adding promptColl"

        #itk_component add promptColl {
        #    promptColl::create .promptColl $pfile
        #} {}

        #puts "promptColl added"

        #set _viewer [semu_waves::create_viewer]
       
        #mkMenuBar $frame
        #mkInstrumentWin $frame $pane
        #mkMessageWin $frame
    }

    destructor {
        #puts "instrument destructor"
        exit
    }

    # destructor {
    #     status_window::stop
    #    # destroy $_Frame
    # }


   proc create {frame top args} {
        uplevel #0 instrument_viewer $frame  $top $args
    }
    
    method mkInstrumentWin {frame} {
        if {$frame == "none"} {
            set frame $itk_interior
        }

        mkPanedWin $frame
        mkSelectionWin $frame 
       
       return $itk_component(ipw)

    }

 
    method packInst {} {
        pack $itk_component(pw)  -fill both -expand true -pady 0 
        pack $itk_component(swf) -fill both -expand true -pady 0 
    }


    method viewHier {viewHierVal} {
        if {$viewHierVal} {
            pack $itk_component(pw) -fill both -expand true -pady 0 -side bottom
        } else {
            pack forget $itk_component(pw)
        }
    }

    method viewColl {viewCollVal} {
        if {$viewCollVal} {
            pack $itk_component(swf) -fill both -expand true -pady 0  -side bottom
        } else {
            pack forget $itk_component(swf)
        }
    }


    method getFraction {} {
        set fraction [$itk_component(ipw) fraction]
        return $fraction
    }

    method setFraction {value} {
        eval $itk_component(ipw) fraction $value
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



    method mkPanedWin {frame product} {
        set _Product $product
        itk_component add pw {
            base::panedwindow $frame.pw  -orient vertical 
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
        pack $itk_component(hier) -fill both -expand 1 -pady 0
        #####   Signal pane ##########################################
        $itk_component(pw) add signal
        set cs [$itk_component(pw) childsite signal]
        itk_component add filter {
            iwidgets::combobox  $cs.filter \
                -labeltext {Filter} -command "$itk_interior filterSignalUpdate" \
                -completion false -selectioncommand "$itk_interior filterSignalUpdate"
        } {}
        itk_component add signal {
            base::hierarchy $cs.h -labeltext "Signals" -labelpos nw \
                -alwaysquery true  -visibleitems 5x5 \
                -querycommand "$itk_interior listSignals %n" \
                -selectcommand "$itk_interior selectSignal {%n} %s" \
                -selectonemorecommand "$itk_interior selectAnotherSignal {%n} %s" \
		-texttags [list {enabled {-foreground green}} {unavailable {-foreground red}} {pruned {-foreground purple}} {net {-foreground gray}} ]
        } {}
        $itk_component(filter) insert list end * 
        pack $itk_component(signal) -fill both -expand 1  -pady 0
        pack $itk_component(filter) -fill x -expand 0 -pady 0 

        configure_hierviewer signal

      #  pack $itk_component(signal) -fill both -expand 1  -pady 0

        
        $itk_component(pw) fraction 50 50
        return $itk_component(pw)
    }
    
    method mkBreakExprs {frame} {

        set selectcmd [itcl::code $this doSelExpr]

        itk_component add bf {
           iwidgets::Labeledframe $frame.bw -labeltext "Enable Breakpoints" -labelpos ne 
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
		    #puts "CODE: $code"
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

    method mkSelectionWin {frame} {
       # puts "in mkSelectionWin"
        #set product "full"
        itk_component add swf {
            ttk::frame $frame.swf
        } {}

        set swf $itk_component(swf)

        itk_component add sw {
                 iwidgets::Labeledframe $swf.sw  -labeltext "Signal Collection"
        } {}

        set csw [$itk_component(sw) childsite]
        
        set vsb $csw.vsb
        set hsb $csw.hsb
        
        #puts "add slist"
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

        mkBreakExprs $itk_component(swf)

        #puts "BreakExprs added"
        pack $itk_component(sw) -fill both -expand true -pady 0  
        #puts "returning swf"
        return $itk_component(swf)        
    }    

    proc emptyStr val { return "" }
    proc formatHex val {
        if {$val == ""} {
            set val 0
        }
        format "0x%08x" $val 
    }
   
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
	#puts "in editStartcmd $tbl $row $col $text"
        set s [$tbl columncget $col -name]
        #puts "switch is $s"
	switch -glob [$tbl columncget $col -name] {
            capture {
                #puts "switch is capture"
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
                #puts "switch is bk*"
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

    method editLEndCmd {tbl row col text} {
        #puts "in editEndcmd $tbl $row $col $text"
	switch [$tbl columncget $col -name] {
	    nickname {
		return $text
	    }
            dataform {
                return $text
            }
        }
    }
    
    method editLStartCmd {tbl row col text} {
	#puts "in editStartcmd $tbl $row $col $text"
        set s [$tbl columncget $col -name]
        #puts "switch is $s"
	switch -glob [$tbl columncget $col -name] {
            dataform {
                set w [$tbl editwinpath]
                foreach d {bin hex dec} {
                    eval $w insert list end $d
                }
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
              #puts "name $name Product  $_Product"
              if {$_Product == "full"} {
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
              } else {
                  $h component itemMenu add command -label "Add" \
                      -command "$itk_interior edit_action signal addand"
              }
          }
     }


    # Signal in signal list --  list of children signals
    method listSignals {node} {
        set res [list]
        if {$node == ""} {
            set res [redirect::netlist lsnet $_lasthier]
            set f [$itk_component(filter) get]
            set res [lsearch -not -glob -nocase -inline -all -index 2 $res  unavailable]
            if { $f != "" } {
                set res [lsearch -glob -nocase -inline -all -index 1 $res $f]
                #puts "filter nonblank"
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
        set siglist $itk_component(signal)
        refreshEnabled
	refreshEditable
        set a [lindex [$siglist yview] 0] 
        $itk_component(signal) configure -querycommand [$itk_component(signal) cget -querycommand]
        update
        $siglist yview moveto $a
        update
    }


    # reload the signal list either via a new hier node or filter.
    method testSignalList {} {
       # puts "testSignalList"
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
        #set flist  [$itk_component(filter) component list get 0 end]
        #puts "flist $flist"

        if { [lsearch -exact [$itk_component(filter) component list get 0 end] $pat] == -1 } {
           # puts "add to list"
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
            #puts "nets $nets"
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
            if {$_Product == "full"} {
                foreach {bk} [getBkFields] {
                    $itk_component(slist) cellconfigure $row,$bk -editable 1 -bg ""
                }
            
	    $itk_component(slist) cellconfigure $row,mask -editable 1 -bg ""
            $itk_component(slist) cellconfigure $row,value -editable 1 -bg ""
	    }

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
	#puts "addterm $net $value $mask $track"
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

    method action_enable {nets} {
	#puts "action enable $nets"
	foreach net $nets {
	    redirect::netlist enable $net
	}
	foreach net $nets {
	    while {![redirect::netlist isenabled $net]} {}
        }
	refreshSignalList
    }

    method action_disable {nets} {
	#puts "action disable $nets"
	foreach net $nets {
	    redirect::netlist disable $net
	}
	foreach net $nets {
	    while {[redirect::netlist isenabled $net]} {}
	}
	refreshSignalList
    }


    method action_add {nets en} {
        #puts "action_add nets: $nets"
	foreach net $nets {
	    add_selection $net 0 ""
	}
	if {$en} {
	    action_enable $nets
	} else {
	    refreshSignalList
	}
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
	return  [redirect::netlist isenabled $net]
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
			#puts "bk: $bk bkvalue: $bkvalue"
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


    # method add_selection {net en nick} {     
    #     if {$_Product == "full"} {
    #         add_selection_full $net $en $nick
    #     } else {
    #         if {[$itk_component(slist) searchcolumn hierarchy $net] == -1} {
    #             add_selection_lite $net $en 
    #         }
    #     }
    # }

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

    # method add_selection_lite {net en} {   
    #     set amask 0
    #     set avalue 0
    #     set sname [lindex [split $net /] end]
    #     set rindex [$itk_component(slist) insert end ""]
	
    #     $itk_component(slist) cellconfigure $rindex,del -window [itcl::code $this createDelButton]
    #     $itk_component(slist) configcells $rindex,name -text $sname
    #     $itk_component(slist) configcells $rindex,hierarchy -text $net
    #     $itk_component(slist) cellconfigure $rindex,del -image $deleteimg
    #     $itk_component(slist) cellconfigure $rindex,waves -window [itcl::code $this createButton]
    #     $itk_component(slist) cellconfigure $rindex,dataform -text "hex"
    # }

    
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
            # tk_messageBox -title "Error" -icon error -message "file name must be given." \
            #     -parent $itk_interior
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
        #puts "file_data $file_data"
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
	set nets [list]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
	    $itk_component(slist) cellconfigure $row,capture -text 1
	    $itk_component(slist) cellconfigure $row,capture -image "$checkimg"
	    lappend nets $net
	}
	action_enable $nets
	refreshSignalList
    }

    method disableCollection {} {
        #puts "in disableCollection"
        set size [$itk_component(slist) size]
	set nets [list]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
	    $itk_component(slist) cellconfigure $row,capture -text 0
	    $itk_component(slist) cellconfigure $row,capture -image "$uncheckimg"
            for {set col 7} {$col < 11} {incr col} {
                if {[$itk_component(slist) getcells $row,$col]} {
                    action_unsetbreakpoint $row $col
                }
            }
	    lappend nets $net
	}
	action_disable $nets
        #puts "nets disabled"
	refreshSignalList
        #puts "leaving disableCollection"
    }
    
    method startWaveViewer {} {
        set _viewer [semu_waves::start_viewer]
    }

    method viewer_load_dump_file {} {
        set type {}
        set at {}
        set dir [pwd]
        #puts "dir is $dir"
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
            #puts "dump file is $d, viewer is $_viewer"
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
	
	## take care of cases like foo[23/25]
	set slash [string first "/" $nbits]
	incr slash
	set nbits [string range $nbits $slash end]

	return [expr $nbits]
    }

    method closeWindow {} {
        status_window::stop
        exit
#        destroy $_Frame
    }
    





}


# catch "itcl::delete class promptColl"        
# itcl::class promptColl {
#     inherit iwidgets::Dialog
    
#     variable default_name ""
#     variable defcheck 0
#     variable _collist 
#     variable _CType

#     constructor {product args} {
#         lappend args -modality application
        
#         eval itk_initialize $args


#         wm title $itk_component(hull) "Select Collection Name"
       
#         array set _collist [list]

#         puts "promptColl constructor"
#         loadFrame $product

#         puts "promptColl loadFrame complete"

#     }
    
#     method loadFrame {ctype} {
#         set _CType $ctype

#         puts "ctype $_CType $ctype"
#         itk_component add nframe {
#             frame $itk_interior.nframe
#         }
#         puts "nframe added"
        
#         itk_component add nfield {
#             iwidgets::combobox $itk_component(nframe).nfield \
#                 -labeltext "Collection Name" 
#         } {}
        
#         puts "before loading names"
#         if {[file exists $_CType] == 1} {
#             loadNames
#             set names [array get _collist]
            
#             foreach {n d} $names {
#                 eval $itk_component(nfield) insert list end $n
#             }
#         }
        
#         puts "names loaded"

#         $itk_component(hull) hide Apply
#         $itk_component(hull) hide Help

#         pack $itk_component(nfield)  -padx 10 -pady 10 -fill both
#         pack $itk_component(nframe)  -padx 10 -pady 10 -fill both
        
#     } 
        
#     proc create {pathname product args} {
#         uplevel #0 promptColl $pathname $product -modality application $args
#     }


#     method loadNames {} {
#         puts "loadNames CType $_CType"
#         set names ""
#         if {![file exists $_CType]} {
#             "puts no sigfile"
#             return
#         }
#         set lfile [open $_CType "r"]
#         while {[gets $lfile line] >= 0} {
#             regexp {(.*):(.*)} $line fulline key value
#             set name $key
#             set _collist($name) $value 
#         }
#         parray _collist
#         set names [array get _collist]
#         return $names
#     }
    
#     method getNewName {} {
#         set values ""
#         set newname [$itk_component(nfield) get]
#         return $newname
#     }

# }           


