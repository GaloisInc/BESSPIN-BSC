package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist
package require Redirect


package provide Instlite 1.0

catch "itcl::delete class instlite_viewer"
#My::debug
#puts "creating class instlite_viewer"
itcl::class instlite_viewer {
    inherit itk::Widget

    tablelist::addIncrCombobox

    protected variable _lasthier ""
    protected variable _collectionfile "signals.col"
    variable _litefile "sig.col"
    protected variable _viewer ""
    protected variable _vcdfile "dump1.vcd"
    protected variable _current_top "mkBridge"
    protected variable _original_top "mkBridge"

    variable _Frame
    variable _Product "full"
    variable _lastName ""

    common checkimg   ""
    common uncheckimg ""
    common deleteimg  ""
    common waveimg    ""

    constructor {frame top  args} {
        if {$frame == "none"} {
            set frame $itk_interior
        }

        set checkimg   [::fonts::get_image checked]
        set uncheckimg [::fonts::get_image unchecked]
        set deleteimg  [::fonts::get_image delete]
        set waveimg    [::fonts::get_image gtkwave_24x24]
        
        #set _Product $product
        #puts "instrument constructor product $_Product"
        set _Frame $frame
        set nl [lindex [redirect::netlist lsinst] 0]
        #puts "after first netlist call nl $nl"
        set _original_top [lindex [split $nl " "] 0]
        set _current_top $_original_top
	if {$top != ""} {
            set _current_top $top
        }
        #puts "current top $_current_top"
        set pfile $_litefile
        
        #puts "adding promptColl"
        itk_component add promptColl {
            promptColl::create .promptColl 
        } {}

        #puts "promptColl added"

        #set _viewer [semu_waves::create_viewer]
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
       uplevel #0 instlite_viewer $frame $top $args
   }
    
    method mkInstrumentWin {frame} {
        if {$frame == "none"} {
            set frame $itk_interior
        }

        mkPanedWin $frame
        mkSelectionWin $frame 
        mkBreakExprs
       
       return $itk_component(ipw)

    }

 
    method packInst {} {
        pack $itk_component(pw)  -fill both -expand true -pady 0 
        pack $itk_component(swf) -fill both -expand true -pady 0 
        #pack $itk_component(ipw)  -fill both -expand true -pady 0 -side bottom
#        pack $itk_component(iwf)  -fill both -expand true -pady 0 -side bottom
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
    


    method mkSelectLite {frame} {
        #puts "in mkSelectLite"
        #set product "lite"
        itk_component add swf {
            ttk::frame $frame.swf
        } {}

        set swf $itk_component(swf)

        itk_component add sw {
                 iwidgets::Labeledframe $frame.sw  -labeltext "Signal Collection"
        } {}

        set csw [$itk_component(sw) childsite]
        
        set vsb $csw.vsb
        set hsb $csw.hsb
        #puts "add slist"
        itk_component add slist {
            tablelist::tablelist $csw.slist  \
                -columns {4 "Del" left \
                               0 "Name" left \
                               0 "Hierarchy" left \
                              8 "Width" center \
                               0 "Value" center \
                              0 "Format" center
                               0 "hvalue" center \
                              0 "bvalue" center } \
                -labelcommand tablelist::sortByColumn \
                -sortcommand instlite_viewer::compareAsSet \
                 -width 100 -stretch {1 2 4} \
                -xscrollcommand [list $hsb set] -yscrollcommand [list $vsb set] \
                -background white -stripebg #e4e8ec \
                -movablecolumns true -movablerows true -resizablecolumns true \
                -editstartcommand [itcl::code $this editLStartCmd] \
                -editendcommand [itcl::code $this editLEndCmd] \
            } {}

        set slist $itk_component(slist)
       # puts "slist added"

        $slist columnconfigure 1 -name name
        $slist columnconfigure 2 -name hierarchy
        $slist columnconfigure 3 -name width
        $slist columnconfigure 4 -name lvalue -editable no
        $slist columnconfigure 5 -name dataform -editable yes -editwindow combobox 
        $slist columnconfigure 0 -name del  -editable no       
        $slist columnconfigure 6 -name hvalue -hide yes
        $slist columnconfigure 7 -name bvalue -hide yes

        set bodyTag [$slist bodytag]
        bind $bodyTag <Double-1> [itcl::code $this selectCmd]
        

        scrollbar $vsb -orient vertical -command [list $csw.slist yview]
        scrollbar $hsb -orient horizontal -command [list $csw.slist xview]
      
        grid $slist  -row 0 -rowspan 2 -column 0 -sticky news
        
        grid $vsb -row 1 -column 1 -sticky ns
        grid $hsb -row 2 -column 0 -sticky ew
        grid rowconfigure $csw 1 -weight 1
        grid columnconfigure $csw 0 -weight 1

        itk_component add bbox {
            iwidgets::buttonbox $swf.bbox
        } {}
        $itk_component(bbox) add read -text "Read State" -underline 0 -command "[itcl::code $this doRead]"
        
        pack $itk_component(sw) -fill both -expand true -pady 0  
        pack $itk_component(bbox) -pady 0 -side bottom -fill both -expand true

        return $itk_component(swf)         
    }   

    method doRead {} {
        #puts "do readback"
        bsdebug::semulite readback
        #puts "readback done"
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
            #puts "net $net"
            set retvals [bsdebug::semulite sigval $net]
            #puts "retvals $retvals"
            set width [lindex $retvals 0]
            set hvalue [lindex $retvals 1]
            set bvalue [lindex $retvals 2]
            set dataform [$itk_component(slist) getcells $row,dataform]
            $itk_component(slist) cellconfigure $row,hvalue -text  $hvalue
            $itk_component(slist) cellconfigure $row,bvalue -text  $bvalue   
            set formed [dispValue $width $hvalue $bvalue $dataform]
            $itk_component(slist) configcells $row,width -text $width
            $itk_component(slist) configcells $row,lvalue -text $formed
        }
        #semu_waves::reload_vcd
    }

    method dispValue {width hvalue bvalue dataform} {
        switch $dataform {
            bin {
                if {$width > 4} {
                    set formed [addCommas $width $bvalue "_" 4]
                } else {
                    set formed $bvalue
                } 
            }
            hex {
                if {$width > 16} {
                    set digits [expr $width/4]
                    set formed [addCommas $digits $hvalue "_" 4]
                } else {
                    set formed $hvalue
                }
            }
            dec {
                set dvalue [scan $hvalue %x]
                set digits [string length $dvalue]
                #puts "hvalue $hvalue dvalue $dvalue"
                if {$digits > 3} {
                    set formed [addCommas $digits $dvalue "," 3]
                } else {
                    set formed $dvalue
                }
            }
        }
        return $formed
    }
        

    method getValues {siglist} {
        set retvalue ""
        #puts "siglist $siglist"
        set cmd "exec -ignorestderr readback -c xpc -x syn/top.xrf $siglist"
        #puts "cmd $cmd"
        if {[catch $cmd status]} {
	    puts stderr "Error: $status"
	    return
	}
        #set retval [eval exec $cmd]
        #puts "retvalue $retval"
        set results [regexp -line -all -inline {^/.*$} $status]
        return $results

        #regexp -line {^/.*$} $retval sigline
        #puts "line $sigline"
        #puts "regexp -line {^/.*$} $foo ff"
        #return $sigline
    }


    method addCommas {width value symbol size} {
        set tmpstring ""
        set tmprange ""
        set numcommas [expr $width/$size]
        if {[expr $width%$size] == 0} {
            incr numcommas -1
        }
        set y [expr $width -1]
        set x [expr $width - $size]
        #puts "width $width value $value commas $numcommas y $y"
        for {set l 0} {$l <= $numcommas} {incr l} {
         #   puts "l $l x $x y $y"
            set tmprange [string range $value $x $y]
            if {$l == 0} {
                set newstring $tmprange
          #      puts "tmprange $tmprange newstring $newstring"
            } else {
                set tmpstring $newstring
                set newstring ""
                append newstring $tmprange $symbol $tmpstring 
           #     puts "tmprange $tmprange newstring $newstring"
            }
            incr x -$size
            incr y -$size
        }
        return $newstring
    }
                
  
    method formatValue {hval bval width  mode} {
        #puts "formatValue $hval $bval $width $mode"
        if {$mode == "hex"} {
            set newval [format "%d'h%x" $width $hval]
        } elseif {$mode == "dec"} {
            set newval [format "%d'd%d" $width $hval]
        } elseif {$mode == "bin"} {
            set newval $bval
        }
        return $newval
    }



    proc emptyStr val { return "" }
    proc formatHex val {
        if {$val == ""} {
            set val 0
        }
        format "0x%08x" $val 
    }

    proc convertfromhex {format val} {
        switch $format {
            bin {
            }
            dec {
                
            }
        }   
    }

    proc convertfrombin {format val} {
    }
    
    proc convertfromdec {format val} {
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




    method editLEndCmd {tbl row col text} {
        #puts "in editEndcmd $tbl $row $col $text"
	if {[$tbl columncget $col -name] == "dataform"} {
            set hvalue [$tbl getcells $row,hvalue]
            set bvalue [$tbl getcells $row,bvalue]
            switch $text {
                bin {
                    set width [string length $bvalue]
                    if {$width > 4} {
                        set newvalue [addCommas $width $bvalue "_" 4]
                    } else {
                        set newvalue $bvalue
                    } 
                }
                dec {
                    set dvalue [scan $hvalue %x]
                    set digits [string length $dvalue]
                    if {$digits > 3} {
                        set newvalue [addCommas $digits $dvalue "," 3]
                    } else {
                        set newvalue $dvalue
                    }                 
                }
                hex {
                    set digits [string length $hvalue]
                    if {$digits > 4} {
                        set newvalue [addCommas $digits $hvalue "_" 4]
                    } else {
                        set newvalue $hvalue
                    }
                }
            }
            $tbl configcells $row,lvalue -text $newvalue
            return $text
        }
    }
    
        

    method editLStartCmd {tbl row col text} {
	#puts "in editStartcmd $tbl $row $col $text"
        set s [$tbl columncget $col -name]
        #puts "switch is $s"
	switch -glob [$tbl columncget $col -name] {
            dataform {
                set w [$tbl editwinpath]
                $w configure -editable false
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
              $h component itemMenu add command -label "Add" \
                  -command "$itk_interior edit_action signal addand"
              
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
        }
    }

    # reload the signal list either via a new hier node or filter.
    method refreshSignalList {} {
        set siglist $itk_component(signal)
	#enableAllEdits
        set a [lindex [$siglist yview] 0] 
        $itk_component(signal) configure -querycommand [$itk_component(signal) cget -querycommand]
        update
        $siglist yview moveto $a
        update
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


  
        method action_add {nets en} {
        #puts "action_add nets: $nets"
	foreach net $nets {
	    add_selection $net 0 ""
	}
        refreshSignalList
    }

    method action_disable {nets} {
        foreach net $nets {
            redirect::netlist disable $net
        }
        refreshSignalList
    }
        

    method add_selection {net en nick} {     
        if {[$itk_component(slist) searchcolumn hierarchy $net] == -1} {
            add_selection_lite $net $en 
            redirect::netlist enable $net
        }
    }
    
  
    method add_selection_lite {net en} {   
        set amask 0
        set avalue 0
        set sname [lindex [split $net /] end]
        set rindex [$itk_component(slist) insert end ""]
	
        $itk_component(slist) cellconfigure $rindex,del -window [itcl::code $this createDelButton]
        $itk_component(slist) configcells $rindex,name -text $sname
        $itk_component(slist) configcells $rindex,hierarchy -text $net
        $itk_component(slist) cellconfigure $rindex,del -image $deleteimg
        $itk_component(slist) cellconfigure $rindex,dataform -text "hex"
    }

    
    method signalDelete {row} {
        set net [$itk_component(slist) getcells $row,hierarchy]
        $itk_component(slist) delete $row
        instlite_viewer::action_disable $net
        refreshSignalList
    }


    method doToAllEdits {action} {
	set size [$itk_component(slist) size]

	for {set row 0} {$row < $size} {incr row} {
	    $itk_component($comp) $action $row
	}
    }

        
    
    method clearCollection {} {
        #puts "in clearCollection"
        set size [$itk_component(slist) size]
        set nets [list]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
            lappend nets $net
        }
        action_disable $nets
        $itk_component(slist) delete 0 end
        refreshSignalList
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
    
    method sendAllSignals {} {
        set size [$itk_component(slist) size]
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
            set nbits [getSignalWidth $net]
            #puts " send net $net $nbits"
            semu_waves::send_to_viewer $net $nbits
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
    

    method modColl {name} {
        set saved False
        set colfile [open $_litefile "r"]
        set tmpfile [open temp.col "w+"]
        set pln ""
        while {[gets $colfile fline] >= 0} {
            regexp {(.*):(.*)} $fline fulline key value
            #puts "modcoll $fline"
            if {$key == $name } {
                if {!$saved} {
                    set size [$itk_component(slist) size]
                    #puts "size $size"
                    for {set row 0} {$row < $size} {incr row} {
                        set net [$itk_component(slist) getcells $row,hierarchy]
                        set form [$itk_component(slist) getcells $row,dataform]
                        append pln "$name: $net $form" \n
                        set saved True
                    } 
                }
            } elseif {$fline != ""} {
                append pln $fline \n
            }
	}
        puts $tmpfile $pln
        catch [close $colfile]        
	catch [close $tmpfile]
        file copy -force "temp.col" $_litefile
    }

    method addColl {name} {
        set colfile [open $_litefile "a+"]
    }
        
    method saveLiteCollection {} {
        if {[file exists $_litefile] && $_lastName != ""} {
            modColl $_lastName
        } else {
            saveLiteCollectionAs 
        }
    }

    method saveLiteCollectionAs {} {
        if {[$itk_component(promptColl) activate] == 0} {
            return
        }
        #puts "response $response"
        set collName [$itk_component(promptColl) getNewName]
        if {$collName == ""} {
           tk_messageBox -title "Error" -icon error -type ok \
                            -message "The collection name cannot be blank."
            return
        }
        #puts "collection Name $collName"
        if {[file exists $_litefile]} {
            set names [promptColl::loadNames $_litefile]
            if {[lsearch -exact $names $collName] == -1} {
                newColl add $collName
            } else {
                modColl $collName
            }
        } else {
            newColl new $collName
        }
    }

   

    method newColl {type collName} {
        #puts "in doSaveLite name $collName"
        if {$type == "new"} {
            set colfile [open $_litefile "w"]
        } else {
            set colfile [open $_litefile "a+"]
        }
        set pln ""
        set size [$itk_component(slist) size]
        #puts "size $size"
        for {set row 0} {$row < $size} {incr row} {
            set net [$itk_component(slist) getcells $row,hierarchy]
            set form [$itk_component(slist) getcells $row,dataform]
            append pln "$collName: $net $form" \n
	}
        puts $colfile $pln
	catch [close $colfile]
    }

    method loadLiteCollection {collname} {
        set collist {}
	set colfile [open $_litefile "r"]
        while {[gets $colfile line] >= 0} {
            #puts "readline $line"
            regexp {(.*):(.*)} $line fulline key value
            if {$key == $collname && $line != ""} {
                lappend collist $value
            }
        }
        #puts "collist $collist"
        add_lite_collection $collist
        
        catch [close $colfile]
        set _lastName $collname

        $itk_component(hier) expand $_current_top
    }


    method add_lite_collection {collist} {

        set nets {}
        foreach line $collist {
            foreach {net form}  $line {
                if {[$itk_component(slist) searchcolumn hierarchy $net] == -1} {
                    lappend nets $net
                    #set net [lindex $signal 0]
                    #puts "net $net form $form"
                    set sname [lindex [split $net /] end]
                    #set form [lindex $signal 1]
                    set rindex [$itk_component(slist) insert end ""]
                    $itk_component(slist) configcells $rindex,hierarchy -text $net
                    $itk_component(slist) configcells $rindex,name -text $sname
                    $itk_component(slist) configcells $rindex,dataform -text $form
                    $itk_component(slist) cellconfigure $rindex,del -window [itcl::code $this createDelButton]
                    $itk_component(slist) cellconfigure $rindex,del -image $deleteimg
                    redirect::netlist enable $net
                }
            }
        }
        refreshSignalList
    }


    }

catch "itcl::delete class promptColl"        
itcl::class promptColl {
    inherit iwidgets::Dialog
    
    variable default_name ""
    variable defcheck 0
    variable _CType "sig.col"

    constructor {args} {
        lappend args -modality application
        
        eval itk_initialize $args

        wm title $itk_component(hull) "Select Collection Name"
        #puts "promptColl constructor"
        loadFrame  

        #puts "promptColl loadFrame complete"

    }
    
    method loadFrame {} {
        itk_component add nframe {
            frame $itk_interior.nframe
        }
        #puts "nframe added"
        
        itk_component add nfield {
            iwidgets::combobox $itk_component(nframe).nfield \
                -labeltext "Collection Name" 
        } {}
        
        #puts "before loading names"
        if {[file exists $_CType] == 1} {
            set names [loadNames $_CType]
            #puts "names $names"
            foreach n $names {
                eval $itk_component(nfield) insert list end $n            
            }
        }
            
        # puts "names loaded"

        $itk_component(hull) hide Apply
        $itk_component(hull) hide Help

        pack $itk_component(nfield)  -padx 10 -pady 10 -fill both
        pack $itk_component(nframe)  -padx 10 -pady 10 -fill both
        
    } 
        
    proc create {pathname args} {
        uplevel #0 promptColl $pathname -modality application $args
    }


    proc loadNames {filename} {
        set names ""
        if {![file exists $filename]} {
            return
        }
        set lfile [open $filename "r"]
        while {[gets $lfile line] >= 0} {
            regexp {(.*):(.*)} $line fulline key value
            if {$key == ""} {
                return
            }
            set name $key
            if {[lsearch -exact $names $name] == -1} {
                lappend names $name
            }
        }
        #puts "loading names $names"
        close $lfile
        return $names
    }
    
    method getNewName {} {
        set values ""
        set newname [$itk_component(nfield) get]
        return $newname
    }

}           


