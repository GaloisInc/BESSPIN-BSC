package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist

package provide semu_pin 1.0

tablelist::addIncrCombobox

catch "itcl::delete class semu_pin"
itcl::class semu_pin {
    inherit iwidgets::Dialog

    variable _Pinfile ""
    variable _TempPinfile ""
    variable _Clock ""
    variable _Cnum 0
    variable _Tran
    variable _Tname
    variable _Verify
    variable _Transactors
    variable TN
    variable uid
    variable _Children
    variable _dragging false
    variable _lastwidget {}
    variable _dragwidget
    variable _dragname
    variable _dragdir
    variable _dragsize
#    variable _dragclock
    variable _bodyTagH
    variable _dragrow
    variable _moving false
    variable _contextrow -1
    variable _transload
    variable _newrow
    variable _memory false
    variable _pipes true
    variable _Complete true
    variable _editing false

    common interface [list \
                          {"<Add Port>" Port ""} \
                          {"<Add RDY/EN>" RDY/EN ""} \
                          {"<Add Memory>" Memory "" 32 256} \
                          ]

    common rdyen [list \
                      {"<Add Data>" DATA ""} \
                      {"<Add RDY>" RDY output} \
                      {"<Add EN>" EN input} \
                      ] 


    common memory [list \
                       {"<MEMREQ Name>" Request "output"} \
                       {"<MEMRESP Name>" Response "input"}\
                       ]

    common Request [list \
                      {"<Add Data>" DATA output} \
                      {"<Add RDY>" RDY output} \
                      {"<Add EN>" EN input} \
                      ] 
    common Response [list \
                      {"<Add Data>" DATA input} \
                      {"<Add RDY>" RDY output} \
                      {"<Add EN>" EN input} \
                      ] 
                        

    constructor {args} {
        lappend -args -modality application 

        eval itk_initialize $args   

        array set _Tran [list]
        array set _Tname [list]
        array set _Transactors [list]
        array set TN [list]
        array set ret [list]

        wm title $itk_component(hull) "Pin Interface Definition"
        wm minsize $itk_component(hull) 1000 500

        itk_component add semu_build {
            semu_build::getTbSettings
        } {}

        itk_component add popUpEntry {
            popUpEntry::create .pop
        } {}
        
        itk_component add popUpPin {
            popUpPin::create .state
        } {}
        
        itk_component add popUpMem {
            popUpMem::create .mem
        } {}
        
   #     $itk_component(hull) hide Help
        $itk_component(hull) hide Apply
      
        
        buttonconfigure OK -text "Save"  -command [itcl::code $this finishPins]
        buttonconfigure Apply -text "Verify" -command [itcl::code $this verifyAll]
        buttonconfigure Cancel  -text "Configure Testbench" -command [itcl::code $this configTb]
        buttonconfigure Help -text "Cancel" -command [itcl::code $this deactivate 0] 

        mkAdvPin
        drag init
    }

    destructor {}


    method mkAdvPin {} {
        mkPaneWin
        mkCMenu
        mkPins
        mkTranHier
        mkMenuBar
 
        pack $itk_component(mb) -fill x
        pack $itk_component(ppw) -fill both -expand true -side top
   }


    proc create {pathname args} { 
        uplevel #0 semu_pin  $pathname -modality application $args
    }

    private method mkMenuBar {} {
        itk_component add mb {
            iwidgets::menubar $itk_interior.mb -menubuttons {
                menubutton edit -text "Edit" -underline 0 -menu {
                    options -tearoff false
                    command moveall -label "Move all unassigned pins to ports" -underline 4 \
                        -command "[itcl::code $this moveAll Port]"
                    command lockstep -label "Make all pins Lockstep" -underline 13 \
                        -command "[itcl::code $this moveAll LOCKSTEP]"
                    # command moveleft -label "Move selected back to unassigned pins" -underline 4 \
                    #     -command "[itcl::code $this moveSelectLeft]"
                }
                # menubutton view -text "View" -underline 0 -menu {
                #     options -tearoff false
                #     command expand -label "Expand All" -underline 0 \
                #         -command "$itk_component(thier) expandall"
                #         command collapse -label "Collapse All" -underline 0 \
                #         -command "$itk_component(thier) collapseall"
                # }
            }       
        }
    }

     
    private method mkPaneWin {} {
        itk_component add ppw {
            base::panedwindow $itk_interior.ppw -orient vertical 
        } 
    }
    
    method mkTranHier  {} {
        $itk_component(ppw) add stw
        set tws [$itk_component(ppw) childsite stw]

        itk_component add sw {
            iwidgets::Labeledframe $tws.tw -labeltext "Interfaces" -labelpos nw
        } {}
        
        set tw [$itk_component(sw) childsite]

        set vsb $tw.vsb
        set hsb $tw.hsb

        itk_component add thier {
            tablelist::tablelist $tw.thier \
                -columns {0 "Name" left \
                    0 "Type" left \
                    0 "Direction" left \
                              0 "Size" left } \
                -treecolumn 0  -treestyle ubuntu \
                -resizablecolumns true \
                -labelcommand tablelist::sortByColumn \
                -background white \
                -yscrollcommand [list $vsb set] -setgrid no -width 0 \
                -xscrollcommand [list $hsb set] -setgrid no -width 0 \
                -editendcommand [itcl::code $this editEndCmd] \
                -editstartcommand [itcl::code $this editStartCmd] \
                -selectmode single 
        } {}

        set thier $itk_component(thier)            

        $thier columnconfigure 0 -name name -editable 0 
        $thier columnconfigure 1 -name type \
            -sortmode command -sortcommand "[itcl::code $this sortType]"
        $thier columnconfigure 2 -name dir
        $thier columnconfigure 3 -name size
        

        scrollbar $vsb -orient vertical -command [list $tw.thier yview]
        scrollbar $hsb -orient horizontal -command [list $tw.thier xview]
        
        set menu $tw.menu
        menu $menu -tearoff no
        $menu add command -label "Display Children" \
            -command [list [itcl::code $this putChildrenOfSelWidget $thier]]

        set _bodyTagH [$thier bodytag]
        
        bind $_bodyTagH <ButtonPress-1> +[itcl::code $this drag start %W %x %y]
        bind $_bodyTagH <Motion> [itcl::code $this drag motion %X %Y]
        bind $_bodyTagH <ButtonRelease-1> +[itcl::code $this drag stop %X %Y %x %y]
        bind $_bodyTagH <Button-4> [itcl::code $this newMouse]
        bind $_bodyTagH <Button-5> [itcl::code $this newMouse]
        

        bind $_bodyTagH <<DragOver>> [itcl::code $this  drag over %W ]
        bind $_bodyTagH <<DragLeave>> [itcl::code $this  drag leave %W]
        bind $_bodyTagH <<DragDrop>> [itcl::code $this  drag drop %W %x %y]
      
        grid $thier -row 0 -rowspan 2 -column 0 -sticky news
        grid $vsb -row 0 -rowspan 2 -column 1 -sticky ns
        grid $hsb -row 2  -column 0 -sticky ew
        grid rowconfigure $tw 1 -weight 1
        grid columnconfigure $tw 0 -weight 1
        pack $itk_component(sw) -side top -expand yes -fill both

    }

    method editEndCmd {tbl row col text} {
        #puts "in editEndCmd"
        set _editing false
        #puts "editing $_editing"
        return $text
        # switch [$tbl columncget $col -name] {
	#     name {
        #         set _editing false
        #         puts _editing "editing"
	# 	return $text
        #     }
        # }
    }

    method editStartCmd {tbl row col text} {
        #puts "in editStartCmd"
        switch [$tbl columncget $col -name] {
            name {
                set _editing true
                #puts "editing $_editing"
            }
        }
    }
    
    method newMouse {} {
        #puts "in NewMouse"
        if {$_editing} {
            return -code break
        }
    }

    method sortType {a b} { 
        set compvalue 0
        if {$a == "DATA" && $b == "RDY"} {
            set compvalue -1
        }
        if {$a == "RDY" && $b == "EN"} {
            set compvalue -1
        }
        if {$a == "RDY" && $b == "DATA"} {
            set compvalue 1
        }
        if {$a == "EN" && $b == "RDY"} {
            set compvalue 1
        }
        if {$a == "RDY/EN" && $b == "Memory"} {
            set compvalue -1
        }
        if {$a == "Memory" && $b == "RDY/EN"} {
            set compvalue 1
        }
        if {$a == "RDY/EN" && $b == "Port"} {
           set  compvalue 1
        }
        if {$a == "Port" && $b == "RDY/EN"} {
            set compvalue -1
        }
        if {$a == "Port" && $b == "Memory"} {
            set compvalue -1
        }
        if {$a == "Memory" && $b == "Port"} {
            set compvalue 1
        }
        if {$a == "Reset" && $b == "Port"} {
            set compvalue -1
        }
        if {$a == "Port" && $b == "Reset"} {
            set compvalue 1
        }
        return $compvalue
    }


    # method sortName {a b} {
    #     set compvalue 0

    #     if {$a == $b} {
    #       set compvalue 0
    #     }
    #     if {[regexp {^<Add} $a] && ![regexp {^<Add} $b]} {
    #         set  compvalue -1
    #     }
    #     if {[regexp {^<Add} $b] && ![regexp {^<Add} $a]} {
    #          set compvalue 1
    #     }
    #     return $compvalue
    # }

    method mkCMenu {} {

        itk_component add cmb {
            menu $itk_interior.cmb -tearoff 0
        } {}

        itk_component add dmb {
            menu $itk_interior.dmb -tearoff 0
        } {}

        itk_component add amb {
            menu $itk_interior.amb -tearoff 0
        } {}
        itk_component add pmb {
            menu $itk_interior.pmb -tearoff 0
        } {}

        
   }
           
    method contextMenu {menu W x y X Y} {
        #Get the index of the cell just clicked
        foreach {tbl a b} [tablelist::convEventFields $W $x $y] {}
        if {[$tbl containing $b] < 0} {
            return
        }

       # set _contextrow [$tbl containing  $b]
        set puid [$tbl containing  $b]
        set _contextrow $puid
        
        #$tbl selection set $_contextrow
        
        
        if {$tbl == $itk_component(pnlist)} {
            tk_popup $itk_component(pmb) $X $Y
        } else {
            set type [$tbl getcell $puid,type]
        
            if {$type == "Clock"} {
                tk_popup $itk_component(cmb) $X $Y
            } elseif {$type == "RDY/EN" || $type == "Request" || $type == "Response" || $type == "Memory"} {
                tk_popup $itk_component(dmb) $X $Y
            } elseif {$type == "DATA" || $type == "RDY" || $type == "EN" || $type == "Port"} {
                tk_popup $itk_component(amb) $X $Y
            }
        }
    }

    method moveSelectLeft {srow} {
        set tb $itk_component(thier)
        if {$srow == ""} {
            set srow [$tb curselection]
        }
        set name [$tb getcells $srow,name]
        if {[regexp {^<} $name]} {
            tk_messageBox -title "Error" -icon error\
                -message "An assigned pin must be selected" \
                -parent $itk_interior
            return
        }
        set type [$tb getcells $srow,type]
        set pkey [$tb parentkey $srow]  
        if {$type == "Clock"} {
            moveClock
            addNewRow $srow $type 0
        } else {
            moveSelectType $type $pkey $srow
        }
    }

    method moveSelectType {type pkey srow} {
        set tb $itk_component(thier)
        if {$type == "Handshake"} {
            moveChildren $srow
            $tb delete $srow
        } elseif {$type == "Memory"} {
            set memchildren [$tb childkeys $srow]
            foreach child $memchildren {
                moveChildren $child
                $tb delete $child
            }
            $tb delete $srow
        } elseif {$type == "Request" || $type == "Response"} {
            moveChildren $srow
            if {$type == "Request"} {
                set ifc $Request
            } else {
                set ifc $Response
            }
            foreach tran $ifc {
                set size ""
                set dir [lindex $tran 2]
                set type [lindex $tran 1]
                set name [lindex $tran 0]
                
                set item {}
                lappend item $name $type $dir $size
                set newrow [$tb insertchild $srow end $item]
                $tb configrows $newrow -fg red
            }
        } else {
            moveLeft $srow 
            addNewRow $srow $type $pkey
        }
        if {[checkAddRow]} {
            set item {}
            lappend item "<Drag pin for Interface>" 
            set newrow [$itk_component(thier) insertchild 0 end $item]
            $itk_component(thier) configrows $newrow -fg gray55
        }    
    }
    
    method moveChildren {pkey} {
        set tb $itk_component(thier)
        set children [$tb childkeys $pkey]
        foreach child $children {
            set ctype [$tb getcells $child,type]
            set cname [$tb getcells $child,name]
            if {$ctype == "DATA" || $ctype == "EN" || $ctype == "RDY" || $ctype == "Reset"} {
                if {![regexp {^<} $cname]} {
                    moveLeft $child
                }
                $tb delete $child
            } 
        }
    }

    method moveClock {} {
        set tb $itk_component(thier) 
        set pn $itk_component(pnlist)
        set srow 0
        set name [$tb getcells $srow,name]
        set dir [$tb getcells $srow,dir]
        set size [$tb getcells $srow,size]
        set item {}
        lappend item $dir $name $size
        $pn insert end $item
        set children [$tb childkeys 0]
        foreach child $children {
            moveSelectLeft $child
        }
    }
            
    method checkAddRow {} {
        set tsize [$itk_component(thier) size]
        for {set row 0} {$row < $tsize} {incr row} {
            set name [$itk_component(thier) getcells $row,name]
            #puts "checkaddrow $name"
            if {[regexp {^<Drag pin} $name] } {
                #puts "checkaddRow returning false"
                return False
            }
        }
        return True
    }

    method addNewRow {srow type pkey} {
        set tb $itk_component(thier)
        set ptype [$tb getcells $pkey,type]
        set newname "<Add $type>"
        $tb delete $srow
        set item {}
        if {$type == "EN" || $type == "RDY"} {
            if {$type == "EN"} {
                set dir "input"
            } else {
                set dir "output"
            }
            lappend item $newname $type $dir ""
            set newrow [$tb insertchild $pkey end $item]
            $tb configrows $newrow -fg red
        } elseif {$ptype == "Request" } {
            set dir "output"
            lappend item $newname $type $dir ""
            set newrow [$tb insertchild $pkey end $item]
            $tb configrows $newrow -fg red
        } elseif {$ptype == "Response"} {
            set dir "input"
            lappend item $newname $type $dir ""
            set newrow [$tb insertchild $pkey end $item]
            $tb configrows $newrow -fg red
        } elseif  {$type == "DATA"} {
            set datakeys [$tb searchcolumn type DATA -parent $pkey -descend -all]
            set datanames ""
            foreach dataitem $datakeys {
                lappend datanames [$tb getcells $dataitem,name]
            }
            if {[lsearch -regexp $datanames "<Add Data>" ] < 0} {
                lappend item "<Add Data>" DATA ""
                set newrow [$tb insertchild $pkey end $item]
                $tb configrows $newrow -fg gray55
            }            
            if {[llength $datakeys] == 1} {
                set datarow [lindex $datakeys 0]
                if {[regexp {^<} [$tb getcells $datarow,name]]} {
                    $tb configrows $datarow -fg red 
                } 
            }
            
        } elseif {$type == "Clock"} {
            lappend item   "<Drag Clock Pin from Unconnected Pins>" 
            set newrow [$tb insertchild root 0 $item]
            $tb configrows $newrow -fg red
        } elseif {$type == "Reset"} {
            lappend item "<Drag Reset Pin from Unconnected Pins>" 
            set newrow [$tb insertchild 0 0 $item]
            $tb configrows $newrow -fg red
            set item ""
            # lappend item "<Drag pin for Interface>" 
            # set newrow [$itk_component(thier) insertchild 0 end $item]
            # $itk_component(thier) configrows $newrow -fg gray55
        }
    }

    method moveLeft {srow} {
        set tb $itk_component(thier)
        set pn $itk_component(pnlist)
        if {$srow == "none"} {
            set srow [$tb curselection]
        }
        set name [$tb getcells $srow,name]
        set dir [$tb getcells $srow,dir]
        set size [$tb getcells $srow,size]
        set item {}
        lappend item $dir $name $size
        $pn insert end $item
    }

    method mkPins {} {
        
        $itk_component(ppw) add pins
        set pw [$itk_component(ppw) childsite pins]

        itk_component add pnframe {
            iwidgets::Labeledframe $pw.pnframe -labeltext "Unconnected Pins" -labelpos nw
        } {}
        set pnframe [$itk_component(pnframe) childsite]
        set vsbp $pnframe.vsbp
        set hsbp $pnframe.hsbp

        itk_component add pnlist {
            tablelist::tablelist $pnframe.pnlist \
                -columns {0 "Input/Output" left \
                              0 "Name" left \
                          0 "Size" left}\
                -labelcommand tablelist::sortByColumn \
                -xscrollcommand [list $hsbp set] -yscrollcommand [list $vsbp set] \
                -height 15 -width 100 -stretch all \
                -background white -stripebg #e4e8ec \
                -resizablecolumns true \
                -selectmode single
        } {}
        set pnlist $itk_component(pnlist)
        $pnlist columnconfigure 0 -name dir  
        $pnlist columnconfigure 1 -name name -sortmode dictionary
        $pnlist columnconfigure 2 -name size

 
        scrollbar $vsbp -orient vertical -command [list $pnframe.pnlist yview]
        scrollbar $hsbp -orient horizontal -command [list $pnframe.pnlist xview]
      
        set bodyTag [$pnlist bodytag]

        bind $bodyTag <ButtonPress-1> +[itcl::code $this drag start %W %x %y]
        bind $bodyTag <Motion> [itcl::code $this drag motion %X %Y]
        bind $bodyTag <ButtonRelease-1> +[itcl::code $this drag stop %X %Y %x %y]
        
        bind $bodyTag <<DragOver>> [itcl::code $this  drag over %W ]
        bind $bodyTag <<DragLeave>> [itcl::code $this  drag leave %W]
        bind $bodyTag <<DragDrop>> [itcl::code $this  drag drop %W %x %y]


        grid $pnlist  -row 0 -rowspan 2 -column 0 -sticky news
        grid $vsbp -row 1 -column 1 -sticky ns
        grid $hsbp -row 2 -column 0 -sticky ew
        grid rowconfigure $pnframe 1 -weight 1
        grid columnconfigure $pnframe 0 -weight 1
        
        pack $itk_component(pnframe) -expand true -fill both -side top

    }

    method drag {cmd args} {
        switch $cmd {
            init {
                set _lastwidget {}
                set _dragging false
            }

            start {
                set w [lindex $args 0]
                set x [lindex $args 1]
                set y [lindex $args 2]

                foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
                set row [$tbl containing $y] 
                if {$row < 0} {
                    set _dragging false
                    return
                }
                
                set pnbody $itk_component(pnlist).body
                set hbody $itk_component(thier).body
                if {$w == "hbody"} {
                    if {[regexp {^<} $name]} {
                        set _dragging false
                        return
                    }
                }

                $w configure -cursor hand2

                set _dragging true
                set _lastwidget $w
                set _dragwidget $w
                set _dragname [$tbl getcells $row,name]
                set _dragdir [$tbl getcells $row,dir]
                set _dragrow $row
                set _dragsize [$tbl getcells $row,size]
            }

            motion {
                if {!$_dragging} {return}

                set x [lindex $args 0]
                set y [lindex $args 1]
                set w [winfo containing $x $y]
                if {$w != $_lastwidget && [winfo exists $_lastwidget]} {
                    event generate $_lastwidget <<DragLeave>>
                }
                $w configure -cursor hand2 

                set _lastwidget $w
                if {[winfo exists $w]} {
                    event generate $w <<DragOver>>
                }

                if {$w == $itk_component(thier)} {
                    $_dragwidget configure -cursor hand2
                } else {
                    $_dragwidget configure -cursor hand2
                }
            }
            
            stop {
                if {!$_dragging} {return}
                set X [lindex $args 0]
                set Y [lindex $args 1]
                set w [winfo containing $X $Y]
                set x [lindex $args 2]
                set y [lindex $args 3]
                if {[winfo exists $w]} {
                    event generate $w <<DragLeave>>
                    event generate $w <<DragDrop>> -x $x -y $y
                }
                foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
                set row [$tbl containing $y]
                set _dragging false   
                $_dragwidget configure -cursor ""
            }

            leave { 
                if {!$_dragging} {return} 
                set w [lindex $args 0]
                set tbl $itk_component(thier)
                $w configure -cursor {}
            }            
            
            drop {
                set w [lindex $args 0]
                set x [lindex $args 1]
                set y [lindex $args 2]

                if {$w == $_dragwidget} {
                    return
                }
 
                set tb $itk_component(thier).body
                set pn $itk_component(pnlist).body

                if {$w == $tb} {
                    dropHier $w $x $y
                } elseif {$w == $pn} {
                    moveSelectLeft $_dragrow
                } 
            }    
        }
    }

    method dropHier {w x y} {
        foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
        set row [$tbl containing $y] 
        
        if {$row < 0} {
            return
        }
                
        set name [$itk_component(thier) getcells $row,name]
        set dir [$itk_component(thier) getcells $row,dir]
        set type [$itk_component(thier) getcells $row,type]
        
        if {![regexp {^<} $name]} {
            moveLeft $row
            #return
        } 
        if {[regexp {Clock} $name]} {
            if {$_dragdir != "input"} {
                tk_messageBox -title "Error" -icon error \
                    -message "Clock pin must be an input"
                -parent $itk_interior -type ok
                return
            } elseif {$_dragsize > 1} {
                tk_messageBox -title "Error" -icon error \
                    -message "Clock pin must be 1 bit" \
                    -parent $itk_interior -type ok
                return
            } else {
                $itk_component(thier) configcells $row,name -text $_dragname
                $itk_component(thier) configcells $row,size -text $_dragsize
                $itk_component(thier) configcells $row,type -text "Clock"
                $itk_component(thier) configcells $row,dir -text $_dragdir
                $itk_component(thier) configrows  $row -fg black
                set item {}
                lappend item "<Drag Reset Pin from Unconnected Pins>" 
                set _newrow [$itk_component(thier) insertchild 0 end $item]
                $itk_component(thier) configrows $_newrow -fg red
            }
        } elseif { [regexp {Reset} $name]} {
            if {$_dragdir != "input"} {
                tk_messageBox -title "Error" -icon error \
                    -message "Reset pin must be an input" \
                    -parent $itk_interior -type ok
                return
            } elseif {$_dragsize > 1} {
                tk_messageBox -title "Error" -icon error \
                    -message "Reset pin must be 1 bit" \
                    -parent $itk_interior -type ok
                return
            } else {
                $itk_component(thier) configcells $row,name -text $_dragname
                $itk_component(thier) configcells $row,size -text $_dragsize
                $itk_component(thier) configcells $row,type -text "Reset"
                $itk_component(thier) configcells $row,dir -text $_dragdir
                $itk_component(thier) configrows $row -fg black
                if {[checkAddRow]} {
                    set item {}
                    lappend item "<Drag pin for Interface>" 
                    set newrow [$itk_component(thier) insertchild 0 end $item]
                    $itk_component(thier) configrows $newrow -fg gray55
                }
            }
        } elseif { [regexp {Interface} $name]} {
            #set type "Interface"
            if {[addIfc $row $_dragname $_dragdir] == 0} {
                $itk_component(thier) cellselection clear anchor end
                $itk_component(thier) finishediting
                return
            }   
            $itk_component(thier) configrows $row -fg black
            
        } elseif [checkDir $type $_dragdir] {
            return
        } else {
            if {$type == "DATA"} {
                set prow [$itk_component(thier) parentkey $row]
                set ptype [$itk_component(thier) getcells $prow,type]
                set pdir [$itk_component(thier) getcells $prow,dir]
                #$itk_component(thier) configcells $prow,size -text $_dragsize
                
                if {$ptype == "Handshake"} {
                    if {$pdir == "" || $pdir == $_dragdir} {
                        $itk_component(thier) configcells $prow,dir -text $_dragdir
                        lappend item "<Add Data>" DATA ""
                        set newrow [$itk_component(thier) insertchild $prow end $item]
                        $itk_component(thier) configrows $newrow -fg gray55
                    } else {
                        tk_messageBox -title "Error" -icon error \
                            -message "Data pin must match interface direction" \
                            -parent $itk_interior -type ok
                        return
                    }
                }
            } elseif {$type == "EN" || $type == "RDY"} {
                if {$_dragsize > 1} {
                    tk_messageBox -title "Error" -icon error \
                        -message "$type size cannot be greater than 1" \
                        -parent $itk_interior -type ok
                    return
                }
            }
            
            $itk_component(thier) configcells $row,name -text $_dragname
            $itk_component(thier) configcells $row,size -text $_dragsize
            $itk_component(thier) configcells $row,type -text $type
            $itk_component(thier) configcells $row,dir -text $_dragdir
            $itk_component(thier) configrows $row -fg black
        }   
        $itk_component(thier) cellselection clear anchor end
        $itk_component(pnlist) delete $_dragrow
        $itk_component(pnlist) sortbycolumn 1
        $itk_component(thier) finishediting
        $itk_component(thier) yview moveto 1
        checkAllDone
    }
    
    method checkAllDone {} {
        #puts "checkAllDone"
        set drow ""
        if {[$itk_component(pnlist) size] == 0} {
            set tsize [$itk_component(thier) size]
            for {set row 0} {$row < $tsize} {incr row} {
                set name [$itk_component(thier) getcells $row,name]
         #       puts "row $row name $name"
                if {[regexp {^<} $name]} {
                    if {[$itk_component(thier) rowcget $row -fg] == "gray55"} {
          #              puts "drop name $name"
                        lappend drow $row
                    }
                }
            }
            $itk_component(thier) delete $drow
        }
    }      
    
    method loadTempPins {module} {
        set _Pinfile $module.pin
        set _TempPinfile $module.pin-temp
        if {[file exists $_TempPinfile]} {
            readFile $_TempPinfile
        }
    }

    method readFile { filename } {
        set _transload false        
        set tb $itk_component(thier)
        set pn $itk_component(pnlist) 
        $tb delete 0 end
        $pn delete 0 end
        set pinfile [open $filename "r+"]
        set prow 0
        while {[gets $pinfile line] >= 0} {
            if {[regexp {^//unassigned} $line]} {
                set side pn
            } elseif {[regexp {^//complete} $line]} {
                set _transload true
                break
            }               
            if {[regexp {^//} $line]} {continue}
            if {[regexp {^port} $line]} {continue}
            if {[regexp {^endport} $line]} {continue}
            if {[regexp {^memory} $line]} {continue}
            if {[regexp {^endmemory} $line]} {continue}
            if {$line == ""} {continue}
            if {[regexp {clock:} $line]} {
                set _transload true
            }
            set name [lindex $line 1]
            set dir [lindex $line 0]
            set size [lindex $line 2]
            set item {}
            lappend item $dir $name $size
            $pn insert 0 $item
        }
        if {$_transload} {
            loadTran $pinfile
        } else {
            buildStart
            $pn sortbycolumn 1
        }
        
    }


    method loadTran {pinfile} {
        set tbl $itk_component(thier)
        #set pinfile [open $_Pinfile "r+"]
        set pnode 0
        set mname ""
        while {[gets $pinfile line] >= 0} {
            if {[regexp {^//} $line]} {continue}
            if {[regexp {^port} $line]} {continue}
            if {[regexp {^endport} $line]} {continue} 
            if {[regexp {^endmemory} $line]} {continue}
            if {[regexp {^  } $line]} {continue}
            if {$line == ""} {continue}
            set tran [lindex $line 6]
            set type [lindex $line 4]
            set name [lindex $line 1]
            set dir [lindex $line 0]
            set size [lindex $line 2]
            set trantype [lindex $line 5]
            set clock [lindex $line 3]
            if {[regexp {clock:} $line]} {
                set item {}
                lappend item $name Clock $dir $size
                set type Clock
                set pnode [$tbl insertchild root end $item]
                continue
            } elseif {[regexp {reset:} $line]} {
                set type Reset
                #set item {}
                #lappend item $name Reset $dir $size
                #$tbl insertchild $pnode end $item
            } elseif {$type == "" & $clock == "CLK"} {
                set type Port
                set pnode 0
            } elseif {$type == "PIPE" & $clock == "CLK"} {
                set type Port
                set pnode 0
            } elseif {[regexp {^memory} $line]} {
                if {[gets $pinfile line] >= 0} {
                    if {[regexp {address=} $line]} {
                        set awidth [lindex [split [lindex $line 1] =] 1]
                        set dwidth [lindex [split [lindex $line 2] =] 1]
                        set msize [concat $awidth $dwidth]
                        set memsize [join $msize ,]
                        set mnode [$tbl searchcolumn type Memory -descend]
                        $tbl configcells $mnode,size -text $memsize
                        $tbl configcells $mnode,name -editable 1
                        continue
                    } else {
                        continue
                    }
                }
            } elseif {$trantype == "GET" || $trantype == "PUT"} {
                set ttype "Handshake"
                #set tsize [$tbl size]
                set pnode [$tbl searchcolumn name $tran -descend ]
                if {$pnode == -1} {
                    if {$trantype == "GET"} {
                        set trandir output
                    } else {
                        set trandir input
                    }
                    set item {}
                    lappend item $tran $ttype $trandir
                    set pnode [$tbl insertchild 0 end $item]

                } 
            } elseif {$trantype == "PIPEGET" || $trantype == "PIPEPUT"} {
                set ttype "Handshake"
                #set tsize [$tbl size]
                set pnode [$tbl searchcolumn name $tran -descend ]
                if {$pnode == -1} {
                    if {$trantype == "PIPEGET"} {
                        set trandir output
                    } else {
                        set trandir input
                    }
                    set item {}
                    lappend item $tran $ttype $trandir
                    set pnode [$tbl insertchild 0 end $item]
                    $tbl configcells $pnode,name -editable 1
                } 
            } elseif {$trantype == "MEMREQ" || $trantype == "MEMRESP"} {
                set node [$tbl searchcolumn name $tran -descend ]
                if {$node == -1} {
                    set item {}
                    lappend item $tran Memory 
                    set mnode [$tbl insertchild 0 end $item]
                    set item {}
                    lappend item MEMREQ Request output
                    set mreq [$tbl insertchild $mnode end $item]
                    set item {}
                    lappend item MEMRESP Response input
                    set mres [$tbl insertchild $mnode end $item]
                } 
                if {$trantype == "MEMREQ"} {
                    set pnode $mreq
                } elseif {$trantype == "MEMRESP"} {
                    set pnode $mres
                }
            }               
            set item {}
            lappend item $name $type  $dir $size
            set newrow [$tbl insertchild $pnode end $item]
            if {[regexp {^<} $name]} {
                if {$size == "red"} {
                    $tbl configrows $newrow -fg red
                } else {
                    $tbl configrows $newrow -fg gray55
                }
            }
        }
        # set item {}
        # lappend item "<Drag pin for Interface>" 
        # set newrow [$itk_component(thier) insertchild 0 end $item]
        # $itk_component(thier) configrows $newrow -fg gray55   
        
    }

    method buildStart {} {
        set tbl $itk_component(thier)
        $tbl delete 0 end
        set item {}
        lappend item   "<Drag Clock Pin from Unconnected Pins>" 
        set newrow [$tbl insertchild root 0 $item]
        $tbl configrows $newrow -fg red
    }
    
    # method addClock {} {
    #     set item {}
    #     lappend item "<Drag pin for next clock>" 
    #     $itk_component(thier) insertchild root end $item
    # }

    method addIfc {row name dir} {
        set tb $itk_component(thier)
#        set pn $itk_component(pnlist)
        set mem [$tb searchcolumn type Memory -descend]
        $itk_component(popUpEntry) setChoice $mem
        if {[$itk_component(popUpEntry) activate] == 0} {
            return 0
        }
        set type [$itk_component(popUpEntry) getSelection]
        switch $type {
            RDY/EN {
                $tb configcells $row,type -text "Handshake"
                $tb configcells $row,name -editable 1
                if {$_dragsize > 1} {
                    set ifcname $_dragname
                    set ifctype DATA
                } else {
                    $itk_component(popUpPin) sendData $dir $name
                    if {[$itk_component(popUpPin) activate] == 0} {
                        return 0
                    }
                    set ifctype [$itk_component(popUpPin) getSelection]
                    set ifcname [$itk_component(popUpPin) getName]
                }
                $tb configcells $row,name -text $ifcname
                if {$ifctype == "DATA"} {
                    $tb configcells $row,dir -text $dir
                }
                addrdyenifc $row $name $dir $ifctype
            } 
            Memory {
                $tb configcells $row,type -text "Memory"
                $tb configcells $row,name -editable 1
                $itk_component(popUpMem) sendData $dir $name
                if {[$itk_component(popUpMem) activate] == 0} {
                    return 0
                }
                set values [$itk_component(popUpMem) getValues]
                addmemory $row $values
            }
            Port {
                $tb delete $row
                set pitem {}
                lappend pitem $_dragname $type $_dragdir $_dragsize
                $tb insertchild 0 end $pitem
            }
            Lock {
                moveAll LOCKSTEP
                return 0
            }
        }
        lappend item "<Drag pin for new Interface>" 
        set newrow [$itk_component(thier) insertchild 0 end $item]
        $tb configrows $newrow -fg gray55
        return 1
    } 


    method addrdyenifc {pnode pname pdir ifctype} {
        set dirError false
        set tbl $itk_component(thier)
        foreach ifc $rdyen {
            set size ""
            set dir [lindex $ifc 2]
            set name [lindex $ifc 0]
            set type [lindex $ifc 1]
            if {$ifctype == $type} {
                set dirError [checkDir $type $dir]
                if {$dirError} {return}
                set name $_dragname
                set size $_dragsize
                set dir $_dragdir
                if {$ifctype == "DATA"} {
                    set item {}
                    lappend item "<Add Data>" DATA ""
                    set newrow [$itk_component(thier) insertchild $pnode end $item]
                    $itk_component(thier) configrows $newrow -fg gray55
                }
            }                 
            set item {}
            lappend item $name $type $dir $size
            set newrow [$tbl insertchild $pnode end $item]
            if {$ifctype != $type} {
                $tbl configrows $newrow -fg red
            }
        }
    }       

    

    method addmemory {row values} {
        set tb $itk_component(thier)
        $tb configcells $row,name -text [lindex $values 0]
        $tb configcells $row,size -text [lindex $values 3]
        
        #set pnode [insertNext $row Memory]
        addMemChild $row $memory blank $values
        set children [$tb childkeys $row]
        foreach child $children {
            set ifc [$tb getcell $child,type]
            if {$ifc == "Request"} {
                set ifc $Request
                set ifctype Request
                $tb configcells $child,name -text "MEMREQ"
                $tb configrows $child -fg black                
            } else {
                set ifc $Response
                set ifctype Response
                $tb configcells $child,name -text "MEMRESP"
                $tb configrows $child -fg black                
            }
            addMemChild $child $ifc $ifctype $values
        }
    }

    method addMemChild {pnode ifc ifctype values} {
        set tbl $itk_component(thier)
        foreach tran $ifc {
            set size ""
            set dir [lindex $tran 2]
            set type [lindex $tran 1]
            set name [lindex $tran 0]
                        
            if {$ifctype == [lindex $values 1] && $type == [lindex $values 2]} {
                #set name [lindex $values 2]
                set dirError [checkDir $type $_dragdir]
                if {$dirError} {return}
                set name $_dragname
                set size $_dragsize
            }
            set item {}
            lappend item $name $type $dir $size
            set newrow [$tbl insertchild $pnode end $item]
            if {$ifctype != [lindex $values 1]} {
                $tbl configrows $newrow -fg red
            }
            if {$ifctype == [lindex $values 1] && $type != [lindex $values 2]} {
                $tbl configrows $newrow -fg red
            }
        }
    }

    method checkDir {type dir} {
        set dirError false
        if {$type == "EN"} {
            if {$dir == "output"} {
                set dirError true
            }
        } elseif {$type == "RDY"} {
            if {$dir == "input"} {
                set dirError true
            }
        }
        if {$dirError} {
            tk_messageBox -title "Error" -icon error \
                -message "Direction of pin must match direction of Interface"\
                -parent $itk_interior -type ok
        }
        return $dirError
    }      


    # method insertNext {pnode type} {
    #     set tbl $itk_component(thier)
    #     #set clocknode [$tbl parentkey $pnode]
    #     set item {}
    #     if {$type == "RDY/EN"} {
    #         lappend item "<Add RDY/EN>" "RDY/EN" ""
    #     } elseif {$type == "Port"} {
    #         lappend item "<Add Port>" "Port" ""
    #     } elseif {$type == "Clock"} {
    #         lappend item "<Add Clock>" "Clock" input
    #         set clocknode root
    #     } elseif {$type == "Memory"} {
    #         lappend item "<Add Memory>" "<Memory>" ""
    #     }
    #     set row [$tbl insertchild $pnode end $item]
    #     return $row
    # }


    method moveAll {type} {
        set tb $itk_component(thier)
        set pn $itk_component(pnlist)
        if {[$tb searchcolumn type Clock -descend] < 0 || [$tb searchcolumn type Reset -descend] < 0} {
            tk_messageBox -title "Error" -icon error \
                -message "Clock and Reset must be defined before Interfaces" \
                -parent $itk_interior -type ok
            return
        }
        if {$type == "LOCKSTEP"} {
            if {![checkAllLock]} {
                return
            }
        }
        set psize [$pn size]
        set pln ""
        for {set row 0} {$row < $psize} {incr row} {
            set name [$pn getcells $row,name]
            set dir [$pn getcells $row,dir]
            set size [$pn getcells $row,size]
            set item {}
            lappend item $name $type $dir $size 
            $tb insertchild 0 end $item            
        }
        $pn delete 0 end

        checkAllDone
    }


    # method checkifAllLock {} {
    #     set move True
    #     set tb $itk_component(thier)
    #     set rows [$tb size] 
    #     if {$rows > 3} {
    #         set answer [tk_messageBox -title "Too many interface types" -icon info \
    #            -message "All interfaces must be LOCKSTEP. " \
    #            -parent $itk_interior -type ok ]
    #         set move False
    #     }
    #     return $move
    # }

    method checkAllLock {} {
        set move True
        set tb $itk_component(thier)
        set rows [$tb size] 
        if {$rows > 3} {
            set answer [tk_messageBox -title "Change All Interfaces" -icon question \
               -message "All interfaces will change to LOCKSTEP.  Continue?" \
               -parent $itk_interior -type yesno ]
            if {$answer == "yes"} {
                set row 2
                set Done False
                while {!$Done} {
                    set name [$tb getcells $row,name]
                    if {![regexp {^<} $name]} {   
                        set type [$tb getcells $row,type]
                        if {$type != ""} {
                            set pkey [$tb parentkey $row]  
                            moveSelectType $type $pkey $row
                        } else {
                            set Done True
                        }
                    } else {
                        set Done True
                    }
                }
            } else {
                set move False
            }        
        }
        return $move
    }

    method finishPins {} {
        $itk_component(thier) finishediting
        
        if {[verify]} {
            savePinList
        } else {
            return
        }
        .view checkCanBuild

        [itcl::code $this deactivate 1]
    }

    method verify {} {
        set _Complete true
        verifyPins
        if {$_Complete == "true"} {
            verifyIfc
        }
        if {$_Complete != "true"} {
            set answer [tk_messageBox -title "Save" -icon error \
                            -message "The pin file is not complete. Do you still want to save?" \
                            -parent $itk_interior -type yesno]
            if {$answer == "no"} {
                return False
            }
        }
        return True
    }

    method verifyAll {} {
        set _Complete true
        set msg "Pin Interface is Complete"
        verifyPins
        if {$_Complete == "true"} {
            verifyIfc
        }
        if {$_Complete != "true"} {
            if {$_Complete == "ifc"} {
                set msg "Incomplete: All red interfaces must be completed"
            } elseif {$_Complete == "pin"} {
                set msg "Incomplete: All unconnected pins must be assigned"
            }
        }
        verifyMsg $msg
    }

    method verifyMsg {msg} {
        tk_messageBox -title "Results" -icon info \
            -message $msg -parent $itk_interior 
    }

    method verifyIfc {} {
        set tb $itk_component(thier)
        #check that no red interfaces exist
        set size [$tb size]
        for {set row 0} {$row < $size} {incr row} {
            set name [$tb getcells $row,name]
            if {[regexp {^<} $name]} {
                set color [$tb rowcget $row -fg]
                if {$color == "red"} {
                    set _Complete ifc
                    return
                }
            }
        }
    }
    
    method verifyPins {} {
        set pnlist $itk_component(pnlist)
        set psize [$pnlist size]
        if {$psize > 0} {
            set _Complete pin
        }
    }

    # method verifyReset {} {
    #     set tb $itk_component(thier) 
    #     set isize [$tb size]
    #     for {set row 0} {$row < $isize} {incr row} {
    #         if {[$tb getcells $row,name] == "<Add Reset>"} {
    #             tk_messageBox -title "Error" -icon error \
    #                 -message "Each clock must have a Reset defined" \
    #                 -parent $itk_interior -type ok
    #             set _Verify false
    #             return
    #         }
    #     }
    # }

    method savePinList {} {
        set pn $itk_component(pnlist)
        set psize [$itk_component(pnlist) size]
        file delete -force $_Pinfile
        set fh [open $_Pinfile "w"]
        set pln ""
       
        if {$psize > 0} {
            append pln "//unassigned" \n
            for {set row 0} {$row < $psize} {incr row} {
                set ln ""
                set name [$pn getcells $row,name]
                set dir [$pn getcells $row,dir]
                set size [$pn getcells $row,size]
                lappend ln $dir $name $size CLK
                append pln $ln \n
            }
            puts $fh $pln
        }        
        set size [$itk_component(thier) size]
        if {$size > 1} {
            saveIfc $fh
        }
        close $fh   
    }

    method saveIfc {fh} {
        set _memory false
        set memname ""
        if {$_pipes} {
            set pipe "PIPE"
        } else {
            set pipe ""
        }
        
      #  file delete -force $_Pinfile
      #  set fh [open $_Pinfile "w"]
        set pln ""
        append pln "//complete" \n
        append pln "port" \n
        set tb $itk_component(thier)
        set tsize [$tb size]
        #puts "tsize $tsize"
        # for {set row 0} {$row < $tsize} {incr row} {
        #     set name [$tb getcells $row,name]
        #     #puts "row $row $name"
        # }
        for {set row 0} {$row < $tsize} {incr row} {
            set ln ""
            set type blank
            set type [$tb getcells $row,type]
            set name [$tb getcells $row,name]
            set size [$tb getcells $row,size]
            set dir [$tb getcells $row,dir]
            set pclock CLK
            #puts "row $row name $name type $type"
            if {$type == "Clock"} {
                lappend ln input $name $size "clock:CLK"
                append pln $ln \n
            } elseif {$type == "Reset"} {
                lappend ln input $name $size "reset:RST_N"
                append pln $ln \n
            } elseif {$type == "Port"} {
                #puts "type port"
                set dir [$tb getcells $row,dir]
                lappend ln $dir $name $size $pclock $pipe
                append pln $ln \n
            } elseif {$type == "LOCKSTEP"} {
                set dir [$tb getcells $row,dir]
                lappend ln $dir $name $size $pclock LOCKSTEP
                append pln $ln \n
            } elseif {$type == "Handshake"} {
                #puts "type Handshake"
                set ifcname [$tb getcells $row,name]
                set dir [$tb getcells $row,dir]
                if {$dir == "input"} {
                    if {$_pipes} {
                        set trantype "PIPEPUT"
                    } else {
                        set trantype "PUT"
                    }
                } else {
                    if {$_pipes} {
                        set trantype "PIPEGET"
                    } else {
                        set trantype "GET"
                    }
                }
                #puts "ifcname $ifcname dir $dir trantype $trantype"
            } elseif {$type == "Memory"} {
                #puts "type memory"
                set _memory true
                set memname $name
                set ifcname $name
                set memsize $size
            } elseif {$type == "Request" || $type == "Response"} {
               # puts "type $type REQ or RES"
                set trantype [$tb getcells $row,name]
            } elseif {[regexp {^Drag Reset} $name]} {
                lappend ln $dir $name red $pclock
                append pln $ln \n
            } elseif {[regexp {^<Drag} $name]} {
                #puts "$name size [$itk_component(pnlist) size]"
                if {[$itk_component(pnlist) size] > 0 } {
                 #   puts "size > 0"
                    lappend ln $dir $name $size $pclock
                    append pln $ln \n
                }
            } elseif {[regexp {^<} $name]} {
               # puts "$name"
                set color [$tb rowcget $row -fg]
                lappend ln $dir $name $color $pclock $type $trantype $ifcname
                append pln $ln \n
            } else  {
              #  puts "type DATA, RDY, EN $type"
                lappend ln $dir $name $size $pclock $type $trantype $ifcname 
                #puts "$ln"
                append pln $ln \n
            }
        }
        append pln "endport" \n
        set ln ""
        if {$_memory} {
            lappend ln "memory"
            append pln $ln \n
            set ln ""
            set awidth [lindex [split $memsize ,] 0]
            set dwidth [lindex [split $memsize ,] 1]
            lappend ln $memname address=$awidth data=$dwidth
            append pln $ln \n
            set ln ""
            lappend ln endmemory
            append pln $ln \n
        }
        puts $fh $pln
#        close $fh
    }

    method getMem {} {
      #  puts "memory in semu_pin" $_memory"
        return $_memory
    }

    method configTb {} {
        set answer [tk_messageBox -title "Save?" -icon question \
-message "Do you want to save the Pin Interface Definition?"\
                        -type yesnocancel -parent $itk_interior]
        switch -- $answer {
            cancel {
                return
            }
            yes {
                finishPins
            }
        }
        [itcl::code $this deactivate 1]
        $itk_component(semu_build) fill_ctb
        $itk_component(semu_build) fill_stb
        $itk_component(semu_build) activate
    }


}

catch "itcl::delete class popUpEntry"
itcl::class popUpEntry {
    inherit iwidgets::Dialog
    constructor {args} {
        lappend args -modality application 

        eval itk_initialize $args
        wm minsize $itk_component(hull) 400 250
        wm title $itk_component(hull) "Specify Interface"
#        wm geometry $itk_component(hull) 0x0+400+300
               
        itk_component add pbox {
            iwidgets::radiobox $itk_interior.pbox  \
                -labeltext "Interface Type"
        } {}

        $itk_component(pbox) add Port -text "Port" 
        $itk_component(pbox) add RDY/EN -text "Handshake (Ready/Enable)" 
        $itk_component(pbox) add Memory -text "Memory" 
        $itk_component(pbox) add Lock -text "Lockstep"
        $itk_component(pbox) select 0

        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help

        pack $itk_component(pbox) -padx 10 -pady 10 -expand yes -fill both

    }

    proc create {pathname args} {
        uplevel #0 popUpEntry $pathname -modality application $args
    }


    method setChoice {mem} {
        #puts "setChoice $mem"
        if {$mem > -1} {
            [$itk_component(pbox) childsite].rb3 configure -state disabled
            $itk_component(pbox) select 0
        } else {
            [$itk_component(pbox) childsite].rb3 configure -state normal
        }
    }

    method getSelection {} {
       # puts "get Selection Entry"
        set ifc [$itk_component(pbox) get]
        return $ifc
    }
    
}   

catch "itcl::delete class popUpPin"
itcl::class popUpPin {
    inherit iwidgets::Dialog
    constructor {args} {
        lappend args -modality application 

        eval itk_initialize $args
        wm minsize $itk_component(hull) 400 250
        wm title $itk_component(hull) "Specify Pin Type"
#        wm geometry $itk_component(hull) 0x0+400+300
               
        itk_component add pinbox {
            iwidgets::radiobox $itk_interior.pinbox  \
                -labeltext "What type of pin is this?"
        } {}

        $itk_component(pinbox) add DATA -text "Data" 
        $itk_component(pinbox) add RDY -text "Ready"
        $itk_component(pinbox) add EN -text "Enable" 

        $itk_component(pinbox) select 0

        itk_component add ifcname {
            iwidgets::entryfield $itk_interior.ifcname -labeltext "Name of Interface" \
                -textvariable ifcname
            } {}

        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help

        pack $itk_component(ifcname) -padx 10 -pady 10 -expand yes -fill both
        pack $itk_component(pinbox) -padx 10 -pady 10 -expand yes -fill both
      
    }

    proc create {pathname args} {
        uplevel #0 popUpPin $pathname -modality application $args
    }

    method activate {} {
        after 500 focus -force $itk_component(ifcname)
        chain 
     }

     method sendData {dir name} {
         $itk_component(ifcname) clear
         $itk_component(ifcname) insert 0 $name
         if {$dir == "input"} {
             [$itk_component(pinbox) childsite].rb2 configure -state disabled
             [$itk_component(pinbox) childsite].rb3 configure -state normal
             $itk_component(pinbox) select 0
         } else {
             [$itk_component(pinbox) childsite].rb3 configure -state disabled
             [$itk_component(pinbox) childsite].rb2 configure -state normal
             $itk_component(pinbox) select 0
         }
     }


    method getSelection {} {
        set ifc [$itk_component(pinbox) get]
        return $ifc
    }
    
    method getName {} {
        set ifcname [$itk_component(ifcname) get]
        return $ifcname
    }
    
}

catch "itcl::delete class popUpMem"
itcl::class popUpMem {
    inherit iwidgets::Dialog

    variable _dir ""
    variable _name ""

    constructor {args} {
        lappend args -modality application 

        eval itk_initialize $args
        wm minsize $itk_component(hull) 600 550
        wm title $itk_component(hull) "Specify Memory Type"
#        wm geometry $itk_component(hull) 0x0+400+300

        itk_component add pframe {
            frame $itk_interior.pframe
        }
        
        itk_component add pinlabel {
            ttk::label $itk_component(pframe).pinlabel  \
                -text "Pin Name" -width 22
        } {}
        itk_component add pinname {
            ttk::label $itk_component(pframe).pinname -relief sunken  \
                -textvariable $_name -width 50
        } {}
        pack $itk_component(pinlabel) -side left
        pack $itk_component(pinname) -side left 

        itk_component add iframe {
            frame $itk_interior.iframe
        } {}
        itk_component add ifclabel {
            ttk::label $itk_component(iframe).ifclabel \
                -text "Request/Response" -width 22
        } {}
        itk_component add ifcname {
            ttk::label $itk_component(iframe).ifcname -relief sunken \
                -width 50
        } {}
        pack $itk_component(ifclabel) -side left
        pack $itk_component(ifcname) -side left 

        # itk_component add ifcname {
        #     iwidgets::entryfield $itk_interior.ifcname -labeltext "Request/Response" -textvariable $_name
        # } {}

        itk_component add mframe {
            frame $itk_interior.mframe
        } {}
        itk_component add awidth {
            iwidgets::entryfield $itk_component(mframe).awidth -labeltext "Address Width" \
                -width 5 
        } {}
        itk_component add dwidth {
            iwidgets::entryfield $itk_component(mframe).dwidth -labeltext "Data Width" \
                -width 5

        } {}
        pack $itk_component(awidth) -side left -padx 10 -pady 10
        pack $itk_component(dwidth) -side left -padx 10 -pady 10
                
        

        itk_component add membox {
            iwidgets::radiobox $itk_interior.membox  \
                -labeltext "Which memory component is this?" \
                -command "[itcl::code $this memcommand]"
        } {}
        $itk_component(membox) add Request -text "Request" 
        $itk_component(membox) add Response -text "Response"
        
        #$itk_component(membox) select 0

        itk_component add pinbox {
            iwidgets::radiobox $itk_interior.pinbox  \
                -labeltext "What type of pin is this?"
        } {}

        $itk_component(pinbox) add DATA -text "Data" 
        $itk_component(pinbox) add RDY -text "Ready"
        $itk_component(pinbox) add EN -text "Enable" 

        $itk_component(pinbox) select 0

        itk_component add memname {
            iwidgets::entryfield $itk_interior.memname -labeltext "Memory Interface Name" -width 50
            } {}
       
        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help

        pack $itk_component(pframe) -padx 10 -pady 10  -fill both
        pack $itk_component(iframe) -padx 10 -pady 10  -fill both
        pack $itk_component(memname) -padx 10 -pady 10 -fill both
        pack $itk_component(mframe) -padx 10 -pady 10 -fill both 
        pack $itk_component(membox) -padx 10 -pady 10  -fill both
        pack $itk_component(pinbox) -padx 10 -pady 10  -fill both 
        pack forget $itk_component(pinbox)
        
        buttonconfigure OK -text "OK"  -command [itcl::code $this finishMem]

    }

    proc create {pathname args} {
        uplevel #0 popUpMem $pathname -modality application $args
    }

    method activate {} {
        #puts "activate mem"
        after 500 focus -force $itk_component(memname)
        pack forget $itk_component(pinbox)
        chain 
     }


    method finishMem {} {
        #puts "finish Mem"
        set valid [verifyMem]
        if {!$valid} {return}
        [itcl::code $this deactivate 1]
    }

    method verifyMem {} {
        set awidth [$itk_component(awidth) get] 
        set dwidth [$itk_component(dwidth) get]
        #puts "verify $awidth $dwidth"
        if {$awidth == "" || $dwidth == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "address width and data width must be specified"\
                -parent $itk_interior -type ok
            return false
        } else {
            return true
        }
    }


    method memcommand {} {
        pack $itk_component(pinbox) -padx 10 -pady 10 -expand yes -fill both 
        
        set memtype [$itk_component(membox) get]
        if {$_dir == "input"} {
            [$itk_component(pinbox) childsite].rb2 configure -state disabled
            [$itk_component(pinbox) childsite].rb3 configure -state normal
            if {$memtype == "Response"} {
                [$itk_component(pinbox) childsite].rb1 configure -state normal
                $itk_component(ifcname) configure -text "MEMRESP"
                $itk_component(pinbox) select 0
            } else {
                [$itk_component(pinbox) childsite].rb1 configure -state disabled
                $itk_component(ifcname) configure -text "MEMREQ"
                $itk_component(pinbox) select 2
            }
        } else {
            [$itk_component(pinbox) childsite].rb3 configure -state disabled
            [$itk_component(pinbox) childsite].rb2 configure -state normal
            if {$memtype == "Response"} {
                [$itk_component(pinbox) childsite].rb1 configure -state disabled
                $itk_component(ifcname) configure -text "MEMRESP"
                $itk_component(pinbox) select 1
            } else {
                [$itk_component(pinbox) childsite].rb1 configure -state normal
                $itk_component(ifcname) configure -text "MEMREQ"
                $itk_component(pinbox) select 0
            }
        }
    }
    
    method sendData {dir name} {
        #puts "Mem send Data"
        set _name $name
        set _dir $dir 
        $itk_component(pinname) configure -text "$name"
        $itk_component(memname) clear
        $itk_component(memname) insert 0 $name
    }

    method getValues {} {
        #puts "mem getValues"
        set values {}
        set ifc [$itk_component(pinbox) get]
        set memifc [$itk_component(membox) get]
        set memname [$itk_component(memname) get]
        set lsize [concat [$itk_component(awidth) get] [$itk_component(dwidth) get]]
        set memsize [join $lsize ,]
        lappend values $memname $memifc $ifc $memsize
        return $values
    }

}   
