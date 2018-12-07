package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist


# package require BWidget

# option add *Tablelist*Checkbutton.background            white
# option add *Tablelist*Checkbutton.activeBackground      white
# option add *Tablelist*Entry.background                  white

# tablelist::addBWidgetEntry
# tablelist::addBWidgetSpinBox
# tablelist::addBWidgetComboBox


tablelist::addIncrCombobox

# standard Bluespec colours and fonts
# fonts::set_colours
# fonts::initialize

catch "itcl::delete class semu_pin"
itcl::class semu_pin {
    inherit iwidgets::Dialog

    variable _Pinfile ""
    variable _Clock
    variable _Reset
    variable _Tran
    variable _Tname
    variable _Verify

    constructor {args} {
        lappend -args -modality application 

        eval itk_initialize $args   
       
        array set _Tran [list]
        array set _Tname [list]

        mkPinWindow
        #mkPaneWin
        #mkPins

        wm title $itk_component(hull) "Pin Definition"
        wm minsize $itk_component(hull) 600 400

        $itk_component(hull) hide Help
        buttonconfigure OK -text "Save" -underline 0 -command [itcl::code $this finishPins]
        buttonconfigure Apply -text "Check Values" -underline 0 -command [itcl::code $this verifyPins]
        buttonconfigure Cancel -underline 0
    }

    destructor {}

    proc create {pathname args} {
        uplevel #0 semu_pin $pathname -modality application $args
    }

    private method mkPaneWin {} {
        itk_component add ppw {
            base::panedwindow $itk_interior.ppw -orient vertical 
        } 

        $itk_component(ppw) add thier
        set tw [$itk_component(ppw) childsite thier]
        
        itk_component add thier {
            base::hierarchy $tw.thier \
                -labeltext "Interfaces" -labelpos nw\
            } {}

        pack $itk_component(thier) -fill both -expand true -side top
        pack $itk_component(ppw) -fill both -expand true -side top
    }

    private method mkHier {} {

    }

    private method mkPins {} {
        
        $itk_component(ppw) add pins
        set pw [$itk_component(ppw) childsite pins]

        itk_component add pnframe {
            iwidgets::Labeledframe $pw.pnframe -labeltext "Pins" -labelpos nw
        } {}
        set pnframe [$itk_component(pnframe) childsite]
        set vsb $pnframe.vsb
        set hsb $pnframe.hsb

        itk_component add pnlist {
            tablelist::tablelist $pnframe.pnlist \
                -columns {0 "Input/Output" left \
                              0 "Name" left \
                              0 "Clock\nDomain" left }\
                -labelcommand tablelist::sortByColumn \
                -sortcommand semu_pin::compareAsSet \
                -xscrollcommand [list $hsb set] -yscrollcommand [list $vsb set] \
                -height 15 -width 100 -stretch all \
                -background white -stripebg #e4e8ec \
                -movablecolumns true -movablerows true -resizablecolumns true \
                -editstartcommand [itcl::code $this editStartCmd] \
                -editendcommand [itcl::code $this editEndCmd] \
        } {}
        set pnlist $itk_component(pnlist)
        $pnlist columnconfigure 0 -name inout  
        $pnlist columnconfigure 1 -name name 
        $pnlist columnconfigure 2 -name clock -editable yes
 
        scrollbar $vsb -orient vertical -command [list $pnframe.pnlist yview]
        scrollbar $hsb -orient horizontal -command [list $pnframe.pnlist xview]
      
        grid $pnlist  -row 0 -rowspan 2 -column 0 -sticky news
        
        grid $vsb -row 1 -column 1 -sticky ns
        grid $hsb -row 2 -column 0 -sticky ew
        grid rowconfigure $pnframe 1 -weight 1
        grid columnconfigure $pnframe 0 -weight 1


        pack $itk_component(pnframe) -expand true -fill x -side top
    }

    private method mkPinWindow {} {
        itk_component add pframe {
            frame $itk_interior.pframe
        } {}

        set pframe $itk_component(pframe)

        set vsb $pframe.vsb
        set hsb $pframe.hsb

        itk_component add plist {
            tablelist::tablelist $pframe.plist \
                -columns {0 "Input/Output" left \
                              0 "Name" left \
                               0 "Clock\nDomain" left \
                               0 "Type" left \
                               0 "Get/\nPut" left \
                               0 "Interface\nName" left} \
                               -labelcommand tablelist::sortByColumn \
                 -sortcommand semu_pin::compareAsSet \
                 -xscrollcommand [list $hsb set] -yscrollcommand [list $vsb set] \
                 -height 15 -width 100 -stretch all \
                 -background white -stripebg #e4e8ec \
                 -movablecolumns true -movablerows true -resizablecolumns true \
                 -editstartcommand [itcl::code $this editStartCmd] \
                 -editendcommand [itcl::code $this editEndCmd] \
         } {}

         set plist $itk_component(plist)
         $plist columnconfigure 0 -name inout  
         $plist columnconfigure 1 -name name 
         $plist columnconfigure 2 -name clock -editable yes
         $plist columnconfigure 3 -name type -editable yes -editwindow combobox
         $plist columnconfigure 4 -name direction -editable yes -editwindow combobox 
         $plist columnconfigure 5 -name transact -editable yes 

         scrollbar $vsb -orient vertical -command [list $pframe.plist yview]
         scrollbar $hsb -orient horizontal -command [list $pframe.plist xview]

         grid $plist  -row 0 -rowspan 2 -column 0 -sticky news

         grid $vsb -row 1 -column 1 -sticky ns
         grid $hsb -row 2 -column 0 -sticky ew
         grid rowconfigure $pframe 1 -weight 1
         grid columnconfigure $pframe 0 -weight 1

         pack $itk_component(pframe) -expand true -fill x
     }

     method editEndCmd {tbl row col text} {
         return $text
     }

     method editStartCmd {tbl row col text} {
         set w [$tbl editwinpath]

         switch [$tbl columncget $col -name] {
             type {
                 $w insert list end RDY EN DATA
             }
             direction {
                 $w insert list end PUT GET
             }
         }
         return $text 
     }

     method loadPins {module clock_pin reset_pin} {
         set _Clock [lindex [split $clock_pin :] 1]
         set _Reset [lindex [split $reset_pin :] 1]
         set _Pinfile $module.pin
         if {[file exists $_Pinfile]} {
             readFile
         }
     }

     method readFile {} {
         $itk_component(plist) delete 0 end
         set pinfile [open $_Pinfile "r+"]
         set row 0
         while {[gets $pinfile line] >= 0} {
             if {[regexp {^//} $line]} {continue}
             if {[regexp {^port} $line]} {continue}
             if {[regexp {^endport} $line]} {continue}

             $itk_component(plist) insert $row ""
             $itk_component(plist) configcells $row,inout -text [lindex $line 0]
             $itk_component(plist) configcells $row,name -text  [lindex $line 1]
             $itk_component(plist) configcells $row,clock -text  [lindex $line 2]
             if {[lindex $line 3] != ""} { 
                 set transact [lindex $line 3]
                 $itk_component(plist) configcells $row,type -text [lindex $transact 0]
                 $itk_component(plist) configcells $row,direction -text [lindex $transact 1]
                 $itk_component(plist) configcells $row,transact -text [lindex $transact 2]
             }
             incr row
         }                
     }


     method finishPins {} {
         verifyPins
         if {!$_Verify} {
             return
         }
         savePins
         [itcl::code $this deactivate 1]
    }

    method verifyPins {} {
        set _Verify true
        set plist $itk_component(plist)
        $plist finishediting
        array unset _Tran
        array unset _Tname
        set size [$itk_component(plist) size]
        #check for multiple EN or RDY signals
        for {set row 0} {$row < $size} {incr row} {
            set name [$plist getcells $row,5]
            set type [$plist getcells $row,3]
            set getput [$plist getcells $row,4]
            set pname [$plist getcells $row,1]  
            if {$name != ""} {
                if {$pname == $_Clock || $pname == $_Reset} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Clock/Reset cannot have an Interface assignment"  \
                        -parent $itk_interior -type ok
                    set _Verify false
                } elseif {$type == "" || $getput == ""} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Interface must have Type and Get/Put defined" \
                        -parent $itk_interior -type ok
                    set _Verify false
                } elseif {[info exists _Tran($name,$type)] } {
                    if {$type == "DATA"} {continue} 
                    tk_messageBox -title "Error" -icon error \
                        -message "Interface $name can only have 1 $type" -type ok \
                        -parent $itk_interior
                    set _Verify false
                } else {
                    set _Tran($name,$type) $row
                    set _Tname($name) $row
                }
            } else {
                if {$type != "" || $getput != ""} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Interface name must be defined" \
                        -parent $itk_interior -type ok
                    set _Verify false
                }
            }
        }
        #check for at least 1 Data, RDY, and EN
        foreach tname [array names _Tname *] {
            foreach type {EN RDY DATA} {
                if {[info exists _Tran($tname,$type)] } {continue}
                tk_messageBox -title "Error" -icon error \
                    -message "$tname does not have $type" -type ok \
                    -parent $itk_interior
                    set _Verify false
            }
        }
        if {$_Verify} {
            tk_messageBox -title "Pin Definition" -icon info \
                -message "Validation complete.  No errors found." -type ok \
                -parent $itk_interior
        }
    }

   method savePins {}  {
       set plist $itk_component(plist)
       file delete -force $_Pinfile
       set fh [open $_Pinfile "w"]
       set pln ""
       append pln "port" \n
       set size [$itk_component(plist) size]
       for {set row 0} {$row < $size} {incr row} {
           set ln ""
           set ln2 ""
           for {set col 0} {$col < 3} {incr col} {
               lappend ln [$plist getcells $row,$col]
           }
           for {set col 3} {$col < 6} {incr col} { 
               lappend ln2 [$plist getcells $row,$col]
           }
           # for {set col 0} {$col < 6} {incr col} { 
           #     lappend ln [$plist getcells $row,$col]
           # }               
           lappend ln $ln2
           append pln $ln \n
       }
       append pln "endport" \n
       puts $fh $pln
       close $fh
   }
}

