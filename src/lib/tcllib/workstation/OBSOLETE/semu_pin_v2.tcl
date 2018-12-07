package require Waves 2.0
package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require Tablelist
lappend auto_path .

#package require BWidget

# option add *Tablelist*Checkbutton.background            white
# option add *Tablelist*Checkbutton.activeBackground      white
# option add *Tablelist*Entry.background                  white

# tablelist::addBWidgetEntry
# tablelist::addBWidgetSpinBox
# tablelist::addBWidgetComboBox


#tablelist::addIncrCombobox

# standard Bluespec colours and fonts
# fonts::set_colours
# fonts::initialize

catch "itcl::delete class adv_pin"
itcl::class adv_pin {
    inherit iwidgets::Dialog

    variable _Pinfile ""
    variable _Clock ""
    variable _Cnum 0
    variable _Tran
    variable _Tname
    variable _Verify
    variable _Transactors
    variable TN
    variable uid
    variable _Children
    variable _dragging 0
    variable _lastwidget {}
    variable _dragwidget
    variable _dragname
    variable _draginout
    variable _dragclock
    variable _bodyTagH
    variable _dragrow
    variable _moving false
    variable _contextrow -1
    variable _transload

    # common transactors [list \
    #                         {<PinName> Pin ""} \
    #                         {<GETname> GET "Data Out"} \
    #                         {<PUTName> PUT "Data In"} \
    #                        ]
    common transactors [list \
                            {<GETname> GET "Data Out"} \
                            {<PUTName> PUT "Data In"} \
                           ]
    common clocks [list \
                       {<Clock> clock input} \
                       {<Reset> reset input} \
                       ]


    common PUTtypes [list \
                        {rdypin RDY output} \
                        {enpin EN input}\
                        {datapin DATA input} \
                       ]

    common GETtypes [list \
                        {rdypin RDY output} \
                        {enpin EN input } \
                        {datapin DATA output} \
   
                    ]

    # common pintypes [list \
    #                      {datapin Data "" } \

    #                 ]


    # common checkimg [::fonts::get_imagechecked]
    # common uncheckimg [::fonts::get_image unchecked]



    constructor {args} {
        lappend -args -modality application 

        eval itk_initialize $args   
       
        array set _Tran [list]
        array set _Tname [list]
        array set _Transactors [list]
        array set TN [list]
        array set ret [list]

        wm title $itk_component(hull) "Pin Definition"
        wm minsize $itk_component(hull) 800 500

        $itk_component(hull) hide Help
        $itk_component(hull) hide Apply

        buttonconfigure OK -text "Save"  -command [itcl::code $this finishPins]
#        buttonconfigure Apply -text "Check Values" -command [itcl::code $this verifyPins]

        mkAdvPin
    }

    destructor {}

    method mkAdvPin {} {
        mkPaneWin
        mkPins
        mkTLists
    }

    method mkTLists {} {
        mkCMenu
        #buildTranList 0
        mkTranHier
        mkMenuBar
 
        pack $itk_component(mb) -fill x
        # $itk_component(ppw) fraction 60 40
        pack $itk_component(ppw) -fill both -expand true -side top
   }


    proc create {pathname args} { 
        uplevel #0 adv_pin  $pathname -modality application $args
    }

    private method mkMenuBar {} {
        itk_component add mb {
            iwidgets::menubar $itk_interior.mb -menubuttons {
                menubutton tran -text "Transactors" -underline 0 -menu {
                    options -tearoff false
                    command clk -label "Add Clock" -underline 4 \
                        -command "[itcl::code $this addClock]"
                    command get -label "Add GET" -underline 4 \
                        -command "[itcl::code $this addTran GET]"
                    command put -label "Add PUT" -underline 4 \
                        -command "[itcl::code $this addTran PUT]"
                    command data -label "Add Data" -underline 4 \
                        -command "[itcl::code $this addData menu]"
                    separator tran0
                    command delete -label "Delete Selected" -underline 0\
                        -command "[itcl::code $this delTran]"

                }
                menubutton view -text "View" -underline 0 -menu {
                    options -tearoff false
                    command expand -label "Expand All" -underline 0 \
                        -command "$itk_component(thier) expandall"
                        command collapse -label "Collapse All" -underline 0 \
                        -command "$itk_component(thier) collapseall"
                }
            }       
        }
    }
     
    private method mkPaneWin {} {
        itk_component add ppw {
            base::panedwindow $itk_interior.ppw -orient vertical 
        } 
    }
    
    method mkTranHier  {} {
        #puts "starting mkTranHier"
        $itk_component(ppw) add stw
        set tws [$itk_component(ppw) childsite stw]

        itk_component add sw {
            iwidgets::Labeledframe $tws.tw -labeltext "Transactors" -labelpos nw
        } {}
        
        set tw [$itk_component(sw) childsite]

        set vsb $tw.vsb
        set hsb $tw.hsb
        
        itk_component add thier {
            tablelist::tablelist $tw.thier \
                -columns {0 "Type" left 
                    0 "Name" left
                    0 "Direction" left } \
                -treecolumn 0  -treestyle plastique \
                -movablerows true -movablerows true -resizablecolumns true \
                -labelcommand tablelist::sortByColumn \
                -yscrollcommand [list $vsb set] -setgrid no -width 0 \
                -xscrollcommand [list $hsb set] -setgrid no -width 0 \
                -editendcommand [itcl::code $this editEndCmd] \
                -forceeditendcommand 1 \
        } {}

        set thier $itk_component(thier)            

        $thier columnconfigure 0 -name type -sortmode command -sortcommand "[itcl::code $this sortType]"
        $thier columnconfigure 1 -name name -editable yes
        $thier columnconfigure 2 -name inout

        scrollbar $vsb -orient vertical -command [list $tw.thier yview]
        scrollbar $hsb -orient horizontal -command [list $tw.thier xview]
        
        set menu $tw.menu
        menu $menu -tearoff no
        $menu add command -label "Display Children" \
            -command [list [itcl::code $this putChildrenOfSelWidget $thier]]

        set _bodyTagH [$thier bodytag]
        bind $_bodyTagH  <Double-1>  [itcl::code $this putChildrenOfSelWidget $thier]
        bind $_bodyTagH <ButtonPress-1> +[itcl::code $this movePin paste %W %x %y]
        bind $_bodyTagH <ButtonPress-3>  [itcl::code $this contextMenu $itk_component(cmb) %W %x %y %X %Y]

        # bind $_bodyTagH <ButtonPress-3> [itcl::code $this drag start %W %x %y]
        # bind $_bodyTagH <ButtonRelease-3> [itcl::code $this drag stop %W %x %y]
        # bind $_bodyTagH <<DragDrop>>  [itcl::code $this drag drop %W %x %y]
        # bind $_bodyTagH <<DragLeave>> [itcl::code $this drag leave %W %x %y]
       

        grid $thier -row 0 -rowspan 2 -column 0 -sticky news
        grid $vsb -row 0 -rowspan 2 -column 1 -sticky ns
        grid $hsb -row 2  -column 0 -sticky ew
        grid rowconfigure $tw 1 -weight 1
        grid columnconfigure $tw 0 -weight 1
        pack $itk_component(sw) -side top -expand yes -fill both
        #puts "end putTranHier before putChildrenStart"

        #putChildrenStart 
        #return $thier
    }

    
    method sortType {a b} {
        if {$a == $b} {
            return 0
        }
        if {$a == "GET" && $b == "PUT"} {
            return 1
        }
        if {$a == "PUT" && $b == "GET"} {
            return -1
        }
        return 0
    }


    method mkCMenu {} {

        itk_component add cmb {
            menu $itk_interior.cmb -tearoff 0
        } {}

        itk_component add dmb {
            menu $itk_interior.dmb -tearoff 0
        } {}

        $itk_component(cmb) add command -label "Add Pin" -underline 4 \
                        -command "[itcl::code $this addTran pin]"
        $itk_component(cmb) add command -label "Add GET" -underline 4 \
                        -command "[itcl::code $this addTran GET]"
        $itk_component(cmb) add command -label "Add PUT" -underline 5 \
                        -command "[itcl::code $this addTran PUT]"
        $itk_component(cmb) add command -label "Delete Clock" -underline 0 \
            -command "[itcl::code $this delData]"
 
        $itk_component(dmb) add command -label "Add Data" -underline 4 \
            -command [itcl::code $this addData context]

        $itk_component(dmb) add command -label "Delete Transactor" -underline 0 \
            -command [itcl::code $this delData]


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

        #set puid [$tbl rowattrib $_contextrow uid]
        #set puid [$tbl getcell $_contextrow,uid]
        #puts "contextMenu puid $puid"
        #set type $TN($puid,type)
        set type [$tbl getcell $puid,type]
        
        if {$type == "Clock/Reset"} {
            tk_popup $itk_component(cmb) $X $Y
        } elseif {$type == "Pin" || $type == "GET" || $type == "PUT"} {
            tk_popup $itk_component(dmb) $X $Y
        }
    }

    method addClock {} {
        set item {}
        #puts "in addclock"
        set _Clock Clock$_Cnum
        set row [expr [$itk_component(thier) size] + 1]
        lappend item "Clock/Reset" $_Clock Input
        $itk_component(thier) insertchild root $row $item
    }

    method delData {} {
        $itk_component(thier) delete $_contextrow
    }

    method addData {from} {
        set item {}
        if {$from == "menu"} {
            set _contextrow [$itk_component(thier) curselection]
            if {$_contextrow == ""} {
                tk_messageBox -title "Error" -icon error\
                    -message "A GET or PUT transactor must be selected" \
                    -parent $itk_interior
                return
            }
        }
        set ptype [$itk_component(thier) getcell $_contextrow,type]
        set dir ""
        if {$ptype == "GET"} {
            set dir output
        } elseif {$ptype == "PUT"} {
            set dir input
        } else {
            tk_messageBox -title "Error" -icon error\
                -message "A GET or PUT transactor must be selected" \
                -parent $itk_interior
            return
        }
        lappend item "DATA" "datapin" $dir
        $itk_component(thier) insertchild $_contextrow end $item
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
                              0 "Clock\nDomain" left }\
                -labelcommand tablelist::sortByColumn \
                -xscrollcommand [list $hsbp set] -yscrollcommand [list $vsbp set] \
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
 
        scrollbar $vsbp -orient vertical -command [list $pnframe.pnlist yview]
        scrollbar $hsbp -orient horizontal -command [list $pnframe.pnlist xview]
      
        set bodyTag [$pnlist bodytag]
        bind $bodyTag <ButtonPress-1> +[itcl::code $this movePin select  %W %x %y]
        
        grid $pnlist  -row 0 -rowspan 2 -column 0 -sticky news
        grid $vsbp -row 1 -column 1 -sticky ns
        grid $hsbp -row 2 -column 0 -sticky ew
        grid rowconfigure $pnframe 1 -weight 1
        grid columnconfigure $pnframe 0 -weight 1

        pack $itk_component(pnframe) -expand true -fill both -side top

    }

    method drag {cmd w x y} {
        switch $cmd {
            start {
                foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
                set row [$tbl containing $y] 
                puts "start clicked on cell [$tbl containingcell $x $y]"
                set _dragging 1
                set _lastwidget $w
                set _dragwidget $w
                set _dragname [$tbl getcells $row,name]
                set _draginout [$tbl getcells $row,inout]
                set _dragclock [$tbl getcells $row,clock]
                set _dragrow $row
                puts "start row:$row name:$_dragname "       
            }

            motion {
                if {!$_dragging} {return}
                foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
                
                puts "motion tbl [$tbl containingcolumn $x] w $w"
                # if {$w != $_lastwidget} {
                #     event generate $_lastwidget <<DragLeave>>
                # }
                if {[$tbl containingcolumn $x] < 0} {
                    event generate $_lastwidget <<DragLeave>>
                }
                set _lastwidget $w
            }
            
            stop {
                if {!$_dragging} {return}
                
                # foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
                # set row [$tbl containing $y] 
                # puts "stop clicked on cell [$tbl containingcell $x $y]"
                
                set tbody $itk_component(thier).body
                puts "stop x:$x y:$y"
                foreach {tbl x y} [tablelist::convEventFields $tbody $x $y]  {}
                set row [$tbl containing $y] 
                puts "row stop $row "
                puts "value in row: [$tbl get $row]"
                
                # $thier configcells $row,name -text $_dragname
                # $itk_component(pnlist) delete $_dragrow
                # $itk_component(pnlist) insert $row ""
                # $itk_component(pnlist) configcells $row,inout -text $inout
                # $itk_component(pnlist) configcells $row,name -text  $name
                # $itk_component(pnlist) configcells $row,clock -text $clock
               set _dragging 0                
            }
            
            leave {
                if {!$_dragging} {return} 
                set tbl $itk_component(thier)
                
                foreach {w x y} [tablelist::convEventFields $tbl $x $y]  {}
                set row [$tbl containing $y] 
                puts "leave row $row"
            }
            
            drop {
                puts "drop clicked on cell [$tbl containingcell $x $y]"
                
            }
        }
    }    
    

    method movePin {cmd w x y} {
        switch $cmd {

           select {
               set _moving true
               foreach {tbl x y} [tablelist::convEventFields $w $x $y]  {}
               set row [$tbl containing $y] 
               if {$row < 0} {
                   return
               }
               #puts "value in start row: [$tbl get $row]"
               set _dragname [$tbl getcells $row,name]
               set _draginout [$tbl getcells $row,inout]
               set _dragclock [$tbl getcells $row,clock]
               set _dragrow $row
               #puts "start row:$row name:$_dragname " 
           }

           paste {
               if {!$_moving} {return}
               set tbody $itk_component(thier).body
               foreach {tbl x y} [tablelist::convEventFields $tbody $x $y]  {}
               set row [$tbl containing $y] 
               if {$row < 0} {
                   return
               }
               #puts "value in row $row: [$tbl get $row]"
               set _moving false
               set dir [$itk_component(thier) getcells $row,inout]
               set type [$itk_component(thier) getcells $row,type]
               
               if { $dir != $_draginout && $type != "pin"} {
                    tk_messageBox  -title "Direction Mismatch" -icon error\
                        -message "Direction of pin does not match direction of transactor" \
                        -parent $itk_interior
                    return                   
               } elseif  {$type == "pin"} {
                   $itk_component(thier) configcells $row,inout -text $_draginout
               }
               $itk_component(thier) configcells $row,name -text $_dragname
               $itk_component(thier) cellselection clear anchor end
               $itk_component(pnlist) delete $_dragrow
           }
       }
        $itk_component(thier) finishediting
    }

    
    method putChildrenOfSelWidget {tbl} {
        set w [$tbl rowattrib [$tbl curselection] pathname]
        putChildren  $tbl $w
    }


    method putChildren {nodeidx} {
        set tbl $itk_component(thier)
        #puts "in putChildren $nodeidx"
        if {$nodeidx == 0} {
            #puts "nodeidx 0"
            $tbl delete 0 end
            set row 0
            set item {}
            lappend item $TN($nodeidx,tag) $TN($nodeidx,name) $TN($nodeidx,dir) $nodeidx
            $tbl insertchild root 0 $item
        } else {
            set row [$tbl searchcolumn uid $nodeidx -exact -numeric -descend]
            #puts "row found $row"
            #set row [expr {$nodeidx + 1}]
        }

        set itemList {}
        foreach child $TN($nodeidx,child) {
            #puts "child $child"
            lappend itemList [list $TN($child,tag) $TN($child,name) $TN($child,dir) $child]
         }
        $tbl insertchildlist $row end $itemList
    }

    method startChildren {} {
        set node 0
        putChildren $node
        foreach child $TN($node,child) {
            #puts "startChildren putChildren $child"
            putChildren $child
        }
    }

    method addChildren {pnode type} {
        set tbl $itk_component(thier)
        #puts "type is $type"
        foreach tran $type {
            #puts "tran is $tran"
            set dir [lindex $tran 2]
            set tag [lindex $tran 1]
            set name [lindex $tran 0]
            set item {}
            lappend item $tag $name $dir 
            $tbl insertchild $pnode end $item
        }
    }
 
    method buildDefault {} {
        #puts "in BuildDefault"
        set tbl $itk_component(thier)
#       $tbl delete 0 end
#        set item {}
#        lappend item Clock/Reset $_Clock Input 0
#        $tbl insertchild root 0 $item 
        set pnode 0
        addChildren $pnode $transactors
        
        foreach tran $transactors {
            set type [lindex $tran 1]
            set pnode [$tbl searchcolumn type $type -descend]
            #puts "type $type pnode $pnode"
            set trantype [append type types]
            #puts "trantype $trantype"
            switch $trantype {
                pintypes {
                 #   addChildren $pnode $pintypes
                    continue
                }
                GETtypes {
                    #puts "GETtypes"
                    addChildren $pnode $GETtypes
                }
                PUTtypes {
                    #puts "PUTtypes"
                    addChildren $pnode $PUTtypes
                }
            }
        }
    }


    method delTran {} {
        set tbl $itk_component(thier)
        set srow [$tbl curselection]
        $tbl delete $srow
    }
    
    method addTran {type} {
        #puts "in addTran"
        set tbl $itk_component(thier)
        set srow [$tbl curselection]
        # when multiclocks are enabled, must select clock.
        #set _contextrow 0
        set _contextrow $srow
        set item [list]
        
        switch $type {
            Pin {
                lappend item Pin <PinName> ""
                set prow [$tbl insertchild $_contextrow end $item]
                #addChildren $prow $pintypes
            }
            GET {
                lappend item GET <GetName> "Data Out"
                set prow [$tbl insertchild $_contextrow end $item]
                addChildren $prow $GETtypes
            }
            PUT {
                lappend item PUT <PUTName> "Data In"
                set prow [$tbl insertchild $_contextrow end $item]
                addChildren $prow $PUTtypes
            }
        }
    }
            

    method editEndCmd {tbl row col text} {
        return $text
    }

    method editStartCmd {tbl row col text} {
        set w [$tbl editwinpath]

        switch [$tbl columncget $col -name] {
            name {
               $tbl configcells $row,$col  ""
            } 
        }
    }

    method gettran {} {return $transactors}
    method getgpt {} {return $gptypes} 
#    method getpin {} {return $pintypes}


    method loadPins {module clock reset} {
        set _Clock ""
        set _Clock [join [lappend _Clock $clock $reset] /]
        set _Pinfile $module.pin
        if {[file exists $_Pinfile]} {
            #puts "reading in file"
            readpnFile $clock $reset
        } else {
            tk_messageBox -title "Error" -icon error \
                -message "Pin file must exist. Select "generate" on the Configuration panel to create." \
                -parent $itk_interior -type ok
            return
        }
    }



    method readpnFile {clock reset} {
        $itk_component(pnlist) delete 0 end
        $itk_component(thier) delete 0 end
        set pinfile [open $_Pinfile "r+"]
        set row 0
        set _transload false
        set item {}
        lappend item Clock/Reset $_Clock Input 0
        $itk_component(thier) insertchild root 0 $item 
        while {[gets $pinfile line] >= 0} {
            if {[regexp {^//} $line]} {continue}
            if {[regexp {^port} $line]} {continue}
            if {[regexp {^endport} $line]} {continue}
            if {[lindex $line 4] == ""} {  
                if {[lindex $line 1] == $clock || [lindex $line 1] == $reset} {continue}
                $itk_component(pnlist) insert $row ""
                $itk_component(pnlist) configcells $row,inout -text [lindex $line 0]
                $itk_component(pnlist) configcells $row,name -text  [lindex $line 1]
                $itk_component(pnlist) configcells $row,clock -text  [lindex $line 2]
                incr row
            } else {
                set _transload true
                loadTran $row $line
            }                
        }
        if {!$_transload} {
            buildDefault
        }
    }

    method loadTran {row line} {
        set tbl $itk_component(thier)
        set clock [lindex $line 2]
        set tran [lindex $line 5]
        set type [lindex $line 3]
        set name [lindex $line 1]
        set dir [lindex $line 0]
        set trantype [lindex $line 4]
        set size [$tbl size]
        set pnode [$tbl searchcolumn name $tran -descend ]
        #puts "pnode $pnode name $tran"
        if {$pnode == -1} {
            set clocklist [$tbl searchcolumn type "Clock/Reset" -all]
            #puts "clocklist $clocklist"
            foreach crow $clocklist {
                set fullclockname [$tbl getcells $crow,name]
                set clockname [lindex [split $fullclockname /] 0]
                #puts "fullclock $fullclockname clockname $clockname clock $clock"
                if {$clockname == $clock} {
                    set item {}
                    lappend item $trantype $tran $dir
                    #puts "crow $crow $item"
                    set pnode [$tbl insertchild $crow end $item]
                }
            }
        }            
        set item {}
        lappend item $type $name $dir
        $tbl insertchild $pnode end $item
    }
    
    

    method finishPins {} {
        # #verifyPins
        # if {!$_Verify} {
        #     return
        # }
        $itk_component(thier) finishediting
        savePins
        
        [itcl::code $this deactivate 1]
    }

    # method verifyPins {} {
    #     set _Verify true
    #     set tbl $itk_component(thier)
    #     $tbl finishediting
    #     set size [$tbl size]
    #     for {set row 0} {$row < $size} {incr row} {
    #         set name [$tbl getcells $row,name]
    #     }
    # }

    method verifyPins {} {
        set _Verify true
        set plist $itk_component(plist)
        $plist finishediting
        array unset _Tran
        array unset _Tname
        set size [$itk_component(plist) size]
        #check for multiple EN or RDY signals
        for {set row 0} {$row < $size} {incr row} {
            set name [$plist getcells $row,6]
            set type [$plist getcells $row,4]
            set getput [$plist getcells $row,5]
            set pname [$plist getcells $row,1]  
            if {$name != ""} {
                if {$pname == $_Clock || $pname == $_Reset} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Clock/Reset cannot have a Transactor specification"  \
                        -parent $itk_interior -type ok
                    set _Verify false
                } elseif {$type == "" || $getput == ""} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Transactor must have Type and GET/PUT defined" \
                        -parent $itk_interior -type ok
                    set _Verify false
                } elseif {[info exists _Tran($name,$type)] } {
                    if {$type == "DATA"} {continue} 
                    tk_messageBox -title "Error" -icon error \
                        -message "Transactor $name can only have 1 $type" -type ok \
                        -parent $itk_interior
                    set _Verify false
                } else {
                    set _Tran($name,$type) $row
                    set _Tname($name) $row
                }
            } else {
                if {$type != "" || $getput != ""} {
                    tk_messageBox -title "Error" -icon error \
                        -message "Transactor name must be defined" \
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
       file delete -force $_Pinfile
       set fh [open $_Pinfile "w"]
       set pln ""
       append pln "port" \n
       set pn $itk_component(pnlist)
       set size [$pn size]
       for {set row 0} {$row < $size} {incr row} {
           set ln ""
           set dir [$pn getcells $row,inout]
           set name [$pn getcells $row,name]
           set clk [$pn getcells $row,clock]
           lappend ln $dir $name $clk
           append pln $ln \n
       }
       set tb $itk_component(thier)
       set size [$tb size]
       for {set row 0} {$row < $size} {incr row} {
           #puts "thier row $row"
           set ln ""
           set type [$tb getcells $row,type]
           if {$type == "Clock/Reset"} {
               set clockexp [$tb getcells $row,name]
               set pclock [lindex [split $clockexp /] 0]
               set reset [lindex [split $clockexp /] 1]
               lappend ln input $pclock $pclock 
               append pln $ln \n
               set ln ""
               lappend ln input $reset $pclock
               append pln $ln \n
           } elseif {$type == "Pin"} {
               set ln ""
               set name [$tb getcells $row,name]
               set dir [$tb getcells $row,inout]
               lappend ln $dir $name $pclock 
               append pln $ln \n
           } elseif {$type == "GET" } {
               set tran [$tb getcells $row,name]
               set trantype "GET"
           } elseif {$type == "PUT"} { 
               set tran [$tb getcells $row,name]
               set trantype "PUT"
           } else {
               set ln ""
               set name [$tb getcells $row,name]
               set dir [$tb getcells $row,inout]
               #puts "name $name dir $dir type $type"
               lappend ln $dir $name $pclock $type $trantype $tran 
               append pln $ln \n
           }
       }
       append pln "endport" \n
       puts $fh $pln
       close $fh
   }
}

        
   
  
 # private method configure_tranview {} {
    #     set h $itk_component(thier)
    #     $h component itemMenu add command -label "Name" \
    #         -command "$itk_interior action_on_node hier name"
    # }
    # method putChildrenStart {} {
    #     puts "putChildrenStart"
    #     set tbl $itk_component(thier)
    #     $tbl delete 0 end
    #     set row 0
    #     set prow 0
    #     set uid 0
    #     putChild $tbl 0 root $row
    #     set prow $row
    #     incr row
    #     set puid $uid 
    #     set children $TN($puid,child)
    #     foreach child $children {
    #         #puts "foreach child $child"
    #         set cuid $child
    #         set cprow [putChild $tbl $child $prow $row]
    #         incr row
    #         if { $TN($cuid,child)!= ""} {
    #             set children $TN($cuid,child)
    #             foreach child $children {
    #                 set nuid $child
    #                 putChild $tbl $nuid $cprow $row
    #                 incr row
    #             }
    #         }
    #     }
    # }
   
    # method putChild {tbl uid prow row} {
    #     #puts "putchild $uid $prow $row"
    #     set ret [list]
    #     set name  $TN($uid,name)
    #     set tag $TN($uid,tag)
    #     set dir $TN($uid,dir)
    #     lappend ret  $tag $name $dir $uid
    #     $tbl insertchild $prow end $ret
    #     #$tbl rowattrib $row uid $uid
    #     return $row
    # }


    # #buils the default transactor list 
    # method buildTranList {uid} {
    #     #puts "in BuildTranList"
    #     #set uid 0
    #     set children [list]
    #     # set first $uid  clock structure
    #     set TN($uid,name) $_Clock
    #     set TN($uid,tag) "Clock/Reset"
    #     set TN($uid,dir) "Input" 
    #     set puid $uid
    #     foreach tran $transactors {
    #         incr uid
    #         lappend children $uid
    #         set _Children [list]
    #         set puid $uid
    #         set dir [lindex $tran 2]
    #         set tag [lindex $tran 1]
    #         set TN($uid,name) [lindex $tran 0]
    #         set TN($uid,tag) [lindex $tran 1]
    #         set TN($uid,dir) [lindex $tran 2]
    #         if {$tag == "pin"} {
    #             set _Children [list]
    #             foreach type $pintypes {
    #                 incr uid
    #                 definePin $uid $type $dir 
    #             }             
    #             set TN($puid,child) $_Children
    #         } elseif {$tag == "put"} {
    #             set _Children [list]
    #             foreach type $putypes {
    #                 incr uid   
    #                 definePin $uid $type $dir 
    #             }
    #             set TN($puid,child) $_Children
    #         } elseif {$tag == "get"} {
    #             set _Children [list]
    #             foreach type $getypes {
    #                 incr uid   
    #                 definePin $uid $type $dir 
    #             }
    #             set TN($puid,child) $_Children
    #         }
    #         incr uid
    #     }
    #     set TN(0,child) $children
    #     incr _Cnum
    #     parray TN
    # }

    # method definePin {uid type dir } {
    #     #puts "definePin"
    #     set name [lindex $type 0]
    #     set TN($uid,name) $name
    #     set TN($uid,tag) [lindex $type 1]
    #     set TN($uid,dir) [lindex $type 2]
    #     lappend _Children $uid
    #     #set TN($uid,child) ""
    # }


    # method findHigh {} {
    #     set high 0
    #     foreach {key value} [array get TN "*,name"]  {
    #         set uid [lindex [split $key ","] 0]
    #         if {[expr $uid > $high] } {
    #             set high $uid
    #         }
    #     }
    #     set uid [expr $high + 1]
    #     return $uid
    # }
#     method addTran {type} {
#         set thier $itk_component(thier)    
#         set puid $_contextrow
# #        set puid [$thier getcell $_contextrow,uid]
# #        puts "addTran puid $puid"
# #        set _Children $TN($puid,child)
# #        set uid [findHigh]
# #        lappend _Children $uid
# #        set TN($puid,child) $_Children
# #        puts "addTran children $_Children"
#         set item {}
#         switch -exact $type {
#             pin {
#                 set _Children [list]
#                 set dir "Port"
#                 set TN($uid,name) "<PortName>"
#                 set TN($uid,tag) pin
#                 set TN($uid,dir) $dir
#                 insertRow $uid $row
#                 set puid $uid
#                 foreach type $pintypes {
#                     incr uid
#                     definePin $uid $type $dir
#                 }             
#             }
#             get {
#                 set _Children [list]
#                 set dir "Data Out"
#                 set TN($uid,name) "<GETName>"
#                 set TN($uid,tag) get
#                 set TN($uid,dir) "Data Out"
#                 insertRow $uid $row
#                 set puid $uid
#                 foreach type $getypes {
#                     incr uid
#                     definePin $uid $type $dir
#                 }
#             }
#             put {
#                 set _Children [list]
#                 set dir "Data In"
#                 set TN($uid,name) "<PUTName>"
#                 set TN($uid,tag) put
#                 set TN($uid,dir) $dir
#                 insertRow $uid $row
#                 set puid $uid
#                 foreach type $putypes {
#                     incr uid  
#                     definePin $uid $type $dir
#                 }
#             }
#         }
#         #puts "after switch"
#         set TN($puid,child) $_Children
#         parray TN
#         putChildren $puid
#         #putChildrenStart 
#     }
