package provide semu_advanced 1.0

#catch "itcl::delete class semu_advanced"
itcl::class semu_advanced {
    inherit iwidgets::Dialog

    common  _Initvals
    common ver_sim ""
    common advmode "config"
    common _ctb
    common _target
    common _type ""
    common _rel

    #array (field label WidgetType attr)
    common vfields [list \
                        verilog-lib-directories {Verilog Search Paths} BDir {} \
                        verilog-inc-directories {Verilog Include Directories} BDir {} \
                        bsc-link-options {Verilog Link Options} Entry {} \
                        verilog-define {Verilog Macro Definitions} Entry {} \
                       ]
    common ofields [list \
                        imported-vhdl-files {Imported VHDL Files} Browse {} \
                        xilinx-xdc-supplement-file {Supplemental xdc Files} Browse {} \
                       ]

    constructor {args} {
        lappend -args -modality application -height 400 -width 600

        eval itk_initialize $args
        array set _Initvals [list]
        array set _rel [list]

        itk_component add browse_dia {
            file_dialog::create .#auto
        } {}


        wm title $itk_component(hull) "Advanced Configuration Options" 
        
        addVFields
        #alignVFields
        addotherFields
        alignVFields

        #pack  $itk_component(vframe) -expand true -fill x
       # pack $itk_component(oframe) -expand true -fill x
        $itk_component(hull) hide Help
        $itk_component(hull) hide Apply
      
        buttonconfigure OK -text "Save" -underline 0 -command [itcl::code $this finish]
        buttonconfigure Cancel -underline 0

    }

    destructor {}


    # method buildVFields {} {
    #     wm title $itk_component(hull) "Advanced Configuration Options" 
    #     addVFields
    #     alignVFields
    # }

    method finish {} {
        [itcl::code $this deactivate 1]
    }
    
    method setmode {mode target name type} {
        set _type $type
        set _target $target
        set advmode $mode
        if {$mode == "config"} {
            wm title $itk_component(hull) "Advanced Configuration Options" 
            #pack forget $itk_component(tbname)
            #pack forget $itk_component(cframe)
#            $itk_component(cframe) configure -labeltext "Options"
            pack  $itk_component(vframe) -expand true -fill x
            pack  $itk_component(oframe) -expand true -fill x

#            pack $itk_component(cframe) -expand true -fill x
        }
    }

    
    proc create {pathname args} {
        uplevel #0 semu_advanced $pathname -modality application $args
    }


    method getVFile {values} {
        array set _Initvals $values
    }



    method returnFile {} {
        set values [array get _Initvals]
        return $values
    }

    method getVfields {} {return $vfields}
    method getOfields {} {return $ofields}

    method addVFields {} {   
        itk_component add vframe {
            iwidgets::labeledframe $itk_interior.vframe -labelpos nw \
                -labeltext "Verilog Options"
        } {}
        set vframe [$itk_component(vframe) childsite]
        foreach {f l widget attr} [getVfields] {
            if {$widget == "Entry"} {
                addEntryField $vframe $f $l $attr
            } elseif {$widget == "Combo"} {
                addComboBox $vframe $f $l $attr
            } elseif {$widget == "Browse"} {
                addEntryBrowse $vframe $f $l $attr
            } elseif {$widget == "BDir"} {
                addDirBrowse $vframe $f $l $attr
            }
        }
    }

    method addotherFields {} {   
        #puts "addotherfields"
        itk_component add oframe {
            iwidgets::labeledframe $itk_interior.oframe -labelpos nw \
                -labeltext "Other Options"
        } {}
        set oframe [$itk_component(oframe) childsite]
        foreach {f l widget attr} [getOfields] {
            if {$widget == "Entry"} {
                addEntryField $oframe $f $l $attr
            } elseif {$widget == "Combo"} {
                addComboBox $oframe $f $l $attr
            } elseif {$widget == "Browse"} {
                addEntryBrowse $oframe $f $l $attr
            } elseif {$widget == "BDir"} {
                addDirBrowse $oframe $f $l $attr
            }
        }
    }

    
    method addEntryField {w f l attr} {
        itk_component add $f {
            iwidgets::entryfield $w.$f -highlightthickness 0 -labelpos w \
                -width 60 \
                -labeltext $l -textvariable [itcl::scope _Initvals($f)]
        } {}
        pack $itk_component($f) -pady 5 -anchor w 
    }

    method addEntryBrowse {w f l attr} {
        itk_component add frame_$f {
            frame $w.frame_$f 
        }
        set g $itk_component(frame_$f)

        itk_component add $f {
            iwidgets::entryfield $g.$f -highlightthickness 0 -labelpos w \
                -width 60 \
                -labeltext $l -textvariable [itcl::scope _Initvals($f)]
        } {}
        itk_component add bb_$f {
            ttk::button $g.bb_$f -text "Browse..." -width 8 \
                -command "$this mkBrowse $f *.*"
        } {}

        itk_component add rel_$f {
            checkbutton $g.rel_$f -text "relative" \
                -variable [itcl::scope _rel($f)]
        } {}

        $itk_component(rel_$f) select


        pack $itk_component($f)  -side left
        pack $itk_component(bb_$f) -side left  -padx 5
        pack $itk_component(rel_$f) -side left -padx 5
        pack $itk_component(frame_$f) -side top -fill x -expand true
    }

    method addDirBrowse {w f l attr} {
        itk_component add frame_$f {
            frame $w.frame_$f 
        }
        set g $itk_component(frame_$f)

        itk_component add $f {
            iwidgets::entryfield $g.$f -highlightthickness 0 -labelpos w \
                -width 60 \
                -labeltext $l -textvariable [itcl::scope _Initvals($f)]
        } {}
        itk_component add bb_$f {
            ttk::button $g.bb_$f -text "Browse..." -width 8 \
                -command "$this mkDirBrowse $f *.*"
        } {}
        
        itk_component add rel_$f {
            checkbutton $g.rel_$f -text "relative" \
                -variable [itcl::scope _rel($f)]
        } {}
        $itk_component(rel_$f) select

        pack $itk_component($f)  -side left
        pack $itk_component(bb_$f) -side left  -padx 5
        pack $itk_component(rel_$f) -side left -padx 5
        pack $itk_component(frame_$f) -side top -fill x -expand true
    }
    
    method addComboBox {w f l attr} {
        itk_component add $f {
            iwidgets::combobox $w.$f -labeltext $l -textvariable [itcl::scope _Initvals($f)]
        } {}
        
        eval $itk_component($f) insert list 0 $attr

        pack $itk_component($f) -pady 5 -anchor w 
    }
    
    method alignVFields {} {
        set lab {}
        foreach {f l widget attr} [getVfields] {
            lappend lab $itk_component($f)
        } 
        foreach {f l wdget attr} [getOfields] {
            lappend lab $itk_component($f)
        }
        eval iwidgets::Labeledwidget::alignlabels $lab
    }
    

    method mkBrowse {field file_type} {
        #puts "mkBrowse pwd: [pwd]"
        $itk_component(browse_dia) setMask $file_type
        if {[$itk_component(browse_dia) activate]} {
            if {$_rel($field)} {
                set f [utils::make_related_path [$itk_component(browse_dia) getSelection]]
            } else {
                set f [$itk_component(browse_dia) getSelection]
            }
            $itk_component($field) insert end " $f"
        } else {
            $itk_component($field) insert end ""
        }
    }    


    method mkDirBrowse {field file_type } {
        set dir [tk_chooseDirectory  -initialdir [pwd] -title "Choose a directory" \
                     -parent $itk_interior]
        if {$dir != ""} {
            if {$_rel($field)} {
                set dir [utils::make_related_path $dir]
            }
            $itk_component($field) insert end  " $dir"
        }    
    }
}




