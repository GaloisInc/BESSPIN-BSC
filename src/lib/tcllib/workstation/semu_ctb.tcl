package require SemuInit
package provide semu_ctb 1.0

#catch "itcl::delete class semu_ctb"
itcl::class semu_ctb {
    inherit iwidgets::Dialog

    common  _Initvals
    common ver_sim ""
    common advmode "config"
    common _ctb
    common _target
    common _type ""
    common _Xactor


    #array (field label WidgetType attr)
    
    common cfields [list \
                        c++-compiler {c++-compiler} Entry {}\
                        c++-define {c++ Macro Definitions} Entry {} \
                        c++-options {c++ options} Entry {} \
                        c++-libraries {Additional c++ link libraries} Entry {} \
                        ]
    # common cfields [list \
    #                     c++-compiler {c++-compiler} Entry {}\
    #                     c++-files {c++ source files} Browse {} \
    #                     c++-define {c++ Macro Definitions} Entry {} \
    #                     c++-options {c++ options} Entry {} \
    #                     c++-libraries {Additional c++ link libraries} Entry {} \
    #                     tbgui-file {C Testbench Gui file} Browse {} \
    #                     ]

    

   
    constructor {args} {
        lappend -args -modality application -height 400 -width 600

        eval itk_initialize $args
        array set _Initvals [list]

        itk_component add browse_dia {
            file_dialog::create .#auto
        } {}

        wm title $itk_component(hull) "Advanced Options"      
        addCFields
        alignCFields
                
#        pack $itk_component(tbname) -pady 5 -anchor w   
        pack  $itk_component(cframe) -expand true -fill x
   
        $itk_component(hull) hide Help
        $itk_component(hull) hide Apply
      
#        buttonconfigure OK -text "OK" -underline 0 -command [itcl::code $this finish]
#        buttonconfigure Cancel -underline 0

    }

    destructor {}
    
    method getValues {} {
        set values [array get _Initvals]
        return $values
    }

    
    proc create {pathname args} {
        uplevel #0 semu_ctb $pathname -modality application $args
    }


    method getCFile {values} {
        array set _Initvals $values
        if {$_Initvals(c++-compiler) == ""} {
            $itk_component(c++-compiler) insert 0 g++
        }
    }
    
    
    method fillCFields {} {
        #parray _Initvals
        if {$_Initvals(c++-files) == ""} {
            $itk_component(c++-files) insert 0 $cfiles
            $itk_component(c++-options) insert 0 $coptions
            $itk_component(c++-libraries) insert 0 $clibraries
            $itk_component(c++-compiler) insert 0 $ccompiler
        }
    }

    method getCfields {} {return $cfields}

    method addCFields {} {
        
        itk_component add cframe {
            frame $itk_interior.cframe
        }
        set cframe $itk_component(cframe)
        
        foreach {f l widget attr} [getCfields] {
            if {$widget == "Entry"} {
                addEntryField $cframe $f $l $attr 
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

    
    
    method alignCFields {} {
        set lab {}
        foreach {f l widget attr} [getCfields] {
            lappend lab $itk_component($f)
        }
#        lappend lab $itk_component(tbname)
        eval iwidgets::Labeledwidget::alignlabels $lab
    }

}




