#catch "itcl::delete class input_source"

itcl::class input_source {
    inherit iwidgets::Dialog
    ##

    variable _rbinput

    constructor {args} {

        lappend args -modality application -title "Input Source" 
                                                      

        # construct the dialog      
        eval itk_initialize $args
   
#        mkInputSrc

        mkSource

    }
   
   
    destructor {}

    private method mkSource {} {

        set input "manual"

        itk_component add sf {
            iwidgets::Labeledframe $itk_interior.sf -labeltext "Input Source" \
                -labelpos nw
        } {}
        
        set sf [$itk_component(sf) childsite]

	itk_component add opt_dia {
            file_dialog::create .odia
        } {}

        itk_component add man_radio {
            ttk::radiobutton $sf.man_radio -text "Manual" -variable [itcl::scope _rbinput] -value manual -command [itcl::code $this doButtons]
        } {}
        set man_radio $itk_component(man_radio)
        
        itk_component add vcd_radio {  
            ttk::radiobutton $sf.vcd_radio -text "VCD Player" -variable [itcl::scope _rbinput] -value vcd -command [itcl::code $this doButtons]
        } {}
        set vcd_radio $itk_component(vcd_radio)

        itk_component add vcd_field {
            iwidgets::entryfield $sf.vcd_field -labeltext "VCD File" -width 50
        } {}
        set vcd_field $itk_component(vcd_field)
       
        itk_component add bbv {
            ttk::button $sf.bbv -text "Browse..." -command "$this mkOpenFile vcd_field *.vcd "
        } {}

        set bbv $itk_component(bbv)
            
        itk_component add capi_radio {
            ttk::radiobutton $sf.capi_radio -text "C API" -variable [itcl::scope _rbinput]  -value capi -command [itcl::code $this doButtons]
        } {}
        set capi_radio $itk_component(capi_radio)

        itk_component add api_field {
            iwidgets::entryfield $sf.api_field -labeltext "C-API File"  -width 50
        } {}
        set api_field $itk_component(api_field)

        itk_component add bb {
            ttk::button $sf.browse -text "Browse..." -command  "$this mkOpenFile api_field *.c" 
        } {}  
        set bb $itk_component(bb)
        $man_radio invoke
         
        # pack $man_radio -fill x -side left
        # pack $vcd_radio -fill x -side left
        # pack $vcd_field $bbv
        # pack $capi_radio -fill x -side left
        # pack $api_field $bb

        grid $man_radio -row 1 -column 0 -sticky w 
        grid $vcd_radio -row 2 -column 0 -sticky w 
        grid $vcd_field -row 3 -column 0 -padx 5
        grid $bbv -row 3 -column 1 -padx 5
        grid $capi_radio -row 4 -column 0 -sticky w
        grid $api_field -row 5 -column 0 -padx 5
        grid $bb -row 5 -column 1 -padx 5

        pack $itk_component(sf) -fill x -side top        

        .src hide Help
        .src hide Apply
        

    }

    private method doButtons {} {
        switch -exact $_rbinput {
            "manual" {
                $itk_component(vcd_field) configure -state disabled
                $itk_component(api_field) configure -state disabled
            }
            "vcd" {
                $itk_component(api_field) configure -state disabled
                $itk_component(vcd_field) configure -state normal
}
            "capi" {
                $itk_component(api_field) configure -state normal
                $itk_component(vcd_field) configure -state disabled
            }
        }
    }

    method getInputSource {} {
        return $_rbinput
    }
        

 

    method create_man_fields {} {
        puts "create_man_fields"
        
     }


    method create_vcd_fields {} {

       itk_component add vcd_field {
            iwidgets::entryfield $sf.vcd_field -labeltext "VCD File" -width 50
        } {}
        set vcd_field $itk_component(vcd_field)
       
        itk_component add bbv {
            ttk::button $sf.bbv -text "Browse..." -command "$this mkOpenFile vcd_field "
        } {}
        set bbv $itk_component(bbv)
        
        pack $itk_component(vcd_field)

     }

    method create_capi_fields {} {
        puts "create_capi_fields"
     }

    proc create {pathname args} {
        uplevel #1 input_source $pathname -modality none $args
    }

    method mkOpenFile {field file_type} {
        $itk_component(opt_dia) setMask $file_type
        if {[$itk_component(opt_dia) activate]} {
            $itk_component($field) insert 0 [$itk_component(opt_dia) getSelection]
        } else {
            $itk_component($field) insert 0 ""
        }
    }    

}
