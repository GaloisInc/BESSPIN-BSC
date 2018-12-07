#  Copyright Bluespec Inc. 2009-2012

# Tcl/Tk package for building a graphical interface for user input for semu

# top-level user script to invoke hdl editing commands
# lappend auto_path $env(BLUESPECDIR)/tcllib/workstation

package require types
package require Iwidgets 4.0
package require Itk

package provide SemuInit 1.0

catch "itcl::delete class project_dialog"

itcl::class project_dialog {
    inherit iwidgets::Dialog
    variable projdir ""
    variable projfile ""

    constructor {args} {
        lappend -args -modality application 

        eval itk_initialize $args

        wm title $itk_component(hull) "Project File Options"
        wm minsize $itk_component(hull) 400 200
        set g [split [winfo geometry .view] "+"]
        wm geometry $itk_component(hull) "+[lindex $g 1]+[lindex $g 2]"
#        wm geometry $itk_component(hull) 0x0+40+40
        mkProject

        $itk_component(hull) hide Help
        $itk_component(hull) hide Apply

        
    }

    method mkProject {} {
        itk_component add pdir {
            iwidgets::entryfield $itk_interior.pdir -labeltext "Directory"  -width 40 -labelpos w 
        } {}
        itk_component add dir_button {
            ttk::button $itk_interior.dir_button -text "Browse..."  -command "[itcl::code $this selectDir]" 
        } {}

        
        itk_component add pname {
            iwidgets::entryfield $itk_interior.pname -labeltext "Project" -labelpos w -width 40
        } {}

        eval iwidgets::Labeledwidget::alignlabels $itk_component(pname) $itk_component(pdir)
        
        $itk_component(pdir) selection clear
        $itk_component(pdir) insert 0 [pwd]
        
       
        pack $itk_component(pname) -pady 5 -padx 5
        pack $itk_component(pdir)  -pady 5  -padx 5
        pack $itk_component(dir_button) -pady 5  -padx 5

    }

    method selectDir {} {
        set dir [tk_chooseDirectory -parent "$itk_interior" -title "Select Project Directory"]
        if {$dir == ""} {
            return
        } else {
            $itk_component(pdir) delete 0 end
            $itk_component(pdir) insert 0 $dir
        }
    }

    method setTitle { title  } {
        wm title $itk_component(hull) $title
        $itk_component(pname) delete 0 end
    }


    proc create {pathname args} {
        uplevel #0 project_dialog $pathname -modality application $args
    }

       
}


      
catch "itcl::delete class file_dialog"

itcl::class file_dialog {
    inherit iwidgets::Dialog
    ##
    common _relative

    constructor {args} {

        lappend args -modality application -title "File Open" \
                                                        -height 400 -width 400

        # construct the dialog
        eval itk_initialize $args

#       set types [list {{Verilog Files} {.v}}  {{All files} {*}}]
        itk_component add lf {
            iwidgets::Labeledframe $itk_interior.lf -labelpos nw -labeltext "Files"
        } {}
        pack $itk_component(lf)  -anchor nw -expand 1 -fill x

        itk_component add lfdialog {
	    iwidgets::extfileselectionbox [$itk_component(lf) childsite].lfd \
		-width 8i -height 4i -fileslabel "Files"
        } {}

        # itk_component add rel {
        #     checkbutton [$itk_component(lf) childsite].rel -text "relative" \
        #         -textvariable [itcl::scope _relative]
        # } {}
        # $itk_component(rel) select
        
        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help
        pack $itk_component(lfdialog)  -anchor nw -expand 1 -fill x
        #pack $itk_component(rel) 

    }
    destructor {}

    method setMask {file_type} {
        $itk_component(lfdialog) configure -mask "$file_type" -directory [pwd]
        $itk_component(lfdialog) filter
    }

    method getSelection {} {
        set selection [$itk_component(lfdialog) get]
        return $selection
    }


    proc create {pathname args} {
        uplevel #0 file_dialog $pathname -modality application $args
    }
}

