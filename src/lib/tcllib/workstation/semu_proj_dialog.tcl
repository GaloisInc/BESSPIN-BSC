
package require Iwidgets 4.0


#catch "itcl::delete class semu_proj_dialog"

itcl::class semu_proj_dialog {
    inherit iwidgets::Dialog
    common _Projdir "."
    common _Projname ""


    constructor {args} {
        lappend args -modality application -title "Define New Project" \
            -height 200 -width 450

        eval itk_initialize $args

        # itk_component add lf {
        #     iwidgets::Labeledframe $itk_interior.lf -labelpos nw \
        #         -labeltext "Project Directories"
        # } {}
        
        
        itk_component add projname {
            iwidgets::entryfield $itk_interior.projname -labeltext "Project Name:" \
                -textvariable [itcl::scope _Projname] 

        } {}
        
        itk_component add projdir {
            iwidgets::entryfield $itk_interior.projdir -labeltext "Parent Directory:" \
                -command "[itcl::code $this returnCmd ]" -textvariable [itcl::scope _Projdir]
        } {}

        itk_component add projbb {
            ttk::button $itk_interior.projbb -text "Browse..." -command "[itcl::code $this browseDir]"
        } {}

        $itk_component(hull) hide Apply
        $itk_component(hull) hide Help

        $itk_component(hull) buttonconfigure OK -command "[itcl::code $this checkDir]"

        grid $itk_component(projdir)  -row 0 -column 0 -sticky w -pady 10
        grid $itk_component(projbb) -row 0 -column 1 -padx 5 -pady 10
        grid $itk_component(projname) -row 1 -column 0 -sticky w -pady 10
        #pack $itk_component(lf) -anchor nw -expand 1 -fill x   

        eval iwidgets::Labeledwidget::alignlabels $itk_component(projname) $itk_component(projdir)

        
      
    }

    destructor {}

    method checkDir {} {
        puts "checkDir"
        if { $_Projname == "" } {
            tk_messageBox -title "Error" -icon error -message "project name must be given"
            raise $itk_component(hull)
        } elseif {[file exists $_Projdir/$_Projname/$_Projname.config] == 1} {
            tk_messageBox -title "Error" -icon error -message "The project already exists." 
            raise $itk_component(hull)
        } else {
            file mkdir $_Projdir/$_Projname
            $itk_component(hull) deactivate 1
        }      
    }

    method setTitle { title } {
        wm title $itk_component(hull) $title
    }

    method getProject {} {
        set proj $_Projdir/$_Projname
        puts "project:  $proj"
        return $proj
    }


    method browseDir {} {
        set dir [tk_chooseDirectory -parent "$itk_interior" -title "Select Project Directory"]
        if {$dir == ""} {
            return
        } else {
            set _Projdir $dir
        }
    }


    proc create {pathname args} {
        uplevel #0 semu_proj_dialog $pathname -modality none $args
    }
}
