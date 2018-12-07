package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require semu_ctb

package require SemuInit

package provide semu_testbench 1.0

catch "itcl::delete class semu_testbench"
itcl::class semu_testbench {
    inherit iwidgets::Dialog
    variable _topfile ""
    variable _target
    variable _type 
    common _Initvals
    variable _Topmod
    variable _tbrelative
    variable _tb_name ""
    variable _tb_path ""
    variable _tb_path_prev ""
    variable _new_tb_name "<new>"
    variable _tb_name_init ""
    variable _tb_name_prev ""

    common cfields [list \
                        c++-compiler {c++-compiler} Entry {}\
                        c++-define {c++ Macro Definitions} Entry {} \
                        c++-options {c++ options} Entry {} \
                        c++-libraries {Additional c++ link libraries} Entry {} \
                        ]
    common gencfields [list \
                           TclTb.cpp \
                           usertb.cpp \
                           ]


    constructor {args} {
        lappend -args -modality application -height 400 -width 600

        eval itk_initialize $args
        array set _Initvals [list]

        itk_component add browse_dia {
            file_dialog::create .#auto
        } {}

        itk_component add semu_ctb {
            semu_ctb::create .ctb
        } {}

        wm title $itk_component(hull) "C++ Testbench Definition"      

        mkName
        mkTb
   
        $itk_component(hull) hide Help
      
        #pack $itk_component(tbname) -pady 5 -anchor w
        #pack $itk_component(tbframe) -expand true -fill x
        
        buttonconfigure OK -text "Save"  -command [itcl::code $this finish]
        buttonconfigure Apply -text "Advanced" -underline 0 -command [itcl::code $this mkAdvanced]
        buttonconfigure Cancel -underline 0

    }

    destructor {}

    proc create {pathname args} {
        uplevel #0 semu_testbench $pathname -modality application $args
    }

    method activate {} {
        after 500 focus -force $itk_component(tbdir)
        chain
    }

    method getCfields {} {return $cfields}
    method getgenc {} {return $gencfields}
    
    method getTopmod {topmod} {
        set _Topmod $topmod
    }


    method mkName {} {
        itk_component add tbname {
            iwidgets::entryfield $itk_interior.tbname -highlightthickness 0 -labelpos w \
                -width 20 -labeltext "Testbench Name" \
                -textvariable [itcl::scope _tb_name]
        } {}

        itk_component add tbframe {
            frame $itk_interior.tbframe
        } {}
              
        itk_component add tbdir {
            iwidgets::entryfield $itk_component(tbframe).tbdir -highlightthickness 0 -labelpos w \
                -width 40 -labeltext "Testbench Directory" -textvariable [itcl::scope _Initvals(tbdir)]
        } {}

        itk_component add dirbut {
            ttk::button $itk_component(tbframe).dirbut -text "Browse..." -width 8 \
                -command  "$this mkDirBrowse tbdir"
        } {}

        #add relative checkbox
        itk_component add dircb {
            checkbutton $itk_component(tbframe).dircb -text "relative" \
                -variable [itcl::scope _tbrelative]
        } {}
        $itk_component(dircb) select

        itk_component add tplate_but {
            ttk::button $itk_component(tbframe).tplate_but -text "Add Starter Templates" \
                -width 20 -command "$this checkTb"
        } {}
        
        grid $itk_component(tbdir) -row 0 -column 0 -sticky w
        grid $itk_component(dirbut) -row 0 -column 1 -padx 10
        grid $itk_component(dircb) -row 0 -column 2 -padx 5
        grid $itk_component(tplate_but) -row 0 -column 3 -padx 30

        pack $itk_component(tbname) -pady 5 -anchor w
        pack $itk_component(tbframe) -pady 5 -anchor w 
 
       
        eval iwidgets::Labeledwidget::alignlabels $itk_component(tbname) $itk_component(tbdir)
    }

    method mkTb {} {
        mkScrollBox csource {C++ Source Files} file
        mkScrollBox idir {Include Directories} dir
    }

    method toggleRelative {obj} {
	foreach index [$obj curselection] text  [$obj getcurselection] {
	    set flip  [utils::make_related_path $text]
	    if {$text == $flip || [string range $text 0 0] == "."} {
		set flip [file normalize $text]
	    }
	    $obj delete $index
	    $obj insert $index $flip
	    $obj selection set $index
	}
	return
    }

    method mkScrollBox {f l type} {
        set tbframe $itk_interior
        
        itk_component add $f {
            iwidgets::scrolledlistbox $tbframe.$f -width 300 \
		-selectmode extended \
                -labeltext $l -labelpos nw -height 150p
        } {
            keep -cursor -background
        }
        set fc $itk_component($f)

        pack $fc -padx 2 -pady 2 -side left -fill both -expand true
        $fc selection set 0 0
        $fc justify right

        itk_component add but_$f {
            iwidgets::buttonbox $tbframe.but_$f \
                -orient vertical -pady 0 -padx 5
        } {
            keep -cursor -background
        }
        set bbox $itk_component(but_$f)
        pack $bbox -padx 0 -pady 0 -side left
        if {$type == "file"} {
        $bbox add add_dir -text "Add" -command "$this mkBrowse $f *.*" \
            -height 40 -width 100 -padx 1 -pady 5
        } else {
            $bbox add add_dir -text "Add" -command "$this mkDirBrowse $f" \
                -height 40 -width 100 -padx 1 -pady 5
        }
        
        $bbox add remove -text "Remove" -command [itcl::code $this rm_dir $f] \
            -height 40 -width 100 -padx 1 -pady 5

        $bbox add toggle -text "./ <=> /" -command [itcl::code $this toggleRelative $tbframe.$f] \
            -height 40 -width 100 -padx 1 -pady 5
    }

    method checkDir {} {
        $itk_component(csource) delete
    }

    method rm_dir {f} {
        if {[set c [$itk_component($f) curselection]] != ""} {
            set p [$itk_component($f) get $c $c]
            $itk_component($f) delete $c $c
        }
    }
    
    method mkAdvanced {} {
        set values [array get _Initvals]
        $itk_component(semu_ctb) getCFile $values
        $itk_component(semu_ctb) activate
    }

    method finish {} {
        set values [$itk_component(semu_ctb) getValues]
        array set tempvals $values
        if {$tempvals(c++-compiler) != ""} {
             array set _Initvals $values
        } 

        if {$_type == "new"} {
            if {[checkduplicates]} {
                return 
            } else {
                if {![savetb]} {
                    return 
                }
            }
        } else {
            if {![savetbEdits]} {
                return 
            }         
        }
        #checkTb
        [itcl::code $this deactivate 1]
    }
    
    method getname {} {
        return [$itk_component(tbname) get]
    }

    method checkTb {} {
        #puts "checktb tbdir $_Initvals(tbdir)"
	set tbfile "$_Initvals(tbdir)/TclTb.cpp"
        set fileexist [file exists $tbfile]
        #puts "fileexist: $fileexist"
        if { $fileexist } {
            set action [tk_messageBox -title "Overwrite Testbench Template Files" -icon question -type yesno \
                            -message "The testbench files already exist.  Do you wish to overwrite them with a starter template version?"\
                            -detail "Select yes to overwrite, no to keep current files."\
                            -parent $itk_interior]
            switch -- $action {
                yes {
		    file delete -force $tbfile
                    genTb
                }
                no {
                    genTbdone "Current testbench files unchanged." 

                    return
                }
            }
        } else {
            genTb
        }
    }

    method genTb {} {
        #puts "gentb tbdir: $_Initvals(tbdir)"
        #parray _Initvals

        set projfile [open project.cfg "r+"]
        while {[gets $projfile line] >= 0} {
            if {[regexp {(verilog-lib-directories): (.*)} $line fullline key value]} {
                set vlib $value
            } elseif {[regexp {(verilog-inc-directories): (.*)} $line fullline key value]} {
                set inc $value
            }
        }
        set vlist {}
        foreach file $vlib {
            append vlist "-y $file "
        }
        set inclist {}
        foreach file $inc {
            append inclist "+incdir+$file "
        }

        set bdir ./build/cpp

        set cmd "$::env(BLUESPECDIR)/board_support/scripts/Generate_TBTemplate.py $_Topmod.pin $_Initvals(tbdir) $bdir"
        semu_viewer::execute_in_shell_wait $cmd [itcl::code $this genTbdone "File generation complete"]

        if {[file exists $_Initvals(tbdir)/TclTb.cpp]} {
            return true
        } else {
            tk_messageBox -title "Error" -icon error \
                -message "Default files could not be created" -type ok \
                -parent $itk_interior
            return false
        }
 }
    
    method genTbdone {msg} {
        semu_viewer::display_message $msg false
    }


    method checkduplicates {} {
        set tbname [$itk_component(tbname) get]
        if {[llength $tbname] > 1} {
            tk_messageBox -title "Error" -icon error \
                -message "The testbench name may not contain spaces." \
                -parent $itk_interior
            return true
        }
        if {[lsearch -exact $semu_build::_ctb $tbname] >= 0 || [lsearch -exact $semu_build::_stb $tbname] >= 0 } {
            tk_messageBox -title "Error" -icon error \
                -message "A testbench by that name already exists." \
                -parent $itk_interior
            return true
        } else {
            return false
        }
    }

    method readfile {editname} {
        #puts "in readfile"

        $itk_component(csource) clear
        $itk_component(idir) clear
        
        #puts "readfile $editname"
        set tbfile [open "testbench.bld" "r"]
        while {[gets $tbfile line] >= 0} {
            if {[regexp {^\[emu(.*)\]} $line tbfull]} {
                regsub -all {\[|\]} $tbfull "" tbname
                regsub  {^emu} $tbname "" tbnamefinal
                if {$editname == $tbnamefinal} {
                    readtarget $line $tbfile
                    return
                }
            }
        }
        close $tbfile
    }

    method readtarget {line tbfile} {

        #puts "readtarget"
        while {$line != "" && $line >=0} {
            gets $tbfile line
	    set ignore ""
            regexp {(.*):( +)(.*)} $line fulline key ignore value
            #puts "key $key value $value"
           
            if {$key == "#sourcedir"} {
                set _Initvals(tbdir) $value
            } elseif {$key == "c++-files"} {
                foreach file $value {
                    $itk_component(csource) insert 0  $file
                }
                $itk_component(csource) justify right
            } elseif {$key == "c++-options"} {
                set options {}
                foreach file $value {
                    if {[regexp {(^-I)(.*)} $file fullline hr newvalue]} {
                        $itk_component(idir) insert 0 $newvalue
                    } else {
                       # puts "else $file"
                        lappend options $file
                    }
                }
                $itk_component(idir) justify right
                set value $options
            }
            set _Initvals($key) $value
        }
        close $tbfile
    }

    method getTbname {} {
        set tbname [$itk_component(tbname) get]
        return $tbname
    }

    method savetbEdits {} {
        set editname [$itk_component(tbname) get]
        if {$editname == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The testbench must have a name" \
                -parent $itk_interior
            return false
        }
        set tmpfile [open "temp.bld" "w"]
        set tbfile [open "testbench.bld" "r"]
        while {[gets $tbfile line] >= 0} {
            if  {[regexp {^\[(.*)\]} $line tbfull]} {
                regsub -all {\[|\]} $tbfull "" tbname
                set tbnamefinal [string range $tbname 3 end]
                set tbtarget [string range $tbname 0 2]
                if {$editname != $tbnamefinal} {
                    savenontarget $line $tbfile $tmpfile $tbname
                } else {
                    if {$tbtarget == "emu"} {
                    saveboth $tmpfile $tbnamefinal
                    }
                }
            }
        }
        close $tmpfile
        close $tbfile
        #file copy -force "temp.bld" "testbench.bld"
        file rename -force "temp.bld" "testbench.bld"
        return true
    }

    method deletetb {delname} {
        set tmpfile [open "temp.bld" "w"]
        set tbfile [open "testbench.bld" "r"]
        while {[gets $tbfile line] >= 0} {
            if  {[regexp {^\[(.*)\]} $line tbfull]} {
                regsub -all {\[|\]} $tbfull "" tbname
                set tbnamefinal [string range $tbname 3 end]
                set tbtarget [string range $tbname 0 2]
                if {$delname != $tbnamefinal} {
                    savenontarget $line $tbfile $tmpfile $tbname
                }
            }
        }
        close $tmpfile
        close $tbfile
        #file copy -force "temp.bld" "testbench.bld"
        file rename -force "temp.bld" "testbench.bld"
    }

    method savenontarget {line tbfile tmpfile tbname} {
        set pln ""
        append pln "\[$tbname\]" \n
        while {$line != "" && $line >=0} {
            gets $tbfile line
            append pln $line \n
        }
        puts $tmpfile $pln
    }


    method saveboth {file tbname} {
        set pln ""
        foreach target {emu sim} {
            set clist [join [$itk_component(csource) get 0 end] " "]
            if {$_type == "new"} {
                foreach f [getgenc] {
                    lappend clist "$_Initvals(tbdir)/$f"
                }
            }
            set optlist [join [$itk_component(idir) get 0 end] " "]

            if {$_type == "new"} {
                lappend optlist $_Initvals(tbdir)
            }
            set newlist {}
            set fulltbname $target$tbname
            append pln "\[$fulltbname\]" \n
            append pln "target-root-directory:  build/$target" \n
            append pln "extends-target:        c++_base" \n
            #append pln "exe-file:              $fulltbname" \n
            append pln "c++-files: $clist" \n

            foreach {key l widget attr} [getCfields] {
                if {$key == "c++-options"} {
                    set newlist $_Initvals($key)
                    foreach opt $optlist {
                        lappend newlist -I$opt
                    }
                    append pln "$key: $newlist" \n                    
                } else {
                    append pln "$key: $_Initvals($key)" \n
                }
            }
	    append pln "c++-uses-readback: true" \n
            append pln "shared-lib: libbsdebug.so" \n
            append pln "#sourcedir: $_Initvals(tbdir)" \n
            append pln "" \n
        }
        puts $file $pln
    }

    method savetb {} {
        set _tb_path_prev $_Initvals(tbdir)
        set tbname [$itk_component(tbname) get]
        if {$tbname == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The testbench must have a name" \
                -parent $itk_interior
            return false
        }
            
        if {![file exists "testbench.bld"]} {
            set tbfile [open "testbench.bld" "w"]
        } else {
            set tbfile [open "testbench.bld" "a"]
        }
        saveboth $tbfile $tbname
        close $tbfile
        return true
    }


    method updateTbPath {args} {
        set _tb_path_prev ./src/$_tb_name_prev
        set _tbname_prev $_tb_name
        set tbdir ./src/$_tb_name
        $itk_component(tbdir) clear
        $itk_component(tbdir) insert 0 $tbdir
    }

    method setmode {mode target name type topfile} {
        set _type $type
        set _target $target
        set advmode $mode
        set _topfile $topfile
        $itk_component(csource) clear
        $itk_component(idir) clear

        $itk_component(tbname) delete 0 end
        if {$type == "edit"} {
            if {$name != ""} {
                set _tb_name $name
            }
            set _tb_name_init $name
            $itk_component(tbname) clear
            $itk_component(tbname) insert 0 $name
            $itk_component(tbname) configure -state disabled
            $itk_component(tbdir) configure -state disabled
            $itk_component(dirbut) configure -state disabled
            $itk_component(dircb) configure -state disabled
            #updateTbPath
        } else {
            set _tb_name_init $name
            $itk_component(tbname) clear
 
           if {$name != ""} {
               set _tb_name $name
            } else {
                set _tb_name $_new_tb_name
            }

            updateTbPath arg
            trace add variable [itcl::scope _tb_name] write [itcl::code $this updateTbPath]
            #set _Initvals(tbdir) ./src/$_tb_name

            $itk_component(tbname) clear
            $itk_component(tbname) insert 0 $name
            $itk_component(tbname) configure -state normal
            $itk_component(tbdir) configure -state normal
            $itk_component(dirbut) configure -state normal
            $itk_component(dircb) configure -state normal
            foreach {key l widget attr} [getCfields] {            
                if {$key == "c++-compiler"} {
                    set _Initvals($key) g++
                } elseif {$key == "c++-options"} {
                    set _Initvals($key) "-O3"
                } else {
                    set _Initvals($key) ""
                }
            }
        }
    }

    method alignCFields {} {
        set lab {}
        foreach {f l widget attr} [getCfields] {
            lappend lab $itk_component($f)
        }
        lappend lab $itk_component(tbname)
        eval iwidgets::Labeledwidget::alignlabels $lab
    }

    method mkBrowse {field file_type} {
        $itk_component(browse_dia) setMask $file_type
        if {[$itk_component(browse_dia) activate]} {
            set insertfield "[$itk_component(browse_dia) getSelection] "
            $itk_component($field) insert 0 $insertfield
        } else {
            #$itk_component($field) insert 0 ""
        }
    }    

    method mkDirBrowse {field } {
        set dir [tk_chooseDirectory  -title "Choose a directory" \
                     -parent $itk_interior]
        if {$dir != ""} {
            if {$field == "tbdir"} {
                if {$_tbrelative} {
                    set dir [utils::make_related_path $dir]
                }
                $itk_component($field) clear
            }
            $itk_component($field) insert 0 $dir
        }    
    }
}




