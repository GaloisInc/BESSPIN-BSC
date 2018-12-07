
package require Itcl
package require Bluetcl
package require semu_stb
package require semu_config
package require SemuInit

package provide sim_testbench 1.0

global _tb_name ""
global _tb_name_prev ""
global _tb_path  ""

## not sure why this is needed
set _tb_name_prev ""

proc ::updateTbPath args {
    ::updateTbPathInternal
}

proc ::updateTbPathInternal {} {
    global _tb_name
    global _tb_name_prev
    global _tb_path
    #set _tb_path_prev ./tbs/$_tb_name_prev/src
    set _tb_path_prev ./src/$_tb_name_prev
    set _tb_name_prev $_tb_name
    if {$_tb_path == $_tb_path_prev || $_tb_path == ""} {
	set _tb_path ./src/$_tb_name
    }
}

catch "itcl::delete class sim_testbench"
itcl::class sim_testbench {
    inherit iwidgets::Dialog
    variable _topfile ""
    variable _target
    variable _type 
    common _Initvals
    variable _Topmod
    variable _tbrelative
    variable _simulator ""
    variable _top_module ""
    variable _tb_name_init ""
    variable _new_tb_name "<new>"


    common cfields [list \
                        c++-compiler {c++-compiler} Entry {}\
                        c++-define {c++ Macro Definitions} Entry {} \
                        c++-options {c++ options} Entry {} \
                        c++-libraries {Additional c++ link libraries} Entry {} \
                        ]
    common gencfields [list \
                           usertb.cpp \
                           ]



    constructor {args} {
        lappend -args -modality application -height 400 -width 600

        eval itk_initialize $args
        array set _Initvals [list]

        itk_component add browse_dia {
            file_dialog::create .#auto
        } {}

        itk_component add semu_stb {
            semu_stb::create .stb
        } {}

        wm title $itk_component(hull) "Simulation Testbench Definition"

        mkName
        mkTb
   
        $itk_component(hull) hide Help
      
        #pack $itk_component(tbname) -pady 5 -anchor w
        #pack $itk_component(tbframe) -expand true -fill x
        
        buttonconfigure OK -text "Save" -underline 0 -command [itcl::code $this finish]
        buttonconfigure Apply -text "Advanced" -underline 0 -command [itcl::code $this mkAdvanced]
        buttonconfigure Cancel -underline 0 -command [itcl::code $this doCancel]

    }

    destructor {}

    proc create {pathname args} {
        uplevel #0 sim_testbench $pathname -modality application $args
    }

    method activate {} {
        after 500 focus -force $itk_component(tbdir)
        chain
    }

    method getCfields {} {return $cfields}
#    method getgenc {} {return $gencfields}
    method getgenc {} {return [list]}

    method getTopmod {topmod} {
        set _Topmod $topmod
    }


    method mkName {} {
	global _tb_name
	global _tb_name_prev
	global _tb_path

        itk_component add tbname {
            iwidgets::entryfield $itk_interior.tbname -highlightthickness 0 -labelpos w \
                -width 20 -labeltext "Testbench Name" -textvariable _tb_name
        } {}

        itk_component add tbframe {
            frame $itk_interior.tbframe
        } {}
              
        itk_component add topname {
            iwidgets::entryfield $itk_interior.topname -highlightthickness 0 -labelpos w \
                -width 20 -labeltext "Top Module" -textvariable [itcl::scope _top_module]

        } {}

        itk_component add tbdir {
            iwidgets::entryfield $itk_component(tbframe).tbdir -highlightthickness 0 -labelpos w \
                -width 40 -labeltext "Testbench Directory" -textvariable _tb_path
#                -width 40 -labeltext "Testbench Directory" -textvariable [itcl::scope _Initvals(tbdir)]

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
            ttk::button $itk_component(tbframe).tplate_but -text "Add Starter Templates" -width 20 \
                -command  "$this addTemplates"
        } {}

	itk_component add simname {
	    iwidgets::combobox  $itk_interior.simname  -labeltext "RTL Simulator" -textvariable [itcl::scope _simulator] -width 15
        } {}

        foreach i [semu_config::get_semu_simulators] {
            eval $itk_component(simname) insert list end $i
	}
        
        grid $itk_component(tbdir) -row 0 -column 0 -sticky w
        grid $itk_component(dirbut) -row 0 -column 1 -padx 10
        grid $itk_component(dircb) -row 0 -column 2 -padx 5
        grid $itk_component(tplate_but) -row 0 -column 3 -padx 30

        
        pack $itk_component(tbname)  -pady 5 -anchor w
        pack $itk_component(topname) -pady 5 -anchor w
        pack $itk_component(tbframe) -pady 5 -anchor w -fill x
        pack $itk_component(simname) -pady 5 -anchor w        

        eval iwidgets::Labeledwidget::alignlabels $itk_component(tbname) $itk_component(topname) $itk_component(tbdir) $itk_component(simname)
    }

    method mkTb {} {
        mkScrollBox vsource {Source Files} file
        mkScrollBox idir {Include Directories} dir
    }

    method toggleRelative {obj} {
	foreach index [$obj curselection] text  [$obj getcurselection] {
	    set flip  [utils::make_related_path $text]
	    if {$text == $flip || [string range $text 0 0] == "."} {
		set flip [file normalize $text]
	    }
	    if {[string range $text 0 0] == "\$"} {
		set flip $text
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
                -labeltext $l -labelpos nw -height 100p
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
        $bbox add add_dir -text "Add" -command "$this mkBrowse $f *.* 1" \
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
        $itk_component(vsource) delete
    }

    method rm_dir {f} {
        if {[set c [$itk_component($f) curselection]] != ""} {
            set p [$itk_component($f) get $c $c]
            $itk_component($f) delete $c $c
        }
    }
    
    method mkAdvanced {} {
	# set values [array get _Initvals]
	# # $itk_component(semu_advanced) buildVFields
        # $itk_component(semu_advanced) getVFile $values
        # $itk_component(semu_advanced) setmode "config" "" "" ""
        # $itk_component(semu_advanced) activate

        # set values [array get _Initvals]
        # $itk_component(semu_stb) getCFile $values
        # $itk_component(semu_stb) activate
    }

    method doCancel {} {
	global   _tb_name
	variable _tb_name_init
	variable _new_tb_name

	set _tb_name $_tb_name_init

	# if {$_tb_name == $_new_tb_name} {
	    
	# }
        [itcl::code $this deactivate 1]
    }

    method finish {} {
	global   _tb_name
	variable _tb_name_init

	# if {$_tb_name != $_tb_name_init} {
	#     if {[checkduplicates]} {
        #         return
	#     }
	# }

        set values [$itk_component(semu_stb) getValues]
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
        checkTb
        [itcl::code $this deactivate 1]
    }
    
    method getname {} {
        return [$itk_component(tbname) get]
    }

    method checkTb {} {
#	genTb
    }

    method addTemplates {} {
	global _tb_path
	variable _tb_name_init
	if {$_tb_name_init != [$itk_component(tbname) get]} {
	    if {[checkduplicates]} {
		return true
	    }
	}

	set dir_exists [createTbDir]

	if {$dir_exists} {
	    set cmd "cp -f $::env(BLUESPECDIR)/tcllib/simtb/simtb_gui_dut.tcl $_tb_path/gui_dut.tcl"
	    
	    if {[file exists $_tb_path/gui_dut.tcl]} {
		set action [tk_messageBox -title "Overwrite Testbench Gui File?" -icon question -type yesno \
				-message "The gui testbench file '$_tb_path/gui_dut.tcl' already exists. Do you wish to overwrite it with a starter template version?" \
				-detail "Select yes to overwrite, no to keep current file."\
				-parent $itk_interior]
		switch -- $action {
		    yes {
			semu_viewer::execute_in_shell_wait $cmd [itcl::code $this displayMsg "Template testbench gui file added."]
		    }
		    no {
			displayMsg "Current testbench gui file unchanged."
		    }
		}
	    } else {
		semu_viewer::execute_in_shell_wait $cmd [itcl::code $this displayMsg "Template testbench gui file added."]
	    }

	    set first_list [create_include_list]
	    set optlist {}
	    foreach {dir} $first_list {
		lappend optlist -y
		lappend optlist $dir

	    }
	    lappend optlist -y
	    lappend optlist $::env(BLUESPECDIR)/Verilog
	    set cmd "$::env(BLUESPECDIR)/board_support/scripts/Generate_SimTBTemplate.py $_Topmod.pin $_tb_path"

	    set proxy_file "$_Topmod\_proxy.v"

	    if {[file exists "$_tb_path/$proxy_file"]} {
		set action [tk_messageBox -title "Overwrite verilog proxy file?" -icon question -type yesno \
				-message "The verilog proxy file '$_tb_path/$proxy_file' already exists. Do you wish to overwrite it with a starter template version?" \
				-detail "Select yes to overwrite, no to keep current file."\
				-parent $itk_interior]
		switch -- $action {
		    yes {
			semu_viewer::execute_in_shell_wait $cmd [itcl::code $this displayMsg "Verilog proxy file added."]
		    }
		    no {
			displayMsg "Curent Verilog proxy file unchanged."
		    }
		}


	    } else {
		semu_viewer::execute_in_shell_wait $cmd [itcl::code $this displayMsg "Verilog proxy file added."]
	    }
	}
	return true;
    }
    
    method createTbDir {} {
	global _tb_path
	
        set cmd "mkdir -p $_tb_path"
	if {![file exists $_tb_path]} {

            set action [tk_messageBox -title "Create Testbench Directory?" -icon question -type yesno \
                            -message "The testbench directory '$_tb_path' does not exist. Do you wish to create it?" \
                            -detail "Select yes to create the directory."\
                            -parent $itk_interior]
	    switch -- $action {
                yes {
		    semu_viewer::execute_in_shell_wait $cmd [itcl::code $this displayMsg "Testbench directory created."]
                }
                no {
		    displayMsg "No testbench directory created."
		    return false
		}
	    }
	}

	set dirs [join [$itk_component(idir) get 0 end] " "]
	if {[lsearch -exact $dirs $_tb_path] < 0} {
	    $itk_component(idir) insert 0 $_tb_path
	}
	return true;
    }
    
    method displayMsg {msg} {
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
                -message "A testbench named '$tbname' already exists. Please pick another name" \
                -parent $itk_interior
            return true
        } else {
            return false
        }
    }

    method readfile {editname} {
        #puts "in readfile"

        $itk_component(vsource) clear
        $itk_component(idir) clear
        
        #puts "readfile $editname"
        set tbfile [open "sim_tb.bld" "r"]
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
	variable _simulator
	variable _top_module
        #puts "readtarget"
        while {$line != "" && $line >=0} {
            gets $tbfile line
            regexp {(.*): +(.*)} $line fulline key value
            #puts "key $key value $value"
           
            if {$key == "#sourcedir"} {
                set _tb_path $value
            } elseif {$key == "imported-verilog-files"} {
                foreach file $value {
                    $itk_component(vsource) insert 0  $file
                }
                $itk_component(vsource) justify right
            } elseif {$key == "verilog-inc-directories"} {
                foreach file $value {
		    $itk_component(idir) insert 0  $file
		}
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
            } elseif {$key == "verilog-simulator"} {
		set _simulator $value
            } elseif {$key == "link-top-module"} {
		set _top_module $value
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
	variable _new_tb_name

        set editname [$itk_component(tbname) get]
        if {$editname == "" || $editname == $_new_tb_name} {
            tk_messageBox -title "Error" -icon error \
                -message "The testbench must have a valid name." \
                -parent $itk_interior
            return false
        }
        set topname [$itk_component(topname) get]
	if {$topname == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The top module name must be specified" \
                -parent $itk_interior
            return false
        }

        set tmpfile [open "temp.bld" "w"]
        set tbfile [open "sim_tb.bld" "r"]
        while {[gets $tbfile line] >= 0} {
	    if {[regexp {^\[(.*)\]} $line whole tbfull]} {
		if {[regexp {^tcl_tb(.*)} $tbfull whole tbname]} {
		    set tbnamefinal [string range $tbname 3 end]
		    set tbtarget [string range $tbname 0 2]
		    if {$editname != $tbnamefinal} {
			set tbname "tcl_tb$tbname"
			savenontarget $line $tbfile $tmpfile $tbname
		    }
		} else {
		    regsub -all {\[|\]} $tbfull "" tbname
		    set tbnamefinal [string range $tbname 3 end]
		    set tbtarget [string range $tbname 0 2]
		    if {$editname != $tbnamefinal} {
			savenontarget $line $tbfile $tmpfile $tbname
		    } else {
			if  {[regexp {^tcl_tb(.*)} $tbfull]} {continue}
			if {$tbtarget == "emu"} {
			    saveboth $tmpfile $tbnamefinal
			}
		    }
                }
	    }
	}
        close $tmpfile
        close $tbfile
        #file copy -force "temp.bld" "sim_tb.bld"
        file rename -force "temp.bld" "sim_tb.bld"
        return true
    }

    method deletetb {delname} {
        set tmpfile [open "temp.bld" "w"]
        set tbfile [open "sim_tb.bld" "r"]
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
        #file copy -force "temp.bld" "sim_tb.bld"
        file rename -force "temp.bld" "sim_tb.bld"
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

    method create_include_list {{exclude_bdir true}} {
	global   _tb_path
	set first_list [join [$itk_component(idir) get 0 end] " "]
	set inclist {}
	foreach d $first_list {
	    if {$d != "\$BLUESPECDIR/Verilog" || !$exclude_bdir} {
		lappend inclist $d
	    }
	}
	if {[lsearch -exact $inclist $_tb_path] < 0} {
	    lappend inclist $_tb_path
	}
	return $inclist
    }


    method saveboth {file tbname} {
	variable _top_module
	global   _tb_path
        set pln ""
        foreach target {emu sim} {
            set clist [join [$itk_component(vsource) get 0 end] " "]
            if {$_type == "new"} {
                foreach f [getgenc] {
                    lappend clist $_tb_path/$f
                }
            }
#            set first_list [join [$itk_component(idir) get 0 end] " "]
	    set optlist [create_include_list]

	    # foreach d $first_list {
	    # 	if {$d != "\$BLUESPECDIR/Verilog"} {
	    # 	    lappend optlist $d
	    # 	}
	    # }
	    # if {[lsearch -exact $optlist $_tb_path] < 0} {
	    # 	lappend optlist $_tb_path
	    # }
            set newlist {}
            set fulltbname $target$tbname
            append pln "\[tcl_tb$fulltbname\]" \n
            append pln "target-root-directory:  build/$target" \n
            append pln "top-file:              bsv/Bridge.bsv" \n
            append pln "binary-directory:      obj" \n
	    append pln "scemi-parameters-file: scripts/scemi.params" \n
            append pln "build-for: c++" \n
            append pln "scemi-tb" \n
            append pln "uses-tcl" \n
            append pln "c++-header-directory:  build/cpp" \n
            append pln "c++-options: -O3 -DUSINGSCEMI -I \$BLUESPECDIR/SceMi/bsvxactors -I \$BLUESPECDIR/SceMi/readback -L\$BLUESPECDIR/tcllib/lib.linux32 -L\$BLUESPECDIR/tcllib/lib.linux64 -l simtb" \n
	    append pln "c++-uses-readback: true" \n
            append pln "shared-lib:  libbsdebug.so" \n
            append pln "c++-files: \$BLUESPECDIR/tcllib/include/SimTb.cpp \$BLUESPECDIR/tcllib/include/bsdebug_common.cpp -l design" \n
            append pln "log-directory:  logs" \n
            append pln "#sourcedir: $_tb_path" \n
            append pln "" \n



            append pln "\[$fulltbname\]" \n
            append pln "target-root-directory:  build/$target" \n
            append pln "build-for: rtllink" \n
            append pln "verilog-simulator:      $_simulator" \n
#            append pln "extends-target:         c++_base" \n
            append pln "exe-file:               scripts/$tbname" \n
            append pln "link-top-module:        $_top_module" \n
	    append pln "imported-verilog-files: $clist" \n
	    set opt_line "bsc-link-options:    -L \$BLUESPECDIR/tcllib/lib.linux32 -L \$BLUESPECDIR/tcllib/lib.linux64 -scemiTB -l simtb -l design -Xv -sv -Xv +incdir+\$BLUESPECDIR/Verilog"
	    foreach {dir} $optlist {
		set opt_line "$opt_line+$dir"
	    }
	    append pln $opt_line \n
	    append pln "verilog-inc-directories: \$BLUESPECDIR/Verilog $optlist" \n
	    append pln "verilog-lib-directories: \$BLUESPECDIR/Verilog $optlist" \n

            append pln "log-directory:  logs" \n
#            append pln "shared-lib: libbsdebug.so" \n
            append pln "#sourcedir: $_tb_path" \n
            append pln "" \n
        }
        puts $file $pln
    }

    method savetb {} {
	variable _new_tb_name

        set tbname [$itk_component(tbname) get]
        if {$tbname == "" || $tbname == $_new_tb_name } {
            tk_messageBox -title "Error" -icon error \
                -message "The testbench must have a valid name." \
                -parent $itk_interior
            return false
        }
	set topname [$itk_component(topname) get]
	if {$topname == ""} {
            tk_messageBox -title "Error" -icon error \
                -message "The top module name must be specified" \
                -parent $itk_interior
            return false
        }
            
        if {![file exists "sim_tb.bld"]} {
            set tbfile [open "sim_tb.bld" "w"]
        } else {
            set tbfile [open "sim_tb.bld" "a"]
        }
        saveboth $tbfile $tbname
        close $tbfile
        return true
    }

    method setmode {mode target name type topfile name2 sim} {
        set _type $type
        set _target $target
        set advmode $mode
        set _topfile $topfile
        $itk_component(vsource) clear
        $itk_component(idir) clear
	global _tb_name
	global _tb_name_prev
	variable _tb_name_init
	variable _simulator
	variable _top_module
	variable _new_tb_name

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
	    ::updateTbPath
        } else {
	    ## new testbench
	    set _simulator $sim
	    set _top_module ""
	    set _tb_name_init $name2
            $itk_component(tbname) clear
	    set _tb_name $_new_tb_name
	    if {$name != ""} {
		set _tb_name $name
	    }
	    ::updateTbPath
	    trace add variable _tb_name write ::updateTbPath

	    $itk_component(idir) insert 0 "\$BLUESPECDIR/Verilog"

#            $itk_component(tbname) insert 0 $name
            $itk_component(tbname) configure -state normal
            $itk_component(tbdir) configure -state normal
            $itk_component(dirbut) configure -state normal
            $itk_component(dircb) configure -state normal

            # foreach {key l widget attr} [getCfields] {            
            #     if {$key == "c++-compiler"} {
            #         set _Initvals($key) g++
            #     } elseif {$key == "c++-options"} {
            #         set _Initvals($key) "-O3"
            #     } else {
            #         set _Initvals($key) ""
            #     }
            # }
        }
    }

    # method alignCFields {} {
    #     set lab {}
    #     foreach {f l widget attr} [getCfields] {
    #         lappend lab $itk_component($f)
    #     }
    #     lappend lab $itk_component(tbname)
    #     eval iwidgets::Labeledwidget::alignlabels $lab
    # }

    method mkBrowse {field file_type {relative 0}} {
        $itk_component(browse_dia) setMask $file_type
        if {[$itk_component(browse_dia) activate]} {
            set insertfield "[$itk_component(browse_dia) getSelection] "
	    if {$relative} {
		set insertfield [utils::make_related_path $insertfield]
	    }
            $itk_component($field) insert 0 $insertfield
        } else {
            $itk_component($field) insert 0 ""
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




