#lappend auto_path $env(BLUESPECDIR)/tcllib/workstation

package require types
package require Iwidgets 4.0
package require Itk

package require Bluetcl

package require semu_advanced
package require semu_pin
package require SemuInit
package require read_hdl

package provide semu_config 1.0

 #My::showtime "before fonts"
#My::debug "before fonts"
# fonts::set_colours
# fonts::initialize
 #My::showtime "after fonts"

#catch "itcl::delete class semu_config"
itcl::class semu_config {
    inherit iwidgets::Dialog
    variable _Filevals
    variable _Initvals
    variable _File ""
    variable _rbinput
    variable _Oldvals 
    common ver_sim ""
    variable _toprel
    variable _board
    variable _Boards

    common checkimg   [::fonts::get_image checked]
    common uncheckimg [::fonts::get_image unchecked]

    #Array (field )
    common fields [list \
                       top-file \
                       top-module \
                       design-clock-frequency \
                       board \
                       verilog-simulator \
                       xilinx-readback \
                      ]

    common vfields [list \
                        verilog-lib-directories \
                        verilog-inc-directories \
                        bsc-link-options \
                        verilog-define \
                        imported-vhdl-files \
                        xilinx-xdc-supplement-file \
                        ]
    
    common cfields [list \
                        c++-compiler \
                        c++-define \
                        c++-files \
                        c++-options \
                        c++-libraries \
                        ]
    
    common emtargets [list \
                          Xilinx_KC705 kc705 \
                          Xilinx_VC707  vc707\
                          Xilinx_VC709  vc709\
                          Dini_DNPCie_10G_K7_LL dn10gk7ll\
                          Dini_DNV7F2A   dnv7f2a\
                          HyperSilicon_VeriTiger-DH2000TQ  dh2000tq\
                          HyperSilicon_HyperHPC-B2000T  b2000t\
                          Xilinx_ML605 ml605\
			  ProDesign_FM-XC7V2000T-R2 pdv72kr2\
                         ]

    # common emtargets [list \
    #                       kc705  \
    #                       vc707  \
    #                       10gk7ll \
    #                       dh2000tq \
    #                       b2000t \
    #                       ml605 \
    #                      ]

    constructor {args} {
        lappend -args -modality application -title "semu Initial Build"\
            -height 400 -width 600
         # construct the dialog      
        eval itk_initialize $args
        array set _Initvals [list] 
        array set _Oldvals [list] 
        array set _Boards [get_boards]
        
        itk_component add browse_dia {
            file_dialog::create .bwin
        } {}

        itk_component add semu_advanced {
           semu_advanced::create .adv
        } {}
        
        itk_component add semu_pin {
            semu_pin::create .pin
        } {}

        itk_component add iframe {
            frame $itk_interior.iframe
        } {}
        
        addFields
        alignFields

        wm title $itk_component(hull) "Configure Hardware"
 #        wm geometry $itk_component(hull)  400x400+200+100

         pack $itk_component(iframe) -expand 1 -fill x 
         pack $itk_component(fframe) -expand true -fill x
         pack $itk_component(advanced) -pady 5 


         $itk_component(hull) hide Help
#         $itk_component(hull) hide Apply

        buttonconfigure OK -text "Save" -underline 0 -command [itcl::code $this doFinish]
        buttonconfigure Apply -text "Edit Pin" -underline 0 -command [itcl::code $this menuEditPin] 
        buttonconfigure Cancel -underline 0
        #My::showtime "semu_config done"
    }

     destructor {}

     method getFile {file} {
         set _File $file
     } 

    method doFinish {} {
        foreach {f} [getFields] {
            if {$_Initvals($f) == "" } {
                if {$f == "verilog-simulator" || $f == "board"} {
                    continue
                } else {
                    tk_messageBox -title "Error" -icon error \
                        -message "$f cannot be blank" -type ok -parent $itk_interior
                    #                raise .setup
                    return
                }
            }
        }
        set speed $_Initvals(design-clock-frequency)
        if {$speed < 5 || $speed > 500} {
            tk_messageBox -title "Invalid speed" -type ok \
                -icon error -message "Frequency must be between 5 and 500 Mhz"\
                -parent $itk_interior
            return 
        }
        #puts "going to savefinish"
        savefinish
    }
    
    method savefinish {} {
        set change False
        set newfile True
        #puts "savefinish $_Oldvals(top-file)"
        if {$_Oldvals(top-file) != ""} { 
            #puts "top file not blank"
            set newfile False
            if {[checkChanges]} {
                set change [askSaveChanges]
            } 
        }
        #puts "savefinish change $change newfile $newfile"
        if {$change || $newfile} {

            writeFile $_File
            semu_viewer::execute_in_shell_wait "build -i $_File" [itcl::code execDone "Project File Created"]
        }
        
        if {[file exists project.bld] == 0} {
            semu_viewer::execute_in_shell_wait "build -i $_File" [itcl::code execDone "Project File Created"]
        }
        writeBoard

        .view checkCanBuild
        [itcl::code $this deactivate 1] 
    }
    
    method writeBoard {} {
        #puts "in writeBoard"
        if {$_Initvals(board) == ""} {
            set board ""
        } else {
            set code $_Initvals(board)
            set board $_Boards($code)
        }
        #puts "code $code"
        #parray _Boards
        semu_build::writeBuild $board
    }


    proc execDone {msg} {
        semu_viewer::display_message $msg false
    }
    
    method checkChanges {} {
        #puts "checkChanges"
        set change False
        
        foreach {f} [getFields] {
	    # if {$change} {
            #     #puts "check1 $change"
	    #     break
	    # }
            #puts "$f  $_Initvals($f)  $_Oldvals($f)"
            if  {$_Initvals($f) != $_Oldvals($f)} {
		set change True
                #puts "check3 $change $f"
	    }
        }
        foreach {f} [getVfields] {
	    if {$change} {
                #puts "check4 $change"
		break
	    }
            if  {$_Initvals($f) != $_Oldvals($f)} {
                set change True
                #puts "check6 $change $f"
             }
        }
        return $change
    }

    method askSaveChanges {} {
        set action [tk_messageBox -title "Save Configuration Changes" -icon question \
                        -message "Configuration changes will require rebuilding project." \
                        -detail "Yes to save changes, No to discard changes" \
                        -type yesno -parent $itk_interior]
        switch -- $action {
            no {
                set change False
            }
            yes {
                set change True
            }
        }
        
        return $change
    }

     private method addFields {} {
         set w $itk_component(iframe)
         itk_component add fframe {
             iwidgets::labeledframe $w.fframe -labelpos nw -labeltext "Design"
         } {}
         set fframe [$itk_component(fframe) childsite]

         foreach {f} [getFields] {
             addEntryField $fframe.$f $f $fframe
         }

        # puts "all done add Entry"
        # addMem $fframe
         
         itk_component add advanced {
              ttk::button $w.advanced -text "Advanced Options..." -underline 0\
                  -command "[itcl::code $this mkAdvanced]"
          } {}
      }

     private method addEntryField {pw f w} {
          #puts "Field $f"
          if {$f == "top-file" } {
              itk_component add g {
                  frame $w.g
              }

              itk_component add $f {
                  iwidgets::entryfield $itk_component(g).$f -highlightthickness 0 \
                      -width 20 -labelpos w \
                      -labeltext $f -textvariable [itcl::scope _Initvals($f)]
              }
              itk_component add bb { 
                  ttk::button $itk_component(g).bb -text "Browse..." -width 8 -underline 0 \
                      -command "$this mkBrowse $f *.v"
              } {}       
              
              itk_component add dirbut {
                  checkbutton $itk_component(g).dirbut -text "relative" \
                      -variable [itcl::scope _toprel]
              } {}
              $itk_component(dirbut) select

              pack $itk_component($f)  -side left -fill x -expand true
              pack $itk_component(bb) -side left  -padx 5 -fill x
              pack $itk_component(dirbut) -side left -padx 5 -fill x -expand true
              pack $itk_component(g) -side top -fill x -expand true

          } elseif {$f == "design-clock-frequency"} {
              itk_component add $f {
                  iwidgets::entryfield $pw -highlightthickness 0 \
                      -labelpos w -width 20 \
                      -labeltext "frequency in Mhz" -textvariable [itcl::scope _Initvals($f)]
              } {}
              pack $itk_component($f) -pady 5 -anchor w  -expand true -fill x
          } elseif {$f == "verilog-simulator"} {
              itk_component add $f {
                  iwidgets::combobox $pw -labeltext $f -textvariable [itcl::scope _Initvals($f)] 
              } {}
              foreach i [get_semu_simulators] {
                  eval $itk_component($f) insert list end $i
              }
              pack $itk_component($f) -pady 5 -anchor w -fill x -expand true 

          } elseif {$f == "xilinx-readback"} {
              itk_component add $f {
                  checkbutton $pw -text "Build for signal debugging" -onvalue true\
                      -offvalue false -variable [itcl::scope _Initvals($f)]
              } {}
              pack $itk_component($f) -pady 5 -anchor w
          } elseif {$f == "board"} {
              itk_component add $f {
                  iwidgets::combobox $pw -labeltext "FPGA Board" \
                      -textvariable [itcl::scope _Initvals($f)]
              } {}
              foreach {i b} [get_boards] {
                  eval $itk_component($f) insert list end $i
              }
              pack $itk_component($f) -pady 5 -anchor w -fill x -expand true
          } else {
              itk_component add $f {
                  iwidgets::entryfield $pw -highlightthickness 0 -labelpos w \
                      -width 20 \
                      -labeltext $f -textvariable [itcl::scope _Initvals($f)]
              } {}
              pack $itk_component($f) -pady 5 -anchor w  -fill x -expand true
          }
      }


    private method mkAdvanced {} {
        #puts "mkAdvanced"
        set values [array get _Initvals]
       # $itk_component(semu_advanced) buildVFields
        $itk_component(semu_advanced) getVFile $values
        $itk_component(semu_advanced) setmode "config" "" "" ""
        $itk_component(semu_advanced) activate
        array set _Initvals [$itk_component(semu_advanced) returnFile]
    }        

    # method initCFields {} {
    #     set values [array get _Initvals]
    #     $itk_component(semu_advanced) getCFile $values
    #     $itk_component(semu_advanced) setmode "config" "" "" ""
    # }

    # private method addPin {} {
    #     set w $itk_component(iframe)
    #     itk_component add pframe {
    #         iwidgets::labeledframe $w.pframe -labelpos nw -labeltext "Pin Specifications"
    #     } {}
    #     set pframe [$itk_component(pframe) childsite]
            
    #     # set topm [itcl::scope _Initvals(top-module)]
    #     # set pinfile $topm.pin
    #     # puts "topm: $topm pinfile: $pinfile"

    #     itk_component add pf {
    #         iwidgets::entryfield $pframe.pf -highlightthickness 0 -labelpos e \
    #             -width 20 -labeltext ".pin" \
    #             -textvariable [itcl::scope _Initvals(top-module)] \
    #             -state readonly -borderwidth 0 -justify right -highlightcolor gray80
    #     } {}
        
    #     pack $itk_component(pf) -pady 5 -anchor w -fill x -expand true
        
    #     itk_component add pbb {
    #         iwidgets::buttonbox $pframe.pbb
    #     }
    #     $itk_component(pbb) add gen -text "Generate" -underline 0 -command "[itcl::code $this doGenerate]"
    #     $itk_component(pbb) add edit -text "Edit"  -underline 0 -command "[itcl::code $this doEdit]"
    #     pack $itk_component(pbb) -fill x -expand true
    # }

    method menuEditPin {} {
        if {![file exists project.cfg]} {
            #puts "going to doFinish"
            doFinish
        } else {
            if {[checkChanges]} {
                set action [tk_messageBox -title "Save Configuration Changes?" -icon question \
                                -message "Do you want to save the Hardware Configuration?" \
                                -type yesno -parent $itk_interior]
                
                if {$action == "yes"} {
                    #puts "going to doFinish"
                    doFinish
                }
            }
        }
        [itcl::code $this deactivate 1]
        editPin
    }

    method editPin {} {
        set pinstatus [checkPinFile]
        if {$pinstatus == "complete" || $pinstatus == "part"} {
            set answer [tk_messageBox -title "Generate Pin Specification" -icon info \
                            -message "Pin File already exists. Press Yes to edit, No to overwrite" \
                            -type yesnocancel ]
            switch -- $answer {
                yes {
		    # Copy the existing pin-file to a temp file
		    set top_mod $_Initvals(top-module)
		    set pin_file "${top_mod}.pin"
		    set temp_pin_file "${top_mod}.pin-temp"
		    file copy -force $pin_file $temp_pin_file
                    doEdit
                }
                no {
		    # Generate a new temp pin-file from the Verilog
                    if {[genPins]} {
                        doEdit
                    } else {
                        return
                    }
                }
                cancel {
                    return
                }
            }
        } else {
	    # Generate a new temp pin-file from the Verilog
            if {[genPins]} {
                doEdit
            } else {
                return
            }
        }
    }

   
    method checkPinFile {} {
        if {[file exists $_Initvals(top-module).pin]} {
            set pinfile [open $_Initvals(top-module).pin "r+"]
            gets $pinfile line
            if {[regexp {^//complete} $line]} {
                return complete
            } else {
                return part
            }
        } else {
            return none
        }
    }

    
    method genPins {} {
	set top_vfile $_Initvals(top-file)
	set top_mod $_Initvals(top-module)
	set temp_pin_file "${top_mod}.pin-temp"

	    set cmd "read_hdl::get_verilog_ports $top_vfile $top_mod"
	    if { [catch $cmd ports] } {
		set msg $ports
		if { $msg eq {} } {
		    set msg "Cannot identify the Verilog module's ports"
		}
		tk_messageBox -title "Error" -icon error \
		    -message $msg -type ok \
		    -parent $itk_interior
		return false
	    }

	    # Write an initial pin file in which all ports are listed
	    # as basic input or output on an undeclared clock CLK.
	    # This will be read by the next step, which will replace it
	    # with a real pin file.
	    if { [catch "open $temp_pin_file w" fh] } {
		tk_messageBox -title "Error" -icon error \
		    -message "Can not create pin file: $fh" -type ok \
		    -parent $itk_interior
		return false
	    }

	    puts $fh "port"
	    foreach { pname pinfo } $ports {
		set ptype [lindex $pinfo 0]
		set pwidth [lindex $pinfo 1]
		if { $ptype eq {input} || $ptype eq {output} } {
		    puts $fh " $ptype $pname $pwidth CLK"
		}
		# otherwise, it's 'inout', which we ignore
	    }
	    puts $fh "endport"

	    catch [close $fh]
	    return true
    }
 
    method readyEdit {msg} {
        semu_viewer::display_message $msg false
        #doEdit
    }

    method doEdit {} {
	set pin_file "$_Initvals(top-module).pin"
	set temp_pin_file "$_Initvals(top-module).pin-temp"

        $itk_component(semu_pin) loadTempPins $_Initvals(top-module)
        $itk_component(semu_pin) activate

	# Remove the temp file
        file delete -force $temp_pin_file
	
	# If the pin-file changed, update any dependent state
        if {[file exists $pin_file]} {

	    # The GUI may have unsaved state, so refresh from the saved file
	    $itk_component(semu_pin) readFile $pin_file
	   
	    set memory [$itk_component(semu_pin) getMem]
	    #puts "pin returned memory $memory"

	    # In the cfg file, we don't know what type of memory to specify;
	    # the 'build -i' call will fill it in with the appropriate value
	    # for the board.  In the cfg file, the existence of the directive
	    # or not is how we specify whether to use the bridge with DDR
	    # access or not.
	    set need_rebuild false
	    if {$memory} {
		set _Initvals(sodimm-style) ""
		if { ! [info exists _Oldvals(sodimm-style)] } {
		    set need_rebuild true
		}
	    } else {
		catch {unset _Initvals(sodimm-style) ""}
		if { [info exists _Oldvals(sodimm-style)] } {
		    set need_rebuild true
		}
	    }

	    if { $need_rebuild } {
		writeFile $_File
		semu_viewer::execute_in_shell_wait "build -i $_File" [itcl::code execDone "Project File Created"]
	    }

	} ; # if pinfile exists
    }
    
    method getFields {} { return $fields }
    method getVfields {} { return $vfields }

     private method alignFields {} {
         #puts "alignFields"
         set lab {}
         foreach {f} [getFields] {
             if {$f == "xilinx-readback"} {
                 continue
             } else {
                 lappend lab $itk_component($f)
             }
         }
         eval iwidgets::Labeledwidget::alignlabels $lab
     }

    proc create {pathname args} {
        uplevel #0 semu_config $pathname -modality none $args
    }

 
    method newFile {name dir } {
        set _File [file join $dir project.cfg]
        set _Initvals(top-file) ""
        set _Initvals(top-module) ""
#        set _Initvals(verilog-search-flags) ""
        set _Initvals(design-clock-frequency) 10
        set _Oldvals(top-file) ""
        #set _Initvals(sodimm-style) ""
        #set _Oldvals(sodimm-style) ""
        set _Initvals(xilinx-readback) true
        set _Initvals(board) ""
        set _Oldvals(board) ""

        foreach  {f} [getVfields] {
            set _Initvals($f) ""
        }
        set _Initvals(verilog-define) "BSV_TIMESCALE=1ns/1ps"
        #parray _Initvals
        #parray _Oldvals
    }
    
    
    method openInFile { } {
         if {[file exists $_File]} {
             readFile $_File
         } 
        if {[file exists project.blt]} {
            readBFile
        }
     }

    method readBFile {} {
        set bfile [open project.blt "r+"]
        while {[gets $bfile line] >= 0} {
            if {[regexp {(.*): (.*)} $line fulline key value] } {
                if {$key == "board"} {
                    foreach {text board} [array get _Boards] {
                        if {$board == $value} {
                            set value $text
                        }
                        set _Initvals($key) $value
                        set _Oldvals($key) $value
                    }
                }
            }
        }
    }
    

    method checkStatus {} {
        if {$_Initvals(top-module) == ""} {
            return true
        } else {
            return false
        }
    }


    method readFile {filename} {
        set projfile [open $filename "r+"]
        while {[gets $projfile line] >= 0} {
            if {[regexp {^#} $line]} {
            } elseif {[regexp {^\[} $line]} {
                
            } elseif {[regexp {(.*): (.*)} $line fulline key value] } {
                set _Initvals($key) $value
                set _Oldvals($key) $value
            }
        }
	# initialize new directives that might not be in the previous
	# .cfg file
	foreach {directive dflt} {xilinx-readback true board "" imported-vhdl-files "" xilinx-xdc-supplement-file ""} {
	    if {![info exists _Oldvals($directive)]} {
		set _Initvals($directive) $dflt
		set _Oldvals($directive) $dflt
	    }
	}
    }

    method configSpeed {} {
        return $_Initvals(design-clock-frequency)
    }

    method writeFile {filename} {
        #parray _Initvals
        file delete -force $filename
        set fh [open $filename "w"]
        set pln ""
        append pln "\[Default\]" \n
        foreach key [array names _Initvals *] {
            #puts "$key $_Initvals($key)"
            if {$key == "c++-files" || $key == "c++-libraries" || $key == "c++-options"} {
                continue
            } elseif {$key == "board"} {
                continue
            } else {
                append pln  "$key: $_Initvals($key)" \n
            }
        }
        puts $fh $pln
        close $fh
    }
    
  

    method mkBrowse {field file_type} {
        $itk_component(browse_dia) setMask $file_type
        if {[$itk_component(browse_dia) activate]} {
            $itk_component($field) clear 
            if {$_toprel} {
                set f [utils::make_related_path [$itk_component(browse_dia) getSelection]]
            } else {
                set f [$itk_component(browse_dia) getSelection]
            }
            $itk_component($field) insert 0 $f
        } else {
            $itk_component($field) insert 0 ""
        }
    }    

    proc get_boards {} {return $emtargets}

    proc get_semu_simulators {} {
        global env
        set notsemu_valid [list iverilog xsim isim]
        set sims [glob -nocomplain $env(BLUESPECDIR)/bin/bsc_build_*]
        array set Valid [list]
        foreach s $sims {
            set scr [file tail $s]
            if { [catch "exec $s detect" res] } {
                
            } else {
                set called [lindex $res 0]
                if {[lsearch -exact $notsemu_valid $called] < 0} {
                    set Valid($called) 1
                }
            }
            set final [lsort [array names Valid]]
            if {[llength $final] == 0 } {
                set final [list "None Found"]
            }
            
            set simulators $final
        }
        return $simulators
    }


}
