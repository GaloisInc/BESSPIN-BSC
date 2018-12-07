#  Copyright Bluespec Inc. 2009-2010

# Tcl/Tk package for building a graphical interface to the

namespace eval ::ProbeGui {

    package require BSDebug
    package require Waves 2.0
    package require types
    package require Iwidgets 4.0

    # Top level public interface
    #  mkProbePanel -- builds, but does not pack the probe window.
    #  statusLoop  -- starts the status loop, must be called after the window is built
    #  shutdown    -- close wave form viewer

    # Arrays keys by probe name
    variable Enable_button
    variable Disable_button
    variable File_button
    variable Sim_button
    variable to_file 1
    variable Send_button
    variable NameToNum
    variable NameEnabled
    variable BSVType
    variable ProbePath
    #0-normal 1-capture 2-trigger 3-newcapture 4-scan
    #5-powermeter 6-powerchanges 7-poweriso 8-powergroup 9-powersrc
    variable ProbeType
    variable TriggerEnableCount

    variable probe_frameTOP
    variable wave_button
    variable wave_reload_button
    variable vcd_file
    variable updatetime 1000
    variable Viewer ""

    # ----------
    # The probe panel

    # Probe Functionality procedures

    proc mkProbePanel {tl {vcdfilename ""} } {
        variable NameToNum
        variable BSVType
        variable ProbePath
        variable ProbeType
        variable vcd_file
        variable probe_frameTOP

        set probe_frameTOP [ttk::labelframe $tl.probe_panel -relief ridge -text "Probe Control" -width 300]
        set probe_frameS [iwidgets::scrolledframe $probe_frameTOP.sf -vscrollmode dynamic -hscrollmode dynamic -width 300 -height 250]
        set probe_frame [$probe_frameS childsite]

	set vcd_file [bsdebug::probe query_vcdfile]

	if {$vcd_file == ""} {
	    set vcd_file $vcdfilename
	}

	set num_probe [bsdebug::probe query -1]
	set probe_list [list]
	set capture_list [list]
	set trigger_list [list]
	set scan_list [list]
	set power_list [list]
        set i 0
	while {$i < $num_probe} {
	    set probe_info [bsdebug::probe query $i]
	    set probe_name [lindex $probe_info 0]
	    set probe_name [eliminate_space $probe_name]
	    set enabled [lindex $probe_info 3]
	    set probe_type [lindex $probe_info 4]
	    set probe_num [lindex $probe_info 5]

	    set BSVType($probe_name) [lindex $probe_info 2]
	    set ProbePath($probe_name) [lindex $probe_info 6]
	    set path $ProbePath($probe_name)
            set NameToNum($probe_name) $probe_num
            set ProbeType($probe_name) $probe_type

	    # Normal Probe
	    if {$probe_type == 0} {
		lappend probe_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 1 || $probe_type == 3} {
		lappend capture_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 2} {
		lappend trigger_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 4} {
		lappend scan_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 5} {
		lappend power_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 8} {
		lappend power_list [list $probe_name $probe_num $enabled]
	    } elseif {$probe_type == 9} {
		lappend power_list [list $probe_name $probe_num $enabled]
	    }
	    incr i
	}

	if {[llength $probe_list] > 0} {
	    set probe_list [lsort -dictionary -index 0 $probe_list]
	    set probe_label [ttk::label $probe_frame.probelbl -text "Probes"]
	    pack $probe_label -padx 5 -pady 10
	    foreach info $probe_list {
		set probe_name [lindex $info 0]
		set enabled [lindex $info 2]
		catch "mkProbeWidget $probe_frame $probe_name $enabled"
	    }
	}

	if {[llength $capture_list] > 0} {
	    set capture_list [lsort -dictionary -index 0 $capture_list]
	    set probe_label [ttk::label $probe_frame.capturelbl -text "Captures"]
	    pack $probe_label -padx 5 -pady 10
	    foreach info $capture_list {
		set probe_name [lindex $info 0]
		set enabled [lindex $info 2]
		catch "mkProbeWidget $probe_frame $probe_name $enabled"
	    }
	}

	if {[llength $trigger_list] > 0} {
	    set trigger_list [lsort -dictionary -index 0 $trigger_list]
	    set trigger_label [ttk::label $probe_frame.triggerlbl -text "Triggers"]
	    pack $trigger_label -padx 5 -pady 10
	    foreach info $trigger_list {
		set probe_name [lindex $info 0]
		set enabled [lindex $info 2]
		catch "mkTriggerWidget $probe_frame $probe_name $enabled"
	    }
	}

	if {[llength $scan_list] > 0} {
	    set scan_list [lsort -dictionary -index 0 $scan_list]
	    set scan_label [ttk::label $probe_frame.scanlbl -text "Cosim"]
	    pack $scan_label -padx 5 -pady 10
	    foreach info $scan_list {
		set probe_name [lindex $info 0]
		catch "mkScanWidget $probe_frame $probe_name"
	    }
	}

	set n [llength $power_list]
	if {[llength $power_list] > 0} {
	    set power_list [lsort -dictionary -index 0 $power_list]
	    set power_label [ttk::label $probe_frame.powerlbl -text "Power"]
	    pack $power_label -padx 5 -pady 10
	    foreach info $power_list {
		set probe_name [lindex $info 0]
		set enabled [lindex $info 2]
		catch "mkProbeWidget $probe_frame $probe_name $enabled"
	    }
	}

	set wframe [mkWaveWidget $probe_frameTOP $probe_list $capture_list $trigger_list $power_list]

        pack $wframe -side left -anchor nw
        pack $probe_frameS -expand 1 -fill both

        # load the bluespec package -- continue on failures...
        catch loadBluespecPackages
        Waves::open_xhost
        bind $probe_frameTOP <Destroy> +::ProbeGui::destroy
        return $probe_frameTOP
    }

# ----------
    proc destroy {} {
        variable updatetime
        set updatetime 0
    }
    proc shutdown {} {
        variable Viewer
        if { $Viewer ne "" } {
            $Viewer close
        }
    }

    proc enable_probe {pname} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
	variable NameEnabled

        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)
	set NameEnabled($pname) 1

        $dbutton state !disabled
        $ebutton state disabled

	#puts "Enable probe $pname $NameToNum($pname)"
        bsdebug::probe enable $NameToNum($pname)
    }

    proc disable_probe {pname} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
	variable NameEnabled

	set NameEnabled($pname) 0
        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)

        $ebutton state !disabled
        $dbutton state disabled

        bsdebug::probe disable $NameToNum($pname)
    }

    proc enable_all_probes {probe_list} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
	variable NameEnabled

	if {[llength $probe_list] > 0} {
	    set probe_list [lsort -dictionary -index 0 $probe_list]
	    foreach info $probe_list {
		set pname [lindex $info 0]
		set NameEnabled($pname) 1

		set ebutton $Enable_button($pname)
		set dbutton $Disable_button($pname)

		$dbutton state !disabled
		$ebutton state disabled

		bsdebug::probe enable $NameToNum($pname)
	    }
	}
    }

    proc disable_all_probes {probe_list} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
	variable NameEnabled

	if {[llength $probe_list] > 0} {
	    set probe_list [lsort -dictionary -index 0 $probe_list]
	    foreach info $probe_list {
		set pname [lindex $info 0]
		set NameEnabled($pname) 0

		set ebutton $Enable_button($pname)
		set dbutton $Disable_button($pname)

		$ebutton state !disabled
		$dbutton state disabled

		bsdebug::probe disable $NameToNum($pname)
	    }
	}
    }

    proc enable_trigger {pname} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
        variable TriggerEnableCount

        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)

        # $ebutton state disabled
        $dbutton state !disabled

        bsdebug::probe enable $NameToNum($pname)

	incr TriggerEnableCount($pname)
    }

    proc disable_trigger {pname} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
        variable TriggerEnableCount

        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)

        $ebutton state !disabled
	$dbutton state disabled

        bsdebug::probe disable $NameToNum($pname)
	set TriggerEnableCount($pname) 0
    }

    proc enable_all_triggers {trigger_list} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
        variable TriggerEnableCount

	if {[llength $trigger_list] > 0} {
	    set probe_list [lsort -dictionary -index 0 $trigger_list]
	    foreach info $trigger_list {
		set pname [lindex $info 0]

		set ebutton $Enable_button($pname)
		set dbutton $Disable_button($pname)

		$dbutton state !disabled
		$ebutton state disabled

		bsdebug::probe enable $NameToNum($pname)

		incr TriggerEnableCount($pname)
	    }
	}
    }

    proc disable_all_triggers {trigger_list} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
        variable TriggerEnableCount

	if {[llength $trigger_list] > 0} {
	    set probe_list [lsort -dictionary -index 0 $trigger_list]
	    foreach info $trigger_list {
		set pname [lindex $info 0]

		set ebutton $Enable_button($pname)
		set dbutton $Disable_button($pname)

		$ebutton state !disabled
		$dbutton state disabled

		bsdebug::probe disable $NameToNum($pname)

		set TriggerEnableCount($pname) 0
	    }
	}
    }

    proc send_probe {probe_name} {
        variable Send_button
        variable BSVType
        variable ProbePath
        variable NameToNum
        variable Viewer

	set type $BSVType($probe_name)
	set path $ProbePath($probe_name)
	set newprobe [string first "/" $path]
	if {$newprobe == -1} {
	    set sig "/main/$path"
	} else {
	    set sig "$path"
	}

	set probe_num $NameToNum($probe_name)
	set probe_info [bsdebug::probe query_subnets $probe_num]
	set has_subnets [lindex $probe_info 0]

	set power_probe [bsdebug::probe query_power $probe_num]
	set total_power_id [bsdebug::probe query_total_power_id]
	set power_group [bsdebug::probe query_power_group $probe_num]
	set power_source [bsdebug::probe query_power_source $probe_num]

	if {$has_subnets > 0} {
	    set subnets [lindex $probe_info 1]
	    foreach n [split $subnets ","] {
		set p1 [expr [string first ":" $n] + 1]
		set p2 [expr [string first ":" $n] - 1]
		set bitn [string range $n $p1 [string length $n]]
		set type "Bit#(${bitn})"
		set n1 [string range $n 0 $p2]
		set sig1 "${sig}/${n1}"
                $Viewer send_typed_signals [list [list $type $sig1]]
	    }
	} else {
	    if {$power_probe == 1} {
		if {$power_source == 1 || $power_group == 1 || $probe_num == $total_power_id} {
		    set lastslash [string last "/" $sig]
		    set l [expr [string length $sig] - 1]
		    if {$lastslash != $l} {
			set sig1 "${sig}/${probe_name}"
		    } else {
			set sig1 "${sig}${probe_name}"
		    }
		} else {
		    set sig1 "${sig}/${probe_name}_Power"
		}
		set type "real"
		#puts "send $type $sig1 ${probe_name} $probe_num $total_power_id"
		$Viewer send_typed_signals [list [list $type $sig1]]

	    } else { #comment out the else if you want the counters info

		set lastslash [string last "/" $sig]
		set l [expr [string length $sig] - 1]
		if {$lastslash != $l} {
		    set sig1 "${sig}/${probe_name}"
		} else {
		    set sig1 "${sig}${probe_name}"
		}
		set type $BSVType($probe_name)
		#puts "send2 $type $sig1"
		$Viewer send_typed_signals [list [list $type $sig1]]
	    }
	}

        $Send_button($probe_name) state disabled
    }

    proc send_all_enabled_probes {probe_list} {
        variable NameToNum
	variable NameEnabled

	if {[llength $probe_list] > 0} {
	    foreach info $probe_list {

		set pname [lindex $info 0]
		set enabled $NameEnabled($pname)

		if {$enabled == 1} {
		    catch "send_probe $pname"
		}
	    }
	}
    }

    proc mkProbeWidget {pn probe_name enabled} {
        variable Enable_button
        variable Disable_button
        variable Send_button
        variable NameToNum
	variable NameEnabled

        set wname $NameToNum($probe_name)
        set probe_frame [ttk::frame $pn.frame_$wname]
        set probe_text [ttk::label $probe_frame.l -text $probe_name -justify left -width 0 -anchor e]
        set probe_enable_button  [ttk::button $probe_frame.enable  -text "Enable"         -command "ProbeGui::enable_probe $probe_name"]
        set probe_disable_button [ttk::button $probe_frame.disable -text "Disable"        -command "ProbeGui::disable_probe $probe_name"]
        set probe_send_button    [ttk::button $probe_frame.send    -text "Send To Viewer" -command [list ProbeGui::send_probe $probe_name]]
	set NameEnabled($probe_name) $enabled

        set Enable_button($probe_name) $probe_enable_button
        set Disable_button($probe_name) $probe_disable_button
	set Send_button($probe_name) $probe_send_button

	if {$enabled == 0} {
	    $probe_enable_button state !disabled
	    $probe_disable_button state disabled
	    $probe_send_button state disabled
	} else {
	    $probe_enable_button state disabled
	    $probe_disable_button state !disabled
	    $probe_send_button state disabled
	}

        pack $probe_text -padx 5 -side left
        pack $probe_enable_button -padx 5 -side left
        pack $probe_disable_button -padx 5 -side left
        pack $probe_send_button -padx 5 -side left
        pack $probe_frame -side top -anchor ne
        #pack $probe_enable_label -padx 5 -side left
    }


    proc mkScanWidget {pn probe_name} {
        variable Enable_button
        variable Disable_button
        variable File_button
        variable Sim_button
        variable Run_button
        variable to_file
        variable NameToNum

        set wname $NameToNum($probe_name)
        set probe_frame [ttk::frame $pn.frame_$wname]
        set probe_text [ttk::label $probe_frame.l -text $probe_name -justify left -width 0 -anchor e]
        set probe_enable_button  [ttk::button $probe_frame.enable  -text "Enable"            -command "ProbeGui::enable_scan $probe_name"]
        set probe_disable_button [ttk::button $probe_frame.disable -text "Disable"           -command "ProbeGui::disable_scan $probe_name"]
        set probe_file_button    [ttk::button $probe_frame.file    -text "Send To File"      -command [list ProbeGui::set_file_dest $probe_name]]
        set probe_sim_button     [ttk::button $probe_frame.sim     -text "Send To Simulator" -command [list ProbeGui::set_sim_dest $probe_name]]
        set probe_run_button     [ttk::button $probe_frame.run     -text "Run Simulator"     -command [list ProbeGui::run_sim $probe_name]]

        set Enable_button($probe_name) $probe_enable_button
        set Disable_button($probe_name) $probe_disable_button
	set File_button($probe_name) $probe_file_button
	set Sim_button($probe_name) $probe_sim_button
	set Run_button($probe_name) $probe_run_button

        $probe_disable_button state disabled
	if ($to_file) {
	    $probe_file_button    state disabled
	    $probe_sim_button     state !disabled
	} else {
	    $probe_file_button    state !disabled
	    $probe_sim_button     state disabled
	}

        pack $probe_text -padx 5 -side left
        pack $probe_enable_button -padx 5 -side left
        pack $probe_disable_button -padx 5 -side left
        pack $probe_run_button -padx 5 -side left
        pack $probe_frame -side top -anchor ne
    }

    proc eliminate_space {name} {
	regsub -all { } $name {_} result
	return $result
    }

    proc run_sim {pname} {
	variable Run_button

	set rbutton $Run_button($pname)

	$rbutton state disabled
        . configure -cursor watch
	update
	# run cosim in background to allow event updates
	if { [catch {open "|./cosim.exe +bscvcd" r+} csim] } {
	    bgerror $csim
	    $rbutton state !disabled
	    . configure -cursor {}
	} else {
	    fileevent $csim readable "ProbeGui::run_sim_bg $csim $pname"
	}
    }

    proc run_sim_bg {chan pname} {
	variable Run_button

	if { [eof $chan] } {
	    fileevent $chan readable ""
	    set rbutton $Run_button($pname)
	    $rbutton state !disabled
	    . configure -cursor {}
	} else {
	    fcopy $chan stdout
	}
	update
    }

    proc set_file_dest {pname} {
        variable File_button
        variable Sim_button
        variable to_file

	set to_file 1

        set fbutton $File_button($pname)
        set sbutton $Sim_button($pname)

        $fbutton state disabled
        $sbutton state !disabled
    }

    proc set_sim_dest {pname} {
        variable File_button
        variable Sim_button
        variable to_file

	set to_file 0

        set fbutton $File_button($pname)
        set sbutton $Sim_button($pname)

        $fbutton state !disabled
        $sbutton state disabled
    }

    proc enable_scan {pname} {
        variable Enable_button
        variable Disable_button
        variable NameToNum
	variable File_button
        variable Sim_button

        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)
        set fbutton $File_button($pname)
        set sbutton $Sim_button($pname)

        $dbutton state !disabled
        $ebutton state disabled
        $fbutton state disabled
        $sbutton state disabled

        bsdebug::probe enable $NameToNum($pname)
    }

    proc disable_scan {pname} {
	variable Enable_button
        variable Disable_button
        variable NameToNum
	variable File_button
        variable Sim_button
        variable to_file

        set ebutton $Enable_button($pname)
        set dbutton $Disable_button($pname)
        set fbutton $File_button($pname)
        set sbutton $Sim_button($pname)

        $dbutton state disabled
        $ebutton state !disabled

	if ($to_file) {
	    $fbutton    state disabled
	    $sbutton     state !disabled
	} else {
	    $fbutton    state !disabled
	    $sbutton     state disabled
	}

        bsdebug::probe disable $NameToNum($pname)
    }


    proc mkTriggerWidget {pn probe_name enabled} {
        variable Enable_button
        variable Disable_button
        variable TriggerEnableCount
        variable NameToNum

        set wname $NameToNum($probe_name)
        set probe_frame [ttk::frame $pn.frame_$wname]
        set probe_text [ttk::label $probe_frame.l -text $probe_name -justify left -width 0 -anchor e]
        set probe_enable_button  [ttk::button $probe_frame.enable  -text "Enable"  -command "ProbeGui::enable_trigger  $probe_name"]
        set probe_disable_button [ttk::button $probe_frame.disable -text "Disable" -command "ProbeGui::disable_trigger $probe_name"]

	# The number of outstanding enable
	set TriggerEnableCount($probe_name) 0
	set probe_enable_label [ttk::label $probe_frame.elabel_$probe_name -textvar ::ProbeGui::TriggerEnableCount($probe_name) -justify left -width 4 -anchor e]

        set Enable_button($probe_name)  $probe_enable_button
        set Disable_button($probe_name) $probe_disable_button

	if {$enabled == 0} {
	    $probe_enable_button state !disabled
	    $probe_disable_button state disabled
	} else {
	    $probe_enable_button state disabled
	    $probe_disable_button state !disabled
	}

        pack $probe_text -padx 5 -side left
        pack $probe_enable_button -padx 5 -side left
        pack $probe_disable_button -padx 5 -side left
        pack $probe_enable_label -padx 5 -side left
        pack $probe_frame -side top -anchor ne
    }

    proc mkWaveWidget {parent probe_list capture_list trigger_list power_list} {
        variable wave_button
        variable wave_reload_button

	set pn [frame $parent.ww -relief flat -borderwidth 0]
	set wave_button [ttk::button $pn.wave -text "Wave Viewer" -command "ProbeGui::start_viewer"]
	set wave_reload_button [ttk::button $pn.wave_reload -text "Reload VCD" -command "ProbeGui::reload_vcd"]
	set en_dis_buttons [list]

	# Enable and disable buttons
	if {[llength $probe_list] > 0} {
	    set en_all_probes [ttk::button $pn.en_all_prb -text "Enable Probes" -command "ProbeGui::enable_all_probes {$probe_list}"]
	    set dis_all_probes [ttk::button $pn.dis_all_prb -text "Disable Probes" -command "ProbeGui::disable_all_probes {$probe_list}"]
	    set send_all_en_probes [ttk::button $pn.send_all_en_prb -text "Send Enabled Probes" -command "ProbeGui::send_all_enabled_probes {$probe_list}"]
	    lappend en_dis_buttons $en_all_probes
	    lappend en_dis_buttons $dis_all_probes
	    lappend en_dis_buttons $send_all_en_probes
	}

	if {[llength $capture_list] > 0} {
	    set en_all_captures [ttk::button $pn.en_all_capt -text "Enable Captures" -command "ProbeGui::enable_all_probes {$capture_list}"]
	    set dis_all_captures [ttk::button $pn.dis_all_capt -text "Disable Captures" -command "ProbeGui::disable_all_probes {$capture_list}"]
	    lappend en_dis_buttons $en_all_captures
	    lappend en_dis_buttons $dis_all_captures
	}

	if {[llength $trigger_list] > 0} {
	    set en_all_triggers [ttk::button $pn.en_all_trig -text "Enable Triggers" -command "ProbeGui::enable_all_triggers {$trigger_list}"]
	    set dis_all_triggers [ttk::button $pn.dis_all_trig -text "Disable Triggers" -command "ProbeGui::disable_all_triggers {$trigger_list}"]
	    lappend en_dis_buttons $en_all_triggers
	    lappend en_dis_buttons $dis_all_triggers
	}

	if {[llength $power_list] > 0} {
	    set en_all_powers [ttk::button $pn.en_all_pwr -text "Enable Power Meters" -command "ProbeGui::enable_all_probes {$power_list}"]
	    set dis_all_powers [ttk::button $pn.dis_all_pwr -text "Disable Power Meters" -command "ProbeGui::disable_all_probes {$power_list}"]
	    set send_all_en_powers [ttk::button $pn.send_all_en_pwr -text "Send Enabled Meters" -command "ProbeGui::send_all_enabled_probes {$power_list}"]
	    lappend en_dis_buttons $en_all_powers
	    lappend en_dis_buttons $dis_all_powers
	    lappend en_dis_buttons $send_all_en_powers
	}

	if {[llength $en_dis_buttons] > 0} {
            pack $wave_button -padx 5 -pady 2 -side top -fill y -expand 1
            pack $wave_reload_button -padx 5 -pady 2 -side top -fill y -expand 1
	}

	$wave_button state !disabled

        foreach button $en_dis_buttons {
            $button configure -width 15
            pack $button -padx 5 -pady 2 -side top -fill y -expand 1
        }
        return $pn
    }

    proc start_viewer {} {
        variable Send_button
        variable wave_button
        variable vcd_file
        variable Viewer

        $wave_button state disabled
        bsdebug::probe query_timestamp
        set Viewer [Waves::create_viewer]
        $Viewer configure -nonbsv_hierarchy ""
	$Viewer start
	$Viewer load_dump_file $vcd_file
        $wave_button state disabled
        foreach {name but} [array get Send_button] {
	    $but state !disabled
        }
    }

    proc reload_vcd {} {
        variable Viewer

	puts "Reloading vcd"
        bsdebug::probe query_timestamp
	$Viewer reload_dump_file
	puts "Done reloading"
    }

    proc loadBluespecPackages {} {
        variable BSVType
        set packs [list Prelude PreludeBSV]
        foreach {unusedprobename t} [array get BSVType] {
            while { [regexp {([A-Za-z][^ :#\(\),]*)::(.*)} $t unused ty x] } {
                lappend packs $ty
                set t $x
            }
        }
        types::import_package 0 $packs
    }

    proc statusLoop {} {
        if { [catch statusLoopInternal err] } {
            puts stderr "Probe status loop failed,  interface will not respond"
            puts stderr $err
	    return
        }
    }
    proc statusLoopInternal {} {
        variable TriggerEnableCount
        variable Disable_button
        variable updatetime
        variable wave_button
        variable probe_frameTOP
        variable Viewer

        if {$updatetime == 0} { return }
	if {! [winfo exists $probe_frameTOP]} { return }

	set num_probe [bsdebug::probe query -1]
	set i 0

	while {$i < $num_probe} {
	    set probe_info [bsdebug::probe query $i]
	    set probe_name [lindex $probe_info 0]
	    set probe_enabled [lindex $probe_info 3]
	    set probe_type [lindex $probe_info 4]
	    if {$probe_type == 2} {
		set TriggerEnableCount($probe_name) $probe_enabled
                set dbutton $Disable_button($probe_name)
		if {$probe_enabled == 0} {
		    $dbutton state disabled
		} else {
		    $dbutton state !disabled
                }
	    }
	    incr i
        }
	if { $Viewer eq "" } {
	    $wave_button state !disabled
        } elseif { [$Viewer isRunning ] } {
	    $wave_button state disabled
	} else {
	    $wave_button state !disabled
	}

	after $updatetime ProbeGui::statusLoop
    }

    proc mkPowerRatePanel {tl {ratefilename ""} } {

    }
}

package provide ProbeGui 1.0
