package require Waves 2.0
package require Bluetcl
package require types

itcl::class semu_waves { 
    common _Viewer ""
    common _vcd_file "./dump1.vcd"


    proc create_viewer {} {
        set _Viewer [Waves::create_viewer]
    }
    
    proc start_viewer {} {
        if {$_Viewer == "" } {
            loadBluespecPackages
            set _Viewer [Waves::create_viewer]
            $_Viewer configure -nonbsv_hierarchy ""
            $_Viewer start
            update
            $_Viewer load_dump_file $_vcd_file
            update
        }
        if {![$_Viewer isRunning]} {
            loadBluespecPackages
            $_Viewer configure -nonbsv_hierarchy ""
            $_Viewer start
            update
            $_Viewer load_dump_file $_vcd_file
            update
        }
        return $_Viewer
    }

    proc attach_viewer {viewer} {
        $_Viewer attach $viewer
    }

    proc get_viewer {} {return $_Viewer}

    proc reload_vcd {} {
        $_Viewer reload_dump_file
    }

    proc  send_to_viewer {signal_name bitn} {
	set type "Bit#(${bitn})"
	#puts "send2 $type $signal_name"
	$_Viewer send_typed_signals [list [list $type $signal_name]]
    }

    proc loadBluespecPackages {} {
        set packs [list Prelude PreludeBSV]
        types::import_package 0 $packs
    }   
    
    set _Viewer [Waves::create_viewer]
}
