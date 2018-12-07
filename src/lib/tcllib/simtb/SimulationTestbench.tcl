package require Redirect

namespace eval ::simtb {

    proc startSimulator {sim_exec port {bscvcd false}} {
	global pid
	set pid 0

	if {$bscvcd} {
	    set r [catch [list exec $sim_exec +bscvcd=tb_dump.vcd +tcl_port=$port &] msg]
	} else {
	    set r [catch [list exec $sim_exec +tcl_port=$port &] msg]
	}
	# puts "R: $r MSG: $msg"
	if {$r} {
	    puts "Error: $::errorInfo"
	    exit
	} else {
	    ::redirect::setRemote $port
	    # puts "REMOTE PORT: $port"
	    set pid $msg
	}
	set trys 0
	while {1} {
#	    set r [catch [list ::redirect::send_cmd 0 0] msg]
	    set r [catch [list ::redirect::check_interp] msg]
	    # puts "TRY: $trys"
	    if {$r == 0} {
		break
	    } else {
		set trys [expr $trys + 1]
		after 100
	    }
	    if {$trys == 1000} {
		puts "Error: Unable to communicate with simulation (pid = $pid). Exiting."
		after 200
		set r [catch [list exec pkill -P $pid] msg]
		set r [catch [list exec kill -TERM $pid] msg]
		exit
	    }
	}
	return $pid
    }
}

package provide SimulationTestbench 1.0