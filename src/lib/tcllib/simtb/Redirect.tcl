package provide Redirect 1.0

package require comm
package require Interp

namespace eval ::redirect {

    variable remotePort 0

    proc setRemote {port} {
	variable remotePort
	set remotePort $port
    }

    proc setLocal {} {
	variable remotePort
	set remotePort 0
    }

    ################################################################################
    ###
    ################################################################################

    proc scemi {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [list eval bsdebug::scemi $args]]
	} else {
	    return [eval bsdebug::scemi $args]
	}
    }

    proc netlist {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [list eval bsdebug::netlist $args]]
	} else {
	    return [eval bsdebug::netlist $args]
	}
    }

    proc rdbk {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [list eval bsdebug::rdbk $args]]
	} else {
	    return [eval bsdebug::rdbk $args]
	}
    }

    proc emu {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [list eval bsdebug::emu $args]]
	} else {
	    return [eval bsdebug::emu $args]
	}
    }

    proc send {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [lindex $args 0]]
	} else {
	    return [eval [lindex $args 0]]
	}
    }

    proc send_cmd {args} {
	variable remotePort
	if {$remotePort} {
	    return [::comm::comm send $remotePort [list eval ::interp::send $args]]
	} else {
	    return [eval ::interp::send $args]
	}
    }

    proc check_interp {} {
	return [::redirect::send [list expr 1 - 1]]
    }
}

