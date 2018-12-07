package require comm
package require Interp

namespace eval ::sim {

    variable ::sim::noError 1

    proc mysocket {args} { 
	variable ::sim::noError
	set ::sim::noError 0;
	set r [eval ::socket $args]
	set ::sim::noError 1
	return $r}

    proc set_comm_port {num} {
	catch [list ::comm::comm configure -port $num] msg
#	set savedInfo $errorInfo
	if {!$::sim::noError} {
	    puts stderr "LLLLLLLLLLLLLLLLL"
	    set ::sim::noError 1
	    error $msg 

	}
	return 0
    }

    ::comm::comm configure -socketcmd ::sim::mysocket

    # set __result [catch {$::env(BLUESPECDIR)} msg]

    # if {$__result} {
    # 	return -code error "The BLUESPECDIR environment variable is not set."
    # }
}

package provide boot 1.0