package require cmdline;

package provide CmdOptions 1.0;

namespace eval ::cmdoptions {

    proc getOptions {arglistVar optlist {usage options:}} {
	upvar 1 $arglistVar argv
	
	set opts [cmdline::GetOptionDefaults $optlist result]
	
	set argc [llength $argv]
	set count 0
	set values [list]
	set has_value false
	set next ""
	while {1} {
	    if {[llength $argv] == 0} { break }
	    set count [expr $count + 1]
	    if {$count == 20} { break}
	    set err [cmdline::getKnownOpt argv $opts opt arg]
	    if {$err == 0} { ## encountered a naked value
		lappend values [lindex $argv 0]
		set argv [lrange $argv 1 end]
		set has_value true
		set next [lindex $argv 0]
		continue
	    }
	    if {$err < 0} {
		return -code error "Error: $arg\n [::cmdline::usage $optlist $usage]"
	    }
	    if {$has_value} {
		return -code error "Error: encountered option flag \"$next\" after argument \"[lindex $values end]\"\nAll option flags must come before naked arguments.\n[::cmdline::usage $optlist $usage]"
	    }
	    if {$opt == "help" || $opt == [set ::cmdline::?_sym]} {
		return -code error [usage $optlist $usage]
	    }
	    set result($opt) $arg
	}
	set argv $values
	return [array get result]
    }

    proc setHelpSym {sym} {
	set ::cmdline::?_sym $sym
    }

    proc getArgv0 {} {
	return [::cmdline::getArgv0]
    }

    proc usage {optlist {usage {options:}}} {
	return [::cmdline::usage $optlist $usage]
    }
}

