package provide read_hdl 1.0

namespace eval read_hdl {

    proc get_verilog_ports { top_file top_mod } {

        set modname {}

        set fh [open $top_file "r"]

	proc doErr { str } {
	    close fh
	    set msg "Error in reading $top_file: $str"
	    return -code error $msg
	}

        proc substituteParameters { str } {
            regsub -all {([a-zA-Z]\w*)} $str {$params(\0)}
        }

	proc addParam { name val } {
	    upvar params p
	    # XXX Check for duplicates?
	    set p($name) $val
	}

	proc addPort { name type width } {
	    upvar ports p
	    # XXX Check for duplicates
	    set p($name) [list $type $width]
	}

        while { [gets $fh line] != -1 } {
            #puts "Line -- $line"
            # Kill comments
            set line [regsub {//.*$} $line ""]
            switch -regexp -matchvar ms -- $line {
                {\mmodule\s+(\w+)} {
                    #puts "matched module [lindex $ms 1]"
		    if { $modname eq {} } {
		        set modname [lindex $ms 1]
		    } else {
			doErr "unexpected module in module"
		    }
                }
                {\mparameter\s+(\w+)\s*=\s*\"(\w+)\"} {
                    # String parameter
		    if { $modname eq $top_mod } {
			set name [lindex $ms 1]
			set val  [lindex $ms 2]
			addParam $name $val
			#puts "$name $val -- local -- [info local]"
		    }
                }
                {\mparameter\s+(\w+)\s*=(.+);} {
		    if { $modname eq $top_mod } {
			set name [lindex $ms 1]
			set val  [substituteParameters [lindex $ms 2]]
			addParam $name $val
			#puts "$name $val -- local -- [info local]"
		    }
                }
                {\m(input|output|inout)\s+(\w+)}  {
		    if { $modname eq $top_mod } {
			#puts "matched [lrange $ms 1 end]"
			addPort [lindex $ms 2] [lindex $ms 1] 1
		    }
                }
                {\m(input|output|inout)\s+\[(.+)\s*:\s*(.+)\s*\]\s+(\w+)}  {
		    if { $modname eq $top_mod } {
			set upper [substituteParameters [lindex $ms 2]]
			set lower [substituteParameters [lindex $ms 3]]
			set wid [expr ($upper) - ($lower) + 1]
			addPort [lindex $ms 4] [lindex $ms 1] $wid
		    }
                }
                {\mendmodule} {
		    if { $modname eq $top_mod } {
			break
		    } else {
			set modname {}
		    }
		}

            } ; # switch

        } ; # while line

	if { $modname eq "" } {
	    doErr "Did not find module `$top_mod'"
	}

	close $fh
	return [array get ports]

    } ; # get_verilog_ports

} ; # namespace eval read_hdl
