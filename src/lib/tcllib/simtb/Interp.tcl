package provide Interp 1.0

set ::channelIn 0
set ::channelRecv 0

proc ::handle_emu {args} {
    upvar #0 ::channelIn channel_in
    variable ::emuChannel
    if {$channel_in != 0} {
	set channel_number [expr ($channel_in >> 32) - 1]
	if {$channel_number == 7033} {
	    set value [expr $channel_in & 0xFFFFFFFF]
	    if {$value == 0} {
		bsdebug::rdbk stop
		after 100
	    }
	    uplevel #0 {set ::channelRecv 1}
	}
    }
}

trace add variable ::channelIn write "::handle_emu"
    
namespace eval ::interp {

    global ::channelOut  = 0;
    global ::channelIn   = 0;
    global ::channelRecv = 0;

    variable ::emuChannel = 7033;

    proc send {channel value {force false}} {
	global ::channelOut
	set count 0
	while {[expr $::channelOut >> 32] != 0 && $count != 512} {
	    after 10
	    set count [expr $count + 1]
	}
	if {!$force && $count == 512} {
	    puts "Error: (::interp::send) Unable to send to channel $channel"
	    return 1
	}
	set ::channelOut [expr (($channel + 1) << 32) | $value]
	return 0
    }

    proc flush {channel} {
	global ::channelOut
	set ::channelOut 0
	return 0
    }

    proc sendx {channel value {force false}} {
	global ::channelOut
	set ::channelOut 0
	after 100
	set ::channelOut [expr (($channel + 1) << 32) | $value]
	return 0
    }
}






