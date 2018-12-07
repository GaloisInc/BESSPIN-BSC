#!/bin/sh
# the next line restarts using wish \
exec wish "$0" -name "$1" -- "$@"

#puts $argc
#puts $argv
if { $argc == 1 } {
    puts "expected cmd name"    
    exit;
}
set name [lindex $argv 0]
puts $argv
for { set i 1 } { $i < $argc } { incr i } {
    set opt [lindex $argv $i]
    regsub {^-} $opt "" opt
    puts "Options: $i = $opt"
    switch -exact $opt {
        {tkName} { 
            incr i 
            set n2 [lindex $argv $i]
            puts "got name $i $n2"
            tk appname $n2
            set APPNAME $n2
        }
        default {} 
    }
}


switch -exact $name {
    nWave { exec mkdir -p nWaveLog;
            exec touch nWaveLog/turbo.log }
    debussy { exec mkdir -p debussyLog;
            exec touch debussyLog/turbo.log }
    default {}
}

set fout [open $argv.cmdlog "w"]
fconfigure $fout -buffering line

set textbox [text .tb]
pack $textbox

set cnt 0

proc dump { args } {
    global fout 
    global textbox
    global cnt 
    incr cnt

    set str [join [concat $cnt ":" $args " "]]
    $textbox insert end ${str}\n
    puts $fout $str
    $textbox see end
}


# over write the unknown proc, so everythng goes there
proc  unknown { args } {
    dump $args 
}



proc wvGetCurrentWindow {} {
    global APPNAME
    return $APPNAME
    
}

proc wvRaiseWindow {} {
    dump wvRaiseWindow
    raise .
}

# Novas uses temp files  display them here
proc wvRestoreSignal { file } {
    global fout 
    global textbox
    global cnt 

    dump "wvRestoreSignal" -->

    if { [catch "open $file RDONLY"  tfile] } { 
        puts stderr "Could not open $tfile"
        return 
    }
    while { ! [eof $tfile] } {
        gets $tfile line
        #puts stderr $line
        $textbox insert end ${line}\n
        puts $fout $line
    }
    $textbox see end
}

proc wvGetActiveFile {} {
    global fout
    return $fout 
}
