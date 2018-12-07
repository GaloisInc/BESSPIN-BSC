## Copyright (c) 2010-2011, Bluespec, Inc.  ALL RIGHTS RESERVED

package require Tk
package require Bluetcl
package require Iwidgets

namespace eval MemoryGui {
    variable PutsCmd puts
    variable AfterCmd ""
    variable AfterDelay 400
    variable MemInitial [list 0 20 0xAABBCCDDEEFF1122 0x111111]
    variable WinName "memoryX"

    proc mkMemoryControl { parent } {
        variable MemInitial
        variable WinName
        variable AfterCmd

        set top $parent.$WinName
        if { $top == "..$WinName" } { set top ".$WinName" }
        set win $top

        ttk::labelframe $win -text "Memory Access"
        ttk::label $win.a -text "Address/Count"
        ttk::label $win.d -text "Data/Incr"

        set entries [list al ah dd di]

        foreach e $entries ini $MemInitial {
            ttk::entry $win.$e -width 20 -justify r
            $win.$e insert end $ini
        }

        set buttons [list br bw bd bf]
        set bnames  [list "Read" "Write" "Dump" "Fill"]
        set bcmds   [list read write dump fill]
        foreach b $buttons n $bnames c $bcmds {
            ttk::button $win.$b -width 14 -text $n  -command [namespace code "buttonCmd $win $c"]
        }

        set order [list \
                       [list a al ah br bd ] \
                       [list d dd di bw bf ]\
                      ]
        set r -1
        foreach rs $order {
            incr r
            set c -1
            foreach cs $rs {
                incr c
                grid $win.$cs -row $r -column $c
            }
        }
    

        set AfterCmd [after 1000 [namespace code "update"]]
        bind $win <Destroy> +[namespace code destroy_callback]

        return $win

    }

    proc update {} {
        variable AfterCmd 
        variable AfterDelay
        variable PutsCmd
        set d [memoryCmd responseQ]
        if { $d != "" } {
            formatDisplay $d
        }

        # update
        set AfterCmd [after $AfterDelay [namespace code "update"]]
    }

    proc buttonCmd { win but } {
        set al [$win.al get]
        set al [expr $al]
        set ah [$win.ah get]
        set ah [expr $ah]
        set dd [$win.dd get]
        set dd [expr $dd]
        set di [$win.di get]
        set di [expr $di]
        switch $but {
            read { memoryCmd readQ $al}
            write {memoryCmd writeQ $al $dd}
            dump { memoryCmd dumpQ $al $ah}
            fill { memoryCmd fillQ $al $ah $dd $di}
            default {puts stderr "Unxpected button command: $but"}
        }
    }


    proc memoryCmd { args } {
        variable PutsCmd
        set cmd [lindex $args 0]
        if { [catch "uplevel #1 bsdebug::memory $args" err] } {
            $PutsCmd "Caught Error: on $args"
            $PutsCmd " --> [join $err]"
            set err ""
        } else {
            if { $cmd != "responseQ" } {
                # $PutsCmd [join [concat $err]]
            }
        }
        return $err
    }


    proc formatDisplay { data } {
        variable PutsCmd
        lassign $data addr vdata
        while { $vdata != {} } {
            set os [format "0x%08x :" $addr]
            for { set i 0 } { $i < 4 } { incr i } {
                set vdata [lassign $vdata dat]
                set hi [string range $dat 0 7 ]
                set lo [string range $dat 8 15]
                set os [format "%s  %s %s" $os $hi $lo]
            }
            $PutsCmd $os
            set addr [expr $addr + $i]
        }
    }
    proc set_puts_cmd {cmd} {
        variable PutsCmd
        set PutsCmd $cmd
    }

    proc destroy_callback {} {
        variable AfterCmd
        catch "after cancel $AfterCmd"
    }

}

package provide MemoryGui 1.0
