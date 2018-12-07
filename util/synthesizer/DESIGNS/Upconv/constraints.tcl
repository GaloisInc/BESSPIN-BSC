## Ed Czeck   March 2005

## Bus timing contrains for ST Bus  upsize converter.

#   input  CLK;
#   input  RST_N;

set late  [format "%gp" [expr $periodA * 0.8]]
set med   [format "%gp" [expr $periodA * 0.4]]
set early [format "%gp" [expr $periodA * 0.2]]


## Bring tcl up to 1990 with map function
proc map { func lst } {
    set ret [list]
    foreach elem $lst {
        lappend ret [$func $elem]
    }
    return $ret
}

proc getname { net } {
    [data get $net name ]
}

## given a pattern list return a list of nets matching the pattern
proc plist_to_list { plist } {
    global m
    set ret [list]
    foreach pattern $plist {
        set sigs [data find $m $pattern -type mpin]
        set ret [concat  $ret $sigs]
    }
    return $ret 
}



## Initiator early inputs  -- NONE
## Initiator medium inputs -- NONE
## Initiator late inputs

set init_late_inpts { init_in_req init_in_eop init_in_lck \
                          init_in_r_gnt init_in_addr*     \
                          init_in_data* init_in_be*       \
                          init_in_opc*  init_in_src*      \
                          init_in_tid*  init_in_pri*      \
                      }


## Initiator early outputs
set init_early_outs { init_r_req init_r_lck init_r_opc* init_r_src*  init_r_data* init_r_tid* init_r_eop}

## Initiator med outputs
set init_med_outs { init_gnt }

## Initiator late outputs -- NONE


## Destination side, early inputs -- NONE
## Destination side med inputs -- 
set targ_med_inpts { dest_in_gnt }
set targ_late_inpts { dest_in_r_req dest_in_r_eop dest_in_r_lck  \
                          dest_in_r_opc* dest_in_r_src* \
                          dest_in_r_tid*  dest_in_r_data* \
                      }


## destination size early outs
set targ_early_outs {  dest_req  dest_eop  dest_lck dest_r_gnt \
                           dest_addr* dest_data* dest_be*   \
                           dest_opc*  dest_src*  dest_tid*  dest_pri*  \
                       }



set ili [ plist_to_list $init_late_inpts]
set imo [ plist_to_list  $init_med_outs]
set ieo [ plist_to_list $init_early_outs]

set dmi [ plist_to_list $targ_med_inpts]
set dli [ plist_to_list $targ_late_inpts]
set deo [ plist_to_list $targ_early_outs ]


## actual magma constraints
force timing delay $clk $ili -time $late  -replace

force timing check $imo $clk -time $med  -replace -type setup_rising
force timing check $ieo $clk -time $early  -replace -type setup_rising

## 
force timing delay $clk $dmi -time $med  -replace
force timing delay $clk $dli -time $late  -replace

force timing check $deo $clk -time $early  -replace -type setup_rising
