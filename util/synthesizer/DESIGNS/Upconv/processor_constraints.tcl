#
# identify clocks, nets, and I/Os
#

set clk [data find $m CLK -type mpin]

set all_inputs [list]
foreach input [data list {model_pin -direction in} $m] {
#   XXX only works because $clk is a single clock
    if { [data get $input name] != [data get $clk name] } {
        lappend all_inputs $input
    }
}
set all_outputs [data list {model_pin -direction out} $m]
set all_inputs_outputs [concat $all_inputs $all_outputs]
set all_nets [list]
foreach net [data list {model_net -net_use signal} $m] {
#   XXX only works because $clk is a single clock
    if { [data get $net name] != [data get $clk name] } {
        lappend all_nets $net
    }
}

#
# constrain clock cycle and set ideal behavior
#

force timing clock $clk $period

#force timing clock $clk 3.8n -slew 0.2n
#force timing drive -resistance 0 $clk
# force timing delay on all nets causes errors
#force timing delay $clk $all_nets -time 0


#
# constrain I/O delays, capacitance, and timing model
#

#force timing delay $clk $all_inputs -time 0.5n
#force timing check $all_outputs $clk -time 0.5n

#force load capacitance $all_outputs 0.2p

#force timing drive -model $l/INV/INVX16 $all_inputs -outpin Y

#
# constrain transition time on external pins
#
#force limit slew $all_inputs_outputs 0.7n -type max_rise
#force limit slew $all_inputs_outputs 0.7n -type max_fall

