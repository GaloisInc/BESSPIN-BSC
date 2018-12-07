set rootdir "/export/home/synthesizer/DESIGNS/UTMI/UTMI"
set rtldir  "/export/home/synthesizer/DESIGNS/UTMI/UTMI"
set b       "mkUTMI"
set m       "/work/mkUTMI/mkUTMI"
#set clk     "$m/CLK"

set vfiles  [list ClockDiv.v ClockMux.v FIFO2.v GatedClock.v MakeClock.v SizedFIFO.v SyncBit.v SyncBit05.v SyncBit1.v SyncBit15.v SyncFIFO.v SyncHandshake.v SyncPulse.v SyncRegister.v SyncReset.v SyncResetA.v SyncWire.v RegAligned.v MakeResetA.v GatedClockDiv.v module_centreTable_HS.v mkReceiver.v mkTransmitter.v mkUTMI.v]

set period  2n
set utilization 0.50
set volcano "/tools/magma/lib/tsmc18gl.volcano"
set l       "/tsmc18_6lm"

############################################################
############################################################
#
## Config report timing path *********
config report timing path {PIN_NAME MODEL_NAME DELAY AT GAIN SLEW PIN_CAP PIN_LOAD WIRE_LOAD TOTAL_LOAD FANOUT LENGTH EDGE GAIN}

config hierarchy separator /
config rtl datapath physical off

# downgrade CK-23 and CK-347 ERRORs
config message level CK-23 warning
config message level CK-347 warning
config message level RTL-2027 info

# set include_dir $rtldir/include
# set constraints_dir $rootdir/constraints

if {[data exists /work]} {data delete object /work}

import volcano "$volcano"

config report timing path {PIN_NAME MODEL_NAME DELAY AT GAIN SLEW PIN_CAP PIN_LOAD WIRE_LOAD TOTAL_LOAD FANOUT LENGTH EDGE GAIN}

config hierarchy separator /
config rtl datapath physical off

# find clock cells in the library, and hide them
set _clk_libcells [data find $l CLK*]
foreach i $_clk_libcells {force hide $i}

report arc $l -file arc.rpt

####### IMPORTING RTL ##########
config rtl verilog 2000 on

foreach v $vfiles {import rtl $v}

run rtl elaborate $b -verilog

run bind logical $m $l

check model $m -print unbound_cells -file unbound.rpt

config rtl clockgate off

fix rtl $m

### SETTING SCAN-STYLE ###
# define prior to fix-netlist to avoid Create
# inserting scan-flops
force dft scan style $m muxed_flip_flop

#######################################################
# Make sure we "force keep" hand-instantiated cells
#######################################################

# data loop entity lib_entity $l {
#   data loop models entity_model $entity {
#     data loop upcells model_upcell $models {
#       set model_types [data get $models model_type]
#       if {$model_types == "standard"} {
#         regsub -all % $upcells "" upcell
#         puts "force keep $upcells -model"
#         force keep $upcells -model
#       }
#     }
#   }
# }

# area opt
fix netlist $m $l

###############
# run DFT
###############

# force dft scan clock $m "clk" -group_id 0
# force dft scan chain $m 0 scan_in_0 scan_out_0
# force dft scan control $m scan_en scan_enable
# run dft check $m $l -pre_scan
# run dft scan insert $m $l
# run dft check $m $l

###############
# load SDC
###############
# import sdc $m $constraints_dir/SDC_FILE.tcl -translation sdc_translation.tcl
#force timing clock $clk $period

force timing clock $m/CLK_clk_480_0 2083p -name c0
force timing clock $m/CLK_clk_480_1 2083p -name c1
force timing clock $m/CLK_clk_480_2 2083p -name c2
force timing clock $m/CLK_clk_480_3 2083p -name c3
force timing clock $m/CLK_clk_480_4 2083p -name c4
force timing clock $m/CLK_clk_480_5 2083p -name c5
force timing clock $m/CLK_clk_480_6 2083p -name c6
force timing clock $m/CLK_clk_480_7 2083p -name c7

force timing clock $m/CLK 8333p -name clk

#force timing latency $m/CLK_clk_480_0
force timing latency $m/CLK_clk_480_1 260p -type source
force timing latency $m/CLK_clk_480_2 520p -type source
force timing latency $m/CLK_clk_480_3 781p -type source
force timing latency $m/CLK_clk_480_4 1041p -type source
force timing latency $m/CLK_clk_480_5 1302p -type source
force timing latency $m/CLK_clk_480_6 1562p -type source
force timing latency $m/CLK_clk_480_7 1823p -type source

#set elf /work/SyncBit/SyncBit

#set _false_starts [data find $elf *sSyncReg*]
#set _false_stops  [data find $elf *dSyncReg*]
#force timing break -from *sSyncReg_reg* -to *dSyncReg1_reg*

force timing false -from $m/c1 -to $m/c0
force timing false -from $m/c2 -to $m/c0
force timing false -from $m/c3 -to $m/c0
force timing false -from $m/c4 -to $m/c0
force timing false -from $m/c5 -to $m/c0
force timing false -from $m/c6 -to $m/c0
force timing false -from $m/c7 -to $m/c0

force timing false -from $m/c0 -to $m/clk

force timing false -from $m/*new_gate_reg/Q

#foreach start [data find $m -type net *tallies*] {
#  foreach end [data find $m -type net *outbufrs*] {
#    force timing multicycle -from $start -to $end -cycles 40
#  }
#}

#force timing multicycle -from "$m/dut_the_ovs_reg_tallies*/Q" -to "$m/dut_the_ovs_outbufrs*/D $m/dut_the_ovs_outbufrs*/E" -cycles 40

###############
# run fix time
###############

config hierarchy separator /
config undriven 0
run gate sweep $m -hier

data flatten $m

check model $m -print weird_nets -file weirdnets.rpt
check model $m -print netlist_violations -file netviolations.rpt

# timing optimization
fix time $m $l -rtl

#############################
# floorplanning and fix-cell
#############################
force plan net VDD $m -port VDD -usage power
force plan net VSS $m -port VSS -usage ground

data create floorplan $m fp1
set f ${m}/floorplan:fp1
force floorplan parameters $f -utilization $utilization -aspect_ratio 1
run floorplan apply $m
run plan create pin $m

# import def all -mode netlist -section floorplan -section pin -model_type macro def_name.def $l $l
# run dft scan trace $m

# *HELP* *HACK* we really need run this step, don't we?
fix cell $m $l


#############################################################################
# Report slack, area, and CPU time for the synthesizer system
set worst_slack [query measure slack -worst $m]
set best_area [query measure cellarea $m]
set cpu_time [query cputime]

message info "RSLT-123" "OK: $worst_slack $best_area $cpu_time"
