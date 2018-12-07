# Copyright 2009 Altera Corporation. All rights reserved.  
# Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
# functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
# documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
# Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
# that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
# distributors.  Please refer to the applicable agreement for further details.




set script_dir [file dirname [info script]]

source "$script_dir/ddr2_v10_1_parameters.tcl"

global ::GLOBAL_ddr2_v10_1_corename


set 	sequencer_cpu 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu:the_sequencer_cpu
set 	sequencer_cpu_oci 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_nios2_oci:the_sequencer_cpu_nios2_oci
set 	sequencer_cpu_oci_break 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_nios2_oci_break:the_sequencer_cpu_nios2_oci_break
set 	sequencer_cpu_ocimem 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_nios2_ocimem:the_sequencer_cpu_nios2_ocimem
set 	sequencer_cpu_oci_debug 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_nios2_oci_debug:the_sequencer_cpu_nios2_oci_debug
set 	sequencer_cpu_wrapper 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_jtag_debug_module_wrapper:the_sequencer_cpu_jtag_debug_module_wrapper
set 	sequencer_cpu_jtag_tck 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_jtag_debug_module_tck:the_sequencer_cpu_jtag_debug_module_tck
set 	sequencer_cpu_jtag_sysclk 	${::GLOBAL_ddr2_v10_1_corename}_sequencer_cpu_jtag_debug_module_sysclk:the_sequencer_cpu_jtag_debug_module_sysclk
set 	sequencer_cpu_oci_path 	 [format "%s|%s" $sequencer_cpu $sequencer_cpu_oci]
set 	sequencer_cpu_oci_break_path 	 [format "%s|%s" $sequencer_cpu_oci_path $sequencer_cpu_oci_break]
set 	sequencer_cpu_ocimem_path 	 [format "%s|%s" $sequencer_cpu_oci_path $sequencer_cpu_ocimem]
set 	sequencer_cpu_oci_debug_path 	 [format "%s|%s" $sequencer_cpu_oci_path $sequencer_cpu_oci_debug]
set 	sequencer_cpu_jtag_tck_path 	 [format "%s|%s|%s" $sequencer_cpu_oci_path $sequencer_cpu_wrapper $sequencer_cpu_jtag_tck]
set 	sequencer_cpu_jtag_sysclk_path 	 [format "%s|%s|%s" $sequencer_cpu_oci_path $sequencer_cpu_wrapper $sequencer_cpu_jtag_sysclk]
set 	sequencer_cpu_jtag_sr 	 [format "%s|*sr" $sequencer_cpu_jtag_tck_path]


set_false_path -from [get_keepers *$sequencer_cpu_oci_break_path|break_readreg*] -to [get_keepers *$sequencer_cpu_jtag_sr*]
set_false_path -from [get_keepers *$sequencer_cpu_oci_debug_path|*resetlatch]     -to [get_keepers *$sequencer_cpu_jtag_sr[33]]
set_false_path -from [get_keepers *$sequencer_cpu_oci_debug_path|monitor_ready]  -to [get_keepers *$sequencer_cpu_jtag_sr[0]]
set_false_path -from [get_keepers *$sequencer_cpu_oci_debug_path|monitor_error]  -to [get_keepers *$sequencer_cpu_jtag_sr[34]]
set_false_path -from [get_keepers *$sequencer_cpu_ocimem_path|*MonDReg*] -to [get_keepers *$sequencer_cpu_jtag_sr*]
set_false_path -from *$sequencer_cpu_jtag_sr*    -to *$sequencer_cpu_jtag_sysclk_path|*jdo*
set_false_path -from sld_hub:*|irf_reg* -to *$sequencer_cpu_jtag_sysclk_path|ir*
set_false_path -from sld_hub:*|sld_shadow_jsm:shadow_jsm|state[1] -to *$sequencer_cpu_oci_debug_path|monitor_go
