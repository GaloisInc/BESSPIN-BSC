# Copyright 2009 Altera Corporation. All rights reserved.  
# Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
# functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
# documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
# Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
# that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
# distributors.  Please refer to the applicable agreement for further details.

#####################################################################
#
# THIS IS AN AUTO-GENERATED FILE!
# -------------------------------
# If you modify this files, all your changes will be lost if you
# regenerate the core!
#
# FILE DESCRIPTION
# ----------------
# This file contains the timing constraints for the UniPHY memory
# interface.
#    * The timing parameters used by this file are assigned
#      in the ddr2_v10_1_timing.tcl script.
#    * The helper routines are defined in ddr2_v10_1_pin_map.tcl
#
# NOTE
# ----

set script_dir [file dirname [info script]]

source "$script_dir/ddr2_v10_1_parameters.tcl"
source "$script_dir/ddr2_v10_1_timing.tcl"
source "$script_dir/ddr2_v10_1_pin_map.tcl"

load_package ddr_timing_model

####################
#                  #
# GENERAL SETTINGS #
#                  #
####################

# This is a global setting and will apply to the whole design.
# This setting is required for the memory interface to be
# properly constrained.
derive_clock_uncertainty

# Debug switch. Change to 1 to get more run-time debug information
set debug 0

# All timing requirements will be represented in nanoseconds with up to 3 decimal places of precision
set_time_format -unit ns -decimal_places 3

# Determine if entity names are on
set entity_names_on [ are_entity_names_on ]

	##################
	#                #
	# QUERIED TIMING #
	#                #
	##################

	set io_standard "$::GLOBAL_ddr2_v10_1_io_standard CLASS I"

	# This is the peak-to-peak jitter on the whole read capture path
	set DQSpathjitter [expr [get_io_standard_node_delay -dst DQDQS_JITTER -io_standard $io_standard -parameters [list IO $::GLOBAL_ddr2_v10_1_io_interface_type] -in_fitter]/1000.0]
	set DQSpathjitter_setup_prop [expr [get_io_standard_node_delay -dst DQDQS_JITTER_DIVISION -io_standard $io_standard -parameters [list IO $::GLOBAL_ddr2_v10_1_io_interface_type] -in_fitter]/100.0]

	set tJITper [expr [get_io_standard_node_delay -dst MEM_CK_PERIOD_JITTER -io_standard $io_standard  -parameters [list IO $::GLOBAL_ddr2_v10_1_io_interface_type] -in_fitter -period $t(CK)]/2000.0]

	##################
	#                #
	# DERIVED TIMING #
	#                #
	##################

	# These parameters are used to make constraints more readeable

	# Half of memory clock cycle
	set half_period [ round_3dp [ expr $t(CK) / 2.0 ] ]

	# Half of reference clock
	set ref_half_period [ round_3dp [ expr $t(refCK) / 2.0 ] ]

	# Minimum delay on data output pins
	set data_output_min_delay [ round_3dp [ expr - $t(DH) - $t(WL_DCD) - $t(WL_JITTER) - $t(WL_PSE) - $board(intra_DQS_group_skew) - $SSN(rel_pullin_o) - $ISI(DQ)/2 - $ISI(DQS)/2]]

	# Maximum delay on data output pins
	set data_output_max_delay [ round_3dp [ expr $t(DS) + $t(WL_DCD) + $t(WL_JITTER) + $t(WL_PSE) + $board(intra_DQS_group_skew) + $SSN(rel_pushout_o) + $ISI(DQ)/2 + $ISI(DQS)/2]]

	# Maximum delay on data input pins
	set data_input_max_delay [ round_3dp [ expr $t(DQSQ) + $DQSpathjitter*$DQSpathjitter_setup_prop + $board(intra_DQS_group_skew) + $SSN(rel_pushout_i) ]]

	# Minimum delay on data input pins
	set data_input_min_delay [ round_3dp [ expr -$t(DCD) - $DQSpathjitter*(1.0-$DQSpathjitter_setup_prop) - $board(intra_DQS_group_skew) - $SSN(rel_pullin_i) ]]

	# Minimum delay on address and command paths
	set ac_min_delay [ round_3dp [ expr - $t(IH) -$fpga(tPLL_JITTER) - $fpga(tPLL_PSERR) - $board(intra_addr_ctrl_skew) + $board(addresscmd_CK_skew) - $ISI(addresscmd_hold) ]]

	# Maximum delay on address and command paths
	set ac_max_delay [ round_3dp [ expr $t(IS) +$fpga(tPLL_JITTER) + $fpga(tPLL_PSERR) + $board(intra_addr_ctrl_skew) + $board(addresscmd_CK_skew) + $ISI(addresscmd_setup) ]]

if { $debug } {
	post_message -type info "SDC: Computed Parameters:"
	post_message -type info "SDC: --------------------"
	post_message -type info "SDC: half_period: $half_period"
	post_message -type info "SDC: data_output_min_delay: $data_output_min_delay"
	post_message -type info "SDC: data_output_max_delay: $data_output_max_delay"
	post_message -type info "SDC: data_input_min_delay: $data_input_min_delay"
	post_message -type info "SDC: data_input_max_delay: $data_input_max_delay"
	post_message -type info "SDC: ac_min_delay: $ac_min_delay"
	post_message -type info "SDC: ac_max_delay: $ac_max_delay"

		post_message -type info "SDC: Using Timing Models: Micro"
}

# This is the main call to the netlist traversal routines
# that will automatically find all pins and registers required
# to apply timing constraints.
# During the fitter, the routines will be called only once
# and cached data will be used in all subsequent calls.
if { ! [ info exists ddr2_v10_1_sdc_cache ] } {
	set ddr2_v10_1_sdc_cache 1
	ddr2_v10_1_initialize_ddr_db ddr2_v10_1_ddr_db
} else {
	if { $debug } {
		post_message -type info "SDC: reusing cached DDR DB"
	}
}

# If multiple instances of this core are present in the
# design they will all be constrained through the
# following loop
set instances [ array names ddr2_v10_1_ddr_db ]
foreach { inst } $instances {
	if { [ info exists pins ] } {
		# Clean-up stale content
		unset pins
	}
	array set pins $ddr2_v10_1_ddr_db($inst)

	set prefix $inst
	if { $entity_names_on } {
		set prefix [ string map "| |*:" $inst ]
		set prefix "*:$prefix"
	}

	#####################################################
	#                                                   #
	# Transfer the pin names to more readable variables #
	#                                                   #
	#####################################################

	set dqs_pins $pins(dqs_pins)
	set dqsn_pins $pins(dqsn_pins)
	set q_groups [ list ]
	foreach { q_group } $pins(q_groups) {
		set q_group $q_group
		lappend q_groups $q_group
	}
	set all_dq_pins [ join [ join $q_groups ] ]

	set ck_pins $pins(ck_pins)
	set ckn_pins $pins(ckn_pins)
	set add_pins $pins(add_pins)
	set ba_pins $pins(ba_pins)
	set cmd_pins $pins(cmd_pins)
	set ac_pins [ concat $add_pins $ba_pins $cmd_pins ]
	set dm_pins $pins(dm_pins)
	set all_dq_dm_pins [ concat $all_dq_pins $dm_pins ]

	set pll_ref_clock $pins(pll_ref_clock)
	set pll_afi_clock $pins(pll_afi_clock)
	set pll_ck_clock $pins(pll_ck_clock)
	set pll_write_clock $pins(pll_write_clock)
	set pll_ac_clock $pins(pll_ac_clock)
	set pll_afi_half_clock $pins(pll_afi_half_clock)
	set pll_avl_clock $pins(pll_avl_clock)
	set pll_config_clock $pins(pll_config_clock)

	set dqs_in_clocks $pins(dqs_in_clocks)
	set dqs_out_clocks $pins(dqs_out_clocks)
	set dqsn_out_clocks $pins(dqsn_out_clocks)
	set leveling_pins $pins(leveling_pins)

	set afi_reset_reg $pins(afi_reset_reg)
	set seq_reset_reg $pins(seq_reset_reg)
	set sync_reg $pins(sync_reg)
	set read_capture_ddio $pins(read_capture_ddio)
	set fifo_wraddress_reg $pins(fifo_wraddress_reg)
	set fifo_wrdata_reg $pins(fifo_wrdata_reg)
	set fifo_rddata_reg $pins(fifo_rddata_reg)
	set valid_fifo_wrdata_reg $pins(valid_fifo_wrdata_reg)
	set valid_fifo_rddata_reg $pins(valid_fifo_rddata_reg)

	##################
	#                #
	# QUERIED TIMING #
	#                #
	##################

	# Phase Jitter on DQS paths. This parameter is queried at run time
	set fpga(tDQS_PHASE_JITTER) [ expr [ get_integer_node_delay -integer $::GLOBAL_ddr2_v10_1_dqs_delay_chain_length -parameters {IO MAX HIGH} -src DQS_PHASE_JITTER -in_fitter ] / 1000.0 ]

	# Phase Error on DQS paths. This parameter is queried at run time
	set fpga(tDQS_PSERR) [ expr [ get_integer_node_delay -integer $::GLOBAL_ddr2_v10_1_dqs_delay_chain_length -parameters {IO MAX HIGH} -src DQS_PSERR -in_fitter ] / 1000.0 ]

		# Correct input min/max delay for queried parameters
		set final_data_input_max_delay [ round_3dp [ expr $data_input_max_delay + $fpga(tDQS_PSERR) ]]
		set final_data_input_min_delay [ round_3dp [ expr $data_input_min_delay - $t(CK) / 2.0 + $t(QH_time) - $fpga(tDQS_PSERR) - $tJITper]]


	if { $debug } {
		post_message -type info "SDC: Jitter Parameters"
		post_message -type info "SDC: -----------------"
		post_message -type info "SDC:    DQS Phase: $::GLOBAL_ddr2_v10_1_dqs_delay_chain_length"
		post_message -type info "SDC:    fpga(tDQS_PHASE_JITTER): $fpga(tDQS_PHASE_JITTER)"
		post_message -type info "SDC:    fpga(tDQS_PSERR): $fpga(tDQS_PSERR)"
		post_message -type info "SDC:    t(QH_time): $t(QH_time)"
		post_message -type info "SDC:"
		post_message -type info "SDC: Derived Parameters:"
		post_message -type info "SDC: -----------------"
		post_message -type info "SDC:    Corrected data_input_max_delay: $final_data_input_max_delay"
		post_message -type info "SDC:    Corrected data_input_min_delay: $final_data_input_min_delay"
		post_message -type info "SDC: -----------------"
	}

	# ----------------------- #
	# -                     - #
	# --- REFERENCE CLOCK --- #
	# -                     - #
	# ----------------------- #

	# This is the reference clock used by the PLL to derive any other clock in the core
	if { [get_collection_size [get_clocks -nowarn $pll_ref_clock]] > 0 } { remove_clock $pll_ref_clock }
	create_clock -period $t(refCK) -waveform [ list 0 $ref_half_period ] $pll_ref_clock

	# ------------------ #
	# -                - #
	# --- PLL CLOCKS --- #
	# -                - #
	# ------------------ #

	# PLL Clock Names
	set local_pll_afi_clk "${inst}|ddr2_v10_1_pll_afi_clk"
	set local_pll_mem_clk "${inst}|ddr2_v10_1_pll_mem_clk"
	set local_pll_write_clk "${inst}|ddr2_v10_1_pll_write_clk"
	set local_pll_addr_cmd_clk "${inst}|ddr2_v10_1_pll_addr_cmd_clk"
	set local_pll_afi_half_clk "${inst}|ddr2_v10_1_pll_afi_half_clk"
	set local_pll_avl_clock "${inst}|ddr2_v10_1_pll_avl_clock"
	set local_pll_config_clock "${inst}|ddr2_v10_1_pll_config_clock"

	if { [get_collection_size [get_clocks -nowarn "$local_pll_afi_clk"]] > 0 } { remove_clock "$local_pll_afi_clk" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_mem_clk"]] > 0 } { remove_clock "$local_pll_mem_clk" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_write_clk"]] > 0 } { remove_clock "$local_pll_write_clk" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_addr_cmd_clk"]] > 0 } { remove_clock "$local_pll_addr_cmd_clk" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_afi_half_clk"]] > 0 } { remove_clock "$local_pll_afi_half_clk" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_avl_clock"]] > 0 } { remove_clock "$local_pll_avl_clock" }
	if { [get_collection_size [get_clocks -nowarn "$local_pll_config_clock"]] > 0 } { remove_clock "$local_pll_config_clock" }

	# W A R N I N G !
	# The PLL parameters are statically defined in the ddr2_v10_1_parameters.tcl
	# file at generation time!
	# To ensure timing constraints and timing reports are correct, when you make
	# any changes to the PLL component using the MegaWizard Plug-In,
	# apply those changes to the PLL parameters in the ddr2_v10_1_parameters.tcl

	create_generated_clock -add -name "$local_pll_afi_clk" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_0_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_0_div -phase $::GLOBAL_ddr2_v10_1_pll_0_phase $pll_afi_clock
if {[string compare -nocase $pll_afi_clock $pll_ck_clock]==0} {
	set local_pll_mem_clk $local_pll_afi_clk
} else {
	create_generated_clock -add -name "$local_pll_mem_clk" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_1_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_1_div -phase $::GLOBAL_ddr2_v10_1_pll_1_phase $pll_ck_clock
}
	create_generated_clock -add -name "$local_pll_write_clk" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_2_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_2_div -phase $::GLOBAL_ddr2_v10_1_pll_2_phase $pll_write_clock
	create_generated_clock -add -name "$local_pll_addr_cmd_clk" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_3_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_3_div -phase $::GLOBAL_ddr2_v10_1_pll_3_phase $pll_ac_clock
	create_generated_clock -add -name "$local_pll_afi_half_clk" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_4_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_4_div -phase $::GLOBAL_ddr2_v10_1_pll_4_phase $pll_afi_half_clock
	create_generated_clock -add -name "$local_pll_avl_clock" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_5_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_5_div -phase $::GLOBAL_ddr2_v10_1_pll_5_phase $pll_avl_clock
	create_generated_clock -add -name "$local_pll_config_clock" -source $pll_ref_clock -multiply_by $::GLOBAL_ddr2_v10_1_pll_6_mult -divide_by $::GLOBAL_ddr2_v10_1_pll_6_div -phase $::GLOBAL_ddr2_v10_1_pll_6_phase $pll_config_clock



	# -------------------- #
	# -                  - #
	# --- SYSTEM CLOCK --- #
	# -                  - #
	# -------------------- #

	# This is the CK clock
	foreach { ck_pin } $ck_pins {
		create_generated_clock -multiply_by 1 -source $pll_ck_clock -master_clock "$local_pll_mem_clk" $ck_pin -name $ck_pin
	}

	# This is the CK# clock
	foreach { ckn_pin } $ckn_pins {
		create_generated_clock -multiply_by 1 -invert -source $pll_ck_clock -master_clock "$local_pll_mem_clk" $ckn_pin -name $ckn_pin
	}

	# ------------------- #
	# -                 - #
	# --- READ CLOCKS --- #
	# -                 - #
	# ------------------- #


	foreach dqs_in_clock_struct $dqs_in_clocks {
		array set dqs_in_clock $dqs_in_clock_struct
		# This is the DQS clock for Read Capture analysis (micro model)
		create_clock -period $t(CK) -waveform [ list 0 $half_period ] $dqs_in_clock(dqs_pin) -name $dqs_in_clock(dqs_pin)_IN -add

		# DIV clock is generated on the output of the clock divider.
		# This clock is created using DQS as the source. However, in the netlist there's an inverter
		# between the two clocks. In order to create the right waveform, the clock is divided and shifted by 90
		# degrees.
		create_generated_clock -name $dqs_in_clock(div_name) -source $dqs_in_clock(dqs_pin) -divide_by 2 -phase 90 $dqs_in_clock(div_pin) -master $dqs_in_clock(dqs_pin)_IN

			# Clock Uncertainty is accounted for by the ...pathjitter parameters
			set_clock_uncertainty -from [ get_clocks $dqs_in_clock(dqs_pin)_IN ] 0
	}

	# ----------------------- #
	# -                     - #
	# --- LEVELING CLOCKS --- #
	# -                     - #
	# ----------------------- #

	create_generated_clock -add -name "${inst}|memphy_leveling_clk" -master_clock [get_clocks $local_pll_write_clk] -source $pll_write_clock -phase $::GLOBAL_ddr2_v10_1_leveling_capture_phase [ get_pins $leveling_pins ]
	
	# -------------------- #
	# -                  - #
	# --- WRITE CLOCKS --- #
	# -                  - #
	# -------------------- #

	# This is the DQS clock for Data Write analysis (micro model)
	foreach dqs_out_clock_struct $dqs_out_clocks {
		array set dqs_out_clock $dqs_out_clock_struct
		# Set DQS phase to the ideal 90 degrees and let the calibration scripts take care of
		# properly adjust the margins
		create_generated_clock -add -multiply_by 1 -master_clock [get_clocks "${inst}|memphy_leveling_clk"] -source $dqs_out_clock(src) -phase 90 $dqs_out_clock(dst) -name $dqs_out_clock(dst)_OUT

			# Clock Uncertainty is accounted for by the ...pathjitter parameters
	       		set_clock_uncertainty -from [get_clocks ${inst}|memphy_leveling_clk] -to [ get_clocks $dqs_out_clock(dst)_OUT ] 0
	}

	# This is the DQS# clock for Data Write analysis (micro model)
	foreach dqsn_out_clock_struct $dqsn_out_clocks {
		array set dqsn_out_clock $dqsn_out_clock_struct
		# Set DQS# phase to the ideal 90 degrees and let the calibration scripts take care of
		# properly adjust the margins
		create_generated_clock -add -multiply_by 1 -master_clock [get_clocks "${inst}|memphy_leveling_clk"] -source $dqsn_out_clock(src) -phase 90 $dqsn_out_clock(dst) -name $dqsn_out_clock(dst)_OUT

			# Clock Uncertainty is accounted for by the ...pathjitter parameters
			set_clock_uncertainty -from [get_clocks ${inst}|memphy_leveling_clk] -to [ get_clocks $dqsn_out_clock(dst)_OUT ] 0
	}


	##################
	#                #
	# READ DATA PATH #
	#                #
	##################

		##################
		#                #
		# (Micro Model)  #
		#                #
		##################

		foreach { dqs_pin } $dqs_pins { dq_pins } $q_groups {
			foreach { dq_pin } $dq_pins {
				set_max_delay -from [get_ports $dq_pin] -to $read_capture_ddio 0
				set_min_delay -from [get_ports $dq_pin] -to $read_capture_ddio [expr 0-$half_period]
							
				# Specifies the maximum delay difference between the DQ pin and the DQS pin:
				set_input_delay -max $final_data_input_max_delay -clock [get_clocks ${dqs_pin}_IN ] [get_ports $dq_pin] -add_delay

				# Specifies the minimum delay difference between the DQ pin and the DQS pin:
				set_input_delay -min $final_data_input_min_delay -clock [get_clocks ${dqs_pin}_IN ] [get_ports $dq_pin] -add_delay
			}
		}

	# Constraint to increase resynchronization timing margin
	if {(($::quartus(nameofexecutable) eq "quartus_fit"))} {
		set_max_delay -from $pins(fifo_rdaddress_reg) -to $pins(fifo_rddata_reg) [expr $t(CK)/2.0]
		set_min_delay -from $pins(fifo_rdaddress_reg) -to $pins(fifo_rddata_reg) 0
	}

	###################
	#                 #
	# WRITE DATA PATH #
	#                 #
	###################

		###################
		#                 #
		# (Micro Model)   #
		#                 #
		###################

		foreach { dqs_pin } $dqs_pins { dq_pins } $q_groups {
			foreach { dq_pin } $dq_pins {
				# Specifies the minimum delay difference between the DQS pin and the DQ pins:
				set_output_delay -min $data_output_min_delay -clock [get_clocks ${dqs_pin}_OUT ] [get_ports $dq_pin] -add_delay

				# Specifies the maximum delay difference between the DQS pin and the DQ pins:
				set_output_delay -max $data_output_max_delay -clock [get_clocks ${dqs_pin}_OUT ] [get_ports $dq_pin] -add_delay
			}
		}

		foreach { dqsn_pin } $dqsn_pins { dq_pins } $q_groups {
			foreach { dq_pin } $dq_pins {
				# Specifies the minimum delay difference between the DQS# pin and the DQ pins:
				set_output_delay -min $data_output_min_delay -clock [get_clocks ${dqsn_pin}_OUT ] [get_ports $dq_pin] -add_delay

				# Specifies the maximum delay difference between the DQS# pin and the DQ pins:
				set_output_delay -max $data_output_max_delay -clock [get_clocks ${dqsn_pin}_OUT ] [get_ports $dq_pin] -add_delay
			}
		}

		foreach dqs_out_clock_struct $dqs_out_clocks {
			array set dqs_out_clock $dqs_out_clock_struct

			if { [string length $dqs_out_clock(dm_pin)] > 0 } {
				# Specifies the minimum delay difference between the DQS and the DM pins:
				set_output_delay -min $data_output_min_delay -clock [get_clocks $dqs_out_clock(dst)_OUT ] [get_ports $dqs_out_clock(dm_pin)] -add_delay

				# Specifies the maximum delay difference between the DQS and the DM pins:
				set_output_delay -max $data_output_max_delay -clock [get_clocks $dqs_out_clock(dst)_OUT ] [get_ports $dqs_out_clock(dm_pin)] -add_delay
			}
		}

		foreach dqsn_out_clock_struct $dqsn_out_clocks {
			array set dqsn_out_clock $dqsn_out_clock_struct

			if { [string length $dqsn_out_clock(dm_pin)] > 0 } {
				# Specifies the minimum delay difference between the DQS and the DM pins:
				set_output_delay -min $data_output_min_delay -clock [get_clocks $dqsn_out_clock(dst)_OUT ] [get_ports $dqsn_out_clock(dm_pin)] -add_delay

				# Specifies the maximum delay difference between the DQS and the DM pins:
				set_output_delay -max $data_output_max_delay -clock [get_clocks $dqsn_out_clock(dst)_OUT ] [get_ports $dqsn_out_clock(dm_pin)] -add_delay
			}
		}

	############
	#          #
	# A/C PATH #
	#          #
	############

	foreach { ck_pin } $ck_pins {
		# Specifies the minimum delay difference between the DQS pin and the address/control pins:
		set_output_delay -min $ac_min_delay -clock [get_clocks $ck_pin] [ get_ports $ac_pins ] -add_delay

		# Specifies the maximum delay difference between the DQS pin and the address/control pins:
		set_output_delay -max $ac_max_delay -clock [get_clocks $ck_pin] [ get_ports $ac_pins ] -add_delay
	}


	##########################
	#                        #
	# MULTICYCLE CONSTRAINTS #
	#                        #
	##########################


	# The transfer from capture DDIO to FIFO is a zero-cycle transfer
	# to allows the fitter to better balance out setup and hold margins
	foreach dqs_in_clock_struct $dqs_in_clocks {
		array set dqs_in_clock $dqs_in_clock_struct
		set_multicycle_path -from [get_clocks $dqs_in_clock(dqs_pin)_IN] -to [get_clocks $dqs_in_clock(div_name)] -start 0
	}

	set_multicycle_path -from *sequencer_rw_mgr_core*cmd_op*_avl* -to *sequencer_rw_mgr_core*cmd_op*_afi* -end -hold 1
	set_multicycle_path -from *sequencer_rw_mgr_core*creg*_avl* -to *sequencer_rw_mgr_core*creg*_afi* -end -hold 1
	set_multicycle_path -from *sequencer_rw_mgr_core*creg_odt_avl -to *sequencer_rw_mgr_core*afi_odt[*] -end -hold 1
	set_multicycle_path -from *sequencer_rw_mgr_core*creg_cke_avl -to *sequencer_rw_mgr_core*afi_cke[*] -end -hold 1
	set_multicycle_path -from *sequencer_rw_mgr*cmd_exec -to *sequencer_rw_mgr_core*cmd_exec_afi -end -hold 1

	##########################
	#                        #
	# FALSE PATH CONSTRAINTS #
	#                        #
	##########################

	# Cut paths from DQS_OUT to DQS_IN
	foreach { dqs_pin } $dqs_pins {
		set_false_path -from ${dqs_pin}_OUT -to ${dqs_pin}_IN
		set_false_path -from ${dqs_pin}_IN -to ${dqs_pin}_OUT
		
		set_false_path -from ${inst}|memphy_leveling_clk -to ${dqs_pin}_IN
		set_false_path -from ${inst}|memphy_leveling_clk -to ${dqs_pin}_OUT
	}
	
	# Cut paths for memory clocks to avoid unconstrained warnings
	foreach { pin } [concat $dqs_pins $dqsn_pins $ck_pins $ckn_pins] {
		set_false_path -to [get_ports $pin]
	}

	foreach dqs_in_clock_struct $dqs_in_clocks {
		array set dqs_in_clock $dqs_in_clock_struct

		# Cut paths between DQS_OUT and Div Clock
		set_false_path -from [ get_clocks $dqs_in_clock(dqs_pin)_OUT ] -to [ get_clocks $dqs_in_clock(div_name) ]
		set_false_path -from [ get_clocks $dqs_in_clock(div_name) ] -to [ get_clocks $dqs_in_clock(dqs_pin)_OUT ]

		# Cut paths between AFI Clock and Div Clock
		set_false_path -from [ get_clocks $local_pll_afi_clk ] -to [ get_clocks $dqs_in_clock(div_name) ]
		if { [get_collection_size [get_clocks -nowarn  $pll_afi_clock]] > 0 } {
			set_false_path -from [ get_clocks $pll_afi_clock ] -to [ get_clocks $dqs_in_clock(div_name) ]
		}		
		set_false_path -from [ get_clocks $local_pll_afi_half_clk ] -to [ get_clocks $dqs_in_clock(div_name) ]
		if { [get_collection_size [get_clocks -nowarn  $pll_afi_half_clock]] > 0 } {
			set_false_path -from [ get_clocks $pll_afi_half_clock ] -to [ get_clocks $dqs_in_clock(div_name) ]
		}		

		# Cut reset path to clock divider (reset signal controlled by the sequencer)
		set_false_path -from [ get_clocks $local_pll_afi_clk ] -to $dqs_in_clock(div_pin)
		if { [get_collection_size [get_clocks -nowarn  $pll_afi_clock]] > 0 } {
			set_false_path -from [ get_clocks $pll_afi_clock ] -to $dqs_in_clock(div_pin)
		}		
		set_false_path -from [ get_clocks $local_pll_afi_half_clk ] -to $dqs_in_clock(div_pin)
		if { [get_collection_size [get_clocks -nowarn  $pll_afi_half_clock]] > 0 } {
			set_false_path -from [ get_clocks $pll_afi_half_clock ] -to $dqs_in_clock(div_pin)
		}		

		# Cut reset path from sequencer to the clock divider
		set_false_path -from $seq_reset_reg -to $dqs_in_clock(div_pin)
	}
	# This is a register based memory operating as an asynchronous FIFO
	# therefore there is no timing path between the write and read side
	set_false_path -from [get_registers $fifo_wrdata_reg] -to [get_registers $fifo_rddata_reg]

	# The dqs_edge_detect register in the PHY can be considered an asynchronous
	# path to the PHY CSR port. As a result, cut the path.
	set_false_path -from [get_registers *dqs_edge_detect_reg\[*\]] -to [get_registers *csr_register_0007\[*\]]
	set_false_path -from [get_registers *reset_n_fifo_write_side\[*\]] -to [get_registers *dqs_edge_detect_reg\[*\]]


	# The paths between DQS_ENA_CLK and DQS_IN are calibrated, so they must not be analyzed
	set_false_path -from [get_clocks $local_pll_write_clk] -to [get_clocks {*_IN}]
	
	# False path through the output enable because it is enabled one clock cycle before writing
	set_false_path -from *aligned_oe* -to $all_dq_pins	
	# Cut paths between the afi_half and avl clocks
	set_false_path -from [get_clocks $local_pll_avl_clock] -to [get_clocks $local_pll_afi_half_clk]
	set_false_path -from [get_clocks $local_pll_afi_half_clk] -to [get_clocks $local_pll_avl_clock]

	# NIOS false paths
	set_false_path -from *usequencer*mux_sel -to [remove_from_collection [get_registers *] [get_registers [list *oct_reg *oct_bar_reg]]]

	set tCK_AFI [ expr $t(CK) * 2.0 ]

	# Add clock (DQS) uncertainty applied from the DDIO registers to registers in the core
	set_max_delay -from [get_registers ${prefix}*read_data_out*] -to $fifo_wrdata_reg -0.05
	set_min_delay -from [get_registers ${prefix}*read_data_out*] -to $fifo_wrdata_reg [ round_3dp [expr -$t(CK) + 0.15]]
	
}

	if {(($::quartus(nameofexecutable) ne "quartus_fit") && ($::quartus(nameofexecutable) ne "quartus_map"))} {
		set dqs_clocks [ddr2_v10_1_get_all_instances_dqs_pins ddr2_v10_1_ddr_db]
		# Leave clocks active when in debug mode
		if {[llength $dqs_clocks] > 0 && !$debug} {
			post_sdc_message info "Setting DQS clocks as inactive; use Report DDR to timing analyze DQS clocks"
			set_active_clocks [remove_from_collection [get_active_clocks] [get_clocks $dqs_clocks]]
		}
	}	



######################
#                    #
# REPORT DDR COMMAND #
#                    #
######################

add_ddr_report_command "source [list [file join [file dirname [info script]] ${::GLOBAL_ddr2_v10_1_corename}_report_timing.tcl]]"

