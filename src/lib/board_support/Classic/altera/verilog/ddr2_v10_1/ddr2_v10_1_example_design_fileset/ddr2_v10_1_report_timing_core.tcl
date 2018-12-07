# Copyright 2009 Altera Corporation. All rights reserved.  
# Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
# functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
# documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
# Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
# that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
# distributors.  Please refer to the applicable agreement for further details.

##############################################################
## Write Timing Analysis
##############################################################
proc perform_flexible_write_launch_timing_analysis {opcs opcname inst family DQS_min_max interface_type max_package_skew dll_length period pin_array_name timing_parameters_array_name summary_name MP_name IP_name board_name} {

	################################################################################
	# This timing analysis covers the write timing constraints.  It includes support 
	# for uncalibrated and calibrated write paths.  The analysis starts by running a 
	# conventional timing analysis for the write paths and then adds support for 
	# topologies and IP options which are unique to source-synchronous data transfers.  
	# The support for further topologies includes common clock paths in DDR3 as well as 
	# correlation between D and K.  The support for further IP includes support for 
	# write-deskew calibration.
	# 
	# During write deskew calibration, the IP will adjust delay chain settings along 
	# each signal path to reduce the skew between D pins and to centre align the K 
	# clock within the DVW.  This operation has the benefit of increasing margin on the 
	# setup and hold, as well as removing some of the unknown process variation on each 
	# signal path.  This timing analysis emulates the IP process by deskewing each pin as 
	# well as accounting for the elimination of the unknown process variation.  Once the 
	# deskew emulation is complete, the analysis further considers the effect of changing 
	# the delay chain settings to the operation of the device after calibration: these 
	# effects include changes in voltage and temperature which may affect the optimality 
	# of the deskew process.
	# 
	# The timing analysis creates a write summary report indicating how the timing analysis 
	# was performed starting with a typical timing analysis before calibration.
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $pin_array_name pins
	upvar 1 $MP_name MP
	upvar 1 $IP_name IP
	upvar 1 $board_name board

	set num_failing_path $IP(num_report_paths)

	set debug 0

	set result 1
	
	######################################## 
	########################################
	## Initialize the write analysis panel	
	set panel_name "$inst Write"
	set root_folder_name [get_current_timequest_report_folder]

	if { ! [string match "${root_folder_name}*" $panel_name] } {
		set panel_name "${root_folder_name}||$panel_name"
	}
	# Create the root if it doesn't yet exist
	if {[get_report_panel_id $root_folder_name] == -1} {
		set panel_id [create_report_panel -folder $root_folder_name]
	}

	# Delete any pre-existing summary panel
	set panel_id [get_report_panel_id $panel_name]
	if {$panel_id != -1} {
		delete_report_panel -id $panel_id
	}

	set panel_id [create_report_panel -table $panel_name]
	add_row_to_table -id $panel_id [list "Operation" "Setup Slack" "Hold Slack"] 
	
	##################################
	# Find the clock output of the PLL
	set ref_clock_input $pins(pll_ref_clock)
	set msg_list [ list ]	
	set dqs_pll_clock_id [get_output_clock_id $pins(dqs_pins) "DQS output" msg_list]
	if {$dqs_pll_clock_id == -1} {
		foreach {msg_type msg} $msg_list {
			post_message -type $msg_type "$msg"
		}
		post_message -type warning "Failed to find PLL clock for pins [join $pins(dqs_pins)]"
		set result 0
	} else {
		set dqsclksource [get_node_info -name $dqs_pll_clock_id]
	}

	foreach q_group $pins(q_groups) {
		set q_group $q_group
		lappend q_groups $q_group
	}
	set all_dq_pins [ join [ join $q_groups ] ]
	set dm_pins $pins(dm_pins)
	set all_dq_dm_pins [ concat $all_dq_pins $dm_pins ]	
	
	if {$IP(write_deskew_mode) == "dynamic"} {
		set panel_name_setup  "Before Calibration \u0028Negative slacks are OK\u0029||$inst Write \u0028Before Calibration\u0029 (setup)"
		set panel_name_hold   "Before Calibration \u0028Negative slacks are OK\u0029||$inst Write \u0028Before Calibration\u0029 (hold)"
	} else {
		set panel_name_setup  "Before Spatial Pessimism Removal \u0028Negative slacks are OK\u0029||$inst Write (setup)"
		set panel_name_hold   "Before Spatial Pessimism Removal \u0028Negative slacks are OK\u0029||$inst Write (hold)"
	}
	
	######################################################################		
	## Default Write Analysis
	set before_calibration_reporting [get_ini_var -name "qsta_enable_before_calibration_ddr_reporting"]
	if {![string equal -nocase $before_calibration_reporting off]}  {
		set res_0 [report_timing -detail full_path -to [get_ports $all_dq_dm_pins] \
			-npaths $num_failing_path -panel_name $panel_name_setup -setup -disable_panel_color -quiet]
		set res_1 [report_timing -detail full_path -to [get_ports $all_dq_dm_pins] \
			-npaths $num_failing_path -panel_name $panel_name_hold -hold -disable_panel_color -quiet]
	}

	# Perform the default timing analysis to get required and arrival times
	set paths_setup [get_timing_paths -to [get_ports $all_dq_dm_pins] -npaths 400 -setup]
	set paths_hold  [get_timing_paths -to [get_ports $all_dq_dm_pins] -npaths 400 -hold]

	######################################
	## Find Memory Calibration Improvement 
	######################################				
	
	set mp_setup_slack 0
	set mp_hold_slack  0
	if {($IP(write_deskew_mode) == "dynamic") && ($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		# Reduce the effect of tDS on the setup slack
		set mp_setup_slack [expr $MP(DS)*$t(DS)]
		
		# Reduce the effect of tDH on the hold slack
		set mp_hold_slack  [expr $MP(DH)*$t(DH)]
	}
	
	################################
	## Extra common clock pessimism removal (to PLL) that is not caught by STA
	################################
	
	set ref_pll_paths_max [get_path -from ${ref_clock_input}*input|o -to $dqsclksource -nworst 1]
	set ref_pll_paths_min [get_path -from ${ref_clock_input}*input|o -to $dqsclksource -nworst 1 -min_path]
	set ref_pll_min_of_max [min_in_collection $ref_pll_paths_max "arrival_time"]
	set ref_pll_min_of_min [min_in_collection $ref_pll_paths_min "arrival_time"]	
	set pll_ccpp [expr $ref_pll_min_of_max - $ref_pll_min_of_min]	

	#########################################
	# Go over each pin and compute its slack
	# Then include any effects that are unique
	# to source synchronous designs including
	# common clocks, signal correlation, and
	# IP calibration options to compute the
	# total slack of the instance
	
	set setup_slack 1000000000
	set hold_slack  1000000000
	set default_setup_slack 1000000000
	set default_hold_slack  1000000000	
	set DQDQSpessimismremoval 1000000000
		
	if {($result == 1)} {

		set group_number -1
		#Go over each DQS pin
		foreach dqpins $pins(q_groups) {
		
			set group_number [expr $group_number + 1]
			
			set dqspin [lindex $pins(dqs_pins) $group_number]
			set dqsnpin [lindex $pins(dqsn_pins) $group_number]
			set dmpins [lindex $pins(dm_pins) $group_number]
			set dqdmpins $dqpins
			if {[llength $dmpins] > 0} {
				lappend dqdmpins $dmpins
			}


			# Find DQS clock node before the periphery 
			set dqs_periphery_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*dqs_alignment|clk
			# Find paths from PLL to DQS clock periphery node
			set DQSpaths_max [get_path -from $dqsclksource -to $dqs_periphery_node -nworst 1]
			set DQSpaths_min [get_path -from $dqsclksource -to $dqs_periphery_node -nworst 1 -min_path]
			set DQSmin_of_max [min_in_collection $DQSpaths_max "arrival_time"]
			set DQSmax_of_min [max_in_collection $DQSpaths_min "arrival_time"]
			set DQSmax_of_max [max_in_collection $DQSpaths_max "arrival_time"]
			set DQSmin_of_min [min_in_collection $DQSpaths_min "arrival_time"]					
		
			##############################################
			## Find extra DQS pessimism due to correlation
			##############################################
			
			# Find paths from DQS clock periphery node to beginning of output buffer
			set output_buffer_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*obuf*_0|i
			
			set DQSperiphery_min [get_path -from $dqs_periphery_node -to $output_buffer_node -min_path -nworst 100]
			
			set DQSperiphery_delay [min_in_collection $DQSperiphery_min "arrival_time"]
			set DQSpath_pessimism [expr $DQSperiphery_delay*$DQS_min_max]				
			
			# Go over each DQ pin in group
			set dq_index 0
			set dm_index 0
			
			foreach dqpin $dqdmpins {
			
				if {[lsearch -exact $dmpins $dqpin] >= 0} {
					set isdmpin 1
					regexp {\d+} $dqpin dm_pin_index
				} else {
					set isdmpin 0
					regexp {\d+} $dqpin dq_pin_index
				}
				
				# Perform the default timing analysis to get required and arrival times
				set pin_setup_slack [min_in_collection_to_name $paths_setup "slack" $dqpin]
				set pin_hold_slack  [min_in_collection_to_name $paths_hold "slack" $dqpin]

				set default_setup_slack [min $default_setup_slack $pin_setup_slack]
				set default_hold_slack  [min $default_hold_slack  $pin_hold_slack]		

				if { $debug } {
					puts "$group_number $dqspin $dqpin $pin_setup_slack $pin_hold_slack"	
				}
				
				
				################################
				## Extra common clock pessimism removal (from PLL) that is not caught by STA
				################################
				
				# Find the DQ clock node before the periphery
				if {$isdmpin == 1} {
					if {$family == "arria ii"}  {
						set dq_periphery_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|uwrite_pads|*|dm_mapping[${group_number}].uwrite_mask_pad|output_dq_${dm_index}_output_ddio_out_inst|muxsel
					} else {
						set dq_periphery_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*extra_output_pad_gen[0].data_alignment|clk
					}
				} else {
					if {$family == "arria ii"}  {
						set dq_periphery_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|uwrite_pads|*|dq_ddio[*].ubidir_dq_dqs|bidir_dq_*_output_ddio_out_inst|muxsel
					} else {
						set dq_periphery_node ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*output_path_gen[${dq_index}].data_alignment|clk
					}
				}

				# Find paths from PLL to DQ clock periphery node
				set DQpaths_max [get_path -from $dqsclksource -to $dq_periphery_node -nworst 1]
				set DQpaths_min [get_path -from $dqsclksource -to $dq_periphery_node -nworst 1 -min_path]
				set DQmin_of_max [min_in_collection $DQpaths_max "arrival_time"]
				set DQmax_of_min [max_in_collection $DQpaths_min "arrival_time"]
				set DQmax_of_max [max_in_collection $DQpaths_max "arrival_time"]
				set DQmin_of_min [min_in_collection $DQpaths_min "arrival_time"]			
				if {([expr abs($DQSmin_of_max - $DQmin_of_max)] < 0.002) && ([expr abs($DQSmax_of_min - $DQmax_of_min)] < 0.002)} {
					set extra_ccpp_max [expr $DQSmax_of_max - $DQSmax_of_min]
					set extra_ccpp_min [expr $DQSmin_of_max - $DQSmin_of_min]
					if {[string match -nocase "HARDCOPY*" $family] } {
						set extra_ccpp [expr 0.68*[min $extra_ccpp_max $extra_ccpp_min] + $pll_ccpp]
					} else {
						set extra_ccpp [expr 0.85*[min $extra_ccpp_max $extra_ccpp_min] + $pll_ccpp]
					}
				} else {
					set extra_ccpp pll_ccpp
				}
				
				# Add the extra ccpp to both setup and hold slacks
				set pin_setup_slack [expr $pin_setup_slack + $extra_ccpp]
				set pin_hold_slack [expr $pin_hold_slack + $extra_ccpp]
								
				
				#########################################
				## Add the memory calibration improvement
				#########################################
				
				set pin_setup_slack [expr $pin_setup_slack + $mp_setup_slack]
				set pin_hold_slack [expr $pin_hold_slack + $mp_hold_slack]				
		
				#############################################
				## Find extra DQ pessimism due to correlation
				#############################################
				
				# Find the DQ clock node before the periphery
				if {$isdmpin == 1} {
					set output_buffer_node_dq ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*extra_output_pad_gen[0].obuf_1|i					
				} else {
					set output_buffer_node_dq ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*pad_gen[${dq_index}].data_out|i
				}
				
				set DQperiphery_min [get_path -from $dq_periphery_node -to $output_buffer_node_dq -min_path -nworst 100]
				set DQperiphery_delay [min_in_collection $DQperiphery_min "arrival_time"]
				set DQpath_pessimism [expr $DQperiphery_delay*$DQS_min_max]	
				
				#########################################
				## Merge current slacks with other slacks
				#########################################			

				# If write deskew is available, the setup and hold slacks for this pin will be equal
				#   and can also remove the extra DQS and DQ pessimism removal
				if {$IP(write_deskew_mode) == "dynamic"} {
				
					# Consider the maximum range of the deskew when deskewing
					set shift_setup_slack [expr ($pin_setup_slack + $pin_hold_slack)/2 - $pin_setup_slack]
					set max_write_deskew [expr $IP(write_deskew_range)*$IP(quantization_T9)]
					if {$shift_setup_slack >= $max_write_deskew} {
						if { $debug } {
							puts "limited setup"
						}
						set pin_setup_slack [expr $pin_setup_slack + $max_write_deskew]
						set pin_hold_slack [expr $pin_hold_slack - $max_write_deskew]
					} elseif {$shift_setup_slack <= -$max_write_deskew} {
						if { $debug } {
							puts "limited hold"
						}
						set pin_setup_slack [expr $pin_setup_slack - $max_write_deskew]
						set pin_hold_slack [expr $pin_hold_slack + $max_write_deskew]
					} else {
						# In this case we can also consider the DQS/DQpath pessimism since we can guarantee we have enough delay chain settings to align it
						set pin_setup_slack [expr $pin_setup_slack + $shift_setup_slack + $DQSpath_pessimism/2 + $DQpath_pessimism/2]
						set pin_hold_slack [expr $pin_hold_slack - $shift_setup_slack + $DQSpath_pessimism/2 + $DQpath_pessimism/2]						
					}
				} else {
					# For uncalibrated calls, there is some spatial correlation between DQ and DQS signals, so remove
					# some of the pessimism
					set total_DQ_DQS_pessimism [expr $DQSpath_pessimism + $DQpath_pessimism]
					set dqs_width [llength $dqpins]
					if {$dqs_width <= 9} {
						set DQDQSpessimismremoval [min $DQDQSpessimismremoval [expr 0.45*$total_DQ_DQS_pessimism]]
						set pin_setup_slack [expr $pin_setup_slack + 0.35*$total_DQ_DQS_pessimism]
						set pin_hold_slack  [expr $pin_hold_slack  + 0.35*$total_DQ_DQS_pessimism]
					} 
				} 
				

				set setup_slack [min $setup_slack $pin_setup_slack]
				set hold_slack  [min $hold_slack $pin_hold_slack]
				
				if { $debug } {
					puts "                                $extra_ccpp $DQSpath_pessimism $DQpath_pessimism ($pin_setup_slack $pin_hold_slack $setup_slack $hold_slack)" 
				}

				if {$isdmpin == 0} {
					set dq_index [expr $dq_index + 1]
				} else {
					set dm_index [expr $dm_index + 1]
				}
			}
		}
	}

	################################
	## Consider some post calibration effects on calibration
	##  and output the write summary report
	################################
	set positive_fcolour [list "black" "blue" "blue"]
	set negative_fcolour [list "black" "red"  "red"]	
	
	set wr_summary [list]
	
	if {$IP(write_deskew_mode) == "dynamic"} {
		lappend wr_summary [list "  Before Calibration Write" [format_3dp $default_setup_slack] [format_3dp $default_hold_slack]]
	} else {
		lappend wr_summary [list "  Standard Write" [format_3dp $default_setup_slack] [format_3dp $default_hold_slack]]
	}
	
	if {($IP(write_deskew_mode) == "dynamic") && ($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		lappend wr_summary [list "  Memory Calibration" [format_3dp $mp_setup_slack] [format_3dp $mp_hold_slack]]
	}		
	
	if {$IP(write_deskew_mode) == "dynamic"} {
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		# Remove external delays (add slack) that are fixed by the dynamic deskew
		if { $IP(discrete_device) == 1 } {
			set t(WL_PSE) 0
		}
		[catch {get_float_table_node_delay -src {DELAYCHAIN_T9} -dst {VTVARIATION} -parameters [list IO $interface_type]} t9_vt_variation_percent]		
		set setup_slack [expr $setup_slack + $board(intra_DQS_group_skew) + [round_3dp [expr (1.0-$t9_vt_variation_percent)*$t(WL_PSE)]]]
		set hold_slack [expr $hold_slack + $board(intra_DQS_group_skew) + [round_3dp [expr (1.0-$t9_vt_variation_percent)*$t(WL_PSE)]]]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		set deskew_setup [expr $setup_slack - $default_setup_slack -$mp_setup_slack]
		set deskew_hold  [expr $hold_slack - $default_hold_slack - $mp_hold_slack]
		lappend wr_summary [list "  Deskew Write and/or more clock pessimism removal" [format_3dp $deskew_setup] [format_3dp $deskew_hold]]
		
		# Consider errors in the dynamic deskew
		set t9_quantization $IP(quantization_T9)
		set setup_slack [expr $setup_slack - $t9_quantization]
		set hold_slack  [expr $hold_slack - $t9_quantization]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend wr_summary [list "  Quantization error" [format_3dp [expr 0-$t9_quantization]] [format_3dp [expr 0-$t9_quantization]]]
		
		# Consider variation in the delay chains used during dynamic deksew
		set offset_from_90 [expr abs(90-(360.0/$dll_length*2))/360.0*$period]
		set t9_variation [expr [min [expr $offset_from_90 + (2*$board(intra_DQS_group_skew) + 2*$max_package_skew + $t(WL_PSE))] max_write_deskew]*2*$t9_vt_variation_percent]
		set setup_slack [expr $setup_slack - $t9_variation]
		set hold_slack  [expr $hold_slack - $t9_variation]	
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend wr_summary [list "  Calibration uncertainty" [format_3dp [expr 0-$t9_variation]] [format_3dp [expr 0-$t9_variation]]] 
	} else {
		set pessimism_setup [expr $setup_slack - $default_setup_slack - $mp_setup_slack]
		set pessimism_hold  [expr $hold_slack - $default_hold_slack - $mp_hold_slack]
		lappend wr_summary [list "  Spatial correlation pessimism removal" [format_3dp $pessimism_setup] [format_3dp $pessimism_hold]]
	}	
	
	################################
	## Consider Duty Cycle Calibration if enabled
	################################	

	if {($IP(write_dcc) == "dynamic") && ($IP(mem_if_memtype) == "DDR3 SDRAM")} {
		#First remove the Systematic DCD
		set setup_slack [expr $setup_slack + $t(WL_DCD)]
		set hold_slack  [expr $hold_slack + $t(WL_DCD)]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend wr_summary [list "  Duty cycle correction" $t(WL_DCD) $t(WL_DCD)]
		
		#Add errors in the DCC
		set DCC_quantization $IP(quantization_DCC)
		set setup_slack [expr $setup_slack - $DCC_quantization]
		set hold_slack  [expr $hold_slack - $DCC_quantization]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend wr_summary [list "  Duty cycle correction quantization error" [format_3dp [expr 0-$DCC_quantization]] [format_3dp [expr 0-$DCC_quantization]]]
		
		# Consider variation in the DCC 
		[catch {get_float_table_node_delay -src {DELAYCHAIN_DUTY_CYCLE} -dst {VTVARIATION} -parameters [list IO $interface_type]} dcc_vt_variation_percent]
		set dcc_variation [expr $t(WL_DCD)*2*$dcc_vt_variation_percent]
		set setup_slack [expr $setup_slack - $dcc_variation]
		set hold_slack  [expr $hold_slack - $dcc_variation]		
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend wr_summary [list "  Duty cycle correction calibration uncertainity" [format_3dp [expr 0-$dcc_variation]] [format_3dp [expr 0-$dcc_variation]]]
	}
	
	if {($IP(write_deskew_mode) == "dynamic") && ($IP(mem_if_memtype) == "DDR3 SDRAM")} {
		set fcolour [get_colours $setup_slack $hold_slack]
		add_row_to_table -id $panel_id [list "After Calibration Write" [format_3dp $setup_slack] [format_3dp $hold_slack]] -fcolor $fcolour
		lappend summary [list $opcname 0 "Write ($opcname)" $setup_slack $hold_slack]
	} else {
		set fcolour [get_colours $setup_slack $hold_slack] 
		add_row_to_table -id $panel_id [list "Write" [format_3dp $setup_slack] [format_3dp $hold_slack]] -fcolor $fcolour
		lappend summary [list $opcname 0 "Write ($opcname)" [format_3dp $setup_slack] [format_3dp $hold_slack]]
	}

	foreach summary_line $wr_summary {
		add_row_to_table -id $panel_id $summary_line -fcolors $positive_fcolour
	}
}

proc perform_macro_write_launch_timing_analysis {opcs opcname inst dll_length interface_type pin_array_name timing_parameters_array_name summary_name IP_name ISI_name board_name} {

	################################################################################
	# This timing analysis covers the write timing constraints using the Altera 
	# characterized values.  Altera performs characterization on its devices and IP 
	# to determine all the FPGA uncertainties, and uses these values to provide accurate 
	# timing analysis.  
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $IP_name IP
	upvar 1 $pin_array_name pins
	upvar 1 $ISI_name ISI
	upvar 1 $board_name board
	
	# First find the phase offset between DQ and DQS
	set dq2dqs_output_phase_offset [expr 2.0/$dll_length*360.0]

	if {$IP(write_deskew_mode) == "dynamic"} {
		# Determine the calibration uncertainty due to using T9 delay chains to centre
		set offset_from_90 [expr abs(90-(360.0/$dll_length*2))/360.0*$t(CK)]
		set t9_vt_variation_percent [get_float_table_node_delay -src {DELAYCHAIN_T9} -dst {VTVARIATION} -parameters [list IO $interface_type]]
		set t9_variation [expr $offset_from_90*2*$t9_vt_variation_percent]
		
		# Deskew will make sure the strobe in centred
		set dq2dqs_output_phase_offset 90.0		
	} else {
		set t9_variation 0.0
	}
	
	set tccs [get_tccs $IP(mem_if_memtype) $pins(dqs_pins) $t(CK) -dll_length $dll_length -config_period $t(CK)] 		

	set board_skew $board(intra_DQS_group_skew)	
	set write_board_skew $board_skew
	if {$IP(write_deskew_mode) == "dynamic"} {
		if {[catch {get_micro_node_delay -micro EXTERNAL_SKEW_THRESHOLD -parameters [list IO]} compensated_skew] != 0 || $compensated_skew == ""} {
		  # No skew compensation
		} else {
		  set compensated_skew [expr $compensated_skew / 1000.0]
		  if {$board_skew > $compensated_skew} {
			set write_board_skew [expr $board_skew - $compensated_skew]
		  } else {
			set write_board_skew 0
		  }
		}
	}

	set su   [round_3dp [expr {$t(CK)*$dq2dqs_output_phase_offset/360.0 - $write_board_skew - [lindex $tccs 0]/1000.0 - $t(DS) - $t9_variation}]]
	set hold [round_3dp [expr {$t(CK)*(0.5 - $dq2dqs_output_phase_offset/360.0) - $write_board_skew - [lindex $tccs 1]/1000.0 - $t(DH) - $t9_variation}]]
	if {$IP(write_deskew_mode) == "dynamic"} {
		set su [expr ($su + $hold)/2]
		set hold $su
	}	
	lappend summary [list "All Conditions" 0 "Write (All Conditions)" $su $hold 1 1]

}
	
##############################################################
## Read Timing Analysis
##############################################################
proc perform_flexible_read_capture_timing_analysis {opcs opcname inst family DQS_min_max io_std interface_type max_package_skew dqs_phase period all_dq_pins pin_array_name timing_parameters_array_name summary_name MP_name IP_name board_name fpga_name} {

	#################################################################################
	# This timing analysis covers the read timing constraints.  It includes support 
	# for uncalibrated and calibrated read paths.  The analysis starts by running a 
	# conventional timing analysis for the read paths and then adds support for 
	# topologies and IP options which are unique to source-synchronous data transfers.  
	# The support for further topologies includes correlation between Q and CQ signals
	# The support for further IP includes support for read-deskew calibration.
	# 
	# During read deskew calibration, the IP will adjust delay chain settings along 
	# each signal path to reduce the skew between Q pins and to centre align the CQ 
	# strobe within the DVW.  This operation has the benefit of increasing margin on the 
	# setup and hold, as well as removing some of the unknown process variation on each 
	# signal path.  This timing analysis emulates the IP process by deskewing each pin as 
	# well as accounting for the elimination of the unknown process variation.  Once the 
	# deskew emulation is complete, the analysis further considers the effect of changing 
	# the delay chain settings to the operation of the device after calibration: these 
	# effects include changes in voltage and temperature which may affect the optimality 
	# of the deskew process.
	# 
	# The timing analysis creates a read summary report indicating how the timing analysis 
	# was performed starting with a typical timing analysis before calibration.
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $pin_array_name pins
	upvar 1 $MP_name MP
	upvar 1 $IP_name IP
	upvar 1 $board_name board
	upvar 1 $fpga_name fpga

	set num_failing_path $IP(num_report_paths)


	# Debug switch. Change to 1 to get more run-time debug information
	set debug 0	
	set result 1

	########################################
	## Initialize the read analysis panel	
	set panel_name "$inst Read Capture"
	set root_folder_name [get_current_timequest_report_folder]

	if { ! [string match "${root_folder_name}*" $panel_name] } {
		set panel_name "${root_folder_name}||$panel_name"
	}
	# Create the root if it doesn't yet exist
	if {[get_report_panel_id $root_folder_name] == -1} {
		set panel_id [create_report_panel -folder $root_folder_name]
	}

	# Delete any pre-existing summary panel
	set panel_id [get_report_panel_id $panel_name]
	if {$panel_id != -1} {
		delete_report_panel -id $panel_id
	}

	set panel_id [create_report_panel -table $panel_name]
	add_row_to_table -id $panel_id [list "Operation" "Setup Slack" "Hold Slack"]

	foreach dqsclock $pins(dqs_pins) {
		lappend dqs_pins_in ${dqsclock}_IN
	}

	if {$IP(read_deskew_mode) == "dynamic"} {
		set panel_name_setup  "Before Calibration \u0028Negative slacks are OK\u0029||$inst Read Capture \u0028Before Calibration\u0029 (setup)"
		set panel_name_hold   "Before Calibration \u0028Negative slacks are OK\u0029||$inst Read Capture \u0028Before Calibration\u0029 (hold)"
	} else {
		set panel_name_setup  "Before Spatial Pessimism Removal \u0028Negative slacks are OK\u0029||$inst Read Capture (setup)"
		set panel_name_hold   "Before Spatial Pessimism Removal \u0028Negative slacks are OK\u0029||$inst Read Capture (hold)"
	}	

	######################################################################		
	## Default Read Analysis
	set before_calibration_reporting [get_ini_var -name "qsta_enable_before_calibration_ddr_reporting"]
	if {![string equal -nocase $before_calibration_reporting off]} {
		set res_0 [report_timing -detail full_path -from [get_ports $all_dq_pins] \
			-to_clock [get_clocks $dqs_pins_in] -npaths $num_failing_path -panel_name $panel_name_setup -setup -disable_panel_color -quiet]
		set res_1 [report_timing -detail full_path -from [get_ports $all_dq_pins] \
			-to_clock [get_clocks $dqs_pins_in] -npaths $num_failing_path -panel_name $panel_name_hold -hold -disable_panel_color -quiet]
	}	

	set paths_setup [get_timing_paths -from [get_ports $all_dq_pins] -to_clock [get_clocks $dqs_pins_in] -npaths 400 -setup]
	set paths_hold  [get_timing_paths -from [get_ports $all_dq_pins] -to_clock [get_clocks $dqs_pins_in] -npaths 400 -hold]		

	######################################
	## Find Memory Calibration Improvement
	######################################				
	
	set mp_setup_slack 0
	set mp_hold_slack  0
	if {($IP(read_deskew_mode) == "dynamic") && ($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		# Reduce the effect of tDQSQ on the setup slack
		set mp_setup_slack [expr $MP(DQSQ)*$t(DQSQ)]

		# Reduce the effect of tQH_time on the hold slack
		set mp_hold_slack  [expr $MP(QHS)*$t(QHS)]
	}

	#########################################
	# Go over each pin and compute its slack
	# Then include any effects that are unique
	# to source synchronous designs including
	# common clocks, signal correlation, and
	# IP calibration options to compute the
	# total slack of the instance	

	set prefix [ string map "| |*:" $inst ]
	set prefix "*:$prefix"	
	# Get some of the FPGA jitter and DCD specs
	# When not specified all jitter values are peak-to-peak jitters in ns
	set tJITper [expr [get_io_standard_node_delay -dst MEM_CK_PERIOD_JITTER -io_standard $io_std -parameters [list IO $interface_type] -period $period]/1000.0]
	set tJITdty [expr [get_io_standard_node_delay -dst MEM_CK_DC_JITTER -io_standard $io_std -parameters [list IO $interface_type]]/1000.0]
	# DCD value that is looked up is in %, and thus needs to be divided by 100
	set tDCD [expr [get_io_standard_node_delay -dst MEM_CK_DCD -io_standard $io_std -parameters [list IO $interface_type]]/100.0]
	
	# This is the peak-to-peak jitter on the whole DQ-DQS read capture path
	set DQSpathjitter [expr [get_io_standard_node_delay -dst DQDQS_JITTER -io_standard $io_std -parameters [list IO $interface_type]]/1000.0]
	# This is the proportion of the DQ-DQS read capture path jitter that applies to setup (looed up value is in %, and thus needs to be divided by 100)
	set DQSpathjitter_setup_prop [expr [get_io_standard_node_delay -dst DQDQS_JITTER_DIVISION -io_standard $io_std -parameters [list IO $interface_type]]/100.0]
	# Phase Error on DQS paths. This parameter is queried at run time
	set fpga(tDQS_PSERR) [ expr [ get_integer_node_delay -integer $::GLOBAL_ddr2_v10_1_dqs_delay_chain_length -parameters {IO MAX HIGH} -src DQS_PSERR -in_fitter ] / 1000.0 ]

	set setup_slack 1000000000
	set hold_slack  1000000000
	set default_setup_slack 1000000000
	set default_hold_slack  1000000000	
		
	# Find quiet jitter values during calibration
	set quiet_setup_jitter [expr 0.8*$DQSpathjitter*$DQSpathjitter_setup_prop]
	set quiet_hold_jitter  [expr 0.8*$DQSpathjitter*(1-$DQSpathjitter_setup_prop) + 0.8*$tJITper/2]
		
	if {($result == 1)} {

		#Go over each DQS pin
		set group_number -1
		foreach qpins $pins(q_groups) {
			
			set group_number [expr $group_number + 1]
			
			set dqspin [lindex $pins(dqs_pins) $group_number]
			set dqsnpin [lindex $pins(dqsn_pins) $group_number]
			
			##############################################
			## Find extra DQS pessimism due to correlation
			##############################################

			# Find paths from output of the input buffer to the end of the DQS periphery
			set input_buffer_node "${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*strobe_in|o" 
			set DQScapture_node [list "${prefix}*dq_ddio[${group_number}].ubidir_dq_dqs|*input_path_gen[*].capture_reg~DFFLO" \
				                      "${prefix}*dq_ddio[${group_number}].ubidir_dq_dqs|*read_data_out[*]" ]

			set DQSperiphery_min [get_path -from $input_buffer_node -to $DQScapture_node]
			set DQSperiphery_delay [max_in_collection $DQSperiphery_min "arrival_time"]
			set DQSpath_pessimism [expr ($DQSperiphery_delay-$dqs_phase/360.0*$period)*$DQS_min_max]
			
			# Go over each DQ pin in group
			set q_index 0
			foreach qpin $qpins {
				regexp {\d+} $qpin q_pin_index
			
				# Perform the default timing analysis to get required and arrival times
				set pin_setup_slack [min_in_collection_from_name $paths_setup "slack" $qpin]
				set pin_hold_slack  [min_in_collection_from_name $paths_hold "slack" $qpin]

				set default_setup_slack [min $default_setup_slack $pin_setup_slack]
				set default_hold_slack  [min $default_hold_slack  $pin_hold_slack]		

				if { $debug } {
					puts "READ: $group_number $dqspin $dqpin $pin_setup_slack $pin_hold_slack (MP: $mp_setup_slack $mp_hold_slack)"
				}
			
				################################
				## Add the memory calibration improvement
				################################
				
				set pin_setup_slack [expr $pin_setup_slack + $mp_setup_slack]
				set pin_hold_slack [expr $pin_hold_slack + $mp_hold_slack]
				
				#############################################
				## Find extra DQ pessimism due to correlation
				#############################################
				
				# Find paths from output of the input buffer to the end of the DQ periphery
				set input_buffer_node_dq ${inst}|controller_phy_inst|memphy_top_inst|umemphy|uio_pads|dq_ddio[${group_number}].ubidir_dq_dqs|*pad_gen[${q_index}].data_in|o
				set DQcapture_node [list "${prefix}*dq_ddio[${group_number}].ubidir_dq_dqs|*input_path_gen[${q_index}].capture_reg~DFFLO" \
				                      "${prefix}*dq_ddio[${group_number}].ubidir_dq_dqs|*read_data_out[${q_index}]" ]

				set DQperiphery_min [get_path -from $input_buffer_node_dq -to $DQcapture_node -min_path -nworst 10]
				set DQperiphery_delay [min_in_collection $DQperiphery_min "arrival_time"]
				set DQpath_pessimism [expr $DQperiphery_delay*$DQS_min_max]
				
				#########################################
				## Merge current slacks with other slacks
				#########################################			

				# If read deskew is available, the setup and hold slacks for this pin will be equal
				#   and can also remove the extra CQ pessimism removal
				if {$IP(read_deskew_mode) == "dynamic"} {
				
					# Consider the maximum range of the deskew when deskewing
					set shift_setup_slack [expr (($pin_setup_slack + $quiet_setup_jitter) + ($pin_hold_slack + $quiet_hold_jitter))/2 - $pin_setup_slack - $quiet_setup_jitter]
					set max_read_deskew [expr $IP(read_deskew_range)*$IP(quantization_T1)]
					if {$shift_setup_slack >= $max_read_deskew} {
						if { $debug } {
							puts "limited setup"
						}
						set pin_setup_slack [expr $pin_setup_slack + $max_read_deskew]
						set pin_hold_slack [expr $pin_hold_slack - $max_read_deskew]
					} elseif {$shift_setup_slack <= -$max_read_deskew} {
						if { $debug } {
							puts "limited hold"
						}
						set pin_setup_slack [expr $pin_setup_slack - $max_read_deskew]
						set pin_hold_slack [expr $pin_hold_slack + $max_read_deskew]
					} else {
						# In this case we can also consider the CQ path pessimism since we can guarantee we have enough delay chain settings to align it
						set pin_setup_slack [expr $pin_setup_slack + $shift_setup_slack + $DQSpath_pessimism/2 + $DQpath_pessimism/2]
						set pin_hold_slack [expr $pin_hold_slack - $shift_setup_slack + $DQSpath_pessimism/2 + $DQpath_pessimism/2]
					}
				} else {
					# For uncalibrated calls, there is some spatial correlation between Q and CQ signals, so remove
					# some of the pessimism
					set total_DQ_DQS_pessimism [expr $DQSpath_pessimism + $DQpath_pessimism]
					set dqs_width [llength $qpins]
					if {$dqs_width <= 9} {
						set pin_setup_slack [expr $pin_setup_slack + 0.35*$total_DQ_DQS_pessimism]
						set pin_hold_slack  [expr $pin_hold_slack  + 0.35*$total_DQ_DQS_pessimism]
					} 
				}

				set setup_slack [min $setup_slack $pin_setup_slack]
				set hold_slack  [min $hold_slack $pin_hold_slack]
				
				if { $debug } {
					puts "READ:               $DQSpath_pessimism $DQpath_pessimism ($pin_setup_slack $pin_hold_slack $setup_slack $hold_slack)" 
				}
				set q_index [expr $q_index + 1]
			}
		}
	}
	
	#########################################################
	## Consider some post calibration effects on calibration
	##  and output the read summary report
	#########################################################	
	
	set positive_fcolour [list "black" "blue" "blue"]
	set negative_fcolour [list "black" "red"  "red"]	

	set rc_summary [list]	
	
	set fcolour [get_colours $default_setup_slack $default_hold_slack]
	if {$IP(read_deskew_mode) == "dynamic"} {
		lappend rc_summary [list "  Before Calibration Read Capture" [format_3dp $default_setup_slack] [format_3dp $default_hold_slack]]
	} else {
		lappend rc_summary [list "  Standard Read Capture" [format_3dp $default_setup_slack] [format_3dp $default_hold_slack]] 
	}
	
	if {($IP(read_deskew_mode) == "dynamic") && ($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		lappend rc_summary [list "  Memory Calibration" [format_3dp $mp_setup_slack] [format_3dp $mp_hold_slack]] 
	}
	
	if {$IP(read_deskew_mode) == "dynamic"} {
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		# Remove external delays (add slack) that are fixed by the dynamic deskew
		[catch {get_float_table_node_delay -src {DELAYCHAIN_T1} -dst {VTVARIATION} -parameters [list IO $interface_type]} t1_vt_variation_percent]		
		set setup_slack [expr $setup_slack + $board(intra_DQS_group_skew) + [round_3dp [expr (1.0-$t1_vt_variation_percent)*$fpga(tDQS_PSERR)]]]
		set hold_slack [expr $hold_slack + $board(intra_DQS_group_skew) + [round_3dp [expr (1.0-$t1_vt_variation_percent)*$fpga(tDQS_PSERR)]]]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		set deskew_setup [expr $setup_slack - $default_setup_slack - $mp_setup_slack]
		set deskew_hold  [expr $hold_slack - $default_hold_slack - $mp_hold_slack]
		lappend rc_summary [list "  Deskew Read" [format_3dp $deskew_setup] [format_3dp $deskew_hold]]
		
		# Consider errors in the dynamic deskew
		set t1_quantization $IP(quantization_T1)
		set setup_slack [expr $setup_slack - $t1_quantization]
		set hold_slack  [expr $hold_slack - $t1_quantization]
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend rc_summary [list "  Quantization error" [format_3dp [expr 0-$t1_quantization]] [format_3dp [expr 0-$t1_quantization]]]
		
		# Consider variation in the delay chains used during dynamic deksew
		set t1_variation [expr [min [expr abs(90-$dqs_phase)/360.0*$period + ($MP(DQSQ)*$t(DQSQ) + $MP(QHS)*$t(QHS) + 2*$board(intra_DQS_group_skew) + 2*$max_package_skew + $fpga(tDQS_PSERR))] $max_read_deskew]*2*$t1_vt_variation_percent]
		set setup_slack [expr $setup_slack - $t1_variation]
		set hold_slack  [expr $hold_slack - $t1_variation]	
		if { $debug } {
			puts "	$setup_slack $hold_slack"
		}
		lappend rc_summary [list "  Calibration uncertainty" [format_3dp [expr 0-$t1_variation]] [format_3dp [expr 0-$t1_variation]]]
	} else {
		set pessimism_setup [expr $setup_slack - $default_setup_slack - $mp_setup_slack]
		set pessimism_hold  [expr $hold_slack - $default_hold_slack - $mp_hold_slack]
		lappend rc_summary [list "  Spatial correlation pessimism removal" [format_3dp $pessimism_setup] [format_3dp $pessimism_hold]] 
	}
	
	if {$IP(read_deskew_mode) == "dynamic"} {
		set fcolour [get_colours $setup_slack $hold_slack] 
		add_row_to_table -id $panel_id [list "After Calibration Read Capture" [format_3dp $setup_slack] [format_3dp $hold_slack]] -fcolor $fcolour
		lappend summary [list $opcname 0 "Read Capture ($opcname)" $setup_slack $hold_slack]
	} else {
		set fcolour [get_colours $setup_slack $hold_slack] 
		add_row_to_table -id $panel_id [list "Read Capture" [format_3dp $setup_slack] [format_3dp $hold_slack]] -fcolor $fcolour
		lappend summary [list $opcname 0 "Read Capture ($opcname)" $setup_slack $hold_slack]  
	}	
	
	foreach summary_line $rc_summary {
		add_row_to_table -id $panel_id $summary_line -fcolors $positive_fcolour
	}	
}

proc perform_macro_read_capture_timing_analysis {opcs opcname inst dqs_phase dll_length interface_type all_dq_pins pin_array_name timing_parameters_array_name summary_name IP_name board_name} {

	################################################################################
	# This timing analysis covers the read timing constraints using the Altera 
	# characterized values.  Altera performs characterization on its devices and IP 
	# to determine all the FPGA uncertainties, and uses these values to provide accurate 
	# timing analysis.  
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $IP_name IP
	upvar 1 $pin_array_name pins
	upvar 1 $board_name board
	
	if {$IP(read_deskew_mode) == "dynamic"} {
		# Determine the calibration uncertainty due to using T1 delay chains to centre
		set offset_from_90 [expr abs(90-$dqs_phase)/360.0*$t(CK)]
		set t1_vt_variation_percent [get_float_table_node_delay -src {DELAYCHAIN_T1} -dst {VTVARIATION} -parameters [list IO $interface_type]]
		set t1_variation [expr $offset_from_90*2*$t1_vt_variation_percent]
		
		# Deskew will make sure the strobe in centred
		set dqs_phase 90.0		
	} else {
		set t1_variation 0.0
	}	
	
	set board_skew $board(intra_DQS_group_skew)
	set read_board_skew board_skew
	if {$IP(read_deskew_mode) == "dynamic"} {
		if {[catch {get_micro_node_delay -micro EXTERNAL_SKEW_THRESHOLD -parameters [list IO]} compensated_skew] != 0 || $compensated_skew == ""} {
			# No skew compensation
		} else {
			set compensated_skew [expr $compensated_skew / 1000.0]
			if {$board_skew > $compensated_skew} {
				set read_board_skew [expr $board_skew - $compensated_skew]
			} else {
				set read_board_skew 0
			}
		}
	}
	
	# Read capture
	set tsw [get_tsw $IP(mem_if_memtype) $pins(dqs_pins) $t(CK) -dll_length $dll_length -config_period $t(CK)]  
	set su   [round_3dp [expr {$t(CK)*$dqs_phase/360.0 - [lindex $tsw 0]/1000.0 - $t(DQSQ) - $read_board_skew - $t1_variation}]]
	set hold [round_3dp [expr {$t(QH_time) - $dqs_phase/360.0 * $t(CK) - [lindex $tsw 1]/1000.0 - $read_board_skew - $t1_variation}]]
	if {($IP(read_deskew_mode) == "dynamic") && ($IP(RDIMM) == 1)} {
		set su [expr ($su + $hold)/2]
		set hold $su
	}	
	lappend summary [list "All Conditions" 0 "Read Capture (All Conditions)" $su $hold 1 1]

}

##############################################################
## Other Timing Analysis
##############################################################

proc perform_phy_analyses {opcs opcname inst pin_array_name timing_parameters_array_name summary_name IP_name} {

	################################################################################
	# The PHY analysis concerns the timing requirements of the PHY which includes
	# soft registers in the FPGA core as well as some registers in the hard periphery
	# The read capture and write registers are not analyzed here, even though they 
	# are part of the PHY since they are timing analyzed separately. 
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $pin_array_name pins
	upvar 1 $IP_name IP
	
	set num_failing_path $IP(num_report_paths)

	set entity_names_on [ are_entity_names_on ]

	set prefix [ string map "| |*:" $inst ]
	set prefix "*:$prefix"

	if { ! $entity_names_on } {
		set core_regs [remove_from_collection [get_registers $inst|*] [get_registers $pins(read_capture_ddio)]]
	} else {
		set core_regs [remove_from_collection [get_registers $prefix|*] [get_registers $pins(read_capture_ddio)]]
	}

	# Core
	set res_0 [report_timing -detail full_path -to $core_regs -npaths $num_failing_path -panel_name "$inst Core (setup)" -setup]
	set res_1 [report_timing -detail full_path -to $core_regs -npaths $num_failing_path -panel_name "$inst Core (hold)" -hold]
	lappend summary [list $opcname 0 "Core ($opcname)" [lindex $res_0 1] [lindex $res_1 1] [lindex $res_0 0] [lindex $res_1 0]]

	# Core Recovery/Removal
	set res_0 [report_timing -detail full_path -to $core_regs -npaths $num_failing_path -panel_name "$inst Core Recovery/Removal (recovery)" -recovery]
	set res_1 [report_timing -detail full_path -to $core_regs -npaths $num_failing_path -panel_name "$inst Core Recovery/Removal (removal)" -removal]
	lappend summary [list $opcname 0 "Core Recovery/Removal ($opcname)" [lindex $res_0 1] [lindex $res_1 1] [lindex $res_0 0] [lindex $res_1 0]]
	

}

proc perform_ac_analyses {opcs opcname inst pin_array_name timing_parameters_array_name summary_name IP_name} {

	################################################################################
	# The adress/command analysis concerns the timing requirements of the pins (other
	# than the D/Q pins) which go to the memory device/DIMM.  These include address/command
	# pins, some of which are runing at Single-Data-Rate (SDR) and some which are 
	# running at Half-Data-Rate (HDR).  
	################################################################################
	
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $pin_array_name pins
	upvar 1 $IP_name IP

	set num_failing_path $IP(num_report_paths)

	set add_pins $pins(add_pins)
	set ba_pins $pins(ba_pins)
	set cmd_pins $pins(cmd_pins)
	set ac_pins [ concat $add_pins $ba_pins $cmd_pins ]

	set entity_names_on [ are_entity_names_on ]

	set prefix [ string map "| |*:" $inst ]
	set prefix "*:$prefix"

	set res_0 [report_timing -detail full_path -to $ac_pins -npaths $num_failing_path -panel_name "$inst Address Command (setup)" -setup]
	set res_1 [report_timing -detail full_path -to $ac_pins -npaths $num_failing_path -panel_name "$inst Address Command (hold)" -hold]
	lappend summary [list $opcname 0 "Address Command ($opcname)" [lindex $res_0 1] [lindex $res_1 1] [lindex $res_0 0] [lindex $res_1 0]]
}

##############################################################
## Write Levelling Timing Analysis
##############################################################

proc perform_flexible_write_levelling_timing_analysis {opcs opcname instname family period dll_length interface_type tJITper tJITdty tDCD pll_steps pin_array_name timing_parameters_array_name summary_name  MP_name IP_name SSN_name board_name ISI_parameters_name} {

	################################################################################
	# The write levelling analysis concerns meeting the DDR3 requirements between DQS 
	# and CK such as tDQSS and tDSH/tDSS.  The write-levelling is achieved through 
	# calibration by changing the phase of the DQS signal and finding the lowest and
	# and higtest phases that allow writes to complete and then picking a midpoint
	# phase. Because of this, the exact length of the DQS and CK traces 
	# don't affect the timing analysis, and the calibration will remove any static 
	# offset from the rest of the path too.  With static offset removed, the 
	# remaining uncertainties with be limited to mostly VT variation, and jitter.
	# Note that the special write levelling registers are unused in the UniPHY calibration.
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $IP_name IP
	upvar 1 $pin_array_name pins
	upvar 1 $board_name board	
	upvar 1 $MP_name MP
	upvar 1 $SSN_name SSN
	upvar 1 $ISI_parameters_name ISI		
	
	#################################
	## tDQSS specification
	
	# Ideal setup and hold slacks is the tDQSS specification
	set tDQSS_CK $t(DQSS)
	if {($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		set tDQSS_CK [expr 0.5 - (1-$MP(DQSS))*(0.5-$t(DQSS))]
	} elseif {$IP(mp_calibration) == 1} {
		set tDQSS_CK [expr 0.5 - (1-$MP(DQSS)/2)*(0.5-$t(DQSS))]
	}
	set tDQSS [expr $tDQSS_CK * $period]
	set setup_slack $tDQSS
	set hold_slack  $tDQSS
	
	# Remove Multirank board skew effects - Calibration will calibrate to the average of multiple ranks
	set max_calibration_offset 0
	if {$IP(num_ranks) > 1} {
		set max_calibration_offset [expr abs($board(maxCK_DQS_skew) - $board(minCK_DQS_skew))]
		set setup_slack [expr $setup_slack - $max_calibration_offset - $ISI(DQS)/2]
		set hold_slack  [expr $hold_slack  - $max_calibration_offset - $ISI(DQS)/2]
	}		
	
	# Remove the uncertainty due to VT variations in T9
	set write_levelling_phase_step [get_micro_node_delay -micro WL_PHASE_TAP -parameters [list IO $interface_type]]
	set max_t9_delay [expr $write_levelling_phase_step/360.0*$period]
	set t9_vt_variation_percent [get_float_table_node_delay -src {DELAYCHAIN_T9} -dst {VTVARIATION} -parameters [list IO $interface_type]]
	set t9_vt_variation [expr $max_t9_delay*$t9_vt_variation_percent*2]
	set setup_slack [expr $setup_slack - $t9_vt_variation]
	set hold_slack  [expr $hold_slack  - $t9_vt_variation]
	
	# Remove the jitter on the clock out to the memory (CK and DQS could be in worst-case directions) 
	set setup_slack [expr $setup_slack - $tJITper/2 - $tJITper/2]
	set hold_slack  [expr $hold_slack  - $tJITper/2 - $tJITper/2]
	
	# Remove the jitter on the Write Levelling delay chains (this is peak-to-peak jitter
	set WL_jitter [expr [get_micro_node_delay -micro WL_JITTER -parameters [list IO $interface_type]]/1000.0]
	set setup_slack [expr $setup_slack - $WL_jitter/2]
	set hold_slack  [expr $hold_slack  - $WL_jitter/2]	
	
	# Remove SSN effects
	set setup_slack [expr $setup_slack - $SSN(pushout_o) - $SSN(pullin_o)]
	set hold_slack  [expr $hold_slack  - $SSN(pushout_o) - $SSN(pullin_o)]	
	
	# Remove the worst-case quantization uncertainty
	set setup_slack [expr $setup_slack - $IP(quantization_WL)/2]
	set hold_slack [expr $hold_slack - $IP(quantization_WL)/2]

	# This section of the analysis tries to make sure that the CK path is longer than the DQS path so that we
	# can calibrate the first DQS group
	
	foreach q_group $pins(q_groups) {
		set q_group $q_group
		lappend q_groups $q_group
	}
	set all_dq_pins [ join [ join $q_groups ] ]
	set add_pins $pins(add_pins)

	# Get the launch times of CLK and DQS
	set data_hold  [get_timing_paths -to [get_ports $all_dq_pins] -hold]
	set ac_setup   [get_timing_paths -to $add_pins -setup]
	set dqs_launch_time [expr [max_in_collection $data_hold "latch_time"] + $t(CK)/$dll_length*2 - $t(CK)]
	set clk_launch_time [expr [min_in_collection $ac_setup "latch_time"]]

	# Get the min CLK delay
	set clk_min [min_in_collection [get_path -from $pins(pll_ck_clock) -to $pins(ck_pins) -min_path] "arrival_time"]

	# Get the max DQS delay, slightly complicated because TimeQuest doesn't allow the path to go through the levelling delay chains
	set dqs_path_part1 [get_path -to $pins(dqs_pins)]
	foreach_in_collection path $dqs_path_part1 { set tempstartnode [get_path_info $path -from] }
	set tempstart [get_node_info $tempstartnode -name]
	set dqs_path_part2 [get_path -from $pins(pll_write_clock) -to $tempstart]
	set dqs_max [expr [max_in_collection $dqs_path_part1 "arrival_time"] + [max_in_collection $dqs_path_part2 "arrival_time"]]

	# Get the total delays
	set dqs_time [expr $dqs_launch_time + $dqs_max + $t(WL_DCD) + $t(WL_JITTER) + $t(WL_PSE) + $board(inter_DQS_group_skew) + $SSN(pushout_o) + $ISI(DQS)/2]
	set clk_time [expr $clk_launch_time + $clk_min - $tJITper/2 + $board(minCK_DQS_skew) - $SSN(pullin_o)]
	
	if {$dqs_time > $clk_time} {
		post_message -type warning "Write Leveling may not be able to calibrate due to longer delay to DQS pins compared to CK pins"
		set setup_slack [expr $clk_time - $dqs_time]
		set hold_slack 0
	} else {
		set setup_slack [min $setup_slack [expr $clk_time - $dqs_time]]
	}

	lappend summary [list $opcname 0 "Write Leveling tDQSS ($opcname)" $setup_slack $hold_slack]
	
	#################################
	## tDSS/tDSH specification	
	
	# Ideal setup and hold slacks is half the period
	set setup_slack [expr 0.5*$period]
	set hold_slack  [expr 0.5*$period]	
	
	# Remove the tDSS and tDSH specifications
	set tDSS_CK $t(DSS)
	set tDSH_CK $t(DSH)
	if {($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		set tDSS_CK [expr (1.0-$MP(DSS))*$t(DSS)]
		set tDSH_CK [expr (1.0-$MP(DSH))*$t(DSH)]	
	} elseif {$IP(mp_calibration) == 1} {
		set tDSS_CK [expr (1.0-$MP(DSS)/2.5)*$t(DSS)]
		set tDSH_CK [expr (1.0-$MP(DSH)/2.5)*$t(DSH)]	
	}
	set tDSS [expr $tDSS_CK * $period]
	set tDSH [expr $tDSH_CK * $period]
	set setup_slack [expr $setup_slack - $tDSS]
	set hold_slack  [expr $hold_slack  - $tDSH]
	
	# Remove the uncertainty due to VT variations in T9
	set setup_slack [expr $setup_slack - $t9_vt_variation]
	set hold_slack  [expr $hold_slack  - $t9_vt_variation]		
	
	# Remove the jitter on the clock out to the memory (CK and DQS could be in worst-case directions) 
	set setup_slack [expr $setup_slack - $tJITper/2 - $tJITper/2]
	set hold_slack  [expr $hold_slack  - $tJITper/2 - $tJITper/2]
	
	# Remove the jitter on the Write Levelling delay chains (this is peak-to-peak jitter
	set setup_slack [expr $setup_slack - $WL_jitter/2]
	set hold_slack  [expr $hold_slack  - $WL_jitter/2]	
	
	# Remove SSN effects
	set setup_slack [expr $setup_slack - $SSN(pushout_o) - $SSN(pullin_o)]
	set hold_slack  [expr $hold_slack  - $SSN(pushout_o) - $SSN(pullin_o)]		
	
	# Multirank derating
	if {$IP(num_ranks) > 1} {
		set max_calibration_offset [expr abs($board(maxCK_DQS_skew) - $board(minCK_DQS_skew))]
		set setup_slack [expr $setup_slack - $max_calibration_offset - $ISI(DQS)/2]
		set hold_slack  [expr $hold_slack  - $max_calibration_offset - $ISI(DQS)/2]
	}		
	
	# Remove the worst-case quantization uncertainty
	set setup_slack [expr $setup_slack - $IP(quantization_WL)/2]
	set hold_slack [expr $hold_slack - $IP(quantization_WL)/2]	
	
	# Duty Cycle Effects
	set setup_slack [expr $setup_slack - $t(WL_DCJ) - $t(WL_DCD)]
	set hold_slack  [expr $hold_slack  - $t(WL_DCJ) - $t(WL_DCD)]			
	
	# Duty Cycle Correction
	if {$IP(write_dcc) == "dynamic"} {
		# First remove the Systematic DCD
		set setup_slack [expr $setup_slack + $t(WL_DCD)]
		set hold_slack  [expr $hold_slack + $t(WL_DCD)]
		
		# Add errors in the DCC
		set DCC_quantization $IP(quantization_DCC)
		set setup_slack [expr $setup_slack - $DCC_quantization]
		set hold_slack  [expr $hold_slack - $DCC_quantization]
		
		# Consider variation in the DCC 
		set dcc_vt_variation_percent [get_float_table_node_delay -src {DELAYCHAIN_DUTY_CYCLE} -dst {VTVARIATION} -parameters [list IO $interface_type]]
		set dcc_variation [expr $t(WL_DCD)*2*$dcc_vt_variation_percent]
		set setup_slack [expr $setup_slack - $dcc_variation]
		set hold_slack  [expr $hold_slack - $dcc_variation]		
	}	

	lappend summary [list $opcname 0 "Write Leveling tDSS/tDSH ($opcname)" $setup_slack $hold_slack]

}

proc perform_macro_write_levelling_timing_analysis {opcs opcname instname family period dll_length interface_type tJITper tJITdty tDCD pll_steps pin_array_name timing_parameters_array_name summary_name  MP_name IP_name SSN_name board_name ISI_parameters_name} {

	################################################################################
	# The write levelling analysis concerns meeting the DDR3 requirements between DQS 
	# and CK such as tDQSS and tDSH/tDSS.  The write-levelling is achieved through 
	# calibration by changing the phase of the DQS signal and finding the lowest and
	# and higtest phases that allow writes to complete and then picking a midpoint
	# phase. Because of this, the exact length of the DQS and CK traces 
	# don't affect the timing analysis, and the calibration will remove any static 
	# offset from the rest of the path too.  With static offset removed, the 
	# remaining uncertainties with be limited to mostly VT variation, and jitter.
	# Note that the special write levelling registers are unused in the UniPHY calibration.
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $IP_name IP
	upvar 1 $pin_array_name pins
	upvar 1 $board_name board	
	upvar 1 $MP_name MP
	upvar 1 $SSN_name SSN
	upvar 1 $ISI_parameters_name ISI	
	
	# The ALTMEMPHY write leveling algorithm increases the DQS output phase and delay until the first 0->1 is found in the write leveling feedback, thus DQS is slightly skewed 1 step to the right of ideal alignment with CK
	set mem_fmax [expr 1.0/$period*1000.0]
	set min_period [round_3dp [expr 1000.0/$mem_fmax]]

	set tWLS $t(WLS)
	set tWLH $t(WLH)
	set tDQSS_CK $t(DQSS)
	set tDQSS [expr $tDQSS_CK * $period]
	
	set fpga_leveling_step $IP(quantization_T9)
	set fpga_leveling_step_variation 0.019

	# tDQSS specification
	set max_write_leveling_delay_steps [expr floor($period / $dll_length / $fpga_leveling_step)]
	set write_leveling_delay_VT_variation [expr $max_write_leveling_delay_steps * $fpga_leveling_step_variation]
	set min_write_leveling_inaccuracy [expr $write_leveling_delay_VT_variation + $fpga_leveling_step/2]
	set max_write_leveling_inaccuracy [expr $write_leveling_delay_VT_variation + $fpga_leveling_step/2]
	
	set su [round_3dp [expr $tDQSS - $min_write_leveling_inaccuracy ]]
	set hold [round_3dp [expr $tDQSS - $max_write_leveling_inaccuracy ]]
	lappend summary [list $opcname 0 "Write Leveling tDQSS ($opcname)" $su $hold]
	
	# tDSS/tDSH specification
	set CK_period_jitter $tJITper
	set tDSS_CK $t(DSS)
	set tDSH_CK $t(DSH)
	set tDSS [expr $tDSS_CK * $period]
	set tDSH [expr $tDSH_CK * $period]
	set min_DQS_low [expr $period*(0.5 - $tDCD) - $tJITdty]
	set min_DQS_high [expr $period*(0.5 - $tDCD) - $tJITdty]
	
	set su [round_3dp [expr $min_DQS_low - $max_write_leveling_inaccuracy - $CK_period_jitter/2 - $tDSS ]]
	set hold [round_3dp [expr $min_DQS_high - $min_write_leveling_inaccuracy - $tDSH ]]
	lappend summary [list $opcname 0 "Write Leveling tDSS/tDSH ($opcname)" $su $hold]

}

proc perform_resync_timing_analysis {opcs opcname inst fbasename family DQS_min_max io_std interface_type period pin_array_name timing_parameters_array_name summary_name MP_name IP_name board_name fpga_name SSN_name} {

	################################################################################
	# The resynchronization timing analysis concerns transferring read data that
	# has been captured with a DQS strobe to a clock domain under the control of
	# the UniPHY. A special FIFO is used to resynchronize the data which has a wide
	# tolerance to any changes in the arrival time of data from DQS groups
	################################################################################

	########################################
	## Need access to global variables
	upvar 1 $summary_name summary
	upvar 1 $timing_parameters_array_name t
	upvar 1 $pin_array_name pins
	upvar 1 $MP_name MP
	upvar 1 $IP_name IP
	upvar 1 $board_name board
	upvar 1 $fpga_name fpga
	upvar 1 $SSN_name SSN
	
	set num_paths 5000

	########################################
	## Node names
	set dqs_pins *${fbasename}_read_datapath:uread_datapath|read_capture_clk_div2[*]
	set fifo *${fbasename}_flop_mem:read_buffering[*].read_subgroup[*].uread_fifo|data_stored[*][*]
	set reg_in_rd_clk_domain *${fbasename}_flop_mem:read_buffering[*].read_subgroup[*].uread_fifo|rd_data[*]
	set reg_wr_address *${fbasename}_read_datapath:uread_datapath|read_buffering[*].read_subgroup[*].wraddress[*]
	set reg_rd_address *${fbasename}_read_datapath:uread_datapath|read_buffering[*].read_subgroup[*].rdaddress[*]
	
	########################################	
	## Paths
	set max_DQS_to_fifo_paths  [get_path -from $dqs_pins -to $fifo -npaths $num_paths -nworst 1]
	set min_DQS_to_fifo_paths  [get_path -from $dqs_pins -to $fifo -npaths $num_paths -min_path  -nworst 1]
	
	set max_fifo_to_rd_clk_domain_paths [get_path -from $fifo -to $reg_in_rd_clk_domain -npaths $num_paths  -nworst 1]
	set min_fifo_to_rd_clk_domain_paths [get_path -from $fifo -to $reg_in_rd_clk_domain -npaths $num_paths -min_path  -nworst 1]
	
	set max_DQS_to_wr_address_paths [get_path -from $dqs_pins -to $reg_wr_address -npaths $num_paths -nworst 1]
	set min_DQS_to_wr_address_paths [get_path -from $dqs_pins -to $reg_wr_address -npaths $num_paths -min_path  -nworst 1]
	
	set max_rd_address_to_rd_data_paths [get_path -from $reg_rd_address -to $reg_in_rd_clk_domain -npaths $num_paths -nworst 1]
	set min_rd_address_to_rd_data_paths [get_path -from $reg_rd_address -to $reg_in_rd_clk_domain -npaths $num_paths -min_path -nworst 1]
	
	set max_dqs_common_to_fifo [max_in_collection [get_path -from $dqs_pins -to $fifo -nworst 1] "arrival_time"]
	
	##########################################
	## Limit to one endpoint/startpoint
	
	foreach_in_collection path $max_DQS_to_fifo_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set startpoint [get_node_info -name [get_path_info $path -from]]
		if {[info exist max_DQS_to_fifo_paths_max($startpoint)]} {
			if {$arrival_time > $max_DQS_to_fifo_paths_max($startpoint)} {
				set max_DQS_to_fifo_paths_max($startpoint) $arrival_time
			}
		} else {
			set max_DQS_to_fifo_paths_max($startpoint) $arrival_time
		}
	}
	
	foreach_in_collection path $min_DQS_to_fifo_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set startpoint [get_node_info -name [get_path_info $path -from]]
		if {[info exist min_DQS_to_fifo_paths_min($startpoint)]} {
			if {$arrival_time < $min_DQS_to_fifo_paths_min($startpoint)} {
				set min_DQS_to_fifo_paths_min($startpoint) $arrival_time
			}
		} else {
			set min_DQS_to_fifo_paths_min($startpoint) $arrival_time
		}
	}	

	
	foreach_in_collection path $max_fifo_to_rd_clk_domain_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set endpoint [get_node_info -name [get_path_info $path -to]]
		if {[info exist max_fifo_to_rd_clk_domain_paths_max($endpoint)]} {
			if {$arrival_time > $max_fifo_to_rd_clk_domain_paths_max($endpoint)} {
				set max_fifo_to_rd_clk_domain_paths_max($endpoint) $arrival_time
			}
		} else {
			set max_fifo_to_rd_clk_domain_paths_max($endpoint) $arrival_time
		}
	}
	
	foreach_in_collection path $min_fifo_to_rd_clk_domain_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set endpoint [get_node_info -name [get_path_info $path -to]]
		if {[info exist min_fifo_to_rd_clk_domain_paths_min($endpoint)]} {
			if {$arrival_time < $min_fifo_to_rd_clk_domain_paths_min($endpoint)} {
				set min_fifo_to_rd_clk_domain_paths_min($endpoint) $arrival_time
			}
		} else {
			set min_fifo_to_rd_clk_domain_paths_min($endpoint) $arrival_time
		}
	}
	
	foreach_in_collection path $max_rd_address_to_rd_data_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set endpoint [get_node_info -name [get_path_info $path -to]]
		if {[info exist max_rd_address_to_rd_data_paths_max($endpoint)]} {
			if {$arrival_time > $max_rd_address_to_rd_data_paths_max($endpoint)} {
				set max_rd_address_to_rd_data_paths_max($endpoint) $arrival_time
			}
		} else {
			set max_rd_address_to_rd_data_paths_max($endpoint) $arrival_time
		}
	}
	
	foreach_in_collection path $min_rd_address_to_rd_data_paths {
		set arrival_time [get_path_info $path -arrival_time]
		set endpoint [get_node_info -name [get_path_info $path -to]]
		if {[info exist min_rd_address_to_rd_data_paths_min($endpoint)]} {
			if {$arrival_time < $min_rd_address_to_rd_data_paths_min($endpoint)} {
				set min_rd_address_to_rd_data_paths_min($endpoint) $arrival_time
			}
		} else {
			set min_rd_address_to_rd_data_paths_min($endpoint) $arrival_time
		}
	}		
	
	########################################
	## TCO times
	set i 0
	set tco_fifo_min 0
	set tco_fifo_max 0
	foreach_in_collection register [get_keepers $fifo] {
		set tcotemp [get_register_info $register -tco]
		if {$i == 0} {
			set tco_fifo_min $tcotemp
			set tco_fifo_max $tcotemp
		} else {
			if {$tcotemp < $tco_fifo_min} {
				set tco_fifo_min $tcotemp
			} elseif {$tcotemp > $tco_fifo_max} {
				set tco_fifo_max $tcotemp
			}
		}
		incr i
	}
	set i 0
	set tco_wr_address_min 0
	set tco_wr_address_max 0
	foreach_in_collection register [get_keepers $reg_wr_address] {
		set tcotemp [get_register_info $register -tco]
		if {$i == 0} {
			set tco_wr_address_min $tcotemp
			set tco_wr_address_max $tcotemp
		} else {
			if {$tcotemp < $tco_wr_address_min} {
				set tco_wr_address_min $tcotemp
			} elseif {$tcotemp > $tco_wr_address_max} {
				set tco_wr_addressmax $tcotemp
			}
		}
		incr i
	}
	
	########################################
	## Other parameters
	set fly_by_wire 0
	set min_latency 1
	set max_latency 2.5
	set fifo_depth [get_collection_size [get_keepers *${fbasename}_flop_mem:read_buffering[0].read_subgroup[0].uread_fifo|data_stored[*][0]]]	
	if {($IP(mp_calibration) == 1) && ($IP(num_ranks) == 1)} {
		# Reduce the effect of tDQSCK
		set mp_DQSCK [expr $MP(DQSCK)*$t(DQSCK)]
	} else {
		set mp_DQSCK 0
	}
	set hf_DQS_variation [expr [get_io_standard_node_delay -dst MEM_CK_PERIOD_JITTER -io_standard $io_std -parameters [list IO $interface_type] -period $period]/1000.0*2/2]
	set hf_DQS_variation [expr $hf_DQS_variation + $SSN(pushout_o) + $SSN(pullin_o) + 2*$t(DQSCK) - 2*$mp_DQSCK + $SSN(pullin_i)]
	set hf_DQS_variation [expr $hf_DQS_variation + [get_float_table_node_delay -src {DELAYCHAIN_T9} -dst {VTVARIATION} -parameters [list IO $interface_type]]*$max_dqs_common_to_fifo/2]
	
	########################################
	## Board parameters
	set board_skew [expr $board(inter_DQS_group_skew)/2.0]
	if {$IP(num_ranks) > 1} {
		set board_skew [expr $board_skew + $board(tpd_inter_DIMM)]
	}	

	########################################
	## Body of Resync analysis
	## Go over each DQ pin

	set total_setup_slack 10000000
	set total_hold_slack  10000000
	
	set regs [get_keepers $reg_in_rd_clk_domain]
	set dqs_regs [get_keepers $dqs_pins]
	set dqs_regs_list [list]
	foreach_in_collection reg $dqs_regs {
		lappend dqs_regs_list [get_register_info -name $reg]
	}
	lsort $dqs_regs_list

	foreach_in_collection reg $regs {
	
		set reg_name [get_register_info -name $reg]
		regexp {read_buffering\[(\d+)\]\.read_subgroup} $reg_name match dqs_group_number

		set dqs_pin [lindex $dqs_regs_list $dqs_group_number]

		set max_DQS_to_fifo $max_DQS_to_fifo_paths_max($dqs_pin)
		set min_DQS_to_fifo $min_DQS_to_fifo_paths_min($dqs_pin)
		set max_fifo_to_rd_clk_domain $max_fifo_to_rd_clk_domain_paths_max($reg_name)
		set min_fifo_to_rd_clk_domain $min_fifo_to_rd_clk_domain_paths_min($reg_name)
		set max_rd_address_to_rd_data $max_rd_address_to_rd_data_paths_max($reg_name)
		set min_rd_address_to_rd_data $min_rd_address_to_rd_data_paths_min($reg_name)


		################
		# Setup analysis	
		set setup_arrival_time  [expr ($max_DQS_to_fifo - $min_DQS_to_fifo) + $tco_fifo_max + $max_fifo_to_rd_clk_domain + $hf_DQS_variation]
		set setup_required_time [expr $min_latency*$period*2 + $tco_wr_address_min + $min_rd_address_to_rd_data]
		set setup_slack [expr $setup_required_time - $setup_arrival_time - $board_skew]

		################
		# Hold analysis
		set hold_arrival_time  [expr ($min_DQS_to_fifo - $max_DQS_to_fifo) + $tco_fifo_min + $min_fifo_to_rd_clk_domain + $fifo_depth*$period*2]
		set hold_required_time [expr $hf_DQS_variation + $max_rd_address_to_rd_data + $tco_wr_address_max + $max_latency*$period*2  + $fly_by_wire]	
		set hold_slack [expr -$hold_required_time + $hold_arrival_time - $board_skew]

		if {$setup_slack < $total_setup_slack} {
			set total_setup_slack $setup_slack
		}
		
		if {$hold_slack < $total_hold_slack} {
			set total_hold_slack $hold_slack
		}				
	}
	
	lappend summary [list $opcname 0 "Read Resync ($opcname)" $total_setup_slack $total_hold_slack]
}

