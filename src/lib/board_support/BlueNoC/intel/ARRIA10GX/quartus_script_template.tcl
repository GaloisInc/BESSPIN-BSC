# Copyright (C) 2018  Intel Corporation. All rights reserved.
# Your use of Intel Corporation's design tools, logic functions
# and other software and tools, and its AMPP partner logic
# functions, and any output files from any of the foregoing
# (including device programming or simulation files), and any
# associated documentation or information are expressly subject
# to the terms and conditions of the Intel Program License
# Subscription Agreement, the Intel Quartus Prime License Agreement,
# the Intel FPGA IP License Agreement, or other applicable license
# agreement, including, without limitation, that your use is for
# the sole purpose of programming logic devices manufactured by
# Intel and sold by Intel or its authorized distributors.  Please
# refer to the applicable agreement for further details.

# Quartus Prime: Generate Tcl File for Project
# File: standalone.tcl

# Load Quartus Prime Tcl Project package
package require ::quartus::project
package require ::quartus::flow

set need_to_close_project 1
set make_assignments 1

if {[project_exists __TOP_MODULE__]} {
    file delete -force __TOP_MODULE__.qsf
    file delete -force __TOP_MODULE__.qpf
    file delete -force qdb
}

project_new __TOP_MODULE__

# Make assignments
set_global_assignment -name ORIGINAL_QUARTUS_VERSION 18.0.0
set_global_assignment -name PROJECT_CREATION_TIME_DATE "15:33:31  OCTOBER 05, 2018"
set_global_assignment -name LAST_QUARTUS_VERSION "18.0.0 Pro Edition"
set_global_assignment -name SEARCH_PATH "__BLUESPECDIR__/Libraries"
set_global_assignment -name SEARCH_PATH "__BLUESPECDIR__/Verilog"
set_global_assignment -name SEARCH_PATH "__BLUESPECDIR__/board_support/bluenoc/intel/ARRIA10GX/verilog"
set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files
set_global_assignment -name DEVICE 10AX115S2F45I1SG
set_global_assignment -name FAMILY "Arria 10"
set_global_assignment -name ERROR_CHECK_FREQUENCY_DIVISOR 1
set_global_assignment -name MIN_CORE_JUNCTION_TEMP "-40"
set_global_assignment -name MAX_CORE_JUNCTION_TEMP 100
set_global_assignment -name EDA_SIMULATION_TOOL "ModelSim-Altera (Verilog)"
set_global_assignment -name EDA_OUTPUT_DATA_FORMAT "VERILOG HDL" -section_id eda_simulation
set_global_assignment -name POWER_AUTO_COMPUTE_TJ ON
set_global_assignment -name POWER_PRESET_COOLING_SOLUTION "23 MM HEAT SINK WITH 200 LFPM AIRFLOW"
set_global_assignment -name POWER_BOARD_THERMAL_MODEL "NONE (CONSERVATIVE)"
set_global_assignment -name ENABLE_SIGNALTAP ON
set_global_assignment -name IP_FILE "__BLUESPECDIR__/board_support/bluenoc/intel/ARRIA10GX/verilog/p1b.ip"
set_global_assignment -name IP_FILE "__BLUESPECDIR__/board_support/bluenoc/intel/ARRIA10GX/verilog/pll2.ip"
set_global_assignment -name IP_FILE "__BLUESPECDIR__/board_support/bluenoc/intel/ARRIA10GX/verilog/clkbuf.ip"
set_global_assignment -name VERILOG_FILE __VDIR__/__TOP_MODULE__.v
set_location_assignment PIN_AL37 -to CLK_ref_clk
set_instance_assignment -name IO_STANDARD HCSL -to CLK_ref_clk -entity __TOP_MODULE__
set_location_assignment PIN_BC30 -to RST_N_pin_perst
set_instance_assignment -name IO_STANDARD "1.8 V" -to RST_N_pin_perst -entity __TOP_MODULE__
set_location_assignment PIN_BB44 -to PCIE_tx_out0
set_location_assignment PIN_BA42 -to PCIE_tx_out1
set_location_assignment PIN_AY44 -to PCIE_tx_out2
set_location_assignment PIN_AW42 -to PCIE_tx_out3
set_location_assignment PIN_AV44 -to PCIE_tx_out4
set_location_assignment PIN_AU42 -to PCIE_tx_out5
set_location_assignment PIN_AT44 -to PCIE_tx_out6
set_location_assignment PIN_AR42 -to PCIE_tx_out7
set_location_assignment PIN_BB43 -to "PCIE_tx_out0(n)"
set_location_assignment PIN_BA41 -to "PCIE_tx_out1(n)"
set_location_assignment PIN_AY43 -to "PCIE_tx_out2(n)"
set_location_assignment PIN_AW41 -to "PCIE_tx_out3(n)"
set_location_assignment PIN_AV43 -to "PCIE_tx_out4(n)"
set_location_assignment PIN_AU41 -to "PCIE_tx_out5(n)"
set_location_assignment PIN_AT43 -to "PCIE_tx_out6(n)"
set_location_assignment PIN_AR41 -to "PCIE_tx_out7(n)"
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out0 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out1 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out2 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out3 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out4 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out5 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out6 -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "HIGH SPEED DIFFERENTIAL I/O" -to PCIE_tx_out7 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out0 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out1 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out2 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out3 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out4 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out5 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out6 -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_tx_out7 -entity __TOP_MODULE__
set_location_assignment PIN_AT40 -to PCIE_rx_in0_x
set_location_assignment PIN_AP40 -to PCIE_rx_in1_x
set_location_assignment PIN_AN42 -to PCIE_rx_in2_x
set_location_assignment PIN_AM40 -to PCIE_rx_in3_x
set_location_assignment PIN_AL42 -to PCIE_rx_in4_x
set_location_assignment PIN_AK40 -to PCIE_rx_in5_x
set_location_assignment PIN_AJ42 -to PCIE_rx_in6_x
set_location_assignment PIN_AH40 -to PCIE_rx_in7_x
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in0_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in1_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in2_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in3_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in4_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in5_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in6_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_TERM_SEL R_R1 -to PCIE_rx_in7_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in0_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in1_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in2_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in3_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in4_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in5_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in6_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ONE_STAGE_ENABLE NON_S1_MODE -to PCIE_rx_in7_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in0_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in1_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in2_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in3_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in4_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in5_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in6_x -entity __TOP_MODULE__
set_instance_assignment -name IO_STANDARD "CURRENT MODE LOGIC (CML)" -to PCIE_rx_in7_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in0_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in1_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in2_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in3_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in4_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in5_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in6_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_A10_RX_ADP_CTLE_ACGAIN_4S RADP_CTLE_ACGAIN_4S_0 -to PCIE_rx_in7_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in0_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in1_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in2_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in3_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in4_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in5_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in6_x -entity __TOP_MODULE__
set_instance_assignment -name XCVR_VCCR_VCCT_VOLTAGE 1_0V -to PCIE_rx_in7_x -entity __TOP_MODULE__
set_instance_assignment -name PARTITION_COLOUR 4291231571 -to __TOP_MODULE__ -entity __TOP_MODULE__
