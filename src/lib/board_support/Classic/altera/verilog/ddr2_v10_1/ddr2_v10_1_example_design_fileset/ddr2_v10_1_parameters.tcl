#
# AUTO-GENERATED FILE: Do not edit ! ! ! 
#

set ::GLOBAL_ddr2_v10_1_corename "ddr2_v10_1"
set ::GLOBAL_ddr2_v10_1_io_standard "SSTL-18"
set ::GLOBAL_ddr2_v10_1_io_interface_type "HPAD"
set ::GLOBAL_ddr2_v10_1_io_standard_differential "1.8-V SSTL"
set ::GLOBAL_ddr2_v10_1_number_of_dqs_groups 8
set ::GLOBAL_ddr2_v10_1_dqs_group_size 8
set ::GLOBAL_ddr2_v10_1_number_of_ck_pins 2
set ::GLOBAL_ddr2_v10_1_number_of_dm_pins 8
set ::GLOBAL_ddr2_v10_1_dqs_delay_chain_length 3
set ::GLOBAL_ddr2_v10_1_uniphy_temp_ver_code 2090388195
# PLL Parameters

# W A R N I N G !
# The PLL parameters are statically defined in this
# file at generation time!
# To ensure timing constraints and timing reports are correct, when you make 
# any changes to the PLL component using the MegaWizard Plug-In,
# apply those changes to the PLL parameters in this file

set ::GLOBAL_ddr2_v10_1_num_pll_clock 7
set ::GLOBAL_ddr2_v10_1_pll_0_mult 5
set ::GLOBAL_ddr2_v10_1_pll_0_div 4
set ::GLOBAL_ddr2_v10_1_pll_0_phase 0
set ::GLOBAL_ddr2_v10_1_pll_1_mult 5
set ::GLOBAL_ddr2_v10_1_pll_1_div 2
set ::GLOBAL_ddr2_v10_1_pll_1_phase 0.0
set ::GLOBAL_ddr2_v10_1_pll_2_mult 5
set ::GLOBAL_ddr2_v10_1_pll_2_div 2
set ::GLOBAL_ddr2_v10_1_pll_2_phase 90.0
set ::GLOBAL_ddr2_v10_1_pll_3_mult 5
set ::GLOBAL_ddr2_v10_1_pll_3_div 4
set ::GLOBAL_ddr2_v10_1_pll_3_phase 270.0
set ::GLOBAL_ddr2_v10_1_pll_4_mult 5
set ::GLOBAL_ddr2_v10_1_pll_4_div 8
set ::GLOBAL_ddr2_v10_1_pll_4_phase 0
set ::GLOBAL_ddr2_v10_1_pll_5_mult 5
set ::GLOBAL_ddr2_v10_1_pll_5_div 8
set ::GLOBAL_ddr2_v10_1_pll_5_phase 0
set ::GLOBAL_ddr2_v10_1_pll_6_mult 5
set ::GLOBAL_ddr2_v10_1_pll_6_div 24
set ::GLOBAL_ddr2_v10_1_pll_6_phase 0

set ::GLOBAL_ddr2_v10_1_leveling_capture_phase 60.0

##############################################################
## IP options
##############################################################
set IP(write_dcc) "static"
set IP(write_deskew_range) 15
set IP(read_deskew_range) 15
set IP(mem_if_memtype) "ddr2"
set IP(RDIMM) 0
set IP(mp_calibration) 1
set IP(quantization_T9) 0.050
set IP(quantization_T1) 0.050
set IP(quantization_DCC) 0.050
set IP(quantization_T7) 0.050
set IP(quantization_WL) 0.050
# Can be either dynamic or static
set IP(write_deskew_mode) "dynamic"
set IP(read_deskew_mode) "dynamic"
set IP(discrete_device) 1
set IP(num_ranks) 2

set IP(num_report_paths) 20
