######################################################################################################
##  File name :       default_ooc.xdc
##
##  Details :     Constraints file
##                    FPGA family:       virtex ultrascale
##                    FPGA:              xcvu440-flga2892-1-c
##
######################################################################################################

######################################################################################################
# I/O STANDARDS
######################################################################################################
set_property IOSTANDARD LVCMOS18    [get_ports { RST_N_pci_sys_reset }]
set_property IOSTANDARD LVCMOS18    [get_ports { leds[*] }]
set_property IOSTANDARD LVCMOS18    [get_ports { PCIE_CPRSNT PCIE_CWAKE }]


######################################################################################################
# CELL LOCATIONS
######################################################################################################

#
# Transceiver instance placement.  This constraint selects the
# transceivers to be used, which also dictates the pinout for the
# transmit and receive differential pairs.  Please refer to the
# Virtex-7 GT Transceiver User Guide (UG) for more information.
#

#set_property LOC BUFG_GT_X0Y36 [get_cells -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_pclk}]
#set_property LOC BUFG_GT_X0Y37 [get_cells -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_userclk}]
#set_property LOC BUFG_GT_X0Y38 [get_cells -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_coreclk}]

#
# PCI Express Block placement. This constraint selects the PCI Express
# Block to be used.
#

set_property LOC PCIE_3_1_X0Y0 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/PCIE_3_1_inst}]

#
# Buffer (BRAM) Placement Constraints
#

#Request Buffer RAMB Placement

set_property LOC RAMB18_X12Y2 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_req_inst/bram_req_8k_inst/RAMB18E2[0].ramb18e2_inst}]
set_property LOC RAMB18_X12Y3 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_req_inst/bram_req_8k_inst/RAMB18E2[1].ramb18e2_inst}]
set_property LOC RAMB18_X12Y4 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_req_inst/bram_req_8k_inst/RAMB18E2[2].ramb18e2_inst}]
set_property LOC RAMB18_X12Y5 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_req_inst/bram_req_8k_inst/RAMB18E2[3].ramb18e2_inst}]

# Completion Buffer RAMB Placement

# Extreme - 4
set_property LOC RAMB18_X12Y8 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_cpl_inst/CPL_FIFO_16KB.bram_16k_inst/RAMB18E2[0].ramb18e2_inst}]
set_property LOC RAMB18_X12Y9 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_cpl_inst/CPL_FIFO_16KB.bram_16k_inst/RAMB18E2[1].ramb18e2_inst}]
set_property LOC RAMB18_X12Y10 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_cpl_inst/CPL_FIFO_16KB.bram_16k_inst/RAMB18E2[2].ramb18e2_inst}]
set_property LOC RAMB18_X12Y11 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_cpl_inst/CPL_FIFO_16KB.bram_16k_inst/RAMB18E2[3].ramb18e2_inst}]

# Replay Buffer RAMB Placement
set_property LOC RAMB36_X12Y9 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_rep_inst/bram_rep_8k_inst/RAMB36E2[0].ramb36e2_inst}]
set_property LOC RAMB36_X12Y10 [get_cells -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/pcie3_uscale_wrapper_inst/bram_inst/bram_rep_inst/bram_rep_8k_inst/RAMB36E2[1].ramb36e2_inst}]

######################################################################################################
# AREA GROUPS
######################################################################################################

startgroup

create_pblock pblock_pcie0
set_property CONTAIN_ROUTING true [get_pblocks pblock_pcie0]
resize_pblock pblock_pcie0 -add {SLICE_X253Y0:SLICE_X358Y59 DSP48E2_X6Y0:DSP48E2_X7Y23 RAMB18_X9Y0:RAMB18_X13Y22 RAMB36_X9Y0:RAMB36_X13Y22}

# The imported PCIE wrapper
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *pcie_ep*}]

# The Axi packet buffers
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *fAxiCc_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *fAxiRq_*}]

# The mkConnection buffers (Axi-to-TLP) from the endpoint to the bridge
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *cq_f_cq_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *cq_gearbox_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *cc_f_tlps_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *cc_gearbox_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *rq_f_tlps_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *rq_gearbox_*}]
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *rc_gearbox_*}]

# The bridge (mkPCIEtoBNoC)
add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~ *pbb*}]

# SceMiNoC MIMOs for outgoing messages (to the bridge)
if { [llength [get_cells -hier -filter {NAME =~  *fS1OutPort*}]] > 0 } {
    add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *fS1OutPort*}]
}
if { [llength [get_cells -hier -filter {NAME =~  *fS1MsgOut*}]] > 0 } {
    add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *fS1MsgOut*}]
}
# this one will only exist if SceMi pipes are used
if { [llength [get_cells -hier -filter {NAME =~  *fS2MsgOut*}]] > 0 } {
    add_cells_to_pblock pblock_pcie0 [get_cells -hier -filter {NAME =~  *fS2MsgOut*}]
}

endgroup

######################################################################################################
# TIMING CONSTRAINTS
######################################################################################################

# Bluespec constraints
#

# clocks
create_clock -name pci_refclk -period 10 [get_ports CLK_pci_sys_clk_p]
# CDN MK added
set_clock_groups -name async_with_sample_clk_1 -asynchronous -group {pci_refclk} -group {CVA_SAMPLE_CLK}
set_clock_groups -name async_with_fast_clk_1 -asynchronous -group {pci_refclk} -group {CVA_FAST_CLK}
set_clock_groups -name async_with_step_clk_1 -asynchronous -group {pci_refclk} -group {CVA_STEP_CLK}

create_clock -name noc_clk -period 8 [get_pins -hier -filter { NAME =~ *clkgen_pll/CLKOUT0 }]
# CDN MK added
set_clock_groups -name async_with_sample_clk_2 -asynchronous -group {noc_clk} -group {CVA_SAMPLE_CLK}
set_clock_groups -name async_with_fast_clk_2 -asynchronous -group {noc_clk} -group {CVA_FAST_CLK}
set_clock_groups -name async_with_step_clk_2 -asynchronous -group {noc_clk} -group {CVA_STEP_CLK}

# BSPEC JES added
create_clock -name gthe3_clk  -period 4 [get_pins -hier -filter { NAME =~ *GTHE3_CHANNEL_TXOUTCLK[0] }]
#
set_clock_groups -name async_GTHE3_fast_clock_virt -asynchronous -group {gthe3_clk} -group {CVA_FAST_CLK_VIRT}
set_clock_groups -name async_GTHE3_sample_clock -asynchronous -group {gthe3_clk} -group {CVA_SAMPLE_CLK}
set_clock_groups -name async_GTHE3_noc_clock -asynchronous -group {gthe3_clk} -group {noc_clk}

# False Paths
set_false_path -from [get_ports { RST_N_pci_sys_reset }]
set_false_path -to   [get_ports { leds[?] }]

#
# Xilinx IP constraints
#

# TXOUTCLKSEL switches during reset. Set the tool to analyze timing with TXOUTCLKSEL set to 'b101.
set_case_analysis 1 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/PHY_TXOUTCLKSEL[2]}]
set_case_analysis 0 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/PHY_TXOUTCLKSEL[1]}]
set_case_analysis 1 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/PHY_TXOUTCLKSEL[0]}]

# These are in the XDC for the 2016.4 IP, but not in the XDC for 2015.3/4
#set_case_analysis 0 [get_pins -hier -filter {NAME =~ *gen_channel_container[*].*gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/TXRATE[0]}]
#set_case_analysis 0 [get_pins -hier -filter {NAME =~ *gen_channel_container[*].*gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/RXRATE[0]}]
#set_case_analysis 0 [get_pins -hier -filter {NAME =~ *gen_channel_container[*].*gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/TXRATE[1]}]
#set_case_analysis 0 [get_pins -hier -filter {NAME =~ *gen_channel_container[*].*gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/RXRATE[1]}]

# Set Divide By 1
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_userclk/DIV[0]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_userclk/DIV[1]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_userclk/DIV[2]}]
# Set Divide By 2
set_case_analysis 1 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_pclk/DIV[0]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_pclk/DIV[1]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_pclk/DIV[2]}]

# Set Divide By 2
set_case_analysis 1 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/bufg_mcap_clk/DIV[0]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/bufg_mcap_clk/DIV[1]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/bufg_mcap_clk/DIV[2]}]
# Set Divide By 1
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_coreclk/DIV[0]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_coreclk/DIV[1]}]
set_case_analysis 0 [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_coreclk/DIV[2]}]
#

# CDC Registers

# This path is crossing clock domains between pipe_clk and sys_clk
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/prst_n_r_reg[7]/C}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/sync_prst_n/sync_vec[0].sync_cell_i/sync_reg[0]/D}]

# These paths are crossing clock domains between sys_clk and user_clk

# This is the version from the XDC for the 2016.4 IP
#set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/idle_reg/C}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/init_ctrl_inst/reg_phy_rdy_reg[0]/D}]
#
# This is the version from the XDC for the 2015.3/4 IP
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/idle_reg/C}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/init_ctrl_inst/reg_phy_rdy_reg[*]/PRE}]
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/idle_reg/C}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/pcie3_uscale_top_inst/init_ctrl_inst/reg_reset_timer_reg[*]/CLR}]

# This is the 2015.3/4 version
# The 2016.4 is the same but with v4_2 instead of v4_1
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/gt_wizard.gtwizard_top_i/pcie3_ultrascale_v4_1_gt_i/inst/gen_gtwizard_gthe3_top.pcie3_ultrascale_v4_1_gt_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[*].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/RXUSRCLK2}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/sync_phystatus/sync_vec[*].sync_cell_i/sync_reg[0]/D}]
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/gt_wizard.gtwizard_top_i/pcie3_ultrascale_v4_1_gt_i/inst/gen_gtwizard_gthe3_top.pcie3_ultrascale_v4_1_gt_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[*].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/RXUSRCLK2}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/sync_rxresetdone/sync_vec[*].sync_cell_i/sync_reg[0]/D}]
set_false_path -from [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/gt_wizard.gtwizard_top_i/pcie3_ultrascale_v4_1_gt_i/inst/gen_gtwizard_gthe3_top.pcie3_ultrascale_v4_1_gt_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[*].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[*].GTHE3_CHANNEL_PRIM_INST/TXUSRCLK2}] -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_rst_i/sync_txresetdone/sync_vec[*].sync_cell_i/sync_reg[0]/D}]

# Asynchronous Pins

# Async reset registers
# These are in the XDC for the 2016.4 IP, but not in the XDC for 2015.3/4
#set_false_path -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/user_lnk_up_reg/CLR}]
#set_false_path -to [get_pins -hier -filter {NAME =~ */pcie_ep/*/user_reset_reg/PRE}]

# These pins are not associated with any clock domain
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/RXELECIDLE}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/PCIEPERST0B}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/PCIERATEGEN3}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/RXPRGDIVRESETDONE}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/TXPRGDIVRESETDONE}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/PCIESYNCTXSYNCDONE}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/GTPOWERGOOD}]
set_false_path -through [get_pins -hier -filter {NAME=~*/pcie_ep/*/CPLLLOCK}]

## Set the clock root on the PCIe clocks to limit skew to the PCIe Hardblock pins.
#
# This is the version from the XDC for the 2016.4 IP (which had it commented out)
##set_property USER_CLOCK_ROOT X7Y0 [get_nets -of_objects [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_pclk/O}]]
##set_property USER_CLOCK_ROOT X7Y0 [get_nets -of_objects [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_userclk/O}]]
##set_property USER_CLOCK_ROOT X7Y0 [get_nets -of_objects [get_pins -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/bufg_gt_coreclk/O}]]
#
# This is the version from the XDC for the 2015.3/4 IP
# However, USER_CLOCK_ROOT must be changed to CLOCK_ROOT for 2015.2
set_property USER_CLOCK_ROOT X7Y0 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/CLK_PCLK}]
set_property USER_CLOCK_ROOT X7Y0 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/CLK_USERCLK}]
set_property USER_CLOCK_ROOT X7Y0 [get_nets -hier -filter {NAME =~ */pcie_ep/*/gt_top_i/phy_clk_i/CLK_CORECLK}]
#
