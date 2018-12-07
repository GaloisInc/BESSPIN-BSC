// megafunction wizard: %DDR2 SDRAM Controller with UniPHY v11.0%
// GENERATION: XML
// ddr2_v11_0.v

// 

`timescale 1 ps / 1 ps
module ddr2_v11_0 (
		input  wire         pll_ref_clk,       //     pll_ref_clk.clk
		input  wire         global_reset_n,    //    global_reset.reset_n
		input  wire         soft_reset_n,      //      soft_reset.reset_n
		output wire         afi_clk,           //         afi_clk.clk
		output wire         afi_half_clk,      //    afi_half_clk.clk
		output wire         afi_reset_n,       //       afi_reset.reset_n
		output wire [15:0]  mem_a,             //          memory.mem_a
		output wire         mem_cas_n,         //                .mem_cas_n
		output wire [7:0]   mem_dm,            //                .mem_dm
		inout  wire [7:0]   mem_dqs,           //                .mem_dqs
		output wire [1:0]   mem_ck_n,          //                .mem_ck_n
		inout  wire [63:0]  mem_dq,            //                .mem_dq
		output wire         mem_we_n,          //                .mem_we_n
		output wire [1:0]   mem_cs_n,          //                .mem_cs_n
		output wire         mem_ras_n,         //                .mem_ras_n
		output wire [1:0]   mem_odt,           //                .mem_odt
		output wire [2:0]   mem_ba,            //                .mem_ba
		output wire [1:0]   mem_ck,            //                .mem_ck
		inout  wire [7:0]   mem_dqs_n,         //                .mem_dqs_n
		output wire [1:0]   mem_cke,           //                .mem_cke
		output wire         avl_ready,         //             avl.waitrequest_n
		input  wire         avl_burstbegin,    //                .beginbursttransfer
		input  wire [29:0]  avl_addr,          //                .address
		output wire         avl_rdata_valid,   //                .readdatavalid
		output wire [255:0] avl_rdata,         //                .readdata
		input  wire [255:0] avl_wdata,         //                .writedata
		input  wire [31:0]  avl_be,            //                .byteenable
		input  wire         avl_read_req,      //                .read
		input  wire         avl_write_req,     //                .write
		input  wire [2:0]   avl_size,          //                .burstcount
		output wire         local_init_done,   //          status.local_init_done
		output wire         local_cal_fail,    //                .local_cal_fail
		output wire         local_cal_success, //                .local_cal_success
		input  wire         oct_rup,           //             oct.rup
		input  wire         oct_rdn,           //                .rdn
		output wire         local_powerdn_ack, // local_powerdown.local_powerdn_ack
		input  wire         local_powerdn_req, //                .local_powerdn_req
		output wire         csr_waitrequest,   //             csr.waitrequest
		output wire [31:0]  csr_readdata,      //                .readdata
		output wire         csr_readdatavalid, //                .readdatavalid
		input  wire         csr_burstcount,    //                .burstcount
		input  wire [31:0]  csr_writedata,     //                .writedata
		input  wire [18:0]  csr_address,       //                .address
		input  wire         csr_write,         //                .write
		input  wire         csr_read,          //                .read
		input  wire [3:0]   csr_byteenable,    //                .byteenable
		input  wire         csr_debugaccess    //                .debugaccess
	);

	ddr2_v11_0_0002 ddr2_v11_0_inst (
		.pll_ref_clk       (pll_ref_clk),       //     pll_ref_clk.clk
		.global_reset_n    (global_reset_n),    //    global_reset.reset_n
		.soft_reset_n      (soft_reset_n),      //      soft_reset.reset_n
		.afi_clk           (afi_clk),           //         afi_clk.clk
		.afi_half_clk      (afi_half_clk),      //    afi_half_clk.clk
		.afi_reset_n       (afi_reset_n),       //       afi_reset.reset_n
		.mem_a             (mem_a),             //          memory.mem_a
		.mem_cas_n         (mem_cas_n),         //                .mem_cas_n
		.mem_dm            (mem_dm),            //                .mem_dm
		.mem_dqs           (mem_dqs),           //                .mem_dqs
		.mem_ck_n          (mem_ck_n),          //                .mem_ck_n
		.mem_dq            (mem_dq),            //                .mem_dq
		.mem_we_n          (mem_we_n),          //                .mem_we_n
		.mem_cs_n          (mem_cs_n),          //                .mem_cs_n
		.mem_ras_n         (mem_ras_n),         //                .mem_ras_n
		.mem_odt           (mem_odt),           //                .mem_odt
		.mem_ba            (mem_ba),            //                .mem_ba
		.mem_ck            (mem_ck),            //                .mem_ck
		.mem_dqs_n         (mem_dqs_n),         //                .mem_dqs_n
		.mem_cke           (mem_cke),           //                .mem_cke
		.avl_ready         (avl_ready),         //             avl.waitrequest_n
		.avl_burstbegin    (avl_burstbegin),    //                .beginbursttransfer
		.avl_addr          (avl_addr),          //                .address
		.avl_rdata_valid   (avl_rdata_valid),   //                .readdatavalid
		.avl_rdata         (avl_rdata),         //                .readdata
		.avl_wdata         (avl_wdata),         //                .writedata
		.avl_be            (avl_be),            //                .byteenable
		.avl_read_req      (avl_read_req),      //                .read
		.avl_write_req     (avl_write_req),     //                .write
		.avl_size          (avl_size),          //                .burstcount
		.local_init_done   (local_init_done),   //          status.local_init_done
		.local_cal_fail    (local_cal_fail),    //                .local_cal_fail
		.local_cal_success (local_cal_success), //                .local_cal_success
		.oct_rup           (oct_rup),           //             oct.rup
		.oct_rdn           (oct_rdn),           //                .rdn
		.local_powerdn_ack (local_powerdn_ack), // local_powerdown.local_powerdn_ack
		.local_powerdn_req (local_powerdn_req), //                .local_powerdn_req
		.csr_waitrequest   (csr_waitrequest),   //             csr.waitrequest
		.csr_readdata      (csr_readdata),      //                .readdata
		.csr_readdatavalid (csr_readdatavalid), //                .readdatavalid
		.csr_burstcount    (csr_burstcount),    //                .burstcount
		.csr_writedata     (csr_writedata),     //                .writedata
		.csr_address       (csr_address),       //                .address
		.csr_write         (csr_write),         //                .write
		.csr_read          (csr_read),          //                .read
		.csr_byteenable    (csr_byteenable),    //                .byteenable
		.csr_debugaccess   (csr_debugaccess)    //                .debugaccess
	);

endmodule
// Retrieval info: <?xml version="1.0"?>
//<!--
//	Generated by Altera MegaWizard Launcher Utility version 1.0
//	************************************************************
//	THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
//	************************************************************
//	Copyright (C) 1991-2011 Altera Corporation
//	Any megafunction design, and related net list (encrypted or decrypted),
//	support information, device programming or simulation file, and any other
//	associated documentation or information provided by Altera or a partner
//	under Altera's Megafunction Partnership Program may be used only to
//	program PLD devices (but not masked PLD devices) from Altera.  Any other
//	use of such megafunction design, net list, support information, device
//	programming or simulation file, or any other related documentation or
//	information is prohibited for any other purpose, including, but not
//	limited to modification, reverse engineering, de-compiling, or use with
//	any other silicon devices, unless such use is explicitly licensed under
//	a separate agreement with Altera or a megafunction partner.  Title to
//	the intellectual property, including patents, copyrights, trademarks,
//	trade secrets, or maskworks, embodied in any such megafunction design,
//	net list, support information, device programming or simulation file, or
//	any other related documentation or information provided by Altera or a
//	megafunction partner, remains with Altera, the megafunction partner, or
//	their respective licensors.  No other licenses, including any licenses
//	needed under any third party's intellectual property, are provided herein.
//-->
// Retrieval info: <instance entity-name="altera_mem_if_ddr2_emif" version="11.0" >
// Retrieval info: 	<generic name="MEM_VENDOR" value="JEDEC" />
// Retrieval info: 	<generic name="MEM_FORMAT" value="UNBUFFERED" />
// Retrieval info: 	<generic name="AC_PARITY" value="false" />
// Retrieval info: 	<generic name="DISCRETE_FLY_BY" value="true" />
// Retrieval info: 	<generic name="DEVICE_DEPTH" value="1" />
// Retrieval info: 	<generic name="MEM_MIRROR_ADDRESSING" value="0" />
// Retrieval info: 	<generic name="MEM_CLK_FREQ_MAX" value="400.0" />
// Retrieval info: 	<generic name="MEM_ROW_ADDR_WIDTH" value="16" />
// Retrieval info: 	<generic name="MEM_COL_ADDR_WIDTH" value="12" />
// Retrieval info: 	<generic name="MEM_DQ_WIDTH" value="64" />
// Retrieval info: 	<generic name="MEM_DQ_PER_DQS" value="8" />
// Retrieval info: 	<generic name="MEM_BANKADDR_WIDTH" value="3" />
// Retrieval info: 	<generic name="MEM_IF_DM_PINS_EN" value="true" />
// Retrieval info: 	<generic name="MEM_IF_DQSN_EN" value="true" />
// Retrieval info: 	<generic name="MEM_NUMBER_OF_DIMMS" value="1" />
// Retrieval info: 	<generic name="MEM_NUMBER_OF_RANKS_PER_DIMM" value="2" />
// Retrieval info: 	<generic name="MEM_NUMBER_OF_RANKS_PER_DEVICE" value="1" />
// Retrieval info: 	<generic name="MEM_CK_WIDTH" value="2" />
// Retrieval info: 	<generic name="ALTMEMPHY_COMPATIBLE_MODE" value="false" />
// Retrieval info: 	<generic name="NEXTGEN" value="true" />
// Retrieval info: 	<generic name="MEM_IF_BOARD_BASE_DELAY" value="10" />
// Retrieval info: 	<generic name="MEM_BL" value="8" />
// Retrieval info: 	<generic name="MEM_BT" value="Sequential" />
// Retrieval info: 	<generic name="MEM_ASR" value="Manual" />
// Retrieval info: 	<generic name="MEM_SRT" value="2x refresh rate" />
// Retrieval info: 	<generic name="MEM_PD" value="Fast exit" />
// Retrieval info: 	<generic name="MEM_DRV_STR" value="Full" />
// Retrieval info: 	<generic name="MEM_DLL_EN" value="true" />
// Retrieval info: 	<generic name="MEM_RTT_NOM" value="Disabled" />
// Retrieval info: 	<generic name="MEM_ATCL" value="0" />
// Retrieval info: 	<generic name="MEM_TCL" value="5" />
// Retrieval info: 	<generic name="MEM_AUTO_LEVELING_MODE" value="true" />
// Retrieval info: 	<generic name="MEM_USER_LEVELING_MODE" value="Leveling" />
// Retrieval info: 	<generic name="MEM_INIT_EN" value="false" />
// Retrieval info: 	<generic name="MEM_INIT_FILE" value="" />
// Retrieval info: 	<generic name="DAT_DATA_WIDTH" value="32" />
// Retrieval info: 	<generic name="TIMING_TIS" value="250" />
// Retrieval info: 	<generic name="TIMING_TIH" value="375" />
// Retrieval info: 	<generic name="TIMING_TDS" value="100" />
// Retrieval info: 	<generic name="TIMING_TDH" value="225" />
// Retrieval info: 	<generic name="TIMING_TDQSQ" value="300" />
// Retrieval info: 	<generic name="TIMING_TQHS" value="400" />
// Retrieval info: 	<generic name="TIMING_TDQSCK" value="450" />
// Retrieval info: 	<generic name="TIMING_TDQSS" value="0.25" />
// Retrieval info: 	<generic name="TIMING_TDQSH" value="0.35" />
// Retrieval info: 	<generic name="TIMING_TDSH" value="0.2" />
// Retrieval info: 	<generic name="TIMING_TDSS" value="0.2" />
// Retrieval info: 	<generic name="MEM_TINIT_US" value="200" />
// Retrieval info: 	<generic name="MEM_TMRD_CK" value="2" />
// Retrieval info: 	<generic name="MEM_TRAS_NS" value="45.0" />
// Retrieval info: 	<generic name="MEM_TRCD_NS" value="15.0" />
// Retrieval info: 	<generic name="MEM_TRP_NS" value="15.0" />
// Retrieval info: 	<generic name="MEM_TREFI_US" value="7.8" />
// Retrieval info: 	<generic name="MEM_TRFC_NS" value="105.0" />
// Retrieval info: 	<generic name="MEM_TWR_NS" value="15.0" />
// Retrieval info: 	<generic name="MEM_TWTR" value="2" />
// Retrieval info: 	<generic name="MEM_TFAW_NS" value="50.0" />
// Retrieval info: 	<generic name="MEM_TRRD_NS" value="10.0" />
// Retrieval info: 	<generic name="MEM_TRTP_NS" value="10.0" />
// Retrieval info: 	<generic name="CSR_ADDR_WIDTH" value="16" />
// Retrieval info: 	<generic name="CSR_DATA_WIDTH" value="32" />
// Retrieval info: 	<generic name="POWER_OF_TWO_BUS" value="false" />
// Retrieval info: 	<generic name="SOPC_COMPAT_RESET" value="false" />
// Retrieval info: 	<generic name="AVL_MAX_SIZE" value="4" />
// Retrieval info: 	<generic name="BYTE_ENABLE" value="true" />
// Retrieval info: 	<generic name="ENABLE_CTRL_AVALON_INTERFACE" value="true" />
// Retrieval info: 	<generic name="CTL_SELF_REFRESH_EN" value="false" />
// Retrieval info: 	<generic name="AUTO_POWERDN_EN" value="false" />
// Retrieval info: 	<generic name="MEM_AUTO_PD_CYCLES" value="0" />
// Retrieval info: 	<generic name="CTL_USR_REFRESH_EN" value="false" />
// Retrieval info: 	<generic name="CTL_AUTOPCH_EN" value="false" />
// Retrieval info: 	<generic name="ADDR_ORDER" value="1" />
// Retrieval info: 	<generic name="CTL_LOOK_AHEAD_DEPTH" value="4" />
// Retrieval info: 	<generic name="CONTROLLER_LATENCY" value="5" />
// Retrieval info: 	<generic name="CFG_REORDER_DATA" value="true" />
// Retrieval info: 	<generic name="CFG_STARVE_LIMIT" value="10" />
// Retrieval info: 	<generic name="CTL_CSR_ENABLED" value="true" />
// Retrieval info: 	<generic name="CTL_CSR_CONNECTION" value="EXPORT" />
// Retrieval info: 	<generic name="CTL_ECC_ENABLED" value="false" />
// Retrieval info: 	<generic name="CTL_HRB_ENABLED" value="false" />
// Retrieval info: 	<generic name="CTL_ECC_AUTO_CORRECTION_ENABLED" value="false" />
// Retrieval info: 	<generic name="MULTICAST_EN" value="false" />
// Retrieval info: 	<generic name="CTL_DYNAMIC_BANK_ALLOCATION" value="false" />
// Retrieval info: 	<generic name="CTL_DYNAMIC_BANK_NUM" value="4" />
// Retrieval info: 	<generic name="DEBUG_MODE" value="false" />
// Retrieval info: 	<generic name="ENABLE_BURST_MERGE" value="false" />
// Retrieval info: 	<generic name="LOCAL_ID_WIDTH" value="8" />
// Retrieval info: 	<generic name="RDBUFFER_ADDR_WIDTH" value="6" />
// Retrieval info: 	<generic name="WRBUFFER_ADDR_WIDTH" value="6" />
// Retrieval info: 	<generic name="USE_MM_ADAPTOR" value="true" />
// Retrieval info: 	<generic name="USE_AXI_ADAPTOR" value="false" />
// Retrieval info: 	<generic name="HCX_COMPAT_MODE" value="false" />
// Retrieval info: 	<generic name="CTL_CMD_QUEUE_DEPTH" value="8" />
// Retrieval info: 	<generic name="CTL_CSR_READ_ONLY" value="1" />
// Retrieval info: 	<generic name="CFG_DATA_REORDERING_TYPE" value="INTER_BANK" />
// Retrieval info: 	<generic name="REF_CLK_FREQ" value="100.0" />
// Retrieval info: 	<generic name="REF_CLK_FREQ_PARAM_VALID" value="false" />
// Retrieval info: 	<generic name="REF_CLK_FREQ_MIN_PARAM" value="0.0" />
// Retrieval info: 	<generic name="REF_CLK_FREQ_MAX_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_DR_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_DR_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_DR_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_DR_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_DR_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_MEM_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_MEM_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_MEM_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_MEM_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_MEM_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_AFI_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_AFI_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_WRITE_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_WRITE_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_WRITE_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_WRITE_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_WRITE_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_ADDR_CMD_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_ADDR_CMD_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_ADDR_CMD_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_ADDR_CMD_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_ADDR_CMD_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_HALF_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_AFI_HALF_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_AFI_HALF_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_HALF_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_AFI_HALF_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_NIOS_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_NIOS_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_NIOS_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_NIOS_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_NIOS_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_CONFIG_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_CONFIG_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_CONFIG_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_CONFIG_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_CONFIG_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_P2C_READ_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_P2C_READ_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_P2C_READ_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_P2C_READ_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_P2C_READ_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_C2P_WRITE_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_C2P_WRITE_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_C2P_WRITE_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_C2P_WRITE_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_C2P_WRITE_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_HR_CLK_FREQ_PARAM" value="0.0" />
// Retrieval info: 	<generic name="PLL_HR_CLK_FREQ_SIM_STR_PARAM" value="" />
// Retrieval info: 	<generic name="PLL_HR_CLK_PHASE_PS_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_HR_CLK_MULT_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_HR_CLK_DIV_PARAM" value="0" />
// Retrieval info: 	<generic name="PLL_CLK_PARAM_VALID" value="false" />
// Retrieval info: 	<generic name="ENABLE_EXTRA_REPORTING" value="false" />
// Retrieval info: 	<generic name="NUM_EXTRA_REPORT_PATH" value="10" />
// Retrieval info: 	<generic name="ENABLE_ISS_PROBES" value="false" />
// Retrieval info: 	<generic name="READ_VALID_FIFO_SIZE" value="16" />
// Retrieval info: 	<generic name="CALIB_REG_WIDTH" value="8" />
// Retrieval info: 	<generic name="MAX_LATENCY_COUNT_WIDTH" value="5" />
// Retrieval info: 	<generic name="DLL_DELAY_CHAIN_LENGTH" value="10" />
// Retrieval info: 	<generic name="USE_SEQUENCER_BFM" value="false" />
// Retrieval info: 	<generic name="DEFAULT_FAST_SIM_MODEL" value="true" />
// Retrieval info: 	<generic name="PLL_SHARING_MODE" value="None" />
// Retrieval info: 	<generic name="DLL_SHARING_MODE" value="None" />
// Retrieval info: 	<generic name="OCT_SHARING_MODE" value="None" />
// Retrieval info: 	<generic name="ABSTRACT_REAL_COMPARE_TEST" value="false" />
// Retrieval info: 	<generic name="INCLUDE_BOARD_DELAY_MODEL" value="false" />
// Retrieval info: 	<generic name="EXTRA_SETTINGS" value="" />
// Retrieval info: 	<generic name="MEM_DEVICE" value="MISSING_MODEL" />
// Retrieval info: 	<generic name="FORCE_SYNTHESIS_LANGUAGE" value="" />
// Retrieval info: 	<generic name="QSYS_SEQUENCER_DEBUG" value="true" />
// Retrieval info: 	<generic name="SEQUENCER_TYPE" value="NIOS" />
// Retrieval info: 	<generic name="ADVERTIZE_SEQUENCER_SW_BUILD_FILES" value="false" />
// Retrieval info: 	<generic name="SEQ_MODE" value="0" />
// Retrieval info: 	<generic name="ADVANCED_CK_PHASES" value="false" />
// Retrieval info: 	<generic name="COMMAND_PHASE" value="0.0" />
// Retrieval info: 	<generic name="MEM_CK_PHASE" value="0.0" />
// Retrieval info: 	<generic name="PLL_LOCATION" value="Top_Bottom" />
// Retrieval info: 	<generic name="SKIP_MEM_INIT" value="true" />
// Retrieval info: 	<generic name="READ_DQ_DQS_CLOCK_SOURCE" value="INVERTED_DQS_BUS" />
// Retrieval info: 	<generic name="DQ_INPUT_REG_USE_CLKN" value="false" />
// Retrieval info: 	<generic name="DQS_DQSN_MODE" value="DIFFERENTIAL" />
// Retrieval info: 	<generic name="AFI_DEBUG_INFO_WIDTH" value="32" />
// Retrieval info: 	<generic name="CALIBRATION_MODE" value="Skip" />
// Retrieval info: 	<generic name="NIOS_ROM_DATA_WIDTH" value="32" />
// Retrieval info: 	<generic name="NIOS_ROM_ADDRESS_WIDTH" value="12" />
// Retrieval info: 	<generic name="DQS_TRK_ENABLED" value="false" />
// Retrieval info: 	<generic name="READ_FIFO_SIZE" value="8" />
// Retrieval info: 	<generic name="PHY_CSR_ENABLED" value="false" />
// Retrieval info: 	<generic name="PHY_CSR_CONNECTION" value="INTERNAL_JTAG" />
// Retrieval info: 	<generic name="TIMING_BOARD_DERATE_METHOD" value="AUTO" />
// Retrieval info: 	<generic name="TIMING_BOARD_CK_CKN_SLEW_RATE" value="2.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_SLEW_RATE" value="1.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_DQS_DQSN_SLEW_RATE" value="2.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_DQ_SLEW_RATE" value="1.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_TIS" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_TIH" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_TDS" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_TDH" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_ISI_METHOD" value="AUTO" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_EYE_REDUCTION_SU" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_EYE_REDUCTION_H" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_DQ_EYE_REDUCTION" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_DELTA_DQS_ARRIVAL_TIME" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_MAX_CK_DELAY" value="0.6" />
// Retrieval info: 	<generic name="TIMING_BOARD_MAX_DQS_DELAY" value="0.6" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_CKDQS_DIMM_MIN" value="-0.01" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_CKDQS_DIMM_MAX" value="0.01" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_BETWEEN_DIMMS" value="0.05" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_WITHIN_DQS" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_BETWEEN_DQS" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_DQ_TO_DQS_SKEW" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_SKEW" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_TO_CK_SKEW" value="0.6" />
// Retrieval info: 	<generic name="RATE" value="Half" />
// Retrieval info: 	<generic name="MEM_CLK_FREQ" value="250.0" />
// Retrieval info: 	<generic name="SYS_INFO_DEVICE_FAMILY" value="Stratix IV" />
// Retrieval info: 	<generic name="PARSE_FRIENDLY_DEVICE_FAMILY_PARAM_VALID" value="false" />
// Retrieval info: 	<generic name="PARSE_FRIENDLY_DEVICE_FAMILY_PARAM" value="" />
// Retrieval info: 	<generic name="DEVICE_FAMILY_PARAM" value="" />
// Retrieval info: 	<generic name="DISABLE_CHILD_MESSAGING" value="false" />
// Retrieval info: 	<generic name="SPEED_GRADE" value="4" />
// Retrieval info: 	<generic name="ADD_EFFICIENCY_MONITOR" value="false" />
// Retrieval info: 	<generic name="ENABLE_ABS_RAM_MEM_INIT" value="false" />
// Retrieval info: 	<generic name="ABS_RAM_MEM_INIT_FILENAME" value="meminit" />
// Retrieval info: </instance>
// IPFS_FILES : NONE
