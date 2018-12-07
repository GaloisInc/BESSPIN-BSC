// megafunction wizard: %DDR2 SDRAM Controller with UniPHY (New!) v10.1%
// GENERATION: XML
// ddr2_v10_1.v

// 

module ddr2_v10_1 (
		output wire         mem_we_n,                   //        memory_phy.export
		output wire         mem_ras_n,                  //                  .export
		output wire         mem_cas_n,                  //                  .export
		inout  wire [63:0]  mem_dq,                     //                  .export
		output wire [1:0]   mem_cs_n,                   //                  .export
		output wire [1:0]   mem_cke,                    //                  .export
		output wire [1:0]   mem_odt,                    //                  .export
		output wire [1:0]   mem_ck_n,                   //                  .export
		output wire [1:0]   mem_ck,                     //                  .export
		output wire [2:0]   mem_ba,                     //                  .export
		output wire [15:0]  mem_a,                      //                  .export
		inout  wire [7:0]   mem_dqs,                    //                  .export
		output wire [7:0]   mem_dm,                     //                  .export
		inout  wire [7:0]   mem_dqs_n,                  //                  .export
		input  wire         pll_ref_clk,                //        clock_sink.clk
		input  wire         soft_reset_n,               //  clock_sink_reset.reset_n
		output wire         afi_clk,                    //      clock_source.clk
		output wire         afi_half_clk,               // half_clock_source.clk
		input  wire         global_reset_n,             //             Other.export
		output wire         reset_request_n,            //                  .export
		output wire         afi_reset_n,                //                  .export
		output wire         afi_cal_success,            //                  .export
		output wire         afi_cal_fail,               //                  .export
		output wire         local_init_done,            //                  .export
		input  wire         oct_rdn,                    //                  .export
		input  wire         oct_rup,                    //                  .export
		output wire [31:0]  afi_cal_debug_info,         //     afi_cal_debug.export
		output wire         pll_afi_clk,                //       PLL_sharing.export
		output wire         pll_mem_clk,                //                  .export
		output wire         pll_write_clk,              //                  .export
		output wire         pll_addr_cmd_clk,           //                  .export
		output wire         pll_locked,                 //                  .export
		output wire         pll_afi_half_clk,           //                  .export
		output wire         pll_avl_clk,                //                  .export
		output wire         pll_config_clk,             //                  .export
		output wire [5:0]   dll_delayctrl,              //       DLL_sharing.export
		output wire [13:0]  parallelterminationcontrol, //       OCT_sharing.export
		output wire [13:0]  seriesterminationcontrol,   //                  .export
		output wire         avl_ready,                  //    avalon_slave_0.waitrequest_n
		input  wire [29:0]  avl_addr,                   //                  .address
		input  wire [2:0]   avl_size,                   //                  .burstcount
		input  wire         avl_burstbegin,             //                  .beginbursttransfer
		input  wire [31:0]  avl_be,                     //                  .byteenable
		input  wire [255:0] avl_wdata,                  //                  .writedata
		output wire [255:0] avl_rdata,                  //                  .readdata
		input  wire         avl_write_req,              //                  .write
		input  wire         avl_read_req,               //                  .read
		output wire         avl_rdata_valid,            //                  .readdatavalid
                input  wire [15:0]  csr_addr,
                input  wire [3:0]   csr_be,
                input  wire         csr_read_req,
                input  wire [31:0]  csr_wdata,
                input  wire         csr_write_req,
                output wire [31:0]  csr_rdata,
                output wire         csr_rdata_valid,
                output wire         csr_waitrequest
	);

	ddr2_v10_1_0002 ddr2_v10_1_inst (
		.mem_we_n                   (mem_we_n),                   //        memory_phy.export
		.mem_ras_n                  (mem_ras_n),                  //                  .export
		.mem_cas_n                  (mem_cas_n),                  //                  .export
		.mem_dq                     (mem_dq),                     //                  .export
		.mem_cs_n                   (mem_cs_n),                   //                  .export
		.mem_cke                    (mem_cke),                    //                  .export
		.mem_odt                    (mem_odt),                    //                  .export
		.mem_ck_n                   (mem_ck_n),                   //                  .export
		.mem_ck                     (mem_ck),                     //                  .export
		.mem_ba                     (mem_ba),                     //                  .export
		.mem_a                      (mem_a),                      //                  .export
		.mem_dqs                    (mem_dqs),                    //                  .export
		.mem_dm                     (mem_dm),                     //                  .export
		.mem_dqs_n                  (mem_dqs_n),                  //                  .export
		.pll_ref_clk                (pll_ref_clk),                //        clock_sink.clk
		.soft_reset_n               (soft_reset_n),               //  clock_sink_reset.reset_n
		.afi_clk                    (afi_clk),                    //      clock_source.clk
		.afi_half_clk               (afi_half_clk),               // half_clock_source.clk
		.global_reset_n             (global_reset_n),             //             Other.export
		.reset_request_n            (reset_request_n),            //                  .export
		.afi_reset_n                (afi_reset_n),                //                  .export
		.afi_cal_success            (afi_cal_success),            //                  .export
		.afi_cal_fail               (afi_cal_fail),               //                  .export
		.local_init_done            (local_init_done),            //                  .export
		.oct_rdn                    (oct_rdn),                    //                  .export
		.oct_rup                    (oct_rup),                    //                  .export
		.afi_cal_debug_info         (afi_cal_debug_info),         //     afi_cal_debug.export
		.pll_afi_clk                (pll_afi_clk),                //       PLL_sharing.export
		.pll_mem_clk                (pll_mem_clk),                //                  .export
		.pll_write_clk              (pll_write_clk),              //                  .export
		.pll_addr_cmd_clk           (pll_addr_cmd_clk),           //                  .export
		.pll_locked                 (pll_locked),                 //                  .export
		.pll_afi_half_clk           (pll_afi_half_clk),           //                  .export
		.pll_avl_clk                (pll_avl_clk),                //                  .export
		.pll_config_clk             (pll_config_clk),             //                  .export
		.dll_delayctrl              (dll_delayctrl),              //       DLL_sharing.export
		.parallelterminationcontrol (parallelterminationcontrol), //       OCT_sharing.export
		.seriesterminationcontrol   (seriesterminationcontrol),   //                  .export
		.avl_ready                  (avl_ready),                  //    avalon_slave_0.waitrequest_n
		.avl_addr                   (avl_addr),                   //                  .address
		.avl_size                   (avl_size),                   //                  .burstcount
		.avl_burstbegin             (avl_burstbegin),             //                  .beginbursttransfer
		.avl_be                     (avl_be),                     //                  .byteenable
		.avl_wdata                  (avl_wdata),                  //                  .writedata
		.avl_rdata                  (avl_rdata),                  //                  .readdata
		.avl_write_req              (avl_write_req),              //                  .write
		.avl_read_req               (avl_read_req),               //                  .read
		.avl_rdata_valid            (avl_rdata_valid),            //                  .readdatavalid
                .csr_addr                   (csr_addr),
                .csr_be                     (csr_be),
                .csr_read_req               (csr_read_req),
                .csr_wdata                  (csr_wdata),
                .csr_write_req              (csr_write_req),
                .csr_rdata                  (csr_rdata),
                .csr_rdata_valid            (csr_rdata_valid),
                .csr_waitrequest            (csr_waitrequest)
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
// Retrieval info: <instance entity-name="altera_uniphy_ddr2" version="10.1" >
// Retrieval info: 	<generic name="DEVICE_FAMILY" value="Stratix III" />
// Retrieval info: 	<generic name="MEM_AUTO_LEVELING_MODE" value="true" />
// Retrieval info: 	<generic name="MEM_USER_LEVELING_MODE" value="Leveling" />
// Retrieval info: 	<generic name="SPEED_GRADE" value="4" />
// Retrieval info: 	<generic name="MEM_CLK_FREQ" value="250.0" />
// Retrieval info: 	<generic name="REF_CLK_FREQ" value="100.0" />
// Retrieval info: 	<generic name="RATE" value="Half" />
// Retrieval info: 	<generic name="SEQ_MODE" value="0" />
// Retrieval info: 	<generic name="ADVANCED_CK_PHASES" value="false" />
// Retrieval info: 	<generic name="COMMAND_PHASE" value="0.0" />
// Retrieval info: 	<generic name="MEM_CK_PHASE" value="0.0" />
// Retrieval info: 	<generic name="PLL_MAX_VCO_MODE" value="false" />
// Retrieval info: 	<generic name="DEBUGGING_LEVEL" value="Intermediate" />
// Retrieval info: 	<generic name="ADD_EFFICIENCY_MONITOR" value="false" />
// Retrieval info: 	<generic name="ADD_NOISE_GENERATOR" value="false" />
// Retrieval info: 	<generic name="NOISE_GENERATOR_BLOCKS" value="1" />
// Retrieval info: 	<generic name="POWER_OF_TWO_BUS" value="false" />
// Retrieval info: 	<generic name="AVL_MAX_SIZE" value="4" />
// Retrieval info: 	<generic name="PLL_DLL_MASTER" value="true" />
// Retrieval info: 	<generic name="OCT_MASTER" value="true" />
// Retrieval info: 	<generic name="SOPCB_COMPAT_MODE" value="false" />
// Retrieval info: 	<generic name="HCX_COMPAT_MODE" value="false" />
// Retrieval info: 	<generic name="PLL_LOCATION" value="Top_Bottom" />
// Retrieval info: 	<generic name="DISCRETE_FLY_BY" value="true" />
// Retrieval info: 	<generic name="DEVICE_DEPTH" value="1" />
// Retrieval info: 	<generic name="MEM_MIRROR_ADDRESSING" value="0" />
// Retrieval info: 	<generic name="MEM_VENDOR" value="JEDEC" />
// Retrieval info: 	<generic name="MEM_FORMAT" value="UNBUFFERED" />
// Retrieval info: 	<generic name="MEM_CLK_FREQ_MAX" value="266.667" />
// Retrieval info: 	<generic name="MEM_ROW_ADDR_WIDTH" value="16" />
// Retrieval info: 	<generic name="MEM_COL_ADDR_WIDTH" value="12" />
// Retrieval info: 	<generic name="MEM_DQ_WIDTH" value="64" />
// Retrieval info: 	<generic name="MEM_DQ_PER_DQS" value="8" />
// Retrieval info: 	<generic name="MEM_BANKADDR_WIDTH" value="3" />
// Retrieval info: 	<generic name="MEM_IF_DM_PINS_EN" value="true" />
// Retrieval info: 	<generic name="MEM_IF_DQSN_EN" value="true" />
// Retrieval info: 	<generic name="MEM_NUMBER_OF_DIMMS" value="1" />
// Retrieval info: 	<generic name="MEM_NUMBER_OF_RANKS_PER_DIMM" value="2" />
// Retrieval info: 	<generic name="MEM_CK_WIDTH" value="2" />
// Retrieval info: 	<generic name="MEM_BL" value="8" />
// Retrieval info: 	<generic name="MEM_BT" value="Sequential" />
// Retrieval info: 	<generic name="MEM_ASR" value="Manual" />
// Retrieval info: 	<generic name="MEM_SRT" value="2x refresh rate" />
// Retrieval info: 	<generic name="MEM_PD" value="Fast exit" />
// Retrieval info: 	<generic name="MEM_DRV_STR" value="Full" />
// Retrieval info: 	<generic name="MEM_DLL_EN" value="true" />
// Retrieval info: 	<generic name="MEM_RTT_NOM" value="Disabled" />
// Retrieval info: 	<generic name="MEM_ATCL" value="0" />
// Retrieval info: 	<generic name="MEM_TCL" value="4" />
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
// Retrieval info: 	<generic name="CALIBRATION_MODE" value="Skip" />
// Retrieval info: 	<generic name="SKIP_MEM_INIT" value="true" />
// Retrieval info: 	<generic name="READ_FIFO_SIZE" value="8" />
// Retrieval info: 	<generic name="DEBUG_MODE" value="false" />
// Retrieval info: 	<generic name="EXTRA_SETTINGS" value="" />
// Retrieval info: 	<generic name="FORCE_SYNTHESIS_LANGUAGE" value="" />
// Retrieval info: 	<generic name="MEM_DEVICE_MAX_ADDR_WIDTH" value="22" />
// Retrieval info: 	<generic name="AC_PARITY" value="false" />
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
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_CKDQS_DIMM_MIN" value="-0.01" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_CKDQS_DIMM_MAX" value="0.01" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_BETWEEN_DIMMS" value="0.05" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_WITHIN_DQS" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_SKEW_BETWEEN_DQS" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_DQ_TO_DQS_SKEW" value="0.0" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_SKEW" value="0.02" />
// Retrieval info: 	<generic name="TIMING_BOARD_AC_TO_CK_SKEW" value="0.6" />
// Retrieval info: 	<generic name="CTL_HRB_ENABLED" value="false" />
// Retrieval info: 	<generic name="CTL_SELF_REFRESH_EN" value="false" />
// Retrieval info: 	<generic name="AUTO_POWERDN_EN" value="false" />
// Retrieval info: 	<generic name="MEM_AUTO_PD_CYCLES" value="0" />
// Retrieval info: 	<generic name="CTL_USR_REFRESH_EN" value="false" />
// Retrieval info: 	<generic name="CTL_AUTOPCH_EN" value="false" />
// Retrieval info: 	<generic name="ADDR_ORDER" value="0" />
// Retrieval info: 	<generic name="CTL_LOOK_AHEAD_DEPTH" value="4" />
// Retrieval info: 	<generic name="CTL_CSR_ENABLED" value="true" />
// Retrieval info: 	<generic name="CTL_CSR_CONNECTION" value="EXPORT" />
// Retrieval info: 	<generic name="CTL_ECC_ENABLED" value="false" />
// Retrieval info: 	<generic name="CTL_ECC_AUTO_CORRECTION_ENABLED" value="false" />
// Retrieval info: 	<generic name="MULTICAST_EN" value="false" />
// Retrieval info: 	<generic name="ENABLE_BURST_MERGE" value="false" />
// Retrieval info: 	<generic name="CONTROLLER_LATENCY" value="5" />
// Retrieval info: 	<generic name="CTL_DYNAMIC_BANK_ALLOCATION" value="false" />
// Retrieval info: 	<generic name="CTL_DYNAMIC_BANK_NUM" value="4" />
// Retrieval info: 	<generic name="NEXTGEN" value="false" />
// Retrieval info: 	<generic name="LOCAL_ID_WIDTH" value="8" />
// Retrieval info: 	<generic name="CTL_TBP_NUM" value="4" />
// Retrieval info: 	<generic name="RDBUFFER_ADDR_WIDTH" value="12" />
// Retrieval info: 	<generic name="WRBUFFER_ADDR_WIDTH" value="12" />
// Retrieval info: 	<generic name="CFG_STARVE_LIMIT" value="4" />
// Retrieval info: 	<generic name="CFG_REORDER_DATA" value="false" />
// Retrieval info: 	<generic name="USE_MM_ADAPTOR" value="true" />
// Retrieval info: 	<generic name="USE_AXI_ADAPTOR" value="false" />
// Retrieval info: 	<generic name="AUTO_CLOCK_SINK_CLOCK_RATE" value="-1" />
// Retrieval info: </instance>
// IPFS_FILES : NONE
