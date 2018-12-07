// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis translate_off
`timescale 1ps / 1ps
// synthesis translate_on

module ddr2_v10_1_0002 (
	pll_ref_clk,
	pll_afi_clk,
	pll_mem_clk,
	pll_write_clk,
	pll_addr_cmd_clk,
	pll_afi_half_clk,
	pll_avl_clk,
	pll_config_clk,
	pll_locked,
	dll_delayctrl,

	global_reset_n,
	soft_reset_n,
	oct_rdn,
	oct_rup,
    parallelterminationcontrol,
    seriesterminationcontrol,

	reset_request_n,
	afi_clk,
	afi_half_clk,
	afi_reset_n,
	afi_cal_debug_info,
	afi_cal_success,
	afi_cal_fail,
	local_init_done,


	mem_a,
	mem_ba,
	mem_ck,
	mem_ck_n,
	mem_cke,
	mem_cs_n,
	mem_dm,
	mem_odt,
	mem_ras_n,
	mem_cas_n,
	mem_we_n,
	csr_addr,
	csr_be,
	csr_rdata,
	csr_read_req,
	csr_wdata,
	csr_write_req,
	csr_rdata_valid,
	csr_waitrequest,
	mem_dq,
	mem_dqs,
	mem_dqs_n,

	avl_ready,
	avl_write_req,
	avl_read_req,
	avl_burstbegin,
	avl_addr,
	avl_size,
	avl_be,
	avl_wdata,
	avl_rdata_valid,
	avl_rdata

);

parameter CSR_ADDR_WIDTH = 16;
parameter CSR_BE_WIDTH = 4;
parameter CSR_DATA_WIDTH = 32;
parameter OCT_SERIES_TERM_CONTROL_WIDTH   = 14;
parameter OCT_PARALLEL_TERM_CONTROL_WIDTH = 14;


// PLL/DLL Interface
input   pll_ref_clk;        // PLL reference clock

// When the PHY is selected to be a PLL/DLL MASTER, the PLL and DLL are instantied on this top level
output	pll_afi_clk;		// See pll_memphy instantiation below for detailed description of each clock
output	pll_mem_clk;	
output	pll_write_clk;
output	pll_addr_cmd_clk;
output	pll_afi_half_clk;
output	pll_avl_clk;
output	pll_config_clk;
output	pll_locked;
output	[6-1:0]  dll_delayctrl;


input			global_reset_n;
input			soft_reset_n;
// On-Chip Termination
// These should be connected to reference resistance pins on the board, via OCT control block if instantiated by user
input   oct_rdn;
input   oct_rup;
// for OCT master, termination control signals will be available to top level
output [OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] parallelterminationcontrol;
output [OCT_SERIES_TERM_CONTROL_WIDTH-1:0] seriesterminationcontrol;

output			reset_request_n;
output			afi_clk;
output			afi_half_clk;
output			afi_reset_n;
output [32 - 1:0] 	afi_cal_debug_info;
output			afi_cal_success;
output			afi_cal_fail;
output          local_init_done;

output  [16-1:0]	mem_a;
output  [3-1:0]	mem_ba;
output  [2-1:0]	mem_ck;
output  [2-1:0]	mem_ck_n;
output  [2-1:0] mem_cke;
output  [2-1:0] mem_cs_n;
output  [8-1:0]  mem_dm;
output  [2-1:0] mem_odt;
output  [1-1:0] mem_ras_n;
output  [1-1:0] mem_cas_n;
output  [1-1:0] mem_we_n;
inout   [64-1:0]  mem_dq;
inout   [8-1:0] mem_dqs;
inout   [8-1:0] mem_dqs_n;


output			avl_ready;
input			avl_write_req;
input			avl_read_req;
input			avl_burstbegin;
input	[30-1:0]	avl_addr;
input	[3-1:0]	avl_size;
input	[32-1:0]	avl_be;
input	[256-1:0]	avl_wdata;
output			avl_rdata_valid;
output	[256-1:0]	avl_rdata;
input    [CSR_ADDR_WIDTH - 1: 0]    csr_addr;
input    [CSR_BE_WIDTH - 1: 0]      csr_be;
input                               csr_read_req;
input    [CSR_DATA_WIDTH - 1: 0]    csr_wdata;
input                               csr_write_req;
output   [CSR_DATA_WIDTH - 1: 0]    csr_rdata;
output                              csr_rdata_valid;
output                              csr_waitrequest;



ddr2_v10_1_0002_controller_phy controller_phy_inst (
	.pll_ref_clk		(pll_ref_clk),
	.pll_afi_clk		(pll_afi_clk),
	.pll_addr_cmd_clk	(pll_addr_cmd_clk),
	.pll_mem_clk		(pll_mem_clk),
	.pll_write_clk		(pll_write_clk),
	.pll_afi_half_clk	(pll_afi_half_clk),
	.pll_avl_clk		(pll_avl_clk),
	.pll_config_clk		(pll_config_clk),
	.pll_locked			(pll_locked),
	.dll_delayctrl		(dll_delayctrl),
	.global_reset_n		(global_reset_n),
	.soft_reset_n		(soft_reset_n),
	.oct_rdn			(oct_rdn),
	.oct_rup			(oct_rup),
	.parallelterminationcontrol (parallelterminationcontrol),
	.seriesterminationcontrol (seriesterminationcontrol),
	.reset_request_n	(reset_request_n),
	.afi_clk			(afi_clk),
	.afi_half_clk		(afi_half_clk),
	.afi_reset_n		(afi_reset_n),
	.afi_cal_debug_info(afi_cal_debug_info),
	.afi_cal_success	(afi_cal_success),
	.afi_cal_fail		(afi_cal_fail),
    .local_init_done    (local_init_done),
	.mem_a				(mem_a),
	.mem_ba				(mem_ba),
	.mem_ck				(mem_ck),
	.mem_ck_n			(mem_ck_n),
	.mem_cke			(mem_cke),
	.mem_cs_n			(mem_cs_n),
	.mem_dm				(mem_dm),
	.mem_odt			(mem_odt),
	.mem_ras_n			(mem_ras_n),
	.mem_cas_n			(mem_cas_n),
	.mem_we_n			(mem_we_n),
	.mem_dq				(mem_dq),
	.mem_dqs			(mem_dqs),
	.mem_dqs_n			(mem_dqs_n),
	.avl_ready			(avl_ready),
	.avl_write_req		(avl_write_req),
	.avl_read_req		(avl_read_req),
    .csr_addr             (csr_addr         ), 
    .csr_be               (csr_be           ),
    .csr_rdata            (csr_rdata        ),
    .csr_read_req         (csr_read_req     ),
    .csr_wdata            (csr_wdata        ),
    .csr_write_req        (csr_write_req    ),
    .csr_rdata_valid      (csr_rdata_valid  ),
    .csr_waitrequest      (csr_waitrequest  ),
	.avl_burstbegin		(avl_burstbegin),
	.avl_addr			(avl_addr),
	.avl_size			(avl_size),
	.avl_be				(avl_be),
	.avl_wdata			(avl_wdata),
	.avl_rdata_valid	(avl_rdata_valid),
	.avl_rdata			(avl_rdata)
);
defparam controller_phy_inst.DEVICE_FAMILY							= "StratixIV";
defparam controller_phy_inst.MEM_IF_BANKADDR_WIDTH                  = 3;
defparam controller_phy_inst.MEM_IF_DQS_WIDTH						= 8;
defparam controller_phy_inst.MEM_IF_DWIDTH							= 64;
defparam controller_phy_inst.MEM_IF_ADDR_WIDTH						= 16;
defparam controller_phy_inst.MEM_IF_DM_WIDTH						= 8;
defparam controller_phy_inst.MEM_IF_READ_DQS_WIDTH					= 8;
defparam controller_phy_inst.MEM_IF_WRITE_DQS_WIDTH					= 8;
defparam controller_phy_inst.MEM_IF_CLK_EN_WIDTH					= 2;
defparam controller_phy_inst.MEM_IF_CK_WIDTH						= 2;
defparam controller_phy_inst.MEM_IF_ODT_WIDTH						= 2;
defparam controller_phy_inst.MEM_IF_CS_WIDTH						= 2;
defparam controller_phy_inst.MEM_IF_CONTROL_WIDTH					= 1;
defparam controller_phy_inst.MEM_IF_DQ_PER_DQS						= 8;
defparam controller_phy_inst.MEM_IF_CLK_PS							= 4000;
defparam controller_phy_inst.MEM_TINIT_CK							= 50000;
defparam controller_phy_inst.MEM_TMRD_CK							= 2;
defparam controller_phy_inst.MR0_BL									= 3;
defparam controller_phy_inst.MR0_BT									= 0;
defparam controller_phy_inst.MR0_CAS_LATENCY						= 4;
defparam controller_phy_inst.MR0_WR									= 3;
defparam controller_phy_inst.MR0_PD									= 0;
defparam controller_phy_inst.MR1_DLL								= 0;
defparam controller_phy_inst.MR1_ODS								= 0;
defparam controller_phy_inst.MR1_RTT								= 0;
defparam controller_phy_inst.MR1_AL									= 0;
defparam controller_phy_inst.MR1_DQS								= 0;
defparam controller_phy_inst.MR1_RDQS								= 0;
defparam controller_phy_inst.MR1_QOFF								= 0;
defparam controller_phy_inst.MR2_SRF								= 1;
defparam controller_phy_inst.MEM_BURST_LENGTH						= 8;
defparam controller_phy_inst.MEM_T_WL								= 2;
defparam controller_phy_inst.MEM_T_RL								= 4;
defparam controller_phy_inst.AFI_BANKADDR_WIDTH						= 6;
defparam controller_phy_inst.AFI_CLK_EN_WIDTH						= 4;
defparam controller_phy_inst.AFI_ODT_WIDTH							= 4;
defparam controller_phy_inst.AFI_DQS_WIDTH							= 16;
defparam controller_phy_inst.AFI_ADDR_WIDTH							= 32;
defparam controller_phy_inst.AFI_DM_WIDTH							= 32;
defparam controller_phy_inst.AFI_DWIDTH								= 256;
defparam controller_phy_inst.AFI_CONTROL_WIDTH						= 2;
defparam controller_phy_inst.AFI_CS_WIDTH							= 4;
defparam controller_phy_inst.AVL_ADDR_WIDTH							= 30;
defparam controller_phy_inst.AVL_SIZE_WIDTH							= 3;
defparam controller_phy_inst.AVL_DWIDTH								= 256;
defparam controller_phy_inst.AVL_BE_WIDTH                           = 32;
defparam controller_phy_inst.AVL_NUM_BYTES							= 32;
defparam controller_phy_inst.SEQ_BURST_COUNT_WIDTH					= 1;
defparam controller_phy_inst.DLL_DELAY_CTRL_WIDTH					= 6;
defparam controller_phy_inst.VCALIB_COUNT_WIDTH                     = 2;
defparam controller_phy_inst.NUM_SUBGROUP_PER_READ_DQS              = 1;
defparam controller_phy_inst.QVLD_EXTRA_FLOP_STAGES					= 0;
defparam controller_phy_inst.QVLD_WR_ADDRESS_OFFSET					= 3;
defparam controller_phy_inst.NUM_WRITE_PATH_FLOP_STAGES				= 0;

defparam controller_phy_inst.MEM_TYPE                               = "DDR2";
                                                                    
defparam controller_phy_inst.LOCAL_SIZE_WIDTH                       = 3;
defparam controller_phy_inst.LOCAL_ADDR_WIDTH                       = 30;
defparam controller_phy_inst.LOCAL_DATA_WIDTH                       = 256;
defparam controller_phy_inst.LOCAL_IF_TYPE                          = "AVALON";      
                                                                    
defparam controller_phy_inst.MEM_IF_CHIP_BITS                       = 1;
defparam controller_phy_inst.MEM_IF_CKE_WIDTH                       = 2;
defparam controller_phy_inst.MEM_IF_ROW_WIDTH                       = 16; // max supported row bits
defparam controller_phy_inst.MEM_IF_COL_WIDTH                       = 12; // max supported column bits  
defparam controller_phy_inst.MEM_IF_CLK_PAIR_COUNT                  = 2;
defparam controller_phy_inst.MEM_IF_CS_PER_DIMM                     = 2; // 2 ranks 1 slot
defparam controller_phy_inst.DWIDTH_RATIO                           = 4;

defparam controller_phy_inst.CTL_CS_WIDTH                           = 2;
defparam controller_phy_inst.CTL_LOOK_AHEAD_DEPTH                   = 4;
defparam controller_phy_inst.CTL_CMD_QUEUE_DEPTH                    = 8;
defparam controller_phy_inst.CTL_HRB_ENABLED                        = 0; // off by default
defparam controller_phy_inst.CTL_ECC_ENABLED                        = 0;
defparam controller_phy_inst.CTL_ECC_RMW_ENABLED                    = 0;
defparam controller_phy_inst.CTL_ECC_CSR_ENABLED                    = 0;
defparam controller_phy_inst.CTL_ECC_MULTIPLES_40_72                = 0;
defparam controller_phy_inst.CTL_CSR_ENABLED                        = 1; // off by default
defparam controller_phy_inst.CTL_CSR_READ_ONLY                      = 1;
defparam controller_phy_inst.CTL_ODT_ENABLED                        = 0;
defparam controller_phy_inst.CTL_REGDIMM_ENABLED                    = 0;
defparam controller_phy_inst.CSR_ADDR_WIDTH                         = CSR_ADDR_WIDTH;
defparam controller_phy_inst.CSR_DATA_WIDTH                         = CSR_DATA_WIDTH;
defparam controller_phy_inst.CSR_BE_WIDTH                           = CSR_BE_WIDTH;
defparam controller_phy_inst.CTL_OUTPUT_REGD                        = 0; // 1 is registered version and tested, low latency is being tested
defparam controller_phy_inst.CTL_USR_REFRESH                        = 0; // 1 when user has control over refresh
defparam controller_phy_inst.LOW_LATENCY                            = 0; 
defparam controller_phy_inst.CTL_DYNAMIC_BANK_ALLOCATION            = 0;
defparam controller_phy_inst.CTL_DYNAMIC_BANK_NUM                   = 4;
defparam controller_phy_inst.ENABLE_BURST_MERGE                     = 0;
defparam controller_phy_inst.MULTICAST_WR_EN                        = 0;
        
defparam controller_phy_inst.MEM_CAS_WR_LAT                         = 3;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_ADD_LAT                            = 0;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TCL                                = 4;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRRD                               = 3;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TFAW                               = 13;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRFC                               = 27;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TREFI                              = 1950;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRCD                               = 4;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRP                                = 4;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TWR                                = 4;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TWTR                               = 2;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRTP                               = 3;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRAS                               = 12;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_TRC                                = 15;        // these timing parameter must be set properly for controller to work
defparam controller_phy_inst.MEM_AUTO_PD_CYCLES                     = 0;        // these timing parameter must be set properly for controller to work

defparam controller_phy_inst.MEM_IF_RD_TO_WR_TURNAROUND_OCT = 6;
defparam controller_phy_inst.MEM_IF_WR_TO_RD_TURNAROUND_OCT = 3;


defparam controller_phy_inst.ADDR_ORDER                             = 0 ;        // normally we will use '1' for chip; bank, row, column arrangement
defparam controller_phy_inst.DELAY_PER_OPA_TAP 					= 333;
defparam controller_phy_inst.DELAY_PER_DCHAIN_TAP 				= 50;
defparam controller_phy_inst.DLL_DELAY_CHAIN_LENGTH 			= 12;
defparam controller_phy_inst.MEM_NUMBER_OF_RANKS 				= 2;
defparam controller_phy_inst.MEM_MIRROR_ADDRESSING 				= 0;
defparam controller_phy_inst.DLL_OFFSET_CTRL_WIDTH              = 6;
defparam controller_phy_inst.PLL_PHASE_COUNTER_WIDTH            = 4;
defparam controller_phy_inst.ALTDQDQS_INPUT_FREQ = 250.0;
defparam controller_phy_inst.ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = "HIGH";
defparam controller_phy_inst.ALTDQDQS_DQS_PHASE_SETTING = 3;
defparam controller_phy_inst.ALTDQDQS_DQS_PHASE_SHIFT = 9000;
defparam controller_phy_inst.ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = 2;
defparam controller_phy_inst.AFI_DEBUG_INFO_WIDTH = 32;
endmodule
