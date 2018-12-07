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

module ddr2_v10_1_controller_phy (
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
    mem_dq,
    mem_dqs,
    mem_dqs_n,
    csr_addr,
    csr_be,
    csr_rdata,
    csr_read_req,
    csr_wdata,
    csr_write_req,
    csr_rdata_valid,
    csr_waitrequest,
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


parameter DEVICE_FAMILY							= "";

parameter MEM_IF_DWIDTH							= 0;
parameter MEM_IF_ADDR_WIDTH						= 0;
parameter MEM_IF_DQS_WIDTH		                = 0;
parameter MEM_IF_BANKADDR_WIDTH                 = 0;
parameter MEM_IF_CLK_EN_WIDTH					= 0;
parameter MEM_IF_CK_WIDTH						= 0;
parameter MEM_IF_ODT_WIDTH						= 0;
parameter MEM_IF_CS_WIDTH						= 0;
parameter MEM_IF_CONTROL_WIDTH					= 0;
parameter MEM_IF_DM_WIDTH						= 0;
parameter MEM_IF_DQ_PER_DQS						= 0;
parameter MEM_IF_READ_DQS_WIDTH					= 0;
parameter MEM_IF_WRITE_DQS_WIDTH				= 0;
parameter MEM_IF_CLK_PS							= 0; 


parameter MEM_TINIT_CK = "";
parameter MEM_TMRD_CK = "";

parameter MR0_BL								= 0;
parameter MR0_BT								= 0;
parameter MR0_CAS_LATENCY						= 0;
parameter MR0_WR								= 0;
parameter MR0_PD								= 0;
parameter MR1_DLL								= 0;
parameter MR1_ODS								= 0;
parameter MR1_RTT								= 0;
parameter MR1_AL								= 0;
parameter MR1_DQS								= 0;
parameter MR1_RDQS								= 0;
parameter MR1_QOFF								= 0;
parameter MR2_SRF								= 0;

parameter MEM_BURST_LENGTH						= 0;
parameter MEM_T_WL								= 0;
parameter MEM_T_RL								= 0;

parameter AFI_ADDR_WIDTH						= 0;
parameter AFI_BANKADDR_WIDTH					= 0;
parameter AFI_MAX_WRITE_LATENCY_COUNT_WIDTH		= 5;
parameter AFI_MAX_READ_LATENCY_COUNT_WIDTH		= 5;
parameter AFI_CLK_EN_WIDTH						= 0;
parameter AFI_ODT_WIDTH							= 0;
parameter AFI_DQS_WIDTH							= 0;
parameter AFI_CONTROL_WIDTH						= 0;
parameter AFI_CS_WIDTH							= 0;
parameter AFI_DM_WIDTH							= 0;
parameter AFI_DWIDTH							= 0;


parameter AVL_ADDR_WIDTH						= 0;
parameter AVL_SIZE_WIDTH						= 0;
parameter AVL_DWIDTH							= 0;
parameter AVL_BE_WIDTH							= 0;
parameter AVL_NUM_BYTES							= 0;

parameter SEQ_BURST_COUNT_WIDTH					= 0;
parameter DLL_DELAY_CTRL_WIDTH					= 0;

parameter VCALIB_COUNT_WIDTH                    = 0;
parameter NUM_SUBGROUP_PER_READ_DQS             = 0;
parameter QVLD_EXTRA_FLOP_STAGES				= 0;
parameter QVLD_WR_ADDRESS_OFFSET				= 0;
parameter NUM_WRITE_PATH_FLOP_STAGES			= 0;

parameter 	    MEM_TYPE           = "DDR3";
                
parameter       LOCAL_SIZE_WIDTH   = 3;
parameter       LOCAL_ADDR_WIDTH   = 26;
parameter       LOCAL_DATA_WIDTH   = 32;
parameter       LOCAL_IF_TYPE      = "AVALON";      
        
parameter       MEM_IF_CHIP_BITS   = 1;
parameter       MEM_IF_CKE_WIDTH   = 1;
parameter       MEM_IF_ROW_WIDTH   = 13; // max supported row bits
parameter       MEM_IF_COL_WIDTH   = 10; // max supported column bits  
parameter       MEM_IF_CLK_PAIR_COUNT = 2;
parameter       MEM_IF_CS_PER_DIMM = 2; // 2 ranks 1 slot
parameter       DWIDTH_RATIO       = 2;

parameter       CTL_CS_WIDTH                   = 0;
parameter       CTL_LOOK_AHEAD_DEPTH           = 4;
parameter       CTL_CMD_QUEUE_DEPTH            = 8;
parameter       CTL_HRB_ENABLED                = 0; // off by default
parameter       CTL_ECC_ENABLED                = 0;
parameter       CTL_ECC_RMW_ENABLED            = 0;
parameter       CTL_ECC_CSR_ENABLED            = 0;
parameter       CTL_ECC_MULTIPLES_40_72        = 1;
parameter       CTL_CSR_ENABLED                = 0; // off by default
parameter       CTL_CSR_READ_ONLY              = 0;
parameter       CTL_ODT_ENABLED                = 1;
parameter       CTL_REGDIMM_ENABLED            = 0;
parameter       CSR_ADDR_WIDTH                 = 16;
parameter       CSR_DATA_WIDTH                 = 32;
parameter       CSR_BE_WIDTH                   = 4;
parameter       CTL_OUTPUT_REGD                = 0; // 1 is registered version and tested, low latency is being tested
parameter       CTL_USR_REFRESH                = 0; // 1 when user has control over refresh
parameter       LOW_LATENCY                    = 1; 
parameter       CTL_DYNAMIC_BANK_ALLOCATION    = 0; 
parameter       CTL_DYNAMIC_BANK_NUM           = 4;
parameter       ENABLE_BURST_MERGE             = 1;
parameter       MULTICAST_WR_EN                = 0;
        
parameter       MEM_CAS_WR_LAT                 = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_ADD_LAT                    = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TCL                        = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRRD                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TFAW                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRFC                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TREFI                      = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRCD                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRP                        = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TWR                        = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TWTR                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRTP                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRAS                       = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_TRC                        = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_AUTO_PD_CYCLES             = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_IF_RD_TO_WR_TURNAROUND_OCT = 0;        // these timing parameter must be set properly for controller to work
parameter       MEM_IF_WR_TO_RD_TURNAROUND_OCT = 0;        // these timing parameter must be set properly for controller to work
       
parameter       ADDR_ORDER          = 1 ;        // normally we will use '1' for chip; bank, row, column arrangement
parameter       RDIMM_CONFIG        = 64'h0;
parameter AFI_DEBUG_INFO_WIDTH = "";

parameter DELAY_PER_OPA_TAP   = "";
parameter DELAY_PER_DCHAIN_TAP   = "";
parameter DLL_DELAY_CHAIN_LENGTH = "";
parameter MEM_NUMBER_OF_RANKS = "";
parameter MEM_MIRROR_ADDRESSING = "";

// The DLL offset control width
parameter DLL_OFFSET_CTRL_WIDTH = "";

// The PLL Phase counter width
parameter PLL_PHASE_COUNTER_WIDTH = "";

parameter ALTDQDQS_INPUT_FREQ = "";
parameter ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = "";
parameter ALTDQDQS_DQS_PHASE_SETTING = "";
parameter ALTDQDQS_DQS_PHASE_SHIFT = "";
parameter ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = "";


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
output	[DLL_DELAY_CTRL_WIDTH-1:0]  dll_delayctrl;



input								global_reset_n;
input								soft_reset_n;
input   oct_rdn;
input   oct_rup;
output [OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] parallelterminationcontrol;
output [OCT_SERIES_TERM_CONTROL_WIDTH-1:0] seriesterminationcontrol;

output								reset_request_n;
output								afi_clk;
output								afi_half_clk;
output								afi_reset_n;
output [AFI_DEBUG_INFO_WIDTH - 1:0]	afi_cal_debug_info;
output								afi_cal_success;
output								afi_cal_fail;
output                              local_init_done;

output	[MEM_IF_ADDR_WIDTH-1:0]		mem_a;
output	[MEM_IF_BANKADDR_WIDTH-1:0]	mem_ba;
output  [MEM_IF_CK_WIDTH-1:0]	mem_ck;
output  [MEM_IF_CK_WIDTH-1:0]	mem_ck_n;
output  [MEM_IF_CLK_EN_WIDTH-1:0] mem_cke;
output  [MEM_IF_CS_WIDTH-1:0] mem_cs_n;
output  [MEM_IF_DM_WIDTH-1:0]  mem_dm;
output  [MEM_IF_ODT_WIDTH-1:0] mem_odt;
output  [MEM_IF_CONTROL_WIDTH-1:0] mem_ras_n;
output  [MEM_IF_CONTROL_WIDTH-1:0] mem_cas_n;
output  [MEM_IF_CONTROL_WIDTH-1:0] mem_we_n;
inout   [MEM_IF_DWIDTH-1:0]  mem_dq;
inout   [MEM_IF_DQS_WIDTH-1:0] mem_dqs;
inout   [MEM_IF_DQS_WIDTH-1:0] mem_dqs_n;

output								avl_ready;
input								avl_write_req;
input								avl_read_req;
input								avl_burstbegin;
input	[AVL_ADDR_WIDTH-1:0]		avl_addr;
input	[AVL_SIZE_WIDTH-1:0]		avl_size;
input	[AVL_BE_WIDTH-1:0]			avl_be;
input	[AVL_DWIDTH-1:0]			avl_wdata;
output								avl_rdata_valid;
output	[AVL_DWIDTH-1:0]			avl_rdata;
input    [CSR_ADDR_WIDTH - 1: 0]    csr_addr;
input    [CSR_BE_WIDTH - 1: 0]      csr_be;
input                               csr_read_req;
input    [CSR_DATA_WIDTH - 1: 0]    csr_wdata;
input                               csr_write_req;
output   [CSR_DATA_WIDTH - 1: 0]    csr_rdata;
output                              csr_rdata_valid;
output                              csr_waitrequest;





wire								phy_afi_clk;
wire								ctl_afi_clk;
wire								afi_reset_n;
wire	[AFI_ADDR_WIDTH-1:0]		afi_addr;
wire	[AFI_DM_WIDTH-1:0]			afi_dm;
wire    [AFI_DWIDTH-1:0]			afi_wdata;
wire	[AFI_DQS_WIDTH-1:0] 		afi_wdata_valid;
wire								afi_rdata_en;
wire								afi_rdata_en_full;
wire	[AFI_DWIDTH-1:0]			afi_rdata;
wire								afi_rdata_valid;
wire								afi_cal_success;
wire								afi_cal_fail;
wire								local_init_done;

wire	[AFI_BANKADDR_WIDTH-1:0]    afi_ba;
wire	[AFI_CONTROL_WIDTH-1:0] afi_cas_n;
wire	[AFI_CLK_EN_WIDTH-1:0] afi_cke;
wire	[AFI_CS_WIDTH-1:0] afi_cs_n;
wire  [(CTL_CS_WIDTH * (DWIDTH_RATIO/2)) - 1:0] ctl_cs_n;
wire	[AFI_ODT_WIDTH-1:0] afi_odt;
wire	[AFI_CONTROL_WIDTH-1:0] afi_ras_n;
wire	[AFI_CONTROL_WIDTH-1:0] afi_we_n;

wire								local_avl_ready;
wire								local_avl_write_req;
wire								local_avl_read_req;
wire								local_avl_burstbegin;
wire	[AVL_ADDR_WIDTH-1:0]		local_avl_addr;
wire	[AVL_SIZE_WIDTH-1:0]		local_avl_size;
wire	[AVL_BE_WIDTH-1:0]			local_avl_be;
wire	[AVL_DWIDTH-1:0]			local_avl_wdata;
wire								local_avl_rdata_valid;
wire	[AVL_DWIDTH-1:0]			local_avl_rdata;
wire	afi_mem_clk_disable;
wire	[AFI_DQS_WIDTH-1:0] afi_dqs_burst;
wire	[AFI_MAX_WRITE_LATENCY_COUNT_WIDTH-1:0]	afi_wlat;
wire	[AFI_MAX_READ_LATENCY_COUNT_WIDTH-1:0]	afi_rlat;

wire    [CSR_ADDR_WIDTH - 1: 0]    ctl_csr_addr;
wire                               ctl_csr_addr_valid;
wire    [CSR_BE_WIDTH - 1: 0]      ctl_csr_be;
wire                               ctl_csr_read_req;
wire    [CSR_DATA_WIDTH - 1: 0]    ctl_csr_wdata;
wire                               ctl_csr_write_req;
wire    [CSR_DATA_WIDTH - 1: 0]    ctl_csr_rdata;
wire                               ctl_csr_rdata_valid;
wire                               ctl_csr_waitrequest;

wire    [CSR_ADDR_WIDTH - 1: 0]    phy_csr_addr;
wire                               phy_csr_addr_valid;
wire    [CSR_BE_WIDTH - 1: 0]      phy_csr_be;
wire                               phy_csr_read_req;
wire    [CSR_DATA_WIDTH - 1: 0]    phy_csr_wdata;
wire                               phy_csr_write_req;
wire    [CSR_DATA_WIDTH - 1: 0]    phy_csr_rdata;
wire                               phy_csr_rdata_valid;
wire                               phy_csr_waitrequest;



assign ctl_csr_addr_valid = (csr_addr & 9'h100) == {{(CSR_ADDR_WIDTH-9){1'b0}},9'h100};
assign phy_csr_addr_valid = (csr_addr & 9'h100) == {{(CSR_ADDR_WIDTH-9){1'b0}},9'h000};

assign ctl_csr_addr = csr_addr & {{(CSR_ADDR_WIDTH-8){1'b0}},8'hff};
assign ctl_csr_be = csr_be;
assign ctl_csr_read_req = ctl_csr_addr_valid & csr_read_req;
assign ctl_csr_wdata = csr_wdata;
assign ctl_csr_write_req = ctl_csr_addr_valid & csr_write_req;


// Output rdata based on the controller rdata_valid
assign csr_rdata = (ctl_csr_rdata_valid) ? ctl_csr_rdata : phy_csr_rdata;
assign csr_rdata_valid = (ctl_csr_rdata_valid) ? ctl_csr_rdata_valid : phy_csr_rdata_valid;

//synthesis translate_off
//synthesis translate_on

assign csr_waitrequest = ctl_csr_waitrequest | phy_csr_waitrequest;

assign phy_csr_addr = csr_addr & {{(CSR_ADDR_WIDTH-8){1'b0}},8'hff};
assign phy_csr_be = csr_be;
assign phy_csr_read_req = phy_csr_addr_valid & csr_read_req;
assign phy_csr_wdata = csr_wdata;
assign phy_csr_write_req = phy_csr_addr_valid & csr_write_req;







assign ctl_afi_clk = phy_afi_clk;
assign afi_clk = phy_afi_clk;




assign avl_ready = local_avl_ready;
assign local_avl_read_req = avl_read_req;
assign local_avl_write_req = avl_write_req;
assign local_avl_size = avl_size;
assign local_avl_burstbegin = avl_burstbegin;
assign local_avl_addr = avl_addr;
assign avl_rdata_valid = local_avl_rdata_valid;
assign avl_rdata = local_avl_rdata;
assign local_avl_wdata = avl_wdata;
assign local_avl_be = avl_be;

ddr2_v10_1_alt_ddrx_controller # (
    .MEM_TYPE                       ( MEM_TYPE                       ),
    .LOCAL_SIZE_WIDTH               ( LOCAL_SIZE_WIDTH               ),
    .LOCAL_ADDR_WIDTH               ( LOCAL_ADDR_WIDTH               ),
    .LOCAL_DATA_WIDTH               ( LOCAL_DATA_WIDTH               ),
    .LOCAL_IF_TYPE                  ( LOCAL_IF_TYPE                  ),
    .MEM_IF_CS_WIDTH                ( CTL_CS_WIDTH                   ),
    .MEM_IF_CHIP_BITS               ( MEM_IF_CHIP_BITS               ),
    .MEM_IF_CKE_WIDTH               ( MEM_IF_CKE_WIDTH               ),
    .MEM_IF_ODT_WIDTH               ( MEM_IF_ODT_WIDTH               ),
    .MEM_IF_ADDR_WIDTH              ( MEM_IF_ADDR_WIDTH              ),
    .MEM_IF_ROW_WIDTH               ( MEM_IF_ROW_WIDTH               ),
    .MEM_IF_COL_WIDTH               ( MEM_IF_COL_WIDTH               ),
    .MEM_IF_BA_WIDTH                ( MEM_IF_BANKADDR_WIDTH                ),
    .MEM_IF_DQS_WIDTH               ( MEM_IF_DQS_WIDTH               ),
    .MEM_IF_DQ_WIDTH                ( MEM_IF_DWIDTH                ),
    .MEM_IF_DM_WIDTH                ( MEM_IF_DM_WIDTH                ),
    .MEM_IF_CLK_PAIR_COUNT          ( MEM_IF_CLK_PAIR_COUNT          ),
    .MEM_IF_CS_PER_DIMM             ( MEM_IF_CS_PER_DIMM             ),
    .DWIDTH_RATIO                   ( DWIDTH_RATIO                   ),
    .CTL_LOOK_AHEAD_DEPTH           ( CTL_LOOK_AHEAD_DEPTH           ),
    .CTL_CMD_QUEUE_DEPTH            ( CTL_CMD_QUEUE_DEPTH            ),
    .CTL_HRB_ENABLED                ( CTL_HRB_ENABLED                ),
    .CTL_ECC_ENABLED                ( CTL_ECC_ENABLED                ),
    .CTL_ECC_RMW_ENABLED            ( CTL_ECC_RMW_ENABLED            ),
    .CTL_ECC_CSR_ENABLED            ( CTL_ECC_CSR_ENABLED            ),
    .CTL_ECC_MULTIPLES_40_72        ( CTL_ECC_MULTIPLES_40_72        ),
    .CTL_CSR_ENABLED                ( CTL_CSR_ENABLED                ),
    .CTL_CSR_READ_ONLY              ( CTL_CSR_READ_ONLY              ),
    .CTL_ODT_ENABLED                ( CTL_ODT_ENABLED                ),
    .CTL_REGDIMM_ENABLED            ( CTL_REGDIMM_ENABLED            ),
    .CSR_ADDR_WIDTH                 ( CSR_ADDR_WIDTH                 ),
    .CSR_DATA_WIDTH                 ( CSR_DATA_WIDTH                 ),
    .CTL_OUTPUT_REGD                ( CTL_OUTPUT_REGD                ),
    .CTL_USR_REFRESH                ( CTL_USR_REFRESH                ),
    .MEM_CAS_WR_LAT                 ( MEM_CAS_WR_LAT                 ),
    .MEM_ADD_LAT                    ( MEM_ADD_LAT                    ),
    .MEM_TCL                        ( MEM_TCL                        ),
    .MEM_TRRD                       ( MEM_TRRD                       ),
    .MEM_TFAW                       ( MEM_TFAW                       ),
    .MEM_TRFC                       ( MEM_TRFC                       ),
    .MEM_TREFI                      ( MEM_TREFI                      ),
    .MEM_TRCD                       ( MEM_TRCD                       ),
    .MEM_TRP                        ( MEM_TRP                        ),
    .MEM_TWR                        ( MEM_TWR                        ),
    .MEM_TWTR                       ( MEM_TWTR                       ),
    .MEM_TRTP                       ( MEM_TRTP                       ),
    .MEM_TRAS                       ( MEM_TRAS                       ),
    .MEM_TRC                        ( MEM_TRC                        ),
    .MEM_AUTO_PD_CYCLES             ( MEM_AUTO_PD_CYCLES             ),
    .MEM_IF_RD_TO_WR_TURNAROUND_OCT ( MEM_IF_RD_TO_WR_TURNAROUND_OCT ),
    .MEM_IF_WR_TO_RD_TURNAROUND_OCT ( MEM_IF_WR_TO_RD_TURNAROUND_OCT ),
    .ADDR_ORDER                     ( ADDR_ORDER                     ),
    .LOW_LATENCY                    ( LOW_LATENCY                    ),
    .CTL_DYNAMIC_BANK_ALLOCATION    ( CTL_DYNAMIC_BANK_ALLOCATION    ),
    .CTL_DYNAMIC_BANK_NUM           ( CTL_DYNAMIC_BANK_NUM           ),
    .ENABLE_BURST_MERGE             ( ENABLE_BURST_MERGE             ),
    .MULTICAST_WR_EN                ( MULTICAST_WR_EN                ),
	.USE_BYTEENABLE                 ( 1                              )
    
) alt_ddrx_controller_inst (
    
    .ctl_clk                        ( ctl_afi_clk                    ),
    .ctl_reset_n                    ( afi_reset_n                    ),
    .ctl_half_clk                   ( afi_half_clk                   ),
    .ctl_half_clk_reset_n           ( afi_reset_n                  ),
    .local_ready                    ( local_avl_ready              ),
    .local_read_req                 ( local_avl_read_req           ),
    .local_write_req                ( local_avl_write_req          ),
    .local_wdata_req				(                 ),
    .local_size                     ( local_avl_size               ),
    .local_burstbegin               ( local_avl_burstbegin         ),
    .local_addr                     ( local_avl_addr               ),
    .local_rdata_valid              ( local_avl_rdata_valid        ),
    .local_rdata_error              (               ),
    .local_rdata                    ( local_avl_rdata              ),
    .local_wdata                    ( local_avl_wdata              ),
    .local_be                       ( local_avl_be                 ),
    .local_autopch_req              ( 1'b0              ),
    .local_multicast                ( 1'b0                ),
    .local_init_done                ( local_init_done                ),
    .local_refresh_req              ( 1'b0              ),
    .local_refresh_chip             ( {CTL_CS_WIDTH{1'b0}}             ),
    .local_refresh_ack              (               ),
    .local_self_rfsh_req            ( 1'b0            ),
    .local_self_rfsh_chip           ( {CTL_CS_WIDTH{1'b0}}           ),
    .local_self_rfsh_ack            (             ),
    .local_power_down_ack           (            ),
    .ctl_cal_success                ( afi_cal_success                ),
    .ctl_cal_fail                   ( afi_cal_fail                   ),
    .ctl_cal_req                    (         ),
    .ctl_mem_clk_disable            ( afi_mem_clk_disable            ),
    .ctl_cal_byte_lane_sel_n        (         ),
    .afi_cke                        ( afi_cke                        ),
    .afi_cs_n                       ( ctl_cs_n                       ),
    .afi_ras_n                      ( afi_ras_n                      ),
    .afi_cas_n                      ( afi_cas_n                      ),
    .afi_we_n                       ( afi_we_n                       ),
    .afi_ba                         ( afi_ba                         ),
    .afi_addr                       ( afi_addr                       ),
    .afi_odt                        ( afi_odt                        ),
    .afi_rst_n                      (                                ),
    .afi_dqs_burst                  ( afi_dqs_burst                  ),
    .afi_wdata_valid                ( afi_wdata_valid                ),
    .afi_wdata                      ( afi_wdata                      ),
    .afi_dm                         ( afi_dm                         ),
    .afi_wlat                       ( afi_wlat                       ),
    .afi_doing_read                 ( afi_rdata_en                   ),
    .afi_doing_read_full            ( afi_rdata_en_full              ),
    .afi_rdata                      ( afi_rdata                      ),
    .afi_rdata_valid                ( afi_rdata_valid                ),
    .csr_write_req                  ( ctl_csr_write_req            ),
    .csr_read_req                   ( ctl_csr_read_req             ),
    .csr_addr                       ( ctl_csr_addr                 ),
    .csr_be                         ( ctl_csr_be                   ),
    .csr_wdata                      ( ctl_csr_wdata                ),
    .csr_waitrequest                ( ctl_csr_waitrequest          ),
    .csr_rdata                      ( ctl_csr_rdata                ),
    .csr_rdata_valid                ( ctl_csr_rdata_valid          ),
    .ecc_interrupt                  (  ),
    .bank_information               (              ),
    .bank_open                      (              )
);

ddr2_v10_1_memphy_top memphy_top_inst (
	.global_reset_n(global_reset_n),
	.soft_reset_n(soft_reset_n),
	.reset_request_n(reset_request_n),
	.ctl_reset_n(afi_reset_n),
	.oct_rdn			(oct_rdn),
	.oct_rup			(oct_rup),
	.parallelterminationcontrol (parallelterminationcontrol),
	.seriesterminationcontrol (seriesterminationcontrol),
	.pll_ref_clk(pll_ref_clk),
	.pll_afi_clk            (pll_afi_clk),
	.pll_addr_cmd_clk       (pll_addr_cmd_clk),
	.pll_mem_clk            (pll_mem_clk),
	.pll_write_clk          (pll_write_clk),
	.pll_afi_half_clk		(pll_afi_half_clk),
	.pll_avl_clk			(pll_avl_clk),
	.pll_config_clk			(pll_config_clk),
	.pll_locked				(pll_locked),
	.dll_delayctrl      	(dll_delayctrl),
	.afi_clk(phy_afi_clk),
	.afi_half_clk(afi_half_clk),
	.afi_addr(afi_addr),
	.afi_ba					(afi_ba),
	.afi_cas_n				(afi_cas_n),
	.afi_cke				(afi_cke),
	.afi_cs_n				(afi_cs_n),
	.afi_odt				(afi_odt),
	.afi_ras_n				(afi_ras_n),
	.afi_we_n				(afi_we_n),
	.afi_mem_clk_disable	(afi_mem_clk_disable),
	.afi_dqs_burst			(afi_dqs_burst),
	.afi_wlat				(afi_wlat),
	.afi_rlat				(afi_rlat),	
	.afi_wdata(afi_wdata),
	.afi_wdata_valid(afi_wdata_valid),
	.afi_dm(afi_dm),
	.afi_rdata(afi_rdata),
	.afi_rdata_en(afi_rdata_en),
	.afi_rdata_en_full(afi_rdata_en_full),
	.afi_rdata_valid(afi_rdata_valid),
	.afi_cal_debug_info(afi_cal_debug_info),
	.afi_cal_success(afi_cal_success),
	.afi_cal_fail(afi_cal_fail),
    .csr_write_req                  (phy_csr_write_req),
    .csr_read_req                   (phy_csr_read_req),
    .csr_addr                       (phy_csr_addr),
    .csr_be                         (phy_csr_be),
    .csr_wdata                      (phy_csr_wdata),
    .csr_waitrequest                (phy_csr_waitrequest),
    .csr_rdata                      (phy_csr_rdata),
    .csr_rdata_valid                (phy_csr_rdata_valid),
	.mem_a			(mem_a),
	.mem_ba			(mem_ba),
	.mem_ck			(mem_ck),
	.mem_ck_n		(mem_ck_n),
	.mem_cke		(mem_cke),
	.mem_cs_n		(mem_cs_n),
	.mem_dm			(mem_dm),
	.mem_odt		(mem_odt),
	.mem_ras_n		(mem_ras_n),
	.mem_cas_n		(mem_cas_n),
	.mem_we_n		(mem_we_n),
	.mem_dq			(mem_dq),
	.mem_dqs		(mem_dqs),
	.mem_dqs_n		(mem_dqs_n)
);

defparam memphy_top_inst.DEVICE_FAMILY						= DEVICE_FAMILY;
defparam memphy_top_inst.MEM_ADDRESS_WIDTH					= MEM_IF_ADDR_WIDTH;
defparam memphy_top_inst.MEM_BANK_WIDTH                     = MEM_IF_BANKADDR_WIDTH;
defparam memphy_top_inst.MEM_CHIP_SELECT_WIDTH              = MEM_IF_CS_WIDTH;
defparam memphy_top_inst.MEM_CLK_EN_WIDTH              		= MEM_IF_CLK_EN_WIDTH;
defparam memphy_top_inst.MEM_CK_WIDTH              			= MEM_IF_CK_WIDTH;
defparam memphy_top_inst.MEM_ODT_WIDTH              		= MEM_IF_ODT_WIDTH;
defparam memphy_top_inst.MEM_DQS_WIDTH						= MEM_IF_DQS_WIDTH;
defparam memphy_top_inst.MEM_DM_WIDTH						= MEM_IF_DM_WIDTH;
defparam memphy_top_inst.MEM_CONTROL_WIDTH					= MEM_IF_CONTROL_WIDTH;
defparam memphy_top_inst.MEM_DQ_WIDTH						= MEM_IF_DWIDTH;
defparam memphy_top_inst.MEM_READ_DQS_WIDTH					= MEM_IF_READ_DQS_WIDTH;
defparam memphy_top_inst.MEM_WRITE_DQS_WIDTH				= MEM_IF_WRITE_DQS_WIDTH;
defparam memphy_top_inst.AFI_ADDRESS_WIDTH					= AFI_ADDR_WIDTH;
defparam memphy_top_inst.AFI_BANK_WIDTH                     = AFI_BANKADDR_WIDTH;
defparam memphy_top_inst.AFI_CHIP_SELECT_WIDTH              = AFI_CS_WIDTH;
defparam memphy_top_inst.AFI_CLK_EN_WIDTH              		= AFI_CLK_EN_WIDTH;
defparam memphy_top_inst.AFI_ODT_WIDTH              		= AFI_ODT_WIDTH;
defparam memphy_top_inst.AFI_MAX_WRITE_LATENCY_COUNT_WIDTH 	= AFI_MAX_WRITE_LATENCY_COUNT_WIDTH;
defparam memphy_top_inst.AFI_MAX_READ_LATENCY_COUNT_WIDTH 	= AFI_MAX_READ_LATENCY_COUNT_WIDTH;
defparam memphy_top_inst.AFI_DATA_MASK_WIDTH				= AFI_DM_WIDTH;
defparam memphy_top_inst.AFI_DQS_WIDTH              		= AFI_DQS_WIDTH;
defparam memphy_top_inst.AFI_CONTROL_WIDTH					= AFI_CONTROL_WIDTH;
defparam memphy_top_inst.AFI_DATA_WIDTH						= AFI_DWIDTH;
defparam memphy_top_inst.MEM_TINIT_CK                       = MEM_TINIT_CK;
defparam memphy_top_inst.MEM_TMRD_CK                        = MEM_TMRD_CK;
defparam memphy_top_inst.MR0_BL								= MR0_BL;
defparam memphy_top_inst.MR0_BT								= MR0_BT;
defparam memphy_top_inst.MR0_CAS_LATENCY					= MR0_CAS_LATENCY;
defparam memphy_top_inst.MR0_WR								= MR0_WR;
defparam memphy_top_inst.MR0_PD								= MR0_PD;
defparam memphy_top_inst.MR1_DLL							= MR1_DLL;
defparam memphy_top_inst.MR1_ODS							= MR1_ODS;
defparam memphy_top_inst.MR1_RTT							= MR1_RTT;
defparam memphy_top_inst.MR1_AL								= MR1_AL;
defparam memphy_top_inst.MR1_DQS							= MR1_DQS;
defparam memphy_top_inst.MR1_RDQS							= MR1_RDQS;
defparam memphy_top_inst.MR1_QOFF							= MR1_QOFF;
defparam memphy_top_inst.MR2_SRF							= MR2_SRF;
defparam memphy_top_inst.MEM_BURST_LENGTH					= MEM_BURST_LENGTH;
defparam memphy_top_inst.MEM_T_WL							= MEM_T_WL;
defparam memphy_top_inst.MEM_T_RL							= MEM_T_RL;

defparam memphy_top_inst.SEQ_BURST_COUNT_WIDTH				= SEQ_BURST_COUNT_WIDTH;
defparam memphy_top_inst.DLL_DELAY_CTRL_WIDTH				= DLL_DELAY_CTRL_WIDTH;
defparam memphy_top_inst.VCALIB_COUNT_WIDTH                 = VCALIB_COUNT_WIDTH;
defparam memphy_top_inst.NUM_SUBGROUP_PER_READ_DQS          = NUM_SUBGROUP_PER_READ_DQS;
defparam memphy_top_inst.QVLD_EXTRA_FLOP_STAGES				= QVLD_EXTRA_FLOP_STAGES;
defparam memphy_top_inst.QVLD_WR_ADDRESS_OFFSET				= QVLD_WR_ADDRESS_OFFSET;
defparam memphy_top_inst.NUM_WRITE_PATH_FLOP_STAGES			= NUM_WRITE_PATH_FLOP_STAGES;
defparam memphy_top_inst.DELAY_PER_OPA_TAP 					= DELAY_PER_OPA_TAP;
defparam memphy_top_inst.DELAY_PER_DCHAIN_TAP 				= DELAY_PER_DCHAIN_TAP;
defparam memphy_top_inst.DLL_DELAY_CHAIN_LENGTH 			= DLL_DELAY_CHAIN_LENGTH;
defparam memphy_top_inst.MEM_NUMBER_OF_RANKS = MEM_NUMBER_OF_RANKS;
defparam memphy_top_inst.MEM_MIRROR_ADDRESSING = MEM_MIRROR_ADDRESSING;
defparam memphy_top_inst.DLL_OFFSET_CTRL_WIDTH              = DLL_OFFSET_CTRL_WIDTH;
defparam memphy_top_inst.PLL_PHASE_COUNTER_WIDTH            = PLL_PHASE_COUNTER_WIDTH;
defparam memphy_top_inst.ALTDQDQS_INPUT_FREQ = ALTDQDQS_INPUT_FREQ;
defparam memphy_top_inst.ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = ALTDQDQS_DELAY_CHAIN_BUFFER_MODE;
defparam memphy_top_inst.ALTDQDQS_DQS_PHASE_SETTING = ALTDQDQS_DQS_PHASE_SETTING;
defparam memphy_top_inst.ALTDQDQS_DQS_PHASE_SHIFT = ALTDQDQS_DQS_PHASE_SHIFT;
defparam memphy_top_inst.ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING;
defparam memphy_top_inst.AFI_DEBUG_INFO_WIDTH = AFI_DEBUG_INFO_WIDTH;

defparam memphy_top_inst.CSR_ADDR_WIDTH = CSR_ADDR_WIDTH;
defparam memphy_top_inst.CSR_DATA_WIDTH = CSR_DATA_WIDTH;
defparam memphy_top_inst.CSR_BE_WIDTH = CSR_BE_WIDTH;



generate
begin
	if (MEM_TYPE == "DDR3" && CTL_REGDIMM_ENABLED == 1 && AFI_CS_WIDTH == 4 && CTL_CS_WIDTH == 1) begin
		assign afi_cs_n[2] = ctl_cs_n[1];
		assign afi_cs_n[0] = ctl_cs_n[0];
		assign afi_cs_n[3] = 1'b1;
		assign afi_cs_n[1] = 1'b1;
	end else begin
		assign afi_cs_n = ctl_cs_n;
	end
end
endgenerate


endmodule

