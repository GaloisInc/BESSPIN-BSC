// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// ******************************************************************************************************************************** 
// File name: memphy_top.v
// This is the top level file of UNIPHY, which instantiates the DLL, PLL and PHY.
// ******************************************************************************************************************************** 
(* altera_attribute = "-name ALLOW_SYNCH_CTRL_USAGE OFF;-name AUTO_CLOCK_ENABLE_RECOGNITION OFF;-name AUTO_SHIFT_REGISTER_RECOGNITION OFF" *)
module ddr2_v10_1_0002_memphy_top(
    global_reset_n,
    soft_reset_n,
	reset_request_n,
	ctl_reset_n,
	oct_rdn,
	oct_rup,
    parallelterminationcontrol,
    seriesterminationcontrol,
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
	afi_clk,
	afi_half_clk,
	afi_addr,
	afi_ba,
	afi_cas_n,
	afi_cke,
	afi_cs_n,
	afi_odt,
	afi_ras_n,
	afi_we_n,
	afi_mem_clk_disable,
	afi_dqs_burst,
	afi_wlat,
	afi_rlat,
    afi_wdata,
    afi_wdata_valid,
    afi_dm,
    afi_rdata,
    afi_rdata_en,
    afi_rdata_en_full,
    afi_rdata_valid,
	afi_cal_debug_info,
    afi_cal_success,
    afi_cal_fail,
    csr_addr,
    csr_be,
    csr_rdata,
    csr_read_req,
    csr_wdata,
    csr_write_req,
    csr_rdata_valid,
    csr_waitrequest,
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
	mem_dqs_n
);


// ******************************************************************************************************************************** 
// BEGIN PARAMETER SECTION
// All parameters default to "" will have their values passed in from higher level wrapper with the controller and driver. 
parameter DEVICE_FAMILY = "StratixIII";

// On-chip termination
parameter OCT_SERIES_TERM_CONTROL_WIDTH   = 14;
parameter OCT_PARALLEL_TERM_CONTROL_WIDTH = 14;

// PHY-Memory Interface
// Memory device specific parameters, they are set according to the memory spec.
parameter MEM_ADDRESS_WIDTH		= 16;
parameter MEM_DQS_WIDTH			= 8;
parameter MEM_BANK_WIDTH        = 3;
parameter MEM_CHIP_SELECT_WIDTH = 2;
parameter MEM_CLK_EN_WIDTH		= 2;
parameter MEM_CK_WIDTH			= 2;
parameter MEM_ODT_WIDTH			= 2;
parameter MEM_DM_WIDTH         	= 8;
parameter MEM_CONTROL_WIDTH    	= 1; 
parameter MEM_DQ_WIDTH         	= 64;
parameter MEM_READ_DQS_WIDTH   	= 8; 
parameter MEM_WRITE_DQS_WIDTH  	= 8;

// PHY-Controller (AFI) Interface
// The AFI interface widths are derived from the memory interface widths based on full/half rate operations.
// The calculations are done on higher level wrapper.
parameter AFI_ADDRESS_WIDTH         = 32; 
parameter AFI_BANK_WIDTH            = 6;
parameter AFI_CHIP_SELECT_WIDTH     = 4;
parameter AFI_CLK_EN_WIDTH			= 4;
parameter AFI_ODT_WIDTH				= 4;
parameter AFI_MAX_WRITE_LATENCY_COUNT_WIDTH	= 5;
parameter AFI_MAX_READ_LATENCY_COUNT_WIDTH = 5;
parameter AFI_DATA_MASK_WIDTH       = 32; 
parameter AFI_CONTROL_WIDTH         = 2; 
parameter AFI_DATA_WIDTH            = 256; 
parameter AFI_DQS_WIDTH				= 16;

// DLL Interface
parameter DLL_DELAY_CTRL_WIDTH	= 6;

parameter NUM_SUBGROUP_PER_READ_DQS        = 1;
parameter QVLD_EXTRA_FLOP_STAGES		   = 0;
parameter QVLD_WR_ADDRESS_OFFSET		   = 3;
	
// Read Datapath parameters, the values should not be changed unless the intention is to change the architecture.
// Read valid prediction FIFO
parameter READ_VALID_FIFO_WRITE_ADDR_WIDTH = 3; // write operates on half rate clock
parameter READ_VALID_FIFO_READ_ADDR_WIDTH  = 3; // read operates on half rate clock
parameter READ_VALID_FIFO_SIZE             = 16;

// Data resynchronization FIFO
parameter READ_FIFO_SIZE				= 8;
parameter READ_FIFO_WRITE_MEM_DEPTH		= READ_FIFO_SIZE / 2; // data is written on half rate clock
parameter READ_FIFO_READ_MEM_DEPTH		= READ_FIFO_SIZE / 2; // data is read out on half rate clock
parameter READ_FIFO_WRITE_ADDR_WIDTH	= ceil_log2(READ_FIFO_WRITE_MEM_DEPTH);
parameter READ_FIFO_READ_ADDR_WIDTH		= ceil_log2(READ_FIFO_READ_MEM_DEPTH);

// Read valid prediction parameters
parameter READ_VALID_TIMEOUT_WIDTH		   = 8; // calibration fails when the timeout counter expires 

// Latency calibration parameters
parameter MAX_LATENCY_COUNT_WIDTH		   = 5; // calibration finds the best latency by reducing the maximum latency
parameter MAX_READ_LATENCY				   = 2**MAX_LATENCY_COUNT_WIDTH; 


// Write Datapath
// The sequencer uses this value to control write latency during calibration
parameter MAX_WRITE_LATENCY_COUNT_WIDTH = 4;
parameter NUM_WRITE_PATH_FLOP_STAGES    = 0;

// Initialization Sequence
// The init counter is used to maintain the stable condition wait time required by the memory protocol
parameter INIT_COUNT_WIDTH      = 17;      


parameter MEM_TINIT_CK							= 50000;
parameter MEM_TMRD_CK							= 2;
parameter RDIMM										= 0;
parameter MR0_BL									= 3;
parameter MR0_BT									= 0;
parameter MR0_CAS_LATENCY						= 4;
parameter MR0_WR									= 3;
parameter MR0_PD									= 0;
parameter MR1_DLL								= 0;
parameter MR1_ODS								= 0;
parameter MR1_RTT								= 0;
parameter MR1_AL									= 0;
parameter MR1_DQS								= 0;
parameter MR1_RDQS								= 0;
parameter MR1_QOFF								= 0;
parameter MR2_SRF								= 1;
parameter MEM_BURST_LENGTH						= 8;
parameter MEM_T_WL								= 2;
parameter MEM_T_RL								= 4;

// The sequencer issues back-to-back reads during calibration, NOPs may need to be inserted depending on the burst length
parameter SEQ_BURST_COUNT_WIDTH = 1;

// Width of the counter used to determine the number of cycles required
// to calculate if the rddata pattern is all 0 or all 1.
parameter VCALIB_COUNT_WIDTH = 2;

parameter DELAY_PER_OPA_TAP 					= 333;
parameter DELAY_PER_DCHAIN_TAP 				= 50;
parameter DLL_DELAY_CHAIN_LENGTH 			= 12;
parameter MEM_NUMBER_OF_RANKS 				= 2;
parameter MEM_MIRROR_ADDRESSING 				= 0;

// The DLL offset control width
parameter DLL_OFFSET_CTRL_WIDTH = 6;

// The PLL Phase counter width
parameter PLL_PHASE_COUNTER_WIDTH = 4;

parameter ALTDQDQS_INPUT_FREQ = 250.0;
parameter ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = "HIGH";
parameter ALTDQDQS_DQS_PHASE_SETTING = 3;
parameter ALTDQDQS_DQS_PHASE_SHIFT = 9000;
parameter ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = 2;
parameter AFI_DEBUG_INFO_WIDTH = 32;

// CSR Port parameters
parameter       CSR_ADDR_WIDTH                 = 16;
parameter       CSR_DATA_WIDTH                 = 32;
parameter       CSR_BE_WIDTH                   = 4;

// END PARAMETER SECTION
// ******************************************************************************************************************************** 


// ******************************************************************************************************************************** 
// BEGIN PORT SECTION


input	pll_ref_clk;		// PLL reference clock

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



// Reset Interface, AFI 2.0
input   global_reset_n;		// Resets (active-low) the whole system (all PHY logic + PLL)
input	soft_reset_n;		// Resets (active-low) PHY logic only, PLL is NOT reset
output	reset_request_n;	// When 1, PLL is out of lock
output	ctl_reset_n;		// Asynchronously asserted and synchronously de-asserted on afi_clk domain
							// should be used to reset system level afi_clk domain logic

// On-Chip Termination
// These should be connected to reference resistance pins on the board, via OCT control block if instantiated by user
input   oct_rdn;
input   oct_rup;
// for OCT master, termination control signals will be available to top level
output [OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] parallelterminationcontrol;
output [OCT_SERIES_TERM_CONTROL_WIDTH-1:0] seriesterminationcontrol;

// PHY-Controller Interface, AFI 2.0
// Control Interface
input   [AFI_ADDRESS_WIDTH-1:0] afi_addr;		// address



input   [AFI_BANK_WIDTH-1:0]	afi_ba;
input   [AFI_CONTROL_WIDTH-1:0]	afi_cas_n;
input   [AFI_CLK_EN_WIDTH-1:0]	afi_cke;
input   [AFI_CHIP_SELECT_WIDTH-1:0]	afi_cs_n;
input   [AFI_ODT_WIDTH-1:0]	afi_odt;
input   [AFI_CONTROL_WIDTH-1:0]	afi_ras_n;
input   [AFI_CONTROL_WIDTH-1:0]	afi_we_n;
input   afi_mem_clk_disable;
input   [AFI_DQS_WIDTH-1:0]	afi_dqs_burst;
output	[AFI_MAX_WRITE_LATENCY_COUNT_WIDTH-1:0]	afi_wlat;
output	[AFI_MAX_READ_LATENCY_COUNT_WIDTH-1:0]	afi_rlat;


// Write data interface
input   [AFI_DATA_WIDTH-1:0]    afi_wdata;				// write data
input	[AFI_DQS_WIDTH-1:0]	afi_wdata_valid;			// write data valid, used to maintain write latency required by protocol spec
input   [AFI_DATA_MASK_WIDTH-1:0]   afi_dm;				// write data mask

// Read data interface
output  [AFI_DATA_WIDTH-1:0]    afi_rdata;				// read data				
input   afi_rdata_en;		// read enable, used to maintain the read latency calibrated by PHY
input   afi_rdata_en_full;		// read enable full burst, used to create DQS enable
output  afi_rdata_valid;// read data valid

// Status interface
output [AFI_DEBUG_INFO_WIDTH - 1:0] afi_cal_debug_info;
output  afi_cal_success;	// calibration success
output  afi_cal_fail;		// calibration failure



// PHY-Memory Interface



output	[MEM_ADDRESS_WIDTH-1:0]	mem_a;
output  [MEM_BANK_WIDTH-1:0]	mem_ba;
output	[MEM_CK_WIDTH-1:0]	mem_ck;
output	[MEM_CK_WIDTH-1:0]	mem_ck_n;
output	[MEM_CLK_EN_WIDTH-1:0]	mem_cke;
output	[MEM_CHIP_SELECT_WIDTH-1:0]	mem_cs_n;
output	[MEM_DM_WIDTH-1:0]	mem_dm;
output	[MEM_ODT_WIDTH-1:0]	mem_odt;
output	[MEM_CONTROL_WIDTH-1:0]	mem_ras_n;
output	[MEM_CONTROL_WIDTH-1:0]	mem_cas_n;
output	[MEM_CONTROL_WIDTH-1:0]	mem_we_n;
inout   [MEM_DQ_WIDTH-1:0]  mem_dq;
inout	[MEM_DQS_WIDTH-1:0]	mem_dqs;
inout	[MEM_DQS_WIDTH-1:0]	mem_dqs_n;



// PLL Interface
output	afi_clk;
output	afi_half_clk;

wire	pll_dqs_ena_clk;
wire	seq_clk;


// CSR Port input/output
input    [CSR_ADDR_WIDTH - 1: 0]    csr_addr;
input    [CSR_BE_WIDTH - 1: 0]      csr_be;
input                               csr_read_req;
input    [CSR_DATA_WIDTH - 1: 0]    csr_wdata;
input                               csr_write_req;
output   [CSR_DATA_WIDTH - 1: 0]    csr_rdata;
output                              csr_rdata_valid;
output                              csr_waitrequest;


// END PARAMETER SECTION
// ******************************************************************************************************************************** 

wire   [OCT_SERIES_TERM_CONTROL_WIDTH-1:0] oct_ctl_rs_value;
wire   [OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] oct_ctl_rt_value;

	integer MEM_T_WL_int = (MEM_T_WL/2);
	assign afi_wlat = MEM_T_WL_int[AFI_MAX_WRITE_LATENCY_COUNT_WIDTH-1:0];


// Exporting read latency is currently not supported
assign afi_rlat = 0;

	ddr2_v10_1_0002_memphy	umemphy(
    	.global_reset_n			(global_reset_n),
		.soft_reset_n			(soft_reset_n),
		.reset_request_n		(reset_request_n),
		.ctl_reset_n			(ctl_reset_n),
		.pll_locked				(pll_locked),
        .oct_ctl_rs_value       (oct_ctl_rs_value),
        .oct_ctl_rt_value       (oct_ctl_rt_value),
    	.afi_addr				(afi_addr),
        .afi_ba		            (afi_ba),
        .afi_cs_n               (afi_cs_n),
        .afi_cke              	(afi_cke),
        .afi_odt              	(afi_odt),
        .afi_ras_n              (afi_ras_n),
        .afi_cas_n              (afi_cas_n),
        .afi_we_n               (afi_we_n),
		.afi_dqs_burst			(afi_dqs_burst),
    	.afi_wdata				(afi_wdata),
    	.afi_wdata_valid		(afi_wdata_valid),
    	.afi_dm					(afi_dm),
    	.afi_rdata				(afi_rdata),
    	.afi_rdata_en			(afi_rdata_en),
    	.afi_rdata_en_full		(afi_rdata_en_full),
    	.afi_rdata_valid		(afi_rdata_valid),
		.afi_cal_debug_info(afi_cal_debug_info),
    	.afi_cal_success		(afi_cal_success),
    	.afi_cal_fail			(afi_cal_fail),
    	.mem_a          		(mem_a),
    	.mem_ba         		(mem_ba),
    	.mem_ck         		(mem_ck),
    	.mem_ck_n       		(mem_ck_n),
    	.mem_cke        		(mem_cke),
    	.mem_cs_n       		(mem_cs_n),
    	.mem_dm         		(mem_dm),
    	.mem_odt        		(mem_odt),
    	.mem_ras_n      		(mem_ras_n),
    	.mem_cas_n      		(mem_cas_n),
    	.mem_we_n       		(mem_we_n),
    	.mem_dq         		(mem_dq),
    	.mem_dqs        		(mem_dqs),
    	.mem_dqs_n      		(mem_dqs_n),
    	.pll_afi_clk			(pll_afi_clk),
    	.pll_mem_clk			(pll_mem_clk),
    	.pll_write_clk			(pll_write_clk),
    	.pll_addr_cmd_clk		(pll_addr_cmd_clk),
		.pll_dqs_ena_clk		(pll_dqs_ena_clk),
    	.seq_clk				(seq_clk),
    .csr_write_req                  ( csr_write_req            ),
    .csr_read_req                   ( csr_read_req             ),
    .csr_addr                       ( csr_addr                 ),
    .csr_be                         ( csr_be                   ),
    .csr_wdata                      ( csr_wdata                ),
    .csr_waitrequest                ( csr_waitrequest          ),
    .csr_rdata                      ( csr_rdata                ),
    .csr_rdata_valid                ( csr_rdata_valid          ),
		.pll_avl_clk			(pll_avl_clk),
		.pll_config_clk			(pll_config_clk),
		.dll_phy_delayctrl		(dll_delayctrl)
	);

		defparam umemphy.DEVICE_FAMILY						= DEVICE_FAMILY;
		defparam umemphy.OCT_SERIES_TERM_CONTROL_WIDTH		= OCT_SERIES_TERM_CONTROL_WIDTH;
		defparam umemphy.OCT_PARALLEL_TERM_CONTROL_WIDTH	= OCT_PARALLEL_TERM_CONTROL_WIDTH;
		defparam umemphy.MEM_ADDRESS_WIDTH					= MEM_ADDRESS_WIDTH;
        defparam umemphy.MEM_BANK_WIDTH                     = MEM_BANK_WIDTH;
        defparam umemphy.MEM_CHIP_SELECT_WIDTH              = MEM_CHIP_SELECT_WIDTH;
        defparam umemphy.MEM_CLK_EN_WIDTH              		= MEM_CLK_EN_WIDTH;
        defparam umemphy.MEM_CK_WIDTH              			= MEM_CK_WIDTH;
        defparam umemphy.MEM_ODT_WIDTH              		= MEM_ODT_WIDTH;
		defparam umemphy.MEM_DQS_WIDTH						= MEM_DQS_WIDTH;
		defparam umemphy.MEM_DM_WIDTH						= MEM_DM_WIDTH;
		defparam umemphy.MEM_CONTROL_WIDTH					= MEM_CONTROL_WIDTH;
		defparam umemphy.MEM_DQ_WIDTH						= MEM_DQ_WIDTH;
		defparam umemphy.MEM_READ_DQS_WIDTH					= MEM_READ_DQS_WIDTH;
		defparam umemphy.MEM_WRITE_DQS_WIDTH				= MEM_WRITE_DQS_WIDTH;
		defparam umemphy.AFI_ADDRESS_WIDTH					= AFI_ADDRESS_WIDTH;
		defparam umemphy.AFI_BANK_WIDTH						= AFI_BANK_WIDTH;
		defparam umemphy.AFI_CHIP_SELECT_WIDTH				= AFI_CHIP_SELECT_WIDTH;
		defparam umemphy.AFI_CLK_EN_WIDTH					= AFI_CLK_EN_WIDTH;
		defparam umemphy.AFI_ODT_WIDTH						= AFI_ODT_WIDTH;
		defparam umemphy.AFI_MAX_WRITE_LATENCY_COUNT_WIDTH	= AFI_MAX_WRITE_LATENCY_COUNT_WIDTH;
		defparam umemphy.AFI_MAX_READ_LATENCY_COUNT_WIDTH	= AFI_MAX_READ_LATENCY_COUNT_WIDTH;
		defparam umemphy.AFI_DATA_MASK_WIDTH				= AFI_DATA_MASK_WIDTH;
		defparam umemphy.AFI_DQS_WIDTH						= AFI_DQS_WIDTH;
		defparam umemphy.AFI_CONTROL_WIDTH					= AFI_CONTROL_WIDTH;
		defparam umemphy.AFI_DATA_WIDTH						= AFI_DATA_WIDTH;
		defparam umemphy.DLL_DELAY_CTRL_WIDTH				= DLL_DELAY_CTRL_WIDTH;
		defparam umemphy.INIT_COUNT_WIDTH					= INIT_COUNT_WIDTH;
		defparam umemphy.MEM_BURST_LENGTH					= MEM_BURST_LENGTH;
		defparam umemphy.MEM_T_WL							= MEM_T_WL;
		defparam umemphy.MEM_T_RL							= MEM_T_RL;
		defparam umemphy.MEM_TINIT_CK                       = MEM_TINIT_CK;
		defparam umemphy.MEM_TMRD_CK                        = MEM_TMRD_CK;
		defparam umemphy.RDIMM                              = RDIMM;
		defparam umemphy.MR0_BL								= MR0_BL;
		defparam umemphy.MR0_BT								= MR0_BT;
		defparam umemphy.MR0_CAS_LATENCY					= MR0_CAS_LATENCY;
		defparam umemphy.MR0_WR								= MR0_WR;
		defparam umemphy.MR0_PD								= MR0_PD;
		defparam umemphy.MR1_DLL							= MR1_DLL;
		defparam umemphy.MR1_ODS							= MR1_ODS;
		defparam umemphy.MR1_RTT							= MR1_RTT;
		defparam umemphy.MR1_AL								= MR1_AL;
		defparam umemphy.MR1_DQS							= MR1_DQS;
		defparam umemphy.MR1_RDQS							= MR1_RDQS;
		defparam umemphy.MR1_QOFF							= MR1_QOFF;
		defparam umemphy.MR2_SRF							= MR2_SRF;
		defparam umemphy.CSR_ADDR_WIDTH                      = CSR_ADDR_WIDTH;
		defparam umemphy.CSR_DATA_WIDTH                      = CSR_DATA_WIDTH;
		defparam umemphy.CSR_BE_WIDTH                        = CSR_BE_WIDTH;
		defparam umemphy.READ_VALID_TIMEOUT_WIDTH			= READ_VALID_TIMEOUT_WIDTH;
		defparam umemphy.MAX_LATENCY_COUNT_WIDTH			= MAX_LATENCY_COUNT_WIDTH;	
		defparam umemphy.MAX_READ_LATENCY					= MAX_READ_LATENCY;
		defparam umemphy.READ_FIFO_READ_MEM_DEPTH			= READ_FIFO_READ_MEM_DEPTH;
		defparam umemphy.READ_FIFO_READ_ADDR_WIDTH			= READ_FIFO_READ_ADDR_WIDTH;
		defparam umemphy.READ_FIFO_WRITE_MEM_DEPTH			= READ_FIFO_WRITE_MEM_DEPTH;
		defparam umemphy.READ_FIFO_WRITE_ADDR_WIDTH			= READ_FIFO_WRITE_ADDR_WIDTH;
		defparam umemphy.READ_VALID_FIFO_READ_ADDR_WIDTH	= READ_VALID_FIFO_READ_ADDR_WIDTH;
		defparam umemphy.READ_VALID_FIFO_WRITE_ADDR_WIDTH	= READ_VALID_FIFO_WRITE_ADDR_WIDTH;
		defparam umemphy.READ_VALID_FIFO_SIZE				= READ_VALID_FIFO_SIZE;
		defparam umemphy.MAX_WRITE_LATENCY_COUNT_WIDTH		= MAX_WRITE_LATENCY_COUNT_WIDTH;
		defparam umemphy.NUM_WRITE_PATH_FLOP_STAGES			= NUM_WRITE_PATH_FLOP_STAGES;
		defparam umemphy.SEQ_BURST_COUNT_WIDTH				= SEQ_BURST_COUNT_WIDTH;
        defparam umemphy.VCALIB_COUNT_WIDTH                 = VCALIB_COUNT_WIDTH;
        defparam umemphy.NUM_SUBGROUP_PER_READ_DQS          = NUM_SUBGROUP_PER_READ_DQS;
		defparam umemphy.QVLD_EXTRA_FLOP_STAGES				= QVLD_EXTRA_FLOP_STAGES;
		defparam umemphy.QVLD_WR_ADDRESS_OFFSET				= QVLD_WR_ADDRESS_OFFSET;
		defparam umemphy.DELAY_PER_OPA_TAP 					= DELAY_PER_OPA_TAP;
		defparam umemphy.DELAY_PER_DCHAIN_TAP 				= DELAY_PER_DCHAIN_TAP;
		defparam umemphy.DLL_DELAY_CHAIN_LENGTH 			= DLL_DELAY_CHAIN_LENGTH;
		defparam umemphy.MEM_NUMBER_OF_RANKS = MEM_NUMBER_OF_RANKS;
		defparam umemphy.MEM_MIRROR_ADDRESSING = MEM_MIRROR_ADDRESSING;
		defparam umemphy.ALTDQDQS_INPUT_FREQ = ALTDQDQS_INPUT_FREQ;
		defparam umemphy.ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = ALTDQDQS_DELAY_CHAIN_BUFFER_MODE;
		defparam umemphy.ALTDQDQS_DQS_PHASE_SETTING = ALTDQDQS_DQS_PHASE_SETTING;
		defparam umemphy.ALTDQDQS_DQS_PHASE_SHIFT = ALTDQDQS_DQS_PHASE_SHIFT;
		defparam umemphy.ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING;
		defparam umemphy.AFI_DEBUG_INFO_WIDTH = AFI_DEBUG_INFO_WIDTH;
		defparam umemphy.TB_PROTOCOL       = "DDR2";
		defparam umemphy.TB_MEM_CLK_FREQ       = "250.0";
		defparam umemphy.TB_RATE       = "HALF_RATE";
		defparam umemphy.TB_MEM_DQ_WIDTH       = "64";
		defparam umemphy.TB_MEM_DQS_WIDTH       = "8";
		defparam umemphy.TB_PLL_DLL_MASTER       = "1";

	ddr2_v10_1_0002_dll_memphy  udll_memphy(
        .dll_clk            	(pll_mem_clk),
        .dll_delayctrlout   	(dll_delayctrl)
    );



	ddr2_v10_1_0002_pll_memphy	upll_memphy(
		.areset					(~global_reset_n), 
		.inclk0					(pll_ref_clk),
		.c0						(pll_afi_clk),
		.c1						(pll_mem_clk),
		.c2						(pll_write_clk),
		.c3						(pll_addr_cmd_clk),
		.c4                     (pll_afi_half_clk),
		.c5                     (pll_avl_clk),
		.c6                     (pll_config_clk),
		.locked					(pll_locked)
	);

	// Clock descriptions
	// pll_afi_clk: half-rate clock, 0 degree phase shift, clock for AFI interface logic
	// pll_mem_clk: full-rate clock, 0 degree phase shift, clock output to memory
	// pll_write_clk: full-rate clock, -90 degree phase shift, clocks write data out to memory
	// pll_addr_cmd_clk: half-rate clock, 270 degree phase shift, clocks address/command out to memory
	// pll_afi_half_clk: quad-rate clock, 0 degree phase shift
	// the purpose of these clock settings is so that address/command/write data are centred aligned with the output clock(s) to memory

	assign pll_dqs_ena_clk = pll_write_clk;
	assign seq_clk = pll_afi_clk;

	assign afi_clk = pll_afi_clk;
	assign afi_half_clk = pll_afi_half_clk;

	reg afi_half_clk_reg /* synthesis syn_noprune syn_preserve = 1 */;
	always @(posedge pll_afi_half_clk)
		afi_half_clk_reg <= ~afi_half_clk_reg;


	// On chip termination block
	ddr2_v10_1_0002_oct_control	uoct_control(
		.rdn                            (oct_rdn), // should be connected to the pull down resistance pin on the board
		.rup                           	(oct_rup), // should be connected to the pull up resistance pin on the board
        .parallelterminationcontrol     (oct_ctl_rt_value),
        .seriesterminationcontrol       (oct_ctl_rs_value)
	);
	
assign parallelterminationcontrol = oct_ctl_rt_value;
assign seriesterminationcontrol = oct_ctl_rs_value;

`ifdef ENABLE_ISS_PROBES
ddr2_v10_1_0002_iss_probe #(
	.WIDTH(1)
) afi_cal_success_probe (
	.probe_input(afi_cal_success)
);

ddr2_v10_1_0002_iss_probe #(
	.WIDTH(1)
) afi_cal_fail_probe (
	.probe_input(afi_cal_fail)
);

ddr2_v10_1_0002_iss_probe #(
    .WIDTH(AFI_DEBUG_INFO_WIDTH)
) afi_cal_debug_info_probe (
    .probe_input(afi_cal_debug_info)
);

`endif


// Calculate the ceiling of log_2 of the input value
function integer ceil_log2;
	input integer value;
	begin
		value = value - 1;
		for (ceil_log2 = 0; value > 0; ceil_log2 = ceil_log2 + 1)
			value = value >> 1;
	end
endfunction


endmodule

