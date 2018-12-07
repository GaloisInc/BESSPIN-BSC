//USER ******************************************************************************************************************************** 
//USER File name: sequencer.sv
//USER The sequencer is responsible for intercepting the AFI interface during the initialization and calibration stages
//USER During initialization stage, the sequencer executes a sequence according to the memory device spec
//USER There are 2 steps in the calibration stage:
//USER 1. Calibrates for read data valid in the returned memory clock domain (read valid prediction)
//USER 2. Calibrates for read data valid in the afi_clk domain (read latency calibration)
//USER After successful calibration, the sequencer will pass full control back to the AFI interface
//USER ******************************************************************************************************************************** 

module ddr2_v10_1_nios_sequencer(
	pll_config_clk,
	pll_avl_clk,
	
	reset_n_avl_clk,
	reset_n_scc_clk,	
	scc_data,
	scc_upd,
	scc_dq_ena,
	scc_dqs_ena,
	scc_dqs_io_ena,
	scc_dm_ena,
	
	pll_afi_clk,
	reset_n,
	seq_mux_address,
	seq_mux_bank,
	seq_mux_cs_n,
	seq_mux_cke,
	seq_mux_odt,
	seq_mux_ras_n,
	seq_mux_cas_n,
	seq_mux_we_n,
	seq_mux_wdata,
	seq_mux_wdata_valid,
	seq_mux_dqs_en,
	seq_mux_dm,
	seq_mux_rdata_en,
	mux_seq_rdata,
	mux_seq_read_fifo_q,
	mux_seq_rdata_valid,
	mux_sel,
	seq_read_latency_counter,
	seq_read_increment_vfifo_fr,
	seq_read_increment_vfifo_hr,
	afi_rlat,
	afi_wlat,



	afi_cal_success,
	afi_cal_fail,
	afi_cal_debug_info,
	seq_reset_mem_stable,
	seq_read_fifo_reset,
	seq_calib_init
);

//USER ******************************************************************************************************************************** 
//USER BEGIN PARAMETER SECTION
//USER All parameters default to "" will have their values passed in from higher level wrapper with the controller and driver 


//USER PHY-Memory Interface
//USER Memory device specific parameters, they are set according to the memory spec
parameter MEM_ADDRESS_WIDTH     = ""; 
parameter MEM_BANK_WIDTH        = ""; 
parameter MEM_CHIP_SELECT_WIDTH = ""; 
parameter MEM_CLK_EN_WIDTH 		= ""; 
parameter MEM_ODT_WIDTH			= ""; 
parameter MEM_DM_WIDTH          = ""; 
parameter MEM_CONTROL_WIDTH     = ""; 
parameter MEM_DQ_WIDTH          = ""; 
parameter MEM_READ_DQS_WIDTH    = ""; 
parameter MEM_WRITE_DQS_WIDTH   = "";
parameter DELAY_PER_OPA_TAP   = "";
parameter DELAY_PER_DCHAIN_TAP   = "";
parameter DLL_DELAY_CHAIN_LENGTH 	= "";

parameter MEM_NUMBER_OF_RANKS = "";
parameter MEM_MIRROR_ADDRESSING = "";


//USER PHY-Controller (AFI) Interface
//USER The AFI interface widths are derived from the memory interface widths based on full/half rate operations
//USER The calculations are done on higher level wrapper
parameter AFI_ADDRESS_WIDTH         = ""; 
parameter AFI_DEBUG_INFO_WIDTH = "";
parameter AFI_BANK_WIDTH            = ""; 
parameter AFI_CHIP_SELECT_WIDTH     = ""; 
parameter AFI_CLK_EN_WIDTH 			= ""; 
parameter AFI_ODT_WIDTH				= ""; 
parameter AFI_MAX_WRITE_LATENCY_COUNT_WIDTH	= "";
parameter AFI_MAX_READ_LATENCY_COUNT_WIDTH	= "";
parameter AFI_DATA_MASK_WIDTH       = ""; 
parameter AFI_CONTROL_WIDTH         = ""; 
parameter AFI_DATA_WIDTH            = ""; 
parameter AFI_DQS_WIDTH				= "";



//USER Read Datapath
parameter MAX_LATENCY_COUNT_WIDTH       = "";	//USER calibration finds the best latency by reducing the maximum latency  
parameter MAX_READ_LATENCY              = ""; 
parameter READ_VALID_TIMEOUT_WIDTH		= ""; 
parameter READ_VALID_FIFO_SIZE			= "";

//USER Write Datapath
//USER The sequencer uses this value to control write latency during calibration
parameter MAX_WRITE_LATENCY_COUNT_WIDTH = "";

//USER Initialization Sequence
parameter INIT_COUNT_WIDTH		= "";
parameter MEM_TINIT_CK = "";
parameter MEM_TMRD_CK = "";
parameter INIT_NOP_COUNT_WIDTH	= 8;
parameter MRD_COUNT_WIDTH		= 2;
parameter MR0_BL				= "";
parameter MR0_BT				= "";
parameter MR0_CAS_LATENCY		= "";
parameter MR0_WR				= "";
parameter MR0_PD				= "";
parameter MR1_DLL				= "";
parameter MR1_ODS				= "";
parameter MR1_RTT				= "";
parameter MR1_AL				= "";
parameter MR1_QOFF				= "";
parameter RDIMM					= "";
parameter RP_COUNT_WIDTH		= 3;
parameter RFC_COUNT_WIDTH		= 8;
parameter OIT_COUNT_WIDTH 		= 3;
parameter MR1_DQS				= "";
parameter MR1_RDQS				= "";
//FIXME: This should really be SRT but for now I'll keep it as SRF to avoid
//changing it everywhere else.
parameter MR2_SRF				= "";
parameter MEM_BURST_LENGTH      = "";
parameter MEM_T_WL              = "";
parameter MEM_T_RL				= "";

//USER The sequencer issues back-to-back reads during calibration, NOPs may need to be inserted depending on the burst length
parameter SEQ_BURST_COUNT_WIDTH = "";

//USER Width of the counter used to determine the number of cycles required
//USER to calculate if the rddata pattern is all 0 or all 1.
parameter VCALIB_COUNT_WIDTH    = "";

//USER Width of the calibration status register used to control calibration skipping.
parameter CALIB_REG_WIDTH		= "";

//USER local parameters
localparam AFI_DQ_GROUP_DATA_WIDTH = AFI_DATA_WIDTH / MEM_READ_DQS_WIDTH;

//USER The default VFIFO and LFIFO settings used in skip calibration mode
localparam VFIFO_OFFSET			= 4;
localparam CALIB_VFIFO_OFFSET	= VFIFO_OFFSET + MEM_T_RL + 4;
localparam CALIB_LFIFO_OFFSET	= (MEM_T_RL + 5) / 2;


//USER END PARAMETER SECTION
//USER ******************************************************************************************************************************** 


//USER ******************************************************************************************************************************** 
//USER BEGIN PORT SECTION

input	pll_config_clk;
input	pll_avl_clk;

input	reset_n_avl_clk;
input	reset_n_scc_clk;
output	scc_data;
output	scc_upd;
output	[MEM_DQ_WIDTH-1:0] scc_dq_ena;
output	[MEM_READ_DQS_WIDTH-1:0] scc_dqs_ena;
output	[MEM_READ_DQS_WIDTH-1:0] scc_dqs_io_ena;
output	[MEM_DM_WIDTH-1:0] scc_dm_ena;

input	pll_afi_clk;
input	reset_n;


//USER sequencer version of the AFI interface
output	[AFI_ADDRESS_WIDTH-1:0] seq_mux_address;
output	[AFI_BANK_WIDTH-1:0]    seq_mux_bank;
output	[AFI_CHIP_SELECT_WIDTH-1:0] seq_mux_cs_n;
output	[AFI_CLK_EN_WIDTH-1:0] seq_mux_cke;
output	[AFI_ODT_WIDTH-1:0] seq_mux_odt;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_ras_n;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_cas_n;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_we_n;

output  [AFI_DATA_WIDTH-1:0]    seq_mux_wdata;
output  [AFI_DQS_WIDTH-1:0]		seq_mux_wdata_valid;
output	[AFI_DQS_WIDTH-1:0] 	seq_mux_dqs_en;
output  [AFI_DATA_MASK_WIDTH-1:0]   seq_mux_dm;

output  seq_mux_rdata_en;

//USER signals between the sequencer and the read datapath
input	[AFI_DATA_WIDTH-1:0]    mux_seq_rdata;	//USER read data from read datapath, thru sequencer, back to AFI
input	mux_seq_rdata_valid; //USER read data valid from read datapath, thru sequencer, back to AFI

//USER read data (no reordering) for indepedently FIFO calibrations (multiple FIFOs for multiple DQS groups)
input	[AFI_DATA_WIDTH-1:0]	mux_seq_read_fifo_q; 

output	mux_sel;

//USER sequencer outputs to controller AFI interface
output  [AFI_MAX_WRITE_LATENCY_COUNT_WIDTH-1:0] afi_wlat;
output  [AFI_MAX_READ_LATENCY_COUNT_WIDTH-1:0]  afi_rlat;
output	afi_cal_success;
output [AFI_DEBUG_INFO_WIDTH - 1:0] afi_cal_debug_info;
output	afi_cal_fail;




//USER hold reset in the read capture clock domain until memory is stable
output	seq_reset_mem_stable;

//USER reset the read and write pointers of the data resynchronization FIFO in the read datapath 
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_fifo_reset;

//USER read latency counter value from sequencer to inform read datapath when valid data should be read
output	[MAX_LATENCY_COUNT_WIDTH-1:0] seq_read_latency_counter;

//USER controls from sequencer to read datapath to calibration the valid prediction FIFO pointer offsets
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_fr; //USER increment valid prediction FIFO write pointer by an extra full rate cycle	
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_hr; //USER increment valid prediction FIFO write pointer by an extra half rate cycle
															  //USER in full rate core, both will mean an extra full rate cycle

input	[CALIB_REG_WIDTH-1:0] seq_calib_init;

wire avlt_clk_from_the_sequencer_bridge_0;
wire avlt_reset_n_from_the_sequencer_bridge_0;
wire [15:0] avlt_address_from_the_sequencer_bridge_0;
wire avlt_read_from_the_sequencer_bridge_0;
wire avlt_write_from_the_sequencer_bridge_0;
wire [31:0] avlt_writedata_from_the_sequencer_bridge_0;

wire [31:0] avlt_readdata_to_the_sequencer_bridge_0;
wire avlt_waitrequest_to_the_sequencer_bridge_0;
wire [31:0] avlt_readdata_from_phy_mgr;
wire avlt_waitrequest_from_phy_mgr;
wire [31:0] avlt_readdata_from_ptr_mgr;
wire avlt_waitrequest_from_ptr_mgr;
wire [31:0] avlt_readdata_from_rw_mgr;
wire avlt_waitrequest_from_rw_mgr;
wire [31:0] avlt_readdata_from_scc_mgr;
wire avlt_waitrequest_from_scc_mgr;


	ddr2_v10_1_sequencer_ctrl sequencer_ctrl_inst (
		.avlt_address_from_the_sequencer_bridge_0   (avlt_address_from_the_sequencer_bridge_0),
		.avlt_clk_from_the_sequencer_bridge_0       (avlt_clk_from_the_sequencer_bridge_0),
		.avlt_read_from_the_sequencer_bridge_0      (avlt_read_from_the_sequencer_bridge_0),
		.avlt_readdata_to_the_sequencer_bridge_0    (avlt_readdata_to_the_sequencer_bridge_0),
		.avlt_reset_n_from_the_sequencer_bridge_0   (avlt_reset_n_from_the_sequencer_bridge_0),
		.avlt_waitrequest_to_the_sequencer_bridge_0 (avlt_waitrequest_to_the_sequencer_bridge_0),
		.avlt_write_from_the_sequencer_bridge_0     (avlt_write_from_the_sequencer_bridge_0),
		.avlt_writedata_from_the_sequencer_bridge_0 (avlt_writedata_from_the_sequencer_bridge_0),

		.init_to_the_rom_bridge_0(1'b0),
		.rom_data_ready_to_the_rom_bridge_0(1'b0),
		.rom_write_clock(pll_avl_clk),
		.clk_0                                      (pll_avl_clk),
		.reset_n                                    (reset_n_avl_clk)
    );

	//USER bus merge!
	
	assign avlt_readdata_to_the_sequencer_bridge_0 = 
		avlt_readdata_from_phy_mgr |
		avlt_readdata_from_ptr_mgr |
		avlt_readdata_from_rw_mgr |
		avlt_readdata_from_scc_mgr;
	assign avlt_waitrequest_to_the_sequencer_bridge_0 = 
		avlt_waitrequest_from_phy_mgr |
		avlt_waitrequest_from_ptr_mgr |
		avlt_waitrequest_from_rw_mgr |
		avlt_waitrequest_from_scc_mgr;
	
	ddr2_v10_1_sequencer_phy_mgr sequencer_phy_mgr_inst (
		.avl_clk (avlt_clk_from_the_sequencer_bridge_0),
		.avl_reset_n (avlt_reset_n_from_the_sequencer_bridge_0),
		.avl_address (avlt_address_from_the_sequencer_bridge_0),
		.avl_write (avlt_write_from_the_sequencer_bridge_0),
		.avl_writedata (avlt_writedata_from_the_sequencer_bridge_0),
		.avl_read (avlt_read_from_the_sequencer_bridge_0),
		.avl_readdata (avlt_readdata_from_phy_mgr),
		.avl_waitrequest (avlt_waitrequest_from_phy_mgr),

		//USER PHY control

		.phy_clk (pll_afi_clk),
		.phy_reset_n (reset_n),
		.phy_read_latency_counter (seq_read_latency_counter),
		.phy_read_increment_vfifo_fr (seq_read_increment_vfifo_fr),
		.phy_read_increment_vfifo_hr (seq_read_increment_vfifo_hr),
		.phy_reset_mem_stable (seq_reset_mem_stable),
		.phy_afi_wlat (afi_wlat),
		.phy_afi_rlat (afi_rlat),
		.phy_mux_sel (mux_sel),
		.phy_cal_success (afi_cal_success),
		.phy_cal_fail (afi_cal_fail),
		.phy_cal_debug_info (afi_cal_debug_info),
		.phy_read_fifo_reset (seq_read_fifo_reset),

		.calib_skip_steps (seq_calib_init)
	);
	defparam sequencer_phy_mgr_inst.MAX_LATENCY_COUNT_WIDTH = MAX_LATENCY_COUNT_WIDTH;
	defparam sequencer_phy_mgr_inst.MEM_READ_DQS_WIDTH = MEM_READ_DQS_WIDTH;
	defparam sequencer_phy_mgr_inst.AFI_DEBUG_INFO_WIDTH = AFI_DEBUG_INFO_WIDTH;
	defparam sequencer_phy_mgr_inst.AFI_MAX_WRITE_LATENCY_COUNT_WIDTH = AFI_MAX_WRITE_LATENCY_COUNT_WIDTH;
	defparam sequencer_phy_mgr_inst.AFI_MAX_READ_LATENCY_COUNT_WIDTH = AFI_MAX_READ_LATENCY_COUNT_WIDTH;
	defparam sequencer_phy_mgr_inst.CALIB_VFIFO_OFFSET = CALIB_VFIFO_OFFSET;
	defparam sequencer_phy_mgr_inst.CALIB_LFIFO_OFFSET = CALIB_LFIFO_OFFSET;
	defparam sequencer_phy_mgr_inst.CALIB_SKIP_STEPS_WIDTH = CALIB_REG_WIDTH;
	defparam sequencer_phy_mgr_inst.READ_VALID_FIFO_SIZE = READ_VALID_FIFO_SIZE;
	defparam sequencer_phy_mgr_inst.MEM_T_WL = MEM_T_WL;
	defparam sequencer_phy_mgr_inst.MEM_T_RL = MEM_T_RL;
	
	ddr2_v10_1_sequencer_ptr_mgr sequencer_ptr_mgr_inst (
		.avl_clk (avlt_clk_from_the_sequencer_bridge_0),
		.avl_reset_n (avlt_reset_n_from_the_sequencer_bridge_0),
		.avl_address (avlt_address_from_the_sequencer_bridge_0),
		.avl_write (avlt_write_from_the_sequencer_bridge_0),
		.avl_writedata (avlt_writedata_from_the_sequencer_bridge_0),
		.avl_read (avlt_read_from_the_sequencer_bridge_0),
		.avl_readdata (avlt_readdata_from_ptr_mgr),
		.avl_waitrequest (avlt_waitrequest_from_ptr_mgr)
	);

	ddr2_v10_1_sequencer_rw_mgr sequencer_rw_mgr_inst (
		// Avalon Interface
		.avl_clk (avlt_clk_from_the_sequencer_bridge_0),
		.avl_reset_n (avlt_reset_n_from_the_sequencer_bridge_0),
		.avl_address (avlt_address_from_the_sequencer_bridge_0),
		.avl_write (avlt_write_from_the_sequencer_bridge_0),
		.avl_writedata (avlt_writedata_from_the_sequencer_bridge_0),
		.avl_read (avlt_read_from_the_sequencer_bridge_0),
		.avl_readdata (avlt_readdata_from_rw_mgr),
		.avl_waitrequest (avlt_waitrequest_from_rw_mgr),

		// AFI Interface
		.afi_clk (pll_afi_clk),
		.afi_reset_n (reset_n),
		.afi_address (seq_mux_address),



		.afi_bank (seq_mux_bank),
		.afi_cs_n (seq_mux_cs_n),
		.afi_cke (seq_mux_cke),
		.afi_odt (seq_mux_odt),
		.afi_ras_n (seq_mux_ras_n),
		.afi_cas_n (seq_mux_cas_n),
		.afi_we_n (seq_mux_we_n),
		.afi_dqs_en (seq_mux_dqs_en),

		.afi_wdata (seq_mux_wdata),
		.afi_wdata_valid (seq_mux_wdata_valid),
		.afi_dm (seq_mux_dm),
		.afi_rdata_en (seq_mux_rdata_en),
		.afi_rdata (mux_seq_rdata),
		.afi_rdata_valid (mux_seq_rdata_valid)
	);

	//USER common memory parameters

	defparam sequencer_rw_mgr_inst.AFI_ADDRESS_WIDTH = AFI_ADDRESS_WIDTH;
	defparam sequencer_rw_mgr_inst.AFI_CONTROL_WIDTH = AFI_CONTROL_WIDTH;
	defparam sequencer_rw_mgr_inst.AFI_DATA_WIDTH = AFI_DATA_WIDTH;
	defparam sequencer_rw_mgr_inst.AFI_DATA_MASK_WIDTH = AFI_DATA_MASK_WIDTH;
	defparam sequencer_rw_mgr_inst.AFI_DQS_WIDTH = AFI_DQS_WIDTH;
	defparam sequencer_rw_mgr_inst.MEM_IF_READ_DQS_WIDTH = MEM_READ_DQS_WIDTH;
	defparam sequencer_rw_mgr_inst.MEM_IF_WRITE_DQS_WIDTH = MEM_WRITE_DQS_WIDTH;

	defparam sequencer_rw_mgr_inst.MEM_NUMBER_OF_RANKS = MEM_NUMBER_OF_RANKS;
	defparam sequencer_rw_mgr_inst.MEM_MIRROR_ADDRESSING = MEM_MIRROR_ADDRESSING;

	//USER memory specific parameters


	defparam sequencer_rw_mgr_inst.AFI_BANK_WIDTH = AFI_BANK_WIDTH; 
	defparam sequencer_rw_mgr_inst.AFI_CHIP_SELECT_WIDTH = AFI_CHIP_SELECT_WIDTH;
	defparam sequencer_rw_mgr_inst.AFI_CLK_EN_WIDTH = AFI_CLK_EN_WIDTH; 
	defparam sequencer_rw_mgr_inst.AFI_ODT_WIDTH = AFI_ODT_WIDTH; 
	defparam sequencer_rw_mgr_inst.INIT_NOP_COUNT_WIDTH = INIT_NOP_COUNT_WIDTH;
	defparam sequencer_rw_mgr_inst.MRD_COUNT_WIDTH = MRD_COUNT_WIDTH;
	defparam sequencer_rw_mgr_inst.MEM_TINIT_CK = MEM_TINIT_CK;
	defparam sequencer_rw_mgr_inst.MEM_TMRD_CK = MEM_TMRD_CK;
	defparam sequencer_rw_mgr_inst.MR0_BL = MR0_BL;
	defparam sequencer_rw_mgr_inst.MR0_BT = MR0_BT;
	defparam sequencer_rw_mgr_inst.MR0_CAS_LATENCY = MR0_CAS_LATENCY;
	defparam sequencer_rw_mgr_inst.MR0_WR = MR0_WR;
	defparam sequencer_rw_mgr_inst.MR0_PD = MR0_PD;
	defparam sequencer_rw_mgr_inst.MR1_DLL = MR1_DLL;
	defparam sequencer_rw_mgr_inst.MR1_ODS = MR1_ODS;
	defparam sequencer_rw_mgr_inst.MR1_RTT = MR1_RTT;
	defparam sequencer_rw_mgr_inst.MR1_AL = MR1_AL;
	defparam sequencer_rw_mgr_inst.MR1_QOFF = MR1_QOFF;
	defparam sequencer_rw_mgr_inst.MEM_CHIP_SELECT_WIDTH = MEM_CHIP_SELECT_WIDTH;
	defparam sequencer_rw_mgr_inst.RDIMM = RDIMM;
	defparam sequencer_rw_mgr_inst.RP_COUNT_WIDTH = RP_COUNT_WIDTH;
	defparam sequencer_rw_mgr_inst.RFC_COUNT_WIDTH = RFC_COUNT_WIDTH;
	defparam sequencer_rw_mgr_inst.OIT_COUNT_WIDTH = OIT_COUNT_WIDTH;
	defparam sequencer_rw_mgr_inst.MR1_DQS = MR1_DQS;
	defparam sequencer_rw_mgr_inst.MR1_RDQS = MR1_RDQS;
	defparam sequencer_rw_mgr_inst.MR2_SRT = MR2_SRF;

	ddr2_v10_1_sequencer_scc_mgr sequencer_scc_mgr_inst (
		.avl_clk (avlt_clk_from_the_sequencer_bridge_0),
		.avl_reset_n (avlt_reset_n_from_the_sequencer_bridge_0),
		.avl_address (avlt_address_from_the_sequencer_bridge_0),
		.avl_write (avlt_write_from_the_sequencer_bridge_0),
		.avl_writedata (avlt_writedata_from_the_sequencer_bridge_0),
		.avl_read (avlt_read_from_the_sequencer_bridge_0),
		.avl_readdata (avlt_readdata_from_scc_mgr),
		.avl_waitrequest (avlt_waitrequest_from_scc_mgr),

		.reset_n_scc_clk(reset_n_scc_clk),
		.scc_clk (pll_config_clk),
		.scc_data (scc_data),
		.scc_upd (scc_upd),
		.scc_dq_ena (scc_dq_ena),
		.scc_dqs_ena (scc_dqs_ena),
		.scc_dqs_io_ena (scc_dqs_io_ena),
		.scc_dm_ena (scc_dm_ena)
	);

	defparam sequencer_scc_mgr_inst.MEM_DQS_WIDTH = MEM_READ_DQS_WIDTH; 
	defparam sequencer_scc_mgr_inst.MEM_DQ_WIDTH = MEM_DQ_WIDTH;
	defparam sequencer_scc_mgr_inst.MEM_DM_WIDTH = MEM_DM_WIDTH;
	defparam sequencer_scc_mgr_inst.DELAY_PER_OPA_TAP = DELAY_PER_OPA_TAP;
	defparam sequencer_scc_mgr_inst.DELAY_PER_DCHAIN_TAP = DELAY_PER_DCHAIN_TAP;
	defparam sequencer_scc_mgr_inst.DLL_DELAY_CHAIN_LENGTH = DLL_DELAY_CHAIN_LENGTH;
endmodule
