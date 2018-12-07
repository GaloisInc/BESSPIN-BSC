// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// ******************************************************************************************************************************** 
// File name: sequencer.sv
// The sequencer is responsible for intercepting the AFI interface during the initialization and calibration stages
// During initialization stage, the sequencer executes a sequence according to the memory device spec
// There are 2 steps in the calibration stage:
// 1. Calibrates for read data valid in the returned memory clock domain (read valid prediction)
// 2. Calibrates for read data valid in the afi_clk domain (read latency calibration)
// After successful calibration, the sequencer will pass full control back to the AFI interface
// ******************************************************************************************************************************** 


module ddr2_v10_1_sequencer(
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
	seq_mux_vfifo_rd_en_override,
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
	seq_reset_mem_stable,
	seq_read_fifo_reset,
	seq_calib_init
);

// ******************************************************************************************************************************** 
// BEGIN PARAMETER SECTION
// All parameters default to "" will have their values passed in from higher level wrapper with the controller and driver 


// PHY-Memory Interface
// Memory device specific parameters, they are set according to the memory spec
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


// PHY-Controller (AFI) Interface
// The AFI interface widths are derived from the memory interface widths based on full/half rate operations
// The calculations are done on higher level wrapper
parameter AFI_ADDRESS_WIDTH         = ""; 
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

// Read Datapath
parameter MAX_LATENCY_COUNT_WIDTH       = "";	// calibration finds the best latency by reducing the maximum latency  
parameter MAX_READ_LATENCY              = ""; 
parameter READ_FIFO_READ_ADDR_WIDTH     = ""; 
parameter READ_FIFO_WRITE_ADDR_WIDTH    = ""; 
parameter READ_VALID_TIMEOUT_WIDTH		= ""; 

// Write Datapath
// The sequencer uses this value to control write latency during calibration
parameter MAX_WRITE_LATENCY_COUNT_WIDTH = "";

// Initialization Sequence
parameter INIT_COUNT_WIDTH		= "";
parameter INIT_NOP_COUNT_WIDTH	= 8;
parameter RP_COUNT_WIDTH		= 3;
parameter MRD_COUNT_WIDTH		= 2;
parameter RFC_COUNT_WIDTH		= 8;
parameter OIT_COUNT_WIDTH 		= 3;
parameter MR0_BL				= "";
parameter MR0_BT				= "";
parameter MR0_CAS_LATENCY		= "";
parameter MR0_WR				= "";
parameter MR0_PD				= "";
parameter MR1_DLL				= "";
parameter MR1_ODS				= "";
parameter MR1_RTT				= "";
parameter MR1_AL				= "";
parameter MR1_DQS				= "";
parameter MR1_RDQS				= "";
parameter MR1_QOFF				= "";
parameter MR2_SRF				= "";
parameter MEM_BURST_LENGTH      = "";
parameter MEM_T_WL              = "";
parameter MEM_T_RL              = "";

// The sequencer issues back-to-back reads during calibration, NOPs may need to be inserted depending on the burst length
parameter SEQ_BURST_COUNT_WIDTH = "";

// Width of the counter used to determine the number of cycles required
// to calculate if the rddata pattern is all 0 or all 1.
parameter VCALIB_COUNT_WIDTH    = "";

// Width of the calibration status register used to control calibration skipping.
parameter CALIB_REG_WIDTH		= "";

// local parameters
localparam AFI_DQ_GROUP_DATA_WIDTH = AFI_DATA_WIDTH / MEM_READ_DQS_WIDTH;

// END PARAMETER SECTION
// ******************************************************************************************************************************** 


// ******************************************************************************************************************************** 
// BEGIN PORT SECTION

input	pll_afi_clk;
input	reset_n;


// sequencer version of the AFI interface
output	[AFI_ADDRESS_WIDTH-1:0] seq_mux_address;
output	[AFI_BANK_WIDTH-1:0]    seq_mux_bank;
output	[AFI_CHIP_SELECT_WIDTH-1:0] seq_mux_cs_n;
output	[AFI_CLK_EN_WIDTH-1:0] seq_mux_cke;
output	[AFI_ODT_WIDTH-1:0] seq_mux_odt;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_ras_n;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_cas_n;
output	[AFI_CONTROL_WIDTH-1:0] seq_mux_we_n;

output  [AFI_DATA_WIDTH-1:0]    seq_mux_wdata;
output  [AFI_DQS_WIDTH-1:0]	seq_mux_wdata_valid;
output	[AFI_DQS_WIDTH-1:0]	seq_mux_dqs_en;
output	[MEM_READ_DQS_WIDTH-1:0] seq_mux_vfifo_rd_en_override;
output  [AFI_DATA_MASK_WIDTH-1:0]   seq_mux_dm;

output  seq_mux_rdata_en;

// signals between the sequencer and the read datapath
input	[AFI_DATA_WIDTH-1:0]    mux_seq_rdata;	// read data from read datapath, thru sequencer, back to AFI
input	mux_seq_rdata_valid; // read data valid from read datapath, thru sequencer, back to AFI

// read data (no reordering) for indepedently FIFO calibrations (multiple FIFOs for multiple DQS groups)
input	[AFI_DATA_WIDTH-1:0]	mux_seq_read_fifo_q; 

output	mux_sel;

// sequencer outputs to controller AFI interface
output  [AFI_MAX_WRITE_LATENCY_COUNT_WIDTH-1:0] afi_wlat;
output  [AFI_MAX_READ_LATENCY_COUNT_WIDTH-1:0]  afi_rlat;
output	afi_cal_success;
output	afi_cal_fail;


// hold reset in the read capture clock domain until memory is stable
output	seq_reset_mem_stable;

// reset the read and write pointers of the data resynchronization FIFO in the read datapath 
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_fifo_reset;

// read latency counter value from sequencer to inform read datapath when valid data should be read
output	[MAX_LATENCY_COUNT_WIDTH-1:0] seq_read_latency_counter;

// controls from sequencer to read datapath to calibration the valid prediction FIFO pointer offsets
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_fr; // increment valid prediction FIFO write pointer by an extra full rate cycle	
output	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_hr; // increment valid prediction FIFO write pointer by an extra half rate cycle
															  // in full rate core, both will mean an extra full rate cycle

input	[CALIB_REG_WIDTH-1:0] seq_calib_init;

reg	seq_reset_mem_stable;
reg	[MAX_LATENCY_COUNT_WIDTH-1:0] seq_read_latency_counter;

wire	[MEM_ADDRESS_WIDTH-1:0] seq_address;
wire	[MEM_BANK_WIDTH-1:0]    seq_bank;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] seq_cs_n;
wire	[MEM_CLK_EN_WIDTH-1:0] seq_cke;
wire	[MEM_ODT_WIDTH-1:0] seq_odt;
wire	[MEM_CONTROL_WIDTH-1:0] seq_ras_n;
wire	[MEM_CONTROL_WIDTH-1:0] seq_cas_n;
wire	[MEM_CONTROL_WIDTH-1:0] seq_we_n;
reg	[MEM_READ_DQS_WIDTH-1:0] seq_read_fifo_reset;

wire  [MEM_ADDRESS_WIDTH-1:0] seq_address_l;
wire	[MEM_BANK_WIDTH-1:0]    seq_bank_l;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] seq_cs_n_l;
wire	[MEM_CLK_EN_WIDTH-1:0] seq_cke_l;
wire	[MEM_ODT_WIDTH-1:0] seq_odt_l;
wire	[MEM_CONTROL_WIDTH-1:0] seq_ras_n_l;
wire	[MEM_CONTROL_WIDTH-1:0] seq_cas_n_l;
wire	[MEM_CONTROL_WIDTH-1:0] seq_we_n_l;

wire  [MEM_DQ_WIDTH-1:0]    seq_wrdata;
wire  seq_wrdata_en;
wire  seq_dqs_en;
wire  [AFI_DATA_MASK_WIDTH-1:0]   seq_wrdata_mask;


reg	[SEQ_BURST_COUNT_WIDTH-1:0]	burst_count;

reg	[MAX_WRITE_LATENCY_COUNT_WIDTH-1:0] write_latency_count;
reg	[READ_FIFO_READ_ADDR_WIDTH-1:0] read_flush_count;

reg [INIT_NOP_COUNT_WIDTH-1:0] nop_count;
reg [RP_COUNT_WIDTH-1:0] rp_count;
reg [MRD_COUNT_WIDTH-1:0] mrd_count;
reg [RFC_COUNT_WIDTH-1:0] rfc_count;
reg [OIT_COUNT_WIDTH-1:0] oit_count;

reg	[INIT_COUNT_WIDTH-1:0] mem_stable_count;



wire	write_states;
wire	read_states;
wire	seq_rddata_en;


wire	mrs_states;
wire	precharge_states;
wire	refresh_states;
wire	[2:0] mem_MR0_BL;
wire	mem_MR0_BT;
wire	[2:0] mem_MR0_CAS_LATENCY;
wire	[2:0] mem_MR0_WR;
wire	mem_MR0_PD;
wire	mem_MR1_DLL;
wire	mem_MR1_ODS;
wire	[1:0] mem_MR1_RTT;
wire	[2:0] mem_MR1_AL;
wire	mem_MR1_DQS;
wire	mem_MR1_RDQS;
wire	mem_MR1_QOFF;
wire	mem_MR2_SRF;
wire	[16:0] mr0;
wire	[16:0] mr1;
wire	[16:0] mr2;
wire	[16:0] mr3;
wire	[16:0] mr0_dll_reset;
wire	[16:0] mr1_dll_enable;
wire	[16:0] mr1_ocd_cal_default;
wire	[16:0] mr1_ocd_cal_exit;
wire	[13:0] wr_addr0;
wire	[13:0] wr_addr1;


typedef enum int unsigned {
	STATE_RESET,
	STATE_LOAD_INIT,
	STATE_RESET_VCALIB_RDDATA_BURST,
	STATE_RESET_PHY_SEQ_RDDATA_BURST,
	STATE_RESET_READ_FIFO_RESET,
	STATE_RESET_READ_FIFO_RESET_SYNC,
	STATE_STABLE,
	STATE_NOP,
	STATE_PRECHARGE_ALL0,
	STATE_TRP0,
	STATE_EMR2,
	STATE_TMRD0,
	STATE_EMR3,
	STATE_TMRD1,
	STATE_EMR1_DLL_ENABLE,
	STATE_TMRD2,
	STATE_MRS_DLL_RESET,
	STATE_TMRD3,
	STATE_PRECHARGE_ALL1,
	STATE_TRP1,
	STATE_AUTO_REFRESH0,
	STATE_TRFC0,
	STATE_AUTO_REFRESH1,
	STATE_TRFC1,
	STATE_MRS,
	STATE_TMRD4,
	STATE_EMR1_OCD_CAL_DEFAULT,
	STATE_TMRD5,
	STATE_EMR1_OCD_CAL_EXIT,
	STATE_TOIT,
	STATE_BANK_ACTIVATE,
	STATE_ASSERT_ODT,
	STATE_WRITE_ZERO,
	STATE_WAIT_WRITE_ZERO,
	STATE_WRITE_ONE,
	STATE_WAIT_WRITE_ONE,
	STATE_V_READ_ZERO,
	STATE_V_READ_NOP,
	STATE_V_READ_ONE,
	STATE_V_WAIT_READ,
	STATE_V_COMPARE_READ_ZERO_READ_ONE,
	STATE_V_CHECK_READ_FAIL,
    STATE_V_ADD_FULL_RATE,
	STATE_V_ADD_HALF_RATE,
	STATE_V_READ_FIFO_RESET,
	STATE_V_READ_FIFO_RESET_SYNC,
	STATE_V_CALIB_DONE,
	STATE_L_READ_ONE,
	STATE_L_WAIT_READ,
	STATE_L_COMPARE_READ_ONE,
	STATE_L_REDUCE_LATENCY,
	STATE_L_READ_FLUSH,
	STATE_L_WAIT_READ_FLUSH,
	STATE_L_ADD_MARGIN,
	STATE_BANK_PRECHARGE,
	STATE_CALIB_DONE,
	STATE_CALIB_FAIL
} state_t;

state_t state;


reg	add_fr_cycle_to_valid;
reg	[READ_VALID_TIMEOUT_WIDTH-1:0] read_valid_timeout_count;
reg	[MEM_READ_DQS_WIDTH-1:0] vcalib_count;

wire	[AFI_DATA_WIDTH-1:0] vcalib_rddata;

reg		[2**(SEQ_BURST_COUNT_WIDTH+1)-1:0] [AFI_DATA_WIDTH-1:0] vcalib_rddata_burst;
reg	    [(SEQ_BURST_COUNT_WIDTH+1)-1:0]	vcalib_rddata_burst_count;

wire [2**SEQ_BURST_COUNT_WIDTH-1:0] vcalib_rddata_all_zero_by_burst;
wire [2**SEQ_BURST_COUNT_WIDTH-1:0] vcalib_rddata_all_one_by_burst;

wire [AFI_DATA_WIDTH-1:0] vcalib_rddata_all_ones_pattern;
wire [AFI_DATA_WIDTH-1:0] vcalib_rddata_all_zeros_pattern;
wire vcalib_rddata_all_zero;
wire vcalib_rddata_all_one;

reg [VCALIB_COUNT_WIDTH-1:0] vcalib_rddata_delay_counter;

wire mux_seq_rdata_all_one;
reg		[2**(SEQ_BURST_COUNT_WIDTH)-1:0] [AFI_DATA_WIDTH-1:0] mux_seq_rdata_burst;

wire [2**SEQ_BURST_COUNT_WIDTH-1:0] mux_seq_rdata_all_one_by_burst;



	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			state <= STATE_RESET;
		else
		begin
			case (state)

			// *******************************************************************************************************************
			// INITIALIZATION

				// wait until reset is de-asserted
				STATE_RESET:
					state <= STATE_LOAD_INIT; 

				// load any initialization valus
				STATE_LOAD_INIT:
					state <= STATE_RESET_READ_FIFO_RESET;

				// reset the read group FIFO
				STATE_RESET_READ_FIFO_RESET:
					state <= STATE_RESET_READ_FIFO_RESET_SYNC;
				
				
				// The reset signal needs to cross the API clock domain to the read capture clock domain. For robustness,
				// we must avoid combinational logic in the cross-domain path by registering the generated reset signal 
				// before sending it out. The reset-sync state is to take into account the additional clock cycles.
				STATE_RESET_READ_FIFO_RESET_SYNC:
					state <= STATE_RESET_VCALIB_RDDATA_BURST;
				
				// reset the vcalib_rddata_burst vector
				STATE_RESET_VCALIB_RDDATA_BURST:
					state <= STATE_RESET_PHY_SEQ_RDDATA_BURST;

				// reset the mux_seq_rdata_burst vector
				STATE_RESET_PHY_SEQ_RDDATA_BURST:
					state <= STATE_STABLE; 


				// wait until memory is stable
				STATE_STABLE:
 				begin
					if (&mem_stable_count) // wait until maximum count is reached
						state <= STATE_NOP; 
					else
						state <= STATE_STABLE;
				end

				STATE_NOP:
				begin
					if (&nop_count)
						state <= STATE_PRECHARGE_ALL0; 
					else
						state <= STATE_NOP;
				end
				STATE_PRECHARGE_ALL0:
					state <= STATE_TRP0;
				STATE_TRP0:
				begin
					if (&rp_count)
						state <= STATE_EMR2;
					else
						state <= STATE_TRP0;
				end
				STATE_EMR2:
					state <= STATE_TMRD0;
				STATE_TMRD0:
                begin
                    if (&mrd_count)
                        state <= STATE_EMR3;
                    else
                        state <= STATE_TMRD0;
                end
				STATE_EMR3:
					state <= STATE_TMRD1;
				STATE_TMRD1:
                begin
                    if (&mrd_count)
                        state <= STATE_EMR1_DLL_ENABLE;
                    else
                        state <= STATE_TMRD1;
                end
				STATE_EMR1_DLL_ENABLE:
					state <= STATE_TMRD2;
				STATE_TMRD2:
                begin
                    if (&mrd_count)
                        state <= STATE_MRS_DLL_RESET;
                    else
                        state <= STATE_TMRD2;
                end
				STATE_MRS_DLL_RESET:
					state <= STATE_TMRD3;
				STATE_TMRD3:
                begin
                    if (&mrd_count)
                        state <= STATE_PRECHARGE_ALL1;
                    else
                        state <= STATE_TMRD3;
                end
				STATE_PRECHARGE_ALL1:
					state <= STATE_TRP1;
				STATE_TRP1:
				begin
					if (&rp_count)
						state <= STATE_AUTO_REFRESH0; 
					else
						state <= STATE_TRP1;
				end
				STATE_AUTO_REFRESH0:
					state <= STATE_TRFC0;
				STATE_TRFC0:
				begin
					if (&rfc_count)
						state <= STATE_AUTO_REFRESH1; 
					else
						state <= STATE_TRFC0;
				end
				STATE_AUTO_REFRESH1:
					state <= STATE_TRFC1;
				STATE_TRFC1:
				begin
					if (&rfc_count)
						state <= STATE_MRS;
					else
						state <= STATE_TRFC1;
				end
				STATE_MRS:
					state <= STATE_TMRD4;
				STATE_TMRD4:
                begin
                    if (&mrd_count)
                        state <= STATE_EMR1_OCD_CAL_DEFAULT;
                    else
                        state <= STATE_TMRD4;
                end
				STATE_EMR1_OCD_CAL_DEFAULT:
					state <= STATE_TMRD5;
				STATE_TMRD5:
                begin
                    if (&mrd_count)
						state <= STATE_EMR1_OCD_CAL_EXIT;
                    else
                        state <= STATE_TMRD5;
                end
				STATE_EMR1_OCD_CAL_EXIT:
					state <= STATE_TOIT;
				STATE_TOIT:
                begin
                    if (&oit_count)
                        state <= STATE_BANK_ACTIVATE;
                    else
                        state <= STATE_TOIT;
                end
				STATE_BANK_ACTIVATE:
					state <= STATE_ASSERT_ODT;
				STATE_ASSERT_ODT:
					state <= STATE_WRITE_ZERO; 	


			// *******************************************************************************************************************


	
			// *******************************************************************************************************************
			// WRITE CALIBRATION PATTERNS

				// Write States, 2 patterns: all 0's and all 1's

				// issue write command to address 0, bank 0 (bank address is N/A in QDRII)
				STATE_WRITE_ZERO: 
					state <= STATE_WAIT_WRITE_ZERO;

			    // write data pattern all 0's	
				STATE_WAIT_WRITE_ZERO: 
				begin
					if (&write_latency_count)  // count is long enough (16 cycles) to accomodate for worst case write latency
                    	state <= STATE_WRITE_ONE;
					else
						state <= STATE_WAIT_WRITE_ZERO;
				end

				// issue write command to address 1, bank 1 (bank address is N/A in QDRII)
                STATE_WRITE_ONE: 
                    state <= STATE_WAIT_WRITE_ONE;

				// write data pattern all 1's
                STATE_WAIT_WRITE_ONE: 
                begin
                    if (&write_latency_count)  // count is long enough (16 cycles) to accomodate for worst case write latency
                        state <= STATE_V_READ_ZERO;
                    else
                        state <= STATE_WAIT_WRITE_ONE;
                end
			// *******************************************************************************************************************


			// *******************************************************************************************************************
			// VALID CALIBRATION

			// Issue 2 back-to-back reads
			// 1st read to address 0, expected pattern is all 0's
			// 2nd read to address 1, expected pattern is all 1's
			// so that there will be a transition from 0 to 1 on all the data lines
			// Depending on the burst length, NOP commands need to be inserted between the 2 read commands
			// in order to avoid collision on the data bus
		

				// issue read command to address 0, bank 0 (bank address is N/A in QDRII)
				STATE_V_READ_ZERO:


					// 3 NOPs between commands are required in BL=8 mode
					// In half rate mode, read commands are issued on the high channel
					// the low channel is unused, which results in a full rate cycle of NOP
					// An extra half rate cycle (STATE_V_READ_NOP) will mean 2 more full rate NOPs 
					state <= STATE_V_READ_NOP;

				STATE_V_READ_NOP:
                    state <= STATE_V_READ_ONE;


				// issue read command to address 1, bank 1 (bank address is N/A in QDRII)
				STATE_V_READ_ONE:
					state <= STATE_V_WAIT_READ;

				// wait for valid read data, read data is 2 back to back reads
				STATE_V_WAIT_READ:
                begin
                	if (mux_seq_rdata_valid)
						// In full rate, BL=2, 1 read --> 1 AFI cycle of valid data
						// In full rate, BL=4, 1 read --> 2 AFI cycles of valid data
						// In full rate, BL=8, 1 read --> 4 AFI cycles of valid data
						// In half rate, BL=2, not supported
						// In half rate, BL=4, 1 read --> 1 AFI cycle of valid data
						// In half rate, BL=8, 1 read --> 2 AFI cycles of valid data							
                		if (&vcalib_rddata_burst_count)
							state <= STATE_V_COMPARE_READ_ZERO_READ_ONE;
	                	else
							state <= STATE_V_WAIT_READ;
                	else
						state <= STATE_V_WAIT_READ;
				end

				// parameterizable number of cycles to wait before
				// making the compariosn of rddata to 0.
				// if data is not the valid FIFO pointer needs to be adjusted
				STATE_V_COMPARE_READ_ZERO_READ_ONE:
				begin
					if (&vcalib_rddata_delay_counter)
						if ((vcalib_rddata_all_zero) & (vcalib_rddata_all_one))
							state <= STATE_V_CALIB_DONE;
						else
							state <= STATE_V_CHECK_READ_FAIL;
					else
						state <= STATE_V_COMPARE_READ_ZERO_READ_ONE;
					
				end

				// when a read fails, the write pointer (in AFI clock domain) of the valid FIFO is incremented
				// the read pointer of the valid FIFO is in the memory returned clock domain
				// the gap between the read and write pointers is effectively the latency between 
				// the time when the PHY receives the read command and the time valid data is returned to the PHY
				// see read_datapath.v for detailed implementation	
				STATE_V_CHECK_READ_FAIL: 
				begin
					if (add_fr_cycle_to_valid)	
						state <= STATE_V_ADD_FULL_RATE; 
					else
						state <= STATE_V_ADD_HALF_RATE;
				end

				// advance read valid fifo write pointer by an extra full rate cycle
				STATE_V_ADD_FULL_RATE: 
					state <= STATE_V_READ_FIFO_RESET;

				// advance read valid fifo write pointer by an extra half rate cycle (i.e. 2 full rate cycle)
				// in full rate core, this will just add another full rate cycle
				STATE_V_ADD_HALF_RATE: 
					state <= STATE_V_READ_FIFO_RESET;

				// reset the read and write pointers of the read data synchronization FIFO
				// read valid FIFO read output controls the write pointer of the read data synchronization FIFO
				// in the proper case, write pointer of data synchronization FIFO should only be incremented when read command is issued
				// Examples of failing cases:
				// 1. During warm reset, there are left over read commands in the valid FIFO, valid FIFO outputs a 1, write pointer of 
				//    synchronization FIFO is incremented but read pointer of synchronization FIFO is not because there is no real read command
				//    issued after the warm reset, write and read pointers are now out of sync.
				// 2. Read and write pointers of the valid FIFO can be pointing to the same location (e.g. right after reset), writing and 
				//    reading at the same time can result in unknown output, this can cause a false increment on the write pointer of the data
				//    synchronization FIFO. 
				// The READ_FIFO_RESET state ensures that both the write and read pointers of the synchronization FIFO are restarting at 0.
				STATE_V_READ_FIFO_RESET:
					if (&read_valid_timeout_count)	// calibration fails when timeout reaches maximum
						state <= STATE_CALIB_FAIL;
					else
						state <= STATE_V_READ_FIFO_RESET_SYNC;

				
				// The reset signal needs to cross the API clock domain to the read capture clock domain. For robustness,
				// we must avoid combinational logic in the cross-domain path by registering the generated reset signal 
				// before sending it out. The reset-sync state is to take into account the additional clock cycle so
				// that the reset signal never clashes with a subsequent wren signal to the read FIFO.
				STATE_V_READ_FIFO_RESET_SYNC:
					state <= STATE_V_READ_ZERO;

				// repeat valid calibration for each DQS group (1 pair of valid FIFO and data resynchronization per DQS group)
				// when all DQS groups are done, move on to latency calibration
				STATE_V_CALIB_DONE:
				begin
					if (vcalib_count == (MEM_READ_DQS_WIDTH-1))
						state <= STATE_L_READ_ONE;
					else
						state <= STATE_V_READ_ZERO;
				end

			// *******************************************************************************************************************



			// *******************************************************************************************************************
			// LATENCY CALIBRATION
		
				// the purpose of latency calibration is to find the optimal latency
				// keep reading back the all 1's pattern by reducing the latency cycle by cycle until the read fails
	

				// issue read command to address 1, bank 1 (bank address is N/A in QDRII)
				STATE_L_READ_ONE:
					state <= STATE_L_WAIT_READ;

				// Wait until the burst has been received.
				STATE_L_WAIT_READ:
				begin
                    if (mux_seq_rdata_valid)
							begin
								if (&burst_count)
									state <= STATE_L_COMPARE_READ_ONE;
								else
									state <= STATE_L_WAIT_READ;
							end
                    else
                        state <= STATE_L_WAIT_READ;
                end

	
				// wait for valid read data, expected data is all 1's
				// if data is correct, reduce the read latency and try again
				// if data is incorrect, that means the optimal read latency has been found, add margin for run time uncertainties 				
				STATE_L_COMPARE_READ_ONE:
				begin
					if (&vcalib_rddata_delay_counter)
						if (mux_seq_rdata_all_one)
							state <= STATE_L_REDUCE_LATENCY;
						else
							state <= STATE_L_ADD_MARGIN;
					else
						state <= STATE_L_COMPARE_READ_ONE;
					
				end

				// reduce the ready latency by one AFI cycle
				STATE_L_REDUCE_LATENCY:
					state <= STATE_L_READ_FLUSH;

				// flush the data synchronization FIFO with all 0's to make sure that correct data was not from the previous run with 
				// a higher read latency
				STATE_L_READ_FLUSH:
					state <= STATE_L_WAIT_READ_FLUSH;

				// wait for valid data (all 0's) to come back and repeat until every entry of the data FIFO is flushed
				STATE_L_WAIT_READ_FLUSH:
                begin
                    if (mux_seq_rdata_valid & (read_flush_count == 0))
						begin
							if (&burst_count)
								state <= STATE_L_READ_ONE;
							else	
								state <= STATE_L_WAIT_READ_FLUSH; 
						end
                    else if (mux_seq_rdata_valid)
						begin
							if (&burst_count)
								state <= STATE_L_READ_FLUSH;
							else
								state <= STATE_L_WAIT_READ_FLUSH;
						end
					else
                        state <= STATE_L_WAIT_READ_FLUSH;
                end

				// This state is reached from a read failure, add 1 AFI cycle for a correct read (minimum latency)
				// add 2 more AFI cycles to account for run time uncertainties
				STATE_L_ADD_MARGIN:
					state <= STATE_BANK_PRECHARGE;

				STATE_BANK_PRECHARGE:
					state <= STATE_CALIB_DONE;
	
				STATE_CALIB_DONE:
					state <= STATE_CALIB_DONE;

				STATE_CALIB_FAIL:
					state <= STATE_CALIB_FAIL;

			// *******************************************************************************************************************
			endcase
		end
	end

	// Generate the status bits indicating that the entire valid read was
	// all one or all zeros.
	generate
	genvar burstnum;
		for (burstnum=0; burstnum<2**SEQ_BURST_COUNT_WIDTH; burstnum=burstnum+1)
		begin: vcalib_rddata_calc
			assign vcalib_rddata_all_zero_by_burst[burstnum] = (vcalib_rddata_burst[burstnum] == vcalib_rddata_all_zeros_pattern);
			assign vcalib_rddata_all_one_by_burst[burstnum] = (vcalib_rddata_burst[2**SEQ_BURST_COUNT_WIDTH+burstnum] == vcalib_rddata_all_ones_pattern);
		end
	endgenerate
	assign vcalib_rddata_all_zero = &vcalib_rddata_all_zero_by_burst;
	assign vcalib_rddata_all_one = &vcalib_rddata_all_one_by_burst;


	// Generate the status bits indicating that the entire latency read was
	// all one
	generate
	genvar lcalib_burstnum;
		for (lcalib_burstnum=0; lcalib_burstnum<2**SEQ_BURST_COUNT_WIDTH; lcalib_burstnum=lcalib_burstnum+1)
		begin: lcalib_mux_seq_calc
			assign mux_seq_rdata_all_one_by_burst[lcalib_burstnum] = (mux_seq_rdata_burst[lcalib_burstnum] == {AFI_DATA_WIDTH{1'b1}});
		end
	endgenerate
	assign mux_seq_rdata_all_one = &mux_seq_rdata_all_one_by_burst;


	// generate the delay counter used to delay rddata comparison
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			vcalib_rddata_delay_counter <= {VCALIB_COUNT_WIDTH{1'b0}};
		else
			if ((state == STATE_V_COMPARE_READ_ZERO_READ_ONE) || (state == STATE_L_COMPARE_READ_ONE))
				vcalib_rddata_delay_counter <= vcalib_rddata_delay_counter + 1'b1;
			else
				vcalib_rddata_delay_counter <= {VCALIB_COUNT_WIDTH{1'b0}};
	end


	// Create the vcalib_rddata_burst block and counter
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n) begin

			vcalib_rddata_burst_count <= {(SEQ_BURST_COUNT_WIDTH+1){1'b0}};
		end
		else
			if (mux_seq_rdata_valid) begin
				vcalib_rddata_burst_count <= vcalib_rddata_burst_count + 1'b1;
			end
			else begin
				vcalib_rddata_burst_count <= {(SEQ_BURST_COUNT_WIDTH+1){1'b0}};
			end
	end

	// Capture the vcalib_rddata based on its burst index
	always_ff @(posedge pll_afi_clk)
	begin
		if (state == STATE_RESET_VCALIB_RDDATA_BURST) begin
			// The first read is expected to be all 0, and
			// the second all ones. As a result reset to be opposite
			// to avoid false match on first comparison.
			// Perform the reset as a stage in the sequencer algorithm
			// to reduce recovery failures in HardCopy.
			for (int i = 0; i < 2**SEQ_BURST_COUNT_WIDTH; i++)
				vcalib_rddata_burst[i] <= {AFI_DATA_WIDTH{1'b1}};
			for (int i = 0; i < 2**SEQ_BURST_COUNT_WIDTH; i++)
				vcalib_rddata_burst[i+ 2**(SEQ_BURST_COUNT_WIDTH)] <= {AFI_DATA_WIDTH{1'b0}};
		end
		else
			if (mux_seq_rdata_valid)
				vcalib_rddata_burst[vcalib_rddata_burst_count] <= vcalib_rddata;
	end

	// Capture the mux_seq_rdata based on its burst index
	always_ff @(posedge pll_afi_clk)
	begin
		if (state == STATE_RESET_PHY_SEQ_RDDATA_BURST) 
			// mux_seq_rdata_burst is tested against all 1 so reset to 0
			// Perform the reset as a stage in the sequencer algorithm
			// to reduce recovery failures in HardCopy.
			for (int i = 0; i < 2**SEQ_BURST_COUNT_WIDTH; i++)
				mux_seq_rdata_burst[i] <= {AFI_DATA_WIDTH{1'b0}};
		else
			if (mux_seq_rdata_valid)
				mux_seq_rdata_burst[burst_count] <= mux_seq_rdata;
	end

	// refer to STATE_NOP
    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
			nop_count <= {INIT_NOP_COUNT_WIDTH{1'b0}};
		else if (state == STATE_NOP)
			nop_count <= nop_count + 1'b1;
	end

    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
            rp_count <= {RP_COUNT_WIDTH{1'b0}};
        else if (state == STATE_TRP0 || state == STATE_TRP1)
            rp_count <= rp_count + 1'b1;
    end

	// refer to STATE_TMRD0, STATE_TMRD1, STATE_TMRD2
	wire mrd_state;
	assign mrd_state = (state == STATE_TMRD0) || (state == STATE_TMRD1) || (state == STATE_TMRD2) || (state == STATE_TMRD3) || (state == STATE_TMRD4) || (state == STATE_TMRD5);
    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
			mrd_count <= {MRD_COUNT_WIDTH{1'b0}};
		else if (mrd_state)
			mrd_count <= mrd_count + 1'b1;
	end

    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
            rfc_count <= {RFC_COUNT_WIDTH{1'b0}};
        else if (state == STATE_TRFC0 || state == STATE_TRFC1)
            rfc_count <= rfc_count + 1'b1;
    end

    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
			oit_count <= {OIT_COUNT_WIDTH{1'b0}};
		else if (state == STATE_TOIT)
			oit_count <= oit_count + 1'b1;
	end


	// refer to STATE_STABLE 
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			mem_stable_count <= {INIT_COUNT_WIDTH{1'b0}};
		else
			if (state == STATE_LOAD_INIT) begin
				// the mem_stable_count can be overwritten by the user during
				// timing simulation so that fast timing simulation can be enabled.
				if (seq_calib_init[0]) begin
					//synthesis translate_off
					$display("Disabling memory stable delay of calibration");
					//synthesis translate_on
					mem_stable_count <= {{(INIT_COUNT_WIDTH-1){1'b1}},1'b0};
				end
				else
					mem_stable_count <= {INIT_COUNT_WIDTH{1'b0}};
            end
			else if (state == STATE_STABLE)
				mem_stable_count <= mem_stable_count + 1'b1;
	end

	// hold reset in the read capture clock domain until memory is stable to ensure the clock returned from memory is clean 
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			seq_reset_mem_stable <= 1'b0;
		else if (state == STATE_WRITE_ZERO)
			seq_reset_mem_stable <= 1'b1;	
	end



	// the burst_count is used to keep track of the number of cycles that valid data should be expected
	// In full rate, BL=2, 1 read --> 1 AFI cycle of valid data
	// In full rate, BL=4, 1 read --> 2 AFI cycles of valid data
	// In full rate, BL=8, 1 read --> 3 AFI cycles of valid data
	// In half rate, BL=2, not supported
	// In half rate, BL=4, 1 read --> 1 AFI cycle of valid data
	// In half rate, BL=8, 1 read --> 2 AFI cycles of valid data
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			burst_count <= {SEQ_BURST_COUNT_WIDTH{1'b0}};
		else if (mux_seq_rdata_valid)
			burst_count <= burst_count + 1'b1;
		else 
			burst_count <= {SEQ_BURST_COUNT_WIDTH{1'b0}};
	end 

	// adjust the valid FIFO pointer offset during valid calibration, i.e. increment the write pointer by 1 full rate cycle every run
	// in a half rate core, it is implemented as alternating between adding 1 full rate cycle and adding 1 half rate cycle
	// refer to read_datapath.v for implementation details
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			add_fr_cycle_to_valid <= 1'b0;
		else
			add_fr_cycle_to_valid <= (state == STATE_V_ADD_HALF_RATE) ? 1'b1 : ((state == STATE_V_ADD_FULL_RATE) ? 1'b0 : add_fr_cycle_to_valid);
	end

	// 1 set of valid calibration signals per DQS group
generate
genvar dqsgroup;

	for (dqsgroup=0; dqsgroup<MEM_READ_DQS_WIDTH; dqsgroup=dqsgroup+1)
	begin: v_calib_control
		// controls for valid FIFO write pointer increment, vcalib_count keeps track of the DQS group
		assign seq_read_increment_vfifo_fr[dqsgroup] = (state == STATE_V_ADD_FULL_RATE) & (vcalib_count == dqsgroup);
		assign seq_read_increment_vfifo_hr[dqsgroup] = (state == STATE_V_ADD_HALF_RATE) & (vcalib_count == dqsgroup);

		// mux_seq_read_fifo_q is the combined read data from all the read data FIFOs without any reordering
		// the read data of concern is only the one from the FIFO pair (1 per DQS group) currently under calibration
		// mask out the read data (set to 0) from all other groups
		assign vcalib_rddata[(AFI_DQ_GROUP_DATA_WIDTH*(dqsgroup+1)-1) : (AFI_DQ_GROUP_DATA_WIDTH*dqsgroup)] = 
		       mux_seq_read_fifo_q[(AFI_DQ_GROUP_DATA_WIDTH*(dqsgroup+1)-1) : (AFI_DQ_GROUP_DATA_WIDTH*dqsgroup)] & ({AFI_DQ_GROUP_DATA_WIDTH{(vcalib_count == dqsgroup)}});

		// set expected data for the group currently under calibration
		assign vcalib_rddata_all_ones_pattern[(AFI_DQ_GROUP_DATA_WIDTH*(dqsgroup+1)-1) : (AFI_DQ_GROUP_DATA_WIDTH*dqsgroup)] = 
               ({AFI_DQ_GROUP_DATA_WIDTH{(vcalib_count == dqsgroup)}}); 

		assign vcalib_rddata_all_zeros_pattern[(AFI_DQ_GROUP_DATA_WIDTH*(dqsgroup+1)-1) : (AFI_DQ_GROUP_DATA_WIDTH*dqsgroup)] = 
               ({AFI_DQ_GROUP_DATA_WIDTH{1'b0}}); 

		always_ff @(posedge pll_afi_clk or negedge reset_n)
		begin
			if (~reset_n)
				seq_read_fifo_reset[dqsgroup] <= 1'b0;
			else
				seq_read_fifo_reset[dqsgroup] <= (state == STATE_V_READ_FIFO_RESET) & (vcalib_count == dqsgroup);
		end
	end
endgenerate


	// valid calibration fails when timeout reaches maximum
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			read_valid_timeout_count <= {READ_VALID_TIMEOUT_WIDTH{1'b0}};
		else if (state == STATE_V_CALIB_DONE)
			read_valid_timeout_count <= {READ_VALID_TIMEOUT_WIDTH{1'b0}};
		else if (state == STATE_V_READ_FIFO_RESET)
			read_valid_timeout_count <= read_valid_timeout_count + 1'b1;
	end


	// counter used to keep track of the number of DQS groups
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			vcalib_count <= {MEM_READ_DQS_WIDTH{1'b0}};
		else if (state == STATE_V_CALIB_DONE)
			vcalib_count <= vcalib_count + 1'b1;
	end




	// Latency Calibration Controls
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			seq_read_latency_counter <= {MAX_LATENCY_COUNT_WIDTH{1'b1}};
		else if (state == STATE_L_REDUCE_LATENCY)
			// reduce latency by 1 cycle whenever read is correct
			seq_read_latency_counter <= seq_read_latency_counter - 2'd1; 
		else if (state == STATE_L_ADD_MARGIN)
			// add 1 AFI cycle for correct data (min latency)
			// add 1 more AFI cycle (i.e. 2 memory clock cycles) for run time uncertainties
			seq_read_latency_counter <= seq_read_latency_counter + 2'd2;	
	end


	// refer to STATE_L_READ_FLUSH, flush every entry of the data FIFO
	always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
		if (~reset_n)
			read_flush_count <= {READ_FIFO_READ_ADDR_WIDTH{1'b0}};
		else if (state == STATE_L_READ_FLUSH)
			read_flush_count <= read_flush_count + 1'b1;
	end


    // write_latency_count is used to control the write latency, refer to the WRITE states 
    always_ff @(posedge pll_afi_clk or negedge reset_n)
    begin
        if (~reset_n)
            write_latency_count <= {MAX_WRITE_LATENCY_COUNT_WIDTH{1'b0}};
        else if ((state == STATE_WAIT_WRITE_ONE) | (state == STATE_WAIT_WRITE_ZERO))
            write_latency_count <= write_latency_count + 1'b1;
    end

	assign write_states = (state == STATE_WRITE_ZERO) | (state == STATE_WRITE_ONE);
	assign read_states = (state == STATE_V_READ_ZERO) | (state == STATE_V_READ_ONE) |  
						 (state == STATE_L_READ_ONE) | (state == STATE_L_READ_FLUSH);






	// assign the parameter to an integer type before using it to avoid width mismatch warnings
	integer mem_MR0_BL_int                = MR0_BL;
	integer mem_MR0_BT_int                = MR0_BT;
	integer mem_MR0_CAS_LATENCY_int       = MR0_CAS_LATENCY;
	integer mem_MR0_WR_int                = MR0_WR;
	integer mem_MR0_PD_int                = MR0_PD;
	integer mem_MR1_DLL_int               = MR1_DLL;
	integer mem_MR1_ODS_int               = MR1_ODS;
	integer mem_MR1_RTT_int               = MR1_RTT;
	integer mem_MR1_AL_int                = MR1_AL;
	integer mem_MR1_DQS_int               = MR1_DQS;
	integer mem_MR1_RDQS_int              = MR1_RDQS;
	integer mem_MR1_QOFF_int              = MR1_QOFF;
	integer mem_MR2_SRF_int               = MR2_SRF;
	
	assign mem_MR0_BL = mem_MR0_BL_int[2:0];
	assign mem_MR0_BT = mem_MR0_BT_int[0];
	assign mem_MR0_CAS_LATENCY = mem_MR0_CAS_LATENCY_int[2:0];
	assign mem_MR0_WR = mem_MR0_WR_int[2:0];
	assign mem_MR0_PD = mem_MR0_PD_int[0];
	assign mem_MR1_DLL = mem_MR1_DLL_int[0];
	assign mem_MR1_ODS = mem_MR1_ODS_int[0];
	assign mem_MR1_RTT = mem_MR1_RTT_int[1:0];
	assign mem_MR1_AL = mem_MR1_AL_int[1:0];
	assign mem_MR1_DQS = mem_MR1_DQS_int[0];
	assign mem_MR1_RDQS = mem_MR1_RDQS_int[0];
	assign mem_MR1_QOFF = mem_MR1_QOFF_int[0];
	assign mem_MR2_SRF = mem_MR2_SRF_int[0];

	// Mode Register 0
	assign mr0[2:0] = mem_MR0_BL;
	assign mr0[3] = mem_MR0_BT;
	assign mr0[6:4] = mem_MR0_CAS_LATENCY;
	assign mr0[7] = 1'b0;
	assign mr0[8] = 1'b0;
	assign mr0[11:9] = mem_MR0_WR;
	assign mr0[12] = mem_MR0_PD;
	assign mr0[13] = 1'b0;
	assign mr0[15:14] = 2'b00;
	assign mr0[16] = 1'b0;
	assign mr0_dll_reset = mr0 | 16'b0000000100000000;

	// Mode Register 1
	assign mr1[0] = mem_MR1_DLL;
	assign mr1[1] = mem_MR1_ODS;
	assign mr1[2] = mem_MR1_RTT[0];
	assign mr1[5:3] = mem_MR1_AL;
	assign mr1[6] = mem_MR1_RTT[1];
	assign mr1[9:7] = 3'b0;
	assign mr1[10] = mem_MR1_DQS;
	assign mr1[11] = mem_MR1_RDQS;
	assign mr1[12] = mem_MR1_QOFF;
	assign mr1[13] = 1'b0;
	assign mr1[15:14] = 2'b01;
	assign mr1[16] = 1'b0;
	assign mr1_dll_enable = mr1 & 16'b1111110001111110;
	assign mr1_ocd_cal_default = mr1 | 16'b0000001110000000;
	assign mr1_ocd_cal_exit = mr1 & 16'b1111110001111111;

	// Mode Register 2
	assign mr2[2:0] = 3'b0;
	assign mr2[3] = 1'b0;
	assign mr2[6:4] = 3'b0;
	assign mr2[7] = mem_MR2_SRF;
	assign mr2[13:8] = 6'b0;
	assign mr2[15:14] = 2'b10;
	assign mr2[16] = 1'b0;

	// Mode Register 3
	assign mr3[13:0] = 14'b0;
	assign mr3[15:14] = 2'b11;
	assign mr3[16] = 1'b0;

	assign wr_addr0[11:0] = 12'b0;
	assign wr_addr0[13:12] = 2'b1; 

	assign wr_addr1[11:0] = 12'b1000;
	assign wr_addr1[13:12] = 2'b1;

	assign mrs_states = (state == STATE_EMR2) || (state == STATE_EMR3) || (state == STATE_EMR1_DLL_ENABLE) || (state == STATE_MRS_DLL_RESET) ||
						(state == STATE_MRS) || (state == STATE_EMR1_OCD_CAL_DEFAULT) || (state == STATE_EMR1_OCD_CAL_EXIT);
	assign precharge_states = (state == STATE_PRECHARGE_ALL0) || (state == STATE_PRECHARGE_ALL1) || (state == STATE_BANK_PRECHARGE);
	assign refresh_states = (state == STATE_AUTO_REFRESH0) || (state == STATE_AUTO_REFRESH1);


	assign seq_cke = ~(state == STATE_STABLE);
	assign seq_odt = (state == STATE_ASSERT_ODT) ? 1'b1 : 1'b0;

	assign seq_cs_n  = mrs_states ? 1'b0 : precharge_states ? 1'b0 : refresh_states ? 1'b0 : (state == STATE_BANK_ACTIVATE) ? 1'b0 : write_states ? 1'b0 : read_states ? 1'b0 : 1'b1;
	assign seq_ras_n = mrs_states ? 1'b0 : precharge_states ? 1'b0 : refresh_states ? 1'b0 : (state == STATE_BANK_ACTIVATE) ? 1'b0 : write_states ? 1'b1 : read_states ? 1'b1 : 1'b1;
	assign seq_cas_n = mrs_states ? 1'b0 : precharge_states ? 1'b1 : refresh_states ? 1'b0 : (state == STATE_BANK_ACTIVATE) ? 1'b1 : write_states ? 1'b0 : read_states ? 1'b0 : 1'b1;
	assign seq_we_n  = mrs_states ? 1'b0 : precharge_states ? 1'b0 : refresh_states ? 1'b1 : (state == STATE_BANK_ACTIVATE) ? 1'b1 : write_states ? 1'b0 : read_states ? 1'b1 : 1'b1;

	assign seq_address = (state == STATE_EMR2) ? mr2[13:0] : (state == STATE_EMR3) ? mr3[13:0] : (state == STATE_EMR1_DLL_ENABLE) ? mr1_dll_enable[13:0] :
						 (state == STATE_MRS_DLL_RESET) ? mr0_dll_reset[13:0] : (state == STATE_MRS) ? mr0[13:0] :
						 (state == STATE_EMR1_OCD_CAL_DEFAULT) ? mr1_ocd_cal_default[13:0] : (state == STATE_EMR1_OCD_CAL_EXIT) ? mr1_ocd_cal_exit[13:0] :
						 (state == STATE_WRITE_ZERO || state == STATE_V_READ_ZERO || state == STATE_L_READ_FLUSH) ? wr_addr0 :
						 (state == STATE_WRITE_ONE || state == STATE_V_READ_ONE || state == STATE_L_READ_ONE) ? wr_addr1 :
					 	 (state == STATE_PRECHARGE_ALL0 || state == STATE_PRECHARGE_ALL1) ? {MEM_ADDRESS_WIDTH{1'b1}} : {MEM_ADDRESS_WIDTH{1'b0}};
	assign seq_bank = (state == STATE_EMR2) ? mr2[16:14] : (state == STATE_EMR3) ? mr3[16:14] : (state == STATE_EMR1_DLL_ENABLE) ? mr1_dll_enable[16:14] :
					  (state == STATE_MRS_DLL_RESET) ? mr0_dll_reset[16:14] : (state == STATE_MRS) ? mr0[16:14] :
					  (state == STATE_EMR1_OCD_CAL_DEFAULT) ? mr1_ocd_cal_default[16:14] : (state == STATE_EMR1_OCD_CAL_EXIT) ? mr1_ocd_cal_exit[16:14] :
					  {MEM_BANK_WIDTH{1'b0}};


	wire [MEM_READ_DQS_WIDTH-1:0] seq_vfifo_rd_en_override;	
	assign seq_vfifo_rd_en_override = {MEM_READ_DQS_WIDTH{1'b0}};	

	assign seq_cke_l = seq_cke;
	assign seq_cs_n_l = 1'b1;
	assign seq_ras_n_l = 1'b1;
	assign seq_cas_n_l = 1'b1;
	assign seq_we_n_l = 1'b1;
	assign seq_odt_l = 1'b0; 

	assign seq_address_l =  {MEM_ADDRESS_WIDTH{1'b0}};
	assign seq_bank_l = {MEM_BANK_WIDTH{1'b0}};


	// set the write data 1 cycle after the write command is issued (write latency is always more than 2 cycles)
	assign seq_wrdata = (state == STATE_WAIT_WRITE_ZERO) ? {MEM_DQ_WIDTH{1'b0}} : {MEM_DQ_WIDTH{1'b1}};

	// set the bidirectional buffer to output mode until the write is completed
	assign seq_wrdata_en = (state == STATE_WAIT_WRITE_ZERO) | (state == STATE_WAIT_WRITE_ONE);
	
	// start running DQS right after write command is issued (exact preamble is not neccessary for calibration writes)
	assign seq_dqs_en = (state == STATE_WRITE_ZERO) || (state == STATE_WAIT_WRITE_ZERO) ||
						(state == STATE_WRITE_ONE)  || (state == STATE_WAIT_WRITE_ONE);

	// data mask is not used during calibration
	assign seq_wrdata_mask = {AFI_DATA_MASK_WIDTH{1'b0}};

	// this signal is used in the read datapath to start the read latency counter
	// refer to latency shifter in read_datapath.v
	// seq_rddata_en needs to be high for x cycles, where x is the number of cycles that valid data is expected
	// In full rate, BL=2, 1 read --> 1 AFI cycle of valid data
	// In full rate, BL=4, 1 read --> 2 AFI cycles of valid data
	// In full rate, BL=8, 1 read --> 4 AFI cycles of valid data
	// In half rate, BL=2, not supported
	// In half rate, BL=4, 1 read --> 1 AFI cycle of valid data
	// In half rate, BL=8, 1 read --> 2 AFI cycles of valid data             

// assert the seq_rddata_en when in a read state
reg	rddata_en_r;
wire rddata_en;

	assign rddata_en = read_states ? 1'b1 : 1'b0; 

	// this flop stage extends seq_rddata_en for 1 more cycle
    always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			rddata_en_r <= 1'b0;
		else
			rddata_en_r <= rddata_en;
	end		


	// "OR" the flop output together with rddata_en to generate the exteneded seq_rddata_en signal
	assign seq_rddata_en = rddata_en_r | rddata_en;



	// a set of muxes between the sequencer AFI signals and the controller AFI signals
	// during calibration, mux_sel = 1, sequencer AFI signals are selected
	// after calibration is successful, mux_sel = 0, controller AFI signals are selected 


	reg mux_sel;
	always_ff @(posedge pll_afi_clk or negedge reset_n)
	begin
		if (~reset_n)
			mux_sel <= 1'b1;
		else
			mux_sel <= ~(state == STATE_CALIB_DONE); 
	end


	// in half rate, the width of AFI interface is double the width of memory interface
	assign seq_mux_address = {seq_address, seq_address_l};
    assign seq_mux_bank = {seq_bank, seq_bank_l};
    assign seq_mux_cs_n = {seq_cs_n, seq_cs_n_l};
    assign seq_mux_cke = {seq_cke, seq_cke_l};
    assign seq_mux_odt = {seq_odt, seq_odt_l};
    assign seq_mux_ras_n = {seq_ras_n, seq_ras_n_l};
    assign seq_mux_cas_n = {seq_cas_n, seq_cas_n_l};
    assign seq_mux_we_n = {seq_we_n, seq_we_n_l};

	assign seq_mux_wdata = {seq_wrdata, seq_wrdata, seq_wrdata, seq_wrdata}; // AFI data is 4x wide
	assign seq_mux_wdata_valid = {AFI_DQS_WIDTH{seq_wrdata_en}};
	assign seq_mux_dqs_en = {AFI_DQS_WIDTH{seq_dqs_en}};

	assign seq_mux_dm = seq_wrdata_mask;

	assign seq_mux_vfifo_rd_en_override = seq_vfifo_rd_en_override;
	assign seq_mux_rdata_en = seq_rddata_en;

	assign afi_cal_success = (state == STATE_CALIB_DONE);
	assign afi_cal_fail = (state == STATE_CALIB_FAIL);

endmodule
