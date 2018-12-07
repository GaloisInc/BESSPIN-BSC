// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// ******************************************************************************************************************************** 
// File name: read_datapath.v
// The read datapath is responsible for read data resynchronization from the memory clock domain to the AFI clock domain.
// It contains 1 FIFO per DQS group for read valid prediction and 1 FIFO per DQS group for read data synchronization.
// ******************************************************************************************************************************** 

`timescale 1 ps / 1 ps

// altera message_off 10036
module ddr2_v10_1_read_datapath(
	reset_n_afi_clk,
	seq_read_fifo_reset,
	reset_n_resync_clk,
	pll_dqs_ena_clk,
	read_capture_clk,
	ddio_phy_dq,
	pll_afi_clk,
	seq_read_latency_counter,
	seq_read_increment_vfifo_fr,
	seq_read_increment_vfifo_hr,
	afi_rdata_en,
	afi_rdata_en_full,
	afi_rdata,
	phy_mux_read_fifo_q,
	force_oct_off,
	dqs_enable_ctrl,
	afi_rdata_valid,
	seq_calib_init,
	dqs_edge_detect
);

// ******************************************************************************************************************************** 
// BEGIN PARAMETER SECTION
// All parameters default to "" will have their values passed in from higher level wrapper with the controller and driver 

parameter DEVICE_FAMILY = "";

// PHY-Memory Interface
parameter MEM_ADDRESS_WIDTH     = ""; 
parameter MEM_DM_WIDTH          = ""; 
parameter MEM_CONTROL_WIDTH     = ""; 
parameter MEM_DQ_WIDTH          = ""; 
parameter MEM_READ_DQS_WIDTH    = ""; 
parameter MEM_WRITE_DQS_WIDTH   = ""; 

// PHY-Controller (AFI) Interface
parameter AFI_ADDRESS_WIDTH         = ""; 
parameter AFI_DATA_MASK_WIDTH       = ""; 
parameter AFI_CONTROL_WIDTH         = ""; 
parameter AFI_DATA_WIDTH            = ""; 
parameter AFI_DQS_WIDTH				= "";

// Read Datapath
parameter MAX_LATENCY_COUNT_WIDTH          = "";
parameter MAX_READ_LATENCY                 = "";
parameter READ_FIFO_READ_MEM_DEPTH         = "";
parameter READ_FIFO_READ_ADDR_WIDTH        = "";
parameter READ_FIFO_WRITE_MEM_DEPTH        = "";
parameter READ_FIFO_WRITE_ADDR_WIDTH       = "";
parameter READ_VALID_FIFO_READ_ADDR_WIDTH  = "";
parameter READ_VALID_FIFO_WRITE_ADDR_WIDTH = "";
parameter NUM_SUBGROUP_PER_READ_DQS        = "";
parameter MEM_T_RL                         = "";
parameter QVLD_EXTRA_FLOP_STAGES           = "";
parameter QVLD_WR_ADDRESS_OFFSET           = "";

// Width of the calibration status register used to control calibration skipping.
parameter CALIB_REG_WIDTH		= "";

// Local parameters
localparam DOUBLE_MEM_DQ_WIDTH = MEM_DQ_WIDTH * 2;
localparam DQ_GROUP_WIDTH = MEM_DQ_WIDTH / MEM_READ_DQS_WIDTH;
localparam AFI_DQ_GROUP_DATA_WIDTH = AFI_DATA_WIDTH / MEM_READ_DQS_WIDTH;
localparam DDIO_DQ_GROUP_DATA_WIDTH = DOUBLE_MEM_DQ_WIDTH / MEM_READ_DQS_WIDTH;
localparam AFI_DQ_GROUP_DATA_WIDTH_SUBGROUP = AFI_DATA_WIDTH / (MEM_READ_DQS_WIDTH * NUM_SUBGROUP_PER_READ_DQS);
localparam DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP = DOUBLE_MEM_DQ_WIDTH / (MEM_READ_DQS_WIDTH * NUM_SUBGROUP_PER_READ_DQS);
localparam QVLD_SHIFTER_LENGTH = (2 ** READ_VALID_FIFO_WRITE_ADDR_WIDTH) + QVLD_EXTRA_FLOP_STAGES;

localparam OCT_ON_DELAY = (MEM_T_RL > 4) ? ((MEM_T_RL - 4) / 2) : 0;
localparam OCT_OFF_DELAY = (MEM_T_RL + 6) / 2;

// END PARAMETER SECTION
// ******************************************************************************************************************************** 



input	reset_n_afi_clk;
input	[MEM_READ_DQS_WIDTH-1:0] seq_read_fifo_reset; // reset from sequencer to read and write pointers of the data resynchronization FIFO
input	[MEM_READ_DQS_WIDTH-1:0] read_capture_clk;
input	reset_n_resync_clk;
input	pll_dqs_ena_clk;
input	[DOUBLE_MEM_DQ_WIDTH-1:0] ddio_phy_dq;
input	pll_afi_clk;
input	[MAX_LATENCY_COUNT_WIDTH-1:0] seq_read_latency_counter;
input	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_fr;	// increment valid prediction FIFO write pointer by an extra full rate cycle
input	[MEM_READ_DQS_WIDTH-1:0] seq_read_increment_vfifo_hr;	// increment valid prediction FIFO write pointer by an extra half rate cycle
																// in full rate core, both will mean an extra full rate cycle

input	afi_rdata_en;
input	afi_rdata_en_full;

output	[AFI_DATA_WIDTH-1:0] afi_rdata;
output	afi_rdata_valid;

// read data (no reordering) for indepedently FIFO calibrations (multiple FIFOs for multiple DQS groups)
output	[AFI_DATA_WIDTH-1:0] phy_mux_read_fifo_q;	

output	[AFI_DQS_WIDTH-1:0] force_oct_off;
output	[AFI_DQS_WIDTH-1:0] dqs_enable_ctrl;

output [MEM_READ_DQS_WIDTH-1:0] dqs_edge_detect;

input	[CALIB_REG_WIDTH-1:0] seq_calib_init;

reg		afi_rdata_valid;
wire	[AFI_DATA_WIDTH-1:0] read_data;

reg		[MEM_READ_DQS_WIDTH-1:0] qvld_shift_fr_cycle;
wire	[AFI_DQS_WIDTH-1:0] qvld;
wire	[MEM_READ_DQS_WIDTH-1:0] read_valid;
wire	[MEM_READ_DQS_WIDTH-1:0] valid_predict_clk;
wire	[MEM_READ_DQS_WIDTH-1:0] reset_n_valid_predict_clk;
reg		[MEM_READ_DQS_WIDTH-1:0] reset_n_fifo_write_side;
reg		[MEM_READ_DQS_WIDTH-1:0] reset_n_fifo_wraddress;

wire	[MEM_READ_DQS_WIDTH-1:0] read_capture_clk_pos;
wire	[MEM_READ_DQS_WIDTH-1:0] read_capture_clk_neg;
reg		[MEM_READ_DQS_WIDTH-1:0] read_capture_clk_div2;

wire	seq_calib_skip_vfifo;
assign seq_calib_skip_vfifo = seq_calib_init[3];

// *******************************************************************************************************************
// VALID PREDICTION
// Read request (afi_rdata_en) is generated on the AFI clock domain (pll_afi_clk).
// Read data is captured on the read_capture_clk domain (output clock from I/O). 
// The purpose of valid prediction is to determine which read_capture_clk cycle valid data will be returned to the core
// after the request is issued on pll_afi_clk; this is essentially the latency between read request seen on 
// AFI interface and valid data available at the output of ALTDQ_DQS.
// The clock domain crossing between pll_afi_clk and read_capture_clk is handled by a FIFO (uread_valid_fifo).
// The pll_afi_clk controls the write side of the FIFO and the read_capture_clk controls the read side.
// The pll_afi_clk writes into the FIFO on every clock cycle.  When there is no read request, it writes a 0;
// when there is a read request, it writes a 1 (refer to as a token) into the FIFO.
// The read_capture_clk reads from the FIFO every clock cycle, whenever it reads a token, it means that valid data
// is available during that cycle.  Each token represents 1 cycle of valid data.
// In full rate, BL=2, 1 read results in 1 AFI cycle of valid data, controller asserts afi_rdata_en for 1 cycle
// In full rate, BL=4, 1 read results in 2 AFI cycles of valid data, controller asserts afi_rdata_en for 2 cycles
// In full rate, BL=8, 1 read results in 4 AFI cycles of valid data, controller asserts afi_rdata_en for 4 cycles
// In half rate, BL=2, not supported
// In half rate, BL=4, 1 read results in 1 AFI cycle of valid data, controller asserts afi_rdata_en for 1 cycle
// In half rate, BL=8, 1 read results in 2 AFI cycle of valid data, controller asserts afi_rdata_en for 2 cycles
// In full rate, 1 afi_rdata_en cycle = 1 token
// In half rate, 1 afi_rdata_en cycle = 2 tokens
//
// After reset is released, the relationship between the read and write pointers can be arbitrary.
// During calibration, the sequencer keeps incrementing the write pointer (both the sequencer and write pointer operates
// on pll_afi_clk) until the correct latency has been tuned.
// *******************************************************************************************************************


assign valid_predict_clk = {MEM_READ_DQS_WIDTH{pll_dqs_ena_clk}};
assign reset_n_valid_predict_clk = {MEM_READ_DQS_WIDTH{reset_n_resync_clk}};


generate
genvar dqsgroup;

	for (dqsgroup=0; dqsgroup<MEM_READ_DQS_WIDTH; dqsgroup=dqsgroup+1)
	begin: read_valid_predict
		reg [READ_VALID_FIFO_WRITE_ADDR_WIDTH-1:0] qvld_wr_address;

		wire	qvld_in_l;
		wire	qvld_in_h;
		reg		qvld_in_l_r;

`ifndef SIMGEN
 		// synthesis translate_off
`endif
		wire [READ_VALID_FIFO_WRITE_ADDR_WIDTH:0] qvld_wr_address_offset;
		assign qvld_wr_address_offset = QVLD_WR_ADDRESS_OFFSET;
`ifndef SIMGEN
 		// synthesis translate_on
`endif

		// In half rate, 1 afi_rdata_en cycle = 2 tokens, qvld_in_l and qvld_in_h
		// Tokens are written at half rate but read at full rate.
		// The latency needs to be tuned at full rate granularity, 
		// this is achieved by delaying qvld_in_l by one half rate cycle (qvld_in_l_r).
		// In the default case, 1 afi_rdata_en will results in two tokens in write address 0
		// that means read address 0 and read address 1 will both have tokens.
		// If the sequencer request to increase the latency by 1 full rate cycle,
		// the write side first writes 10 into write address 0, then it writes 01 into
		// write address 1 (qvld_in_l is delayed by 1 cyle); this means there are tokens in
		// read address 1 and read address 2 (increased by 1 full rate cycle).

		always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
		begin
			if (~reset_n_afi_clk)
				qvld_in_l_r <= 1'b0;
			else
				qvld_in_l_r <= afi_rdata_en_full;
		end

		always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
		begin
			if (~reset_n_afi_clk)
`ifndef SIMGEN
				// synthesis translate_off
`endif
				qvld_shift_fr_cycle[dqsgroup] <= (seq_calib_skip_vfifo) ? qvld_wr_address_offset[0] : 1'b0;
`ifndef SIMGEN
				// synthesis translate_on
				// synthesis read_comments_as_HDL on
				// qvld_shift_fr_cycle[dqsgroup] <= 1'b0;
				// synthesis read_comments_as_HDL off
`endif
			else
				qvld_shift_fr_cycle[dqsgroup] <= seq_read_increment_vfifo_fr[dqsgroup] ? 1'b1 : (seq_read_increment_vfifo_hr[dqsgroup] ? 1'b0 : qvld_shift_fr_cycle[dqsgroup]);
		end

		assign qvld_in_h = afi_rdata_en_full;
		assign qvld_in_l = qvld_shift_fr_cycle[dqsgroup] ? qvld_in_l_r : afi_rdata_en_full;


		always @(posedge pll_afi_clk)
		begin
			if (~reset_n_afi_clk)
`ifndef SIMGEN
				// synthesis translate_off
`endif
				qvld_wr_address <= (seq_calib_skip_vfifo) ? (qvld_wr_address_offset >> 1) : {READ_VALID_FIFO_WRITE_ADDR_WIDTH{1'b0}};
`ifndef SIMGEN
				// synthesis translate_on
				// synthesis read_comments_as_HDL on
				// qvld_wr_address <= {READ_VALID_FIFO_WRITE_ADDR_WIDTH{1'b0}};
				// synthesis read_comments_as_HDL off
`endif
			else
				qvld_wr_address <= seq_read_increment_vfifo_hr[dqsgroup] ? (qvld_wr_address + 2'd1) : qvld_wr_address;
		end

		reg 	[QVLD_SHIFTER_LENGTH-1:0] qvld_shifter_l;
		reg 	[QVLD_SHIFTER_LENGTH-1:0] qvld_shifter_h;

		always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
		begin
			if (~reset_n_afi_clk)
			begin
				qvld_shifter_l <= {QVLD_SHIFTER_LENGTH{1'b0}};
				qvld_shifter_h <= {QVLD_SHIFTER_LENGTH{1'b0}};
			end
			else
			begin
				qvld_shifter_l <= {1'b0,qvld_shifter_l[QVLD_SHIFTER_LENGTH-1:1]};
				qvld_shifter_h <= {1'b0,qvld_shifter_h[QVLD_SHIFTER_LENGTH-1:1]};
				qvld_shifter_l[qvld_wr_address+QVLD_EXTRA_FLOP_STAGES] <= qvld_in_l;
				qvld_shifter_h[qvld_wr_address+QVLD_EXTRA_FLOP_STAGES] <= qvld_in_h;
		 	end
		end

		assign qvld[dqsgroup] = qvld_shifter_l[0];
		assign qvld[dqsgroup+MEM_READ_DQS_WIDTH] = qvld_shifter_h[0];
	end
endgenerate

assign dqs_enable_ctrl = qvld;


reg	[MAX_READ_LATENCY-1:0] latency_shifter;
reg	[MAX_READ_LATENCY-1:0] full_latency_shifter;

always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
begin
	if (~reset_n_afi_clk) begin
		full_latency_shifter <= {MAX_READ_LATENCY{1'b0}};
		latency_shifter <= {MAX_READ_LATENCY{1'b0}};
	end
	else begin
		full_latency_shifter <= {full_latency_shifter[MAX_READ_LATENCY-2:0], afi_rdata_en_full};
		latency_shifter <= {latency_shifter[MAX_READ_LATENCY-2:0], afi_rdata_en};
	end
end
	

generate
genvar dqs_count, subgroup, dq_count;
	
	for (dqs_count=0; dqs_count<MEM_READ_DQS_WIDTH; dqs_count=dqs_count+1)
	begin: read_buffering

		wire	[NUM_SUBGROUP_PER_READ_DQS-1:0] wren;
		wire	[NUM_SUBGROUP_PER_READ_DQS-1:0] wren_neg;
		wire	read_enable;
		wire	[DDIO_DQ_GROUP_DATA_WIDTH-1:0] ddio_phy_dq_per_dqs;
		wire	[AFI_DQ_GROUP_DATA_WIDTH-1:0] read_data_per_dqs;

		ddr2_v10_1_read_valid_selector uread_valid_selector(
			.reset_n		(reset_n_afi_clk),
			.pll_afi_clk		(pll_afi_clk),
			.latency_shifter	(latency_shifter),
			.latency_counter	(seq_read_latency_counter),
			.read_enable		(),
			.read_valid		(read_valid[dqs_count])
		);
		defparam uread_valid_selector.MAX_LATENCY_COUNT_WIDTH = MAX_LATENCY_COUNT_WIDTH;
		ddr2_v10_1_read_valid_selector uread_valid_full_selector(
			.reset_n		(reset_n_afi_clk),
			.pll_afi_clk		(pll_afi_clk),
			.latency_shifter	(full_latency_shifter),
			.latency_counter	(seq_read_latency_counter),
			.read_enable		(read_enable),
			.read_valid		()
		);
		defparam uread_valid_full_selector.MAX_LATENCY_COUNT_WIDTH = MAX_LATENCY_COUNT_WIDTH;

		assign ddio_phy_dq_per_dqs = ddio_phy_dq[(DDIO_DQ_GROUP_DATA_WIDTH*(dqs_count+1)-1) : (DDIO_DQ_GROUP_DATA_WIDTH*dqs_count)];		

		always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
		begin
			if (~reset_n_afi_clk) begin
				reset_n_fifo_write_side[dqs_count] <= 1'b0;
				reset_n_fifo_wraddress[dqs_count] <= 1'b0;
			end
			else begin
				reset_n_fifo_write_side[dqs_count] <= ~seq_read_fifo_reset[dqs_count];
				reset_n_fifo_wraddress[dqs_count] <= ~seq_read_fifo_reset[dqs_count];
			end
		end
		
		always @(posedge read_capture_clk[dqs_count] or negedge reset_n_fifo_write_side[dqs_count])
		begin
			if (~reset_n_fifo_write_side[dqs_count])
				read_capture_clk_div2[dqs_count] <= 1'b1;
			else
				read_capture_clk_div2[dqs_count] <= ~read_capture_clk_div2[dqs_count];
		end

    `ifndef SIMGEN
		assign #500 read_capture_clk_pos[dqs_count] = read_capture_clk_div2[dqs_count];
    `else
		sim_delay #(
				.delay(500)
			)
			sim_delay_inst(
				.o(read_capture_clk_pos[dqs_count]),
				.i(read_capture_clk_div2[dqs_count]),
			);
    `endif
		assign read_capture_clk_neg[dqs_count] = ~read_capture_clk_pos[dqs_count];

		for (subgroup=0; subgroup<NUM_SUBGROUP_PER_READ_DQS; subgroup=subgroup+1)
		begin: read_subgroup

 			assign wren[subgroup] = 1'b1;
 			assign wren_neg[subgroup] = 1'b1;

	
			reg	[READ_FIFO_WRITE_ADDR_WIDTH-1:0] wraddress /* synthesis dont_merge */;
			reg	[READ_FIFO_WRITE_ADDR_WIDTH-1:0] wraddress_neg /* synthesis dont_merge */;

			// The clock is read_capture_clk while reset_n_afi_clk and seq_read_fifo_reset are coming from the AFI clock domain
			// read_capture_clk is a strobe for valid_data, when reset is de-asserted, read_capture_clk is not toggling and therefore there is no clock
			// to get the reset signal through the reset synchronization pipeline. 
			// For system reset, the de-assertion of the reset (through reset pipeline in AFI domain) will be stable for a long time before read_capture_clock
			// starts toggling, so there is no risk of metastability.
			// For seq_read_fifo_reset, it is controlled by the sequencer to reset the write pointer every time there is a read mismatch, the sequencer will 
			// be responsible to make sure that the de-assertion of the reset is stable at least a few cycles before read_capture_clock starts toggling.  
		
			always @(posedge read_capture_clk_pos[dqs_count] or negedge reset_n_fifo_wraddress[dqs_count])
			begin
				if (~reset_n_fifo_wraddress[dqs_count])
					wraddress <= {READ_FIFO_WRITE_ADDR_WIDTH{1'b0}};
				else if (wren[subgroup])
				begin
					if (READ_FIFO_WRITE_MEM_DEPTH == 2 ** READ_FIFO_WRITE_ADDR_WIDTH)
						wraddress <= wraddress + 1'b1;
					else
						wraddress <= (wraddress == READ_FIFO_WRITE_MEM_DEPTH - 1) ? {READ_FIFO_WRITE_ADDR_WIDTH{1'b0}} : wraddress + 1'b1;
				end
			end

			always @(posedge read_capture_clk_neg[dqs_count] or negedge reset_n_fifo_wraddress[dqs_count])
			begin
				if (~reset_n_fifo_wraddress[dqs_count])
					wraddress_neg <= {READ_FIFO_WRITE_ADDR_WIDTH{1'b0}};
				else if (wren_neg[subgroup])
				begin
					if (READ_FIFO_WRITE_MEM_DEPTH == 2 ** READ_FIFO_WRITE_ADDR_WIDTH)
						wraddress_neg <= wraddress_neg + 1'b1;
					else
						wraddress_neg <= (wraddress_neg == READ_FIFO_WRITE_MEM_DEPTH - 1) ? {READ_FIFO_WRITE_ADDR_WIDTH{1'b0}} : wraddress_neg + 1'b1;
				end
			end

			reg	[READ_FIFO_READ_ADDR_WIDTH-1:0] rdaddress /* synthesis dont_merge */;

			always @(posedge pll_afi_clk)
			begin
				if (seq_read_fifo_reset[dqs_count])
					rdaddress <= {READ_FIFO_READ_ADDR_WIDTH{1'b0}};
				else if (read_enable)
				begin
					if (READ_FIFO_READ_MEM_DEPTH == 2 ** READ_FIFO_READ_ADDR_WIDTH)
						rdaddress <= rdaddress + 1'b1;
					else
						rdaddress <= (rdaddress == READ_FIFO_READ_MEM_DEPTH - 1) ? {READ_FIFO_READ_ADDR_WIDTH{1'b0}} : rdaddress + 1'b1;
				end
			end


			ddr2_v10_1_flop_mem	uread_fifo(
				.wr_clk     (read_capture_clk_pos[dqs_count]),
				.wr_en		(wren[subgroup]),
				.wr_addr	(wraddress),
				.wr_data	(ddio_phy_dq_per_dqs[(DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*(subgroup+1)-1) : 
							 (DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*subgroup)]),
				.rd_reset_n (reset_n_afi_clk),
				.rd_clk		(pll_afi_clk),
				.rd_en		(read_enable),
				.rd_addr	(rdaddress),
				.rd_data	(read_data_per_dqs[(DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*(subgroup+1)-1) :
							 (DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*subgroup)])
			);
			defparam uread_fifo.WRITE_MEM_DEPTH = READ_FIFO_WRITE_MEM_DEPTH;
			defparam uread_fifo.WRITE_ADDR_WIDTH = READ_FIFO_WRITE_ADDR_WIDTH;
			defparam uread_fifo.WRITE_DATA_WIDTH = DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP;
			defparam uread_fifo.READ_MEM_DEPTH = READ_FIFO_READ_MEM_DEPTH;
			defparam uread_fifo.READ_ADDR_WIDTH = READ_FIFO_READ_ADDR_WIDTH;
			defparam uread_fifo.READ_DATA_WIDTH = AFI_DQ_GROUP_DATA_WIDTH_SUBGROUP/2;

			ddr2_v10_1_flop_mem	uread_fifo_neg(
				.wr_clk		(read_capture_clk_neg[dqs_count]),
				.wr_en		(wren_neg[subgroup]),
				.wr_addr	(wraddress_neg),
				.wr_data	(ddio_phy_dq_per_dqs[(DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*(subgroup+1)-1) : 
							 (DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*subgroup)]),
				.rd_reset_n (reset_n_afi_clk),
				.rd_clk		(pll_afi_clk),
				.rd_en		(read_enable),
				.rd_addr	(rdaddress),
				.rd_data	(read_data_per_dqs[(DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*(subgroup+1)-1+DDIO_DQ_GROUP_DATA_WIDTH) :
							 (DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP*subgroup+DDIO_DQ_GROUP_DATA_WIDTH)])
			);
			defparam uread_fifo_neg.WRITE_MEM_DEPTH = READ_FIFO_WRITE_MEM_DEPTH;
			defparam uread_fifo_neg.WRITE_ADDR_WIDTH = READ_FIFO_WRITE_ADDR_WIDTH;
			defparam uread_fifo_neg.WRITE_DATA_WIDTH = DDIO_DQ_GROUP_DATA_WIDTH_SUBGROUP;
			defparam uread_fifo_neg.READ_MEM_DEPTH = READ_FIFO_READ_MEM_DEPTH;
			defparam uread_fifo_neg.READ_ADDR_WIDTH = READ_FIFO_READ_ADDR_WIDTH;
			defparam uread_fifo_neg.READ_DATA_WIDTH = AFI_DQ_GROUP_DATA_WIDTH_SUBGROUP/2;

		end

		assign read_data[(AFI_DQ_GROUP_DATA_WIDTH*(dqs_count+1)-1) : (AFI_DQ_GROUP_DATA_WIDTH*dqs_count)] = read_data_per_dqs;
		
	end

endgenerate


wire	[MEM_DQ_WIDTH-1:0] rdata_0;
wire	[MEM_DQ_WIDTH-1:0] rdata_1;
wire	[MEM_DQ_WIDTH-1:0] rdata_2;
wire	[MEM_DQ_WIDTH-1:0] rdata_3;

generate 
genvar k;
	for (k=0; k<MEM_READ_DQS_WIDTH; k=k+1)
	begin: read_mapping
		assign rdata_0[(DQ_GROUP_WIDTH*(k+1)-1) : DQ_GROUP_WIDTH*k] = read_data[((DQ_GROUP_WIDTH*1-1)+4*DQ_GROUP_WIDTH*k) : (DQ_GROUP_WIDTH*0+4*DQ_GROUP_WIDTH*k)]; 
		assign rdata_1[(DQ_GROUP_WIDTH*(k+1)-1) : DQ_GROUP_WIDTH*k] = read_data[((DQ_GROUP_WIDTH*2-1)+4*DQ_GROUP_WIDTH*k) : (DQ_GROUP_WIDTH*1+4*DQ_GROUP_WIDTH*k)]; 
		assign rdata_2[(DQ_GROUP_WIDTH*(k+1)-1) : DQ_GROUP_WIDTH*k] = read_data[((DQ_GROUP_WIDTH*3-1)+4*DQ_GROUP_WIDTH*k) : (DQ_GROUP_WIDTH*2+4*DQ_GROUP_WIDTH*k)]; 
		assign rdata_3[(DQ_GROUP_WIDTH*(k+1)-1) : DQ_GROUP_WIDTH*k] = read_data[((DQ_GROUP_WIDTH*4-1)+4*DQ_GROUP_WIDTH*k) : (DQ_GROUP_WIDTH*3+4*DQ_GROUP_WIDTH*k)]; 
	end
endgenerate 

assign afi_rdata = {rdata_0, rdata_1, rdata_2, rdata_3};

// Generate an AFI read valid signal from all the read valid signals from all FIFOs
always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
begin
	if (~reset_n_afi_clk)
		afi_rdata_valid <= 1'b0;
	else
		afi_rdata_valid <= &read_valid;
end


assign phy_mux_read_fifo_q = read_data;


reg		[AFI_DQS_WIDTH-1:0] force_oct_off;

generate
genvar oct_num;
for (oct_num = 0; oct_num < AFI_DQS_WIDTH; oct_num = oct_num + 1)
begin : oct_gen
	reg		[OCT_OFF_DELAY-1:0] rdata_en_r /* synthesis dont_merge */;
	wire [OCT_OFF_DELAY:0] rdata_en_shifter;

	assign rdata_en_shifter = {rdata_en_r,afi_rdata_en_full};
	always @(posedge pll_afi_clk or negedge reset_n_afi_clk)
	begin
		if (~reset_n_afi_clk)
		begin
			rdata_en_r <= {OCT_OFF_DELAY{1'b0}};
			force_oct_off[oct_num] = 1'b0;
		end
		else
		begin
			rdata_en_r <= {rdata_en_r[OCT_OFF_DELAY-2:0],afi_rdata_en_full};
			force_oct_off[oct_num] = ~(|rdata_en_shifter[OCT_OFF_DELAY:OCT_ON_DELAY]);
		end
	end
end
endgenerate


// Track if any DQS edges were captured as method of determining if the interface is
// alive during debug.
wire [MEM_READ_DQS_WIDTH-1:0] dqs_detect_reset_n;
reg [MEM_READ_DQS_WIDTH-1:0] dqs_edge_detect_reg;

generate
genvar dqs_detect_count;
	
	for (dqs_detect_count=0; dqs_detect_count<MEM_READ_DQS_WIDTH; dqs_detect_count=dqs_detect_count+1)
	begin: dqs_detection

	always @(posedge read_capture_clk_pos[dqs_detect_count] or negedge reset_n_fifo_write_side[dqs_detect_count])
		if (~reset_n_fifo_write_side[dqs_detect_count])
			dqs_edge_detect_reg[dqs_detect_count] <= 0;
		else
			dqs_edge_detect_reg[dqs_detect_count] <= 1;
		
	end
	
endgenerate 
assign dqs_edge_detect = dqs_edge_detect_reg;

endmodule
