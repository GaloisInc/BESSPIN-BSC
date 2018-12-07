// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

//////////////////////////////////////////////////////////////////////////////
// The Example Driver is a parametrizable Avalon Memory-Mapped Master used to
// test various memory interfaces.  The driver generates pseudo-random traffic
// using a number of different patterns and compare the received data against
// what is expected.
// The Example Driver execute tests in various stages.  There are two test
// stages predefined in this driver, and it can be easily extended to include
// custom stages.
//////////////////////////////////////////////////////////////////////////////

(* altera_attribute = "-name ALLOW_SYNCH_CTRL_USAGE OFF;-name AUTO_CLOCK_ENABLE_RECOGNITION OFF" *)
module ddr2_v10_1_driver(
	clk,
	reset_n,
	avl_ready,
	avl_write_req,
	avl_read_req,
	avl_burstbegin,
	avl_addr,
	avl_size,
	avl_be,
	avl_wdata,
	avl_rdata_valid,
	avl_rdata,
	pass,
	fail,
	test_complete,
	pnf_per_bit,
	pnf_per_bit_persist
);

import ddr2_v10_1_driver_definitions::*;

//////////////////////////////////////////////////////////////////////////////
// BEGIN PARAMETER SECTION

parameter DEVICE_FAMILY							= "";

// AVALON SIGNAL WIDTHS
parameter ADDR_WIDTH							= "";
parameter BURSTCOUNT_WIDTH						= "";
parameter DATA_WIDTH							= "";
parameter BE_WIDTH								= "";

// DRIVER CONFIGURATION
// If set to "TRUE", the driver generates pseudo-random byte enables
parameter RANDOM_BYTE_ENABLE					= "";
// If set to "TRUE", the driver generates 'avl_size' which are powers of two
parameter POWER_OF_TWO_BURSTS_ONLY				= "";
// If set to "TRUE", burst transfers begin at addresses which are multiples of 'avl_size'
parameter BURST_ON_BURST_BOUNDARY				= "";
// If set to "TRUE", per byte address will be generated instead of per word address
parameter GEN_BYTE_ADDR						= "";
// When read compare is enabled, the write address and
// data are buffered for read operations and data compare.
// When it is disabled, the write address and data are not buffered.
parameter ENABLE_READ_COMPARE					= "";
// Timeout counter width
// If the test stages are modified, this parameter
// may need adjustment to avoid premature timeouts.
parameter TIMEOUT_COUNTER_WIDTH					= "";

// TEST STAGES PARAMETERS
// Single write/read stage
parameter SINGLE_RW_SEQ_ADDR_COUNT				= "";
parameter SINGLE_RW_RAND_ADDR_COUNT				= "";
parameter SINGLE_RW_RAND_SEQ_ADDR_COUNT			= "";
// Block write/read stage
parameter BLOCK_RW_SEQ_ADDR_COUNT				= "";
parameter BLOCK_RW_RAND_ADDR_COUNT				= "";
parameter BLOCK_RW_RAND_SEQ_ADDR_COUNT			= "";
parameter BLOCK_RW_BLOCK_SIZE					= "";
// Template stage
parameter TEMPLATE_STAGE_COUNT					= "";

// ADDRESS GENERATORS PARAMETERS
// Sequential address generator
parameter SEQ_ADDR_GEN_MIN_BURSTCOUNT			= "";
parameter SEQ_ADDR_GEN_MAX_BURSTCOUNT			= "";
// Random address generator
parameter RAND_ADDR_GEN_MIN_BURSTCOUNT			= "";
parameter RAND_ADDR_GEN_MAX_BURSTCOUNT			= "";
// Mixed sequential/random address generator
parameter RAND_SEQ_ADDR_GEN_MIN_BURSTCOUNT		= "";
parameter RAND_SEQ_ADDR_GEN_MAX_BURSTCOUNT		= "";
parameter RAND_SEQ_ADDR_GEN_RAND_ADDR_PERCENT	= "";

// MEMORY INTERFACE PROPERTY
// The maximum read latency seen by the driver
// This parameter is used to determine buffer sizes, test stages may fail
// if the actual read latency is larger than specified by this parameter.
parameter MAX_READ_LATENCY						= "";

// NUM_DRIVER_LOOP
// Specifies the maximum number of loops through the driver patterns
// before asserting test complete. A setting of 0 will cause the driver to
// loop infinitely.
parameter NUM_DRIVER_LOOP   = "";

// END PARAMETER SECTION
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// BEGIN LOCALPARAM SECTION

// Determine the size of various FIFOs
localparam AVALON_TRAFFIC_BUFFER_SIZE	= 8;
localparam ADDR_BURSTCOUNT_FIFO_SIZE	= BLOCK_RW_BLOCK_SIZE;
localparam WRITTEN_DATA_FIFO_SIZE		= max(BLOCK_RW_BLOCK_SIZE*(1<<<(BURSTCOUNT_WIDTH-1)),MAX_READ_LATENCY)+AVALON_TRAFFIC_BUFFER_SIZE;

// The number of resynchronized resets to create at this level
localparam NUM_DRIVER_RESET = 8;

// END LOCALPARAM SECTION
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// BEGIN PORT SECTION

// Clock and reset
input							clk;
input							reset_n;

// Avalon master signals
input							avl_ready;
output							avl_write_req;
output							avl_read_req;
output							avl_burstbegin;
output	[ADDR_WIDTH-1:0]		avl_addr;
output	[BURSTCOUNT_WIDTH-1:0]	avl_size;
output	[BE_WIDTH-1:0]			avl_be;
output	[DATA_WIDTH-1:0]		avl_wdata;
input							avl_rdata_valid;
input 	[DATA_WIDTH-1:0]		avl_rdata;

// Driver status signals
output							pass;
output							fail;
output							test_complete;
output	[DATA_WIDTH-1:0]		pnf_per_bit;
output	[DATA_WIDTH-1:0]		pnf_per_bit_persist;

// END PORT SECTION
//////////////////////////////////////////////////////////////////////////////

// Resynchronized reset
wire	[NUM_DRIVER_RESET-1:0]	resync_reset_n;

// Driver status
reg		[DATA_WIDTH-1:0]		pnf_per_bit_persist;

// State machine signals
wire							do_write;
wire							do_read;
logic							can_write;
logic							can_read;
wire							timeout;
wire [31:0]                     loop_counter;
reg [31:0]                      loop_counter_persist;

// Address generator signals
addr_gen_select_t				addr_gen_select;
logic							addr_gen_enable;
wire							addr_gen_ready;
wire	[ADDR_WIDTH-1:0]		addr_gen_addr;
wire	[BURSTCOUNT_WIDTH-1:0]	addr_gen_burstcount;

// Address/burstcount FIFO signals
logic							addr_fifo_write_req;
logic							addr_fifo_read_req;
wire	[ADDR_WIDTH-1:0]		addr_fifo_addr;
wire	[BURSTCOUNT_WIDTH-1:0]	addr_fifo_burstcount;
// Avalon traffic generator signals
wire							traffic_gen_ready;

// Read compare signals
wire							read_compare_fifo_full;
wire							read_compare_fifo_empty;

// Address/data signals
logic	[ADDR_WIDTH-1:0]		read_addr;
logic	[BURSTCOUNT_WIDTH-1:0]	read_burstcount;
wire							wdata_req;
wire	[DATA_WIDTH-1:0]		wdata;
wire	[BE_WIDTH-1:0]			be;
wire							addr_burstcount_fifo_empty;


// Create a synchronized version of the reset against the driver clock
ddr2_v10_1_reset_sync	ureset_driver_clk(
	.reset_n		(reset_n),
	.clk			(clk),
	.reset_n_sync	(resync_reset_n)
);
defparam ureset_driver_clk.NUM_RESET_OUTPUT = NUM_DRIVER_RESET;



// Sticky per bit pnf
always_ff @(posedge clk or negedge resync_reset_n[0])
begin
	if (!resync_reset_n[0])
		pnf_per_bit_persist <= {DATA_WIDTH{1'b1}};
	else
		pnf_per_bit_persist <= pnf_per_bit_persist & pnf_per_bit;
end


// Generate status signals
assign pass = ~timeout & (&pnf_per_bit_persist);
assign fail = ~pass;


// Read address/burstcount select
// can_write and can_read indicates to the state machine whether
// other components are ready for issuing a write or read command
always_comb
begin
	if (ENABLE_READ_COMPARE == "TRUE")
	begin
		addr_gen_enable <= do_write;
		addr_fifo_write_req <= do_write;
		addr_fifo_read_req <= do_read;
		read_addr <= addr_fifo_addr;
		read_burstcount <= addr_fifo_burstcount;

		can_write <= traffic_gen_ready & addr_gen_ready;
		can_read <= traffic_gen_ready & ~addr_burstcount_fifo_empty;
	end
	else
	begin
		addr_gen_enable <= do_write | do_read;
		addr_fifo_write_req <= 1'b0;
		addr_fifo_read_req <= 1'b0;
		read_addr <= addr_gen_addr;
		read_burstcount <= addr_gen_burstcount;

		can_write <= traffic_gen_ready & addr_gen_ready;
		can_read <= traffic_gen_ready & addr_gen_ready;
	end
end


// Address generators
ddr2_v10_1_addr_gen addr_gen_inst (
	.clk				(clk),
	.reset_n			(resync_reset_n[1]),
	.addr_gen_select	(addr_gen_select),
	.enable				(addr_gen_enable),
	.ready				(addr_gen_ready),
	.addr				(addr_gen_addr),
	.burstcount			(addr_gen_burstcount));
defparam addr_gen_inst.ADDR_WIDTH							= ADDR_WIDTH;
defparam addr_gen_inst.DATA_WIDTH							= DATA_WIDTH;
defparam addr_gen_inst.BURSTCOUNT_WIDTH						= BURSTCOUNT_WIDTH;
defparam addr_gen_inst.POWER_OF_TWO_BURSTS_ONLY				= POWER_OF_TWO_BURSTS_ONLY;
defparam addr_gen_inst.BURST_ON_BURST_BOUNDARY				= BURST_ON_BURST_BOUNDARY;
defparam addr_gen_inst.GEN_BYTE_ADDR					= GEN_BYTE_ADDR;
defparam addr_gen_inst.SEQ_ADDR_GEN_MIN_BURSTCOUNT			= SEQ_ADDR_GEN_MIN_BURSTCOUNT;
defparam addr_gen_inst.SEQ_ADDR_GEN_MAX_BURSTCOUNT			= SEQ_ADDR_GEN_MAX_BURSTCOUNT;
defparam addr_gen_inst.RAND_ADDR_GEN_MIN_BURSTCOUNT			= RAND_ADDR_GEN_MIN_BURSTCOUNT;
defparam addr_gen_inst.RAND_ADDR_GEN_MAX_BURSTCOUNT			= RAND_ADDR_GEN_MAX_BURSTCOUNT;
defparam addr_gen_inst.RAND_SEQ_ADDR_GEN_MIN_BURSTCOUNT		= RAND_SEQ_ADDR_GEN_MIN_BURSTCOUNT;
defparam addr_gen_inst.RAND_SEQ_ADDR_GEN_MAX_BURSTCOUNT		= RAND_SEQ_ADDR_GEN_MAX_BURSTCOUNT;
defparam addr_gen_inst.RAND_SEQ_ADDR_GEN_RAND_ADDR_PERCENT	= RAND_SEQ_ADDR_GEN_RAND_ADDR_PERCENT;


// Pseudo-random data generator
ddr2_v10_1_lfsr_wrapper data_gen_inst (
	.clk		(clk),
	.reset_n	(resync_reset_n[2]),
	.enable		(wdata_req),
	.data		(wdata));
defparam data_gen_inst.DATA_WIDTH	= DATA_WIDTH;


// Byte enable generator
generate
if (RANDOM_BYTE_ENABLE == "TRUE")
begin : be_gen
	ddr2_v10_1_lfsr_wrapper be_gen_inst (
		.clk		(clk),
		.reset_n	(resync_reset_n[3]),
		.enable		(wdata_req),
		.data		(be));
	defparam be_gen_inst.DATA_WIDTH	= BE_WIDTH;
end
else
begin : be_const
	assign be = {BE_WIDTH{1'b1}};
end
endgenerate


// The address/burstcount FIFO buffers the write addresses
// and burstcounts which are later used in read operations
ddr2_v10_1_scfifo_wrapper addr_burstcount_fifo (
	.clk		(clk),
	.reset_n	(resync_reset_n[4]),
	.write_req	(addr_fifo_write_req),
	.read_req	(addr_fifo_read_req),
	.data_in	({addr_gen_addr,addr_gen_burstcount}),
	.data_out	({addr_fifo_addr,addr_fifo_burstcount}),
	.full		(),
	.empty		(addr_burstcount_fifo_empty));
defparam addr_burstcount_fifo.DEVICE_FAMILY	= DEVICE_FAMILY;
defparam addr_burstcount_fifo.FIFO_WIDTH	= ADDR_WIDTH + BURSTCOUNT_WIDTH;
defparam addr_burstcount_fifo.FIFO_SIZE		= ADDR_BURSTCOUNT_FIFO_SIZE;
defparam addr_burstcount_fifo.SHOW_AHEAD	= "ON";


// The main state machine of the example driver,
// which contains sub-modules for various test stages
ddr2_v10_1_driver_fsm driver_fsm_inst (
	.clk						(clk),
	.reset_n					(resync_reset_n[5]),
	.can_write					(can_write),
	.can_read					(can_read),
	.read_compare_fifo_full		(read_compare_fifo_full),
	.read_compare_fifo_empty	(read_compare_fifo_empty),
	.addr_gen_select			(addr_gen_select),
	.do_write					(do_write),
	.do_read					(do_read),
	.test_complete				(test_complete),
	.loop_counter               (loop_counter),
	.timeout					(timeout));
defparam driver_fsm_inst.SINGLE_RW_SEQ_ADDR_COUNT		= SINGLE_RW_SEQ_ADDR_COUNT;
defparam driver_fsm_inst.SINGLE_RW_RAND_ADDR_COUNT		= SINGLE_RW_RAND_ADDR_COUNT;
defparam driver_fsm_inst.SINGLE_RW_RAND_SEQ_ADDR_COUNT	= SINGLE_RW_RAND_SEQ_ADDR_COUNT;
defparam driver_fsm_inst.BLOCK_RW_SEQ_ADDR_COUNT		= BLOCK_RW_SEQ_ADDR_COUNT;
defparam driver_fsm_inst.BLOCK_RW_RAND_ADDR_COUNT		= BLOCK_RW_RAND_ADDR_COUNT;
defparam driver_fsm_inst.BLOCK_RW_RAND_SEQ_ADDR_COUNT	= BLOCK_RW_RAND_SEQ_ADDR_COUNT;
defparam driver_fsm_inst.BLOCK_RW_BLOCK_SIZE			= BLOCK_RW_BLOCK_SIZE;
defparam driver_fsm_inst.TEMPLATE_STAGE_COUNT			= TEMPLATE_STAGE_COUNT;
defparam driver_fsm_inst.TIMEOUT_COUNTER_WIDTH			= TIMEOUT_COUNTER_WIDTH;
defparam driver_fsm_inst.NUM_DRIVER_LOOP                = NUM_DRIVER_LOOP;

// The Avalon traffic generator translates the commands
// issued by the state machine into Avalon signals
ddr2_v10_1_avalon_traffic_gen avalon_traffic_gen_inst (
	.clk				(clk),
	.reset_n			(resync_reset_n[6]),
	.avl_ready			(avl_ready),
	.avl_write_req		(avl_write_req),
	.avl_read_req		(avl_read_req),
	.avl_burstbegin		(avl_burstbegin),
	.avl_addr			(avl_addr),
	.avl_size			(avl_size),
	.avl_wdata			(avl_wdata),
	.avl_be				(avl_be),
	.do_write			(do_write),
	.do_read			(do_read),
	.write_addr			(addr_gen_addr),
	.write_burstcount	(addr_gen_burstcount),
	.wdata				(wdata),
	.be					(be),
	.read_addr			(read_addr),
	.read_burstcount	(read_burstcount),
	.ready				(traffic_gen_ready),
	.wdata_req			(wdata_req));
defparam avalon_traffic_gen_inst.DEVICE_FAMILY		= DEVICE_FAMILY;
defparam avalon_traffic_gen_inst.ADDR_WIDTH			= ADDR_WIDTH;
defparam avalon_traffic_gen_inst.BURSTCOUNT_WIDTH	= BURSTCOUNT_WIDTH;
defparam avalon_traffic_gen_inst.DATA_WIDTH			= DATA_WIDTH;
defparam avalon_traffic_gen_inst.BE_WIDTH			= BE_WIDTH;
defparam avalon_traffic_gen_inst.BUFFER_SIZE		= AVALON_TRAFFIC_BUFFER_SIZE;
defparam avalon_traffic_gen_inst.RANDOM_BYTE_ENABLE = RANDOM_BYTE_ENABLE;

// Read compare module
ddr2_v10_1_read_compare read_compare_inst (
	.clk						(clk),
	.reset_n					(resync_reset_n[7]),
	.enable						((ENABLE_READ_COMPARE == "TRUE")),
	.wdata_req					(wdata_req),
	.wdata						(wdata),
	.be							(be),
	.rdata_valid				(avl_rdata_valid),
	.rdata						(avl_rdata),
	.read_compare_fifo_full		(read_compare_fifo_full),
	.read_compare_fifo_empty	(read_compare_fifo_empty),
	.pnf_per_bit				(pnf_per_bit));
defparam read_compare_inst.DATA_WIDTH				= DATA_WIDTH;
defparam read_compare_inst.BE_WIDTH					= BE_WIDTH;
defparam read_compare_inst.WRITTEN_DATA_FIFO_SIZE	= WRITTEN_DATA_FIFO_SIZE;
defparam read_compare_inst.DEVICE_FAMILY			= DEVICE_FAMILY;


`ifdef ENABLE_ISS_PROBES
ddr2_v10_1_iss_probe #(
	.WIDTH(DATA_WIDTH)
) pnf_per_bit_probe (
	.probe_input(pnf_per_bit)
);

ddr2_v10_1_iss_probe #(
	.WIDTH(DATA_WIDTH)
) pnf_per_bit_persist_probe (
	.probe_input(pnf_per_bit_persist)
);

ddr2_v10_1_iss_probe #(
	.WIDTH(1)
) driver_pass_probe (
	.probe_input(pass)
);

ddr2_v10_1_iss_probe #(
	.WIDTH(1)
) driver_fail_probe (
	.probe_input(fail)
);

ddr2_v10_1_iss_probe #(
	.WIDTH(1)
) driver_timeout_probe (
	.probe_input(timeout)
);

ddr2_v10_1_iss_probe #(
	.WIDTH(1)
) driver_test_complete_probe (
	.probe_input(test_complete)
);

// Loop counter stopping on failure
always_ff @(posedge clk)
begin
	if (pass)
		loop_counter_persist <= loop_counter;
end

ddr2_v10_1_iss_probe #(
	.WIDTH(32)
) driver_loop_counter_probe (
	.probe_input(loop_counter_persist)
);
`endif



// Simulation assertions
// synthesis translate_off
initial
begin
	assert (POWER_OF_TWO_BURSTS_ONLY == "TRUE" || POWER_OF_TWO_BURSTS_ONLY == "FALSE")
		else $error ("POWER_OF_TWO_BURSTS_ONLY must be TRUE or FALSE");
	assert (BURST_ON_BURST_BOUNDARY == "TRUE" || BURST_ON_BURST_BOUNDARY == "FALSE")
		else $error ("BURST_ON_BURST_BOUNDARY must be TRUE or FALSE");
	assert (RANDOM_BYTE_ENABLE == "TRUE" || RANDOM_BYTE_ENABLE == "FALSE")
		else $error ("RANDOM_BYTE_ENABLE must be TRUE or FALSE");
end
// synthesis translate_on


endmodule

