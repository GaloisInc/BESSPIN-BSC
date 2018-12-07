// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_example_driver (
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
	avl_rdata,
	avl_rdata_valid,
`ifdef PNF_PER_BIT_OUTPUT
	pnf_per_bit,
	pnf_per_bit_persist,
`endif
	pass,
	fail,
	test_complete
);

 
input								clk;
input								reset_n;
input								avl_ready;

output								avl_write_req;
output								avl_read_req;
output								avl_burstbegin;
output	[30-1:0]	avl_addr;
output	[3-1:0]	avl_size;
output	[32-1:0]	avl_be;
output	[256-1:0]		avl_wdata;
input								avl_rdata_valid;
input	[256-1:0]		avl_rdata;

output  							pass;
output           					fail;
output           					test_complete;
`ifdef PNF_PER_BIT_OUTPUT
output	[256-1:0]		pnf_per_bit;
output	[256-1:0]		pnf_per_bit_persist;
`else
wire	[256-1:0]		pnf_per_bit;
wire	[256-1:0]		pnf_per_bit_persist;
`endif

wire								driver_afi_clk;
assign driver_afi_clk = clk;




ddr2_v10_1_driver driver_inst (
	.clk					(driver_afi_clk),
	.reset_n				(reset_n),
	.avl_ready				(avl_ready),
	.avl_write_req			(avl_write_req),
	.avl_read_req			(avl_read_req),
	.avl_burstbegin			(avl_burstbegin),
	.avl_addr				(avl_addr),
	.avl_size				(avl_size),
	.avl_be					(avl_be),
	.avl_wdata				(avl_wdata),
	.avl_rdata_valid		(avl_rdata_valid),
	.avl_rdata				(avl_rdata),
	.pass					(pass),
	.fail					(fail),
	.test_complete			(test_complete),
	.pnf_per_bit			(pnf_per_bit),
	.pnf_per_bit_persist	(pnf_per_bit_persist));
defparam driver_inst.DEVICE_FAMILY							= "StratixIV";
defparam driver_inst.ADDR_WIDTH								= 30;
defparam driver_inst.BURSTCOUNT_WIDTH						= 3;
defparam driver_inst.DATA_WIDTH								= 256;
defparam driver_inst.BE_WIDTH								= 32;
defparam driver_inst.RANDOM_BYTE_ENABLE						= "TRUE";
defparam driver_inst.POWER_OF_TWO_BURSTS_ONLY				= "FALSE";
defparam driver_inst.BURST_ON_BURST_BOUNDARY				= "FALSE";
defparam driver_inst.GEN_BYTE_ADDR					= "FALSE";
defparam driver_inst.ENABLE_READ_COMPARE					= "TRUE";
defparam driver_inst.TIMEOUT_COUNTER_WIDTH					= 30;
defparam driver_inst.MAX_READ_LATENCY						= 20;
defparam driver_inst.SINGLE_RW_SEQ_ADDR_COUNT				= 32;
defparam driver_inst.SINGLE_RW_RAND_ADDR_COUNT				= 32;
defparam driver_inst.SINGLE_RW_RAND_SEQ_ADDR_COUNT			= 32;
defparam driver_inst.BLOCK_RW_SEQ_ADDR_COUNT				= 8;
defparam driver_inst.BLOCK_RW_RAND_ADDR_COUNT				= 8;
defparam driver_inst.BLOCK_RW_RAND_SEQ_ADDR_COUNT			= 8;
defparam driver_inst.BLOCK_RW_BLOCK_SIZE					= 8;
defparam driver_inst.TEMPLATE_STAGE_COUNT					= 4;
defparam driver_inst.SEQ_ADDR_GEN_MIN_BURSTCOUNT			= 1;
defparam driver_inst.SEQ_ADDR_GEN_MAX_BURSTCOUNT			= (1<<(3-1));
defparam driver_inst.RAND_ADDR_GEN_MIN_BURSTCOUNT			= 1;
defparam driver_inst.RAND_ADDR_GEN_MAX_BURSTCOUNT			= (1<<(3-1));
defparam driver_inst.RAND_SEQ_ADDR_GEN_MIN_BURSTCOUNT		= 1;
defparam driver_inst.RAND_SEQ_ADDR_GEN_MAX_BURSTCOUNT		= (1<<(3-1));
defparam driver_inst.RAND_SEQ_ADDR_GEN_RAND_ADDR_PERCENT	= 50;
defparam driver_inst.NUM_DRIVER_LOOP                        = 1;

endmodule

