//altiobuf_in CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIV" ENABLE_BUS_HOLD="FALSE" NUMBER_OF_CHANNELS=1 USE_DIFFERENTIAL_MODE="FALSE" datain dataout
//VERSION_BEGIN 10.1SP1 cbx_altiobuf_in 2011:01:19:21:23:41:SJ cbx_mgl 2011:01:19:21:24:50:SJ cbx_stratixiii 2011:01:19:21:23:41:SJ cbx_stratixv 2011:01:19:21:23:41:SJ  VERSION_END
// synthesis VERILOG_INPUT_VERSION VERILOG_2001
// altera message_off 10463



// Copyright (C) 1991-2011 Altera Corporation
//  Your use of Altera Corporation's design tools, logic functions 
//  and other software and tools, and its AMPP partner logic 
//  functions, and any output files from any of the foregoing 
//  (including device programming or simulation files), and any 
//  associated documentation or information are expressly subject 
//  to the terms and conditions of the Altera Program License 
//  Subscription Agreement, Altera MegaCore Function License 
//  Agreement, or other applicable license agreement, including, 
//  without limitation, that your use is for the sole purpose of 
//  programming logic devices manufactured by Altera and sold by 
//  Altera or its authorized distributors.  Please refer to the 
//  applicable agreement for further details.



//synthesis_resources = stratixiv_io_ibuf 1 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_0002_mem_cq_buf
	( 
	datain,
	dataout) /* synthesis synthesis_clearbox=1 */;
	input   [0:0]  datain;
	output   [0:0]  dataout;

	wire  [0:0]   wire_ibufa_o;

	stratixiv_io_ibuf   ibufa_0
	( 
	.i(datain),
	.o(wire_ibufa_o[0:0])
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.dynamicterminationcontrol(1'b0),
	.ibar(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	);
	defparam
		ibufa_0.bus_hold = "false",
		ibufa_0.differential_mode = "false",
		ibufa_0.lpm_type = "stratixiv_io_ibuf";
	assign
		dataout = wire_ibufa_o;
endmodule //ddr2_v10_1_0002_mem_cq_buf
//VALID FILE
