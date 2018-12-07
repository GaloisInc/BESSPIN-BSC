//altiobuf_out CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIII" ENABLE_BUS_HOLD="FALSE" NUMBER_OF_CHANNELS=1 OPEN_DRAIN_OUTPUT="FALSE" PSEUDO_DIFFERENTIAL_MODE="FALSE" USE_DIFFERENTIAL_MODE="FALSE" USE_OE="FALSE" USE_OUT_DYNAMIC_DELAY_CHAIN1="FALSE" USE_OUT_DYNAMIC_DELAY_CHAIN2="FALSE" USE_TERMINATION_CONTROL="TRUE" WIDTH_PTC=14 WIDTH_STC=14 datain dataout parallelterminationcontrol seriesterminationcontrol
//VERSION_BEGIN 10.1SP1 cbx_altiobuf_out 2011:01:19:21:23:41:SJ cbx_mgl 2011:01:19:21:24:50:SJ cbx_stratixiii 2011:01:19:21:23:41:SJ cbx_stratixv 2011:01:19:21:23:41:SJ  VERSION_END
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



//synthesis_resources = stratixiii_io_obuf 1 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_0002_write_mask_iobuf
	( 
	datain,
	dataout,
	parallelterminationcontrol,
	seriesterminationcontrol) /* synthesis synthesis_clearbox=1 */;
	input   [0:0]  datain;
	output   [0:0]  dataout;
	input   [13:0]  parallelterminationcontrol;
	input   [13:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   [13:0]  parallelterminationcontrol;
	tri0   [13:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire  [0:0]   wire_obufa_o;
	wire  [0:0]  oe_w;

	stratixiii_io_obuf   obufa_0
	( 
	.i(datain),
	.o(wire_obufa_o[0:0]),
	.obar(),
	.oe(oe_w),
	.parallelterminationcontrol(parallelterminationcontrol),
	.seriesterminationcontrol(seriesterminationcontrol)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.dynamicterminationcontrol(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_0.bus_hold = "false",
		obufa_0.open_drain_output = "false",
		obufa_0.shift_series_termination_control = "false",
		obufa_0.lpm_type = "stratixiii_io_obuf";
	assign
		dataout = wire_obufa_o,
		oe_w = 1'b1;
endmodule //ddr2_v10_1_0002_write_mask_iobuf
//VALID FILE
