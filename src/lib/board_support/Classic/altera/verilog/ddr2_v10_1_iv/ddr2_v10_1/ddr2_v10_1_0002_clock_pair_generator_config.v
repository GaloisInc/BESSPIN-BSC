//altiobuf_bidir CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIV" ENABLE_BUS_HOLD="FALSE" NUMBER_OF_CHANNELS=1 OPEN_DRAIN_OUTPUT="FALSE" USE_DIFFERENTIAL_MODE="TRUE" USE_DYNAMIC_TERMINATION_CONTROL="TRUE" USE_TERMINATION_CONTROL="TRUE" WIDTH_PTC=14 WIDTH_STC=14 datain dataio dataio_b dataout dynamicterminationcontrol dynamicterminationcontrol_b oe oe_b parallelterminationcontrol parallelterminationcontrol_b seriesterminationcontrol seriesterminationcontrol_b
//VERSION_BEGIN 10.1SP1 cbx_altiobuf_bidir 2011:01:19:21:23:41:SJ cbx_mgl 2011:01:19:21:24:50:SJ cbx_stratixiii 2011:01:19:21:23:41:SJ cbx_stratixv 2011:01:19:21:23:41:SJ  VERSION_END
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



//synthesis_resources = stratixiv_io_ibuf 1 stratixiv_io_obuf 2 stratixiv_pseudo_diff_out 1 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_0002_clock_pair_generator_config
	( 
	datain,
	dataio,
	dataio_b,
	dataout,
	dynamicterminationcontrol,
	dynamicterminationcontrol_b,
	oe,
	oe_b,
	parallelterminationcontrol,
	parallelterminationcontrol_b,
	seriesterminationcontrol,
	seriesterminationcontrol_b) /* synthesis synthesis_clearbox=1 */;
	input   [0:0]  datain;
	inout   [0:0]  dataio;
	inout   [0:0]  dataio_b;
	output   [0:0]  dataout;
	input   [0:0]  dynamicterminationcontrol;
	input   [0:0]  dynamicterminationcontrol_b;
	input   [0:0]  oe;
	input   [0:0]  oe_b;
	input   [13:0]  parallelterminationcontrol;
	input   [13:0]  parallelterminationcontrol_b;
	input   [13:0]  seriesterminationcontrol;
	input   [13:0]  seriesterminationcontrol_b;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   [0:0]  dynamicterminationcontrol;
	tri0   [0:0]  dynamicterminationcontrol_b;
	tri1   [0:0]  oe_b;
	tri0   [13:0]  parallelterminationcontrol;
	tri0   [13:0]  parallelterminationcontrol_b;
	tri0   [13:0]  seriesterminationcontrol;
	tri0   [13:0]  seriesterminationcontrol_b;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire  [0:0]   wire_ibufa_o;
	wire  [0:0]   wire_obuf_ba_o;
	wire  [0:0]   wire_obufa_o;
	wire  [0:0]   wire_pseudo_diffa_o;
	wire  [0:0]   wire_pseudo_diffa_obar;

	stratixiv_io_ibuf   ibufa_0
	( 
	.i(dataio),
	.ibar(dataio_b),
	.o(wire_ibufa_o[0:0])
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.dynamicterminationcontrol(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	);
	defparam
		ibufa_0.bus_hold = "false",
		ibufa_0.lpm_type = "stratixiv_io_ibuf";
	stratixiv_io_obuf   obuf_ba_0
	( 
	.dynamicterminationcontrol(dynamicterminationcontrol_b),
	.i(wire_pseudo_diffa_obar),
	.o(wire_obuf_ba_o[0:0]),
	.obar(),
	.oe(oe_b),
	.parallelterminationcontrol(parallelterminationcontrol_b),
	.seriesterminationcontrol(seriesterminationcontrol_b)
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obuf_ba_0.bus_hold = "false",
		obuf_ba_0.open_drain_output = "false",
		obuf_ba_0.lpm_type = "stratixiv_io_obuf";
	stratixiv_io_obuf   obufa_0
	( 
	.dynamicterminationcontrol(dynamicterminationcontrol),
	.i(wire_pseudo_diffa_o),
	.o(wire_obufa_o[0:0]),
	.obar(),
	.oe(oe),
	.parallelterminationcontrol(parallelterminationcontrol),
	.seriesterminationcontrol(seriesterminationcontrol)
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_0.bus_hold = "false",
		obufa_0.open_drain_output = "false",
		obufa_0.lpm_type = "stratixiv_io_obuf";
	stratixiv_pseudo_diff_out   pseudo_diffa_0
	( 
	.i(datain),
	.o(wire_pseudo_diffa_o[0:0]),
	.obar(wire_pseudo_diffa_obar[0:0]));
	assign
		dataio = wire_obufa_o,
		dataio_b = wire_obuf_ba_o,
		dataout = wire_ibufa_o;
endmodule //ddr2_v10_1_0002_clock_pair_generator_config
//VALID FILE
