//altiobuf_bidir CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIII" ENABLE_BUS_HOLD="FALSE" NUMBER_OF_CHANNELS=64 OPEN_DRAIN_OUTPUT="FALSE" USE_DIFFERENTIAL_MODE="FALSE" USE_DYNAMIC_TERMINATION_CONTROL="TRUE" USE_TERMINATION_CONTROL="TRUE" WIDTH_PTC=14 WIDTH_STC=14 datain dataio dataout dynamicterminationcontrol oe parallelterminationcontrol seriesterminationcontrol
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



//synthesis_resources = stratixiii_io_ibuf 64 stratixiii_io_obuf 64 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_write_dq_iobuf
	( 
	datain,
	dataio,
	dataout,
	dynamicterminationcontrol,
	oe,
	parallelterminationcontrol,
	seriesterminationcontrol) /* synthesis synthesis_clearbox=1 */;
	input   [63:0]  datain;
	inout   [63:0]  dataio;
	output   [63:0]  dataout;
	input   [63:0]  dynamicterminationcontrol;
	input   [63:0]  oe;
	input   [895:0]  parallelterminationcontrol;
	input   [895:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   [63:0]  dynamicterminationcontrol;
	tri0   [895:0]  parallelterminationcontrol;
	tri0   [895:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire  [63:0]   wire_ibufa_i;
	wire  [63:0]   wire_ibufa_o;
	wire  [63:0]   wire_obufa_dynamicterminationcontrol;
	wire  [63:0]   wire_obufa_i;
	wire  [63:0]   wire_obufa_o;
	wire  [63:0]   wire_obufa_oe;
	wire  [895:0]   wire_obufa_parallelterminationcontrol;
	wire  [895:0]   wire_obufa_seriesterminationcontrol;

	stratixiii_io_ibuf   ibufa_0
	( 
	.i(wire_ibufa_i[0:0]),
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
		ibufa_0.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_1
	( 
	.i(wire_ibufa_i[1:1]),
	.o(wire_ibufa_o[1:1])
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
		ibufa_1.bus_hold = "false",
		ibufa_1.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_2
	( 
	.i(wire_ibufa_i[2:2]),
	.o(wire_ibufa_o[2:2])
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
		ibufa_2.bus_hold = "false",
		ibufa_2.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_3
	( 
	.i(wire_ibufa_i[3:3]),
	.o(wire_ibufa_o[3:3])
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
		ibufa_3.bus_hold = "false",
		ibufa_3.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_4
	( 
	.i(wire_ibufa_i[4:4]),
	.o(wire_ibufa_o[4:4])
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
		ibufa_4.bus_hold = "false",
		ibufa_4.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_5
	( 
	.i(wire_ibufa_i[5:5]),
	.o(wire_ibufa_o[5:5])
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
		ibufa_5.bus_hold = "false",
		ibufa_5.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_6
	( 
	.i(wire_ibufa_i[6:6]),
	.o(wire_ibufa_o[6:6])
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
		ibufa_6.bus_hold = "false",
		ibufa_6.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_7
	( 
	.i(wire_ibufa_i[7:7]),
	.o(wire_ibufa_o[7:7])
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
		ibufa_7.bus_hold = "false",
		ibufa_7.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_8
	( 
	.i(wire_ibufa_i[8:8]),
	.o(wire_ibufa_o[8:8])
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
		ibufa_8.bus_hold = "false",
		ibufa_8.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_9
	( 
	.i(wire_ibufa_i[9:9]),
	.o(wire_ibufa_o[9:9])
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
		ibufa_9.bus_hold = "false",
		ibufa_9.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_10
	( 
	.i(wire_ibufa_i[10:10]),
	.o(wire_ibufa_o[10:10])
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
		ibufa_10.bus_hold = "false",
		ibufa_10.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_11
	( 
	.i(wire_ibufa_i[11:11]),
	.o(wire_ibufa_o[11:11])
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
		ibufa_11.bus_hold = "false",
		ibufa_11.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_12
	( 
	.i(wire_ibufa_i[12:12]),
	.o(wire_ibufa_o[12:12])
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
		ibufa_12.bus_hold = "false",
		ibufa_12.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_13
	( 
	.i(wire_ibufa_i[13:13]),
	.o(wire_ibufa_o[13:13])
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
		ibufa_13.bus_hold = "false",
		ibufa_13.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_14
	( 
	.i(wire_ibufa_i[14:14]),
	.o(wire_ibufa_o[14:14])
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
		ibufa_14.bus_hold = "false",
		ibufa_14.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_15
	( 
	.i(wire_ibufa_i[15:15]),
	.o(wire_ibufa_o[15:15])
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
		ibufa_15.bus_hold = "false",
		ibufa_15.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_16
	( 
	.i(wire_ibufa_i[16:16]),
	.o(wire_ibufa_o[16:16])
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
		ibufa_16.bus_hold = "false",
		ibufa_16.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_17
	( 
	.i(wire_ibufa_i[17:17]),
	.o(wire_ibufa_o[17:17])
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
		ibufa_17.bus_hold = "false",
		ibufa_17.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_18
	( 
	.i(wire_ibufa_i[18:18]),
	.o(wire_ibufa_o[18:18])
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
		ibufa_18.bus_hold = "false",
		ibufa_18.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_19
	( 
	.i(wire_ibufa_i[19:19]),
	.o(wire_ibufa_o[19:19])
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
		ibufa_19.bus_hold = "false",
		ibufa_19.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_20
	( 
	.i(wire_ibufa_i[20:20]),
	.o(wire_ibufa_o[20:20])
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
		ibufa_20.bus_hold = "false",
		ibufa_20.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_21
	( 
	.i(wire_ibufa_i[21:21]),
	.o(wire_ibufa_o[21:21])
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
		ibufa_21.bus_hold = "false",
		ibufa_21.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_22
	( 
	.i(wire_ibufa_i[22:22]),
	.o(wire_ibufa_o[22:22])
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
		ibufa_22.bus_hold = "false",
		ibufa_22.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_23
	( 
	.i(wire_ibufa_i[23:23]),
	.o(wire_ibufa_o[23:23])
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
		ibufa_23.bus_hold = "false",
		ibufa_23.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_24
	( 
	.i(wire_ibufa_i[24:24]),
	.o(wire_ibufa_o[24:24])
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
		ibufa_24.bus_hold = "false",
		ibufa_24.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_25
	( 
	.i(wire_ibufa_i[25:25]),
	.o(wire_ibufa_o[25:25])
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
		ibufa_25.bus_hold = "false",
		ibufa_25.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_26
	( 
	.i(wire_ibufa_i[26:26]),
	.o(wire_ibufa_o[26:26])
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
		ibufa_26.bus_hold = "false",
		ibufa_26.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_27
	( 
	.i(wire_ibufa_i[27:27]),
	.o(wire_ibufa_o[27:27])
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
		ibufa_27.bus_hold = "false",
		ibufa_27.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_28
	( 
	.i(wire_ibufa_i[28:28]),
	.o(wire_ibufa_o[28:28])
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
		ibufa_28.bus_hold = "false",
		ibufa_28.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_29
	( 
	.i(wire_ibufa_i[29:29]),
	.o(wire_ibufa_o[29:29])
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
		ibufa_29.bus_hold = "false",
		ibufa_29.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_30
	( 
	.i(wire_ibufa_i[30:30]),
	.o(wire_ibufa_o[30:30])
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
		ibufa_30.bus_hold = "false",
		ibufa_30.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_31
	( 
	.i(wire_ibufa_i[31:31]),
	.o(wire_ibufa_o[31:31])
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
		ibufa_31.bus_hold = "false",
		ibufa_31.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_32
	( 
	.i(wire_ibufa_i[32:32]),
	.o(wire_ibufa_o[32:32])
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
		ibufa_32.bus_hold = "false",
		ibufa_32.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_33
	( 
	.i(wire_ibufa_i[33:33]),
	.o(wire_ibufa_o[33:33])
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
		ibufa_33.bus_hold = "false",
		ibufa_33.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_34
	( 
	.i(wire_ibufa_i[34:34]),
	.o(wire_ibufa_o[34:34])
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
		ibufa_34.bus_hold = "false",
		ibufa_34.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_35
	( 
	.i(wire_ibufa_i[35:35]),
	.o(wire_ibufa_o[35:35])
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
		ibufa_35.bus_hold = "false",
		ibufa_35.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_36
	( 
	.i(wire_ibufa_i[36:36]),
	.o(wire_ibufa_o[36:36])
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
		ibufa_36.bus_hold = "false",
		ibufa_36.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_37
	( 
	.i(wire_ibufa_i[37:37]),
	.o(wire_ibufa_o[37:37])
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
		ibufa_37.bus_hold = "false",
		ibufa_37.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_38
	( 
	.i(wire_ibufa_i[38:38]),
	.o(wire_ibufa_o[38:38])
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
		ibufa_38.bus_hold = "false",
		ibufa_38.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_39
	( 
	.i(wire_ibufa_i[39:39]),
	.o(wire_ibufa_o[39:39])
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
		ibufa_39.bus_hold = "false",
		ibufa_39.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_40
	( 
	.i(wire_ibufa_i[40:40]),
	.o(wire_ibufa_o[40:40])
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
		ibufa_40.bus_hold = "false",
		ibufa_40.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_41
	( 
	.i(wire_ibufa_i[41:41]),
	.o(wire_ibufa_o[41:41])
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
		ibufa_41.bus_hold = "false",
		ibufa_41.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_42
	( 
	.i(wire_ibufa_i[42:42]),
	.o(wire_ibufa_o[42:42])
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
		ibufa_42.bus_hold = "false",
		ibufa_42.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_43
	( 
	.i(wire_ibufa_i[43:43]),
	.o(wire_ibufa_o[43:43])
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
		ibufa_43.bus_hold = "false",
		ibufa_43.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_44
	( 
	.i(wire_ibufa_i[44:44]),
	.o(wire_ibufa_o[44:44])
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
		ibufa_44.bus_hold = "false",
		ibufa_44.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_45
	( 
	.i(wire_ibufa_i[45:45]),
	.o(wire_ibufa_o[45:45])
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
		ibufa_45.bus_hold = "false",
		ibufa_45.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_46
	( 
	.i(wire_ibufa_i[46:46]),
	.o(wire_ibufa_o[46:46])
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
		ibufa_46.bus_hold = "false",
		ibufa_46.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_47
	( 
	.i(wire_ibufa_i[47:47]),
	.o(wire_ibufa_o[47:47])
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
		ibufa_47.bus_hold = "false",
		ibufa_47.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_48
	( 
	.i(wire_ibufa_i[48:48]),
	.o(wire_ibufa_o[48:48])
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
		ibufa_48.bus_hold = "false",
		ibufa_48.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_49
	( 
	.i(wire_ibufa_i[49:49]),
	.o(wire_ibufa_o[49:49])
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
		ibufa_49.bus_hold = "false",
		ibufa_49.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_50
	( 
	.i(wire_ibufa_i[50:50]),
	.o(wire_ibufa_o[50:50])
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
		ibufa_50.bus_hold = "false",
		ibufa_50.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_51
	( 
	.i(wire_ibufa_i[51:51]),
	.o(wire_ibufa_o[51:51])
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
		ibufa_51.bus_hold = "false",
		ibufa_51.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_52
	( 
	.i(wire_ibufa_i[52:52]),
	.o(wire_ibufa_o[52:52])
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
		ibufa_52.bus_hold = "false",
		ibufa_52.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_53
	( 
	.i(wire_ibufa_i[53:53]),
	.o(wire_ibufa_o[53:53])
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
		ibufa_53.bus_hold = "false",
		ibufa_53.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_54
	( 
	.i(wire_ibufa_i[54:54]),
	.o(wire_ibufa_o[54:54])
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
		ibufa_54.bus_hold = "false",
		ibufa_54.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_55
	( 
	.i(wire_ibufa_i[55:55]),
	.o(wire_ibufa_o[55:55])
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
		ibufa_55.bus_hold = "false",
		ibufa_55.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_56
	( 
	.i(wire_ibufa_i[56:56]),
	.o(wire_ibufa_o[56:56])
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
		ibufa_56.bus_hold = "false",
		ibufa_56.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_57
	( 
	.i(wire_ibufa_i[57:57]),
	.o(wire_ibufa_o[57:57])
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
		ibufa_57.bus_hold = "false",
		ibufa_57.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_58
	( 
	.i(wire_ibufa_i[58:58]),
	.o(wire_ibufa_o[58:58])
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
		ibufa_58.bus_hold = "false",
		ibufa_58.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_59
	( 
	.i(wire_ibufa_i[59:59]),
	.o(wire_ibufa_o[59:59])
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
		ibufa_59.bus_hold = "false",
		ibufa_59.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_60
	( 
	.i(wire_ibufa_i[60:60]),
	.o(wire_ibufa_o[60:60])
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
		ibufa_60.bus_hold = "false",
		ibufa_60.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_61
	( 
	.i(wire_ibufa_i[61:61]),
	.o(wire_ibufa_o[61:61])
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
		ibufa_61.bus_hold = "false",
		ibufa_61.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_62
	( 
	.i(wire_ibufa_i[62:62]),
	.o(wire_ibufa_o[62:62])
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
		ibufa_62.bus_hold = "false",
		ibufa_62.lpm_type = "stratixiii_io_ibuf";
	stratixiii_io_ibuf   ibufa_63
	( 
	.i(wire_ibufa_i[63:63]),
	.o(wire_ibufa_o[63:63])
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
		ibufa_63.bus_hold = "false",
		ibufa_63.lpm_type = "stratixiii_io_ibuf";
	assign
		wire_ibufa_i = dataio;
	stratixiii_io_obuf   obufa_0
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[0:0]),
	.i(wire_obufa_i[0:0]),
	.o(wire_obufa_o[0:0]),
	.obar(),
	.oe(wire_obufa_oe[0:0]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[13:0]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[13:0])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_0.bus_hold = "false",
		obufa_0.open_drain_output = "false",
		obufa_0.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_1
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[1:1]),
	.i(wire_obufa_i[1:1]),
	.o(wire_obufa_o[1:1]),
	.obar(),
	.oe(wire_obufa_oe[1:1]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[27:14]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[27:14])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_1.bus_hold = "false",
		obufa_1.open_drain_output = "false",
		obufa_1.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_2
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[2:2]),
	.i(wire_obufa_i[2:2]),
	.o(wire_obufa_o[2:2]),
	.obar(),
	.oe(wire_obufa_oe[2:2]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[41:28]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[41:28])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_2.bus_hold = "false",
		obufa_2.open_drain_output = "false",
		obufa_2.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_3
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[3:3]),
	.i(wire_obufa_i[3:3]),
	.o(wire_obufa_o[3:3]),
	.obar(),
	.oe(wire_obufa_oe[3:3]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[55:42]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[55:42])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_3.bus_hold = "false",
		obufa_3.open_drain_output = "false",
		obufa_3.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_4
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[4:4]),
	.i(wire_obufa_i[4:4]),
	.o(wire_obufa_o[4:4]),
	.obar(),
	.oe(wire_obufa_oe[4:4]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[69:56]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[69:56])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_4.bus_hold = "false",
		obufa_4.open_drain_output = "false",
		obufa_4.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_5
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[5:5]),
	.i(wire_obufa_i[5:5]),
	.o(wire_obufa_o[5:5]),
	.obar(),
	.oe(wire_obufa_oe[5:5]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[83:70]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[83:70])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_5.bus_hold = "false",
		obufa_5.open_drain_output = "false",
		obufa_5.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_6
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[6:6]),
	.i(wire_obufa_i[6:6]),
	.o(wire_obufa_o[6:6]),
	.obar(),
	.oe(wire_obufa_oe[6:6]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[97:84]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[97:84])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_6.bus_hold = "false",
		obufa_6.open_drain_output = "false",
		obufa_6.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_7
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[7:7]),
	.i(wire_obufa_i[7:7]),
	.o(wire_obufa_o[7:7]),
	.obar(),
	.oe(wire_obufa_oe[7:7]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[111:98]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[111:98])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_7.bus_hold = "false",
		obufa_7.open_drain_output = "false",
		obufa_7.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_8
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[8:8]),
	.i(wire_obufa_i[8:8]),
	.o(wire_obufa_o[8:8]),
	.obar(),
	.oe(wire_obufa_oe[8:8]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[125:112]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[125:112])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_8.bus_hold = "false",
		obufa_8.open_drain_output = "false",
		obufa_8.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_9
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[9:9]),
	.i(wire_obufa_i[9:9]),
	.o(wire_obufa_o[9:9]),
	.obar(),
	.oe(wire_obufa_oe[9:9]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[139:126]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[139:126])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_9.bus_hold = "false",
		obufa_9.open_drain_output = "false",
		obufa_9.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_10
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[10:10]),
	.i(wire_obufa_i[10:10]),
	.o(wire_obufa_o[10:10]),
	.obar(),
	.oe(wire_obufa_oe[10:10]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[153:140]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[153:140])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_10.bus_hold = "false",
		obufa_10.open_drain_output = "false",
		obufa_10.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_11
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[11:11]),
	.i(wire_obufa_i[11:11]),
	.o(wire_obufa_o[11:11]),
	.obar(),
	.oe(wire_obufa_oe[11:11]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[167:154]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[167:154])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_11.bus_hold = "false",
		obufa_11.open_drain_output = "false",
		obufa_11.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_12
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[12:12]),
	.i(wire_obufa_i[12:12]),
	.o(wire_obufa_o[12:12]),
	.obar(),
	.oe(wire_obufa_oe[12:12]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[181:168]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[181:168])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_12.bus_hold = "false",
		obufa_12.open_drain_output = "false",
		obufa_12.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_13
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[13:13]),
	.i(wire_obufa_i[13:13]),
	.o(wire_obufa_o[13:13]),
	.obar(),
	.oe(wire_obufa_oe[13:13]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[195:182]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[195:182])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_13.bus_hold = "false",
		obufa_13.open_drain_output = "false",
		obufa_13.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_14
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[14:14]),
	.i(wire_obufa_i[14:14]),
	.o(wire_obufa_o[14:14]),
	.obar(),
	.oe(wire_obufa_oe[14:14]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[209:196]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[209:196])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_14.bus_hold = "false",
		obufa_14.open_drain_output = "false",
		obufa_14.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_15
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[15:15]),
	.i(wire_obufa_i[15:15]),
	.o(wire_obufa_o[15:15]),
	.obar(),
	.oe(wire_obufa_oe[15:15]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[223:210]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[223:210])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_15.bus_hold = "false",
		obufa_15.open_drain_output = "false",
		obufa_15.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_16
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[16:16]),
	.i(wire_obufa_i[16:16]),
	.o(wire_obufa_o[16:16]),
	.obar(),
	.oe(wire_obufa_oe[16:16]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[237:224]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[237:224])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_16.bus_hold = "false",
		obufa_16.open_drain_output = "false",
		obufa_16.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_17
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[17:17]),
	.i(wire_obufa_i[17:17]),
	.o(wire_obufa_o[17:17]),
	.obar(),
	.oe(wire_obufa_oe[17:17]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[251:238]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[251:238])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_17.bus_hold = "false",
		obufa_17.open_drain_output = "false",
		obufa_17.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_18
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[18:18]),
	.i(wire_obufa_i[18:18]),
	.o(wire_obufa_o[18:18]),
	.obar(),
	.oe(wire_obufa_oe[18:18]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[265:252]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[265:252])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_18.bus_hold = "false",
		obufa_18.open_drain_output = "false",
		obufa_18.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_19
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[19:19]),
	.i(wire_obufa_i[19:19]),
	.o(wire_obufa_o[19:19]),
	.obar(),
	.oe(wire_obufa_oe[19:19]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[279:266]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[279:266])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_19.bus_hold = "false",
		obufa_19.open_drain_output = "false",
		obufa_19.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_20
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[20:20]),
	.i(wire_obufa_i[20:20]),
	.o(wire_obufa_o[20:20]),
	.obar(),
	.oe(wire_obufa_oe[20:20]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[293:280]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[293:280])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_20.bus_hold = "false",
		obufa_20.open_drain_output = "false",
		obufa_20.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_21
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[21:21]),
	.i(wire_obufa_i[21:21]),
	.o(wire_obufa_o[21:21]),
	.obar(),
	.oe(wire_obufa_oe[21:21]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[307:294]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[307:294])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_21.bus_hold = "false",
		obufa_21.open_drain_output = "false",
		obufa_21.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_22
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[22:22]),
	.i(wire_obufa_i[22:22]),
	.o(wire_obufa_o[22:22]),
	.obar(),
	.oe(wire_obufa_oe[22:22]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[321:308]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[321:308])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_22.bus_hold = "false",
		obufa_22.open_drain_output = "false",
		obufa_22.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_23
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[23:23]),
	.i(wire_obufa_i[23:23]),
	.o(wire_obufa_o[23:23]),
	.obar(),
	.oe(wire_obufa_oe[23:23]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[335:322]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[335:322])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_23.bus_hold = "false",
		obufa_23.open_drain_output = "false",
		obufa_23.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_24
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[24:24]),
	.i(wire_obufa_i[24:24]),
	.o(wire_obufa_o[24:24]),
	.obar(),
	.oe(wire_obufa_oe[24:24]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[349:336]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[349:336])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_24.bus_hold = "false",
		obufa_24.open_drain_output = "false",
		obufa_24.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_25
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[25:25]),
	.i(wire_obufa_i[25:25]),
	.o(wire_obufa_o[25:25]),
	.obar(),
	.oe(wire_obufa_oe[25:25]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[363:350]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[363:350])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_25.bus_hold = "false",
		obufa_25.open_drain_output = "false",
		obufa_25.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_26
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[26:26]),
	.i(wire_obufa_i[26:26]),
	.o(wire_obufa_o[26:26]),
	.obar(),
	.oe(wire_obufa_oe[26:26]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[377:364]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[377:364])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_26.bus_hold = "false",
		obufa_26.open_drain_output = "false",
		obufa_26.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_27
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[27:27]),
	.i(wire_obufa_i[27:27]),
	.o(wire_obufa_o[27:27]),
	.obar(),
	.oe(wire_obufa_oe[27:27]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[391:378]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[391:378])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_27.bus_hold = "false",
		obufa_27.open_drain_output = "false",
		obufa_27.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_28
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[28:28]),
	.i(wire_obufa_i[28:28]),
	.o(wire_obufa_o[28:28]),
	.obar(),
	.oe(wire_obufa_oe[28:28]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[405:392]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[405:392])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_28.bus_hold = "false",
		obufa_28.open_drain_output = "false",
		obufa_28.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_29
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[29:29]),
	.i(wire_obufa_i[29:29]),
	.o(wire_obufa_o[29:29]),
	.obar(),
	.oe(wire_obufa_oe[29:29]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[419:406]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[419:406])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_29.bus_hold = "false",
		obufa_29.open_drain_output = "false",
		obufa_29.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_30
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[30:30]),
	.i(wire_obufa_i[30:30]),
	.o(wire_obufa_o[30:30]),
	.obar(),
	.oe(wire_obufa_oe[30:30]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[433:420]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[433:420])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_30.bus_hold = "false",
		obufa_30.open_drain_output = "false",
		obufa_30.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_31
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[31:31]),
	.i(wire_obufa_i[31:31]),
	.o(wire_obufa_o[31:31]),
	.obar(),
	.oe(wire_obufa_oe[31:31]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[447:434]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[447:434])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_31.bus_hold = "false",
		obufa_31.open_drain_output = "false",
		obufa_31.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_32
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[32:32]),
	.i(wire_obufa_i[32:32]),
	.o(wire_obufa_o[32:32]),
	.obar(),
	.oe(wire_obufa_oe[32:32]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[461:448]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[461:448])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_32.bus_hold = "false",
		obufa_32.open_drain_output = "false",
		obufa_32.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_33
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[33:33]),
	.i(wire_obufa_i[33:33]),
	.o(wire_obufa_o[33:33]),
	.obar(),
	.oe(wire_obufa_oe[33:33]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[475:462]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[475:462])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_33.bus_hold = "false",
		obufa_33.open_drain_output = "false",
		obufa_33.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_34
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[34:34]),
	.i(wire_obufa_i[34:34]),
	.o(wire_obufa_o[34:34]),
	.obar(),
	.oe(wire_obufa_oe[34:34]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[489:476]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[489:476])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_34.bus_hold = "false",
		obufa_34.open_drain_output = "false",
		obufa_34.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_35
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[35:35]),
	.i(wire_obufa_i[35:35]),
	.o(wire_obufa_o[35:35]),
	.obar(),
	.oe(wire_obufa_oe[35:35]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[503:490]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[503:490])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_35.bus_hold = "false",
		obufa_35.open_drain_output = "false",
		obufa_35.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_36
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[36:36]),
	.i(wire_obufa_i[36:36]),
	.o(wire_obufa_o[36:36]),
	.obar(),
	.oe(wire_obufa_oe[36:36]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[517:504]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[517:504])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_36.bus_hold = "false",
		obufa_36.open_drain_output = "false",
		obufa_36.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_37
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[37:37]),
	.i(wire_obufa_i[37:37]),
	.o(wire_obufa_o[37:37]),
	.obar(),
	.oe(wire_obufa_oe[37:37]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[531:518]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[531:518])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_37.bus_hold = "false",
		obufa_37.open_drain_output = "false",
		obufa_37.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_38
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[38:38]),
	.i(wire_obufa_i[38:38]),
	.o(wire_obufa_o[38:38]),
	.obar(),
	.oe(wire_obufa_oe[38:38]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[545:532]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[545:532])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_38.bus_hold = "false",
		obufa_38.open_drain_output = "false",
		obufa_38.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_39
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[39:39]),
	.i(wire_obufa_i[39:39]),
	.o(wire_obufa_o[39:39]),
	.obar(),
	.oe(wire_obufa_oe[39:39]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[559:546]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[559:546])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_39.bus_hold = "false",
		obufa_39.open_drain_output = "false",
		obufa_39.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_40
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[40:40]),
	.i(wire_obufa_i[40:40]),
	.o(wire_obufa_o[40:40]),
	.obar(),
	.oe(wire_obufa_oe[40:40]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[573:560]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[573:560])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_40.bus_hold = "false",
		obufa_40.open_drain_output = "false",
		obufa_40.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_41
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[41:41]),
	.i(wire_obufa_i[41:41]),
	.o(wire_obufa_o[41:41]),
	.obar(),
	.oe(wire_obufa_oe[41:41]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[587:574]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[587:574])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_41.bus_hold = "false",
		obufa_41.open_drain_output = "false",
		obufa_41.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_42
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[42:42]),
	.i(wire_obufa_i[42:42]),
	.o(wire_obufa_o[42:42]),
	.obar(),
	.oe(wire_obufa_oe[42:42]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[601:588]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[601:588])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_42.bus_hold = "false",
		obufa_42.open_drain_output = "false",
		obufa_42.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_43
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[43:43]),
	.i(wire_obufa_i[43:43]),
	.o(wire_obufa_o[43:43]),
	.obar(),
	.oe(wire_obufa_oe[43:43]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[615:602]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[615:602])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_43.bus_hold = "false",
		obufa_43.open_drain_output = "false",
		obufa_43.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_44
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[44:44]),
	.i(wire_obufa_i[44:44]),
	.o(wire_obufa_o[44:44]),
	.obar(),
	.oe(wire_obufa_oe[44:44]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[629:616]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[629:616])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_44.bus_hold = "false",
		obufa_44.open_drain_output = "false",
		obufa_44.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_45
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[45:45]),
	.i(wire_obufa_i[45:45]),
	.o(wire_obufa_o[45:45]),
	.obar(),
	.oe(wire_obufa_oe[45:45]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[643:630]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[643:630])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_45.bus_hold = "false",
		obufa_45.open_drain_output = "false",
		obufa_45.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_46
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[46:46]),
	.i(wire_obufa_i[46:46]),
	.o(wire_obufa_o[46:46]),
	.obar(),
	.oe(wire_obufa_oe[46:46]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[657:644]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[657:644])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_46.bus_hold = "false",
		obufa_46.open_drain_output = "false",
		obufa_46.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_47
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[47:47]),
	.i(wire_obufa_i[47:47]),
	.o(wire_obufa_o[47:47]),
	.obar(),
	.oe(wire_obufa_oe[47:47]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[671:658]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[671:658])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_47.bus_hold = "false",
		obufa_47.open_drain_output = "false",
		obufa_47.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_48
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[48:48]),
	.i(wire_obufa_i[48:48]),
	.o(wire_obufa_o[48:48]),
	.obar(),
	.oe(wire_obufa_oe[48:48]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[685:672]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[685:672])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_48.bus_hold = "false",
		obufa_48.open_drain_output = "false",
		obufa_48.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_49
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[49:49]),
	.i(wire_obufa_i[49:49]),
	.o(wire_obufa_o[49:49]),
	.obar(),
	.oe(wire_obufa_oe[49:49]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[699:686]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[699:686])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_49.bus_hold = "false",
		obufa_49.open_drain_output = "false",
		obufa_49.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_50
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[50:50]),
	.i(wire_obufa_i[50:50]),
	.o(wire_obufa_o[50:50]),
	.obar(),
	.oe(wire_obufa_oe[50:50]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[713:700]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[713:700])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_50.bus_hold = "false",
		obufa_50.open_drain_output = "false",
		obufa_50.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_51
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[51:51]),
	.i(wire_obufa_i[51:51]),
	.o(wire_obufa_o[51:51]),
	.obar(),
	.oe(wire_obufa_oe[51:51]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[727:714]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[727:714])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_51.bus_hold = "false",
		obufa_51.open_drain_output = "false",
		obufa_51.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_52
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[52:52]),
	.i(wire_obufa_i[52:52]),
	.o(wire_obufa_o[52:52]),
	.obar(),
	.oe(wire_obufa_oe[52:52]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[741:728]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[741:728])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_52.bus_hold = "false",
		obufa_52.open_drain_output = "false",
		obufa_52.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_53
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[53:53]),
	.i(wire_obufa_i[53:53]),
	.o(wire_obufa_o[53:53]),
	.obar(),
	.oe(wire_obufa_oe[53:53]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[755:742]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[755:742])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_53.bus_hold = "false",
		obufa_53.open_drain_output = "false",
		obufa_53.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_54
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[54:54]),
	.i(wire_obufa_i[54:54]),
	.o(wire_obufa_o[54:54]),
	.obar(),
	.oe(wire_obufa_oe[54:54]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[769:756]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[769:756])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_54.bus_hold = "false",
		obufa_54.open_drain_output = "false",
		obufa_54.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_55
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[55:55]),
	.i(wire_obufa_i[55:55]),
	.o(wire_obufa_o[55:55]),
	.obar(),
	.oe(wire_obufa_oe[55:55]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[783:770]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[783:770])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_55.bus_hold = "false",
		obufa_55.open_drain_output = "false",
		obufa_55.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_56
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[56:56]),
	.i(wire_obufa_i[56:56]),
	.o(wire_obufa_o[56:56]),
	.obar(),
	.oe(wire_obufa_oe[56:56]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[797:784]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[797:784])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_56.bus_hold = "false",
		obufa_56.open_drain_output = "false",
		obufa_56.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_57
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[57:57]),
	.i(wire_obufa_i[57:57]),
	.o(wire_obufa_o[57:57]),
	.obar(),
	.oe(wire_obufa_oe[57:57]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[811:798]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[811:798])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_57.bus_hold = "false",
		obufa_57.open_drain_output = "false",
		obufa_57.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_58
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[58:58]),
	.i(wire_obufa_i[58:58]),
	.o(wire_obufa_o[58:58]),
	.obar(),
	.oe(wire_obufa_oe[58:58]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[825:812]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[825:812])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_58.bus_hold = "false",
		obufa_58.open_drain_output = "false",
		obufa_58.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_59
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[59:59]),
	.i(wire_obufa_i[59:59]),
	.o(wire_obufa_o[59:59]),
	.obar(),
	.oe(wire_obufa_oe[59:59]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[839:826]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[839:826])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_59.bus_hold = "false",
		obufa_59.open_drain_output = "false",
		obufa_59.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_60
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[60:60]),
	.i(wire_obufa_i[60:60]),
	.o(wire_obufa_o[60:60]),
	.obar(),
	.oe(wire_obufa_oe[60:60]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[853:840]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[853:840])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_60.bus_hold = "false",
		obufa_60.open_drain_output = "false",
		obufa_60.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_61
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[61:61]),
	.i(wire_obufa_i[61:61]),
	.o(wire_obufa_o[61:61]),
	.obar(),
	.oe(wire_obufa_oe[61:61]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[867:854]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[867:854])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_61.bus_hold = "false",
		obufa_61.open_drain_output = "false",
		obufa_61.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_62
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[62:62]),
	.i(wire_obufa_i[62:62]),
	.o(wire_obufa_o[62:62]),
	.obar(),
	.oe(wire_obufa_oe[62:62]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[881:868]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[881:868])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_62.bus_hold = "false",
		obufa_62.open_drain_output = "false",
		obufa_62.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_63
	( 
	.dynamicterminationcontrol(wire_obufa_dynamicterminationcontrol[63:63]),
	.i(wire_obufa_i[63:63]),
	.o(wire_obufa_o[63:63]),
	.obar(),
	.oe(wire_obufa_oe[63:63]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[895:882]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[895:882])
	// synopsys translate_off
	,
	.devoe(1'b1)
	// synopsys translate_on
	);
	defparam
		obufa_63.bus_hold = "false",
		obufa_63.open_drain_output = "false",
		obufa_63.lpm_type = "stratixiii_io_obuf";
	assign
		wire_obufa_dynamicterminationcontrol = dynamicterminationcontrol,
		wire_obufa_i = datain,
		wire_obufa_oe = oe,
		wire_obufa_parallelterminationcontrol = parallelterminationcontrol,
		wire_obufa_seriesterminationcontrol = seriesterminationcontrol;
	assign
		dataio = wire_obufa_o,
		dataout = wire_ibufa_o;
endmodule //ddr2_v10_1_write_dq_iobuf
//VALID FILE
