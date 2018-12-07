//altiobuf_out CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIII" ENABLE_BUS_HOLD="FALSE" LEFT_SHIFT_SERIES_TERMINATION_CONTROL="FALSE" NUMBER_OF_CHANNELS=64 OPEN_DRAIN_OUTPUT="FALSE" PSEUDO_DIFFERENTIAL_MODE="FALSE" USE_DIFFERENTIAL_MODE="FALSE" USE_OE="FALSE" USE_TERMINATION_CONTROL="TRUE" WIDTH_PTC=14 WIDTH_STC=14 datain dataout parallelterminationcontrol seriesterminationcontrol
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



//synthesis_resources = stratixiii_io_obuf 64 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_0002_write_d_iobuf
	( 
	datain,
	dataout,
	parallelterminationcontrol,
	seriesterminationcontrol) /* synthesis synthesis_clearbox=1 */;
	input   [63:0]  datain;
	output   [63:0]  dataout;
	input   [895:0]  parallelterminationcontrol;
	input   [895:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   [895:0]  parallelterminationcontrol;
	tri0   [895:0]  seriesterminationcontrol;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire  [63:0]   wire_obufa_i;
	wire  [63:0]   wire_obufa_o;
	wire  [63:0]   wire_obufa_oe;
	wire  [895:0]   wire_obufa_parallelterminationcontrol;
	wire  [895:0]   wire_obufa_seriesterminationcontrol;
	wire  [63:0]  oe_w;

	stratixiii_io_obuf   obufa_0
	( 
	.i(wire_obufa_i[0:0]),
	.o(wire_obufa_o[0:0]),
	.obar(),
	.oe(wire_obufa_oe[0:0]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[13:0]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[13:0])
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
	stratixiii_io_obuf   obufa_1
	( 
	.i(wire_obufa_i[1:1]),
	.o(wire_obufa_o[1:1]),
	.obar(),
	.oe(wire_obufa_oe[1:1]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[27:14]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[27:14])
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
		obufa_1.bus_hold = "false",
		obufa_1.open_drain_output = "false",
		obufa_1.shift_series_termination_control = "false",
		obufa_1.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_2
	( 
	.i(wire_obufa_i[2:2]),
	.o(wire_obufa_o[2:2]),
	.obar(),
	.oe(wire_obufa_oe[2:2]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[41:28]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[41:28])
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
		obufa_2.bus_hold = "false",
		obufa_2.open_drain_output = "false",
		obufa_2.shift_series_termination_control = "false",
		obufa_2.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_3
	( 
	.i(wire_obufa_i[3:3]),
	.o(wire_obufa_o[3:3]),
	.obar(),
	.oe(wire_obufa_oe[3:3]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[55:42]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[55:42])
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
		obufa_3.bus_hold = "false",
		obufa_3.open_drain_output = "false",
		obufa_3.shift_series_termination_control = "false",
		obufa_3.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_4
	( 
	.i(wire_obufa_i[4:4]),
	.o(wire_obufa_o[4:4]),
	.obar(),
	.oe(wire_obufa_oe[4:4]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[69:56]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[69:56])
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
		obufa_4.bus_hold = "false",
		obufa_4.open_drain_output = "false",
		obufa_4.shift_series_termination_control = "false",
		obufa_4.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_5
	( 
	.i(wire_obufa_i[5:5]),
	.o(wire_obufa_o[5:5]),
	.obar(),
	.oe(wire_obufa_oe[5:5]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[83:70]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[83:70])
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
		obufa_5.bus_hold = "false",
		obufa_5.open_drain_output = "false",
		obufa_5.shift_series_termination_control = "false",
		obufa_5.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_6
	( 
	.i(wire_obufa_i[6:6]),
	.o(wire_obufa_o[6:6]),
	.obar(),
	.oe(wire_obufa_oe[6:6]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[97:84]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[97:84])
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
		obufa_6.bus_hold = "false",
		obufa_6.open_drain_output = "false",
		obufa_6.shift_series_termination_control = "false",
		obufa_6.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_7
	( 
	.i(wire_obufa_i[7:7]),
	.o(wire_obufa_o[7:7]),
	.obar(),
	.oe(wire_obufa_oe[7:7]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[111:98]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[111:98])
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
		obufa_7.bus_hold = "false",
		obufa_7.open_drain_output = "false",
		obufa_7.shift_series_termination_control = "false",
		obufa_7.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_8
	( 
	.i(wire_obufa_i[8:8]),
	.o(wire_obufa_o[8:8]),
	.obar(),
	.oe(wire_obufa_oe[8:8]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[125:112]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[125:112])
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
		obufa_8.bus_hold = "false",
		obufa_8.open_drain_output = "false",
		obufa_8.shift_series_termination_control = "false",
		obufa_8.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_9
	( 
	.i(wire_obufa_i[9:9]),
	.o(wire_obufa_o[9:9]),
	.obar(),
	.oe(wire_obufa_oe[9:9]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[139:126]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[139:126])
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
		obufa_9.bus_hold = "false",
		obufa_9.open_drain_output = "false",
		obufa_9.shift_series_termination_control = "false",
		obufa_9.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_10
	( 
	.i(wire_obufa_i[10:10]),
	.o(wire_obufa_o[10:10]),
	.obar(),
	.oe(wire_obufa_oe[10:10]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[153:140]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[153:140])
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
		obufa_10.bus_hold = "false",
		obufa_10.open_drain_output = "false",
		obufa_10.shift_series_termination_control = "false",
		obufa_10.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_11
	( 
	.i(wire_obufa_i[11:11]),
	.o(wire_obufa_o[11:11]),
	.obar(),
	.oe(wire_obufa_oe[11:11]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[167:154]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[167:154])
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
		obufa_11.bus_hold = "false",
		obufa_11.open_drain_output = "false",
		obufa_11.shift_series_termination_control = "false",
		obufa_11.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_12
	( 
	.i(wire_obufa_i[12:12]),
	.o(wire_obufa_o[12:12]),
	.obar(),
	.oe(wire_obufa_oe[12:12]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[181:168]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[181:168])
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
		obufa_12.bus_hold = "false",
		obufa_12.open_drain_output = "false",
		obufa_12.shift_series_termination_control = "false",
		obufa_12.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_13
	( 
	.i(wire_obufa_i[13:13]),
	.o(wire_obufa_o[13:13]),
	.obar(),
	.oe(wire_obufa_oe[13:13]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[195:182]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[195:182])
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
		obufa_13.bus_hold = "false",
		obufa_13.open_drain_output = "false",
		obufa_13.shift_series_termination_control = "false",
		obufa_13.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_14
	( 
	.i(wire_obufa_i[14:14]),
	.o(wire_obufa_o[14:14]),
	.obar(),
	.oe(wire_obufa_oe[14:14]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[209:196]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[209:196])
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
		obufa_14.bus_hold = "false",
		obufa_14.open_drain_output = "false",
		obufa_14.shift_series_termination_control = "false",
		obufa_14.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_15
	( 
	.i(wire_obufa_i[15:15]),
	.o(wire_obufa_o[15:15]),
	.obar(),
	.oe(wire_obufa_oe[15:15]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[223:210]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[223:210])
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
		obufa_15.bus_hold = "false",
		obufa_15.open_drain_output = "false",
		obufa_15.shift_series_termination_control = "false",
		obufa_15.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_16
	( 
	.i(wire_obufa_i[16:16]),
	.o(wire_obufa_o[16:16]),
	.obar(),
	.oe(wire_obufa_oe[16:16]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[237:224]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[237:224])
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
		obufa_16.bus_hold = "false",
		obufa_16.open_drain_output = "false",
		obufa_16.shift_series_termination_control = "false",
		obufa_16.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_17
	( 
	.i(wire_obufa_i[17:17]),
	.o(wire_obufa_o[17:17]),
	.obar(),
	.oe(wire_obufa_oe[17:17]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[251:238]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[251:238])
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
		obufa_17.bus_hold = "false",
		obufa_17.open_drain_output = "false",
		obufa_17.shift_series_termination_control = "false",
		obufa_17.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_18
	( 
	.i(wire_obufa_i[18:18]),
	.o(wire_obufa_o[18:18]),
	.obar(),
	.oe(wire_obufa_oe[18:18]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[265:252]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[265:252])
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
		obufa_18.bus_hold = "false",
		obufa_18.open_drain_output = "false",
		obufa_18.shift_series_termination_control = "false",
		obufa_18.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_19
	( 
	.i(wire_obufa_i[19:19]),
	.o(wire_obufa_o[19:19]),
	.obar(),
	.oe(wire_obufa_oe[19:19]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[279:266]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[279:266])
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
		obufa_19.bus_hold = "false",
		obufa_19.open_drain_output = "false",
		obufa_19.shift_series_termination_control = "false",
		obufa_19.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_20
	( 
	.i(wire_obufa_i[20:20]),
	.o(wire_obufa_o[20:20]),
	.obar(),
	.oe(wire_obufa_oe[20:20]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[293:280]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[293:280])
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
		obufa_20.bus_hold = "false",
		obufa_20.open_drain_output = "false",
		obufa_20.shift_series_termination_control = "false",
		obufa_20.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_21
	( 
	.i(wire_obufa_i[21:21]),
	.o(wire_obufa_o[21:21]),
	.obar(),
	.oe(wire_obufa_oe[21:21]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[307:294]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[307:294])
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
		obufa_21.bus_hold = "false",
		obufa_21.open_drain_output = "false",
		obufa_21.shift_series_termination_control = "false",
		obufa_21.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_22
	( 
	.i(wire_obufa_i[22:22]),
	.o(wire_obufa_o[22:22]),
	.obar(),
	.oe(wire_obufa_oe[22:22]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[321:308]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[321:308])
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
		obufa_22.bus_hold = "false",
		obufa_22.open_drain_output = "false",
		obufa_22.shift_series_termination_control = "false",
		obufa_22.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_23
	( 
	.i(wire_obufa_i[23:23]),
	.o(wire_obufa_o[23:23]),
	.obar(),
	.oe(wire_obufa_oe[23:23]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[335:322]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[335:322])
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
		obufa_23.bus_hold = "false",
		obufa_23.open_drain_output = "false",
		obufa_23.shift_series_termination_control = "false",
		obufa_23.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_24
	( 
	.i(wire_obufa_i[24:24]),
	.o(wire_obufa_o[24:24]),
	.obar(),
	.oe(wire_obufa_oe[24:24]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[349:336]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[349:336])
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
		obufa_24.bus_hold = "false",
		obufa_24.open_drain_output = "false",
		obufa_24.shift_series_termination_control = "false",
		obufa_24.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_25
	( 
	.i(wire_obufa_i[25:25]),
	.o(wire_obufa_o[25:25]),
	.obar(),
	.oe(wire_obufa_oe[25:25]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[363:350]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[363:350])
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
		obufa_25.bus_hold = "false",
		obufa_25.open_drain_output = "false",
		obufa_25.shift_series_termination_control = "false",
		obufa_25.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_26
	( 
	.i(wire_obufa_i[26:26]),
	.o(wire_obufa_o[26:26]),
	.obar(),
	.oe(wire_obufa_oe[26:26]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[377:364]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[377:364])
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
		obufa_26.bus_hold = "false",
		obufa_26.open_drain_output = "false",
		obufa_26.shift_series_termination_control = "false",
		obufa_26.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_27
	( 
	.i(wire_obufa_i[27:27]),
	.o(wire_obufa_o[27:27]),
	.obar(),
	.oe(wire_obufa_oe[27:27]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[391:378]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[391:378])
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
		obufa_27.bus_hold = "false",
		obufa_27.open_drain_output = "false",
		obufa_27.shift_series_termination_control = "false",
		obufa_27.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_28
	( 
	.i(wire_obufa_i[28:28]),
	.o(wire_obufa_o[28:28]),
	.obar(),
	.oe(wire_obufa_oe[28:28]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[405:392]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[405:392])
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
		obufa_28.bus_hold = "false",
		obufa_28.open_drain_output = "false",
		obufa_28.shift_series_termination_control = "false",
		obufa_28.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_29
	( 
	.i(wire_obufa_i[29:29]),
	.o(wire_obufa_o[29:29]),
	.obar(),
	.oe(wire_obufa_oe[29:29]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[419:406]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[419:406])
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
		obufa_29.bus_hold = "false",
		obufa_29.open_drain_output = "false",
		obufa_29.shift_series_termination_control = "false",
		obufa_29.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_30
	( 
	.i(wire_obufa_i[30:30]),
	.o(wire_obufa_o[30:30]),
	.obar(),
	.oe(wire_obufa_oe[30:30]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[433:420]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[433:420])
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
		obufa_30.bus_hold = "false",
		obufa_30.open_drain_output = "false",
		obufa_30.shift_series_termination_control = "false",
		obufa_30.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_31
	( 
	.i(wire_obufa_i[31:31]),
	.o(wire_obufa_o[31:31]),
	.obar(),
	.oe(wire_obufa_oe[31:31]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[447:434]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[447:434])
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
		obufa_31.bus_hold = "false",
		obufa_31.open_drain_output = "false",
		obufa_31.shift_series_termination_control = "false",
		obufa_31.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_32
	( 
	.i(wire_obufa_i[32:32]),
	.o(wire_obufa_o[32:32]),
	.obar(),
	.oe(wire_obufa_oe[32:32]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[461:448]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[461:448])
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
		obufa_32.bus_hold = "false",
		obufa_32.open_drain_output = "false",
		obufa_32.shift_series_termination_control = "false",
		obufa_32.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_33
	( 
	.i(wire_obufa_i[33:33]),
	.o(wire_obufa_o[33:33]),
	.obar(),
	.oe(wire_obufa_oe[33:33]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[475:462]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[475:462])
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
		obufa_33.bus_hold = "false",
		obufa_33.open_drain_output = "false",
		obufa_33.shift_series_termination_control = "false",
		obufa_33.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_34
	( 
	.i(wire_obufa_i[34:34]),
	.o(wire_obufa_o[34:34]),
	.obar(),
	.oe(wire_obufa_oe[34:34]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[489:476]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[489:476])
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
		obufa_34.bus_hold = "false",
		obufa_34.open_drain_output = "false",
		obufa_34.shift_series_termination_control = "false",
		obufa_34.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_35
	( 
	.i(wire_obufa_i[35:35]),
	.o(wire_obufa_o[35:35]),
	.obar(),
	.oe(wire_obufa_oe[35:35]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[503:490]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[503:490])
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
		obufa_35.bus_hold = "false",
		obufa_35.open_drain_output = "false",
		obufa_35.shift_series_termination_control = "false",
		obufa_35.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_36
	( 
	.i(wire_obufa_i[36:36]),
	.o(wire_obufa_o[36:36]),
	.obar(),
	.oe(wire_obufa_oe[36:36]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[517:504]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[517:504])
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
		obufa_36.bus_hold = "false",
		obufa_36.open_drain_output = "false",
		obufa_36.shift_series_termination_control = "false",
		obufa_36.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_37
	( 
	.i(wire_obufa_i[37:37]),
	.o(wire_obufa_o[37:37]),
	.obar(),
	.oe(wire_obufa_oe[37:37]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[531:518]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[531:518])
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
		obufa_37.bus_hold = "false",
		obufa_37.open_drain_output = "false",
		obufa_37.shift_series_termination_control = "false",
		obufa_37.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_38
	( 
	.i(wire_obufa_i[38:38]),
	.o(wire_obufa_o[38:38]),
	.obar(),
	.oe(wire_obufa_oe[38:38]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[545:532]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[545:532])
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
		obufa_38.bus_hold = "false",
		obufa_38.open_drain_output = "false",
		obufa_38.shift_series_termination_control = "false",
		obufa_38.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_39
	( 
	.i(wire_obufa_i[39:39]),
	.o(wire_obufa_o[39:39]),
	.obar(),
	.oe(wire_obufa_oe[39:39]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[559:546]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[559:546])
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
		obufa_39.bus_hold = "false",
		obufa_39.open_drain_output = "false",
		obufa_39.shift_series_termination_control = "false",
		obufa_39.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_40
	( 
	.i(wire_obufa_i[40:40]),
	.o(wire_obufa_o[40:40]),
	.obar(),
	.oe(wire_obufa_oe[40:40]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[573:560]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[573:560])
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
		obufa_40.bus_hold = "false",
		obufa_40.open_drain_output = "false",
		obufa_40.shift_series_termination_control = "false",
		obufa_40.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_41
	( 
	.i(wire_obufa_i[41:41]),
	.o(wire_obufa_o[41:41]),
	.obar(),
	.oe(wire_obufa_oe[41:41]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[587:574]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[587:574])
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
		obufa_41.bus_hold = "false",
		obufa_41.open_drain_output = "false",
		obufa_41.shift_series_termination_control = "false",
		obufa_41.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_42
	( 
	.i(wire_obufa_i[42:42]),
	.o(wire_obufa_o[42:42]),
	.obar(),
	.oe(wire_obufa_oe[42:42]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[601:588]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[601:588])
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
		obufa_42.bus_hold = "false",
		obufa_42.open_drain_output = "false",
		obufa_42.shift_series_termination_control = "false",
		obufa_42.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_43
	( 
	.i(wire_obufa_i[43:43]),
	.o(wire_obufa_o[43:43]),
	.obar(),
	.oe(wire_obufa_oe[43:43]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[615:602]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[615:602])
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
		obufa_43.bus_hold = "false",
		obufa_43.open_drain_output = "false",
		obufa_43.shift_series_termination_control = "false",
		obufa_43.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_44
	( 
	.i(wire_obufa_i[44:44]),
	.o(wire_obufa_o[44:44]),
	.obar(),
	.oe(wire_obufa_oe[44:44]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[629:616]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[629:616])
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
		obufa_44.bus_hold = "false",
		obufa_44.open_drain_output = "false",
		obufa_44.shift_series_termination_control = "false",
		obufa_44.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_45
	( 
	.i(wire_obufa_i[45:45]),
	.o(wire_obufa_o[45:45]),
	.obar(),
	.oe(wire_obufa_oe[45:45]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[643:630]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[643:630])
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
		obufa_45.bus_hold = "false",
		obufa_45.open_drain_output = "false",
		obufa_45.shift_series_termination_control = "false",
		obufa_45.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_46
	( 
	.i(wire_obufa_i[46:46]),
	.o(wire_obufa_o[46:46]),
	.obar(),
	.oe(wire_obufa_oe[46:46]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[657:644]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[657:644])
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
		obufa_46.bus_hold = "false",
		obufa_46.open_drain_output = "false",
		obufa_46.shift_series_termination_control = "false",
		obufa_46.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_47
	( 
	.i(wire_obufa_i[47:47]),
	.o(wire_obufa_o[47:47]),
	.obar(),
	.oe(wire_obufa_oe[47:47]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[671:658]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[671:658])
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
		obufa_47.bus_hold = "false",
		obufa_47.open_drain_output = "false",
		obufa_47.shift_series_termination_control = "false",
		obufa_47.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_48
	( 
	.i(wire_obufa_i[48:48]),
	.o(wire_obufa_o[48:48]),
	.obar(),
	.oe(wire_obufa_oe[48:48]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[685:672]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[685:672])
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
		obufa_48.bus_hold = "false",
		obufa_48.open_drain_output = "false",
		obufa_48.shift_series_termination_control = "false",
		obufa_48.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_49
	( 
	.i(wire_obufa_i[49:49]),
	.o(wire_obufa_o[49:49]),
	.obar(),
	.oe(wire_obufa_oe[49:49]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[699:686]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[699:686])
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
		obufa_49.bus_hold = "false",
		obufa_49.open_drain_output = "false",
		obufa_49.shift_series_termination_control = "false",
		obufa_49.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_50
	( 
	.i(wire_obufa_i[50:50]),
	.o(wire_obufa_o[50:50]),
	.obar(),
	.oe(wire_obufa_oe[50:50]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[713:700]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[713:700])
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
		obufa_50.bus_hold = "false",
		obufa_50.open_drain_output = "false",
		obufa_50.shift_series_termination_control = "false",
		obufa_50.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_51
	( 
	.i(wire_obufa_i[51:51]),
	.o(wire_obufa_o[51:51]),
	.obar(),
	.oe(wire_obufa_oe[51:51]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[727:714]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[727:714])
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
		obufa_51.bus_hold = "false",
		obufa_51.open_drain_output = "false",
		obufa_51.shift_series_termination_control = "false",
		obufa_51.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_52
	( 
	.i(wire_obufa_i[52:52]),
	.o(wire_obufa_o[52:52]),
	.obar(),
	.oe(wire_obufa_oe[52:52]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[741:728]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[741:728])
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
		obufa_52.bus_hold = "false",
		obufa_52.open_drain_output = "false",
		obufa_52.shift_series_termination_control = "false",
		obufa_52.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_53
	( 
	.i(wire_obufa_i[53:53]),
	.o(wire_obufa_o[53:53]),
	.obar(),
	.oe(wire_obufa_oe[53:53]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[755:742]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[755:742])
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
		obufa_53.bus_hold = "false",
		obufa_53.open_drain_output = "false",
		obufa_53.shift_series_termination_control = "false",
		obufa_53.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_54
	( 
	.i(wire_obufa_i[54:54]),
	.o(wire_obufa_o[54:54]),
	.obar(),
	.oe(wire_obufa_oe[54:54]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[769:756]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[769:756])
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
		obufa_54.bus_hold = "false",
		obufa_54.open_drain_output = "false",
		obufa_54.shift_series_termination_control = "false",
		obufa_54.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_55
	( 
	.i(wire_obufa_i[55:55]),
	.o(wire_obufa_o[55:55]),
	.obar(),
	.oe(wire_obufa_oe[55:55]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[783:770]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[783:770])
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
		obufa_55.bus_hold = "false",
		obufa_55.open_drain_output = "false",
		obufa_55.shift_series_termination_control = "false",
		obufa_55.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_56
	( 
	.i(wire_obufa_i[56:56]),
	.o(wire_obufa_o[56:56]),
	.obar(),
	.oe(wire_obufa_oe[56:56]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[797:784]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[797:784])
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
		obufa_56.bus_hold = "false",
		obufa_56.open_drain_output = "false",
		obufa_56.shift_series_termination_control = "false",
		obufa_56.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_57
	( 
	.i(wire_obufa_i[57:57]),
	.o(wire_obufa_o[57:57]),
	.obar(),
	.oe(wire_obufa_oe[57:57]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[811:798]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[811:798])
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
		obufa_57.bus_hold = "false",
		obufa_57.open_drain_output = "false",
		obufa_57.shift_series_termination_control = "false",
		obufa_57.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_58
	( 
	.i(wire_obufa_i[58:58]),
	.o(wire_obufa_o[58:58]),
	.obar(),
	.oe(wire_obufa_oe[58:58]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[825:812]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[825:812])
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
		obufa_58.bus_hold = "false",
		obufa_58.open_drain_output = "false",
		obufa_58.shift_series_termination_control = "false",
		obufa_58.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_59
	( 
	.i(wire_obufa_i[59:59]),
	.o(wire_obufa_o[59:59]),
	.obar(),
	.oe(wire_obufa_oe[59:59]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[839:826]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[839:826])
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
		obufa_59.bus_hold = "false",
		obufa_59.open_drain_output = "false",
		obufa_59.shift_series_termination_control = "false",
		obufa_59.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_60
	( 
	.i(wire_obufa_i[60:60]),
	.o(wire_obufa_o[60:60]),
	.obar(),
	.oe(wire_obufa_oe[60:60]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[853:840]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[853:840])
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
		obufa_60.bus_hold = "false",
		obufa_60.open_drain_output = "false",
		obufa_60.shift_series_termination_control = "false",
		obufa_60.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_61
	( 
	.i(wire_obufa_i[61:61]),
	.o(wire_obufa_o[61:61]),
	.obar(),
	.oe(wire_obufa_oe[61:61]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[867:854]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[867:854])
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
		obufa_61.bus_hold = "false",
		obufa_61.open_drain_output = "false",
		obufa_61.shift_series_termination_control = "false",
		obufa_61.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_62
	( 
	.i(wire_obufa_i[62:62]),
	.o(wire_obufa_o[62:62]),
	.obar(),
	.oe(wire_obufa_oe[62:62]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[881:868]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[881:868])
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
		obufa_62.bus_hold = "false",
		obufa_62.open_drain_output = "false",
		obufa_62.shift_series_termination_control = "false",
		obufa_62.lpm_type = "stratixiii_io_obuf";
	stratixiii_io_obuf   obufa_63
	( 
	.i(wire_obufa_i[63:63]),
	.o(wire_obufa_o[63:63]),
	.obar(),
	.oe(wire_obufa_oe[63:63]),
	.parallelterminationcontrol(wire_obufa_parallelterminationcontrol[895:882]),
	.seriesterminationcontrol(wire_obufa_seriesterminationcontrol[895:882])
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
		obufa_63.bus_hold = "false",
		obufa_63.open_drain_output = "false",
		obufa_63.shift_series_termination_control = "false",
		obufa_63.lpm_type = "stratixiii_io_obuf";
	assign
		wire_obufa_i = datain,
		wire_obufa_oe = oe_w,
		wire_obufa_parallelterminationcontrol = parallelterminationcontrol,
		wire_obufa_seriesterminationcontrol = seriesterminationcontrol;
	assign
		dataout = wire_obufa_o,
		oe_w = {64{1'b1}};
endmodule //ddr2_v10_1_0002_write_d_iobuf
//VALID FILE
