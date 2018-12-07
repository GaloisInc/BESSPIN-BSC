//altiobuf_in CBX_AUTO_BLACKBOX="ALL" CBX_SINGLE_OUTPUT_FILE="ON" DEVICE_FAMILY="StratixIII" ENABLE_BUS_HOLD="FALSE" NUMBER_OF_CHANNELS=64 USE_DIFFERENTIAL_MODE="FALSE" datain dataout
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



//synthesis_resources = stratixiii_io_ibuf 64 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_mem_q_buf
	( 
	datain,
	dataout) /* synthesis synthesis_clearbox=1 */;
	input   [63:0]  datain;
	output   [63:0]  dataout;

	wire  [63:0]   wire_ibufa_i;
	wire  [63:0]   wire_ibufa_o;

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
		ibufa_0.differential_mode = "false",
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
		ibufa_1.differential_mode = "false",
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
		ibufa_2.differential_mode = "false",
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
		ibufa_3.differential_mode = "false",
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
		ibufa_4.differential_mode = "false",
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
		ibufa_5.differential_mode = "false",
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
		ibufa_6.differential_mode = "false",
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
		ibufa_7.differential_mode = "false",
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
		ibufa_8.differential_mode = "false",
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
		ibufa_9.differential_mode = "false",
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
		ibufa_10.differential_mode = "false",
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
		ibufa_11.differential_mode = "false",
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
		ibufa_12.differential_mode = "false",
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
		ibufa_13.differential_mode = "false",
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
		ibufa_14.differential_mode = "false",
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
		ibufa_15.differential_mode = "false",
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
		ibufa_16.differential_mode = "false",
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
		ibufa_17.differential_mode = "false",
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
		ibufa_18.differential_mode = "false",
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
		ibufa_19.differential_mode = "false",
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
		ibufa_20.differential_mode = "false",
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
		ibufa_21.differential_mode = "false",
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
		ibufa_22.differential_mode = "false",
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
		ibufa_23.differential_mode = "false",
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
		ibufa_24.differential_mode = "false",
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
		ibufa_25.differential_mode = "false",
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
		ibufa_26.differential_mode = "false",
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
		ibufa_27.differential_mode = "false",
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
		ibufa_28.differential_mode = "false",
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
		ibufa_29.differential_mode = "false",
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
		ibufa_30.differential_mode = "false",
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
		ibufa_31.differential_mode = "false",
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
		ibufa_32.differential_mode = "false",
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
		ibufa_33.differential_mode = "false",
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
		ibufa_34.differential_mode = "false",
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
		ibufa_35.differential_mode = "false",
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
		ibufa_36.differential_mode = "false",
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
		ibufa_37.differential_mode = "false",
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
		ibufa_38.differential_mode = "false",
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
		ibufa_39.differential_mode = "false",
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
		ibufa_40.differential_mode = "false",
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
		ibufa_41.differential_mode = "false",
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
		ibufa_42.differential_mode = "false",
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
		ibufa_43.differential_mode = "false",
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
		ibufa_44.differential_mode = "false",
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
		ibufa_45.differential_mode = "false",
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
		ibufa_46.differential_mode = "false",
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
		ibufa_47.differential_mode = "false",
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
		ibufa_48.differential_mode = "false",
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
		ibufa_49.differential_mode = "false",
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
		ibufa_50.differential_mode = "false",
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
		ibufa_51.differential_mode = "false",
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
		ibufa_52.differential_mode = "false",
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
		ibufa_53.differential_mode = "false",
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
		ibufa_54.differential_mode = "false",
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
		ibufa_55.differential_mode = "false",
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
		ibufa_56.differential_mode = "false",
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
		ibufa_57.differential_mode = "false",
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
		ibufa_58.differential_mode = "false",
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
		ibufa_59.differential_mode = "false",
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
		ibufa_60.differential_mode = "false",
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
		ibufa_61.differential_mode = "false",
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
		ibufa_62.differential_mode = "false",
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
		ibufa_63.differential_mode = "false",
		ibufa_63.lpm_type = "stratixiii_io_ibuf";
	assign
		wire_ibufa_i = datain;
	assign
		dataout = wire_ibufa_o;
endmodule //ddr2_v10_1_mem_q_buf
//VALID FILE
