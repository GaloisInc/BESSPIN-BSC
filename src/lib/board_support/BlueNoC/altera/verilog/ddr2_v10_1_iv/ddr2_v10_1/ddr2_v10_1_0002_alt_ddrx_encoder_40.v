// (C) 2001-2011 Altera Corporation. All rights reserved.
// Your use of Altera Corporation's design tools, logic functions and other 
// software and tools, and its AMPP partner logic functions, and any output 
// files any of the foregoing (including device programming or simulation 
// files), and any associated documentation or information are expressly subject 
// to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable 
// license agreement, including, without limitation, that your use is for the 
// sole purpose of programming logic devices manufactured by Altera and sold by 
// Altera or its authorized distributors.  Please refer to the applicable 
// agreement for further details.


// megafunction wizard: %ALTECC%
// GENERATION: STANDARD
// VERSION: WM1.0
// MODULE: altecc_encoder 

// ============================================================
// File Name: ddr2_v10_1_0002_alt_ddrx_encoder_40.v
// Megafunction Name(s):
// 			altecc_encoder
//
// Simulation Library Files(s):
// 			
// ============================================================
// ************************************************************
// THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
//
// 9.1 Internal Build 157 07/06/2009 PN Web Edition
// ************************************************************


//Copyright (C) 1991-2010 Altera Corporation
//Your use of Altera Corporation's design tools, logic functions 
//and other software and tools, and its AMPP partner logic 
//functions, and any output files from any of the foregoing 
//(including device programming or simulation files), and any 
//associated documentation or information are expressly subject 
//to the terms and conditions of the Altera Program License 
//Subscription Agreement, Altera MegaCore Function License 
//Agreement, or other applicable license agreement, including, 
//without limitation, that your use is for the sole purpose of 
//programming logic devices manufactured by Altera and sold by 
//Altera or its authorized distributors.  Please refer to the 
//applicable agreement for further details.


//altecc_encoder CBX_AUTO_BLACKBOX="ALL" device_family="Stratix" lpm_pipeline=1 width_codeword=39 width_dataword=32 clock data q
//VERSION_BEGIN 9.1 cbx_altecc_encoder 2009:06:09:13:39:47:PN cbx_mgl 2009:07:03:02:47:54:PN  VERSION_END
// synthesis VERILOG_INPUT_VERSION VERILOG_2001
// altera message_off 10463


//synthesis_resources = lut 39 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  ddr2_v10_1_0002_alt_ddrx_encoder_40_altecc_encoder_plb
	( 
	clock,
	data,
	q) ;
	input   clock;
	input   [31:0]  data;
	output   [38:0]  q;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   clock;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	reg	[38:0]	output_pipeline0c;
	wire aclr;
	wire clocken;
	wire  [31:0]  data_wire;
	wire  [17:0]  parity_01_wire;
	wire  [9:0]  parity_02_wire;
	wire  [4:0]  parity_03_wire;
	wire  [1:0]  parity_04_wire;
	wire  [0:0]  parity_05_wire;
	wire  [5:0]  parity_06_wire;
	wire  [37:0]  parity_final_wire;
	wire  [38:0]  q_wire;

	// synopsys translate_off
	initial
		output_pipeline0c = 0;
	// synopsys translate_on
	always @ ( posedge clock or  posedge aclr)
		if (aclr == 1'b1) output_pipeline0c <= 39'b0;
		else if  (clocken == 1'b1)   output_pipeline0c <= q_wire;
	assign
		aclr = 1'b0,
		clocken = 1'b1,
		data_wire = data,
		parity_01_wire = {(data_wire[30] ^ parity_01_wire[16]), (data_wire[28] ^ parity_01_wire[15]), (data_wire[26] ^ parity_01_wire[14]), (data_wire[25] ^ parity_01_wire[13]), (data_wire[23] ^ parity_01_wire[12]), (data_wire[21] ^ parity_01_wire[11]), (data_wire[19] ^ parity_01_wire[10]), (data_wire[17] ^ parity_01_wire[9]), (data_wire[15] ^ parity_01_wire[8]), (data_wire[13] ^ parity_01_wire[7]), (data_wire[11] ^ parity_01_wire[6]), (data_wire[10] ^ parity_01_wire[5]), (data_wire[8] ^ parity_01_wire[4]), (data_wire[6] ^ parity_01_wire[3]), (data_wire[4] ^ parity_01_wire[2]), (data_wire[3] ^ parity_01_wire[1]), (data_wire[1] ^ parity_01_wire[0]), data_wire[0]},
		parity_02_wire = {(data_wire[31] ^ parity_02_wire[8]), ((data_wire[27] ^ data_wire[28]) ^ parity_02_wire[7]), ((data_wire[24] ^ data_wire[25]) ^ parity_02_wire[6]), ((data_wire[20] ^ data_wire[21]) ^ parity_02_wire[5]), ((data_wire[16] ^ data_wire[17]) ^ parity_02_wire[4]), ((data_wire[12] ^ data_wire[13]) ^ parity_02_wire[3]), ((data_wire[9] ^ data_wire[10]) ^ parity_02_wire[2]), ((data_wire[5] ^ data_wire[6]) ^ parity_02_wire[1]), ((data_wire[2] ^ data_wire[3]) ^ parity_02_wire[0]), data_wire[0]},
		parity_03_wire = {(((data_wire[29] ^ data_wire[30]) ^ data_wire[31]) ^ parity_03_wire[3]), ((((data_wire[22] ^ data_wire[23]) ^ data_wire[24]) ^ data_wire[25]) ^ parity_03_wire[2]), ((((data_wire[14] ^ data_wire[15]) ^ data_wire[16]) ^ data_wire[17]) ^ parity_03_wire[1]), ((((data_wire[7] ^ data_wire[8]) ^ data_wire[9]) ^ data_wire[10]) ^ parity_03_wire[0]), ((data_wire[1] ^ data_wire[2]) ^ data_wire[3])},
		parity_04_wire = {((((((((data_wire[18] ^ data_wire[19]) ^ data_wire[20]) ^ data_wire[21]) ^ data_wire[22]) ^ data_wire[23]) ^ data_wire[24]) ^ data_wire[25]) ^ parity_04_wire[0]), ((((((data_wire[4] ^ data_wire[5]) ^ data_wire[6]) ^ data_wire[7]) ^ data_wire[8]) ^ data_wire[9]) ^ data_wire[10])},
		parity_05_wire = {((((((((((((((data_wire[11] ^ data_wire[12]) ^ data_wire[13]) ^ data_wire[14]) ^ data_wire[15]) ^ data_wire[16]) ^ data_wire[17]) ^ data_wire[18]) ^ data_wire[19]) ^ data_wire[20]) ^ data_wire[21]) ^ data_wire[22]) ^ data_wire[23]) ^ data_wire[24]) ^ data_wire[25])},
		parity_06_wire = {(data_wire[31] ^ parity_06_wire[4]), (data_wire[30] ^ parity_06_wire[3]), (data_wire[29] ^ parity_06_wire[2]), (data_wire[28] ^ parity_06_wire[1]), (data_wire[27] ^ parity_06_wire[0]), data_wire[26]},
		parity_final_wire = {(q_wire[37] ^ parity_final_wire[36]), (q_wire[36] ^ parity_final_wire[35]), (q_wire[35] ^ parity_final_wire[34]), (q_wire[34] ^ parity_final_wire[33]), (q_wire[33] ^ parity_final_wire[32]), (q_wire[32] ^ parity_final_wire[31]), (q_wire[31] ^ parity_final_wire[30]), (q_wire[30] ^ parity_final_wire[29]), (q_wire[29] ^ parity_final_wire[28]), (q_wire[28] ^ parity_final_wire[27]), (q_wire[27] ^ parity_final_wire[26]), (q_wire[26] ^ parity_final_wire[25]), (q_wire[25] ^ parity_final_wire[24]), (q_wire[24] ^ parity_final_wire[23]), (q_wire[23] ^ parity_final_wire[22]), (q_wire[22] ^ parity_final_wire[21]), (q_wire[21] ^ parity_final_wire[20]), (q_wire[20] ^ parity_final_wire[19]), (q_wire[19] ^ parity_final_wire[18]), (q_wire[18] ^ parity_final_wire[17]), (q_wire[17] ^ parity_final_wire[16]), (q_wire[16] ^ parity_final_wire[15]), (q_wire[15] ^ parity_final_wire[14]), (q_wire[14] ^ parity_final_wire[13]), (q_wire[13] ^ parity_final_wire[12]), (q_wire[12] ^ parity_final_wire[11]), (q_wire[11] ^ parity_final_wire[10]), (q_wire[10] ^ parity_final_wire[9]), (q_wire[9] ^ parity_final_wire[8]), (q_wire[8] ^ parity_final_wire[7]), (q_wire[7] ^ parity_final_wire[6]), (q_wire[6] ^ parity_final_wire[5]), (q_wire[5] ^ parity_final_wire[4]), (q_wire[4] ^ parity_final_wire[3]), (q_wire[3] ^ parity_final_wire[2]), (q_wire[2] ^ parity_final_wire[1]), (q_wire[1] ^ parity_final_wire[0]), q_wire[0]},
		q = output_pipeline0c,
		q_wire = {parity_final_wire[37], parity_06_wire[5], parity_05_wire[0], parity_04_wire[1], parity_03_wire[4], parity_02_wire[9], parity_01_wire[17], data_wire};
endmodule //ddr2_v10_1_0002_alt_ddrx_encoder_40_altecc_encoder_plb
//VALID FILE


// synopsys translate_off
`timescale 1 ps / 1 ps
// synopsys translate_on
module ddr2_v10_1_0002_alt_ddrx_encoder_40 (
	clock,
	data,
	q);

	input	  clock;
	input	[31:0]  data;
	output	[38:0]  q;

	wire [38:0] sub_wire0;
	wire [38:0] q = sub_wire0[38:0];

	ddr2_v10_1_0002_alt_ddrx_encoder_40_altecc_encoder_plb	ddr2_v10_1_0002_alt_ddrx_encoder_40_altecc_encoder_plb_component (
				.clock (clock),
				.data (data),
				.q (sub_wire0));

endmodule

// ============================================================
// CNX file retrieval info
// ============================================================
// Retrieval info: PRIVATE: INTENDED_DEVICE_FAMILY STRING "Stratix"
// Retrieval info: PRIVATE: SYNTH_WRAPPER_GEN_POSTFIX STRING "0"
// Retrieval info: LIBRARY: altera_mf altera_mf.altera_mf_components.all
// Retrieval info: CONSTANT: INTENDED_DEVICE_FAMILY STRING "Stratix"
// Retrieval info: CONSTANT: lpm_pipeline NUMERIC "1"
// Retrieval info: CONSTANT: width_codeword NUMERIC "39"
// Retrieval info: CONSTANT: width_dataword NUMERIC "32"
// Retrieval info: USED_PORT: clock 0 0 0 0 INPUT NODEFVAL "clock"
// Retrieval info: USED_PORT: data 0 0 32 0 INPUT NODEFVAL "data[31..0]"
// Retrieval info: USED_PORT: q 0 0 39 0 OUTPUT NODEFVAL "q[38..0]"
// Retrieval info: CONNECT: @data 0 0 32 0 data 0 0 32 0
// Retrieval info: CONNECT: q 0 0 39 0 @q 0 0 39 0
// Retrieval info: CONNECT: @clock 0 0 0 0 clock 0 0 0 0
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40.v TRUE
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40.inc FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40.cmp FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40.bsf FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40_inst.v FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL ddr2_v10_1_0002_alt_ddrx_encoder_40_bb.v FALSE
