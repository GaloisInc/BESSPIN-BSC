// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis VERILOG_INPUT_VERSION VERILOG_2001






//synthesis_resources = reg 18 stratixiii_ddio_in 8 stratixiii_ddio_oe 2 stratixiii_ddio_out 47 stratixiii_delay_chain 67 stratixiii_dqs_config 2 stratixiii_dqs_delay_chain 1 stratixiii_dqs_enable 1 stratixiii_dqs_enable_ctrl 1 stratixiii_io_clock_divider 2 stratixiii_io_config 10 
// synopsys translate_off
`timescale 1 ps / 1 ps
// synopsys translate_on
module  ddr2_v10_1_0002_bidir_dq_dqs_hr_ddr3
	( 
	config_clk,
	config_datain,
	config_update,
	bidir_dq_io_config_ena,
	output_dm_io_config_ena,
	dqs_config_ena,
	dqs_io_config_ena,
	dqsn_io_config_ena,
	output_dm_hr_output_data_in,
	output_dm_output_data_out,
	bidir_dq_hr_oe_in,
	bidir_dq_hr_output_data_in,
	bidir_dq_input_data_in,
	bidir_dq_input_data_out_high,
	bidir_dq_input_data_out_low,
	bidir_dq_oct_out,
	bidir_dq_oe_out,
	bidir_dq_output_data_out,
	dll_delayctrlin,
	dq_hr_output_reg_clk,
	dq_output_reg_clk,
	dq_output_reg_clkena,
	dqs_bus_out,
	dqs_enable_ctrl_clk,
	dqs_enable_ctrl_in,
	dqs_hr_oe_in,
	dqs_hr_output_data_in,
	dqs_hr_output_reg_clk,
	dqs_input_data_in,
	dqs_oct_out,
	dqs_oe_out,
	dqs_output_data_out,
	dqs_output_reg_clk,
	dqs_output_reg_clkena,
	dqsn_hr_oe_in,
	dqsn_oct_out,
	dqsn_oe_out,
	hr_oct_in,
	hr_oct_reg_clk,
	oct_reg_clk) /* synthesis synthesis_clearbox=1 */;

    input	[7:0]  bidir_dq_io_config_ena;
	input	output_dm_io_config_ena;
    input	config_clk;
    input	config_datain;
    input	config_update;
    input	dqs_config_ena;
    input	[0:0]  dqs_io_config_ena;
    input	[0:0]  dqsn_io_config_ena;

	input	[3:0]	output_dm_hr_output_data_in;
	output	[0:0]	output_dm_output_data_out;

	input   [15:0]  bidir_dq_hr_oe_in;
	input   [31:0]  bidir_dq_hr_output_data_in;
	input   [7:0]  bidir_dq_input_data_in;
	output   [7:0]  bidir_dq_input_data_out_high;
	output   [7:0]  bidir_dq_input_data_out_low;
	output   [7:0]  bidir_dq_oct_out;
	output   [7:0]  bidir_dq_oe_out;
	output   [7:0]  bidir_dq_output_data_out;
	input   [5:0]  dll_delayctrlin;
	
	input   dq_hr_output_reg_clk;
	input   dq_output_reg_clk;
	input   dq_output_reg_clkena;
	output   [0:0]  dqs_bus_out;
	input   dqs_enable_ctrl_clk;
	input   dqs_enable_ctrl_in;
	input   [1:0]  dqs_hr_oe_in;
	input   [3:0]  dqs_hr_output_data_in;
	input   dqs_hr_output_reg_clk;
	input   [0:0]  dqs_input_data_in;
	output   dqs_oct_out;
	output   [0:0]  dqs_oe_out;
	output   [0:0]  dqs_output_data_out;
	input   dqs_output_reg_clk;
	input   dqs_output_reg_clkena;
	input   [1:0]  dqsn_hr_oe_in;
	output   dqsn_oct_out;
	output   [0:0]  dqsn_oe_out;
	input   [1:0]  hr_oct_in;
	input   hr_oct_reg_clk;
	input   oct_reg_clk;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   [15:0]  bidir_dq_hr_oe_in;
	tri0   [31:0]  bidir_dq_hr_output_data_in;
	tri0   [7:0]  bidir_dq_input_data_in;
	tri0   [5:0]  dll_delayctrlin;
	tri0   dq_hr_output_reg_clk;
	tri0   dq_output_reg_clk;
	tri1   dq_output_reg_clkena;
	tri1   dqs_enable_ctrl_clk;
	tri1   dqs_enable_ctrl_in;
	tri0   [1:0]  dqs_hr_oe_in;
	tri0   [3:0]  dqs_hr_output_data_in;
	tri0   dqs_hr_output_reg_clk;
	tri0   [0:0]  dqs_input_data_in;
	tri0   dqs_output_reg_clk;
	tri1   dqs_output_reg_clkena;
	tri0   [1:0]  dqsn_hr_oe_in;
	tri0   [1:0]  hr_oct_in;
	tri0   hr_oct_reg_clk;
	tri0   oct_reg_clk;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire	wire_output_dm_0_output_delay_chain2_inst_dataout;
	wire	wire_output_dm_0_output_delay_chain1_inst_dataout;
	wire	[3:0] wire_output_dm_0_io_config_inst_outputdelaysetting1;
	wire	[2:0] wire_output_dm_0_io_config_inst_outputdelaysetting2;
	wire	wire_output_dm_0_output_ddio_out_inst_dataout;
	wire	wire_output_dm_0_output_hr_ddio_out_high_inst_dataout;
	wire	wire_output_dm_0_output_hr_ddio_out_low_inst_dataout;

	wire	bidir_dq_0_oct_ff_inst;
	wire	bidir_dq_0_oe_ff_inst;
	wire	bidir_dq_1_oct_ff_inst;
	wire	bidir_dq_1_oe_ff_inst;
	wire	bidir_dq_2_oct_ff_inst;
	wire	bidir_dq_2_oe_ff_inst;
	wire	bidir_dq_3_oct_ff_inst;
	wire	bidir_dq_3_oe_ff_inst;
	wire	bidir_dq_4_oct_ff_inst;
	wire	bidir_dq_4_oe_ff_inst;
	wire	bidir_dq_5_oct_ff_inst;
	wire	bidir_dq_5_oe_ff_inst;
	wire	bidir_dq_6_oct_ff_inst;
	wire	bidir_dq_6_oe_ff_inst;
	wire	bidir_dq_7_oct_ff_inst;
	wire	bidir_dq_7_oe_ff_inst;

	wire	[2:0]	dqsinputphasesetting; //clau
	wire	[2:0]	dqsenabledelaysetting;
	
	wire	[3:0]	dqoutputphasesetting0;
	wire			dqoutputphaseinvert0;
	wire	[3:0]	dqsoutputphasesetting0;
	wire			dqsoutputphaseinvert0;
	wire			enaoutputcycledelaysetting0;
	wire			enaoutputphasetransferreg0;
	wire			enaoctcycledelaysetting0;
	wire			enaoctphasetransferreg0;
	
	wire	[3:0]	dqoutputphasesetting1;
	wire			dqoutputphaseinvert1;
	wire	[3:0]	dqsoutputphasesetting1;
	wire			dqsoutputphaseinvert1;
	wire			enaoutputcycledelaysetting1;
	wire			enaoutputphasetransferreg1;
	wire			enaoctcycledelaysetting1;
	wire			enaoctphasetransferreg1;
	
	wire  wire_bidir_dq_0_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_0_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_1_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_1_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_2_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_2_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_3_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_3_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_4_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_4_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_5_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_5_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_6_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_6_ddio_in_inst_regoutlo;
	wire  wire_bidir_dq_7_ddio_in_inst_regouthi;
	wire  wire_bidir_dq_7_ddio_in_inst_regoutlo;
	wire  wire_dqs_0_oe_ddio_oe_inst_dataout;
	wire  wire_dqsn_0_oe_ddio_oe_inst_dataout;
	wire  wire_bidir_dq_0_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_0_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_0_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_0_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_0_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_1_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_1_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_1_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_1_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_1_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_2_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_2_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_2_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_2_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_2_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_3_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_3_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_3_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_3_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_3_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_4_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_4_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_4_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_4_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_4_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_5_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_5_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_5_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_5_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_5_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_6_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_6_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_6_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_6_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_6_output_hr_ddio_out_low_inst_dataout;
	wire  wire_bidir_dq_7_oct_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_7_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_7_output_ddio_out_inst_dataout;
	wire  wire_bidir_dq_7_output_hr_ddio_out_high_inst_dataout;
	wire  wire_bidir_dq_7_output_hr_ddio_out_low_inst_dataout;
	wire  wire_dqs_0_oct_hr_ddio_out_inst_dataout;
	wire  wire_dqs_0_oe_hr_ddio_out_inst_dataout;
	wire  wire_dqs_0_output_ddio_out_inst_dataout;
	wire  wire_dqs_oe_0_output_ddio_out_inst_dataout;
	wire  wire_dqsn_oe_0_output_ddio_out_inst_dataout;
	wire  wire_dqs_oct_0_output_ddio_out_inst_dataout;
	wire  wire_dqsn_oct_0_output_ddio_out_inst_dataout;
	wire  wire_dqs_0_output_hr_ddio_out_high_inst_dataout;
	wire  wire_dqs_0_output_hr_ddio_out_low_inst_dataout;
	wire  wire_dqsn_0_oct_hr_ddio_out_inst_dataout;
	wire  wire_dqsn_0_oe_hr_ddio_out_inst_dataout;
	wire  wire_bidir_dq_0_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_0_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_0_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_0_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_0_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_0_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_0_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_1_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_1_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_1_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_1_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_1_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_1_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_1_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_2_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_2_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_2_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_2_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_2_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_2_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_2_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_3_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_3_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_3_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_3_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_3_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_3_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_3_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_4_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_4_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_4_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_4_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_4_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_4_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_4_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_5_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_5_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_5_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_5_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_5_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_5_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_5_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_6_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_6_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_6_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_6_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_6_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_6_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_6_output_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_7_input_delay_chain_inst_dataout;
	wire  wire_bidir_dq_7_oct_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_7_oct_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_7_oe_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_7_oe_delay_chain2_inst_dataout;
	wire  wire_bidir_dq_7_output_delay_chain1_inst_dataout;
	wire  wire_bidir_dq_7_output_delay_chain2_inst_dataout;
	wire  wire_dqs_0_oct_delay_chain1_inst_dataout;
	wire  wire_dqs_0_oct_delay_chain2_inst_dataout;
	wire  wire_dqs_0_oe_delay_chain1_inst_dataout;
	wire  wire_dqs_0_oe_delay_chain2_inst_dataout;
	wire  wire_dqs_0_output_delay_chain1_inst_dataout;
	wire  wire_dqs_0_output_delay_chain2_inst_dataout;
	wire  wire_dqsbusout_delay_chain_inst_dataout;
	wire  wire_dqsn_0_oct_delay_chain1_inst_dataout;
	wire  wire_dqsn_0_oct_delay_chain2_inst_dataout;
	wire  wire_dqsn_0_oe_delay_chain1_inst_dataout;
	wire  wire_dqsn_0_oe_delay_chain2_inst_dataout;
	wire  wire_dqs_config_0_0_inst_dividerphasesetting;
	wire  [3:0]   wire_dqs_config_0_0_inst_dqsbusoutdelaysetting;
	wire wire_dqs_config_0_0_inst_dqsenablectrlphaseinvert;
	wire wire_dqs_config_0_0_inst_enadqsenablephasetransferreg;
    wire  [3:0]   wire_dqs_config_0_0_inst_dqsenablectrlphasesetting;
	wire  [3:0]   wire_dqs_config_0_0_inst_octdelaysetting1;
	wire  [2:0]   wire_dqs_config_0_0_inst_octdelaysetting2;
	wire  wire_dqs_config_0_1_inst_dividerphasesetting;
	wire  [3:0]   wire_dqs_config_0_1_inst_octdelaysetting1;
	wire  [2:0]   wire_dqs_config_0_1_inst_octdelaysetting2;
	wire  wire_dqs_0_delay_chain_inst_dqsbusout;
	wire  wire_dqs_0_enable_inst_dqsbusout;
	wire  wire_dqs_0_enable_ctrl_inst_dqsenableout;
	wire  wire_dqs_0_enable_ctrl_inst_dqsenableout_delay;
	wire  wire_io_clock_divider_0_0_inst_slaveout;
	wire  [3:0]   wire_bidir_dq_0_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_0_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_0_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_1_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_1_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_1_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_2_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_2_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_2_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_3_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_3_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_3_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_4_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_4_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_4_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_5_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_5_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_5_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_6_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_6_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_6_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_bidir_dq_7_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_bidir_dq_7_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_bidir_dq_7_io_config_inst_padtoinputregisterdelaysetting;
	wire  [3:0]   wire_dqs_0_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_dqs_0_io_config_inst_outputdelaysetting2;
	wire  [3:0]   wire_dqsn_0_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_dqsn_0_io_config_inst_outputdelaysetting2;
	wire  [0:0]  dqs_bus_wire;
	wire io_clock_divider_clk;




	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

	stratixiii_output_phase_alignment	dq_7_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_7_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting1),
	.phaseinvertctrl (dqsoutputphaseinvert1),
	.enaoutputcycledelay (enaoctcycledelaysetting1),
	.enaphasetransferreg (enaoctphasetransferreg1),
	.clkena			(1'b1),
	.dataout		(bidir_dq_7_oct_ff_inst)
	);
    defparam
        dq_7_oct_output_phase_align.operation_mode = "rtena",
        dq_7_oct_output_phase_align.use_phasectrlin = "true",
        dq_7_oct_output_phase_align.delay_buffer_mode = "high",
        dq_7_oct_output_phase_align.power_up = "low",
        dq_7_oct_output_phase_align.async_mode = "none",
        dq_7_oct_output_phase_align.sync_mode = "none",
        dq_7_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_7_oct_output_phase_align.use_primary_clock = "true",
        dq_7_oct_output_phase_align.use_delayed_clock = "true",
        dq_7_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_7_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_7_oct_output_phase_align.invert_phase = "dynamic",
        dq_7_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_6_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_6_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting1),
	.phaseinvertctrl (dqsoutputphaseinvert1),
	.enaoutputcycledelay (enaoctcycledelaysetting1),
	.enaphasetransferreg (enaoctphasetransferreg1),
	.clkena			(1'b1),
	.dataout		(bidir_dq_6_oct_ff_inst)
	);
    defparam
        dq_6_oct_output_phase_align.operation_mode = "rtena",
        dq_6_oct_output_phase_align.use_phasectrlin = "true",
        dq_6_oct_output_phase_align.delay_buffer_mode = "high",
        dq_6_oct_output_phase_align.power_up = "low",
        dq_6_oct_output_phase_align.async_mode = "none",
        dq_6_oct_output_phase_align.sync_mode = "none",
        dq_6_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_6_oct_output_phase_align.use_primary_clock = "true",
        dq_6_oct_output_phase_align.use_delayed_clock = "true",
        dq_6_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_6_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_6_oct_output_phase_align.invert_phase = "dynamic",
        dq_6_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_5_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_5_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting1),
	.phaseinvertctrl (dqsoutputphaseinvert1),
	.enaoutputcycledelay (enaoctcycledelaysetting1),
	.enaphasetransferreg (enaoctphasetransferreg1),
	.clkena			(1'b1),
	.dataout		(bidir_dq_5_oct_ff_inst)
	);
    defparam
        dq_5_oct_output_phase_align.operation_mode = "rtena",
        dq_5_oct_output_phase_align.use_phasectrlin = "true",
        dq_5_oct_output_phase_align.delay_buffer_mode = "high",
        dq_5_oct_output_phase_align.power_up = "low",
        dq_5_oct_output_phase_align.async_mode = "none",
        dq_5_oct_output_phase_align.sync_mode = "none",
        dq_5_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_5_oct_output_phase_align.use_primary_clock = "true",
        dq_5_oct_output_phase_align.use_delayed_clock = "true",
        dq_5_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_5_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_5_oct_output_phase_align.invert_phase = "dynamic",
        dq_5_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_4_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_4_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting1),
	.phaseinvertctrl (dqsoutputphaseinvert1),
	.enaoutputcycledelay (enaoctcycledelaysetting1),
	.enaphasetransferreg (enaoctphasetransferreg1),
	.clkena			(1'b1),
	.dataout		(bidir_dq_4_oct_ff_inst)
	);
    defparam
        dq_4_oct_output_phase_align.operation_mode = "rtena",
        dq_4_oct_output_phase_align.use_phasectrlin = "true",
        dq_4_oct_output_phase_align.delay_buffer_mode = "high",
        dq_4_oct_output_phase_align.power_up = "low",
        dq_4_oct_output_phase_align.async_mode = "none",
        dq_4_oct_output_phase_align.sync_mode = "none",
        dq_4_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_4_oct_output_phase_align.use_primary_clock = "true",
        dq_4_oct_output_phase_align.use_delayed_clock = "true",
        dq_4_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_4_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_4_oct_output_phase_align.invert_phase = "dynamic",
        dq_4_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_3_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_3_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
	.clkena			(1'b1),
	.dataout		(bidir_dq_3_oct_ff_inst)
	);
    defparam
        dq_3_oct_output_phase_align.operation_mode = "rtena",
        dq_3_oct_output_phase_align.use_phasectrlin = "true",
        dq_3_oct_output_phase_align.delay_buffer_mode = "high",
        dq_3_oct_output_phase_align.power_up = "low",
        dq_3_oct_output_phase_align.async_mode = "none",
        dq_3_oct_output_phase_align.sync_mode = "none",
        dq_3_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_3_oct_output_phase_align.use_primary_clock = "true",
        dq_3_oct_output_phase_align.use_delayed_clock = "true",
        dq_3_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_3_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_3_oct_output_phase_align.invert_phase = "dynamic",
        dq_3_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_2_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_2_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
	.clkena			(1'b1),
	.dataout		(bidir_dq_2_oct_ff_inst)
	);
    defparam
        dq_2_oct_output_phase_align.operation_mode = "rtena",
        dq_2_oct_output_phase_align.use_phasectrlin = "true",
        dq_2_oct_output_phase_align.delay_buffer_mode = "high",
        dq_2_oct_output_phase_align.power_up = "low",
        dq_2_oct_output_phase_align.async_mode = "none",
        dq_2_oct_output_phase_align.sync_mode = "none",
        dq_2_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_2_oct_output_phase_align.use_primary_clock = "true",
        dq_2_oct_output_phase_align.use_delayed_clock = "true",
        dq_2_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_2_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_2_oct_output_phase_align.invert_phase = "dynamic",
        dq_2_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_1_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_1_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
	.clkena			(1'b1),
	.dataout		(bidir_dq_1_oct_ff_inst)
	);
    defparam
        dq_1_oct_output_phase_align.operation_mode = "rtena",
        dq_1_oct_output_phase_align.use_phasectrlin = "true",
        dq_1_oct_output_phase_align.delay_buffer_mode = "high",
        dq_1_oct_output_phase_align.power_up = "low",
        dq_1_oct_output_phase_align.async_mode = "none",
        dq_1_oct_output_phase_align.sync_mode = "none",
        dq_1_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_1_oct_output_phase_align.use_primary_clock = "true",
        dq_1_oct_output_phase_align.use_delayed_clock = "true",
        dq_1_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_1_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_1_oct_output_phase_align.invert_phase = "dynamic",
        dq_1_oct_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_0_oct_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_0_oct_hr_ddio_out_inst_dataout}),
	.clk			(oct_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin	(dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
	.clkena			(1'b1),
	.dataout		(bidir_dq_0_oct_ff_inst)
	);
    defparam
        dq_0_oct_output_phase_align.operation_mode = "rtena",
        dq_0_oct_output_phase_align.use_phasectrlin = "true",
        dq_0_oct_output_phase_align.delay_buffer_mode = "high",
        dq_0_oct_output_phase_align.power_up = "low",
        dq_0_oct_output_phase_align.async_mode = "none",
        dq_0_oct_output_phase_align.sync_mode = "none",
        dq_0_oct_output_phase_align.use_phasectrl_clock = "true",
        dq_0_oct_output_phase_align.use_primary_clock = "true",
        dq_0_oct_output_phase_align.use_delayed_clock = "true",
        dq_0_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_0_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_0_oct_output_phase_align.invert_phase = "dynamic",
        dq_0_oct_output_phase_align.bypass_input_register = "false";


	stratixiii_output_phase_alignment	dq_7_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_7_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_7_oe_ff_inst)
	);
    defparam
        dq_7_oe_output_phase_align.operation_mode = "oe",
        dq_7_oe_output_phase_align.use_phasectrlin = "true",
        dq_7_oe_output_phase_align.delay_buffer_mode = "high",
        dq_7_oe_output_phase_align.power_up = "low",
        dq_7_oe_output_phase_align.async_mode = "none",
        dq_7_oe_output_phase_align.sync_mode = "none",
        dq_7_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_7_oe_output_phase_align.use_primary_clock = "true",
        dq_7_oe_output_phase_align.use_delayed_clock = "true",
        dq_7_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_7_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_7_oe_output_phase_align.invert_phase = "dynamic",
        dq_7_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_6_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_6_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_6_oe_ff_inst)
	);
    defparam
        dq_6_oe_output_phase_align.operation_mode = "oe",
        dq_6_oe_output_phase_align.use_phasectrlin = "true",
        dq_6_oe_output_phase_align.delay_buffer_mode = "high",
        dq_6_oe_output_phase_align.power_up = "low",
        dq_6_oe_output_phase_align.async_mode = "none",
        dq_6_oe_output_phase_align.sync_mode = "none",
        dq_6_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_6_oe_output_phase_align.use_primary_clock = "true",
        dq_6_oe_output_phase_align.use_delayed_clock = "true",
        dq_6_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_6_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_6_oe_output_phase_align.invert_phase = "dynamic",
        dq_6_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_5_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_5_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_5_oe_ff_inst)
	);
    defparam
        dq_5_oe_output_phase_align.operation_mode = "oe",
        dq_5_oe_output_phase_align.use_phasectrlin = "true",
        dq_5_oe_output_phase_align.delay_buffer_mode = "high",
        dq_5_oe_output_phase_align.power_up = "low",
        dq_5_oe_output_phase_align.async_mode = "none",
        dq_5_oe_output_phase_align.sync_mode = "none",
        dq_5_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_5_oe_output_phase_align.use_primary_clock = "true",
        dq_5_oe_output_phase_align.use_delayed_clock = "true",
        dq_5_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_5_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_5_oe_output_phase_align.invert_phase = "dynamic",
        dq_5_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_4_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_4_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_4_oe_ff_inst)
	);
    defparam
        dq_4_oe_output_phase_align.operation_mode = "oe",
        dq_4_oe_output_phase_align.use_phasectrlin = "true",
        dq_4_oe_output_phase_align.delay_buffer_mode = "high",
        dq_4_oe_output_phase_align.power_up = "low",
        dq_4_oe_output_phase_align.async_mode = "none",
        dq_4_oe_output_phase_align.sync_mode = "none",
        dq_4_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_4_oe_output_phase_align.use_primary_clock = "true",
        dq_4_oe_output_phase_align.use_delayed_clock = "true",
        dq_4_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_4_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_4_oe_output_phase_align.invert_phase = "dynamic",
        dq_4_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_3_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_3_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_3_oe_ff_inst)
	);
    defparam
        dq_3_oe_output_phase_align.operation_mode = "oe",
        dq_3_oe_output_phase_align.use_phasectrlin = "true",
        dq_3_oe_output_phase_align.delay_buffer_mode = "high",
        dq_3_oe_output_phase_align.power_up = "low",
        dq_3_oe_output_phase_align.async_mode = "none",
        dq_3_oe_output_phase_align.sync_mode = "none",
        dq_3_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_3_oe_output_phase_align.use_primary_clock = "true",
        dq_3_oe_output_phase_align.use_delayed_clock = "true",
        dq_3_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_3_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_3_oe_output_phase_align.invert_phase = "dynamic",
        dq_3_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_2_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_2_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_2_oe_ff_inst)
	);
    defparam
        dq_2_oe_output_phase_align.operation_mode = "oe",
        dq_2_oe_output_phase_align.use_phasectrlin = "true",
        dq_2_oe_output_phase_align.delay_buffer_mode = "high",
        dq_2_oe_output_phase_align.power_up = "low",
        dq_2_oe_output_phase_align.async_mode = "none",
        dq_2_oe_output_phase_align.sync_mode = "none",
        dq_2_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_2_oe_output_phase_align.use_primary_clock = "true",
        dq_2_oe_output_phase_align.use_delayed_clock = "true",
        dq_2_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_2_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_2_oe_output_phase_align.invert_phase = "dynamic",
        dq_2_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_1_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_1_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_1_oe_ff_inst)
	);
    defparam
        dq_1_oe_output_phase_align.operation_mode = "oe",
        dq_1_oe_output_phase_align.use_phasectrlin = "true",
        dq_1_oe_output_phase_align.delay_buffer_mode = "high",
        dq_1_oe_output_phase_align.power_up = "low",
        dq_1_oe_output_phase_align.async_mode = "none",
        dq_1_oe_output_phase_align.sync_mode = "none",
        dq_1_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_1_oe_output_phase_align.use_primary_clock = "true",
        dq_1_oe_output_phase_align.use_delayed_clock = "true",
        dq_1_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_1_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_1_oe_output_phase_align.invert_phase = "dynamic",
        dq_1_oe_output_phase_align.bypass_input_register = "false";

	stratixiii_output_phase_alignment	dq_0_oe_output_phase_align
	(
	.datain			({1'b0,wire_bidir_dq_0_oe_hr_ddio_out_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin	(dll_delayctrlin),
	.phasectrlin	(dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
	.clkena			(dq_output_reg_clkena),
	.dataout		(bidir_dq_0_oe_ff_inst)
	);
    defparam
        dq_0_oe_output_phase_align.operation_mode = "oe",
        dq_0_oe_output_phase_align.use_phasectrlin = "true",
        dq_0_oe_output_phase_align.delay_buffer_mode = "high",
        dq_0_oe_output_phase_align.power_up = "low",
        dq_0_oe_output_phase_align.async_mode = "none",
        dq_0_oe_output_phase_align.sync_mode = "none",
        dq_0_oe_output_phase_align.use_phasectrl_clock = "true",
        dq_0_oe_output_phase_align.use_primary_clock = "true",
        dq_0_oe_output_phase_align.use_delayed_clock = "true",
        dq_0_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_0_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_0_oe_output_phase_align.invert_phase = "dynamic",
        dq_0_oe_output_phase_align.bypass_input_register = "false";


	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on



	stratixiii_ddio_in   bidir_dq_0_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_0_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_0_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_0_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_0_ddio_in_inst.async_mode = "none",
		bidir_dq_0_ddio_in_inst.sync_mode = "none",
		bidir_dq_0_ddio_in_inst.use_clkn = "false",
		bidir_dq_0_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_1_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_1_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_1_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_1_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_1_ddio_in_inst.async_mode = "none",
		bidir_dq_1_ddio_in_inst.sync_mode = "none",
		bidir_dq_1_ddio_in_inst.use_clkn = "false",
		bidir_dq_1_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_2_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_2_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_2_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_2_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_2_ddio_in_inst.async_mode = "none",
		bidir_dq_2_ddio_in_inst.sync_mode = "none",
		bidir_dq_2_ddio_in_inst.use_clkn = "false",
		bidir_dq_2_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_3_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_3_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_3_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_3_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_3_ddio_in_inst.async_mode = "none",
		bidir_dq_3_ddio_in_inst.sync_mode = "none",
		bidir_dq_3_ddio_in_inst.use_clkn = "false",
		bidir_dq_3_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_4_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_4_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_4_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_4_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_4_ddio_in_inst.async_mode = "none",
		bidir_dq_4_ddio_in_inst.sync_mode = "none",
		bidir_dq_4_ddio_in_inst.use_clkn = "false",
		bidir_dq_4_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_5_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_5_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_5_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_5_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_5_ddio_in_inst.async_mode = "none",
		bidir_dq_5_ddio_in_inst.sync_mode = "none",
		bidir_dq_5_ddio_in_inst.use_clkn = "false",
		bidir_dq_5_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_6_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_6_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_6_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_6_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_6_ddio_in_inst.async_mode = "none",
		bidir_dq_6_ddio_in_inst.sync_mode = "none",
		bidir_dq_6_ddio_in_inst.use_clkn = "false",
		bidir_dq_6_ddio_in_inst.lpm_type = "stratixiii_ddio_in";
	stratixiii_ddio_in   bidir_dq_7_ddio_in_inst
	( 
	.clk((~ dqs_bus_wire[0])),
	.datain(wire_bidir_dq_7_input_delay_chain_inst_dataout),
	.regouthi(wire_bidir_dq_7_ddio_in_inst_regouthi),
	.regoutlo(wire_bidir_dq_7_ddio_in_inst_regoutlo)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clkn(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_7_ddio_in_inst.async_mode = "none",
		bidir_dq_7_ddio_in_inst.sync_mode = "none",
		bidir_dq_7_ddio_in_inst.use_clkn = "false",
		bidir_dq_7_ddio_in_inst.lpm_type = "stratixiii_ddio_in";

	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on
	stratixiii_ddio_out   bidir_dq_0_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_0_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_0_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_0_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_0_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_0_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[1])),
	.datainlo((~ bidir_dq_hr_oe_in[0])),
	.dataout(wire_bidir_dq_0_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_0_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_0_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_0_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_0_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_0_output_phase_align
    (
    .datain         ({wire_bidir_dq_0_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_0_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
    .phasectrlin    (dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_0_output_ddio_out_inst_dataout)
    );
    defparam
        dq_0_output_phase_align.operation_mode = "ddio_out",
        dq_0_output_phase_align.use_phasectrlin = "true",
        dq_0_output_phase_align.delay_buffer_mode = "high",
        dq_0_output_phase_align.power_up = "low",
        dq_0_output_phase_align.async_mode = "none",
        dq_0_output_phase_align.sync_mode = "none",
        dq_0_output_phase_align.use_phasectrl_clock = "true",
        dq_0_output_phase_align.use_primary_clock = "true",
        dq_0_output_phase_align.use_delayed_clock = "true",
        dq_0_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_0_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_0_output_phase_align.invert_phase = "dynamic",
        dq_0_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_0_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[3]),
	.datainlo(bidir_dq_hr_output_data_in[2]),
	.dataout(wire_bidir_dq_0_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_0_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_0_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_0_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_0_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_0_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[1]),
	.datainlo(bidir_dq_hr_output_data_in[0]),
	.dataout(wire_bidir_dq_0_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_0_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_0_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_0_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_0_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_1_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_1_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_1_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_1_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_1_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_1_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[3])),
	.datainlo((~ bidir_dq_hr_oe_in[2])),
	.dataout(wire_bidir_dq_1_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_1_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_1_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_1_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_1_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_1_output_phase_align
    (
    .datain         ({wire_bidir_dq_1_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_1_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
    .phasectrlin    (dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_1_output_ddio_out_inst_dataout)
    );
    defparam
        dq_1_output_phase_align.operation_mode = "ddio_out",
        dq_1_output_phase_align.use_phasectrlin = "true",
        dq_1_output_phase_align.delay_buffer_mode = "high",
        dq_1_output_phase_align.power_up = "low",
        dq_1_output_phase_align.async_mode = "none",
        dq_1_output_phase_align.sync_mode = "none",
        dq_1_output_phase_align.use_phasectrl_clock = "true",
        dq_1_output_phase_align.use_primary_clock = "true",
        dq_1_output_phase_align.use_delayed_clock = "true",
        dq_1_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_1_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_1_output_phase_align.invert_phase = "dynamic",
        dq_1_output_phase_align.bypass_input_register = "false";

	stratixiii_ddio_out   bidir_dq_1_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[7]),
	.datainlo(bidir_dq_hr_output_data_in[6]),
	.dataout(wire_bidir_dq_1_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_1_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_1_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_1_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_1_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_1_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[5]),
	.datainlo(bidir_dq_hr_output_data_in[4]),
	.dataout(wire_bidir_dq_1_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_1_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_1_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_1_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_1_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_2_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_2_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_2_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_2_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_2_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_2_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[5])),
	.datainlo((~ bidir_dq_hr_oe_in[4])),
	.dataout(wire_bidir_dq_2_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_2_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_2_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_2_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_2_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_2_output_phase_align
    (
    .datain         ({wire_bidir_dq_2_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_2_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
    .phasectrlin    (dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_2_output_ddio_out_inst_dataout)
    );
    defparam
        dq_2_output_phase_align.operation_mode = "ddio_out",
        dq_2_output_phase_align.use_phasectrlin = "true",
        dq_2_output_phase_align.delay_buffer_mode = "high",
        dq_2_output_phase_align.power_up = "low",
        dq_2_output_phase_align.async_mode = "none",
        dq_2_output_phase_align.sync_mode = "none",
        dq_2_output_phase_align.use_phasectrl_clock = "true",
        dq_2_output_phase_align.use_primary_clock = "true",
        dq_2_output_phase_align.use_delayed_clock = "true",
        dq_2_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_2_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_2_output_phase_align.invert_phase = "dynamic",
        dq_2_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_2_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[11]),
	.datainlo(bidir_dq_hr_output_data_in[10]),
	.dataout(wire_bidir_dq_2_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_2_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_2_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_2_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_2_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_2_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[9]),
	.datainlo(bidir_dq_hr_output_data_in[8]),
	.dataout(wire_bidir_dq_2_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_2_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_2_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_2_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_2_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_3_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_3_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_3_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_3_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_3_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_3_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[7])),
	.datainlo((~ bidir_dq_hr_oe_in[6])),
	.dataout(wire_bidir_dq_3_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_3_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_3_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_3_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_3_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_3_output_phase_align
    (
    .datain         ({wire_bidir_dq_3_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_3_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
    .phasectrlin    (dqoutputphasesetting0),
	.phaseinvertctrl (dqoutputphaseinvert0),
	.enaoutputcycledelay (enaoutputcycledelaysetting0),
	.enaphasetransferreg (enaoutputphasetransferreg0),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_3_output_ddio_out_inst_dataout)
    );
    defparam
        dq_3_output_phase_align.operation_mode = "ddio_out",
        dq_3_output_phase_align.use_phasectrlin = "true",
        dq_3_output_phase_align.delay_buffer_mode = "high",
        dq_3_output_phase_align.power_up = "low",
        dq_3_output_phase_align.async_mode = "none",
        dq_3_output_phase_align.sync_mode = "none",
        dq_3_output_phase_align.use_phasectrl_clock = "true",
        dq_3_output_phase_align.use_primary_clock = "true",
        dq_3_output_phase_align.use_delayed_clock = "true",
        dq_3_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_3_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_3_output_phase_align.invert_phase = "dynamic",
        dq_3_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_3_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[15]),
	.datainlo(bidir_dq_hr_output_data_in[14]),
	.dataout(wire_bidir_dq_3_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_3_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_3_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_3_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_3_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_3_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[13]),
	.datainlo(bidir_dq_hr_output_data_in[12]),
	.dataout(wire_bidir_dq_3_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_3_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_3_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_3_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_3_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_4_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_4_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_4_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_4_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_4_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_4_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[9])),
	.datainlo((~ bidir_dq_hr_oe_in[8])),
	.dataout(wire_bidir_dq_4_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_4_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_4_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_4_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_4_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_4_output_phase_align
    (
    .datain         ({wire_bidir_dq_4_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_4_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
	.phasectrlin    (dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_4_output_ddio_out_inst_dataout)
    );
    defparam
        dq_4_output_phase_align.operation_mode = "ddio_out",
        dq_4_output_phase_align.use_phasectrlin = "true",
        dq_4_output_phase_align.delay_buffer_mode = "high",
        dq_4_output_phase_align.power_up = "low",
        dq_4_output_phase_align.async_mode = "none",
        dq_4_output_phase_align.sync_mode = "none",
        dq_4_output_phase_align.use_phasectrl_clock = "true",
        dq_4_output_phase_align.use_primary_clock = "true",
        dq_4_output_phase_align.use_delayed_clock = "true",
        dq_4_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_4_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_4_output_phase_align.invert_phase = "dynamic",
        dq_4_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_4_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[19]),
	.datainlo(bidir_dq_hr_output_data_in[18]),
	.dataout(wire_bidir_dq_4_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_4_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_4_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_4_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_4_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_4_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[17]),
	.datainlo(bidir_dq_hr_output_data_in[16]),
	.dataout(wire_bidir_dq_4_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_4_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_4_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_4_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_4_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_5_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_5_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_5_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_5_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_5_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_5_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[11])),
	.datainlo((~ bidir_dq_hr_oe_in[10])),
	.dataout(wire_bidir_dq_5_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_5_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_5_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_5_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_5_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_5_output_phase_align
    (
    .datain         ({wire_bidir_dq_5_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_5_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
	.phasectrlin    (dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_5_output_ddio_out_inst_dataout)
    );
    defparam
        dq_5_output_phase_align.operation_mode = "ddio_out",
        dq_5_output_phase_align.use_phasectrlin = "true",
        dq_5_output_phase_align.delay_buffer_mode = "high",
        dq_5_output_phase_align.power_up = "low",
        dq_5_output_phase_align.async_mode = "none",
        dq_5_output_phase_align.sync_mode = "none",
        dq_5_output_phase_align.use_phasectrl_clock = "true",
        dq_5_output_phase_align.use_primary_clock = "true",
        dq_5_output_phase_align.use_delayed_clock = "true",
        dq_5_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_5_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_5_output_phase_align.invert_phase = "dynamic",
        dq_5_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_5_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[23]),
	.datainlo(bidir_dq_hr_output_data_in[22]),
	.dataout(wire_bidir_dq_5_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_5_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_5_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_5_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_5_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_5_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[21]),
	.datainlo(bidir_dq_hr_output_data_in[20]),
	.dataout(wire_bidir_dq_5_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_5_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_5_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_5_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_5_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_6_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_6_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_6_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_6_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_6_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_6_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[13])),
	.datainlo((~ bidir_dq_hr_oe_in[12])),
	.dataout(wire_bidir_dq_6_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_6_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_6_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_6_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_6_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

    stratixiii_output_phase_alignment   dq_6_output_phase_align
    (
    .datain         ({wire_bidir_dq_6_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_6_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
	.phasectrlin    (dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_bidir_dq_6_output_ddio_out_inst_dataout)
    );
    defparam
        dq_6_output_phase_align.operation_mode = "ddio_out",
        dq_6_output_phase_align.use_phasectrlin = "true",
        dq_6_output_phase_align.delay_buffer_mode = "high",
        dq_6_output_phase_align.power_up = "low",
        dq_6_output_phase_align.async_mode = "none",
        dq_6_output_phase_align.sync_mode = "none",
        dq_6_output_phase_align.use_phasectrl_clock = "true",
        dq_6_output_phase_align.use_primary_clock = "true",
        dq_6_output_phase_align.use_delayed_clock = "true",
        dq_6_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_6_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_6_output_phase_align.invert_phase = "dynamic",
        dq_6_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_6_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[27]),
	.datainlo(bidir_dq_hr_output_data_in[26]),
	.dataout(wire_bidir_dq_6_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_6_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_6_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_6_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_6_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_6_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[25]),
	.datainlo(bidir_dq_hr_output_data_in[24]),
	.dataout(wire_bidir_dq_6_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_6_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_6_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_6_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_6_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_7_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_bidir_dq_7_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_7_oct_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_7_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_7_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_7_oe_hr_ddio_out_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi((~ bidir_dq_hr_oe_in[15])),
	.datainlo((~ bidir_dq_hr_oe_in[14])),
	.dataout(wire_bidir_dq_7_oe_hr_ddio_out_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_7_oe_hr_ddio_out_inst.async_mode = "none",
		bidir_dq_7_oe_hr_ddio_out_inst.half_rate_mode = "true",
		bidir_dq_7_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		bidir_dq_7_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

	stratixiii_output_phase_alignment	dq_7_output_phase_align
	(
	.datain			({wire_bidir_dq_7_output_hr_ddio_out_high_inst_dataout, wire_bidir_dq_7_output_hr_ddio_out_low_inst_dataout}),
	.clk			(dq_output_reg_clk),
	.delayctrlin    (dll_delayctrlin),
	.phasectrlin    (dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
	.clkena			(dq_output_reg_clkena),
	.dataout		(wire_bidir_dq_7_output_ddio_out_inst_dataout)
	);
    defparam
        dq_7_output_phase_align.operation_mode = "ddio_out",
        dq_7_output_phase_align.use_phasectrlin = "true",
        dq_7_output_phase_align.delay_buffer_mode = "high",
        dq_7_output_phase_align.power_up = "low",
        dq_7_output_phase_align.async_mode = "none",
        dq_7_output_phase_align.sync_mode = "none",
        dq_7_output_phase_align.use_phasectrl_clock = "true",
        dq_7_output_phase_align.use_primary_clock = "true",
        dq_7_output_phase_align.use_delayed_clock = "true",
        dq_7_output_phase_align.add_phase_transfer_reg = "dynamic",
        dq_7_output_phase_align.add_output_cycle_delay = "dynamic",
        dq_7_output_phase_align.invert_phase = "dynamic",
        dq_7_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   bidir_dq_7_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[31]),
	.datainlo(bidir_dq_hr_output_data_in[30]),
	.dataout(wire_bidir_dq_7_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_7_output_hr_ddio_out_high_inst.async_mode = "none",
		bidir_dq_7_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		bidir_dq_7_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		bidir_dq_7_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   bidir_dq_7_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(bidir_dq_hr_output_data_in[29]),
	.datainlo(bidir_dq_hr_output_data_in[28]),
	.dataout(wire_bidir_dq_7_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dq_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		bidir_dq_7_output_hr_ddio_out_low_inst.async_mode = "none",
		bidir_dq_7_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		bidir_dq_7_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		bidir_dq_7_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   dqs_0_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_dqs_0_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqs_0_oct_hr_ddio_out_inst.half_rate_mode = "true",
		dqs_0_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		dqs_0_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";

    stratixiii_output_phase_alignment   dqs_oct_output_phase_align
    (
    .datain             ({1'b0,wire_dqs_0_oct_hr_ddio_out_inst_dataout}),
    .clk                (oct_reg_clk),
    .delayctrlin        (dll_delayctrlin),
    .phasectrlin        (dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
    .clkena             (dqs_output_reg_clkena),
    .dataout            (wire_dqs_oct_0_output_ddio_out_inst_dataout)
    );
    defparam
        dqs_oct_output_phase_align.operation_mode = "rtena",
        dqs_oct_output_phase_align.use_phasectrlin = "true",
        dqs_oct_output_phase_align.delay_buffer_mode = "high",
        dqs_oct_output_phase_align.power_up = "low",
        dqs_oct_output_phase_align.async_mode = "none",
        dqs_oct_output_phase_align.sync_mode = "none",
        dqs_oct_output_phase_align.use_phasectrl_clock = "true",
        dqs_oct_output_phase_align.use_primary_clock = "true",
        dqs_oct_output_phase_align.use_delayed_clock = "true",
        dqs_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dqs_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dqs_oct_output_phase_align.invert_phase = "dynamic",
        dqs_oct_output_phase_align.bypass_input_register = "false";


    stratixiii_output_phase_alignment   dqsn_oct_output_phase_align
    (
    .datain             ({1'b0,wire_dqsn_0_oct_hr_ddio_out_inst_dataout}),
    .clk                (oct_reg_clk),
    .delayctrlin        (dll_delayctrlin),
    .phasectrlin        (dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
    .clkena             (dqs_output_reg_clkena),
    .dataout            (wire_dqsn_oct_0_output_ddio_out_inst_dataout)
    );
    defparam
        dqsn_oct_output_phase_align.operation_mode = "rtena",
        dqsn_oct_output_phase_align.use_phasectrlin = "true",
        dqsn_oct_output_phase_align.delay_buffer_mode = "high",
        dqsn_oct_output_phase_align.power_up = "low",
        dqsn_oct_output_phase_align.async_mode = "none",
        dqsn_oct_output_phase_align.sync_mode = "none",
        dqsn_oct_output_phase_align.use_phasectrl_clock = "true",
        dqsn_oct_output_phase_align.use_primary_clock = "true",
        dqsn_oct_output_phase_align.use_delayed_clock = "true",
        dqsn_oct_output_phase_align.add_phase_transfer_reg = "dynamic",
        dqsn_oct_output_phase_align.add_output_cycle_delay = "dynamic",
        dqsn_oct_output_phase_align.invert_phase = "dynamic",
        dqsn_oct_output_phase_align.bypass_input_register = "false";


	stratixiii_ddio_out   dqs_0_oe_hr_ddio_out_inst
	( 
	.clkhi(dqs_hr_output_reg_clk),
	.clklo(dqs_hr_output_reg_clk),
	.datainhi((~ dqs_hr_oe_in[1])),
	.datainlo((~ dqs_hr_oe_in[0])),
	.dataout(wire_dqs_0_oe_hr_ddio_out_inst_dataout),
	.muxsel(dqs_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqs_0_oe_hr_ddio_out_inst.async_mode = "none",
		dqs_0_oe_hr_ddio_out_inst.half_rate_mode = "true",
		dqs_0_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		dqs_0_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";

    stratixiii_output_phase_alignment   dqs_oe_output_phase_align
    (
    .datain             ({1'b0,wire_dqs_0_oe_hr_ddio_out_inst_dataout}),
    .clk                (dqs_output_reg_clk),
    .delayctrlin        (dll_delayctrlin),
    .phasectrlin        (dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
    .clkena             (dqs_output_reg_clkena),
    .dataout            (wire_dqs_oe_0_output_ddio_out_inst_dataout)
    );
    defparam
        dqs_oe_output_phase_align.operation_mode = "oe",
        dqs_oe_output_phase_align.use_phasectrlin = "true",
        dqs_oe_output_phase_align.delay_buffer_mode = "high",
        dqs_oe_output_phase_align.power_up = "low",
        dqs_oe_output_phase_align.async_mode = "none",
        dqs_oe_output_phase_align.sync_mode = "none",
        dqs_oe_output_phase_align.use_phasectrl_clock = "true",
        dqs_oe_output_phase_align.use_primary_clock = "true",
        dqs_oe_output_phase_align.use_delayed_clock = "true",
        dqs_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dqs_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dqs_oe_output_phase_align.invert_phase = "dynamic",
        dqs_oe_output_phase_align.bypass_input_register = "false";


	
	//// synopsys translate_off
	//// synopsys translate_on
	//// synopsys translate_off
	//// synopsys translate_on

	stratixiii_output_phase_alignment   dqs_output_phase_align
    (
    .datain             ({wire_dqs_0_output_hr_ddio_out_high_inst_dataout, wire_dqs_0_output_hr_ddio_out_low_inst_dataout}),
    .clk                (dqs_output_reg_clk),
    .delayctrlin        (dll_delayctrlin),
    .phasectrlin        (dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
    .clkena             (dqs_output_reg_clkena),
    .dataout            (wire_dqs_0_output_ddio_out_inst_dataout)
    );
    defparam
        dqs_output_phase_align.operation_mode = "ddio_out",
        dqs_output_phase_align.use_phasectrlin = "true",
        dqs_output_phase_align.delay_buffer_mode = "high",
        dqs_output_phase_align.power_up = "low",
        dqs_output_phase_align.async_mode = "none",
        dqs_output_phase_align.sync_mode = "none",
        dqs_output_phase_align.use_phasectrl_clock = "true",
        dqs_output_phase_align.use_primary_clock = "true",
        dqs_output_phase_align.use_delayed_clock = "true",
        dqs_output_phase_align.add_phase_transfer_reg = "dynamic",
        dqs_output_phase_align.add_output_cycle_delay = "dynamic",
        dqs_output_phase_align.invert_phase = "dynamic",
        dqs_output_phase_align.bypass_input_register = "false";

	

	stratixiii_ddio_out   dqs_0_output_hr_ddio_out_high_inst
	( 
	.clkhi(dqs_hr_output_reg_clk),
	.clklo(dqs_hr_output_reg_clk),
	.datainhi(dqs_hr_output_data_in[3]),
	.datainlo(dqs_hr_output_data_in[2]),
	.dataout(wire_dqs_0_output_hr_ddio_out_high_inst_dataout),
	.muxsel(dqs_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqs_0_output_hr_ddio_out_high_inst.async_mode = "none",
		dqs_0_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		dqs_0_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		dqs_0_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   dqs_0_output_hr_ddio_out_low_inst
	( 
	.clkhi(dqs_hr_output_reg_clk),
	.clklo(dqs_hr_output_reg_clk),
	.datainhi(dqs_hr_output_data_in[1]),
	.datainlo(dqs_hr_output_data_in[0]),
	.dataout(wire_dqs_0_output_hr_ddio_out_low_inst_dataout),
	.muxsel(dqs_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqs_0_output_hr_ddio_out_low_inst.async_mode = "none",
		dqs_0_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		dqs_0_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		dqs_0_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   dqsn_0_oct_hr_ddio_out_inst
	( 
	.clkhi(hr_oct_reg_clk),
	.clklo(hr_oct_reg_clk),
	.datainhi(hr_oct_in[1]),
	.datainlo(hr_oct_in[0]),
	.dataout(wire_dqsn_0_oct_hr_ddio_out_inst_dataout),
	.muxsel(hr_oct_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqsn_0_oct_hr_ddio_out_inst.half_rate_mode = "true",
		dqsn_0_oct_hr_ddio_out_inst.use_new_clocking_model = "true",
		dqsn_0_oct_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   dqsn_0_oe_hr_ddio_out_inst
	( 
	.clkhi(dqs_hr_output_reg_clk),
	.clklo(dqs_hr_output_reg_clk),
	.datainhi((~ dqsn_hr_oe_in[1])),
	.datainlo((~ dqsn_hr_oe_in[0])),
	.dataout(wire_dqsn_0_oe_hr_ddio_out_inst_dataout),
	.muxsel(dqs_hr_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
	.ena(1'b1),
	.sreset(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1),
	.dffhi(),
	.dfflo()
	// synopsys translate_on
	);
	defparam
		dqsn_0_oe_hr_ddio_out_inst.async_mode = "none",
		dqsn_0_oe_hr_ddio_out_inst.half_rate_mode = "true",
		dqsn_0_oe_hr_ddio_out_inst.use_new_clocking_model = "true",
		dqsn_0_oe_hr_ddio_out_inst.lpm_type = "stratixiii_ddio_out";

    stratixiii_output_phase_alignment   dqsn_oe_output_phase_align
    (
    .datain             ({1'b0,wire_dqsn_0_oe_hr_ddio_out_inst_dataout}),
    .clk                (dqs_output_reg_clk),
    .delayctrlin        (dll_delayctrlin),
    .phasectrlin        (dqsoutputphasesetting0),
	.phaseinvertctrl (dqsoutputphaseinvert0),
	.enaoutputcycledelay (enaoctcycledelaysetting0),
	.enaphasetransferreg (enaoctphasetransferreg0),
    .clkena             (dqs_output_reg_clkena),
    .dataout            (wire_dqsn_oe_0_output_ddio_out_inst_dataout)
    );
    defparam
        dqsn_oe_output_phase_align.operation_mode = "oe",
        dqsn_oe_output_phase_align.use_phasectrlin = "true",
        dqsn_oe_output_phase_align.delay_buffer_mode = "high",
        dqsn_oe_output_phase_align.power_up = "low",
        dqsn_oe_output_phase_align.async_mode = "none",
        dqsn_oe_output_phase_align.sync_mode = "none",
        dqsn_oe_output_phase_align.use_phasectrl_clock = "true",
        dqsn_oe_output_phase_align.use_primary_clock = "true",
        dqsn_oe_output_phase_align.use_delayed_clock = "true",
        dqsn_oe_output_phase_align.add_phase_transfer_reg = "dynamic",
        dqsn_oe_output_phase_align.add_output_cycle_delay = "dynamic",
        dqsn_oe_output_phase_align.invert_phase = "dynamic",
        dqsn_oe_output_phase_align.bypass_input_register = "false";




	stratixiii_delay_chain   bidir_dq_0_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[0]),
	.dataout(wire_bidir_dq_0_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_0_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_0_oct_ff_inst),
	.dataout(wire_bidir_dq_0_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_0_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_0_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_0_oe_ff_inst),
	.dataout(wire_bidir_dq_0_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_0_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_0_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_0_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_0_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_0_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_0_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_0_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_0_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_0_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_0_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_0_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[1]),
	.dataout(wire_bidir_dq_1_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_1_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_1_oct_ff_inst),
	.dataout(wire_bidir_dq_1_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_1_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_1_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_1_oe_ff_inst),
	.dataout(wire_bidir_dq_1_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_1_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_1_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_1_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_1_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_1_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_1_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_1_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_1_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_1_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_1_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_1_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[2]),
	.dataout(wire_bidir_dq_2_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_2_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_2_oct_ff_inst),
	.dataout(wire_bidir_dq_2_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_2_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_2_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_2_oe_ff_inst),
	.dataout(wire_bidir_dq_2_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_2_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_2_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_2_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_2_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_2_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_2_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_2_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_2_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_2_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_2_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_2_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[3]),
	.dataout(wire_bidir_dq_3_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_3_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_3_oct_ff_inst),
	.dataout(wire_bidir_dq_3_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_3_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_3_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_3_oe_ff_inst),
	.dataout(wire_bidir_dq_3_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_3_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_3_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_3_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_3_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_3_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_3_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_3_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_3_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_3_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_3_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_3_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[4]),
	.dataout(wire_bidir_dq_4_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_4_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_4_oct_ff_inst),
	.dataout(wire_bidir_dq_4_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_1_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_4_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_4_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_1_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_4_oe_ff_inst),
	.dataout(wire_bidir_dq_4_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_4_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_4_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_4_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_4_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_4_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_4_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_4_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_4_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_4_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_4_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_4_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[5]),
	.dataout(wire_bidir_dq_5_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_5_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_5_oct_ff_inst),
	.dataout(wire_bidir_dq_5_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_1_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_5_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_5_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_1_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_5_oe_ff_inst),
	.dataout(wire_bidir_dq_5_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_5_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_5_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_5_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_5_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_5_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_5_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_5_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_5_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_5_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_5_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_5_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[6]),
	.dataout(wire_bidir_dq_6_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_6_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_6_oct_ff_inst),
	.dataout(wire_bidir_dq_6_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_1_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_6_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_6_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_1_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_6_oe_ff_inst),
	.dataout(wire_bidir_dq_6_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_6_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_6_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_6_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_6_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_6_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_6_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_6_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_6_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_6_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_6_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_6_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_input_delay_chain_inst
	( 
	.datain(bidir_dq_input_data_in[7]),
	.dataout(wire_bidir_dq_7_input_delay_chain_inst_dataout),
	.delayctrlin(wire_bidir_dq_7_io_config_inst_padtoinputregisterdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_oct_delay_chain1_inst
	( 
	.datain(bidir_dq_7_oct_ff_inst),
	.dataout(wire_bidir_dq_7_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_1_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_oct_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_7_oct_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_7_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_1_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_oe_delay_chain1_inst
	( 
	.datain(bidir_dq_7_oe_ff_inst),
	.dataout(wire_bidir_dq_7_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_7_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_oe_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_7_oe_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_7_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_7_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_output_delay_chain1_inst
	( 
	.datain(wire_bidir_dq_7_output_ddio_out_inst_dataout),
	.dataout(wire_bidir_dq_7_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_bidir_dq_7_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   bidir_dq_7_output_delay_chain2_inst
	( 
	.datain(wire_bidir_dq_7_output_delay_chain1_inst_dataout),
	.dataout(wire_bidir_dq_7_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_bidir_dq_7_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_oct_delay_chain1_inst
	( 
	.datain(wire_dqs_oct_0_output_ddio_out_inst_dataout),
	.dataout(wire_dqs_0_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_oct_delay_chain2_inst
	( 
	.datain(wire_dqs_0_oct_delay_chain1_inst_dataout),
	.dataout(wire_dqs_0_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_oe_delay_chain1_inst
	( 
	.datain(wire_dqs_oe_0_output_ddio_out_inst_dataout),
	.dataout(wire_dqs_0_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_0_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_oe_delay_chain2_inst
	( 
	.datain(wire_dqs_0_oe_delay_chain1_inst_dataout),
	.dataout(wire_dqs_0_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_0_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_output_delay_chain1_inst
	( 
	.datain(wire_dqs_0_output_ddio_out_inst_dataout),
	.dataout(wire_dqs_0_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_0_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqs_0_output_delay_chain2_inst
	( 
	.datain(wire_dqs_0_output_delay_chain1_inst_dataout),
	.dataout(wire_dqs_0_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_0_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqsbusout_delay_chain_inst
	( 
	.datain(wire_dqs_0_delay_chain_inst_dqsbusout),
	.dataout(wire_dqsbusout_delay_chain_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_dqsbusoutdelaysetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqsn_0_oct_delay_chain1_inst
	( 
	.datain(wire_dqsn_oct_0_output_ddio_out_inst_dataout),
	.dataout(wire_dqsn_0_oct_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqs_config_0_0_inst_octdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqsn_0_oct_delay_chain2_inst
	( 
	.datain(wire_dqsn_0_oct_delay_chain1_inst_dataout),
	.dataout(wire_dqsn_0_oct_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqs_config_0_0_inst_octdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqsn_0_oe_delay_chain1_inst
	( 
	.datain(wire_dqsn_oe_0_output_ddio_out_inst_dataout),
	.dataout(wire_dqsn_0_oe_delay_chain1_inst_dataout),
	.delayctrlin(wire_dqsn_0_io_config_inst_outputdelaysetting1)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_delay_chain   dqsn_0_oe_delay_chain2_inst
	( 
	.datain(wire_dqsn_0_oe_delay_chain1_inst_dataout),
	.dataout(wire_dqsn_0_oe_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_dqsn_0_io_config_inst_outputdelaysetting2})
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.finedelayctrlin(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_dqs_config   dqs_config_0_0_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dividerphasesetting(wire_dqs_config_0_0_inst_dividerphasesetting),
	.dqoutputphaseinvert(dqoutputphaseinvert0),
	.dqoutputphasesetting(dqoutputphasesetting0),
	.dqsbusoutdelaysetting(wire_dqs_config_0_0_inst_dqsbusoutdelaysetting),
	.dqsbusoutfinedelaysetting(),
	.dqsenablectrlphaseinvert(wire_dqs_config_0_0_inst_dqsenablectrlphaseinvert),
	.dqsenablectrlphasesetting(wire_dqs_config_0_0_inst_dqsenablectrlphasesetting),
	.dqsenabledelaysetting(dqsenabledelaysetting),
	.dqsenablefinedelaysetting(),
	.dqsinputphasesetting(dqsinputphasesetting),
	.dqsoutputphaseinvert(dqsoutputphaseinvert0),
	.dqsoutputphasesetting(dqsoutputphasesetting0),
	.ena(dqs_config_ena),
	.enadataoutbypass(),
	.enadqsenablephasetransferreg(wire_dqs_config_0_0_inst_enadqsenablephasetransferreg),
	.enainputcycledelaysetting(),
	.enainputphasetransferreg(),
	.enaoctcycledelaysetting(enaoctcycledelaysetting0),
	.enaoctphasetransferreg(enaoctphasetransferreg0),
	.enaoutputcycledelaysetting(enaoutputcycledelaysetting0),
	.enaoutputphasetransferreg(enaoutputphasetransferreg0),
	.octdelaysetting1(wire_dqs_config_0_0_inst_octdelaysetting1),
	.octdelaysetting2(wire_dqs_config_0_0_inst_octdelaysetting2),
	.resyncinputphaseinvert(),
	.resyncinputphasesetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_dqs_config   dqs_config_0_1_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dividerphasesetting(wire_dqs_config_0_1_inst_dividerphasesetting),
	.dqoutputphaseinvert(dqoutputphaseinvert1),
	.dqoutputphasesetting(dqoutputphasesetting1),
	.dqsbusoutdelaysetting(),
	.dqsbusoutfinedelaysetting(),
	.dqsenablectrlphaseinvert(),
	.dqsenablectrlphasesetting(),
	.dqsenabledelaysetting(),
	.dqsenablefinedelaysetting(),
	.dqsinputphasesetting(),
	.dqsoutputphaseinvert(dqsoutputphaseinvert1),
	.dqsoutputphasesetting(dqsoutputphasesetting1),
	.ena(dqs_config_ena),
	.enadataoutbypass(),
	.enadqsenablephasetransferreg(),
	.enainputcycledelaysetting(),
	.enainputphasetransferreg(),
	.enaoctcycledelaysetting(enaoctcycledelaysetting1),
	.enaoctphasetransferreg(enaoctphasetransferreg1),
	.enaoutputcycledelaysetting(enaoutputcycledelaysetting1),
	.enaoutputphasetransferreg(enaoutputphasetransferreg1),
	.octdelaysetting1(wire_dqs_config_0_1_inst_octdelaysetting1),
	.octdelaysetting2(wire_dqs_config_0_1_inst_octdelaysetting2),
	.resyncinputphaseinvert(),
	.resyncinputphasesetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_dqs_delay_chain   dqs_0_delay_chain_inst
	( 
	.delayctrlin(dll_delayctrlin),
	.dqsbusout(wire_dqs_0_delay_chain_inst_dqsbusout),
	.dqsin(dqs_input_data_in[0]),
	.phasectrlin(dqsinputphasesetting)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.dqsupdateen(1'b0),
	.offsetctrlin({6{1'b0}}),
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	defparam
		dqs_0_delay_chain_inst.delay_buffer_mode = "high",
		dqs_0_delay_chain_inst.dqs_ctrl_latches_enable = "false",
		dqs_0_delay_chain_inst.dqs_input_frequency = "300",
		dqs_0_delay_chain_inst.dqs_offsetctrl_enable = "false",
		dqs_0_delay_chain_inst.dqs_phase_shift = 9000,
		dqs_0_delay_chain_inst.phase_setting = 3,
		dqs_0_delay_chain_inst.use_phasectrlin = "true",
		dqs_0_delay_chain_inst.lpm_type = "stratixiii_dqs_delay_chain";
	stratixiii_dqs_enable   dqs_0_enable_inst
	( 
	.dqsbusout(wire_dqs_0_enable_inst_dqsbusout),
	.dqsenable(wire_dqs_0_enable_ctrl_inst_dqsenableout_delay),
	.dqsin(wire_dqsbusout_delay_chain_inst_dataout)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);

    stratixiii_delay_chain	dqs_0_enable_delay 
    (
    .datain(wire_dqs_0_enable_ctrl_inst_dqsenableout),
    .dataout(wire_dqs_0_enable_ctrl_inst_dqsenableout_delay),
    .delayctrlin({1'b0,dqsenabledelaysetting})
    `ifndef FORMAL_VERIFICATION
    // synopsys translate_off
    `endif
    ,
    .finedelayctrlin(1'b0)
    `ifndef FORMAL_VERIFICATION
    // synopsys translate_on
    `endif
    // synopsys translate_off
    ,
    .devclrn(1'b1),
    .devpor(1'b1)
    // synopsys translate_on
    );

	stratixiii_dqs_enable_ctrl   dqs_0_enable_ctrl_inst
	( 
	.clk(dqs_enable_ctrl_clk),
	.dqsenablein(dqs_enable_ctrl_in),
	.dqsenableout(wire_dqs_0_enable_ctrl_inst_dqsenableout),
	.delayctrlin(dll_delayctrlin),
	.phasectrlin(wire_dqs_config_0_0_inst_dqsenablectrlphasesetting),
	.phaseinvertctrl(wire_dqs_config_0_0_inst_dqsenablectrlphaseinvert),
	.enaphasetransferreg(wire_dqs_config_0_0_inst_enadqsenablephasetransferreg)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	defparam
		dqs_0_enable_ctrl_inst.add_phase_transfer_reg = "dynamic",
		dqs_0_enable_ctrl_inst.delay_dqs_enable_by_half_cycle = "true",
		dqs_0_enable_ctrl_inst.invert_phase = "dynamic",
		dqs_0_enable_ctrl_inst.level_dqs_enable = "true",
		dqs_0_enable_ctrl_inst.phase_setting = 0,
		dqs_0_enable_ctrl_inst.use_phasectrlin = "true",
		dqs_0_enable_ctrl_inst.lpm_type = "stratixiii_dqs_enable_ctrl";
	stratixiii_io_clock_divider   io_clock_divider_0_0_inst
	( 
	.clk(io_clock_divider_clk),
	.clkout(),
	.phaseselect(wire_dqs_config_0_0_inst_dividerphasesetting),
	.slaveout(wire_io_clock_divider_0_0_inst_slaveout)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.delayctrlin({6{1'b0}}),
	.masterin(1'b0),
	.phasectrlin({4{1'b0}}),
	.phaseinvertctrl(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	defparam
		io_clock_divider_0_0_inst.invert_phase = "false",
		io_clock_divider_0_0_inst.phase_setting = 0,
		io_clock_divider_0_0_inst.use_masterin = "false",
		io_clock_divider_0_0_inst.use_phasectrlin = "false",
		io_clock_divider_0_0_inst.lpm_type = "stratixiii_io_clock_divider";
	stratixiii_io_clock_divider   io_clock_divider_0_1_inst
	( 
	.clk(io_clock_divider_clk),
	.clkout(),
	.masterin(wire_io_clock_divider_0_0_inst_slaveout),
	.phaseselect(wire_dqs_config_0_1_inst_dividerphasesetting),
	.slaveout()
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.delayctrlin({6{1'b0}}),
	.phasectrlin({4{1'b0}}),
	.phaseinvertctrl(1'b0)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_on
	`endif
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	defparam
		io_clock_divider_0_1_inst.invert_phase = "false",
		io_clock_divider_0_1_inst.phase_setting = 0,
		io_clock_divider_0_1_inst.use_masterin = "true",
		io_clock_divider_0_1_inst.use_phasectrlin = "false",
		io_clock_divider_0_1_inst.lpm_type = "stratixiii_io_clock_divider";
	stratixiii_io_config   bidir_dq_0_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[0]),
	.outputdelaysetting1(wire_bidir_dq_0_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_0_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_0_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_1_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[1]),
	.outputdelaysetting1(wire_bidir_dq_1_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_1_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_1_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_2_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[2]),
	.outputdelaysetting1(wire_bidir_dq_2_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_2_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_2_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_3_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[3]),
	.outputdelaysetting1(wire_bidir_dq_3_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_3_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_3_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_4_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[4]),
	.outputdelaysetting1(wire_bidir_dq_4_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_4_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_4_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_5_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[5]),
	.outputdelaysetting1(wire_bidir_dq_5_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_5_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_5_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_6_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[6]),
	.outputdelaysetting1(wire_bidir_dq_6_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_6_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_6_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   bidir_dq_7_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(bidir_dq_io_config_ena[7]),
	.outputdelaysetting1(wire_bidir_dq_7_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_bidir_dq_7_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(wire_bidir_dq_7_io_config_inst_padtoinputregisterdelaysetting),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   dqs_0_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(dqs_io_config_ena[0]),
	.outputdelaysetting1(wire_dqs_0_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_dqs_0_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_config   dqsn_0_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(dqsn_io_config_ena[0]),
	.outputdelaysetting1(wire_dqsn_0_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_dqsn_0_io_config_inst_outputdelaysetting2),
	.outputfinedelaysetting1(),
	.outputfinedelaysetting2(),
	.outputonlydelaysetting2(),
	.outputonlyfinedelaysetting2(),
	.padtoinputregisterdelaysetting(),
	.padtoinputregisterfinedelaysetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);

    stratixiii_io_config   output_dm_0_io_config_inst
    (
    .clk(config_clk),
    .datain(config_datain),
    .dataout(),
    .dutycycledelaymode(),
    .dutycycledelaysettings(),
    .ena(output_dm_io_config_ena),
    .outputdelaysetting1(wire_output_dm_0_io_config_inst_outputdelaysetting1),
    .outputdelaysetting2(wire_output_dm_0_io_config_inst_outputdelaysetting2),
    .outputfinedelaysetting1(),
    .outputfinedelaysetting2(),
    .outputonlydelaysetting2(),
    .outputonlyfinedelaysetting2(),
    .padtoinputregisterdelaysetting(),
    .padtoinputregisterfinedelaysetting(),
    .update(config_update)
	);

    stratixiii_ddio_out   output_dm_0_output_hr_ddio_out_high_inst
    (
    .clkhi(dq_hr_output_reg_clk),
    .clklo(dq_hr_output_reg_clk),
    .datainhi(output_dm_hr_output_data_in[3]),
    .datainlo(output_dm_hr_output_data_in[2]),
    .dataout(wire_output_dm_0_output_hr_ddio_out_high_inst_dataout),
    .muxsel(dq_hr_output_reg_clk)
	);
    defparam
        output_dm_0_output_hr_ddio_out_high_inst.async_mode = "none",
        output_dm_0_output_hr_ddio_out_high_inst.half_rate_mode = "true",
        output_dm_0_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
        output_dm_0_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";

    stratixiii_ddio_out   output_dm_0_output_hr_ddio_out_low_inst
    (
    .clkhi(dq_hr_output_reg_clk),
    .clklo(dq_hr_output_reg_clk),
    .datainhi(output_dm_hr_output_data_in[1]),
    .datainlo(output_dm_hr_output_data_in[0]),
    .dataout(wire_output_dm_0_output_hr_ddio_out_low_inst_dataout),
    .muxsel(dq_hr_output_reg_clk)
	);
    defparam
        output_dm_0_output_hr_ddio_out_low_inst.async_mode = "none",
        output_dm_0_output_hr_ddio_out_low_inst.half_rate_mode = "true",
        output_dm_0_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
        output_dm_0_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";



    stratixiii_output_phase_alignment   dm_0_output_phase_align
    (
    .datain         ({wire_output_dm_0_output_hr_ddio_out_high_inst_dataout,wire_output_dm_0_output_hr_ddio_out_low_inst_dataout}),
    .clk            (dq_output_reg_clk),
    .delayctrlin    (dll_delayctrlin),
    .phasectrlin    (dqoutputphasesetting1),
	.phaseinvertctrl (dqoutputphaseinvert1),
	.enaoutputcycledelay (enaoutputcycledelaysetting1),
	.enaphasetransferreg (enaoutputphasetransferreg1),
    .clkena         (dq_output_reg_clkena),
    .dataout        (wire_output_dm_0_output_ddio_out_inst_dataout)
    );
    defparam
        dm_0_output_phase_align.operation_mode = "ddio_out",
        dm_0_output_phase_align.use_phasectrlin = "true",
        dm_0_output_phase_align.delay_buffer_mode = "high",
        dm_0_output_phase_align.power_up = "low",
        dm_0_output_phase_align.async_mode = "none",
        dm_0_output_phase_align.sync_mode = "none",
        dm_0_output_phase_align.use_phasectrl_clock = "true",
        dm_0_output_phase_align.use_primary_clock = "true",
        dm_0_output_phase_align.use_delayed_clock = "true",
        dm_0_output_phase_align.add_phase_transfer_reg = "dynamic",
        dm_0_output_phase_align.add_output_cycle_delay = "dynamic",
        dm_0_output_phase_align.invert_phase = "dynamic",
        dm_0_output_phase_align.bypass_input_register = "false";


    stratixiii_delay_chain   output_dm_0_output_delay_chain1_inst
    (
    .datain(wire_output_dm_0_output_ddio_out_inst_dataout),
    .dataout(wire_output_dm_0_output_delay_chain1_inst_dataout),
    .delayctrlin(wire_output_dm_0_io_config_inst_outputdelaysetting1)
	);

    stratixiii_delay_chain   output_dm_0_output_delay_chain2_inst
    (
    .datain(wire_output_dm_0_output_delay_chain1_inst_dataout),
    .dataout(wire_output_dm_0_output_delay_chain2_inst_dataout),
    .delayctrlin({{1{1'b0}}, wire_output_dm_0_io_config_inst_outputdelaysetting2})
	);



	assign
		bidir_dq_input_data_out_high = {wire_bidir_dq_7_ddio_in_inst_regouthi, wire_bidir_dq_6_ddio_in_inst_regouthi, wire_bidir_dq_5_ddio_in_inst_regouthi, wire_bidir_dq_4_ddio_in_inst_regouthi, wire_bidir_dq_3_ddio_in_inst_regouthi, wire_bidir_dq_2_ddio_in_inst_regouthi, wire_bidir_dq_1_ddio_in_inst_regouthi, wire_bidir_dq_0_ddio_in_inst_regouthi},
		bidir_dq_input_data_out_low = {wire_bidir_dq_7_ddio_in_inst_regoutlo, wire_bidir_dq_6_ddio_in_inst_regoutlo, wire_bidir_dq_5_ddio_in_inst_regoutlo, wire_bidir_dq_4_ddio_in_inst_regoutlo, wire_bidir_dq_3_ddio_in_inst_regoutlo, wire_bidir_dq_2_ddio_in_inst_regoutlo, wire_bidir_dq_1_ddio_in_inst_regoutlo, wire_bidir_dq_0_ddio_in_inst_regoutlo},
		bidir_dq_oct_out = {wire_bidir_dq_7_oct_delay_chain2_inst_dataout, wire_bidir_dq_6_oct_delay_chain2_inst_dataout, wire_bidir_dq_5_oct_delay_chain2_inst_dataout, wire_bidir_dq_4_oct_delay_chain2_inst_dataout, wire_bidir_dq_3_oct_delay_chain2_inst_dataout, wire_bidir_dq_2_oct_delay_chain2_inst_dataout, wire_bidir_dq_1_oct_delay_chain2_inst_dataout, wire_bidir_dq_0_oct_delay_chain2_inst_dataout},
		bidir_dq_oe_out = {(~ wire_bidir_dq_7_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_6_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_5_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_4_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_3_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_2_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_1_oe_delay_chain2_inst_dataout), (~ wire_bidir_dq_0_oe_delay_chain2_inst_dataout)},
		bidir_dq_output_data_out = {wire_bidir_dq_7_output_delay_chain2_inst_dataout, wire_bidir_dq_6_output_delay_chain2_inst_dataout, wire_bidir_dq_5_output_delay_chain2_inst_dataout, wire_bidir_dq_4_output_delay_chain2_inst_dataout, wire_bidir_dq_3_output_delay_chain2_inst_dataout, wire_bidir_dq_2_output_delay_chain2_inst_dataout, wire_bidir_dq_1_output_delay_chain2_inst_dataout, wire_bidir_dq_0_output_delay_chain2_inst_dataout},
		dqs_bus_out = {wire_dqs_0_enable_inst_dqsbusout},
		dqs_bus_wire = {wire_dqs_0_enable_inst_dqsbusout},
		dqs_oct_out = wire_dqs_0_oct_delay_chain2_inst_dataout,
		dqs_oe_out = {(~ wire_dqs_0_oe_delay_chain2_inst_dataout)},
		dqs_output_data_out = {wire_dqs_0_output_delay_chain2_inst_dataout},
		dqsn_oct_out = wire_dqsn_0_oct_delay_chain2_inst_dataout,
		dqsn_oe_out = {(~ wire_dqsn_0_oe_delay_chain2_inst_dataout)},
        output_dm_output_data_out = {wire_output_dm_0_output_delay_chain2_inst_dataout},
		io_clock_divider_clk = 1'b0;




endmodule //bidir_dq_dqs_hr
