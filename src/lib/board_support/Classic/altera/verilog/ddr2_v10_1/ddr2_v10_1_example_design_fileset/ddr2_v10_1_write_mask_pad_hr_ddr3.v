// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis VERILOG_INPUT_VERSION VERILOG_2001






//synthesis_resources = stratixiii_ddio_out 3 stratixiii_delay_chain 2 stratixiii_dqs_config 1 stratixiii_io_clock_divider 1 stratixiii_io_config 1 
`timescale 1 ps / 1 ps
module  ddr2_v10_1_write_mask_pad_hr_ddr3
	(
	config_clk,
	config_datain,
	config_update,
	output_dq_io_config_ena, 
	dq_hr_output_reg_clk,
	dq_output_reg_clk,
	dq_output_reg_clkena,
	output_dq_hr_output_data_in,
	output_dq_output_data_out) /* synthesis synthesis_clearbox=1 */;


	input	 config_clk;
	input	 config_datain;
	input	 config_update;
	input	 [0:0]  output_dq_io_config_ena;
	input   dq_hr_output_reg_clk;
	input   dq_output_reg_clk;
	input   dq_output_reg_clkena;
	input   [3:0]  output_dq_hr_output_data_in;
	output   [0:0]  output_dq_output_data_out;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_off
`endif
	tri0   dq_hr_output_reg_clk;
	tri0   dq_output_reg_clk;
	tri1   dq_output_reg_clkena;
	tri0   [3:0]  output_dq_hr_output_data_in;
`ifndef ALTERA_RESERVED_QIS
// synopsys translate_on
`endif

	wire  wire_output_dq_0_output_ddio_out_inst_dataout;
	wire  wire_output_dq_0_output_hr_ddio_out_high_inst_dataout;
	wire  wire_output_dq_0_output_hr_ddio_out_low_inst_dataout;
	wire  wire_output_dq_0_output_delay_chain1_inst_dataout;
	wire  wire_output_dq_0_output_delay_chain2_inst_dataout;
	wire  wire_dqs_config_0_0_inst_dividerphasesetting;
	wire  [3:0]   wire_output_dq_0_io_config_inst_outputdelaysetting1;
	wire  [2:0]   wire_output_dq_0_io_config_inst_outputdelaysetting2;
	wire dqs_config_ena;
	wire io_clock_divider_clk;

	stratixiii_ddio_out   output_dq_0_output_ddio_out_inst
	( 
	.clkhi(dq_output_reg_clk),
	.clklo(dq_output_reg_clk),
	.datainhi(wire_output_dq_0_output_hr_ddio_out_high_inst_dataout),
	.datainlo(wire_output_dq_0_output_hr_ddio_out_low_inst_dataout),
	.dataout(wire_output_dq_0_output_ddio_out_inst_dataout),
	.ena(dq_output_reg_clkena),
	.muxsel(dq_output_reg_clk)
	`ifndef FORMAL_VERIFICATION
	// synopsys translate_off
	`endif
	,
	.areset(1'b0),
	.clk(1'b0),
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
		output_dq_0_output_ddio_out_inst.async_mode = "none",
		output_dq_0_output_ddio_out_inst.half_rate_mode = "false",
		output_dq_0_output_ddio_out_inst.sync_mode = "none",
		output_dq_0_output_ddio_out_inst.use_new_clocking_model = "true",
		output_dq_0_output_ddio_out_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   output_dq_0_output_hr_ddio_out_high_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(output_dq_hr_output_data_in[3]),
	.datainlo(output_dq_hr_output_data_in[2]),
	.dataout(wire_output_dq_0_output_hr_ddio_out_high_inst_dataout),
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
		output_dq_0_output_hr_ddio_out_high_inst.async_mode = "none",
		output_dq_0_output_hr_ddio_out_high_inst.half_rate_mode = "true",
		output_dq_0_output_hr_ddio_out_high_inst.use_new_clocking_model = "true",
		output_dq_0_output_hr_ddio_out_high_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_ddio_out   output_dq_0_output_hr_ddio_out_low_inst
	( 
	.clkhi(dq_hr_output_reg_clk),
	.clklo(dq_hr_output_reg_clk),
	.datainhi(output_dq_hr_output_data_in[1]),
	.datainlo(output_dq_hr_output_data_in[0]),
	.dataout(wire_output_dq_0_output_hr_ddio_out_low_inst_dataout),
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
		output_dq_0_output_hr_ddio_out_low_inst.async_mode = "none",
		output_dq_0_output_hr_ddio_out_low_inst.half_rate_mode = "true",
		output_dq_0_output_hr_ddio_out_low_inst.use_new_clocking_model = "true",
		output_dq_0_output_hr_ddio_out_low_inst.lpm_type = "stratixiii_ddio_out";
	stratixiii_delay_chain   output_dq_0_output_delay_chain1_inst
	( 
	.datain(wire_output_dq_0_output_ddio_out_inst_dataout),
	.dataout(wire_output_dq_0_output_delay_chain1_inst_dataout),
	.delayctrlin(wire_output_dq_0_io_config_inst_outputdelaysetting1)
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
	stratixiii_delay_chain   output_dq_0_output_delay_chain2_inst
	( 
	.datain(wire_output_dq_0_output_delay_chain1_inst_dataout),
	.dataout(wire_output_dq_0_output_delay_chain2_inst_dataout),
	.delayctrlin({{1{1'b0}}, wire_output_dq_0_io_config_inst_outputdelaysetting2})
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
	.dqoutputphaseinvert(),
	.dqoutputphasesetting(),
	.dqsbusoutdelaysetting(),
	.dqsbusoutfinedelaysetting(),
	.dqsenablectrlphaseinvert(),
	.dqsenablectrlphasesetting(),
	.dqsenabledelaysetting(),
	.dqsenablefinedelaysetting(),
	.dqsinputphasesetting(),
	.dqsoutputphaseinvert(),
	.dqsoutputphasesetting(),
	.ena(dqs_config_ena),
	.enadataoutbypass(),
	.enadqsenablephasetransferreg(),
	.enainputcycledelaysetting(),
	.enainputphasetransferreg(),
	.enaoctcycledelaysetting(),
	.enaoctphasetransferreg(),
	.enaoutputcycledelaysetting(),
	.enaoutputphasetransferreg(),
	.octdelaysetting1(),
	.octdelaysetting2(),
	.resyncinputphaseinvert(),
	.resyncinputphasesetting(),
	.update(config_update)
	// synopsys translate_off
	,
	.devclrn(1'b1),
	.devpor(1'b1)
	// synopsys translate_on
	);
	stratixiii_io_clock_divider   io_clock_divider_0_0_inst
	( 
	.clk(io_clock_divider_clk),
	.clkout(),
	.phaseselect(wire_dqs_config_0_0_inst_dividerphasesetting),
	.slaveout()
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
	stratixiii_io_config   output_dq_0_io_config_inst
	( 
	.clk(config_clk),
	.datain(config_datain),
	.dataout(),
	.dutycycledelaymode(),
	.dutycycledelaysettings(),
	.ena(output_dq_io_config_ena[0]),
	.outputdelaysetting1(wire_output_dq_0_io_config_inst_outputdelaysetting1),
	.outputdelaysetting2(wire_output_dq_0_io_config_inst_outputdelaysetting2),
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
	assign
		dqs_config_ena = 1'b1,
		io_clock_divider_clk = 1'b0,
		output_dq_output_data_out = {wire_output_dq_0_output_delay_chain2_inst_dataout};
endmodule //write_mask_pad_hr
