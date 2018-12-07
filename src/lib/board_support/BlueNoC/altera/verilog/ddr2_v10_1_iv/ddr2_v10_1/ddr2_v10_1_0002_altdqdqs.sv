// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_0002_altdqdqs (
	dll_delayctrl_in,

	capture_strobe_ena,
	capture_strobe_out,
	
	output_strobe_in,
	output_strobe_ena,
	oct_ena_in,
	
	strobe_io,
	strobe_n_io,
	
	fr_clock_in,
	hr_clock_in,
	strobe_ena_clock_in,
	write_strobe_clock_in,
	parallelterminationcontrol_in,
	seriesterminationcontrol_in,

	read_write_data_io,
		
	write_oe_in,
	read_data_out,
	write_data_in,
	extra_write_data_in,
	extra_write_data_out,

	config_data_in,
	config_dqs_ena,
	config_io_ena,
	config_extra_io_ena,
	config_dqs_io_ena,
	config_update,
	config_clock_in

);

parameter PIN_WIDTH = 9;
parameter PIN_TYPE = "bidir";

parameter USE_INPUT_PHASE_ALIGNMENT = "false";
parameter USE_OUTPUT_PHASE_ALIGNMENT = "false";
parameter USE_HALF_RATE_INPUT = "false";
parameter USE_HALF_RATE_OUTPUT = "true";
parameter USE_HALF_RATE_CAPTURE_STROBE_ENA = "false";

parameter DIFFERENTIAL_CAPTURE_STROBE = "false";
parameter SEPARATE_CAPTURE_STROBE = "false";

parameter INPUT_FREQ = 300;
parameter DELAY_CHAIN_BUFFER_MODE = "high";
parameter DQS_PHASE_SETTING = 3;
parameter DQS_PHASE_SHIFT = 9000;
parameter DQS_ENABLE_PHASE_SETTING = 0;
parameter DELAYED_CLOCK_PHASE_SETTING = 2;
parameter USE_DYNAMIC_CONFIG = "false";
parameter INVERT_CAPTURE_STROBE = "false";
parameter USE_TERMINATION_CONTROL = "true";
parameter USE_OCT_ENA_IN_FOR_OCT = "false";
parameter USE_DQS_ENABLE = "false";
parameter USE_IO_CONFIG = "false";
parameter USE_DQS_CONFIG = "false";

parameter USE_OUTPUT_STROBE = "true";
parameter DIFFERENTIAL_OUTPUT_STROBE = "false";
parameter USE_BIDIR_STROBE = "false";
parameter REVERSE_READ_WORDS = "false";

parameter EXTRA_OUTPUT_WIDTH = 0;
parameter PREAMBLE_TYPE = "none";

localparam RATE_MULT_IN = (USE_HALF_RATE_INPUT == "true") ? 4 : 2;
localparam RATE_MULT_OUT = (USE_HALF_RATE_OUTPUT == "true") ? 4 : 2;

localparam fpga_width_in = PIN_WIDTH * RATE_MULT_IN;
localparam fpga_width_out = PIN_WIDTH * RATE_MULT_OUT;
localparam extra_fpga_width_out = EXTRA_OUTPUT_WIDTH * RATE_MULT_OUT;
parameter  DQS_ENABLE_PHASECTRL = USE_DYNAMIC_CONFIG; 

parameter DYNAMIC_MODE = "false";

parameter OCT_SERIES_TERM_CONTROL_WIDTH   = 14; 
parameter OCT_PARALLEL_TERM_CONTROL_WIDTH = 14; 

input [5:0] dll_delayctrl_in;
input fr_clock_in;
input hr_clock_in;
input strobe_ena_clock_in;
input write_strobe_clock_in;

inout [PIN_WIDTH-1:0] read_write_data_io;

input [1:0] capture_strobe_ena;

input output_strobe_in;
input [1:0] output_strobe_ena;
input [1:0] oct_ena_in;

inout strobe_io;
inout strobe_n_io;

output [fpga_width_in-1:0] read_data_out;
input [fpga_width_out-1:0] write_data_in;

input [2*PIN_WIDTH-1:0] write_oe_in;
output capture_strobe_out;

input [extra_fpga_width_out-1:0] extra_write_data_in;
output [EXTRA_OUTPUT_WIDTH-1:0] extra_write_data_out;

input	[OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] parallelterminationcontrol_in;
input	[OCT_SERIES_TERM_CONTROL_WIDTH-1:0] seriesterminationcontrol_in;

input config_data_in;
input config_update;
input config_dqs_ena;
input [PIN_WIDTH-1:0] config_io_ena;
input [EXTRA_OUTPUT_WIDTH-1:0] config_extra_io_ena;
input config_dqs_io_ena;
input config_clock_in;

wire [5:0] dll_delay_value;
assign dll_delay_value = dll_delayctrl_in;
/*
stratixiv_dll the_dll (
	.clk (fr_clock_in),
	.delayctrlout (dll_delay_value)
);
defparam
	the_dll.delay_buffer_mode="high",
	the_dll.input_frequency = "2857 ps",
	the_dll.delay_chain_length = 10,
	the_dll.jitter_reduction = "true",
	the_dll.static_delay_ctrl = 10,
	the_dll.lpm_type = "stratixiv_dll";
*/

wire dqsbusout;
wire dqsnbusout;

wire [3:0] dqs_outputdelaysetting1;
wire [3:0] dqs_outputdelaysetting2;
wire [3:0] dqs_inputdelaysetting;

wire [3:0] dqsn_outputdelaysetting1;
wire [3:0] dqsn_outputdelaysetting2;
wire [3:0] dqsn_inputdelaysetting;

generate
if (USE_DYNAMIC_CONFIG =="true" && (USE_OUTPUT_STROBE == "true" || PIN_TYPE =="input" || PIN_TYPE == "bidir"))
begin
	assign dqs_outputdelaysetting2 [3] = 1'b0;
	assign dqsn_outputdelaysetting2 [3] = 1'b0;

	stratixiv_io_config dqs_io_config_1 (
		    .datain(config_data_in),          // shared per DQS group
			.clk(config_clock_in),
			.ena(config_dqs_io_ena),
			.update(config_update),       // shared per DQS group

			.outputdelaysetting1(dqs_outputdelaysetting1),
			.outputdelaysetting2(dqs_outputdelaysetting2[2:0]),
			.padtoinputregisterdelaysetting(dqs_inputdelaysetting),

			.outputfinedelaysetting1(),
			.outputfinedelaysetting2(),
			.padtoinputregisterfinedelaysetting(),

			.outputonlyfinedelaysetting2(),
			.outputonlydelaysetting2(),

			.dutycycledelaymode(),
			.dutycycledelaysettings(),

			.dataout()
		);

	stratixiv_io_config dqsn_io_config_1 (
		    .datain(config_data_in),          // shared per DQS group
			.clk(config_clock_in),
			.ena(config_dqs_io_ena),
			.update(config_update),       // shared per DQS group

			.outputdelaysetting1(dqsn_outputdelaysetting1),
			.outputdelaysetting2(dqsn_outputdelaysetting2[2:0]),
			.padtoinputregisterdelaysetting(dqsn_inputdelaysetting),

			.outputfinedelaysetting1(),
			.outputfinedelaysetting2(),
			.padtoinputregisterfinedelaysetting(),

			.outputonlyfinedelaysetting2(),
			.outputonlydelaysetting2(),

			.dutycycledelaymode(),
			.dutycycledelaysettings(),

			.dataout()
		);



end
endgenerate

wire [1:0] oct_ena;
wire fr_term;
wire aligned_term;

generate
	if (USE_HALF_RATE_OUTPUT == "true")
	begin : oct_ena_hr_gen
		if (USE_OCT_ENA_IN_FOR_OCT == "true")
		begin
			assign oct_ena = oct_ena_in;
		end
		else
		begin
			reg oct_ena_hr_reg;
			always @(posedge hr_clock_in)
				oct_ena_hr_reg <= output_strobe_ena[1];
			assign oct_ena[1] = ~output_strobe_ena[1];
			assign oct_ena[0] = ~(oct_ena_hr_reg | output_strobe_ena[1]);
		end
	end
	else
	begin : oct_ena_fr_gen
		if (USE_OCT_ENA_IN_FOR_OCT == "true")
		begin
			assign fr_term = oct_ena_in[0];
		end
		else
		begin
			reg oct_ena_fr_reg;
	        initial
	    		oct_ena_fr_reg = 0;
			always @(posedge hr_clock_in)
				oct_ena_fr_reg <= output_strobe_ena[0];
			assign fr_term = ~(output_strobe_ena[0] | oct_ena_fr_reg);
		end
	end
endgenerate


localparam PINS_PER_DQS_CONFIG = 6;
localparam DQS_CONFIGS = (PIN_WIDTH + EXTRA_OUTPUT_WIDTH + 2) / PINS_PER_DQS_CONFIG;
localparam DQ_BREAKPOINT = 4;

wire dividerphasesetting [DQS_CONFIGS:0];
wire dqoutputphaseinvert [DQS_CONFIGS:0];
wire [3:0] dqoutputphasesetting [DQS_CONFIGS:0];
wire [3:0] dqsbusoutdelaysetting [DQS_CONFIGS:0];
wire dqsbusoutfinedelaysetting [DQS_CONFIGS:0];
wire dqsenablectrlphaseinvert [DQS_CONFIGS:0];
wire [3:0] dqsenablectrlphasesetting [DQS_CONFIGS:0];
wire [2:0] dqsenabledelaysetting [DQS_CONFIGS:0];
wire dqsenablefinedelaysetting [DQS_CONFIGS:0];
wire [2:0] dqsinputphasesetting [DQS_CONFIGS:0];
wire dqsoutputphaseinvert [DQS_CONFIGS:0];
wire [3:0] dqsoutputphasesetting [DQS_CONFIGS:0];
wire enadataoutbypass [DQS_CONFIGS:0];
wire enadqsenablephasetransferreg [DQS_CONFIGS:0];
wire enainputcycledelaysetting [DQS_CONFIGS:0];
wire enainputphasetransferreg [DQS_CONFIGS:0];
wire enaoctcycledelaysetting [DQS_CONFIGS:0];
wire enaoctphasetransferreg [DQS_CONFIGS:0];
wire enaoutputcycledelaysetting [DQS_CONFIGS:0];
wire enaoutputphasetransferreg [DQS_CONFIGS:0];
wire [3:0] octdelaysetting1 [DQS_CONFIGS:0];
wire [2:0] octdelaysetting2 [DQS_CONFIGS:0];
wire resyncinputphaseinvert [DQS_CONFIGS:0];
wire [3:0] resyncinputphasesetting [DQS_CONFIGS:0];


generate
if (USE_DYNAMIC_CONFIG == "true")
begin
	genvar c_num; 
	for (c_num = 0; c_num <= DQS_CONFIGS; c_num = c_num + 1)
	begin :dqs_config_gen
			
		stratixiv_dqs_config   dqs_config_inst
		( 
		.clk(config_clock_in),
		.datain(config_data_in),
		.dataout(),
		.ena(config_dqs_ena),
		.update(config_update),

		.dividerphasesetting(dividerphasesetting[c_num]), //unused 1
		.dqoutputphaseinvert(dqoutputphaseinvert[c_num]), //done 1
		.dqoutputphasesetting(dqoutputphasesetting[c_num]), //done 1
 		.dqsbusoutdelaysetting(dqsbusoutdelaysetting[c_num]), //done 1
		.dqsbusoutfinedelaysetting(dqsbusoutfinedelaysetting[c_num]), //unused 0
		.dqsenablectrlphaseinvert(dqsenablectrlphaseinvert[c_num]), //unused 0
		.dqsenablectrlphasesetting(dqsenablectrlphasesetting[c_num]), //done 1
		.dqsenabledelaysetting(dqsenabledelaysetting[c_num]), //done 1
		.dqsenablefinedelaysetting(dqsenablefinedelaysetting[c_num]), //unused 0
		.dqsinputphasesetting(dqsinputphasesetting[c_num]), //done 1
		.dqsoutputphaseinvert(dqsoutputphaseinvert[c_num]), //done 1
		.dqsoutputphasesetting(dqsoutputphasesetting[c_num]), //done 1
		.enadataoutbypass(enadataoutbypass[c_num]), //unused 0
		.enadqsenablephasetransferreg(enadqsenablephasetransferreg[c_num]), //unused 0
		.enainputcycledelaysetting(enainputcycledelaysetting[c_num]), //unused 0
		.enainputphasetransferreg(enainputphasetransferreg[c_num]), //unused 0
		.enaoctcycledelaysetting(enaoctcycledelaysetting[c_num]), //done 1
		.enaoctphasetransferreg(enaoctphasetransferreg[c_num]),//done 1
		.enaoutputcycledelaysetting(enaoutputcycledelaysetting[c_num]),//done 1
		.enaoutputphasetransferreg(enaoutputphasetransferreg[c_num]), //done 1
		.octdelaysetting1(octdelaysetting1[c_num]),//done 1
		.octdelaysetting2(octdelaysetting2[c_num]),//done 1
		.resyncinputphaseinvert(resyncinputphaseinvert[c_num]), //unused 0
		.resyncinputphasesetting(resyncinputphasesetting[c_num]) //unused 0
		);

	end
end
endgenerate

generate
if (USE_BIDIR_STROBE != "true")
begin
	assign strobe_io = 1'b0;
	assign strobe_n_io = 1'b1;	
end
endgenerate

generate 
if (PIN_TYPE == "input" || PIN_TYPE == "bidir")
begin

	assign capture_strobe_out = dqsbusout;
	wire dqsin;
	
	if (DIFFERENTIAL_CAPTURE_STROBE == "true")
	begin
		stratixiv_io_ibuf strobe_in (
			.i(strobe_io),
			.ibar (strobe_n_io),
			.o(dqsin)
		);
			defparam	strobe_in.bus_hold = "false",	
					strobe_in.lpm_type = "stratixiv_io_ibuf";
	end
	else
	begin
		stratixiv_io_ibuf strobe_in (
			.i(strobe_io),
			.o(dqsin)
		);
			defparam strobe_in.bus_hold = "false";
	end

	wire dqsbusout_preena;
	wire dqsbusout_predelay;
	wire dqs_enable_predelay;
	wire dqs_enable;
	
	if (USE_DYNAMIC_CONFIG == "true")
	begin	

		stratixiv_dqs_delay_chain dqs_delay_chain(
			.dqsin(dqsin),
			.delayctrlin(dll_delay_value),
			.dqsbusout(dqsbusout_predelay),
			.phasectrlin(dqsinputphasesetting[0])
		);
			defparam dqs_delay_chain.dqs_input_frequency = INPUT_FREQ,
					dqs_delay_chain.delay_buffer_mode = DELAY_CHAIN_BUFFER_MODE,
					dqs_delay_chain.phase_setting = DQS_PHASE_SETTING,
					dqs_delay_chain.dqs_phase_shift = DQS_PHASE_SHIFT,
					dqs_delay_chain.dqs_ctrl_latches_enable = "false",
					dqs_delay_chain.dqs_offsetctrl_enable = "false",
					dqs_delay_chain.use_phasectrlin = "true";


		stratixiv_delay_chain dqs_in_delay_1(
			.datain             (dqsbusout_predelay),
			.delayctrlin        (dqsbusoutdelaysetting[0]),
			.dataout            (dqsbusout_preena)
		);

		stratixiv_delay_chain dqs_ena_delay_1(
			.datain             (dqs_enable_predelay),
			.delayctrlin        ({1'b0,dqsenabledelaysetting[0]}),
			.dataout            (dqs_enable)
		);

	end
	else
	begin
		stratixiv_dqs_delay_chain dqs_delay_chain(
			.dqsin(dqsin),
			.delayctrlin(dll_delay_value),
			.dqsbusout(dqsbusout_predelay),
			.dqsupdateen(1'b0)
		);
			defparam dqs_delay_chain.dqs_input_frequency = INPUT_FREQ,
					dqs_delay_chain.delay_buffer_mode = DELAY_CHAIN_BUFFER_MODE,
					dqs_delay_chain.phase_setting = DQS_PHASE_SETTING,
					dqs_delay_chain.dqs_phase_shift = DQS_PHASE_SHIFT,
					dqs_delay_chain.dqs_ctrl_latches_enable = "false",
					dqs_delay_chain.dqs_offsetctrl_enable = "false",
					dqs_delay_chain.use_phasectrlin = "false";
					
		assign dqsbusout_preena = dqsbusout_predelay;
		assign dqs_enable = dqs_enable_predelay;
	end

	if (USE_DQS_ENABLE == "true")
	begin
	
		wire fr_capture_strobe_ena;

		if (USE_HALF_RATE_CAPTURE_STROBE_ENA == "true")
		begin
			stratixiv_ddio_out hr_to_fr_capture_strobe_ena (
				.datainhi(capture_strobe_ena[1]),
				.datainlo(capture_strobe_ena[0]),
				.dataout(fr_capture_strobe_ena),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
			);
			defparam hr_to_fr_capture_strobe_ena.half_rate_mode = "true",
					hr_to_fr_capture_strobe_ena.use_new_clocking_model = "true",
					hr_to_fr_capture_strobe_ena.async_mode = "none";	
		end
		else
		begin
			assign fr_capture_strobe_ena = capture_strobe_ena[0];
		end

		stratixiv_dqs_enable_ctrl enable_ctrl (
			.dqsenablein(fr_capture_strobe_ena),
			.clk(strobe_ena_clock_in),
			.delayctrlin(dll_delay_value),
			.dqsenableout(dqs_enable_predelay),
			.phasectrlin(dqsenablectrlphasesetting[0]),
			.phaseinvertctrl(dqsenablectrlphaseinvert[0]),
			.enaphasetransferreg(enadqsenablephasetransferreg[0])
			);
			defparam enable_ctrl.delay_buffer_mode = "high", //"high" is the only valid input
				enable_ctrl.phase_setting = DQS_ENABLE_PHASE_SETTING,
				enable_ctrl.invert_phase = DYNAMIC_MODE,
				enable_ctrl.add_phase_transfer_reg = DYNAMIC_MODE,
				enable_ctrl.use_phasectrlin = DQS_ENABLE_PHASECTRL,
				enable_ctrl.delay_dqs_enable_by_half_cycle = "true",
				enable_ctrl.level_dqs_enable = "true";


		stratixiv_dqs_enable dqs_enable_block (
			.dqsin(dqsbusout_preena), // Need that for sim & dqs_enable),
			.dqsenable(dqs_enable),
			.dqsbusout(dqsbusout)
		);
	end
	else
	begin
		assign dqsbusout = dqsbusout_preena;
	end



	if (SEPARATE_CAPTURE_STROBE == "true")
	begin
		wire dqsnin;
		wire dqsnbusout_preena;
		wire dqsnbusout_predelay;
		wire dqsn_enable_predelay;
		wire dqsn_enable;

		stratixiv_io_ibuf stroben_in (
			.i(strobe_n_io),
			.o(dqsnin)
		);
		
		stratixiv_dqs_delay_chain dqsn_delay_chain(
			.dqsin(dqsnin),
			.delayctrlin(dll_delay_value),
			.dqsbusout(dqsnbusout_predelay)
		);
			defparam dqsn_delay_chain.dqs_input_frequency = INPUT_FREQ,
				dqsn_delay_chain.delay_buffer_mode = DELAY_CHAIN_BUFFER_MODE,
				dqsn_delay_chain.phase_setting = DQS_PHASE_SETTING,
				dqsn_delay_chain.dqs_phase_shift = DQS_PHASE_SHIFT,
				dqsn_delay_chain.dqs_ctrl_latches_enable = "false",
				dqsn_delay_chain.dqs_offsetctrl_enable = "false",
				dqsn_delay_chain.use_phasectrlin = USE_DYNAMIC_CONFIG;

		if (USE_DYNAMIC_CONFIG == "true")
		begin
			stratixiv_delay_chain dqs_delay_1(
				.datain             (dqsnbusout_predelay),
				.delayctrlin        (dqsbusoutdelaysetting[0]),
				.dataout            (dqsnbusout_preena)
			);

			stratixiv_delay_chain dqs_ena_delay_1(
				.datain             (dqsn_enable_predelay),
				.delayctrlin        (dqsenabledelaysetting[0]),
				.dataout            (dqsn_enable)
			);

		end
		else
		begin
			assign dqsbusout_preena = dqsbusout_predelay;
			assign dqs_enable = dqs_enable_predelay;
		end


		
		if (USE_DQS_ENABLE == "true")
		begin

			wire fr_capture_stroben_ena;

			if (USE_HALF_RATE_CAPTURE_STROBE_ENA == "true")
			begin
				stratixiv_ddio_out hr_to_fr_capture_stroben_ena (
					.datainhi(capture_strobe_ena[1]),
					.datainlo(capture_strobe_ena[0]),
					.dataout(fr_capture_stroben_ena),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
				);
				defparam hr_to_fr_capture_stroben_ena.half_rate_mode = "true",
						hr_to_fr_capture_stroben_ena.use_new_clocking_model = "true",
						hr_to_fr_capture_stroben_ena.async_mode = "none";	
			end
			else
			begin
				assign fr_capture_stroben_ena = capture_strobe_ena[0];
			end

			stratixiv_dqs_enable_ctrl enablen_ctrl (
				.dqsenablein(fr_capture_stroben_ena),
				.clk(strobe_ena_clock_in),
				.delayctrlin(dll_delay_value),
				.dqsenableout(dqsn_enable),
				.phasectrlin(dqsenablectrlphasesetting[0]),
				.phaseinvertctrl(dqsenablectrlphaseinvert[0]),
				.enaphasetransferreg(enadqsenablephasetransferreg[0])

			);
			defparam enablen_ctrl.delay_buffer_mode = "high", //"high" is the only valid input
                		enablen_ctrl.phase_setting = DQS_ENABLE_PHASE_SETTING,
				enablen_ctrl.invert_phase = DYNAMIC_MODE,
				enablen_ctrl.add_phase_transfer_reg = DYNAMIC_MODE,
				enablen_ctrl.use_phasectrlin = DQS_ENABLE_PHASECTRL,
				enablen_ctrl.delay_dqs_enable_by_half_cycle = "true",
				enablen_ctrl.level_dqs_enable = "true";


			stratixiv_dqs_enable dqs_enablen_block (
				.dqsin(dqsnbusout_preena),
				.dqsenable(dqsn_enable),
				.dqsbusout(dqsnbusout)
			);
		end
		else
		begin
			assign dqsnbusout = dqsnbusout_preena;
		end
	end
end
endgenerate

generate
if (USE_OUTPUT_STROBE == "true")
begin
	wire os;
	wire os_bar;
	wire os_delayed1;
	wire os_delayed2;

	wire fr_os_oe;
	wire fr_os_oct;
	wire aligned_os_oe;
	wire aligned_os_oct;
	wire aligned_strobe;
	
	wire fr_os_hi;
	wire fr_os_lo;
	
	if (USE_HALF_RATE_OUTPUT == "true")
	begin
		wire clk_gate;
		
		if (PREAMBLE_TYPE == "low")
		begin
			assign clk_gate = output_strobe_ena[0];
		end
		else
		begin
			if (PREAMBLE_TYPE == "high")
			begin
				assign clk_gate = output_strobe_ena [1];
			end
			else
			begin
				assign clk_gate = 1'b1;
			end
		end

		stratixiv_ddio_out hr_to_fr_os_hi (
				.datainhi(clk_gate),
				.datainlo(clk_gate),
				.dataout(fr_os_hi),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
		);
			defparam hr_to_fr_os_hi.half_rate_mode = "true",
					hr_to_fr_os_hi.use_new_clocking_model = "true",
					hr_to_fr_os_hi.async_mode = "none";	
	
		stratixiv_ddio_out hr_to_fr_os_lo (
				.datainhi(1'b0),
				.datainlo(1'b0),
				.dataout(fr_os_lo),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
		);
			defparam hr_to_fr_os_lo.half_rate_mode = "true",
					hr_to_fr_os_lo.use_new_clocking_model = "true",
					hr_to_fr_os_lo.async_mode = "none";	
	
	
		stratixiv_ddio_out hr_to_fr_os_oe (
				.datainhi(~output_strobe_ena [1]),
				.datainlo(~output_strobe_ena [0]),
				.dataout(fr_os_oe),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
		);
			defparam hr_to_fr_os_oe.half_rate_mode = "true",
					 hr_to_fr_os_oe.use_new_clocking_model = "true",
					 hr_to_fr_os_oe.async_mode = "none";	

		stratixiv_ddio_out hr_to_fr_os_oct (
				.datainhi(oct_ena[1]),
				.datainlo(oct_ena[0]),
				.dataout(fr_os_oct),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
		);
			defparam hr_to_fr_os_oct.half_rate_mode = "true",
					 hr_to_fr_os_oct.use_new_clocking_model = "true",
					 hr_to_fr_os_oct.async_mode = "none";	
	end
	else
	begin
		assign fr_os_oe = ~output_strobe_ena[0];
		assign fr_os_oct = fr_term;

		wire gnd_lut /* synthesis keep = 1*/;
		assign gnd_lut = 1'b0;
		assign fr_os_lo = gnd_lut;

		if (PREAMBLE_TYPE == "low")
		begin
			reg os_ena_reg1;
			initial
				os_ena_reg1 = 0;
			always @(posedge hr_clock_in)
				os_ena_reg1 <= output_strobe_ena[0];

			assign fr_os_hi = os_ena_reg1 & output_strobe_ena[0];
		end
		else
		begin
			if (PREAMBLE_TYPE == "high")
			begin
				assign fr_os_hi = output_strobe_ena[0];
			end
			else
			begin
				wire vcc_lut /* synthesis keep = 1*/;
				assign vcc_lut = 1'b1;
				assign fr_os_hi = vcc_lut;
			end
		end
	end

	if (USE_OUTPUT_PHASE_ALIGNMENT == "true")
	begin
		stratixiv_output_phase_alignment dqs_alignment(
			.datain ({fr_os_hi,fr_os_lo}),
			.clk(fr_clock_in),
			.clkena (1'b1),
			.delayctrlin(dll_delay_value),
			.phaseinvertctrl (dqsoutputphaseinvert[0]),
			.enaoutputcycledelay (enaoctcycledelaysetting[0]),
			.enaphasetransferreg (enaoctphasetransferreg[0]),
	
			.dataout(aligned_strobe),
			.phasectrlin(dqsoutputphasesetting[0])
		);
		defparam dqs_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
			dqs_alignment.use_phasectrl_clock = "true",
			dqs_alignment.use_delayed_clock = "true",
			dqs_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
			dqs_alignment.operation_mode = "ddio_out",
  			dqs_alignment.delay_buffer_mode = "high",
			dqs_alignment.power_up = "low",
			dqs_alignment.async_mode = "none",
			dqs_alignment.sync_mode = "none",
			dqs_alignment.use_primary_clock = "true",
	      		dqs_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
			dqs_alignment.add_output_cycle_delay = DYNAMIC_MODE,
			dqs_alignment.invert_phase = DYNAMIC_MODE,
			dqs_alignment.bypass_input_register = "false";

		stratixiv_output_phase_alignment dqs_oe_alignment(
			.datain({1'b0,fr_os_oe}),
			.clk(fr_clock_in),
			.clkena (1'b1),
			.delayctrlin(dll_delay_value),
			.phaseinvertctrl (dqsoutputphaseinvert[0]),
			.enaoutputcycledelay (enaoctcycledelaysetting[0]),
			.enaphasetransferreg (enaoctphasetransferreg[0]),
	
			.dataout(aligned_os_oe),
			.phasectrlin(dqsoutputphasesetting[0])
		);
		defparam dqs_oe_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
			dqs_oe_alignment.use_phasectrl_clock = "true",
			dqs_oe_alignment.use_delayed_clock = "true",
			dqs_oe_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
			dqs_oe_alignment.operation_mode = "oe",
  			dqs_oe_alignment.delay_buffer_mode = "high",
			dqs_oe_alignment.power_up = "low",
			dqs_oe_alignment.async_mode = "none",
			dqs_oe_alignment.sync_mode = "none",
			dqs_oe_alignment.use_primary_clock = "true",
	      	dqs_oe_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
			dqs_oe_alignment.add_output_cycle_delay = DYNAMIC_MODE,
			dqs_oe_alignment.invert_phase = DYNAMIC_MODE,
			dqs_oe_alignment.bypass_input_register = "false";

		stratixiv_output_phase_alignment	dqs_oct_alignment (
			.datain			({1'b0,fr_os_oct}),
			.clk(fr_clock_in),
			.clkena			(1'b1),
			.delayctrlin(dll_delay_value),
			.phasectrlin (dqsoutputphasesetting[0]),
			.phaseinvertctrl (dqsoutputphaseinvert[0]),
			.enaoutputcycledelay (enaoctcycledelaysetting[0]),
			.enaphasetransferreg (enaoctphasetransferreg[0]),
			.dataout		(aligned_os_oct )
		);
		defparam
			dqs_oct_alignment.operation_mode = "rtena",
			dqs_oct_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
			dqs_oct_alignment.delay_buffer_mode = "high",
			dqs_oct_alignment.power_up = "low",
			dqs_oct_alignment.async_mode = "none",
			dqs_oct_alignment.sync_mode = "none",
			dqs_oct_alignment.use_phasectrl_clock = "true",
			dqs_oct_alignment.use_primary_clock = "true",
			dqs_oct_alignment.use_delayed_clock = "true",
			dqs_oct_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
			dqs_oct_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
			dqs_oct_alignment.add_output_cycle_delay = DYNAMIC_MODE,
			dqs_oct_alignment.invert_phase = DYNAMIC_MODE,
			dqs_oct_alignment.bypass_input_register = "false";
	end
	else
	begin
        stratixiv_ddio_out phase_align_os (
                .datainhi(fr_os_hi),
                .datainlo(fr_os_lo),
                .dataout(aligned_strobe),
                .clkhi (write_strobe_clock_in),
                .clklo (write_strobe_clock_in),
                .muxsel (write_strobe_clock_in)
        );
            defparam phase_align_os.half_rate_mode = "false",
                    phase_align_os.use_new_clocking_model = "true",
                    phase_align_os.async_mode = "none";


		(* ALTERA_ATTRIBUTE = {"FAST_OUTPUT_ENABLE_REGISTER=ON"} *) reg oe_reg;
		(* ALTERA_ATTRIBUTE = {"FAST_OCT_REGISTER=ON"} *) reg oct_reg;
		initial 
		begin
			oe_reg = 0;
			oct_reg = 0;
		end

	        always @ ( posedge write_strobe_clock_in)
	       	    oe_reg <= fr_os_oe;

	        assign aligned_os_oe = oe_reg;

			always @ (posedge fr_clock_in)
				oct_reg <= fr_os_oct;

			assign aligned_os_oct = oct_reg;
	end
	
	wire delayed_os_oct;
	wire delayed_os_oe;
	
	if (USE_DYNAMIC_CONFIG == "true")
	begin
		wire delayed_os_oct_1;
		wire delayed_os_oe_1;
	
		stratixiv_delay_chain dqs_delay_1(
			.datain             (aligned_strobe),
			.delayctrlin        (dqs_outputdelaysetting1),
			.dataout            (os_delayed1)
		);

		stratixiv_delay_chain dqs_delay_2(
			.datain             (os_delayed1),
			.delayctrlin        (dqs_outputdelaysetting2),
			.dataout            (os_delayed2)
		);
		
		stratixiv_delay_chain oct_delay_1(
			.datain             (aligned_os_oct),
			.delayctrlin        (octdelaysetting1 [0]),
			.dataout            (delayed_os_oct_1)
		);

		stratixiv_delay_chain oct_delay_2(
			.datain             (delayed_os_oct_1),
			.delayctrlin        ({1'b0, octdelaysetting2 [0]}),
			.dataout            (delayed_os_oct)
		);
		
		stratixiv_delay_chain oe_delay_1(
			.datain             (aligned_os_oe),
			.delayctrlin        (dqs_outputdelaysetting1),
			.dataout            (delayed_os_oe_1)
		);

		stratixiv_delay_chain oe_delay_2(
			.datain             (delayed_os_oe_1),
			.delayctrlin        (dqs_outputdelaysetting2),
			.dataout            (delayed_os_oe)
		);
		
	end
	else
	begin
		assign os_delayed2 = aligned_strobe;
		assign delayed_os_oct = aligned_term;
		assign delayed_os_oe = aligned_os_oe;
	end

	if (DIFFERENTIAL_OUTPUT_STROBE=="true")
	begin
		wire aligned_os_oe_bar;
		wire aligned_os_oct_bar;
		
		wire delayed_os_bar_oct;
		wire delayed_os_bar_oct_1;

		wire delayed_os_bar_oe;
		wire delayed_os_bar_oe_1;

		wire fr_os_oen;
		wire fr_os_octn;
	
		if (USE_HALF_RATE_OUTPUT == "true")
		begin
			stratixiv_ddio_out hr_to_fr_os_oe_bar (
					.datainhi(~output_strobe_ena [1]),
					.datainlo(~output_strobe_ena [0]),
					.dataout(fr_os_oen),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
			);
				defparam hr_to_fr_os_oe_bar.half_rate_mode = "true",
						hr_to_fr_os_oe_bar.use_new_clocking_model = "true",
						hr_to_fr_os_oe_bar.async_mode = "none";	

			stratixiv_ddio_out hr_to_fr_os_oct_bar (
					.datainhi(oct_ena [1]),
					.datainlo(oct_ena [0]),
					.dataout(fr_os_octn),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
			);
				defparam hr_to_fr_os_oct_bar.half_rate_mode = "true",
						hr_to_fr_os_oct_bar.use_new_clocking_model = "true",
						hr_to_fr_os_oct_bar.async_mode = "none";	
		end
		else
		begin
			assign fr_os_oen = ~output_strobe_ena;
			assign fr_os_octn = fr_term;
		end
	
		if (USE_OUTPUT_PHASE_ALIGNMENT == "true")
		begin
		
			stratixiv_output_phase_alignment dqs_oe_bar_alignment(
				.datain({1'b0,fr_os_oen}),
				.clk(fr_clock_in),
				.clkena (1'b1),
				.delayctrlin(dll_delay_value),
				.phaseinvertctrl (dqsoutputphaseinvert[0]),
				.enaoutputcycledelay (enaoctcycledelaysetting[0]),
				.enaphasetransferreg (enaoctphasetransferreg[0]),
		
				.dataout(aligned_os_oe_bar),
				.phasectrlin(dqsoutputphasesetting[0])
			);
			defparam dqs_oe_bar_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
				dqs_oe_bar_alignment.use_phasectrl_clock = "true",
				dqs_oe_bar_alignment.use_delayed_clock = "true",
				dqs_oe_bar_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
				dqs_oe_bar_alignment.operation_mode = "oe",
				dqs_oe_bar_alignment.delay_buffer_mode = "high",
				dqs_oe_bar_alignment.power_up = "low",
				dqs_oe_bar_alignment.async_mode = "none",
				dqs_oe_bar_alignment.sync_mode = "none",
				dqs_oe_bar_alignment.use_primary_clock = "true",
				dqs_oe_bar_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
				dqs_oe_bar_alignment.add_output_cycle_delay = DYNAMIC_MODE,
				dqs_oe_bar_alignment.invert_phase = DYNAMIC_MODE,
				dqs_oe_bar_alignment.bypass_input_register = "false";

			stratixiv_output_phase_alignment	dqs_oct_bar_alignment (
				.datain			({1'b0,fr_os_octn}),
				.clk(fr_clock_in),
				.clkena			(1'b1),
				.delayctrlin(dll_delay_value),
				.phasectrlin (dqsoutputphasesetting[0]),
				.phaseinvertctrl (dqsoutputphaseinvert[0]),
				.enaoutputcycledelay (enaoctcycledelaysetting[0]),
				.enaphasetransferreg (enaoctphasetransferreg[0]),
				.dataout		(aligned_os_oct_bar )
			);
			defparam
				dqs_oct_bar_alignment.operation_mode = "rtena",
				dqs_oct_bar_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
				dqs_oct_bar_alignment.delay_buffer_mode = "high",
				dqs_oct_bar_alignment.power_up = "low",
				dqs_oct_bar_alignment.async_mode = "none",
				dqs_oct_bar_alignment.sync_mode = "none",
				dqs_oct_bar_alignment.use_phasectrl_clock = "true",
				dqs_oct_bar_alignment.use_primary_clock = "true",
				dqs_oct_bar_alignment.use_delayed_clock = "true",
				dqs_oct_bar_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
				dqs_oct_bar_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
				dqs_oct_bar_alignment.add_output_cycle_delay = DYNAMIC_MODE,
				dqs_oct_bar_alignment.invert_phase = DYNAMIC_MODE,
				dqs_oct_bar_alignment.bypass_input_register = "false";
		end
		else
		begin

			(* ALTERA_ATTRIBUTE = {"FAST_OUTPUT_ENABLE_REGISTER=ON"} *) reg oe_bar_reg;
			(* ALTERA_ATTRIBUTE = {"FAST_OCT_REGISTER=ON"} *) reg oct_bar_reg;
			initial
			begin
				oe_bar_reg = 0;
				oct_bar_reg = 0;
			end
			always @ ( posedge write_strobe_clock_in)
				oe_bar_reg <= fr_os_oen;

			assign aligned_os_oe_bar = oe_bar_reg;

				always @ (posedge fr_clock_in)
					oct_bar_reg <= fr_os_octn;
		
				assign aligned_os_oct_bar = oct_bar_reg;
		end		
		
		if (USE_DYNAMIC_CONFIG == "true")
		begin
			stratixiv_delay_chain oct_delay_1_bar(
				.datain             (aligned_os_oct_bar),
				.delayctrlin        (octdelaysetting1 [0]),
				.dataout            (delayed_os_bar_oct_1)
			);

			stratixiv_delay_chain oct_delay_2_bar(
				.datain             (delayed_os_bar_oct_1),
				.delayctrlin        ({1'b0, octdelaysetting2 [0]}),
				.dataout            (delayed_os_bar_oct)
			);

			stratixiv_delay_chain oe_delay_1_bar(
				.datain             (aligned_os_oe_bar),
				.delayctrlin        (dqsn_outputdelaysetting1),
				.dataout            (delayed_os_bar_oe_1)
			);

			stratixiv_delay_chain oe_delay_2_bar(
				.datain             (delayed_os_bar_oe_1),
				.delayctrlin        (dqsn_outputdelaysetting2),
				.dataout            (delayed_os_bar_oe)
			);
		end
		else
		begin
			assign delayed_os_bar_oct = aligned_os_oct_bar;
			assign delayed_os_bar_oe = aligned_os_oe_bar;
		end

		stratixiv_pseudo_diff_out   pseudo_diffa_0
		( 
		.i(os_delayed2),
		.o(os),
		.obar(os_bar)
		);

		if (USE_BIDIR_STROBE == "true")
		begin
			stratixiv_io_obuf   obuf_os_bar_0
			( 
			.i(os_bar),
			.o(strobe_n_io),
			.obar(),
			.oe(~delayed_os_bar_oe),
			.parallelterminationcontrol	(parallelterminationcontrol_in),
			.seriesterminationcontrol	(seriesterminationcontrol_in),
			.dynamicterminationcontrol	(delayed_os_bar_oct)
			);
			defparam obuf_os_bar_0.bus_hold = "false",
				obuf_os_bar_0.open_drain_output = "false";
		end
		else
		begin
			stratixiv_io_obuf   obuf_os_bar_0
			( 
			.i(os_bar),
			.o(strobe_n_io),
			.obar(),
			.oe(~delayed_os_bar_oe)
			);
			defparam obuf_os_bar_0.bus_hold = "false",
				obuf_os_bar_0.open_drain_output = "false";
		end
	end
	else
		assign os = os_delayed2;


	if (USE_BIDIR_STROBE == "true")
	begin
		stratixiv_io_obuf   obuf_os_0
			( 
			.i(os),
			.o(strobe_io),
			.obar(),
			.oe(~delayed_os_oe),
			.parallelterminationcontrol	(parallelterminationcontrol_in),
			.seriesterminationcontrol	(seriesterminationcontrol_in),
			.dynamicterminationcontrol	(delayed_os_oct)
			);
			defparam obuf_os_0.bus_hold = "false",
				obuf_os_0.open_drain_output = "false";
	end
	else
	begin
		stratixiv_io_obuf   obuf_os_0
			( 
			.i(os),
			.o(strobe_io),
			.obar(),
			.oe(~delayed_os_oe)
			);
			defparam obuf_os_0.bus_hold = "false",
				obuf_os_0.open_drain_output = "false";
	
	end	
end
endgenerate


wire [PIN_WIDTH-1:0] aligned_oe ;
wire [PIN_WIDTH-1:0] aligned_data;
wire [PIN_WIDTH-1:0] ddr_data;
wire [PIN_WIDTH-1:0] aligned_oct;

generate
	if (PIN_TYPE == "output" || PIN_TYPE == "bidir")
	begin
		genvar opin_num;
		for (opin_num = 0; opin_num < PIN_WIDTH; opin_num = opin_num + 1)
		begin :output_path_gen
			wire fr_data_hi;
			wire fr_data_lo;
			wire fr_oe;
			wire fr_oct;
			
			if (USE_HALF_RATE_OUTPUT == "true")
			begin
				stratixiv_ddio_out hr_to_fr_hi (
					.datainhi(write_data_in [opin_num + 3*PIN_WIDTH]),
					.datainlo(write_data_in [opin_num + 1*PIN_WIDTH]),
					.dataout(fr_data_hi),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
				);
				defparam hr_to_fr_hi.half_rate_mode = "true",
						hr_to_fr_hi.use_new_clocking_model = "true",
						hr_to_fr_hi.async_mode = "none";
				
				stratixiv_ddio_out hr_to_fr_lo (
					.datainhi(write_data_in [opin_num + 2*PIN_WIDTH]),
					.datainlo(write_data_in [opin_num + 0]),
					.dataout(fr_data_lo),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
				);
				defparam hr_to_fr_lo.half_rate_mode = "true",
						hr_to_fr_lo.use_new_clocking_model = "true",
						hr_to_fr_lo.async_mode = "none";
				
				stratixiv_ddio_out hr_to_fr_oe (
					.datainhi(~write_oe_in [2*opin_num + 1]),
					.datainlo(~write_oe_in [2*opin_num + 0]),
					.dataout(fr_oe),
					.clkhi (hr_clock_in),
					.clklo (hr_clock_in),
					.muxsel (hr_clock_in)
				);
				defparam hr_to_fr_oe.half_rate_mode = "true",
						hr_to_fr_oe.use_new_clocking_model = "true";
						
					stratixiv_ddio_out hr_to_fr_oct (
						.datainhi(oct_ena [1]),
						.datainlo(oct_ena [0]),
						.dataout(fr_oct),
						.clkhi (hr_clock_in),
						.clklo (hr_clock_in),
						.muxsel (hr_clock_in)
					);
					defparam hr_to_fr_oct.half_rate_mode = "true",
							hr_to_fr_oct.use_new_clocking_model = "true";
														
			end
			else
			begin
				assign fr_data_lo = write_data_in [opin_num+PIN_WIDTH];
				assign fr_data_hi = write_data_in [opin_num];
				assign fr_oe = ~write_oe_in [opin_num];
				assign fr_oct = fr_term;
			end
			
			if (USE_OUTPUT_PHASE_ALIGNMENT == "true")
			begin
				stratixiv_output_phase_alignment data_alignment(
					.datain({fr_data_hi,fr_data_lo}),
					.clk(fr_clock_in),
					.clkena (1'b1),
					.delayctrlin(dll_delay_value),
					.phasectrlin (dqoutputphasesetting[opin_num/DQ_BREAKPOINT]),
					.phaseinvertctrl (dqoutputphaseinvert[opin_num/DQ_BREAKPOINT]),
					.enaoutputcycledelay (enaoutputcycledelaysetting[opin_num/DQ_BREAKPOINT]),
					.enaphasetransferreg (enaoutputphasetransferreg[opin_num/DQ_BREAKPOINT]),
					.dataout(aligned_data[opin_num])
				);
				defparam data_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
					data_alignment.use_phasectrl_clock = "true",
					data_alignment.use_delayed_clock = "true",
					data_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
					data_alignment.operation_mode = "ddio_out",
					data_alignment.delay_buffer_mode = "high",
					data_alignment.power_up = "low",
					data_alignment.async_mode = "none",
					data_alignment.sync_mode = "none",
					data_alignment.use_primary_clock = "true",
					data_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
					data_alignment.add_output_cycle_delay = DYNAMIC_MODE,
					data_alignment.invert_phase = DYNAMIC_MODE,
					data_alignment.bypass_input_register = "false";
	
				stratixiv_output_phase_alignment oe_alignment(
					.datain({1'b0,fr_oe}), 
					.clk(fr_clock_in),
					.clkena(1'b1),
					.delayctrlin(dll_delay_value),
					.phasectrlin (dqoutputphasesetting[opin_num/DQ_BREAKPOINT]),
					.phaseinvertctrl (dqoutputphaseinvert[opin_num/DQ_BREAKPOINT]),
					.enaoutputcycledelay (enaoutputcycledelaysetting[opin_num/DQ_BREAKPOINT]),
					.enaphasetransferreg (enaoutputphasetransferreg[opin_num/DQ_BREAKPOINT]),
					.dataout(aligned_oe[opin_num])
				);
				defparam oe_alignment.operation_mode = "oe",
					oe_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
					oe_alignment.use_phasectrl_clock = "true",
					oe_alignment.use_delayed_clock = "true",
					oe_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
  				   	oe_alignment.delay_buffer_mode = "high",
					oe_alignment.power_up = "low",
					oe_alignment.async_mode = "none",
					oe_alignment.sync_mode = "none",
					oe_alignment.use_primary_clock = "true",
					oe_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
					oe_alignment.add_output_cycle_delay = DYNAMIC_MODE,
					oe_alignment.invert_phase = DYNAMIC_MODE,
					oe_alignment.bypass_input_register = "false";

				stratixiv_output_phase_alignment	oct_alignment (
					.datain			({1'b0,fr_oct}),
					.clk(fr_clock_in),
					.clkena			(1'b1),
					.delayctrlin(dll_delay_value),
					.phasectrlin (dqsoutputphasesetting[opin_num/DQ_BREAKPOINT]),
					.phaseinvertctrl (dqsoutputphaseinvert[opin_num/DQ_BREAKPOINT]),
					.enaoutputcycledelay (enaoctcycledelaysetting[opin_num/DQ_BREAKPOINT]),
					.enaphasetransferreg (enaoctphasetransferreg[opin_num/DQ_BREAKPOINT]),
					.dataout		(aligned_oct [opin_num])
				);
				defparam
					oct_alignment.operation_mode = "rtena",
					oct_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
					oct_alignment.delay_buffer_mode = "high",
					oct_alignment.power_up = "low",
					oct_alignment.async_mode = "none",
					oct_alignment.sync_mode = "none",
					oct_alignment.use_phasectrl_clock = "true",
					oct_alignment.use_primary_clock = "true",
					oct_alignment.use_delayed_clock = "true",
					oct_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
					oct_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
					oct_alignment.add_output_cycle_delay = DYNAMIC_MODE,
					oct_alignment.invert_phase = DYNAMIC_MODE,
					oct_alignment.bypass_input_register = "false";
			end
			else
			begin
				(* ALTERA_ATTRIBUTE = {"FAST_OUTPUT_ENABLE_REGISTER=ON"} *) reg oe_reg;
				(* ALTERA_ATTRIBUTE = {"FAST_OCT_REGISTER=ON"} *) reg oct_reg;		
	
				stratixiv_ddio_out ddio_out (
					.datainhi(fr_data_hi),
					.datainlo(fr_data_lo),
					.dataout(aligned_data[opin_num]),
					.clkhi (fr_clock_in),
					.clklo (fr_clock_in),
					.muxsel (fr_clock_in)
				);
				defparam ddio_out.async_mode = "none",
						ddio_out.half_rate_mode = "false",
						ddio_out.sync_mode = "none",
						ddio_out.use_new_clocking_model = "true";

				initial
				begin
					oe_reg = 0;
					oct_reg = 0;
				end
				always @ ( posedge fr_clock_in)
					oe_reg <= fr_oe;

				assign aligned_oe [opin_num] = oe_reg;

				always @ (posedge fr_clock_in)
					oct_reg <= fr_oct; 

				assign aligned_oct [opin_num] = oct_reg;
				

			end
		end
	end
endgenerate


generate
if (PIN_TYPE == "input" || PIN_TYPE == "bidir")
begin
	genvar ipin_num;
	for (ipin_num = 0; ipin_num < PIN_WIDTH; ipin_num = ipin_num + 1)
	begin :input_path_gen

		wire [1:0] sdr_data;
		wire [1:0] aligned_input;
	
		if (INVERT_CAPTURE_STROBE == "true")
		begin
			stratixiv_ddio_in capture_reg(
				.datain(ddr_data[ipin_num]),
				.clk (~dqsbusout),
				.regouthi(sdr_data[1]),
				.regoutlo(sdr_data[0])
			);
			defparam capture_reg.use_clkn = "false",
					capture_reg.async_mode = "none",
					capture_reg.sync_mode = "none";
		end
		else
		begin
			stratixiv_ddio_in capture_reg(
				.datain(ddr_data[ipin_num]),
				.clk (dqsbusout),
				.regouthi(sdr_data[1]),
				.regoutlo(sdr_data[0])
			);
		end
		if (USE_INPUT_PHASE_ALIGNMENT == "true") 
		begin
		
			stratixiv_input_phase_alignment data_alignment_hi(
				.datain(sdr_data[1]),
				.clk(fr_clock_in),
				.delayctrlin(dll_delay_value),
				.dataout(aligned_input[1])
			);
			defparam data_alignment_hi.USE_PHASECTRLIN = "false",
								data_alignment_hi.USE_PHASECTRL_CLOCK = "false",
								data_alignment_hi.USE_DELAYED_CLOCK = "true",
								data_alignment_hi.PHASE_SETTING_FOR_DELAYED_CLOCK = 0;

			stratixiv_input_phase_alignment data_alignment_lo(
				.datain(sdr_data[0]),
				.clk(fr_clock_in),
				.delayctrlin(dll_delay_value),
				.dataout(aligned_input[0])
			);
			defparam data_alignment_lo.USE_PHASECTRLIN = "false",
								data_alignment_lo.USE_PHASECTRL_CLOCK = "false",
								data_alignment_lo.USE_DELAYED_CLOCK = "true",
								data_alignment_lo.PHASE_SETTING_FOR_DELAYED_CLOCK = 0;
		end
		else
		begin
			assign aligned_input = sdr_data;
		end
		
		if (USE_HALF_RATE_INPUT == "true")
		begin
		/*
			stratixiv_half_rate_input half_rate (
				.datain (aligned_input),
				.clk (hr_clock_in),
				.dataout ({fpga_data_in_hi[ipin_num], fpga_data_in_lo[ipin_num],fpga_data_in_hi_hr[ipin_num],fpga_data_in_lo_hr[ipin_num]})
			);
			*/
		end
		else
		begin
			if (REVERSE_READ_WORDS == "true")
			begin
				assign read_data_out [ipin_num] = aligned_input [1];
				assign read_data_out [PIN_WIDTH +ipin_num] = aligned_input [0];
			end
			else
			begin
				assign read_data_out [ipin_num] = aligned_input [0];
				assign read_data_out [PIN_WIDTH +ipin_num] = aligned_input [1];
			end
		end
	end
end
endgenerate

generate
	genvar pin_num;
	for (pin_num = 0; pin_num < PIN_WIDTH; pin_num = pin_num + 1)
	begin :pad_gen
		
		if (PIN_TYPE != "bidir")
		begin
			assign read_write_data_io [pin_num] = 1'b0;
		end
	
	
		wire delayed_data_in;
		wire delayed_data_out;
		wire delayed_oe;
		
		wire [3:0] dq_outputdelaysetting1;
		wire [3:0] dq_outputdelaysetting2;
		wire [3:0] dq_inputdelaysetting;
		
		assign dq_outputdelaysetting2 [3] = 1'b0;
		
		if (USE_DYNAMIC_CONFIG == "true")
		begin
		stratixiv_io_config config_1 (
		    .datain(config_data_in),          // shared per DQS group
			.clk(config_clock_in),
			.ena(config_io_ena[pin_num]),
			.update(config_update),       // shared per DQS group

			.outputdelaysetting1(dq_outputdelaysetting1),
			.outputdelaysetting2(dq_outputdelaysetting2 [2:0]),
			.padtoinputregisterdelaysetting(dq_inputdelaysetting),

			.outputfinedelaysetting1(),
			.outputfinedelaysetting2(),
			.padtoinputregisterfinedelaysetting(),

			.outputonlyfinedelaysetting2(),
			.outputonlydelaysetting2(),

			.dutycycledelaymode(),
			.dutycycledelaysettings(),

			.dataout()
		);
		end
	
		if (PIN_TYPE == "input" || PIN_TYPE == "bidir")
		begin
			wire raw_input;
			if (USE_DYNAMIC_CONFIG == "true")
			begin
				stratixiv_delay_chain out_delay_1(
					.datain             (raw_input),
					.delayctrlin        (dq_inputdelaysetting),
					.dataout            (ddr_data[pin_num])
				);
			end
			else
			begin
				assign ddr_data[pin_num] = raw_input;
			end	
			
			stratixiv_io_ibuf data_in (
				.i(read_write_data_io [pin_num]),
				.o(raw_input)
			);
		end
		
		wire delayed_oct;
		
		if (PIN_TYPE == "output" || PIN_TYPE == "bidir")
		begin
		
			if (USE_DYNAMIC_CONFIG == "true")
			begin
	
				wire delayed_data_1;
				wire delayed_oe_1;
				wire delayed_oct_1;
						
				stratixiv_delay_chain out_delay_1(
					.datain             (aligned_data[pin_num]),
					.delayctrlin        (dq_outputdelaysetting1),
					.dataout            (delayed_data_1)
				);

				stratixiv_delay_chain out_delay_2(
					.datain             (delayed_data_1),
					.delayctrlin        (dq_outputdelaysetting2),
					.dataout            (delayed_data_out)
				);
				
				stratixiv_delay_chain oe_delay_1(
					.datain             (aligned_oe[pin_num]),
					.delayctrlin        (dq_outputdelaysetting1),
					.dataout            (delayed_oe_1)
				);

				stratixiv_delay_chain oe_delay_2(
					.datain             (delayed_oe_1),
					.delayctrlin        (dq_outputdelaysetting2),
					.dataout            (delayed_oe)
				);
				
				stratixiv_delay_chain oct_delay_1(
					.datain             (aligned_oct[pin_num]),
					.delayctrlin        (octdelaysetting1 [pin_num/DQ_BREAKPOINT]),
					.dataout            (delayed_oct_1)
				);

				stratixiv_delay_chain oct_delay_2(
					.datain             (delayed_oct_1),
					.delayctrlin        ({1'b0, octdelaysetting2 [pin_num/DQ_BREAKPOINT]}),
					.dataout            (delayed_oct)
				);
			end
			else
			begin
				assign delayed_data_out = aligned_data [pin_num];
				assign delayed_oe = aligned_oe [pin_num];
				assign delayed_oct = aligned_oct [pin_num];
			end
		
			if (PIN_TYPE == "output")
			begin
				stratixiv_io_obuf obuf_1 (
					.i (delayed_data_out),
					.o (read_write_data_io [pin_num]),
					.oe (~delayed_oe)
				);
			end
			else if (PIN_TYPE == "bidir")
			begin
				stratixiv_io_obuf data_out (
					.i (delayed_data_out),
					.o (read_write_data_io [pin_num]),
					.oe (~delayed_oe),
					.parallelterminationcontrol	(parallelterminationcontrol_in),
					.seriesterminationcontrol	(seriesterminationcontrol_in),
					.dynamicterminationcontrol	(delayed_oct)
				);
			end
		end
	end
endgenerate

generate
	genvar epin_num;
	for (epin_num = 0; epin_num < EXTRA_OUTPUT_WIDTH; epin_num = epin_num + 1)
	begin :extra_output_pad_gen
		wire fr_data_hi;
		wire fr_data_lo;
		wire aligned_data;
		
		if (USE_HALF_RATE_OUTPUT == "true")
		begin
			stratixiv_ddio_out hr_to_fr_hi (
				.datainhi(extra_write_data_in [epin_num + 3*EXTRA_OUTPUT_WIDTH]),
				.datainlo(extra_write_data_in [epin_num + 2*EXTRA_OUTPUT_WIDTH]),
				.dataout(fr_data_hi),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
			);
			defparam hr_to_fr_hi.half_rate_mode = "true",
					hr_to_fr_hi.use_new_clocking_model = "true",
					hr_to_fr_hi.async_mode = "none";
			
			stratixiv_ddio_out hr_to_fr_lo (
				.datainhi(extra_write_data_in [epin_num + 1*EXTRA_OUTPUT_WIDTH]),
				.datainlo(extra_write_data_in [epin_num + 0]),
				.dataout(fr_data_lo),
				.clkhi (hr_clock_in),
				.clklo (hr_clock_in),
				.muxsel (hr_clock_in)
			);
			defparam hr_to_fr_lo.half_rate_mode = "true",
					hr_to_fr_lo.use_new_clocking_model = "true",
					hr_to_fr_lo.async_mode = "none";
		end
		else
		begin
			assign fr_data_lo = extra_write_data_in [epin_num+EXTRA_OUTPUT_WIDTH];
			assign fr_data_hi = extra_write_data_in [epin_num];	
		end
		
		if (USE_OUTPUT_PHASE_ALIGNMENT == "true")
		begin
			stratixiv_output_phase_alignment data_alignment(
				.datain({fr_data_hi,fr_data_lo}),
				.clk(fr_clock_in),
				.clkena (1'b1),
				.delayctrlin(dll_delay_value),
				.phasectrlin (dqoutputphasesetting[(epin_num+2+PIN_WIDTH)/PINS_PER_DQS_CONFIG]),
				.phaseinvertctrl (dqoutputphaseinvert[(epin_num+2+PIN_WIDTH)/PINS_PER_DQS_CONFIG]),
				.enaoutputcycledelay (enaoutputcycledelaysetting[(epin_num+2+PIN_WIDTH)/PINS_PER_DQS_CONFIG]),
				.enaphasetransferreg (enaoutputphasetransferreg[(epin_num+2+PIN_WIDTH)/PINS_PER_DQS_CONFIG]),
				.dataout(aligned_data)
			);
			defparam data_alignment.use_phasectrlin = USE_DYNAMIC_CONFIG,
				data_alignment.use_phasectrl_clock = "true",
				data_alignment.use_delayed_clock = "true",
				data_alignment.phase_setting_for_delayed_clock = DELAYED_CLOCK_PHASE_SETTING,
				data_alignment.operation_mode = "ddio_out",
				data_alignment.delay_buffer_mode = "high",
				data_alignment.power_up = "low",
				data_alignment.async_mode = "none",
				data_alignment.sync_mode = "none",
				data_alignment.use_primary_clock = "true",
				data_alignment.add_phase_transfer_reg = DYNAMIC_MODE,
				data_alignment.add_output_cycle_delay = DYNAMIC_MODE,
				data_alignment.invert_phase = DYNAMIC_MODE,
				data_alignment.bypass_input_register = "false";

		end
		else
		begin
			(* ALTERA_ATTRIBUTE = {"FAST_OUTPUT_ENABLE_REGISTER=ON"} *) reg oe_reg;
		
			stratixiv_ddio_out ddio_out (
				.datainhi(fr_data_hi),
				.datainlo(fr_data_lo),
				.dataout(aligned_data),
				.clkhi (fr_clock_in),
				.clklo (fr_clock_in),
				.muxsel (fr_clock_in)
			);
			defparam ddio_out.async_mode = "none",
					ddio_out.half_rate_mode = "false",
					ddio_out.sync_mode = "none",
					ddio_out.use_new_clocking_model = "true";
		end
		
		wire delayed_data_out;
		
		wire [3:0] dq_outputdelaysetting1;
		wire [3:0] dq_outputdelaysetting2;
		wire [3:0] dq_inputdelaysetting;
		
		assign dq_outputdelaysetting2 [3] = 1'b0;
	
		if (USE_DYNAMIC_CONFIG == "true")
		begin	
			stratixiv_io_config config_1 (
				.datain(config_data_in),          // shared per DQS group
				.clk(config_clock_in),
				.ena(config_extra_io_ena[epin_num]),
				.update(config_update),       // shared per DQS group

				.outputdelaysetting1(dq_outputdelaysetting1),
				.outputdelaysetting2(dq_outputdelaysetting2 [2:0]),
				.padtoinputregisterdelaysetting(dq_inputdelaysetting),

				.outputfinedelaysetting1(),
				.outputfinedelaysetting2(),
				.padtoinputregisterfinedelaysetting(),

				.outputonlyfinedelaysetting2(),
				.outputonlydelaysetting2(),

				.dutycycledelaymode(),
				.dutycycledelaysettings(),

				.dataout()
			);
		
			wire delayed_data_1;
						
			stratixiv_delay_chain out_delay_1(
				.datain             (aligned_data),
				.delayctrlin        (dq_outputdelaysetting1),
				.dataout            (delayed_data_1)
			);

			stratixiv_delay_chain out_delay_2(
				.datain             (delayed_data_1),
				.delayctrlin        (dq_outputdelaysetting2),
				.dataout            (delayed_data_out)
			);
				
		end
		else
		begin
			assign delayed_data_out = aligned_data;
		end

		stratixiv_io_obuf obuf_1 (
			.i (delayed_data_out),
			.o (extra_write_data_out[epin_num]),
			.oe (1'b1),
			.parallelterminationcontrol	(parallelterminationcontrol_in),
			.seriesterminationcontrol	(seriesterminationcontrol_in)
		);

	end

endgenerate



endmodule
