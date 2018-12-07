// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


// altera message_off 10036
module ddr2_v10_1_0002_new_io_pads(
	reset_n_addr_cmd_clk,
	reset_n_mem_clk,
	oct_ctl_rs_value,
	oct_ctl_rt_value,
	phy_ddio_addr_cmd_clk,
	phy_ddio_address_l,
	phy_ddio_address_h,
	phy_ddio_bank_l,
	phy_ddio_bank_h,
	phy_ddio_cs_n_l,
	phy_ddio_cs_n_h,
	phy_ddio_cke_l,
	phy_ddio_cke_h,
	phy_ddio_odt_l,
	phy_ddio_odt_h,
	phy_ddio_we_n_l,
	phy_ddio_we_n_h,
	phy_ddio_ras_n_l,
	phy_ddio_ras_n_h,
	phy_ddio_cas_n_l,
	phy_ddio_cas_n_h,
	phy_mem_address,
	phy_mem_bank,
	phy_mem_cs_n,
	phy_mem_cke,
	phy_mem_odt,
	phy_mem_we_n,
	phy_mem_ras_n,
	phy_mem_cas_n,
	pll_afi_clk,
	pll_mem_clk,
	pll_write_clk,
	pll_dqs_ena_clk,
	phy_ddio_dq,
	phy_ddio_dqs_en,
	phy_ddio_oct_ena,
	dqs_enable_ctrl,
	phy_ddio_wrdata_en,
	phy_ddio_wrdata_mask,
	phy_mem_dq,
	phy_mem_dm,
	phy_mem_ck,
	phy_mem_ck_n,
	mem_dqs,
	mem_dqs_n,
	dll_phy_delayctrl,
	ddio_phy_dq,
	read_capture_clk, 
	scc_clk,
	scc_data,
	scc_dqs_ena,
	scc_dqs_io_ena,
	scc_dq_ena,
	scc_dm_ena,
	scc_upd
);


parameter DEVICE_FAMILY = "";
parameter OCT_SERIES_TERM_CONTROL_WIDTH = "";  
parameter OCT_PARALLEL_TERM_CONTROL_WIDTH = ""; 
parameter MEM_ADDRESS_WIDTH     = ""; 
parameter MEM_BANK_WIDTH        = ""; 
parameter MEM_CHIP_SELECT_WIDTH = ""; 
parameter MEM_CLK_EN_WIDTH 		= ""; 
parameter MEM_CK_WIDTH 			= ""; 
parameter MEM_ODT_WIDTH 		= ""; 
parameter MEM_DQS_WIDTH			= "";
parameter MEM_DM_WIDTH          = ""; 
parameter MEM_CONTROL_WIDTH     = ""; 
parameter MEM_DQ_WIDTH          = ""; 
parameter MEM_READ_DQS_WIDTH    = ""; 
parameter MEM_WRITE_DQS_WIDTH   = ""; 

parameter AFI_ADDRESS_WIDTH         = ""; 
parameter AFI_BANK_WIDTH            = ""; 
parameter AFI_CHIP_SELECT_WIDTH     = ""; 
parameter AFI_CLK_EN_WIDTH 			= ""; 
parameter AFI_ODT_WIDTH 			= ""; 
parameter AFI_DATA_MASK_WIDTH       = ""; 
parameter AFI_CONTROL_WIDTH         = ""; 
parameter AFI_DATA_WIDTH            = ""; 
parameter AFI_DQS_WIDTH             = ""; 

parameter DLL_DELAY_CTRL_WIDTH  = "";

parameter ALTDQDQS_INPUT_FREQ = "";
parameter ALTDQDQS_DELAY_CHAIN_BUFFER_MODE = "";
parameter ALTDQDQS_DQS_PHASE_SETTING = "";
parameter ALTDQDQS_DQS_PHASE_SHIFT = "";
parameter ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING = "";

localparam DOUBLE_MEM_DQ_WIDTH = MEM_DQ_WIDTH * 2;
localparam HALF_AFI_DATA_WIDTH = AFI_DATA_WIDTH / 2;
localparam HALF_AFI_DQS_WIDTH = AFI_DQS_WIDTH / 2;


input	reset_n_addr_cmd_clk;
input	reset_n_mem_clk;

input   [OCT_SERIES_TERM_CONTROL_WIDTH-1:0] oct_ctl_rs_value;
input   [OCT_PARALLEL_TERM_CONTROL_WIDTH-1:0] oct_ctl_rt_value;

input	phy_ddio_addr_cmd_clk;
input	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_l;
input	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_h;
input	[MEM_BANK_WIDTH-1:0]    phy_ddio_bank_l;
input	[MEM_BANK_WIDTH-1:0]    phy_ddio_bank_h;
input	[MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_l;
input	[MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_h;
input	[MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_l;
input	[MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_h;
input	[MEM_ODT_WIDTH-1:0] phy_ddio_odt_l;
input	[MEM_ODT_WIDTH-1:0] phy_ddio_odt_h;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_l;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_h;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_l;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_h;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_l;
input	[MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_h;

output  [MEM_ADDRESS_WIDTH-1:0]	phy_mem_address;
output	[MEM_BANK_WIDTH-1:0]	phy_mem_bank;
output	[MEM_CHIP_SELECT_WIDTH-1:0]	phy_mem_cs_n;
output  [MEM_CLK_EN_WIDTH-1:0]	phy_mem_cke;
output  [MEM_ODT_WIDTH-1:0]	phy_mem_odt;
output	[MEM_CONTROL_WIDTH-1:0]	phy_mem_we_n;
output	[MEM_CONTROL_WIDTH-1:0] phy_mem_ras_n;
output	[MEM_CONTROL_WIDTH-1:0] phy_mem_cas_n;

input	pll_afi_clk;
input	pll_mem_clk;
input	pll_write_clk;
input	pll_dqs_ena_clk;
input	[AFI_DATA_WIDTH-1:0]  phy_ddio_dq;
input	[AFI_DQS_WIDTH-1:0] phy_ddio_dqs_en;
input	[AFI_DQS_WIDTH-1:0] phy_ddio_oct_ena;
input	[AFI_DQS_WIDTH-1:0] dqs_enable_ctrl;
input	[AFI_DQS_WIDTH-1:0] phy_ddio_wrdata_en;
input	[AFI_DATA_MASK_WIDTH-1:0]	phy_ddio_wrdata_mask;	


inout	[MEM_DQ_WIDTH-1:0]	phy_mem_dq;
output	[MEM_DM_WIDTH-1:0]	phy_mem_dm;
output	[MEM_CK_WIDTH-1:0]	phy_mem_ck;
output	[MEM_CK_WIDTH-1:0]	phy_mem_ck_n;
inout	[MEM_DQS_WIDTH-1:0]	mem_dqs;
inout	[MEM_DQS_WIDTH-1:0]	mem_dqs_n;

input   [DLL_DELAY_CTRL_WIDTH-1:0]  dll_phy_delayctrl;
output	[DOUBLE_MEM_DQ_WIDTH-1:0] ddio_phy_dq;	
output	[MEM_READ_DQS_WIDTH-1:0] read_capture_clk;	

input	scc_clk;
input	scc_data;
input	[MEM_WRITE_DQS_WIDTH - 1:0] scc_dqs_ena;
input	[MEM_WRITE_DQS_WIDTH - 1:0] scc_dqs_io_ena;
input	[MEM_DQ_WIDTH - 1:0] scc_dq_ena;
input	[MEM_DM_WIDTH - 1:0] scc_dm_ena;
input	scc_upd;

wire	[MEM_DQ_WIDTH-1:0] mem_phy_dq;
wire	[DLL_DELAY_CTRL_WIDTH-1:0] read_bidir_dll_phy_delayctrl;
wire	[MEM_READ_DQS_WIDTH-1:0] bidir_read_dqs_bus_out;
wire	[MEM_DQ_WIDTH-1:0] bidir_read_dq_input_data_out_high;
wire	[MEM_DQ_WIDTH-1:0] bidir_read_dq_input_data_out_low;

	ddr2_v10_1_0002_addr_cmd_pads uaddr_cmd_pads(
		.reset_n				(reset_n_addr_cmd_clk), 
		.reset_n_mem_clk		(reset_n_mem_clk), 
		.pll_mem_clk            (pll_mem_clk),
		.phy_ddio_addr_cmd_clk	(phy_ddio_addr_cmd_clk),

		.phy_ddio_address_l 	(phy_ddio_address_l),
		.phy_ddio_address_h 	(phy_ddio_address_h),
		.phy_ddio_bank_l		(phy_ddio_bank_l),
		.phy_ddio_bank_h		(phy_ddio_bank_h),
		.phy_ddio_cs_n_l		(phy_ddio_cs_n_l),
		.phy_ddio_cs_n_h		(phy_ddio_cs_n_h),
		.phy_ddio_cke_l			(phy_ddio_cke_l),
		.phy_ddio_cke_h			(phy_ddio_cke_h),
		.phy_ddio_odt_l			(phy_ddio_odt_l),
		.phy_ddio_odt_h			(phy_ddio_odt_h),
		.phy_ddio_we_n_l		(phy_ddio_we_n_l),	
		.phy_ddio_we_n_h		(phy_ddio_we_n_h),
		.phy_ddio_ras_n_l		(phy_ddio_ras_n_l),
		.phy_ddio_ras_n_h		(phy_ddio_ras_n_h),
		.phy_ddio_cas_n_l		(phy_ddio_cas_n_l),
		.phy_ddio_cas_n_h		(phy_ddio_cas_n_h),

		.phy_mem_address		(phy_mem_address),
		.phy_mem_bank			(phy_mem_bank),
		.phy_mem_cs_n			(phy_mem_cs_n),
		.phy_mem_cke			(phy_mem_cke),
		.phy_mem_odt			(phy_mem_odt),
		.phy_mem_we_n			(phy_mem_we_n),
		.phy_mem_ras_n			(phy_mem_ras_n),
		.phy_mem_cas_n			(phy_mem_cas_n),
		.phy_mem_ck				(phy_mem_ck),
		.phy_mem_ck_n			(phy_mem_ck_n)
    );
	defparam uaddr_cmd_pads.DEVICE_FAMILY			= DEVICE_FAMILY;
	defparam uaddr_cmd_pads.MEM_ADDRESS_WIDTH		= MEM_ADDRESS_WIDTH;
	defparam uaddr_cmd_pads.MEM_BANK_WIDTH			= MEM_BANK_WIDTH;
	defparam uaddr_cmd_pads.MEM_CHIP_SELECT_WIDTH	= MEM_CHIP_SELECT_WIDTH;
	defparam uaddr_cmd_pads.MEM_CLK_EN_WIDTH		= MEM_CLK_EN_WIDTH;
	defparam uaddr_cmd_pads.MEM_CK_WIDTH			= MEM_CK_WIDTH;
	defparam uaddr_cmd_pads.MEM_ODT_WIDTH			= MEM_ODT_WIDTH;
	defparam uaddr_cmd_pads.MEM_CONTROL_WIDTH		= MEM_CONTROL_WIDTH;
	
	localparam NUM_OF_DQDQS = MEM_WRITE_DQS_WIDTH;
	localparam DQDQS_DATA_WIDTH = MEM_DQ_WIDTH / NUM_OF_DQDQS;
	
	localparam DQDQS_DM_WIDTH = MEM_DM_WIDTH / MEM_WRITE_DQS_WIDTH;
		
	localparam NUM_OF_DQDQS_WITH_DM = MEM_WRITE_DQS_WIDTH;		
	
	wire	[HALF_AFI_DQS_WIDTH-1:0]	phy_ddio_oe_l;   
	wire	[HALF_AFI_DQS_WIDTH-1:0]	phy_ddio_oe_h;
	assign phy_ddio_oe_l = phy_ddio_wrdata_en[HALF_AFI_DQS_WIDTH-1:0];
	assign phy_ddio_oe_h = phy_ddio_wrdata_en[AFI_DQS_WIDTH-1:HALF_AFI_DQS_WIDTH];

	generate
	genvar i;
	for (i=0; i<NUM_OF_DQDQS; i=i+1)
	begin: dq_ddio
		wire dqs_busout;

		wire [DQDQS_DATA_WIDTH-1:0] phy_ddio_dq_t0;
		wire [DQDQS_DATA_WIDTH-1:0] phy_ddio_dq_t1;
		wire [DQDQS_DATA_WIDTH-1:0] phy_ddio_dq_t2;
		wire [DQDQS_DATA_WIDTH-1:0] phy_ddio_dq_t3;

		assign phy_ddio_dq_t0 = phy_ddio_dq [DQDQS_DATA_WIDTH*(i+1+2*NUM_OF_DQDQS)-1 : DQDQS_DATA_WIDTH*(i+2*NUM_OF_DQDQS)];  
		assign phy_ddio_dq_t1 = phy_ddio_dq [DQDQS_DATA_WIDTH*(i+1)-1                : DQDQS_DATA_WIDTH*i];                         
		assign phy_ddio_dq_t2 = phy_ddio_dq [DQDQS_DATA_WIDTH*(i+1+3*NUM_OF_DQDQS)-1 : DQDQS_DATA_WIDTH*(i+3*NUM_OF_DQDQS)];  
		assign phy_ddio_dq_t3 = phy_ddio_dq [DQDQS_DATA_WIDTH*(i+1+NUM_OF_DQDQS)-1   : DQDQS_DATA_WIDTH*(i+NUM_OF_DQDQS)];    

		wire [DQDQS_DM_WIDTH-1:0] phy_ddio_wrdata_mask_t0;
		wire [DQDQS_DM_WIDTH-1:0] phy_ddio_wrdata_mask_t1;
		wire [DQDQS_DM_WIDTH-1:0] phy_ddio_wrdata_mask_t2;
		wire [DQDQS_DM_WIDTH-1:0] phy_ddio_wrdata_mask_t3;

		assign phy_ddio_wrdata_mask_t0 = phy_ddio_wrdata_mask [DQDQS_DM_WIDTH*(i+1+2*NUM_OF_DQDQS_WITH_DM)-1 : DQDQS_DM_WIDTH*(i+2*NUM_OF_DQDQS_WITH_DM)];
		assign phy_ddio_wrdata_mask_t1 = phy_ddio_wrdata_mask [DQDQS_DM_WIDTH*(i+1)-1                        : DQDQS_DM_WIDTH*i];     
		assign phy_ddio_wrdata_mask_t2 = phy_ddio_wrdata_mask [DQDQS_DM_WIDTH*(i+1+3*NUM_OF_DQDQS_WITH_DM)-1 : DQDQS_DM_WIDTH*(i+3*NUM_OF_DQDQS_WITH_DM)];
		assign phy_ddio_wrdata_mask_t3 = phy_ddio_wrdata_mask [DQDQS_DM_WIDTH*(i+1+NUM_OF_DQDQS_WITH_DM)-1   : DQDQS_DM_WIDTH*(i+NUM_OF_DQDQS_WITH_DM)];

			ddr2_v10_1_0002_altdqdqs ubidir_dq_dqs (
				.write_strobe_clock_in (1'b0),
				.fr_clock_in (pll_write_clk),
				.hr_clock_in(pll_afi_clk),
				.parallelterminationcontrol_in(oct_ctl_rt_value),
				.seriesterminationcontrol_in(oct_ctl_rs_value),
				.strobe_ena_clock_in (pll_dqs_ena_clk),

				.read_write_data_io (phy_mem_dq[(DQDQS_DATA_WIDTH*(i+1)-1) : DQDQS_DATA_WIDTH*i]),
				.read_data_out (ddio_phy_dq [(2*DQDQS_DATA_WIDTH*(i+1)-1) : 2*DQDQS_DATA_WIDTH*i]),
				.capture_strobe_out(dqs_busout),
			
				.extra_write_data_in ({phy_ddio_wrdata_mask_t0, phy_ddio_wrdata_mask_t1, phy_ddio_wrdata_mask_t2, phy_ddio_wrdata_mask_t3}),
				.write_oe_in ({{DQDQS_DATA_WIDTH{phy_ddio_oe_h[i]}},{DQDQS_DATA_WIDTH{phy_ddio_oe_l[i]}}}),
				
				.write_data_in ({phy_ddio_dq_t0, phy_ddio_dq_t2, phy_ddio_dq_t1, phy_ddio_dq_t3}),

				.output_strobe_in (pll_mem_clk),
				.strobe_io (mem_dqs[i]),
				.strobe_n_io (mem_dqs_n[i]),
				.output_strobe_ena ({phy_ddio_dqs_en[i+NUM_OF_DQDQS],phy_ddio_dqs_en[i]}),
				.capture_strobe_ena ({dqs_enable_ctrl[i+NUM_OF_DQDQS],dqs_enable_ctrl[i]}),
				.oct_ena_in ({phy_ddio_oct_ena[i+NUM_OF_DQDQS],phy_ddio_oct_ena[i]}),
				.extra_write_data_out (phy_mem_dm[i]),
				.config_data_in (scc_data),
				.config_dqs_ena (scc_dqs_ena[i]),
				.config_io_ena (scc_dq_ena[(DQDQS_DATA_WIDTH*(i+1)-1) : DQDQS_DATA_WIDTH*i]),
				.config_dqs_io_ena (scc_dqs_io_ena[i]),
				.config_update (scc_upd),
				.config_clock_in (scc_clk),
				.config_extra_io_ena (scc_dm_ena[i]),
				.dll_delayctrl_in (dll_phy_delayctrl)	
				);
				defparam  ubidir_dq_dqs.PIN_WIDTH = DQDQS_DATA_WIDTH,
					ubidir_dq_dqs.USE_OUTPUT_STROBE = "true",
					ubidir_dq_dqs.DIFFERENTIAL_OUTPUT_STROBE = "true",
					ubidir_dq_dqs.EXTRA_OUTPUT_WIDTH = 1,
					ubidir_dq_dqs.PIN_TYPE = "bidir",
					ubidir_dq_dqs.USE_BIDIR_STROBE = "true",
					ubidir_dq_dqs.DIFFERENTIAL_CAPTURE_STROBE= "true",
					ubidir_dq_dqs.USE_DQS_ENABLE = "true",
					ubidir_dq_dqs.INVERT_CAPTURE_STROBE = "true",
					ubidir_dq_dqs.REVERSE_READ_WORDS = "true",
					ubidir_dq_dqs.USE_OUTPUT_PHASE_ALIGNMENT = "true",
					ubidir_dq_dqs.USE_DYNAMIC_CONFIG = "true",
					ubidir_dq_dqs.DYNAMIC_MODE = "dynamic",
					ubidir_dq_dqs.DQS_ENABLE_PHASECTRL = "true",
					ubidir_dq_dqs.USE_TERMINATION_CONTROL = "true",
					ubidir_dq_dqs.USE_OCT_ENA_IN_FOR_OCT = "true",
					ubidir_dq_dqs.PREAMBLE_TYPE = "low",
					ubidir_dq_dqs.INPUT_FREQ = ALTDQDQS_INPUT_FREQ,
					ubidir_dq_dqs.DELAY_CHAIN_BUFFER_MODE = ALTDQDQS_DELAY_CHAIN_BUFFER_MODE,
					ubidir_dq_dqs.DQS_PHASE_SETTING = ALTDQDQS_DQS_PHASE_SETTING,
					ubidir_dq_dqs.DQS_PHASE_SHIFT = ALTDQDQS_DQS_PHASE_SHIFT,
					ubidir_dq_dqs.DELAYED_CLOCK_PHASE_SETTING = ALTDQDQS_DELAYED_CLOCK_PHASE_SETTING,
					ubidir_dq_dqs.USE_HALF_RATE_OUTPUT = "true",
					ubidir_dq_dqs.USE_HALF_RATE_CAPTURE_STROBE_ENA = "true";
			
		assign read_capture_clk[i] = ~dqs_busout;
	end
	endgenerate

endmodule
