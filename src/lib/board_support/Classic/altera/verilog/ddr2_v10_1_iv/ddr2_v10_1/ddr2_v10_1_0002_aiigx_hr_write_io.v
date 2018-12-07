// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_0002_aiigx_hr_write_io(
	pll_afi_clk,
	pll_write_clk,
	wdata_0,
	wdata_1,
	wdata_2,
	wdata_3,
	wdata_mask_0,
	wdata_mask_1,
	wdata_mask_2,
	wdata_mask_3,
	wdata_en,
	wdata_out,
	dq_oe,
	wdata_mask_out
);

parameter MEM_DQ_WIDTH          = "";
parameter MEM_DM_WIDTH          = "";
parameter MEM_READ_DQS_WIDTH    = "";
parameter MEM_WRITE_DQS_WIDTH   = "";








input	pll_afi_clk;
input	pll_write_clk;
input	[MEM_DQ_WIDTH-1:0]  wdata_0;
input	[MEM_DQ_WIDTH-1:0]  wdata_1;
input	[MEM_DQ_WIDTH-1:0]  wdata_2;
input	[MEM_DQ_WIDTH-1:0]  wdata_3;
input	[MEM_DM_WIDTH-1:0]  wdata_mask_0;
input	[MEM_DM_WIDTH-1:0]  wdata_mask_1;
input	[MEM_DM_WIDTH-1:0]  wdata_mask_2;
input	[MEM_DM_WIDTH-1:0]  wdata_mask_3;
input	wdata_en;

output	[MEM_DQ_WIDTH-1:0]	wdata_out;
output	[MEM_DQ_WIDTH-1:0]  dq_oe;
output	[MEM_DM_WIDTH-1:0]  wdata_mask_out;


wire    [MEM_DQ_WIDTH-1:0] wdata_in_h;
wire    [MEM_DQ_WIDTH-1:0] wdata_in_l;
wire    [MEM_DM_WIDTH-1:0] wdata_mask_in_h;
wire    [MEM_DM_WIDTH-1:0] wdata_mask_in_l;
wire	oe_in;

// Half rate to full rate conversion implemented in soft logic 
generate
genvar dq_count;

    for (dq_count=0; dq_count<MEM_DQ_WIDTH; dq_count=dq_count+1)
    begin: hr_to_fr_data
        ddr2_v10_1_0002_hr_to_fr    uhr_to_fr_d(
            .clk    (pll_afi_clk),
            .d_h0   (wdata_0[dq_count]),
            .d_h1   (wdata_1[dq_count]),
            .d_l0   (wdata_2[dq_count]),
            .d_l1   (wdata_3[dq_count]),
            .q0     (wdata_in_h[dq_count]),
            .q1     (wdata_in_l[dq_count])
        );

    end
endgenerate

generate
genvar dm_count;

    for (dm_count=0; dm_count < MEM_DM_WIDTH; dm_count=dm_count+1)
    begin: hr_to_fr_dm
        ddr2_v10_1_0002_hr_to_fr    uhr_to_fr_dm(
            .clk    (pll_afi_clk),
            .d_h0   (wdata_mask_0[dm_count]),
            .d_h1   (wdata_mask_1[dm_count]),
            .d_l0   (wdata_mask_2[dm_count]),
            .d_l1   (wdata_mask_3[dm_count]),
            .q0     (wdata_mask_in_h[dm_count]),
            .q1     (wdata_mask_in_l[dm_count])
        );

    	ddr2_v10_1_0002_write_mask_pad_fr  uwrite_mask_pad(
    	    .dq_output_reg_clk              (pll_write_clk),
    	    .dq_output_reg_clkena           (1'b1),
    	    .output_dq_output_data_in_high  (wdata_mask_in_h[dm_count]),
    	    .output_dq_output_data_in_low   (wdata_mask_in_l[dm_count]),
    	    .output_dq_output_data_out      (wdata_mask_out[dm_count])
    	);
    end

endgenerate

    // only need channel 0 for oe, ALTDQ_DQS only accept 1 oe input
    ddr2_v10_1_0002_hr_to_fr    uhr_to_fr_oe(
        .clk    (pll_afi_clk),
        .d_h0   (wdata_en),
        .d_h1   (1'b0),
        .d_l0   (wdata_en),
        .d_l1   (1'b0),
        .q0     (oe_in),
        .q1     ()
    );







endmodule
