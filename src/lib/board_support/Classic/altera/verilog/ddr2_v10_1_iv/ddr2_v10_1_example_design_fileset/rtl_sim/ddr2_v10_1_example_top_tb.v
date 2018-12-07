// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis translate_off
`timescale 1ps / 1ps
// synthesis translate_on

module ddr2_v10_1_example_top_tb ();

reg				clk;
reg				reset_n;

reg				soft_reset_n;

wire	[16-1:0]  mem_a;
wire	[3-1:0]  mem_ba;
wire	mem_ck;
wire	mem_ck_n;
wire	[2-1:0] mem_cke;
wire	[2-1:0] mem_cs_n;
wire	[8-1:0]  mem_dm;
wire	[2-1:0] mem_odt;
wire	[1-1:0] mem_ras_n;
wire	[1-1:0] mem_cas_n;
wire	[1-1:0] mem_we_n;
wire	[64-1:0]  mem_dq;
wire	[8-1:0] mem_dqs;
wire	[8-1:0] mem_dqs_n;

wire			pass;
wire			fail;
wire			afi_cal_fail;
wire			afi_cal_success;
reg				afi_cal_success_reported;
wire            local_init_done;
wire			test_complete;
`ifdef PNF_PER_BIT_OUTPUT
wire	[256-1:0]		pnf_per_bit;
wire	[256-1:0]		pnf_per_bit_persist;
`endif




ddr2_v10_1_example_top dut (
	.pll_ref_clk	(clk),
	.global_reset_n	(reset_n),
	.oct_rdn		(),
	.oct_rup		(),
	.mem_a			(mem_a),
	.mem_ba			(mem_ba),
	.mem_ck			(mem_ck),
	.mem_ck_n		(mem_ck_n),
	.mem_cke		(mem_cke),
	.mem_cs_n		(mem_cs_n),
	.mem_dm			(mem_dm),
	.mem_odt		(mem_odt),
	.mem_ras_n		(mem_ras_n),
	.mem_cas_n		(mem_cas_n),
	.mem_we_n		(mem_we_n),
	.mem_dq			(mem_dq),
	.mem_dqs		(mem_dqs),
	.mem_dqs_n		(mem_dqs_n),
	.reset_request_n(),
	.pass			(pass),
	.fail			(fail),
	.test_complete	(test_complete),
`ifdef PNF_PER_BIT_OUTPUT
	.pnf_per_bit	(pnf_per_bit),
	.pnf_per_bit_persist(pnf_per_bit_persist),
`endif
    .local_init_done(local_init_done),
	.afi_cal_success(afi_cal_success),
	.afi_cal_fail	(afi_cal_fail)
);

generate
genvar depth;
genvar width;
for (depth = 0; depth < 1; depth = depth + 1)
begin : depth_gen
	for (width = 0; width < 1; width = width + 1)
	begin : width_gen
		

		mem_model #(
			.MEM_CLK_EN_WIDTH	(2),
			.MEM_IF_BA_WIDTH	(3),
			.MEM_IF_ADDR_WIDTH	(16),
			.MEM_IF_ROW_WIDTH	(16),
			.MEM_IF_COL_WIDTH	(12),
			.MEM_IF_CS_WIDTH	(2 / 1),
			.MEM_IF_CS_PER_RANK (1),
			.MEM_DQS_WIDTH		(8 / 1),
			.MEM_DQ_WIDTH		(64 / 1),
			.MEM_TRTP			(3),
			.MEM_TRCD			(4)
		) mem_inst (
			.mem_a		(mem_a[16-1:0]),
			.mem_ba		(mem_ba),
			.mem_ck		(mem_ck),
			.mem_ck_n	(mem_ck_n),   
			.mem_cke	(mem_cke),
			.mem_cs_n	(mem_cs_n[2/1*(depth+1)-1:2/1*depth]),
			.mem_ras_n	(mem_ras_n),
			.mem_cas_n	(mem_cas_n),
			.mem_we_n	(mem_we_n),
			.mem_dm		(mem_dm[8/1*(width+1)-1:8/1*width]),
			.mem_dq		(mem_dq[64/1*(width+1)-1:64/1*width]),
			.mem_dqs	(mem_dqs[8/1*(width+1)-1:8/1*width]),
			.mem_dqs_n	(mem_dqs_n[8/1*(width+1)-1:8/1*width]),
			.mem_odt	(mem_odt)
		);
	end
end
endgenerate

initial
begin
	clk <= 1'b0;
	soft_reset_n <= 1'b1;
	reset_n <= 0;
	afi_cal_success_reported <= 0;
	#(10000) reset_n <= 1;
end



// Create the PLL reference clock
always #(10000/2) clk <= ~clk;

always @(posedge test_complete)
begin
	@(posedge clk);
	if (pass)
	begin
		$display("          --- SIMULATION PASSED --- ");
		$finish;
	end
	else
	begin
		$display("          --- SIMULATION FAILED --- ");
		$finish;
	end
end

always @(posedge clk) begin
	if (afi_cal_fail)
		begin
			$display("          --- CALIBRATION FAILED --- ");
			$display("          --- SIMULATION FAILED --- ");
			$finish;
		end
	if (afi_cal_success)
		if (!afi_cal_success_reported) begin
			afi_cal_success_reported <= 1'b1;
			$display("          --- CALIBRATION PASSED --- ");
		end
	end
			


endmodule

