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




`timescale 1 ps / 1 ps
module ddr2_v10_1_0002_alt_ddrx_clock_and_reset #
    ( parameter
        CTL_RESET_SYNC_STAGES      = 4,
        CTL_NUM_RESET_OUTPUT       = 1,
        CTL_HALF_RESET_SYNC_STAGES = 4,
        CTL_HALF_NUM_RESET_OUTPUT  = 1
    )
    (
        ctl_clk,
        ctl_reset_n,
        ctl_half_clk,
        ctl_half_clk_reset_n,
        
        resynced_ctl_reset_n,
        resynced_ctl_half_clk_reset_n
    );

input  ctl_clk;
input  ctl_reset_n;
input  ctl_half_clk;
input  ctl_half_clk_reset_n;

output [CTL_NUM_RESET_OUTPUT      - 1 : 0] resynced_ctl_reset_n;
output [CTL_HALF_NUM_RESET_OUTPUT - 1 : 0] resynced_ctl_half_clk_reset_n;

ddr2_v10_1_0002_alt_ddrx_reset_sync #(
    .RESET_SYNC_STAGES (CTL_RESET_SYNC_STAGES),
    .NUM_RESET_OUTPUT  (CTL_NUM_RESET_OUTPUT)
) reset_sync_inst (
	.reset_n           (ctl_reset_n),
	.clk               (ctl_clk),
	.reset_n_sync      (resynced_ctl_reset_n)
);

ddr2_v10_1_0002_alt_ddrx_reset_sync #(
    .RESET_SYNC_STAGES (CTL_HALF_RESET_SYNC_STAGES),
    .NUM_RESET_OUTPUT  (CTL_HALF_NUM_RESET_OUTPUT)
) half_reset_sync_inst (
	.reset_n           (ctl_half_clk_reset_n),
	.clk               (ctl_half_clk),
	.reset_n_sync      (resynced_ctl_half_clk_reset_n)
);


endmodule


module ddr2_v10_1_0002_alt_ddrx_reset_sync #
    ( parameter
        RESET_SYNC_STAGES = 4,
        NUM_RESET_OUTPUT  = 1
    )
    (
    	reset_n,
    	clk,
    	reset_n_sync
    );

input	reset_n;
input	clk;
output	[NUM_RESET_OUTPUT-1:0] reset_n_sync;

(* altera_attribute = {"-name SYNCHRONIZER_IDENTIFICATION FORCED_IF_ASYNCHRONOUS; -name GLOBAL_SIGNAL OFF"}*) reg	[RESET_SYNC_STAGES+NUM_RESET_OUTPUT-2:0] reset_reg /*synthesis dont_merge */;

generate
genvar i;
	for (i=0; i<RESET_SYNC_STAGES+NUM_RESET_OUTPUT-1; i=i+1)
	begin: reset_stage
		always @(posedge clk or negedge reset_n)
		begin
			if (~reset_n)
				reset_reg[i] <= 1'b0;
			else
			begin
				if (i==0)
					reset_reg[i] <= 1'b1;
				else if (i < RESET_SYNC_STAGES)
					reset_reg[i] <= reset_reg[i-1];
				else
					reset_reg[i] <= reset_reg[RESET_SYNC_STAGES-2];
				
			end
		end
	end
endgenerate

	assign reset_n_sync = reset_reg[RESET_SYNC_STAGES+NUM_RESET_OUTPUT-2:RESET_SYNC_STAGES-1];

endmodule

