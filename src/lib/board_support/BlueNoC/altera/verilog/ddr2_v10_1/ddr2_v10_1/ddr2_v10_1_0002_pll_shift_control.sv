// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_0002_pll_shift_control(
	scanclk,
	reset_n,
	phasestep,
	phaseupdown,
	phasestep_ctrl,
	phaseupdown_ctrl
);

input	scanclk;
input	reset_n;
input	phasestep;
input	phaseupdown;
output	phasestep_ctrl;
output	phaseupdown_ctrl;

typedef enum int unsigned {
	STATE_IDLE,
	STATE_P0,
	STATE_P1,
	STATE_P2,
	STATE_P3
} state_t;

state_t state;


localparam SYNC_STAGES = 2;

reg [SYNC_STAGES-1:0] phasestep_sync;

	always_ff @(posedge scanclk or negedge reset_n)
	begin
		if (~reset_n)
			phasestep_sync <= '0;
		else
			phasestep_sync <= {phasestep_sync[SYNC_STAGES-2:0],phasestep};
	end


	always_ff @(posedge scanclk or negedge reset_n)
	begin
		if (~reset_n)
			state <= STATE_IDLE;
		else
		begin
			case (state)
				STATE_IDLE:
				begin
					if (phasestep_sync)
						state <= STATE_P0;
					else 
						state <= STATE_IDLE;
				end

				STATE_P0:
					state <= STATE_P1;
			
				STATE_P1:
					state <= STATE_P2;

				STATE_P2:
					state <= STATE_P3;

				STATE_P3:
					state <= STATE_IDLE;
			endcase
		end
	end		

	assign phasestep_ctrl = (state == STATE_P0) | (state == STATE_P1);
	assign phaseupdown_ctrl =  phaseupdown ? (state == STATE_P1) | (state == STATE_P2) : 1'b0;


endmodule
