// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

//////////////////////////////////////////////////////////////////////////////
// This module rounds up the input address to the next burst boundary.
//////////////////////////////////////////////////////////////////////////////

module ddr2_v10_1_burst_boundary_addr_gen(
	burstcount,
	addr_in,
	addr_out
);

//////////////////////////////////////////////////////////////////////////////
// BEGIN PARAMETER SECTION

// Avalon signal widths
parameter ADDR_WIDTH				= "";
parameter BURSTCOUNT_WIDTH			= "";

// Address generator configuration
parameter BURST_ON_BURST_BOUNDARY	= "";

// END PARAMETER SECTION
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
// BEGIN PORT SECTION

input	[BURSTCOUNT_WIDTH-1:0]	burstcount;
input	[ADDR_WIDTH-1:0]		addr_in;
output 	[ADDR_WIDTH-1:0]		addr_out;

// END PORT SECTION
//////////////////////////////////////////////////////////////////////////////

generate
if (BURST_ON_BURST_BOUNDARY == "TRUE")
begin : burst_boundary_true
	// Burst on burst boundary is enabled

	// Round up the address to the next burst boundary in three steps
	// Step 1: decrement the input address
	wire	[ADDR_WIDTH-1:0]	addr_tmp1;
	assign addr_tmp1 = addr_in - 1'b1;

	// Step 2: Set the lower address bits to 1's
	logic	[ADDR_WIDTH-1:0]	addr_tmp2;
	always_comb
	begin
		for (int i = 0; i < ADDR_WIDTH; i++)
		begin
			if (burstcount > 2**i)
				addr_tmp2[i] <= 1'b1;
			else
				addr_tmp2[i] <= addr_tmp1[i];
		end
	end

	// Step 3: add 1 to get the rounded up address
	assign addr_out = addr_tmp2 + 1'b1;
end
else
begin : burst_boundary_false
	// Burst on burst boundary is disabled, leave the address as is
	assign addr_out = addr_in;
end
endgenerate


endmodule

