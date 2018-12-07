// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


module ddr2_v10_1_0002_altera_avalon_st_jtag_interface (
				 clk,
				 reset_n,
				 source_ready,
				 source_data,
				 source_valid,
				 sink_data,
				 sink_valid,
				 sink_ready,
				 resetrequest
				 );
   input        clk;
   input        reset_n;
   output [7:0] source_data;
   input        source_ready;
   output       source_valid;
   input [7:0]  sink_data;
   input        sink_valid;
   output       sink_ready;
   output       resetrequest;

   parameter PURPOSE = 0; // for discovery of services behind this JTAG Phy - 0
   parameter UPSTREAM_FIFO_SIZE = 0;
   parameter DOWNSTREAM_FIFO_SIZE = 0;
   parameter USE_PLI = 0; // set to 1 enable PLI Simulation Mode 
   parameter PLI_PORT = 50000; // PLI Simulation Port

   wire       clk;
   wire       resetrequest; 
   wire [7:0] source_data;
   wire       source_ready;
   wire       source_valid;
   wire [7:0] sink_data;
   wire       sink_valid;
   wire       sink_ready;

   generate 
     if (USE_PLI == 0)
       begin : normal
         ddr2_v10_1_0002_altera_jtag_dc_streaming #(
           .PURPOSE(PURPOSE),
           .UPSTREAM_FIFO_SIZE(UPSTREAM_FIFO_SIZE),
           .DOWNSTREAM_FIFO_SIZE(DOWNSTREAM_FIFO_SIZE)
         ) jtag_dc_streaming (
           .clk(clk),
           .reset_n(reset_n),
           .source_data(source_data),
           .source_valid(source_valid),
           .sink_data(sink_data),
           .sink_valid(sink_valid),
           .sink_ready(sink_ready),
           .resetrequest(resetrequest)
         );
       end
     else
       begin : pli_mode
         ddr2_v10_1_0002_altera_pli_streaming #(.PURPOSE(PURPOSE), .PLI_PORT(PLI_PORT)) pli_streaming (
           .clk(clk),
           .reset_n(reset_n),
           .source_data(source_data),
           .source_valid(source_valid),
           .source_ready(source_ready),
           .sink_data(sink_data),
           .sink_valid(sink_valid),
           .sink_ready(sink_ready),
           .resetrequest(resetrequest)
         );
       end
   endgenerate
endmodule
