// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.



module ddr2_v10_1_0002_altera_jtag_control_signal_crosser (
   clk,
   reset_n,
   async_control_signal,
   sense_pos_edge,
   sync_control_signal
);
   input  clk;
   input  reset_n;
   input  async_control_signal;
   input  sense_pos_edge;
   output sync_control_signal;

   parameter SYNC_DEPTH = 3; // number of synchronizer stages for clock crossing

   reg sync_control_signal;

   wire synchronized_raw_signal;
   reg  edge_detector_register;

   altera_std_synchronizer #(.depth(SYNC_DEPTH)) synchronizer (
       .clk(clk),
       .reset_n(reset_n),
       .din(async_control_signal),
       .dout(synchronized_raw_signal)
   );

  always @ (posedge clk or negedge reset_n)
    if (~reset_n)
      edge_detector_register <= 1'b0;
    else
      edge_detector_register <= synchronized_raw_signal;

  always @* begin
    if (sense_pos_edge)
      sync_control_signal <= ~edge_detector_register & synchronized_raw_signal;
    else
      sync_control_signal <= edge_detector_register & ~synchronized_raw_signal;
  end
endmodule

module ddr2_v10_1_0002_altera_jtag_src_crosser (
   sink_clk,
   sink_reset_n,
   sink_valid,
   sink_data,
   src_clk,
   src_reset_n,
   src_valid,
   src_data
);
   parameter WIDTH = 8;
   parameter SYNC_DEPTH = 3; // number of synchronizer stages for clock crossing

   input              sink_clk;
   input              sink_reset_n;
   input              sink_valid;
   input [WIDTH-1:0]  sink_data;
   input              src_clk;
   input              src_reset_n;
   output             src_valid;
   output [WIDTH-1:0] src_data;

   reg              sink_valid_buffer;
   reg [WIDTH-1:0]  sink_data_buffer;

   reg              src_valid;
   reg [WIDTH-1:0]  src_data /* synthesis ALTERA_ATTRIBUTE = "PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101 ; {-from \"*\"} CUT=ON " */;

   wire synchronized_valid;

   ddr2_v10_1_0002_altera_jtag_control_signal_crosser #(
     .SYNC_DEPTH(SYNC_DEPTH)
   ) crosser (
       .clk(src_clk),
       .reset_n(src_reset_n),
       .async_control_signal(sink_valid_buffer),
       .sense_pos_edge(1'b1),
       .sync_control_signal(synchronized_valid)
   );
   always @ (posedge sink_clk or negedge sink_reset_n) begin
      if (~sink_reset_n) begin
         sink_valid_buffer <= 1'b0;
         sink_data_buffer <= 'b0;
      end else begin
         sink_valid_buffer <= sink_valid;
         if (sink_valid) begin
           sink_data_buffer <= sink_data;
         end
      end //end if
   end //always sink_clk

   always @ (posedge src_clk or negedge src_reset_n) begin
      if (~src_reset_n) begin
     src_valid <= 1'b0;
     src_data <= {WIDTH{1'b0}};
      end else begin
     src_valid <= synchronized_valid;
     src_data <= synchronized_valid ? sink_data_buffer : src_data;
      end
   end

endmodule

module ddr2_v10_1_0002_altera_jtag_dc_streaming (
                 clk,
                 reset_n,
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
   output       source_valid;
   input [7:0]  sink_data;
   input        sink_valid;
   output       sink_ready;
   output       resetrequest;

   parameter PURPOSE = 0; // for discovery of services behind this JTAG Phy
   parameter UPSTREAM_FIFO_SIZE = 0;
   parameter DOWNSTREAM_FIFO_SIZE = 0;
   parameter TCK_TO_SYSCLK_SYNC_DEPTH = 8;
   parameter SYSCLK_TO_TCK_SYNC_DEPTH = 3;

   wire       jtag_clock;
   wire       jtag_clock_reset_n; // system reset is synchronized with jtag_clock
   wire [7:0] jtag_source_data;
   wire       jtag_source_valid;
   wire [7:0] jtag_sink_data;
   wire       jtag_sink_valid;
   wire       jtag_sink_ready;

   /* Reset Synchronizer module.
    *
    ddr2_v10_1_0002_altera_avalon_packets_to_master The SLD Node does not provide a reset for the TCK clock domain.
    ddr2_v10_1_0002_altera_avalon_packets_to_master Due to the handshaking nature of the Avalon-ST Clock Crosser,
    ddr2_v10_1_0002_altera_avalon_packets_to_master internal states need to be reset to 0 in order to guarantee proper
    ddr2_v10_1_0002_altera_avalon_packets_to_master functionality throughout resets.
    *
    ddr2_v10_1_0002_altera_avalon_packets_to_master This reset block will asynchronously assert reset, and synchronously
    ddr2_v10_1_0002_altera_avalon_packets_to_master deassert reset for the tck clock domain.
    */
   altera_std_synchronizer #(
       .depth(SYSCLK_TO_TCK_SYNC_DEPTH)
   ) synchronizer (
       .clk(jtag_clock),
       .reset_n(reset_n),
       .din(1'b1),
       .dout(jtag_clock_reset_n)
   );

   ddr2_v10_1_0002_altera_jtag_streaming #(
       .PURPOSE(PURPOSE),
       .UPSTREAM_FIFO_SIZE(UPSTREAM_FIFO_SIZE),
       .DOWNSTREAM_FIFO_SIZE(DOWNSTREAM_FIFO_SIZE)
   ) jtag_streaming (
       .tck(jtag_clock),
       .reset_n(jtag_clock_reset_n),
       .source_data(jtag_source_data),
       .source_valid(jtag_source_valid),
       .sink_data(jtag_sink_data),
       .sink_valid(jtag_sink_valid),
       .sink_ready(jtag_sink_ready),
       .clock_to_sample(clk),
       .reset_to_sample(reset_n),
       .resetrequest(resetrequest)
    );


   ddr2_v10_1_0002_altera_avalon_st_clock_crosser #(
       .SYMBOLS_PER_BEAT(1),
       .BITS_PER_SYMBOL(8),
       .FORWARD_SYNC_DEPTH(SYSCLK_TO_TCK_SYNC_DEPTH),
       .BACKWARD_SYNC_DEPTH(TCK_TO_SYSCLK_SYNC_DEPTH)
   ) sink_crosser (
       .in_clk(clk),
       .in_reset(~reset_n),
       .in_data(sink_data),
       .in_ready(sink_ready),
       .in_valid(sink_valid),
       .out_clk(jtag_clock),
       .out_reset(~jtag_clock_reset_n),
       .out_data(jtag_sink_data),
       .out_ready(jtag_sink_ready),
       .out_valid(jtag_sink_valid)
   );

   ddr2_v10_1_0002_altera_jtag_src_crosser #(
       .SYNC_DEPTH(TCK_TO_SYSCLK_SYNC_DEPTH)
   ) source_crosser (
       .sink_clk(jtag_clock),
       .sink_reset_n(jtag_clock_reset_n),
       .sink_valid(jtag_source_valid),
       .sink_data(jtag_source_data),
       .src_clk(clk),
       .src_reset_n(reset_n),
       .src_valid(source_valid),
       .src_data(source_data)
   );

endmodule
