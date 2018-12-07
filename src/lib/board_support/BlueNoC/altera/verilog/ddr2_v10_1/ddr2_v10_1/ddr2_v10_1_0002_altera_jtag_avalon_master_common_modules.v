// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis translate_off
`timescale 1ns / 1ps
// synthesis translate_on


module ddr2_v10_1_0002_altera_jtag_avalon_master_bytes_to_packets (
		input  wire       clk,               //                clk.clk
		input  wire       reset_n,           //          clk_reset.reset_n
		input  wire       out_ready,         // out_packets_stream.ready
		output wire       out_valid,         //                   .valid
		output wire [7:0] out_data,          //                   .data
		output wire [7:0] out_channel,       //                   .channel
		output wire       out_startofpacket, //                   .startofpacket
		output wire       out_endofpacket,   //                   .endofpacket
		output wire       in_ready,          //    in_bytes_stream.ready
		input  wire       in_valid,          //                   .valid
		input  wire [7:0] in_data            //                   .data
	);

	ddr2_v10_1_0002_altera_avalon_st_bytes_to_packets altera_jtag_avalon_master_bytes_to_packets (
		.clk               (clk),               //                clk.clk
		.reset_n           (reset_n),           //          clk_reset.reset_n
		.out_ready         (out_ready),         // out_packets_stream.ready
		.out_valid         (out_valid),         //                   .valid
		.out_data          (out_data),          //                   .data
		.out_channel       (out_channel),       //                   .channel
		.out_startofpacket (out_startofpacket), //                   .startofpacket
		.out_endofpacket   (out_endofpacket),   //                   .endofpacket
		.in_ready          (in_ready),          //    in_bytes_stream.ready
		.in_valid          (in_valid),          //                   .valid
		.in_data           (in_data)            //                   .data
	);

endmodule

module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_0 (
    
      input              clk,
      input              reset_n,
      output reg         in_ready,
      input              in_valid,
      input      [ 7: 0] in_data,
      input      [ 7: 0] in_channel,
      input              in_startofpacket,
      input              in_endofpacket,
      input              out_ready,
      output reg         out_valid,
      output reg [ 7: 0] out_data,
      output reg         out_startofpacket,
      output reg         out_endofpacket
);


   reg          out_channel;

   always @* begin
      in_ready = out_ready;
      out_valid = in_valid;
      out_data = in_data;
      out_startofpacket = in_startofpacket;
      out_endofpacket = in_endofpacket;

      out_channel = in_channel       ;
      if (in_channel > 0) begin
         out_valid = 0;
      end
   end

endmodule

module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_1 (
    
      input              clk,
      input              reset_n,
      output reg         in_ready,
      input              in_valid,
      input      [ 7: 0] in_data,
      input              in_startofpacket,
      input              in_endofpacket,
      input              out_ready,
      output reg         out_valid,
      output reg [ 7: 0] out_data,
      output reg         out_startofpacket,
      output reg         out_endofpacket,
      output reg [ 7: 0] out_channel
);


   reg          in_channel = 0;

   always @* begin
      in_ready = out_ready;
      out_valid = in_valid;
      out_data = in_data;
      out_startofpacket = in_startofpacket;
      out_endofpacket = in_endofpacket;

      out_channel = 0;
      out_channel        = in_channel;
   end

endmodule

module ddr2_v10_1_0002_altera_jtag_avalon_master_packets_to_bytes (
		input  wire       clk,              //               clk.clk
		input  wire       reset_n,          //         clk_reset.reset_n
		output wire       in_ready,         // in_packets_stream.ready
		input  wire       in_valid,         //                  .valid
		input  wire [7:0] in_data,          //                  .data
		input  wire [7:0] in_channel,       //                  .channel
		input  wire       in_startofpacket, //                  .startofpacket
		input  wire       in_endofpacket,   //                  .endofpacket
		input  wire       out_ready,        //  out_bytes_stream.ready
		output wire       out_valid,        //                  .valid
		output wire [7:0] out_data          //                  .data
	);

	ddr2_v10_1_0002_altera_avalon_st_packets_to_bytes altera_jtag_avalon_master_packets_to_bytes (
		.clk              (clk),              //               clk.clk
		.reset_n          (reset_n),          //         clk_reset.reset_n
		.in_ready         (in_ready),         // in_packets_stream.ready
		.in_valid         (in_valid),         //                  .valid
		.in_data          (in_data),          //                  .data
		.in_channel       (in_channel),       //                  .channel
		.in_startofpacket (in_startofpacket), //                  .startofpacket
		.in_endofpacket   (in_endofpacket),   //                  .endofpacket
		.out_ready        (out_ready),        //  out_bytes_stream.ready
		.out_valid        (out_valid),        //                  .valid
		.out_data         (out_data)          //                  .data
	);

endmodule

module ddr2_v10_1_0002_altera_jtag_avalon_master_packets_to_transactions_converter (
		input  wire        clk,               //                  clk.clk
		input  wire        reset_n,           //            clk_reset.reset_n
		input  wire        out_ready,         //           out_stream.ready
		output wire        out_valid,         //                     .valid
		output wire [7:0]  out_data,          //                     .data
		output wire        out_startofpacket, //                     .startofpacket
		output wire        out_endofpacket,   //                     .endofpacket
		output wire        in_ready,          //            in_stream.ready
		input  wire        in_valid,          //                     .valid
		input  wire [7:0]  in_data,           //                     .data
		input  wire        in_startofpacket,  //                     .startofpacket
		input  wire        in_endofpacket,    //                     .endofpacket
		output wire [31:0] address,           // avalon_master_export.export
		input  wire [31:0] readdata,          //                     .export
		output wire        read,              //                     .export
		output wire        write,             //                     .export
		output wire [31:0] writedata,         //                     .export
		input  wire        waitrequest,       //                     .export
		input  wire        readdatavalid,     //                     .export
		output wire [3:0]  byteenable         //                     .export
	);

	wire in_valid_mux;
	wire [7:0] in_data_mux;
	wire in_startofpacket_mux;
	wire in_endofpacket_mux;
	wire out_ready_mux;

	assign in_valid_mux = in_valid;
	assign in_data_mux = in_data;
	assign in_startofpacket_mux = in_startofpacket;
	assign in_endofpacket_mux = in_endofpacket;
	assign out_ready_mux = out_ready;

	ddr2_v10_1_0002_altera_avalon_packets_to_master #(
		.EXPORT_MASTER_SIGNALS (1)
	) altera_jtag_avalon_master_packets_to_transactions_converter (
		.clk               (clk),               //                  clk.clk
		.reset_n           (reset_n),           //            clk_reset.reset_n
		.out_ready         (out_ready_mux),         //           out_stream.ready
		.out_valid         (out_valid),         //                     .valid
		.out_data          (out_data),          //                     .data
		.out_startofpacket (out_startofpacket), //                     .startofpacket
		.out_endofpacket   (out_endofpacket),   //                     .endofpacket
		.in_ready          (in_ready),          //            in_stream.ready
		.in_valid          (in_valid_mux),          //                     .valid
		.in_data           (in_data_mux),           //                     .data
		.in_startofpacket  (in_startofpacket_mux),  //                     .startofpacket
		.in_endofpacket    (in_endofpacket_mux),    //                     .endofpacket
		.address           (address),           // avalon_master_export.export
		.readdata          (readdata),          //                     .export
		.read              (read),              //                     .export
		.write             (write),             //                     .export
		.writedata         (writedata),         //                     .export
		.waitrequest       (waitrequest),       //                     .export
		.readdatavalid     (readdatavalid),     //                     .export
		.byteenable        (byteenable)         //                     .export
	);

endmodule

module ddr2_v10_1_0002_altera_jtag_avalon_master_sc_fifo (
		input  wire       clk,       //       clk.clk
		input  wire       reset,     // clk_reset.reset
		input  wire [7:0] in_data,   //        in.data
		input  wire       in_valid,  //          .valid
		output wire       in_ready,  //          .ready
		output wire [7:0] out_data,  //       out.data
		output wire       out_valid, //          .valid
		input  wire       out_ready  //          .ready
	);

	ddr2_v10_1_0002_altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (8),
		.FIFO_DEPTH          (64),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (0),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (3),
		.USE_MEMORY_BLOCKS   (1),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) altera_jtag_avalon_master_sc_fifo (
		.clk       (clk),       //       clk.clk
		.reset     (reset),     // clk_reset.reset
		.in_data   (in_data),   //        in.data
		.in_valid  (in_valid),  //          .valid
		.in_ready  (in_ready),  //          .ready
		.out_data  (out_data),  //       out.data
		.out_valid (out_valid), //          .valid
		.out_ready (out_ready)  //          .ready
	);

endmodule


module ddr2_v10_1_0002_altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_arbitrator (
                                                                                altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready,
                                                                                altera_jtag_avalon_master_sc_fifo_out_data,
                                                                                altera_jtag_avalon_master_sc_fifo_out_valid,
                                                                                clk,
                                                                                reset_n,

                                                                                altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data,
                                                                                altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa,
                                                                                altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid
                                                                             )
;

  output  [  7: 0] altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data;
  output           altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa;
  output           altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid;
  input            altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready;
  input   [  7: 0] altera_jtag_avalon_master_sc_fifo_out_data;
  input            altera_jtag_avalon_master_sc_fifo_out_valid;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data;
  wire             altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa;
  wire             altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid;
  assign altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data = altera_jtag_avalon_master_sc_fifo_out_data;

  assign altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa = altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready;

  assign altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid = altera_jtag_avalon_master_sc_fifo_out_valid;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_arbitrator (
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel,
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data,
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket,
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket,
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid,
                                                                                   altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa,
                                                                                   clk,
                                                                                   reset_n,

                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready,
                                                                                   altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n
                                                                                )
;

  output           altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready;
  output           altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n;
  input   [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel;
  input   [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid;
  input            altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n;
  assign altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n = reset_n;

  assign altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready = altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_0_in_arbitrator (
                                                                    altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel,
                                                                    altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data,
                                                                    altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket,
                                                                    altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket,
                                                                    altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_ready,
                                                                    clk,
                                                                    reset_n,

                                                                    altera_jtag_avalon_master_channel_adapter_0_in_channel,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_data,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_endofpacket,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_reset_n,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_startofpacket,
                                                                    altera_jtag_avalon_master_channel_adapter_0_in_valid
                                                                 )
;

  output  [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_channel;
  output  [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_data;
  output           altera_jtag_avalon_master_channel_adapter_0_in_endofpacket;
  output           altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa;
  output           altera_jtag_avalon_master_channel_adapter_0_in_reset_n;
  output           altera_jtag_avalon_master_channel_adapter_0_in_startofpacket;
  output           altera_jtag_avalon_master_channel_adapter_0_in_valid;
  input   [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel;
  input   [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket;
  input            altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid;
  input            altera_jtag_avalon_master_channel_adapter_0_in_ready;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_channel;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_data;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_reset_n;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_valid;
  assign altera_jtag_avalon_master_channel_adapter_0_in_channel = altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel;

  assign altera_jtag_avalon_master_channel_adapter_0_in_data = altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data;

  assign altera_jtag_avalon_master_channel_adapter_0_in_endofpacket = altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket;

  assign altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa = altera_jtag_avalon_master_channel_adapter_0_in_ready;

  assign altera_jtag_avalon_master_channel_adapter_0_in_startofpacket = altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket;

  assign altera_jtag_avalon_master_channel_adapter_0_in_valid = altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid;

  assign altera_jtag_avalon_master_channel_adapter_0_in_reset_n = reset_n;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_0_out_arbitrator (
                                                                     altera_jtag_avalon_master_channel_adapter_0_out_data,
                                                                     altera_jtag_avalon_master_channel_adapter_0_out_endofpacket,
                                                                     altera_jtag_avalon_master_channel_adapter_0_out_startofpacket,
                                                                     altera_jtag_avalon_master_channel_adapter_0_out_valid,
                                                                     altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa,
                                                                     clk,
                                                                     reset_n,

                                                                     altera_jtag_avalon_master_channel_adapter_0_out_ready
                                                                  )
;

  output           altera_jtag_avalon_master_channel_adapter_0_out_ready;
  input   [  7: 0] altera_jtag_avalon_master_channel_adapter_0_out_data;
  input            altera_jtag_avalon_master_channel_adapter_0_out_endofpacket;
  input            altera_jtag_avalon_master_channel_adapter_0_out_startofpacket;
  input            altera_jtag_avalon_master_channel_adapter_0_out_valid;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_channel_adapter_0_out_ready;
  assign altera_jtag_avalon_master_channel_adapter_0_out_ready = altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_1_in_arbitrator (
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_ready,
                                                                    altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data,
                                                                    altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket,
                                                                    altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket,
                                                                    altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid,
                                                                    clk,
                                                                    reset_n,

                                                                    altera_jtag_avalon_master_channel_adapter_1_in_data,
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_endofpacket,
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa,
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_reset_n,
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_startofpacket,
                                                                    altera_jtag_avalon_master_channel_adapter_1_in_valid
                                                                 )
;

  output  [  7: 0] altera_jtag_avalon_master_channel_adapter_1_in_data;
  output           altera_jtag_avalon_master_channel_adapter_1_in_endofpacket;
  output           altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa;
  output           altera_jtag_avalon_master_channel_adapter_1_in_reset_n;
  output           altera_jtag_avalon_master_channel_adapter_1_in_startofpacket;
  output           altera_jtag_avalon_master_channel_adapter_1_in_valid;
  input            altera_jtag_avalon_master_channel_adapter_1_in_ready;
  input   [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_1_in_data;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_reset_n;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_valid;
  assign altera_jtag_avalon_master_channel_adapter_1_in_data = altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data;

  assign altera_jtag_avalon_master_channel_adapter_1_in_endofpacket = altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket;

  assign altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa = altera_jtag_avalon_master_channel_adapter_1_in_ready;

  assign altera_jtag_avalon_master_channel_adapter_1_in_startofpacket = altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket;

  assign altera_jtag_avalon_master_channel_adapter_1_in_valid = altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid;

  assign altera_jtag_avalon_master_channel_adapter_1_in_reset_n = reset_n;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_channel_adapter_1_out_arbitrator (
                                                                     altera_jtag_avalon_master_channel_adapter_1_out_channel,
                                                                     altera_jtag_avalon_master_channel_adapter_1_out_data,
                                                                     altera_jtag_avalon_master_channel_adapter_1_out_endofpacket,
                                                                     altera_jtag_avalon_master_channel_adapter_1_out_startofpacket,
                                                                     altera_jtag_avalon_master_channel_adapter_1_out_valid,
                                                                     altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa,
                                                                     clk,
                                                                     reset_n,

                                                                     altera_jtag_avalon_master_channel_adapter_1_out_ready
                                                                  )
;

  output           altera_jtag_avalon_master_channel_adapter_1_out_ready;
  input   [  7: 0] altera_jtag_avalon_master_channel_adapter_1_out_channel;
  input   [  7: 0] altera_jtag_avalon_master_channel_adapter_1_out_data;
  input            altera_jtag_avalon_master_channel_adapter_1_out_endofpacket;
  input            altera_jtag_avalon_master_channel_adapter_1_out_startofpacket;
  input            altera_jtag_avalon_master_channel_adapter_1_out_valid;
  input            altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_channel_adapter_1_out_ready;
  assign altera_jtag_avalon_master_channel_adapter_1_out_ready = altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa;


endmodule


module ddr2_v10_1_0002_altera_jtag_avalon_master_sc_fifo_out_arbitrator (
                                                           altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa,
                                                           altera_jtag_avalon_master_sc_fifo_out_data,
                                                           altera_jtag_avalon_master_sc_fifo_out_valid,
                                                           clk,
                                                           reset_n,

                                                           altera_jtag_avalon_master_sc_fifo_out_ready
                                                        )
;

  output           altera_jtag_avalon_master_sc_fifo_out_ready;
  input            altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa;
  input   [  7: 0] altera_jtag_avalon_master_sc_fifo_out_data;
  input            altera_jtag_avalon_master_sc_fifo_out_valid;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_sc_fifo_out_ready;
  assign altera_jtag_avalon_master_sc_fifo_out_ready = altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa;


endmodule


module ddr2_v10_1_0002_altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_arbitrator (
                                                                                           altera_jtag_avalon_master_channel_adapter_0_out_data,
                                                                                           altera_jtag_avalon_master_channel_adapter_0_out_endofpacket,
                                                                                           altera_jtag_avalon_master_channel_adapter_0_out_startofpacket,
                                                                                           altera_jtag_avalon_master_channel_adapter_0_out_valid,
                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready,
                                                                                           clk,
                                                                                           reset_n,

                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data,
                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket,
                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa,
                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket,
                                                                                           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid
                                                                                        )
;

  output  [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data;
  output           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket;
  output           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa;
  output           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket;
  output           altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid;
  input   [  7: 0] altera_jtag_avalon_master_channel_adapter_0_out_data;
  input            altera_jtag_avalon_master_channel_adapter_0_out_endofpacket;
  input            altera_jtag_avalon_master_channel_adapter_0_out_startofpacket;
  input            altera_jtag_avalon_master_channel_adapter_0_out_valid;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid;
  assign altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data = altera_jtag_avalon_master_channel_adapter_0_out_data;

  assign altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket = altera_jtag_avalon_master_channel_adapter_0_out_endofpacket;

  assign altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa = altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready;

  assign altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket = altera_jtag_avalon_master_channel_adapter_0_out_startofpacket;

  assign altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid = altera_jtag_avalon_master_channel_adapter_0_out_valid;


endmodule




module ddr2_v10_1_0002_altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_arbitrator (
                                                                                            altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa,
                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data,
                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket,
                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket,
                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid,
                                                                                            clk,
                                                                                            reset_n,

                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready,
                                                                                            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n
                                                                                         )
;

  output           altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready;
  output           altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n;
  input            altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa;
  input   [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket;
  input            altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n;
  assign altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n = reset_n;

  assign altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready = altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa;


endmodule


