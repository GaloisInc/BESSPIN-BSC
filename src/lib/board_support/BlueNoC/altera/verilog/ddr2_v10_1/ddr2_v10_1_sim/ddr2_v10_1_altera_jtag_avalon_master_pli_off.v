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

module ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off (
		input  wire       clk,          //        clock.clk
		input  wire       reset_n,      //  clock_reset.reset_n
		output wire [7:0] source_data,  //          src.data
		output wire       source_valid, //             .valid
		input  wire       source_ready, //             .ready
		input  wire [7:0] sink_data,    //         sink.data
		input  wire       sink_valid,   //             .valid
		output wire       sink_ready,   //             .ready
		output wire       resetrequest  // resetrequest.export
	);

	ddr2_v10_1_altera_avalon_st_jtag_interface #(
		.PURPOSE              (1),
		.UPSTREAM_FIFO_SIZE   (0),
		.DOWNSTREAM_FIFO_SIZE (6),
		.USE_PLI              (0),
		.PLI_PORT             (50000)
	) altera_jtag_avalon_master_jtag_interface_pli_off (
		.clk          (clk),          //        clock.clk
		.reset_n      (reset_n),      //  clock_reset.reset_n
		.source_data  (source_data),  //          src.data
		.source_valid (source_valid), //             .valid
		.source_ready (source_ready), //             .ready
		.sink_data    (sink_data),    //         sink.data
		.sink_valid   (sink_valid),   //             .valid
		.sink_ready   (sink_ready),   //             .ready
		.resetrequest (resetrequest)  // resetrequest.export
	);

endmodule


module ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off_src_arbitrator (
                                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_data,
                                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_valid,
                                                                          altera_jtag_avalon_master_sc_fifo_in_ready_from_sa,
                                                                          clk,
                                                                          reset_n,

                                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_ready,
                                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n
                                                                       )
;

  output           altera_jtag_avalon_master_jtag_interface_pli_off_src_ready;
  output           altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n;
  input   [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_src_data;
  input            altera_jtag_avalon_master_jtag_interface_pli_off_src_valid;
  input            altera_jtag_avalon_master_sc_fifo_in_ready_from_sa;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_jtag_interface_pli_off_src_ready;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n;
  assign altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n = reset_n;

  assign altera_jtag_avalon_master_jtag_interface_pli_off_src_ready = altera_jtag_avalon_master_sc_fifo_in_ready_from_sa;


endmodule



module ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off_sink_arbitrator (
                                                                           altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready,
                                                                           altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data,
                                                                           altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid,
                                                                           clk,
                                                                           reset_n,

                                                                           altera_jtag_avalon_master_jtag_interface_pli_off_sink_data,
                                                                           altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa,
                                                                           altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid
                                                                        )
;

  output  [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_sink_data;
  output           altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa;
  output           altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid;
  input            altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready;
  input   [  7: 0] altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data;
  input            altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_sink_data;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid;
  assign altera_jtag_avalon_master_jtag_interface_pli_off_sink_data = altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data;

  assign altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa = altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready;

  assign altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid = altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid;


endmodule



module ddr2_v10_1_altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_arbitrator_pli_off (
                                                                                 altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa,
                                                                                 altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data,
                                                                                 altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid,
                                                                                 clk,
                                                                                 reset_n,

                                                                                 altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready
                                                                              )
;

  output           altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready;
  input            altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa;
  input   [  7: 0] altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data;
  input            altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid;
  input            clk;
  input            reset_n;

  wire             altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready;
  assign altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready = altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa;


endmodule


module ddr2_v10_1_altera_jtag_avalon_master_sc_fifo_in_arbitrator_pli_off (
                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_data,
                                                          altera_jtag_avalon_master_jtag_interface_pli_off_src_valid,
                                                          altera_jtag_avalon_master_sc_fifo_in_ready,
                                                          clk,
                                                          reset_n,

                                                          altera_jtag_avalon_master_sc_fifo_in_data,
                                                          altera_jtag_avalon_master_sc_fifo_in_ready_from_sa,
                                                          altera_jtag_avalon_master_sc_fifo_in_reset,
                                                          altera_jtag_avalon_master_sc_fifo_in_valid
                                                       )
;

  output  [  7: 0] altera_jtag_avalon_master_sc_fifo_in_data;
  output           altera_jtag_avalon_master_sc_fifo_in_ready_from_sa;
  output           altera_jtag_avalon_master_sc_fifo_in_reset;
  output           altera_jtag_avalon_master_sc_fifo_in_valid;
  input   [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_src_data;
  input            altera_jtag_avalon_master_jtag_interface_pli_off_src_valid;
  input            altera_jtag_avalon_master_sc_fifo_in_ready;
  input            clk;
  input            reset_n;

  wire    [  7: 0] altera_jtag_avalon_master_sc_fifo_in_data;
  wire             altera_jtag_avalon_master_sc_fifo_in_ready_from_sa;
  wire             altera_jtag_avalon_master_sc_fifo_in_reset;
  wire             altera_jtag_avalon_master_sc_fifo_in_valid;
  assign altera_jtag_avalon_master_sc_fifo_in_data = altera_jtag_avalon_master_jtag_interface_pli_off_src_data;

  assign altera_jtag_avalon_master_sc_fifo_in_ready_from_sa = altera_jtag_avalon_master_sc_fifo_in_ready;

  assign altera_jtag_avalon_master_sc_fifo_in_valid = altera_jtag_avalon_master_jtag_interface_pli_off_src_valid;

  assign altera_jtag_avalon_master_sc_fifo_in_reset = ~reset_n;


endmodule


module ddr2_v10_1_altera_jtag_avalon_master_pli_off_reset_clk_domain_synch_module (
                                                                          clk,
                                                                          data_in,
                                                                          reset_n,

                                                                          data_out
                                                                       )
;

  output           data_out;
  input            clk;
  input            data_in;
  input            reset_n;

  reg              data_in_d1 /* synthesis ALTERA_ATTRIBUTE = "{-from \"*\"} CUT=ON ; PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101"  */;
  reg              data_out /* synthesis ALTERA_ATTRIBUTE = "PRESERVE_REGISTER=ON ; SUPPRESS_DA_RULE_INTERNAL=R101; GLOBAL_SIGNAL=OFF"  */;
  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_in_d1 <= 0;
      else 
        data_in_d1 <= data_in;
    end


  always @(posedge clk or negedge reset_n)
    begin
      if (reset_n == 0)
          data_out <= 0;
      else 
        data_out <= data_in_d1;
    end



endmodule


module ddr2_v10_1_altera_jtag_avalon_master_pli_off (
                                            clk,
                                            reset_n,

                                            resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_off,

                                            address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,
                                            writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter
                                         )
;

  output  [ 31: 0] address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  output  [  3: 0] byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  output           read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  output           resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_off;
  output           write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  output  [ 31: 0] writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            clk;
  input   [ 31: 0] readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            reset_n;
  input            waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;

  wire    [ 31: 0] address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire    [  7: 0] altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data;
  wire             altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready;
  wire             altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa;
  wire             altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel;
  wire    [  7: 0] altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket;
  wire             altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_channel;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_0_in_data;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_ready;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_reset_n;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_in_valid;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_0_out_data;
  wire             altera_jtag_avalon_master_channel_adapter_0_out_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_out_ready;
  wire             altera_jtag_avalon_master_channel_adapter_0_out_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_0_out_valid;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_1_in_data;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_ready;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_reset_n;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_in_valid;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_1_out_channel;
  wire    [  7: 0] altera_jtag_avalon_master_channel_adapter_1_out_data;
  wire             altera_jtag_avalon_master_channel_adapter_1_out_endofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_out_ready;
  wire             altera_jtag_avalon_master_channel_adapter_1_out_startofpacket;
  wire             altera_jtag_avalon_master_channel_adapter_1_out_valid;
  wire    [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_sink_data;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid;
  wire    [  7: 0] altera_jtag_avalon_master_jtag_interface_pli_off_src_data;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_src_ready;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n;
  wire             altera_jtag_avalon_master_jtag_interface_pli_off_src_valid;
  wire    [  7: 0] altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_channel;
  wire    [  7: 0] altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_data;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_endofpacket;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_reset_n;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_startofpacket;
  wire             altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data;
  wire             altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready;
  wire             altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket;
  wire             altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid;
  wire    [  7: 0] altera_jtag_avalon_master_sc_fifo_in_data;
  wire             altera_jtag_avalon_master_sc_fifo_in_ready;
  wire             altera_jtag_avalon_master_sc_fifo_in_ready_from_sa;
  wire             altera_jtag_avalon_master_sc_fifo_in_reset;
  wire             altera_jtag_avalon_master_sc_fifo_in_valid;
  wire    [  7: 0] altera_jtag_avalon_master_sc_fifo_out_data;
  wire             altera_jtag_avalon_master_sc_fifo_out_ready;
  wire             altera_jtag_avalon_master_sc_fifo_out_valid;
  wire    [  3: 0] byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire             clk_reset_n;
  wire             read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire             reset_n_sources;
  wire             resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_off;
  wire             write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire    [ 31: 0] writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  ddr2_v10_1_altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_arbitrator the_altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream
    (
      .altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data          (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data),
      .altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready         (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready),
      .altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa),
      .altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid         (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid),
      .altera_jtag_avalon_master_sc_fifo_out_data                               (altera_jtag_avalon_master_sc_fifo_out_data),
      .altera_jtag_avalon_master_sc_fifo_out_valid                              (altera_jtag_avalon_master_sc_fifo_out_valid),
      .clk                                                                      (clk),
      .reset_n                                                                  (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_arbitrator the_altera_jtag_avalon_master_bytes_to_packets_out_packets_stream
    (
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel       (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data          (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket   (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready         (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n       (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid         (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid),
      .altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa                (altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa),
      .clk                                                                         (clk),
      .reset_n                                                                     (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_bytes_to_packets the_altera_jtag_avalon_master_bytes_to_packets
    (
      .clk               (clk),
      .in_data           (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_data),
      .in_ready          (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready),
      .in_valid          (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_valid),
      .out_channel       (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel),
      .out_data          (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data),
      .out_endofpacket   (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket),
      .out_ready         (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_ready),
      .out_startofpacket (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket),
      .out_valid         (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid),
      .reset_n           (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_0_in_arbitrator the_altera_jtag_avalon_master_channel_adapter_0_in
    (
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel       (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_channel),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data          (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_data),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket   (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_endofpacket),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_startofpacket),
      .altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid         (altera_jtag_avalon_master_bytes_to_packets_out_packets_stream_valid),
      .altera_jtag_avalon_master_channel_adapter_0_in_channel                      (altera_jtag_avalon_master_channel_adapter_0_in_channel),
      .altera_jtag_avalon_master_channel_adapter_0_in_data                         (altera_jtag_avalon_master_channel_adapter_0_in_data),
      .altera_jtag_avalon_master_channel_adapter_0_in_endofpacket                  (altera_jtag_avalon_master_channel_adapter_0_in_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_in_ready                        (altera_jtag_avalon_master_channel_adapter_0_in_ready),
      .altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa                (altera_jtag_avalon_master_channel_adapter_0_in_ready_from_sa),
      .altera_jtag_avalon_master_channel_adapter_0_in_reset_n                      (altera_jtag_avalon_master_channel_adapter_0_in_reset_n),
      .altera_jtag_avalon_master_channel_adapter_0_in_startofpacket                (altera_jtag_avalon_master_channel_adapter_0_in_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_in_valid                        (altera_jtag_avalon_master_channel_adapter_0_in_valid),
      .clk                                                                         (clk),
      .reset_n                                                                     (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_0_out_arbitrator the_altera_jtag_avalon_master_channel_adapter_0_out
    (
      .altera_jtag_avalon_master_channel_adapter_0_out_data                                (altera_jtag_avalon_master_channel_adapter_0_out_data),
      .altera_jtag_avalon_master_channel_adapter_0_out_endofpacket                         (altera_jtag_avalon_master_channel_adapter_0_out_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_out_ready                               (altera_jtag_avalon_master_channel_adapter_0_out_ready),
      .altera_jtag_avalon_master_channel_adapter_0_out_startofpacket                       (altera_jtag_avalon_master_channel_adapter_0_out_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_out_valid                               (altera_jtag_avalon_master_channel_adapter_0_out_valid),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa),
      .clk                                                                                 (clk),
      .reset_n                                                                             (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_0 the_altera_jtag_avalon_master_channel_adapter_0
    (
      .clk               (clk),
      .in_channel        (altera_jtag_avalon_master_channel_adapter_0_in_channel),
      .in_data           (altera_jtag_avalon_master_channel_adapter_0_in_data),
      .in_endofpacket    (altera_jtag_avalon_master_channel_adapter_0_in_endofpacket),
      .in_ready          (altera_jtag_avalon_master_channel_adapter_0_in_ready),
      .in_startofpacket  (altera_jtag_avalon_master_channel_adapter_0_in_startofpacket),
      .in_valid          (altera_jtag_avalon_master_channel_adapter_0_in_valid),
      .out_data          (altera_jtag_avalon_master_channel_adapter_0_out_data),
      .out_endofpacket   (altera_jtag_avalon_master_channel_adapter_0_out_endofpacket),
      .out_ready         (altera_jtag_avalon_master_channel_adapter_0_out_ready),
      .out_startofpacket (altera_jtag_avalon_master_channel_adapter_0_out_startofpacket),
      .out_valid         (altera_jtag_avalon_master_channel_adapter_0_out_valid),
      .reset_n           (altera_jtag_avalon_master_channel_adapter_0_in_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_1_in_arbitrator the_altera_jtag_avalon_master_channel_adapter_1_in
    (
      .altera_jtag_avalon_master_channel_adapter_1_in_data                                  (altera_jtag_avalon_master_channel_adapter_1_in_data),
      .altera_jtag_avalon_master_channel_adapter_1_in_endofpacket                           (altera_jtag_avalon_master_channel_adapter_1_in_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_in_ready                                 (altera_jtag_avalon_master_channel_adapter_1_in_ready),
      .altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa                         (altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa),
      .altera_jtag_avalon_master_channel_adapter_1_in_reset_n                               (altera_jtag_avalon_master_channel_adapter_1_in_reset_n),
      .altera_jtag_avalon_master_channel_adapter_1_in_startofpacket                         (altera_jtag_avalon_master_channel_adapter_1_in_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_in_valid                                 (altera_jtag_avalon_master_channel_adapter_1_in_valid),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data          (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket   (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid         (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid),
      .clk                                                                                  (clk),
      .reset_n                                                                              (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_1_out_arbitrator the_altera_jtag_avalon_master_channel_adapter_1_out
    (
      .altera_jtag_avalon_master_channel_adapter_1_out_channel                    (altera_jtag_avalon_master_channel_adapter_1_out_channel),
      .altera_jtag_avalon_master_channel_adapter_1_out_data                       (altera_jtag_avalon_master_channel_adapter_1_out_data),
      .altera_jtag_avalon_master_channel_adapter_1_out_endofpacket                (altera_jtag_avalon_master_channel_adapter_1_out_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_out_ready                      (altera_jtag_avalon_master_channel_adapter_1_out_ready),
      .altera_jtag_avalon_master_channel_adapter_1_out_startofpacket              (altera_jtag_avalon_master_channel_adapter_1_out_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_out_valid                      (altera_jtag_avalon_master_channel_adapter_1_out_valid),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa),
      .clk                                                                        (clk),
      .reset_n                                                                    (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_channel_adapter_1 the_altera_jtag_avalon_master_channel_adapter_1
    (
      .clk               (clk),
      .in_data           (altera_jtag_avalon_master_channel_adapter_1_in_data),
      .in_endofpacket    (altera_jtag_avalon_master_channel_adapter_1_in_endofpacket),
      .in_ready          (altera_jtag_avalon_master_channel_adapter_1_in_ready),
      .in_startofpacket  (altera_jtag_avalon_master_channel_adapter_1_in_startofpacket),
      .in_valid          (altera_jtag_avalon_master_channel_adapter_1_in_valid),
      .out_channel       (altera_jtag_avalon_master_channel_adapter_1_out_channel),
      .out_data          (altera_jtag_avalon_master_channel_adapter_1_out_data),
      .out_endofpacket   (altera_jtag_avalon_master_channel_adapter_1_out_endofpacket),
      .out_ready         (altera_jtag_avalon_master_channel_adapter_1_out_ready),
      .out_startofpacket (altera_jtag_avalon_master_channel_adapter_1_out_startofpacket),
      .out_valid         (altera_jtag_avalon_master_channel_adapter_1_out_valid),
      .reset_n           (altera_jtag_avalon_master_channel_adapter_1_in_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off_sink_arbitrator the_altera_jtag_avalon_master_jtag_interface_pli_off_sink
    (
      .altera_jtag_avalon_master_jtag_interface_pli_off_sink_data          (altera_jtag_avalon_master_jtag_interface_pli_off_sink_data),
      .altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready         (altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready),
      .altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa (altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa),
      .altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid         (altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid),
      .altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data    (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data),
      .altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid   (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid),
      .clk                                                                 (clk),
      .reset_n                                                             (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off_src_arbitrator the_altera_jtag_avalon_master_jtag_interface_pli_off_src
    (
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_data    (altera_jtag_avalon_master_jtag_interface_pli_off_src_data),
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_ready   (altera_jtag_avalon_master_jtag_interface_pli_off_src_ready),
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n (altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n),
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_valid   (altera_jtag_avalon_master_jtag_interface_pli_off_src_valid),
      .altera_jtag_avalon_master_sc_fifo_in_ready_from_sa           (altera_jtag_avalon_master_sc_fifo_in_ready_from_sa),
      .clk                                                          (clk),
      .reset_n                                                      (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_jtag_interface_pli_off the_altera_jtag_avalon_master_jtag_interface_pli_off
    (
      .clk          (clk),
      .reset_n      (altera_jtag_avalon_master_jtag_interface_pli_off_src_reset_n),
      .resetrequest (resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_off),
      .sink_data    (altera_jtag_avalon_master_jtag_interface_pli_off_sink_data),
      .sink_ready   (altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready),
      .sink_valid   (altera_jtag_avalon_master_jtag_interface_pli_off_sink_valid),
      .source_data  (altera_jtag_avalon_master_jtag_interface_pli_off_src_data),
      .source_ready (altera_jtag_avalon_master_jtag_interface_pli_off_src_ready),
      .source_valid (altera_jtag_avalon_master_jtag_interface_pli_off_src_valid)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_arbitrator the_altera_jtag_avalon_master_packets_to_bytes_in_packets_stream
    (
      .altera_jtag_avalon_master_channel_adapter_1_out_channel                    (altera_jtag_avalon_master_channel_adapter_1_out_channel),
      .altera_jtag_avalon_master_channel_adapter_1_out_data                       (altera_jtag_avalon_master_channel_adapter_1_out_data),
      .altera_jtag_avalon_master_channel_adapter_1_out_endofpacket                (altera_jtag_avalon_master_channel_adapter_1_out_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_out_startofpacket              (altera_jtag_avalon_master_channel_adapter_1_out_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_1_out_valid                      (altera_jtag_avalon_master_channel_adapter_1_out_valid),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_channel       (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_channel),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_data          (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_data),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_endofpacket   (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_endofpacket),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready         (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready_from_sa),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_reset_n       (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_reset_n),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_startofpacket (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_startofpacket),
      .altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_valid         (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_valid),
      .clk                                                                        (clk),
      .reset_n                                                                    (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_arbitrator_pli_off the_altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream
    (
      .altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa (altera_jtag_avalon_master_jtag_interface_pli_off_sink_ready_from_sa),
      .altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data    (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data),
      .altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready   (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready),
      .altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid   (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid),
      .clk                                                                 (clk),
      .reset_n                                                             (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_bytes the_altera_jtag_avalon_master_packets_to_bytes
    (
      .clk              (clk),
      .in_channel       (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_channel),
      .in_data          (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_data),
      .in_endofpacket   (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_endofpacket),
      .in_ready         (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_ready),
      .in_startofpacket (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_startofpacket),
      .in_valid         (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_valid),
      .out_data         (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_data),
      .out_ready        (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_ready),
      .out_valid        (altera_jtag_avalon_master_packets_to_bytes_out_bytes_stream_valid),
      .reset_n          (altera_jtag_avalon_master_packets_to_bytes_in_packets_stream_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_arbitrator the_altera_jtag_avalon_master_packets_to_transactions_converter_in_stream
    (
      .altera_jtag_avalon_master_channel_adapter_0_out_data                                (altera_jtag_avalon_master_channel_adapter_0_out_data),
      .altera_jtag_avalon_master_channel_adapter_0_out_endofpacket                         (altera_jtag_avalon_master_channel_adapter_0_out_endofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_out_startofpacket                       (altera_jtag_avalon_master_channel_adapter_0_out_startofpacket),
      .altera_jtag_avalon_master_channel_adapter_0_out_valid                               (altera_jtag_avalon_master_channel_adapter_0_out_valid),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data          (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket   (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready         (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready_from_sa),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid         (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid),
      .clk                                                                                 (clk),
      .reset_n                                                                             (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_arbitrator the_altera_jtag_avalon_master_packets_to_transactions_converter_out_stream
    (
      .altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa                         (altera_jtag_avalon_master_channel_adapter_1_in_ready_from_sa),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data          (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket   (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready         (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n       (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket),
      .altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid         (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid),
      .clk                                                                                  (clk),
      .reset_n                                                                              (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_packets_to_transactions_converter the_altera_jtag_avalon_master_packets_to_transactions_converter
    (
      .address           (address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .byteenable        (byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .clk               (clk),
      .in_data           (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_data),
      .in_endofpacket    (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_endofpacket),
      .in_ready          (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_ready),
      .in_startofpacket  (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_startofpacket),
      .in_valid          (altera_jtag_avalon_master_packets_to_transactions_converter_in_stream_valid),
      .out_data          (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_data),
      .out_endofpacket   (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_endofpacket),
      .out_ready         (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_ready),
      .out_startofpacket (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_startofpacket),
      .out_valid         (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_valid),
      .read              (read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .readdata          (readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .readdatavalid     (readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .reset_n           (altera_jtag_avalon_master_packets_to_transactions_converter_out_stream_reset_n),
      .waitrequest       (waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .write             (write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
      .writedata         (writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter)
    );

  ddr2_v10_1_altera_jtag_avalon_master_sc_fifo_in_arbitrator_pli_off the_altera_jtag_avalon_master_sc_fifo_in
    (
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_data  (altera_jtag_avalon_master_jtag_interface_pli_off_src_data),
      .altera_jtag_avalon_master_jtag_interface_pli_off_src_valid (altera_jtag_avalon_master_jtag_interface_pli_off_src_valid),
      .altera_jtag_avalon_master_sc_fifo_in_data                  (altera_jtag_avalon_master_sc_fifo_in_data),
      .altera_jtag_avalon_master_sc_fifo_in_ready                 (altera_jtag_avalon_master_sc_fifo_in_ready),
      .altera_jtag_avalon_master_sc_fifo_in_ready_from_sa         (altera_jtag_avalon_master_sc_fifo_in_ready_from_sa),
      .altera_jtag_avalon_master_sc_fifo_in_reset                 (altera_jtag_avalon_master_sc_fifo_in_reset),
      .altera_jtag_avalon_master_sc_fifo_in_valid                 (altera_jtag_avalon_master_sc_fifo_in_valid),
      .clk                                                        (clk),
      .reset_n                                                    (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_sc_fifo_out_arbitrator the_altera_jtag_avalon_master_sc_fifo_out
    (
      .altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa (altera_jtag_avalon_master_bytes_to_packets_in_bytes_stream_ready_from_sa),
      .altera_jtag_avalon_master_sc_fifo_out_data                               (altera_jtag_avalon_master_sc_fifo_out_data),
      .altera_jtag_avalon_master_sc_fifo_out_ready                              (altera_jtag_avalon_master_sc_fifo_out_ready),
      .altera_jtag_avalon_master_sc_fifo_out_valid                              (altera_jtag_avalon_master_sc_fifo_out_valid),
      .clk                                                                      (clk),
      .reset_n                                                                  (clk_reset_n)
    );

  ddr2_v10_1_altera_jtag_avalon_master_sc_fifo the_altera_jtag_avalon_master_sc_fifo
    (
      .clk       (clk),
      .in_data   (altera_jtag_avalon_master_sc_fifo_in_data),
      .in_ready  (altera_jtag_avalon_master_sc_fifo_in_ready),
      .in_valid  (altera_jtag_avalon_master_sc_fifo_in_valid),
      .out_data  (altera_jtag_avalon_master_sc_fifo_out_data),
      .out_ready (altera_jtag_avalon_master_sc_fifo_out_ready),
      .out_valid (altera_jtag_avalon_master_sc_fifo_out_valid),
      .reset     (altera_jtag_avalon_master_sc_fifo_in_reset)
    );

  ddr2_v10_1_altera_jtag_avalon_master_pli_off_reset_clk_domain_synch_module altera_jtag_avalon_master_pli_off_reset_clk_domain_synch
    (
      .clk      (clk),
      .data_in  (1'b1),
      .data_out (clk_reset_n),
      .reset_n  (reset_n_sources)
    );

  assign reset_n_sources = ~(~reset_n |
    0);


endmodule

