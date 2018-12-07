// ddr2_v11_0_if_csr_m0.v

// This file was auto-generated from altera_jtag_avalon_master_hw.tcl.  If you edit it your changes
// will probably be lost.
// 
// Generated using SOPC Builder version 11.0 157 at 2011.05.23.14:09:57

`timescale 1 ps / 1 ps
module ddr2_v11_0_if_csr_m0 #(
		parameter USE_PLI     = 0,
		parameter PLI_PORT    = 50000,
		parameter FIFO_DEPTHS = 2
	) (
		input  wire        clk_clk,              //          clk.clk
		input  wire        clk_reset_reset,      //    clk_reset.reset
		output wire [31:0] master_address,       //       master.address
		input  wire [31:0] master_readdata,      //             .readdata
		output wire        master_read,          //             .read
		output wire        master_write,         //             .write
		output wire [31:0] master_writedata,     //             .writedata
		input  wire        master_waitrequest,   //             .waitrequest
		input  wire        master_readdatavalid, //             .readdatavalid
		output wire [3:0]  master_byteenable,    //             .byteenable
		output wire        master_reset_reset    // master_reset.reset
	);

	wire        jtag_phy_embedded_in_jtag_master_src_valid; // jtag_phy_embedded_in_jtag_master:source_valid -> timing_adt:in_valid
	wire  [7:0] jtag_phy_embedded_in_jtag_master_src_data;  // jtag_phy_embedded_in_jtag_master:source_data -> timing_adt:in_data
	wire        timing_adt_out_valid;                       // timing_adt:out_valid -> fifo:in_valid
	wire  [7:0] timing_adt_out_data;                        // timing_adt:out_data -> fifo:in_data
	wire        timing_adt_out_ready;                       // fifo:in_ready -> timing_adt:out_ready
	wire        fifo_out_valid;                             // fifo:out_valid -> b2p:in_valid
	wire  [7:0] fifo_out_data;                              // fifo:out_data -> b2p:in_data
	wire        fifo_out_ready;                             // b2p:in_ready -> fifo:out_ready
	wire        b2p_out_packets_stream_endofpacket;         // b2p:out_endofpacket -> b2p_adapter:in_endofpacket
	wire        b2p_out_packets_stream_valid;               // b2p:out_valid -> b2p_adapter:in_valid
	wire        b2p_out_packets_stream_startofpacket;       // b2p:out_startofpacket -> b2p_adapter:in_startofpacket
	wire  [7:0] b2p_out_packets_stream_data;                // b2p:out_data -> b2p_adapter:in_data
	wire        b2p_out_packets_stream_ready;               // b2p_adapter:in_ready -> b2p:out_ready
	wire  [7:0] b2p_out_packets_stream_channel;             // b2p:out_channel -> b2p_adapter:in_channel
	wire        b2p_adapter_out_endofpacket;                // b2p_adapter:out_endofpacket -> transacto:in_endofpacket
	wire        b2p_adapter_out_valid;                      // b2p_adapter:out_valid -> transacto:in_valid
	wire        b2p_adapter_out_startofpacket;              // b2p_adapter:out_startofpacket -> transacto:in_startofpacket
	wire  [7:0] b2p_adapter_out_data;                       // b2p_adapter:out_data -> transacto:in_data
	wire        b2p_adapter_out_ready;                      // transacto:in_ready -> b2p_adapter:out_ready
	wire        transacto_out_stream_endofpacket;           // transacto:out_endofpacket -> p2b_adapter:in_endofpacket
	wire        transacto_out_stream_valid;                 // transacto:out_valid -> p2b_adapter:in_valid
	wire        transacto_out_stream_startofpacket;         // transacto:out_startofpacket -> p2b_adapter:in_startofpacket
	wire  [7:0] transacto_out_stream_data;                  // transacto:out_data -> p2b_adapter:in_data
	wire        transacto_out_stream_ready;                 // p2b_adapter:in_ready -> transacto:out_ready
	wire        p2b_adapter_out_endofpacket;                // p2b_adapter:out_endofpacket -> p2b:in_endofpacket
	wire        p2b_adapter_out_valid;                      // p2b_adapter:out_valid -> p2b:in_valid
	wire        p2b_adapter_out_startofpacket;              // p2b_adapter:out_startofpacket -> p2b:in_startofpacket
	wire  [7:0] p2b_adapter_out_data;                       // p2b_adapter:out_data -> p2b:in_data
	wire  [7:0] p2b_adapter_out_channel;                    // p2b_adapter:out_channel -> p2b:in_channel
	wire        p2b_adapter_out_ready;                      // p2b:in_ready -> p2b_adapter:out_ready
	wire        p2b_out_bytes_stream_valid;                 // p2b:out_valid -> jtag_phy_embedded_in_jtag_master:sink_valid
	wire  [7:0] p2b_out_bytes_stream_data;                  // p2b:out_data -> jtag_phy_embedded_in_jtag_master:sink_data
	wire        p2b_out_bytes_stream_ready;                 // jtag_phy_embedded_in_jtag_master:sink_ready -> p2b:out_ready

	generate
		// If any of the display statements (or deliberately broken
		// instantiations) within this generate block triggers then this module
		// has been instantiated this module with a set of parameters different
		// from those it was generated for.  This will usually result in a
		// non-functioning system.
		if (USE_PLI != 0)
		begin
			initial begin
				$display("Generated module instantiated with wrong parameters");
				$stop;
			end
			instantiated_with_wrong_parameters_error_see_comment_above
					use_pli_check ( .error(1'b1) );
		end
		if (PLI_PORT != 50000)
		begin
			initial begin
				$display("Generated module instantiated with wrong parameters");
				$stop;
			end
			instantiated_with_wrong_parameters_error_see_comment_above
					pli_port_check ( .error(1'b1) );
		end
		if (FIFO_DEPTHS != 2)
		begin
			initial begin
				$display("Generated module instantiated with wrong parameters");
				$stop;
			end
			instantiated_with_wrong_parameters_error_see_comment_above
					fifo_depths_check ( .error(1'b1) );
		end
	endgenerate

	altera_avalon_st_jtag_interface #(
		.PURPOSE              (1),
		.UPSTREAM_FIFO_SIZE   (0),
		.DOWNSTREAM_FIFO_SIZE (0),
		.USE_PLI              (0),
		.PLI_PORT             (50000)
	) jtag_phy_embedded_in_jtag_master (
		.clk          (clk_clk),                                    //        clock.clk
		.reset_n      (~clk_reset_reset),                           //  clock_reset.reset_n
		.source_data  (jtag_phy_embedded_in_jtag_master_src_data),  //          src.data
		.source_valid (jtag_phy_embedded_in_jtag_master_src_valid), //             .valid
		.sink_data    (p2b_out_bytes_stream_data),                  //         sink.data
		.sink_valid   (p2b_out_bytes_stream_valid),                 //             .valid
		.sink_ready   (p2b_out_bytes_stream_ready),                 //             .ready
		.resetrequest (master_reset_reset)                          // resetrequest.reset
	);

	ddr2_v11_0_if_csr_m0_timing_adt timing_adt (
		.clk       (clk_clk),                                    //   clk.clk
		.reset_n   (~clk_reset_reset),                           // reset.reset_n
		.in_valid  (jtag_phy_embedded_in_jtag_master_src_valid), //    in.valid
		.in_data   (jtag_phy_embedded_in_jtag_master_src_data),  //      .data
		.out_valid (timing_adt_out_valid),                       //   out.valid
		.out_data  (timing_adt_out_data),                        //      .data
		.out_ready (timing_adt_out_ready)                        //      .ready
	);

	altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (8),
		.FIFO_DEPTH          (16),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (0),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (3),
		.USE_MEMORY_BLOCKS   (1),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) fifo (
		.clk               (clk_clk),                              //       clk.clk
		.reset             (clk_reset_reset),                      // clk_reset.reset
		.in_data           (timing_adt_out_data),                  //        in.data
		.in_valid          (timing_adt_out_valid),                 //          .valid
		.in_ready          (timing_adt_out_ready),                 //          .ready
		.out_data          (fifo_out_data),                        //       out.data
		.out_valid         (fifo_out_valid),                       //          .valid
		.out_ready         (fifo_out_ready),                       //          .ready
		.csr_address       (2'b00),                                // (terminated)
		.csr_read          (1'b0),                                 // (terminated)
		.csr_write         (1'b0),                                 // (terminated)
		.csr_readdata      (),                                     // (terminated)
		.csr_writedata     (32'b00000000000000000000000000000000), // (terminated)
		.almost_full_data  (),                                     // (terminated)
		.almost_empty_data (),                                     // (terminated)
		.in_startofpacket  (1'b0),                                 // (terminated)
		.in_endofpacket    (1'b0),                                 // (terminated)
		.out_startofpacket (),                                     // (terminated)
		.out_endofpacket   (),                                     // (terminated)
		.in_empty          (1'b0),                                 // (terminated)
		.out_empty         (),                                     // (terminated)
		.in_error          (1'b0),                                 // (terminated)
		.out_error         (),                                     // (terminated)
		.in_channel        (1'b0),                                 // (terminated)
		.out_channel       ()                                      // (terminated)
	);

	altera_avalon_st_bytes_to_packets #(
		.CHANNEL_WIDTH (8),
		.ENCODING      (0)
	) b2p (
		.clk               (clk_clk),                              //                clk.clk
		.reset_n           (~clk_reset_reset),                     //          clk_reset.reset_n
		.out_channel       (b2p_out_packets_stream_channel),       // out_packets_stream.channel
		.out_ready         (b2p_out_packets_stream_ready),         //                   .ready
		.out_valid         (b2p_out_packets_stream_valid),         //                   .valid
		.out_data          (b2p_out_packets_stream_data),          //                   .data
		.out_startofpacket (b2p_out_packets_stream_startofpacket), //                   .startofpacket
		.out_endofpacket   (b2p_out_packets_stream_endofpacket),   //                   .endofpacket
		.in_ready          (fifo_out_ready),                       //    in_bytes_stream.ready
		.in_valid          (fifo_out_valid),                       //                   .valid
		.in_data           (fifo_out_data)                         //                   .data
	);

	altera_avalon_st_packets_to_bytes #(
		.CHANNEL_WIDTH (8),
		.ENCODING      (0)
	) p2b (
		.clk              (clk_clk),                       //               clk.clk
		.reset_n          (~clk_reset_reset),              //         clk_reset.reset_n
		.in_ready         (p2b_adapter_out_ready),         // in_packets_stream.ready
		.in_valid         (p2b_adapter_out_valid),         //                  .valid
		.in_data          (p2b_adapter_out_data),          //                  .data
		.in_channel       (p2b_adapter_out_channel),       //                  .channel
		.in_startofpacket (p2b_adapter_out_startofpacket), //                  .startofpacket
		.in_endofpacket   (p2b_adapter_out_endofpacket),   //                  .endofpacket
		.out_ready        (p2b_out_bytes_stream_ready),    //  out_bytes_stream.ready
		.out_valid        (p2b_out_bytes_stream_valid),    //                  .valid
		.out_data         (p2b_out_bytes_stream_data)      //                  .data
	);

	altera_avalon_packets_to_master #(
		.EXPORT_MASTER_SIGNALS (0),
		.FAST_VER              (0),
		.FIFO_DEPTHS           (2),
		.FIFO_WIDTHU           (1)
	) transacto (
		.clk               (clk_clk),                            //           clk.clk
		.reset_n           (~clk_reset_reset),                   //     clk_reset.reset_n
		.out_ready         (transacto_out_stream_ready),         //    out_stream.ready
		.out_valid         (transacto_out_stream_valid),         //              .valid
		.out_data          (transacto_out_stream_data),          //              .data
		.out_startofpacket (transacto_out_stream_startofpacket), //              .startofpacket
		.out_endofpacket   (transacto_out_stream_endofpacket),   //              .endofpacket
		.in_ready          (b2p_adapter_out_ready),              //     in_stream.ready
		.in_valid          (b2p_adapter_out_valid),              //              .valid
		.in_data           (b2p_adapter_out_data),               //              .data
		.in_startofpacket  (b2p_adapter_out_startofpacket),      //              .startofpacket
		.in_endofpacket    (b2p_adapter_out_endofpacket),        //              .endofpacket
		.address           (master_address),                     // avalon_master.address
		.readdata          (master_readdata),                    //              .readdata
		.read              (master_read),                        //              .read
		.write             (master_write),                       //              .write
		.writedata         (master_writedata),                   //              .writedata
		.waitrequest       (master_waitrequest),                 //              .waitrequest
		.readdatavalid     (master_readdatavalid),               //              .readdatavalid
		.byteenable        (master_byteenable)                   //              .byteenable
	);

	ddr2_v11_0_if_csr_m0_b2p_adapter b2p_adapter (
		.clk               (clk_clk),                              //   clk.clk
		.reset_n           (~clk_reset_reset),                     // reset.reset_n
		.in_ready          (b2p_out_packets_stream_ready),         //    in.ready
		.in_valid          (b2p_out_packets_stream_valid),         //      .valid
		.in_data           (b2p_out_packets_stream_data),          //      .data
		.in_channel        (b2p_out_packets_stream_channel),       //      .channel
		.in_startofpacket  (b2p_out_packets_stream_startofpacket), //      .startofpacket
		.in_endofpacket    (b2p_out_packets_stream_endofpacket),   //      .endofpacket
		.out_ready         (b2p_adapter_out_ready),                //   out.ready
		.out_valid         (b2p_adapter_out_valid),                //      .valid
		.out_data          (b2p_adapter_out_data),                 //      .data
		.out_startofpacket (b2p_adapter_out_startofpacket),        //      .startofpacket
		.out_endofpacket   (b2p_adapter_out_endofpacket)           //      .endofpacket
	);

	ddr2_v11_0_if_csr_m0_p2b_adapter p2b_adapter (
		.clk               (clk_clk),                            //   clk.clk
		.reset_n           (~clk_reset_reset),                   // reset.reset_n
		.in_ready          (transacto_out_stream_ready),         //    in.ready
		.in_valid          (transacto_out_stream_valid),         //      .valid
		.in_data           (transacto_out_stream_data),          //      .data
		.in_startofpacket  (transacto_out_stream_startofpacket), //      .startofpacket
		.in_endofpacket    (transacto_out_stream_endofpacket),   //      .endofpacket
		.out_ready         (p2b_adapter_out_ready),              //   out.ready
		.out_valid         (p2b_adapter_out_valid),              //      .valid
		.out_data          (p2b_adapter_out_data),               //      .data
		.out_startofpacket (p2b_adapter_out_startofpacket),      //      .startofpacket
		.out_endofpacket   (p2b_adapter_out_endofpacket),        //      .endofpacket
		.out_channel       (p2b_adapter_out_channel)             //      .channel
	);

endmodule
