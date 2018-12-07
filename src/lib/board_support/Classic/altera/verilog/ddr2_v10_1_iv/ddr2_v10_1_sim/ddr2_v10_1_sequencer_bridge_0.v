// sequencer_bridge_0.v

// This file was auto-generated as part of a SOPC Builder generate operation.
// If you edit it your changes will probably be lost.

module ddr2_v10_1_sequencer_bridge_0 (
		input  wire        avlf_clk,         //   clock_sink.clk
		input  wire        avlf_reset_n,     //             .reset_n
		input  wire [15:0] avlf_address,     // avalon_slave.address
		input  wire        avlf_write,       //             .write
		input  wire [31:0] avlf_writedata,   //             .writedata
		input  wire        avlf_read,        //             .read
		output wire [31:0] avlf_readdata,    //             .readdata
		output wire        avlf_waitrequest, //             .waitrequest
		output wire        avlt_clk,         //  conduit_end.export
		output wire        avlt_reset_n,     //             .export
		output wire [15:0] avlt_address,     //             .export
		output wire        avlt_write,       //             .export
		output wire [31:0] avlt_writedata,   //             .export
		output wire        avlt_read,        //             .export
		input  wire [31:0] avlt_readdata,    //             .export
		input  wire        avlt_waitrequest  //             .export
	);

	ddr2_v10_1_sequencer_bridge #(
		.AVL_DATA_WIDTH (32),
		.AVL_ADDR_WIDTH (16)
	) sequencer_bridge_0 (
		.avlf_clk         (avlf_clk),         //   clock_sink.clk
		.avlf_reset_n     (avlf_reset_n),     //             .reset_n
		.avlf_address     (avlf_address),     // avalon_slave.address
		.avlf_write       (avlf_write),       //             .write
		.avlf_writedata   (avlf_writedata),   //             .writedata
		.avlf_read        (avlf_read),        //             .read
		.avlf_readdata    (avlf_readdata),    //             .readdata
		.avlf_waitrequest (avlf_waitrequest), //             .waitrequest
		.avlt_clk         (avlt_clk),         //  conduit_end.export
		.avlt_reset_n     (avlt_reset_n),     //             .export
		.avlt_address     (avlt_address),     //             .export
		.avlt_write       (avlt_write),       //             .export
		.avlt_writedata   (avlt_writedata),   //             .export
		.avlt_read        (avlt_read),        //             .export
		.avlt_readdata    (avlt_readdata),    //             .export
		.avlt_waitrequest (avlt_waitrequest)  //             .export
	);

endmodule
