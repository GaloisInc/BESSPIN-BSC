// rom_bridge_0.v

// This file was auto-generated as part of a SOPC Builder generate operation.
// If you edit it your changes will probably be lost.

module ddr2_v10_1_rom_bridge_0 (
		input  wire        write_clock,      //      write_clock.clk
		output wire [13:0] rom_address,      // export_interface.export
		input  wire [31:0] rom_data,         //                 .export
		output wire        rom_rden,         //                 .export
		input  wire        init,             //                 .export
		input  wire        rom_data_ready,   //                 .export
		output wire        init_busy,        //                 .export
		output wire [13:0] avlm_address,     //    avalon_master.address
		output wire [31:0] avlm_writedata,   //                 .writedata
		output wire        avlm_wren,        //                 .write
		input  wire        avlm_waitrequest  //                 .waitrequest
	);

	ddr2_v10_1_rom_bridge #(
		.AVL_DATA_WIDTH (32),
		.AVL_ADDR_WIDTH (14)
	) rom_bridge_0 (
		.write_clock      (write_clock),      //      write_clock.clk
		.rom_address      (rom_address),      // export_interface.export
		.rom_data         (rom_data),         //                 .export
		.rom_rden         (rom_rden),         //                 .export
		.init             (init),             //                 .export
		.rom_data_ready   (rom_data_ready),   //                 .export
		.init_busy        (init_busy),        //                 .export
		.avlm_address     (avlm_address),     //    avalon_master.address
		.avlm_writedata   (avlm_writedata),   //                 .writedata
		.avlm_wren        (avlm_wren),        //                 .write
		.avlm_waitrequest (avlm_waitrequest)  //                 .waitrequest
	);

endmodule
