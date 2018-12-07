// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.



module ddr2_v10_1_0002_memphy_jtag_master (
		input  wire        clk,                                                                              //          clk.clk
		input  wire        reset_n,                                                                          //    clk_reset.reset_n
		output wire [31:0] address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,     //       master.address
		output wire [3:0]  byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,  //             .byteenable
		output wire        read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,        //             .read
		input  wire [31:0] readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter,      //             .readdata
		input  wire        readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter, //             .readdatavalid
		input  wire        waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter,   //             .waitrequest
		output wire        write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,       //             .write
		output wire [31:0] writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter,   //             .writedata
		output wire        resetrequest_from_the_altera_jtag_avalon_master_jtag_interface                    // master_reset.reset
	);

	ddr2_v10_1_0002_altera_jtag_avalon_master #(
		.USE_PLI  (0),
		.PLI_PORT (50000)
	) memphy_jtag_master_inst (
		.clk                                                                              (clk),                                                                              //          clk.clk
		.reset_n                                                                          (reset_n),                                                                          //    clk_reset.reset_n
		.address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter     (address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),     //       master.address
		.byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter  (byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),  //             .byteenable
		.read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter        (read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),        //             .read
		.readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter      (readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),      //             .readdata
		.readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter (readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter), //             .readdatavalid
		.waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter   (waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),   //             .waitrequest
		.write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter       (write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),       //             .write
		.writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter   (writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),   //             .writedata
		.resetrequest_from_the_altera_jtag_avalon_master_jtag_interface                   (resetrequest_from_the_altera_jtag_avalon_master_jtag_interface)                    // master_reset.reset
	);

endmodule
