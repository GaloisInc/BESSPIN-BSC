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


module ddr2_v10_1_altera_jtag_avalon_master (
                                    clk,
                                    reset_n,

                                    resetrequest_from_the_altera_jtag_avalon_master_jtag_interface,

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
  output           resetrequest_from_the_altera_jtag_avalon_master_jtag_interface;
  output           write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  output  [ 31: 0] writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            clk;
  input   [ 31: 0] readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  input            reset_n;
  input            waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter;

  wire    [ 31: 0] address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire    [  3: 0] byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire             read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire             resetrequest_from_the_altera_jtag_avalon_master_jtag_interface;
  wire             write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;
  wire    [ 31: 0] writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter;

  parameter USE_PLI = 0; // to enable PLI Simulation Mode 
  parameter PLI_PORT = 50000; // PLI Simulation Port

  generate 
    if (USE_PLI == 0)
      begin : normal
        ddr2_v10_1_altera_jtag_avalon_master_pli_off altera_jtag_avalon_master_pli_off_inst
          (
            .address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter     (address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter  (byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .clk                                                                              (clk),
            .read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter        (read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter      (readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter (readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .reset_n                                                                          (reset_n),
            .resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_off           (resetrequest_from_the_altera_jtag_avalon_master_jtag_interface),
            .waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter   (waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter       (write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
            .writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter   (writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter)
          );
       end
     else
       begin : pli_mode
         ddr2_v10_1_altera_jtag_avalon_master_pli_on #(
           .PLI_PORT(PLI_PORT)
         ) altera_jtag_avalon_master_pli_on_inst
           (
             .address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter     (address_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter  (byteenable_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .clk                                                                              (clk),
             .read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter        (read_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter      (readdata_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter (readdatavalid_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .reset_n                                                                          (reset_n),
             .resetrequest_from_the_altera_jtag_avalon_master_jtag_interface_pli_on            (resetrequest_from_the_altera_jtag_avalon_master_jtag_interface),
             .waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter   (waitrequest_to_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter       (write_from_the_altera_jtag_avalon_master_packets_to_transactions_converter),
             .writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter   (writedata_from_the_altera_jtag_avalon_master_packets_to_transactions_converter)
           );
       end
   endgenerate
endmodule

