// Copyright (c) 2000-2011 Bluespec, Inc.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// $Revision$
// $Date$

`ifdef BSV_ASSIGNMENT_DELAY
`else
 `define BSV_ASSIGNMENT_DELAY
`endif

// Dual-Ported BRAM (WRITE FIRST) with byte enables
module BRAM2BE(CLKA,
               ENA,
               WEA,
               ADDRA,
               DIA,
               DOA,
               CLKB,
               ENB,
               WEB,
               ADDRB,
               DIB,
               DOB
              );

   parameter                      PIPELINED  = 0;
   parameter                      ADDR_WIDTH = 1;
   parameter                      DATA_WIDTH = 1;
   parameter                      CHUNKSIZE  = 1;
   parameter                      WE_WIDTH   = 1;
   parameter                      MEMSIZE    = 1;

   input                          CLKA;
   input                          ENA;
   input [WE_WIDTH-1:0]           WEA;
   input [ADDR_WIDTH-1:0]         ADDRA;
   input [DATA_WIDTH-1:0]         DIA;
   output [DATA_WIDTH-1:0]        DOA;

   input                          CLKB;
   input                          ENB;
   input [WE_WIDTH-1:0]           WEB;
   input [ADDR_WIDTH-1:0]         ADDRB;
   input [DATA_WIDTH-1:0]         DIB;
   output [DATA_WIDTH-1:0]        DOB;

   wire                           RENA = ENA & !(|WEA);
   wire                           WENA = ENA & |WEA;
   wire                           RENB = ENB & !(|WEB);
   wire                           WENB = ENB & |WEB;
      
   altsyncram
     #(
       .width_a                            (DATA_WIDTH),
       .widthad_a                          (ADDR_WIDTH),
       .numwords_a                         (MEMSIZE),
       .outdata_reg_a                      ((PIPELINED) ? "CLOCK0" : "UNREGISTERED"),
       .address_aclr_a                     ("NONE"),
       .outdata_aclr_a                     ("NONE"),
       .indata_aclr_a                      ("NONE"),
       .wrcontrol_aclr_a                   ("NONE"),
       .byteena_aclr_a                     ("NONE"),
       .width_byteena_a                    (WE_WIDTH),

       .width_b                            (DATA_WIDTH),
       .widthad_b                          (ADDR_WIDTH),
       .numwords_b                         (MEMSIZE),
       .rdcontrol_reg_b                    ("CLOCK1"),//
       .address_reg_b                      ("CLOCK1"),//
       .outdata_reg_b                      ((PIPELINED) ? "CLOCK1" : "UNREGISTERED"),
       .outdata_aclr_b                     ("NONE"),//
       .rdcontrol_aclr_b                   ("NONE"),//
       .indata_reg_b                       ("CLOCK1"),//
       .wrcontrol_wraddress_reg_b          ("CLOCK1"),//
       .byteena_reg_b                      ("CLOCK1"),//
       .indata_aclr_b                      ("NONE"),//
       .wrcontrol_aclr_b                   ("NONE"),//
       .address_aclr_b                     ("NONE"),//
       .byteena_aclr_b                     ("NONE"),//
       .width_byteena_b                    (WE_WIDTH),

       .clock_enable_input_a               ("BYPASS"),
       .clock_enable_output_a              ("BYPASS"),
       .clock_enable_input_b               ("BYPASS"),
       .clock_enable_output_b              ("BYPASS"),

       .clock_enable_core_a                ("USE_INPUT_CLKEN"),//
       .clock_enable_core_b                ("USE_INPUT_CLKEN"),//
       .read_during_write_mode_port_a      ("OLD_DATA"),
       .read_during_write_mode_port_b      ("OLD_DATA"),

       .enable_ecc                         ("FALSE"),//
       .width_eccstatus                    (3),//
       .ecc_pipeline_stage_enabled         ("FALSE"),//

       .operation_mode                     ("BIDIR_DUAL_PORT"),
       .byte_size                          (CHUNKSIZE),//
       .read_during_write_mode_mixed_ports ("DONT_CARE"),//
       .ram_block_type                     ("AUTO"),//
       .init_file                          ("UNUSED"),//
       .init_file_layout                   ("UNUSED"),//
       .maximum_depth                      (MEMSIZE), // number of elements in memory
       .intended_device_family             ("Stratix"),//
       .lpm_hint                           ("ENABLE_RUNTIME_MOD=NO"),
       .lpm_type                           ("altsyncram"),//
       .implement_in_les                   ("OFF"), //
       .power_up_uninitialized             ("FALSE")
       )
   RAM
     (
      .wren_a                              (WENA),
      .rden_a                              (RENB),
      .data_a                              (DIA),
      .address_a                           (ADDRA),
      .clock0                              (CLKA),
      .clocken0                            (1'b1),
      .clocken1                            (1'b1),
      .aclr0                               (1'b0),
      .byteena_a                           (WEA),
      .addressstall_a                      (1'b0),
      .q_a                                 (DOA),

      .wren_b                              (WENB),
      .rden_b                              (RENB),
      .data_b                              (DIB),
      .address_b                           (ADDRB),
      .clock1                              (CLKB),
      .clocken2                            (1'b1),
      .clocken3                            (1'b1),
      .aclr1                               (1'b0),
      .byteena_b                           (WEB),
      .addressstall_b                      (1'b0),
      .q_b                                 (DOB),

      .eccstatus                           ()
      );

endmodule // BRAM2BE
