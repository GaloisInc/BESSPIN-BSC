// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


`timescale 1ns / 100ps
module ddr2_v10_1_0002_altera_avalon_st_bytes_to_packets (

      input              clk,
      input              reset_n,
      input              out_ready,
      output reg         out_valid,
      output reg [7: 0]  out_data,
      output reg [7: 0]  out_channel,
      output reg         out_startofpacket,
      output reg         out_endofpacket,

      output reg         in_ready,
      input              in_valid,
      input      [7: 0]  in_data
);


   reg  received_esc, received_channel;
   wire escape_char, sop_char, eop_char, channel_char;

   wire [7:0] data_out;


   assign sop_char     = (in_data == 8'h7a);
   assign eop_char     = (in_data == 8'h7b);
   assign channel_char = (in_data == 8'h7c);
   assign escape_char  = (in_data == 8'h7d);

   assign data_out = received_esc ? (in_data ^ 8'h20) : in_data;
   
   always @(posedge clk or negedge reset_n) begin
      if (!reset_n) begin
         received_esc <= 0;
         received_channel <= 0;
         out_startofpacket <= 0;
         out_endofpacket <= 0;
      end else begin
         if (in_valid & in_ready) begin
            if (received_esc) begin
               if (out_ready | received_channel) received_esc <= 0;
            end else begin
               if (escape_char)    received_esc <= 1;
               if (sop_char)       out_startofpacket <= 1;
               if (eop_char)       out_endofpacket <= 1;
               if (channel_char)   received_channel <= 1;
            end
            if (received_channel & (received_esc | (~sop_char & ~eop_char & ~escape_char & ~channel_char))) begin
                  received_channel <= 0;
            end
            if (out_ready  & out_valid) begin
               out_startofpacket <= 0;
               out_endofpacket <= 0;
            end 
         end
      end
   end

   always @(posedge clk or negedge reset_n) begin
      if (!reset_n) begin
         out_channel <= 'h0;
      end else begin
         if ((out_ready | ~out_valid) & in_valid) begin
            if (received_channel & (received_esc | (~sop_char & ~eop_char & ~escape_char & ~channel_char))) begin
               out_channel <= data_out;
            end
         end
      end
   end
   always @* begin
      in_ready = out_ready;

      out_valid = 0;
      if ((out_ready | ~out_valid) && in_valid) begin
         out_valid = 1;
         if (received_esc) begin 
           if (received_channel) out_valid = 0;
         end else begin
            if (sop_char | eop_char | escape_char | channel_char | received_channel) out_valid = 0;
         end
      end

      out_data = data_out; 
   end
endmodule
