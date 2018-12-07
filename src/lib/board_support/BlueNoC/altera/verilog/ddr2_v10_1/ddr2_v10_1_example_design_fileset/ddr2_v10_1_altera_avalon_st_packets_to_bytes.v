// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


`timescale 1ns / 100ps
module ddr2_v10_1_altera_avalon_st_packets_to_bytes (

      input              clk,
      input              reset_n,
      output reg         in_ready,
      input              in_valid,
      input      [7: 0]  in_data,
      input      [7: 0]  in_channel,
      input              in_startofpacket,
      input              in_endofpacket,

      input              out_ready,
      output reg         out_valid,
      output reg [7: 0]  out_data
);


   reg  sent_esc, sent_sop, sent_eop;
   reg  sent_channel_char, channel_escaped, sent_channel; 
   reg  [8:0] stored_channel;

   wire channel_changed, channel_needs_esc;
   wire need_sop, need_eop, need_esc, need_channel;


   assign need_esc = (in_data == 8'h7a |
                      in_data == 8'h7b | 
                      in_data == 8'h7c | 
                      in_data == 8'h7d); 
   assign need_eop = (in_endofpacket);
   assign need_sop = (in_startofpacket);
   assign need_channel = (need_sop | channel_changed);

   assign channel_changed = (in_channel != stored_channel);
   assign channel_needs_esc = (in_channel == 8'h7a |
                               in_channel == 8'h7b |
                               in_channel == 8'h7c |
                               in_channel == 8'h7d); 

   always @(posedge clk or negedge reset_n) begin
      if (!reset_n) begin
         sent_esc <= 0; 
         sent_sop <= 0; 
         sent_eop <= 0; 
         sent_channel <= 0; 
         channel_escaped <= 0;
         sent_channel_char <= 0;
         out_data <= 0;
         out_valid <= 0;
      end else begin
         if (out_ready)
           out_valid <= 0;
      
         if ((out_ready | ~out_valid) && in_valid)
           out_valid <= 1; 
           
         if ((out_ready | ~out_valid) && in_valid) begin
            if (need_channel & ~sent_channel) begin
                 if (~sent_channel_char) begin
                    sent_channel_char <= 1;
                    out_data <= 8'h7c;
                 end else if (channel_needs_esc & ~channel_escaped) begin
                    out_data <= 8'h7d;
                    channel_escaped <= 1;
                 end else if (~sent_channel) begin
                    sent_channel <= 1;
                    if (channel_needs_esc) out_data <= in_channel ^ 8'h20;
                    else                   out_data <= in_channel;
                 end
            end else if (need_sop & ~sent_sop) begin
                 sent_sop <= 1;
                 out_data <= 8'h7a;
            end else if (need_eop & ~sent_eop) begin
                 sent_eop <= 1;
                 out_data <= 8'h7b;
            end else if (need_esc & ~sent_esc) begin
                 sent_esc <= 1;
                 out_data <= 8'h7d;
            end else begin
                 if (sent_esc)    out_data <= in_data ^ 8'h20;
                 else             out_data <= in_data;
                 sent_esc <= 0; 
                 sent_sop <= 0; 
                 sent_eop <= 0; 
                 sent_channel <= 0; 
                 channel_escaped <= 0;
                 sent_channel_char <= 0;
            end
         end
      end
   end

   always @(posedge clk or negedge reset_n) begin
      if (!reset_n) begin
         stored_channel <= 9'h1ff;
      end else begin
         if (sent_channel) begin
            stored_channel <= in_channel;
         end
      end
   end

   always @* begin
      
      in_ready = (out_ready | !out_valid) & in_valid & (~need_esc | sent_esc)
                 & (~need_sop | sent_sop) 
                 & (~need_eop | sent_eop)
                 & (~need_channel | sent_channel);
      
   end
endmodule
