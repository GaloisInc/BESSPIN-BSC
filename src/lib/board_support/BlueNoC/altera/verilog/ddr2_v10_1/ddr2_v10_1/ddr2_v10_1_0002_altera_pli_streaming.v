// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.


module ddr2_v10_1_0002_altera_pli_streaming (

    clk,
    reset_n,

    source_valid,
    source_data,
    source_ready,
    
    sink_valid,
    sink_data,
    sink_ready,

    resetrequest
);
    parameter PLI_PORT = 50000;
    parameter PURPOSE = 0;

    input clk;
    input reset_n;
    
    output reg source_valid;
    output reg [7 : 0] source_data;
    input source_ready;
    
    input sink_valid;
    input [7 : 0] sink_data;
    output reg sink_ready;
    output reg resetrequest;
    
//synthesis translate_off
    
    reg pli_out_valid;
    reg pli_in_ready;
    reg [7 : 0] pli_out_data;
    
    always @(posedge clk or negedge reset_n) begin
        if (!reset_n) begin
            pli_out_valid <= 0;
            pli_out_data <= 'b0;
            pli_in_ready <= 0;
        end
        else begin
            $do_transaction(
                PLI_PORT, 
                pli_out_valid, 
                source_ready, 
                pli_out_data,
                sink_valid,
                pli_in_ready,
                sink_data);
        end
    end

//synthesis translate_on
    wire [7:0] jtag_source_data;
    wire jtag_source_valid;
    wire jtag_sink_ready;
    wire jtag_resetrequest;
    
    ddr2_v10_1_0002_altera_jtag_dc_streaming #(.PURPOSE(PURPOSE)) jtag_dc_streaming (
       .clk(clk),
       .reset_n(reset_n),
       .source_data(jtag_source_data),
       .source_valid(jtag_source_valid),
       .sink_data(sink_data),
       .sink_valid(sink_valid),
       .sink_ready(jtag_sink_ready),
       .resetrequest(jtag_resetrequest)
       );

    always @* begin
       source_valid = jtag_source_valid;
       source_data = jtag_source_data;
       sink_ready = jtag_sink_ready;
       resetrequest = jtag_resetrequest;

       //synthesis translate_off
       source_valid = pli_out_valid;
       source_data = pli_out_data;
       sink_ready = pli_in_ready;
       resetrequest = 0;

       //synthesis translate_on
    end

endmodule


