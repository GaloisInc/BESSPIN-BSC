// (C) 2001-2011 Altera Corporation. All rights reserved.
// Your use of Altera Corporation's design tools, logic functions and other 
// software and tools, and its AMPP partner logic functions, and any output 
// files any of the foregoing (including device programming or simulation 
// files), and any associated documentation or information are expressly subject 
// to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable 
// license agreement, including, without limitation, that your use is for the 
// sole purpose of programming logic devices manufactured by Altera and sold by 
// Altera or its authorized distributors.  Please refer to the applicable 
// agreement for further details.



//altera message_off 10036

`timescale 1 ps / 1 ps
module alt_mem_ddrx_ddr3_odt_gen
    # (parameter
        CFG_DWIDTH_RATIO             =   2,
        CFG_PORT_WIDTH_OUTPUT_REGD   =   1,
        CFG_PORT_WIDTH_TCL           =   4,
        CFG_PORT_WIDTH_CAS_WR_LAT    =   4
    )
    (
        ctl_clk,
        ctl_reset_n,
        cfg_tcl,
        cfg_cas_wr_lat,
        cfg_output_regd,
        bg_do_write,
        bg_do_read,
        int_odt_l,
        int_odt_h,
        int_odt_i
    );
    
    localparam  integer CFG_TCL_PIPE_LENGTH =   2**CFG_PORT_WIDTH_TCL;
    //=================================================================================================//
    //        DDR3 ODT timing parameters                                                               //
    //=================================================================================================//
    
    localparam integer    CFG_ODTH8    = 6; //Indicates No. of cycles ODT signal should stay high
    // AL also applies to ODT signal so ODT logic is AL agnostic
    // also regdimm because ODT is registered too
    // ODTLon = CWL + AL - 2
    // ODTLoff = CWL + AL - 2
    
    //=================================================================================================//
    //        input/output declaration                                                                 //
    //=================================================================================================//
    
    input   ctl_clk;
    input   ctl_reset_n;
    input   [CFG_PORT_WIDTH_TCL-1:0]         cfg_tcl;
    input   [CFG_PORT_WIDTH_CAS_WR_LAT-1:0]  cfg_cas_wr_lat;
    input   [CFG_PORT_WIDTH_OUTPUT_REGD-1:0] cfg_output_regd;
    input   bg_do_write;
    input   bg_do_read;
    output  int_odt_l;
    output  int_odt_h;
    output  int_odt_i;
    
    //=================================================================================================//
    //        reg/wire declaration                                                                     //
    //=================================================================================================//
    
    wire    bg_do_write;
    wire    int_do_read;
    reg     do_read_r;
    
    wire [3:0]  diff_unreg; // difference between CL and CWL
    reg  [3:0]  diff;
    
    reg     int_odt_l_int;
    reg     int_odt_l_int_r;
    
    reg     int_odt_h_int;
    reg     int_odt_h_int_r;
    
    reg     int_odt_i_int;
    reg     int_odt_i_int_r;
    
    wire    int_odt_l;
    wire    int_odt_h;
    wire    int_odt_i;
    reg [3:0]   doing_write_count;
    reg [3:0]   doing_read_count;
    
    reg [CFG_TCL_PIPE_LENGTH-1:0]   do_read_pipe;
    
    //=================================================================================================//
    //        Define ODT pulse width during READ operation                                             //
    //=================================================================================================//
    
    //ODTLon/ODTLoff are calculated based on CWL, Below logic is to compensate for that timing during read, Needs to delay ODT signal by cfg_tcl - cfg_cas_wr_lat
    
    assign  diff_unreg  =   cfg_tcl - cfg_cas_wr_lat;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin diff <= 0; end
            else
                begin diff <= diff_unreg; end
        end
    
    assign  int_do_read =   (diff > 1) ? do_read_pipe[diff - 1] : bg_do_read;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin do_read_pipe    <=  0; end
            else
                begin
                    if (bg_do_read)
                        begin do_read_pipe    <=  {do_read_pipe[CFG_TCL_PIPE_LENGTH-2:0],bg_do_read}; end
                    else
                        begin do_read_pipe    <=  {do_read_pipe[CFG_TCL_PIPE_LENGTH-2:0],1'b0}; end
            end
        end
        
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin doing_read_count   <=  0; end
            else
                begin
                    if (int_do_read)
                        begin doing_read_count   <=  1; end
                    else if (doing_read_count >= ((CFG_ODTH8 / (CFG_DWIDTH_RATIO / 2)) - 1))
                        begin doing_read_count   <=  0; end
                    else if (doing_read_count > 0)
                        begin doing_read_count   <=  doing_read_count + 1'b1; end
                end
        end
    
    //=================================================================================================//
    //        Define ODT pulse width during WRITE operation                                            //
    //=================================================================================================//
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin doing_write_count   <=  0; end
            else
                begin
                    if (bg_do_write)
                        begin doing_write_count   <=  1; end
                    else if (doing_write_count >= ((CFG_ODTH8 / (CFG_DWIDTH_RATIO / 2)) - 1))
                        begin doing_write_count   <=  0; end
                    else if (doing_write_count > 0)
                        begin doing_write_count   <=  doing_write_count + 1'b1; end
            end
        end
    
    //=================================================================================================//
    //        ODT signal generation block                                                              //
    //=================================================================================================//
    
    always @ (*)
        begin
            if (bg_do_write || int_do_read)
                begin int_odt_h_int = 1'b1; end
            else if (doing_write_count > 0 || doing_read_count > 0)
                begin int_odt_h_int = 1'b1; end
            else
                begin int_odt_h_int = 1'b0; end
        end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    int_odt_l_int <= 1'b0;
                end
            else
                begin
                    if (bg_do_write || int_do_read)
                        begin int_odt_l_int <= 1'b1; end
                    else if (doing_write_count > 0 || doing_read_count > 0)
                        begin int_odt_l_int <= 1'b1; end
                    else
                        begin int_odt_l_int <= 1'b0; end
                end
        end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    int_odt_i_int <= 1'b0;
                end
            else
                begin
                    if (bg_do_write || int_do_read)
                        begin int_odt_i_int <= 1'b1; end
                    else if (doing_write_count > 1 || doing_read_count > 1)
                        begin int_odt_i_int <= 1'b1; end
                    else
                        begin int_odt_i_int <= 1'b0; end
                end
        end
    
    //Generate registered output
    always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    int_odt_h_int_r <= 1'b0;
                    int_odt_l_int_r <= 1'b0;
                    int_odt_i_int_r <= 1'b0;
                end
            else
                begin
                    int_odt_h_int_r <= int_odt_h_int;
                    int_odt_l_int_r <= int_odt_l_int;
                    int_odt_i_int_r <= int_odt_i_int;
                end
        end
    
    generate
        if (CFG_DWIDTH_RATIO == 2) // full rate
            begin
                assign  int_odt_h   = (cfg_output_regd) ? int_odt_h_int_r : int_odt_h_int;
                assign  int_odt_l   = (cfg_output_regd) ? int_odt_h_int_r : int_odt_h_int;
                assign  int_odt_i   = 1'b0;
            end
        else if (CFG_DWIDTH_RATIO == 4) // half rate
            begin
                assign  int_odt_h   = (cfg_output_regd) ? int_odt_h_int_r : int_odt_h_int;
                assign  int_odt_l   = (cfg_output_regd) ? int_odt_l_int_r : int_odt_l_int;
                assign  int_odt_i   = 1'b0;
            end
        else if (CFG_DWIDTH_RATIO == 8) // quarter rate
            begin
                assign  int_odt_h   = (cfg_output_regd) ? int_odt_h_int_r : int_odt_h_int;
                assign  int_odt_l   = (cfg_output_regd) ? int_odt_l_int_r : int_odt_l_int;
                assign  int_odt_i   = (cfg_output_regd) ? int_odt_i_int_r : int_odt_i_int;
            end
    endgenerate
    
endmodule
