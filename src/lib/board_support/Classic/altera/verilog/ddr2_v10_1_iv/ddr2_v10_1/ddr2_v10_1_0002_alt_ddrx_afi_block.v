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




`timescale 1 ps / 1 ps
// altera message_off 10036
module ddr2_v10_1_0002_alt_ddrx_afi_block
    # (parameter
    
        DWIDTH_RATIO        =   2,
        MEM_IF_DQ_WIDTH     =   8,
        MEM_IF_DQS_WIDTH    =   1,
        MEM_IF_DM_WIDTH     =   1,
        CTL_ECC_ENABLED     =   0,
        CTL_OUTPUT_REGD     =   0,
        STATEMACHINE_TYPE   =   "TWO_CK",
        WLAT_BUS_WIDTH      =   5
    
    )
    (
    
        ctl_clk,
        ctl_reset_n,
        afi_wlat,
        
        do_write,
        do_read,
        do_burst_chop,
        rdwr_data_valid,
        
        ecc_wdata,
        ecc_be,
        
        ecc_wdata_fifo_read,
        afi_dqs_burst,
        afi_wdata_valid,
        afi_wdata,
        afi_dm,
        afi_doing_read,
        afi_doing_read_full
    );
    
    input                           ctl_clk;
    input                           ctl_reset_n;
    input   [WLAT_BUS_WIDTH-1:0]    afi_wlat;
    
    localparam  WLAT_PIPE_LENGTH    =   2**WLAT_BUS_WIDTH;
    localparam  WLAT_SUBTRACT       =   (CTL_OUTPUT_REGD == 0) ? 2 : 1;
    
    input   do_write;
    input   do_read;
    input   do_burst_chop;
    input   rdwr_data_valid;
    
    input   [MEM_IF_DQ_WIDTH*DWIDTH_RATIO-1:0]      ecc_wdata;
    input   [(MEM_IF_DQ_WIDTH*DWIDTH_RATIO)/8-1:0]  ecc_be;
    
    output  ecc_wdata_fifo_read;
    output  [MEM_IF_DQS_WIDTH*DWIDTH_RATIO/2-1:0]   afi_dqs_burst;
    output  [MEM_IF_DQS_WIDTH*DWIDTH_RATIO/2-1:0]   afi_wdata_valid;
    output  [MEM_IF_DQ_WIDTH*DWIDTH_RATIO-1:0]      afi_wdata;
    output  [MEM_IF_DM_WIDTH*DWIDTH_RATIO-1:0]      afi_dm;
    output  [MEM_IF_DQS_WIDTH*DWIDTH_RATIO/2-1:0]   afi_doing_read;
    output  [MEM_IF_DQS_WIDTH*DWIDTH_RATIO/2-1:0]   afi_doing_read_full;
    
    reg     [WLAT_BUS_WIDTH-1:0]    afi_wlat_r;
    
    wire    do_write;
    wire    do_read;
    wire    rdwr_data_valid;
    
    reg     ecc_wdata_fifo_read;
    wire    [MEM_IF_DQS_WIDTH*(DWIDTH_RATIO/2)-1:0] afi_dqs_burst;
    wire    [MEM_IF_DQS_WIDTH*(DWIDTH_RATIO/2)-1:0] afi_wdata_valid;
    wire    [MEM_IF_DQ_WIDTH*DWIDTH_RATIO-1:0]      afi_wdata;
    wire    [MEM_IF_DM_WIDTH*DWIDTH_RATIO-1:0]      afi_dm;
    wire    [MEM_IF_DQS_WIDTH*(DWIDTH_RATIO/2)-1:0] afi_doing_read;
    wire    [MEM_IF_DQS_WIDTH*(DWIDTH_RATIO/2)-1:0] afi_doing_read_full;
    
    reg     int_dqs_burst;
    reg     int_dqs_burst_hr;
    reg     int_wdata_valid;
    reg     int_real_wdata_valid;
    reg     int_real_wdata_valid_r;
    wire    real_wdata_valid;
    
    reg     [WLAT_PIPE_LENGTH-1:0]  rdwr_data_valid_pipe;
    reg                             doing_write;
    reg     [1:0]                   doing_write_count;
    reg     [WLAT_PIPE_LENGTH-1:0]  doing_write_pipe;
    reg                             doing_read;
    reg                             doing_read_combi;
    reg                             doing_read_combi_full;
    reg     [1:0]                   doing_read_count;
    reg                             doing_read_r;
    reg                             doing_read_r_full;
    
    generate
        genvar I;
        for (I = 0; I < MEM_IF_DQS_WIDTH*(DWIDTH_RATIO/2); I = I + 1)
            begin : B
                assign afi_wdata_valid[I]   =   int_wdata_valid;
                assign afi_doing_read[I]    =   (CTL_OUTPUT_REGD == 1) ? doing_read_r : doing_read_combi;
                assign afi_doing_read_full[I]    =   (CTL_OUTPUT_REGD == 1) ? doing_read_r_full : doing_read_combi_full;
            end
    endgenerate
    
    generate
        if (DWIDTH_RATIO == 2)
            begin
                genvar I;
                for (I = 0; I < MEM_IF_DQS_WIDTH; I = I + 1)
                    begin : C
                        assign afi_dqs_burst[I] = int_dqs_burst;
                    end
            end
        else
            begin
                genvar I;
                for (I = 0; I < MEM_IF_DQS_WIDTH; I = I + 1)
                    begin : D
                        assign afi_dqs_burst[I + MEM_IF_DQS_WIDTH]  = int_dqs_burst;
                        assign afi_dqs_burst[I]                     = int_dqs_burst_hr;
                    end
            end
    endgenerate
    
    generate
        if ((MEM_IF_DQ_WIDTH*DWIDTH_RATIO)/8 < MEM_IF_DM_WIDTH*DWIDTH_RATIO) 
            begin
                genvar J;
                for (J = 0; J < MEM_IF_DM_WIDTH*DWIDTH_RATIO; J = J + 1)
                    begin : E
                        if (J % 2 == 0) 
                            assign afi_dm[J]    =   ~ecc_be[J/2] | ~real_wdata_valid;
                        else 
                            assign afi_dm[J]    =   ~ecc_be[(J-1)/2] | ~real_wdata_valid;
                    end
            end
        else
            begin
                genvar J;
                for (J = 0; J < MEM_IF_DM_WIDTH*DWIDTH_RATIO; J = J + 1)
                    begin : F
                        assign afi_dm[J]    =   ~ecc_be[J] | ~real_wdata_valid;
                    end
            end
    endgenerate
    
    assign  afi_wdata = ecc_wdata;
    
    assign real_wdata_valid =   (CTL_ECC_ENABLED == 0) ? int_real_wdata_valid : int_real_wdata_valid_r;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                afi_wlat_r  <=  0;
            else
                afi_wlat_r  <=  afi_wlat;
        end
    
    generate
        if (STATEMACHINE_TYPE == "FOUR_CK")
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_read_count   <=  0;
                        else
                            if (do_read && !do_burst_chop)
                                doing_read_count   <=  1;
                            else if (doing_read_count == 2)
                                doing_read_count   <=  0;
                            else if (doing_read_count > 0)
                                doing_read_count   <=  doing_read_count + 1'b1;
                    end
                
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_read         <=  1'b0;
                        else
                            if (do_read)
                                doing_read         <=  1'b1;
                            else if (doing_read_count > 0)
                                doing_read         <=  1'b1;
                            else
                                doing_read         <=  1'b0;
                    end
            end
        else 
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_read    <= 1'b0;
                        else
                            doing_read    <= do_read & ~do_burst_chop;
                    end
            end
    endgenerate
    
    always @(*)
        begin
            if ((do_read || doing_read) && rdwr_data_valid)
                doing_read_combi        <=  1'b1;
            else
                doing_read_combi        <=  1'b0;
        end
    
    always @(*)
        begin
            if (do_read || doing_read)
                doing_read_combi_full   <=  1'b1;
            else
                doing_read_combi_full   <=  1'b0;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    doing_read_r    <=  1'b0;
                    doing_read_r_full   <=  1'b0;
                end
            else
                begin
                    doing_read_r    <=  doing_read_combi;
                    doing_read_r_full   <=  doing_read_combi_full;
                end
        end
    
    generate
        if (STATEMACHINE_TYPE == "FOUR_CK")
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_write_count   <=  0;
                        else
                            if (do_write && !do_burst_chop)
                                doing_write_count   <=  1;
                            else if (doing_write_count == 2)
                                doing_write_count   <=  0;
                            else if (doing_write_count > 0)
                                doing_write_count   <=  doing_write_count + 1'b1;
                    end
                
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_write         <=  1'b0;
                        else
                            if (do_write)
                                doing_write         <=  1'b1;
                            else if (doing_write_count > 0)
                                doing_write         <=  1'b1;
                            else
                                doing_write         <=  1'b0;
                    end
            end
        else 
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            doing_write    <= 1'b0;
                        else
                            doing_write    <= do_write & ~do_burst_chop;
                    end
            end
    endgenerate
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                doing_write_pipe    <=  0;
            else
                if (do_write)
                    doing_write_pipe    <=  {doing_write_pipe[WLAT_PIPE_LENGTH-2:0],do_write};
                else
                    doing_write_pipe    <=  {doing_write_pipe[WLAT_PIPE_LENGTH-2:0],doing_write};
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                rdwr_data_valid_pipe    <=  0;
            else
                rdwr_data_valid_pipe    <=  {rdwr_data_valid_pipe[WLAT_PIPE_LENGTH-2:0],rdwr_data_valid};
        end
    
    generate
        if (CTL_ECC_ENABLED == 0)
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            ecc_wdata_fifo_read <=  1'b0;
                        else
                            if ((CTL_OUTPUT_REGD == 1 && afi_wlat_r == 0) || (CTL_OUTPUT_REGD == 0 && (afi_wlat_r == 0 || afi_wlat_r == 1)))
                                if ((rdwr_data_valid && do_write) || (rdwr_data_valid && doing_write))
                                    ecc_wdata_fifo_read <=  1'b1;
                                else
                                    ecc_wdata_fifo_read <=  1'b0;
                            else
                                if (rdwr_data_valid_pipe[afi_wlat_r-WLAT_SUBTRACT] && doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT])
                                    ecc_wdata_fifo_read <=  1'b1;
                                else
                                    ecc_wdata_fifo_read <=  1'b0;
                    end
            end
        else 
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            ecc_wdata_fifo_read <=  1'b0;
                        else
                            if ((CTL_OUTPUT_REGD == 1 && (afi_wlat_r == 0 || afi_wlat_r == 1)) || (CTL_OUTPUT_REGD == 0 && (afi_wlat_r == 0 || afi_wlat_r == 1 || afi_wlat_r == 2)))
                                begin
                                    if ((rdwr_data_valid && do_write) || (rdwr_data_valid && doing_write))
                                        ecc_wdata_fifo_read <=  1'b1;
                                    else
                                        ecc_wdata_fifo_read <=  1'b0;
                                end
                            else
                                if (rdwr_data_valid_pipe[afi_wlat_r-WLAT_SUBTRACT-1] && doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT-1])
                                    ecc_wdata_fifo_read <=  1'b1;
                                else
                                    ecc_wdata_fifo_read <=  1'b0;
                    end
            end
    endgenerate
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_real_wdata_valid <=  1'b0;
            else
                int_real_wdata_valid <=  ecc_wdata_fifo_read;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_real_wdata_valid_r   <=  1'b0;
            else
                int_real_wdata_valid_r   <=  int_real_wdata_valid;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_wdata_valid   <=  1'b0;
            else
                if (CTL_OUTPUT_REGD == 0 && afi_wlat_r == 0)
                    if (do_write || doing_write_pipe[afi_wlat_r])
                        int_wdata_valid <=  1'b1;
                    else
                        int_wdata_valid <=  1'b0;
                else
                    if (doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT+1])
                        int_wdata_valid <=  1'b1;
                    else
                        int_wdata_valid <=  1'b0;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_dqs_burst   <=  1'b0;
            else
                if ((CTL_OUTPUT_REGD == 1 && afi_wlat_r == 0) || (CTL_OUTPUT_REGD == 0 && (afi_wlat_r == 0 || afi_wlat_r == 1)))
                    if (do_write || doing_write_pipe[0])
                        int_dqs_burst <=  1'b1;
                    else
                        int_dqs_burst <=  1'b0;
                else
                    if (doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT] || doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT+1])
                        int_dqs_burst <=  1'b1;
                    else
                        int_dqs_burst <=  1'b0;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_dqs_burst_hr   <=  1'b0;
            else
                if (doing_write_pipe[afi_wlat_r-WLAT_SUBTRACT+1])
                    int_dqs_burst_hr <=  1'b1;
                else
                    int_dqs_burst_hr <=  1'b0;
        end

endmodule
