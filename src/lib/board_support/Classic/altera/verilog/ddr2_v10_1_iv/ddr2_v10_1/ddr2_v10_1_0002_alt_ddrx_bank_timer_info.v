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




// altera message_off 10230 10034
module ddr2_v10_1_0002_alt_ddrx_bank_timer_info #
    ( parameter
        ACT_TO_RDWR_WIDTH         = 0,     
        ACT_TO_ACT_WIDTH          = 0,     
        ACT_TO_PCH_WIDTH          = 0,     
        RD_TO_PCH_WIDTH           = 0,     
        WR_TO_PCH_WIDTH           = 0,     
        RD_AP_TO_ACT_WIDTH        = 0,     
        WR_AP_TO_ACT_WIDTH        = 0,     
        PCH_TO_ACT_WIDTH          = 0,     
        
        BANK_TIMER_COUNTER_OFFSET = 3,
        
        MEM_IF_ROW_WIDTH          = 16,
        CLOSE_PAGE_POLICY         = 1
    )
    (
        ctl_clk,
        ctl_reset_n,
        
        act_to_rdwr,
        act_to_act,
        act_to_pch,
        rd_to_pch,
        wr_to_pch,
        rd_ap_to_act,
        wr_ap_to_act,
        pch_to_act,
        
        less_than_x2_act_to_rdwr,
        
        open,
        close,
        read,
        write,
        row_addr,
        
        current_state,
        current_row,
        rdwr_ready,
        act_ready,
        pch_ready
    );

input  ctl_clk;
input  ctl_reset_n;

input  [ACT_TO_RDWR_WIDTH   - 1 : 0] act_to_rdwr;
input  [ACT_TO_ACT_WIDTH    - 1 : 0] act_to_act;
input  [ACT_TO_PCH_WIDTH    - 1 : 0] act_to_pch;
input  [RD_TO_PCH_WIDTH     - 1 : 0] rd_to_pch;
input  [WR_TO_PCH_WIDTH     - 1 : 0] wr_to_pch;
input  [RD_AP_TO_ACT_WIDTH  - 1 : 0] rd_ap_to_act;
input  [WR_AP_TO_ACT_WIDTH  - 1 : 0] wr_ap_to_act;
input  [PCH_TO_ACT_WIDTH    - 1 : 0] pch_to_act;

input  less_than_x2_act_to_rdwr;

input                             open;
input                             close;
input                             read;
input                             write;
input  [MEM_IF_ROW_WIDTH - 1 : 0] row_addr;

output                            current_state; 
output [MEM_IF_ROW_WIDTH - 1 : 0] current_row;
output                            rdwr_ready;
output                            act_ready;
output                            pch_ready;


/*------------------------------------------------------------------------------

    [START] Registers & Wires

------------------------------------------------------------------------------*/
    localparam ACT_COUNTER_WIDTH     = ACT_TO_ACT_WIDTH; 
    localparam GENERAL_COUNTER_WIDTH = 6;                
    
    reg                              current_state;
    reg   [MEM_IF_ROW_WIDTH - 1 : 0] current_row;
    reg                              rdwr_ready;
    reg                              act_ready;
    reg                              pch_ready;
    reg                              doing_read;
    reg                              doing_auto_precharge;
    reg                              doing_precharge;
    
    reg   int_act_to_act_ready;
    reg   int_act_to_pch_ready;
    reg   int_rdwr_to_valid_ready;
    
    reg   [ACT_COUNTER_WIDTH     - 1 : 0] act_counter;
    reg   [GENERAL_COUNTER_WIDTH - 1 : 0] general_counter;
/*------------------------------------------------------------------------------

    [END] Registers & Wires

------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------

    [START] General Logics

------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            act_counter <= 0;
        end
        else
        begin
            if (open)
                act_counter <= BANK_TIMER_COUNTER_OFFSET;
            else if (act_counter != {ACT_COUNTER_WIDTH{1'b1}})
                act_counter <= act_counter + 1'b1;
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            general_counter <= 0;
        end
        else
        begin
            if (read || write || close)
                general_counter <= BANK_TIMER_COUNTER_OFFSET;
            else if (general_counter != {GENERAL_COUNTER_WIDTH{1'b1}})
                general_counter <= general_counter + 1'b1;
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
            doing_read <= 1'b0;
        else
        begin
            if (write)
                doing_read  <= 1'b0;
            else if (read)
                doing_read  <= 1'b1;
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            doing_auto_precharge <= 1'b0;
            doing_precharge      <= 1'b0;
        end
        else
        begin
            if (close)
            begin
                if (read || write)
                begin
                    doing_auto_precharge <= 1'b1;
                    doing_precharge      <= 1'b0;
                end
                else
                begin
                    doing_auto_precharge <= 1'b0;
                    doing_precharge      <= 1'b1;
                end
            end
            else if (open)
            begin
                doing_auto_precharge <= 1'b0;
                doing_precharge      <= 1'b0;
            end
        end
    end
    
    /*------------------------------------------------------------------------------
        Bank Status
    ------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            current_state <= 1'b0;
        end
        else
        begin
            if (open)
                current_state <= 1'b1;
            else if (close)
                current_state <= 1'b0;
        end
    end
    
    /*------------------------------------------------------------------------------
        ACT to RD/WR (tRCD)
    ------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rdwr_ready <= 1'b0;
        end
        else
        begin
            if (close)
                rdwr_ready <= 1'b0;
            else if (open && less_than_x2_act_to_rdwr)
                rdwr_ready <= 1'b1;
            else if (current_state && act_counter >= act_to_rdwr) 
                rdwr_ready <= 1'b1;
            else
                rdwr_ready <= 1'b0;
        end
    end
    
    /*------------------------------------------------------------------------------
        ACT to ACT (tRC)
    ------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_act_to_act_ready <= 1'b1;
        end
        else
        begin
            if (open) 
                int_act_to_act_ready <= 1'b0;
            else if (act_counter >= act_to_act)
                int_act_to_act_ready <= 1'b1;
            else
                int_act_to_act_ready <= 1'b0;
        end
    end
    
    
    
    /*------------------------------------------------------------------------------
        RD/WR(AP) to VALID
    ------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_rdwr_to_valid_ready <= 1'b0;
        end
        else
        begin
            if (close)
                int_rdwr_to_valid_ready <= 1'b0; 
            else
            begin
                if (doing_precharge && general_counter >= pch_to_act) 
                    int_rdwr_to_valid_ready <= 1'b1;
                else if (!doing_precharge)
                begin
                    if (!doing_read &&  doing_auto_precharge && general_counter >= wr_ap_to_act) 
                        int_rdwr_to_valid_ready <= 1'b1;
                    else if ( doing_read &&  doing_auto_precharge && general_counter >= rd_ap_to_act) 
                        int_rdwr_to_valid_ready <= 1'b1;
                    else if (!doing_read && !doing_auto_precharge && general_counter >= wr_to_pch)    
                        int_rdwr_to_valid_ready <= 1'b1;
                    else if ( doing_read && !doing_auto_precharge && general_counter >= rd_to_pch)    
                        int_rdwr_to_valid_ready <= 1'b1;
                    else
                        int_rdwr_to_valid_ready <= 1'b0;
                end
                else
                    int_rdwr_to_valid_ready <= 1'b0;
            end
        end
    end
    
    /*------------------------------------------------------------------------------
        ACT to PCH (tRAS)
    ------------------------------------------------------------------------------*/
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_act_to_pch_ready <= 1'b0;
        end
        else
        begin
            if (open) 
                int_act_to_pch_ready <= 1'b0;
            else if (act_counter >= act_to_pch)
                int_act_to_pch_ready <= 1'b1;
            else
                int_act_to_pch_ready <= 1'b0;
        end
    end
    
    /*------------------------------------------------------------------------------
        ACT/PCH Ready
    ------------------------------------------------------------------------------*/
    always @ (*)
    begin
        act_ready = int_act_to_act_ready & int_rdwr_to_valid_ready;
    end
    
    always @ (*)
    begin
        pch_ready = int_act_to_pch_ready & int_rdwr_to_valid_ready;
    end
    
/*------------------------------------------------------------------------------

    [END] General Logics

------------------------------------------------------------------------------*/









endmodule
