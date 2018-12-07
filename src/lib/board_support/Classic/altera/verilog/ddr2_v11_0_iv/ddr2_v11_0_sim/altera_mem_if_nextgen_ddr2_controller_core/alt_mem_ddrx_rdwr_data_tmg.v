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

///////////////////////////////////////////////////////////////////////////////
// Title         : DDR controller AFi interfacing block
//
// File          : afi_block.v
//
// Abstract      : AFi block
///////////////////////////////////////////////////////////////////////////////

`timescale 1 ps / 1 ps
module alt_mem_ddrx_rdwr_data_tmg
    # (parameter
        CFG_DWIDTH_RATIO              =    2,
        CFG_MEM_IF_DQ_WIDTH           =    8,
        CFG_MEM_IF_DQS_WIDTH          =    1,
        CFG_MEM_IF_DM_WIDTH           =    1,
        CFG_WLAT_BUS_WIDTH            =    5,
        CFG_DATA_ID_WIDTH             =    10,
        CFG_WDATA_REG                 =    0,
        CFG_ECC_ENC_REG               =    0,
        CFG_AFI_INTF_PHASE_NUM        =    2,
        CFG_PORT_WIDTH_ENABLE_ECC     =    1,
        CFG_PORT_WIDTH_OUTPUT_REGD    =    1
    )
    (
        ctl_clk,
        ctl_reset_n,
        
        // configuration
        cfg_enable_ecc,
        cfg_output_regd,
        cfg_output_regd_for_afi_output,
        
        //Arbiter command input
        bg_doing_read,
        bg_doing_write,
        bg_rdwr_data_valid,        //Required for user burst length lesser than dram burst length
        dataid,
        bg_do_rmw_correct,
        bg_do_rmw_partial,
        
        //Inputs from ECC/WFIFO blocks
        ecc_wdata,
        ecc_dm,
        
        //Input from AFI Block
        afi_wlat,
        
        //Output from AFI Block
        afi_doing_read,            //Use to generate rdata_valid signals in PHY
        afi_doing_read_full,      //AFI 2.0 signal, used by UniPHY for dqs enable control
        ecc_wdata_fifo_read,
        ecc_wdata_fifo_dataid,
        ecc_wdata_fifo_dataid_vector,
        ecc_wdata_fifo_rmw_correct,
        ecc_wdata_fifo_rmw_partial,
        afi_dqs_burst,
        afi_wdata_valid,
        afi_wdata,
        afi_dm
    );
    
    localparam integer  CFG_WLAT_PIPE_LENGTH    =   2**CFG_WLAT_BUS_WIDTH;
    localparam integer  CFG_DATAID_ARRAY_DEPTH  =   2**CFG_DATA_ID_WIDTH;
    integer i;
    
    //=================================================================================================//
    //        input/output declaration                                                                 //
    //=================================================================================================//
    
    input  ctl_clk;
    input  ctl_reset_n;
    
    // configuration
    input  [CFG_PORT_WIDTH_ENABLE_ECC-1:0]  cfg_enable_ecc;
    input  [CFG_PORT_WIDTH_OUTPUT_REGD-1:0] cfg_output_regd;
    output [CFG_PORT_WIDTH_OUTPUT_REGD-1:0] cfg_output_regd_for_afi_output;
    
    //Arbiter command input
    input                          bg_doing_read;
    input                          bg_doing_write;
    input                          bg_rdwr_data_valid;
    input  [CFG_DATA_ID_WIDTH-1:0] dataid;
    
    input  [CFG_AFI_INTF_PHASE_NUM-1:0] bg_do_rmw_correct;
    input  [CFG_AFI_INTF_PHASE_NUM-1:0] bg_do_rmw_partial;
    
    //Inputs from ECC/WFIFO blocks
    input  [CFG_MEM_IF_DQ_WIDTH*CFG_DWIDTH_RATIO-1:0]     ecc_wdata;
    input  [(CFG_MEM_IF_DQ_WIDTH*CFG_DWIDTH_RATIO)/8-1:0] ecc_dm;
    
    //Input from AFI Block
    input  [CFG_WLAT_BUS_WIDTH-1:0] afi_wlat;
    
    //output to AFI block
    output [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_doing_read;
    output [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_doing_read_full;
    output                                                 ecc_wdata_fifo_read;
    output [CFG_DATA_ID_WIDTH-1:0]                         ecc_wdata_fifo_dataid;
    output [CFG_DATAID_ARRAY_DEPTH-1:0]                    ecc_wdata_fifo_dataid_vector;
    output                                                 ecc_wdata_fifo_rmw_correct;
    output                                                 ecc_wdata_fifo_rmw_partial;
    output [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_dqs_burst;
    output [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_wdata_valid;
    output [CFG_MEM_IF_DQ_WIDTH*CFG_DWIDTH_RATIO-1:0]      afi_wdata;
    output [CFG_MEM_IF_DM_WIDTH*CFG_DWIDTH_RATIO-1:0]      afi_dm;
    
    //=================================================================================================//
    //        reg/wire declaration                                                                     //
    //=================================================================================================//
    
    wire                               bg_doing_read;
    wire                               bg_doing_write;
    wire                               bg_rdwr_data_valid;
    wire  [CFG_DATA_ID_WIDTH-1:0]      dataid;
    wire  [CFG_AFI_INTF_PHASE_NUM-1:0] bg_do_rmw_correct;
    wire  [CFG_AFI_INTF_PHASE_NUM-1:0] bg_do_rmw_partial;
    
    wire  [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_doing_read;
    wire  [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_doing_read_full;
    wire                                                  ecc_wdata_fifo_read;
    reg                                                   ecc_wdata_fifo_read_r;
    wire  [CFG_DATA_ID_WIDTH-1:0]                         ecc_wdata_fifo_dataid;
    wire  [CFG_DATAID_ARRAY_DEPTH-1:0]                    ecc_wdata_fifo_dataid_vector;
    wire  [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_dqs_burst;
    wire  [CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2)-1:0] afi_wdata_valid;
    wire  [CFG_MEM_IF_DQ_WIDTH*CFG_DWIDTH_RATIO-1:0]      afi_wdata;
    wire  [CFG_MEM_IF_DM_WIDTH*CFG_DWIDTH_RATIO-1:0]      afi_dm;

    

    //Internal signals
    reg  [CFG_PORT_WIDTH_OUTPUT_REGD-1:0] cfg_output_regd_for_afi_output;
    reg  [CFG_PORT_WIDTH_OUTPUT_REGD-1:0] cfg_output_regd_for_wdata_path;
    
    reg                                   doing_read_combi;
    reg                                   doing_read_full_combi;
    reg                                   doing_read_r;
    reg                                   doing_read_full_r;
    reg   [CFG_WLAT_PIPE_LENGTH-1:0]      doing_write_pipe;
    reg   [CFG_WLAT_PIPE_LENGTH-1:0]      rdwr_data_valid_pipe;
    reg   [CFG_WLAT_PIPE_LENGTH-1:0]      rmw_correct_pipe;
    reg   [CFG_WLAT_PIPE_LENGTH-1:0]      rmw_partial_pipe;
    reg   [CFG_DATA_ID_WIDTH-1:0]         dataid_pipe         [CFG_WLAT_PIPE_LENGTH-1:0];
    reg   [CFG_DATAID_ARRAY_DEPTH-1:0]    dataid_vector_pipe  [CFG_WLAT_PIPE_LENGTH-1:0];
    reg   [CFG_DATAID_ARRAY_DEPTH-1:0]    dataid_vector;
    reg                                   int_dqs_burst;
    reg                                   int_dqs_burst_r;
    reg                                   int_wdata_valid;
    reg                                   int_wdata_valid_r;
    reg                                   int_real_wdata_valid;
    reg                                   int_ecc_wdata_fifo_read;
    reg                                   int_ecc_wdata_fifo_read_r;
    reg   [CFG_DATA_ID_WIDTH-1:0]         int_ecc_wdata_fifo_dataid;
    reg   [CFG_DATA_ID_WIDTH-1:0]         int_ecc_wdata_fifo_dataid_r;
    reg   [CFG_DATAID_ARRAY_DEPTH-1:0]    int_ecc_wdata_fifo_dataid_vector;
    reg   [CFG_DATAID_ARRAY_DEPTH-1:0]    int_ecc_wdata_fifo_dataid_vector_r;
    reg                                   int_ecc_wdata_fifo_rmw_correct;
    reg                                   int_ecc_wdata_fifo_rmw_correct_r;
    reg                                   int_ecc_wdata_fifo_rmw_partial;
    reg                                   int_ecc_wdata_fifo_rmw_partial_r;
    
    wire                                  int_do_rmw_correct;
    wire                                  int_do_rmw_partial;
    
    // DQS burst logic for half rate design
    reg                                   int_dqs_burst_half_rate;
    reg                                   int_dqs_burst_half_rate_r;
    
    reg                                   afi_wlat_eq_0;
    reg [CFG_WLAT_BUS_WIDTH   - 1 : 0]    afi_wlat_minus_1;
    reg [CFG_WLAT_BUS_WIDTH   - 1 : 0]    afi_wlat_minus_2;
    reg [CFG_WLAT_BUS_WIDTH   - 1 : 0]    afi_wlat_minus_3;
    reg                                   doing_write_pipe_eq_afi_wlat_minus_0;
    reg                                   doing_write_pipe_eq_afi_wlat_minus_1;
    reg                                   doing_write_pipe_eq_afi_wlat_minus_2;
    reg                                   rdwr_data_valid_pipe_eq_afi_wlat_minus_1;
    reg                                   rdwr_data_valid_pipe_eq_afi_wlat_minus_2;
    reg [CFG_DATA_ID_WIDTH-1:0]           dataid_pipe_eq_afi_wlat_minus_1;
    reg [CFG_DATA_ID_WIDTH-1:0]           dataid_pipe_eq_afi_wlat_minus_2;
    reg [CFG_DATAID_ARRAY_DEPTH-1:0]      dataid_vector_pipe_eq_afi_wlat_minus_1;
    reg [CFG_DATAID_ARRAY_DEPTH-1:0]      dataid_vector_pipe_eq_afi_wlat_minus_2;
    reg                                   rmw_correct_pipe_eq_afi_wlat_minus_1;
    reg                                   rmw_correct_pipe_eq_afi_wlat_minus_2;
    reg                                   rmw_partial_pipe_eq_afi_wlat_minus_1;
    reg                                   rmw_partial_pipe_eq_afi_wlat_minus_2;
    
    reg                                   doing_write_pipe_eq_afi_wlat_minus_x;
    reg                                   rdwr_data_valid_pipe_eq_afi_wlat_minus_x;
    reg [CFG_DATA_ID_WIDTH-1:0]           dataid_pipe_eq_afi_wlat_minus_x;
    reg [CFG_DATAID_ARRAY_DEPTH-1:0]      dataid_vector_pipe_eq_afi_wlat_minus_x;
    reg                                   rmw_correct_pipe_eq_afi_wlat_minus_x;
    reg                                   rmw_partial_pipe_eq_afi_wlat_minus_x;
    
    //=================================================================================================//
    //            Internal cfg_output_regd                                                             //
    //=================================================================================================//
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            cfg_output_regd_for_afi_output <= 1'b0;
            cfg_output_regd_for_wdata_path <= 1'b0;
        end
        else
        begin
            if (CFG_WDATA_REG || CFG_ECC_ENC_REG)
            begin
                if (afi_wlat <= 1)
                begin
                    // We enable output_regd for signals going to PHY
                    // because we need to fetch data 2 clock cycles earlier
                    cfg_output_regd_for_afi_output <= 1'b1;
                    
                    // We disable output_regd for signals going to wdata_path
                    // because we need to fecth data 2 clock cycles earlier
                    cfg_output_regd_for_wdata_path <= 1'b0;
                end
                else
                begin
                    cfg_output_regd_for_afi_output <= cfg_output_regd;
                    cfg_output_regd_for_wdata_path <= cfg_output_regd;
                end
            end
            else
            begin
                cfg_output_regd_for_afi_output <= cfg_output_regd;
                cfg_output_regd_for_wdata_path <= cfg_output_regd;
            end
        end
    end
    
    //=================================================================================================//
    //            Read timing logic                                                                    //
    //=================================================================================================//
    
    //*************************************************************************************************//
    //            afi_doing_read generation logic                                                      //
    //*************************************************************************************************//
    always @(*)
    begin
        if (bg_doing_read && bg_rdwr_data_valid)
        begin
            doing_read_combi = 1'b1;
        end
        else
        begin
            doing_read_combi = 1'b0;
        end
        
        doing_read_full_combi = bg_doing_read;
    end
    
    // registered output
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            doing_read_r      <= 1'b0;
            doing_read_full_r <= 1'b0;
        end
        else
        begin
            doing_read_r      <= doing_read_combi;
            doing_read_full_r <= doing_read_full_combi;
        end
    end
    
    generate
        genvar I;
        for (I = 0; I < CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2); I = I + 1)
            begin : B
                assign afi_doing_read       [I] = (cfg_output_regd_for_afi_output) ? doing_read_r      : doing_read_combi;
                assign afi_doing_read_full  [I] = (cfg_output_regd_for_afi_output) ? doing_read_full_r : doing_read_full_combi;
            end
    endgenerate
    
    //=================================================================================================//
    //            Write timing logic                                   //
    //=================================================================================================//
    // content of pipe shows how long dqs should toggle, used to generate dqs_burst
    // Content of pipe is also used to generate wdata_valid signal
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            doing_write_pipe    <=  0;
        end
        else
        begin
            doing_write_pipe    <=  {doing_write_pipe[CFG_WLAT_PIPE_LENGTH -2 :0],bg_doing_write};
        end
    end
    
    // content of pipe shows how much data should be read out of the write data FIFO
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rdwr_data_valid_pipe    <=  0;
        end
        else
        begin
            rdwr_data_valid_pipe    <=  {rdwr_data_valid_pipe[CFG_WLAT_PIPE_LENGTH - 2:0],bg_rdwr_data_valid};
        end
    end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            for (i=0; i<CFG_WLAT_PIPE_LENGTH; i=i+1)
            begin
                 dataid_pipe[i]    <=  0;
            end
        end
        else
        begin
            dataid_pipe[0] <= dataid;
            
            for (i=1; i<CFG_WLAT_PIPE_LENGTH; i=i+1)
            begin
                dataid_pipe[i]    <=  dataid_pipe[i-1];
            end
        end
    end
    
    //pre-calculated dataid comparison logic
    always @ (*)
    begin
        for (i=0; i<(CFG_DATAID_ARRAY_DEPTH); i=i+1)
        begin
            if (dataid == i)
            begin
                dataid_vector[i] = 1'b1;
            end
            else
            begin
                dataid_vector[i] = 1'b0;
            end
        end
    end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            dataid_vector_pipe[0] <= 0;
            
            for (i=1; i<CFG_WLAT_PIPE_LENGTH; i=i+1)
            begin
                dataid_vector_pipe[i] <= 0;
            end
        end
        else
        begin
            dataid_vector_pipe[0] <= dataid_vector;
            
            for (i=1; i<CFG_WLAT_PIPE_LENGTH; i=i+1)
            begin
                dataid_vector_pipe[i] <= dataid_vector_pipe[i-1];
            end
        end
    end
    
    assign int_do_rmw_correct = |bg_do_rmw_correct;
    assign int_do_rmw_partial = |bg_do_rmw_partial;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rmw_correct_pipe    <=  0;
        end
        else
        begin
            rmw_correct_pipe    <=  {rmw_correct_pipe[CFG_WLAT_PIPE_LENGTH - 2:0],int_do_rmw_correct};
        end
    end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rmw_partial_pipe    <=  0;
        end
        else
        begin
            rmw_partial_pipe    <=  {rmw_partial_pipe[CFG_WLAT_PIPE_LENGTH - 2:0],int_do_rmw_partial};
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            afi_wlat_eq_0 <= 1'b0;
            afi_wlat_minus_1    <= {CFG_WLAT_BUS_WIDTH{1'b0}};
            afi_wlat_minus_2    <= {CFG_WLAT_BUS_WIDTH{1'b0}};
            afi_wlat_minus_3    <= {CFG_WLAT_BUS_WIDTH{1'b0}};
        end
        else
        begin
            if (afi_wlat == 0)
            begin
                afi_wlat_eq_0 <= 1'b1;
            end
            else
            begin
                afi_wlat_eq_0 <= 1'b0;
            end

            afi_wlat_minus_1 <= afi_wlat - 1;
            afi_wlat_minus_2 <= afi_wlat - 2;
            afi_wlat_minus_3 <= afi_wlat - 3;
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b0;
            doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b0;
            doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b0;
        end
        else
        begin
            if (afi_wlat == 0)
            begin
                doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b0;
                doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b0;
            end
            else if (afi_wlat == 1)
            begin
                if (doing_write_pipe[0])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b0;
                end
                
                if (bg_doing_write)
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (bg_doing_write) // we must disable int_cfg_output_regd when (afi_wlat < 2)
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else if (afi_wlat == 2)
            begin
                if (doing_write_pipe[1])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b0;
                end
                
                if (doing_write_pipe[0])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (bg_doing_write)
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else
            begin
                if (doing_write_pipe[afi_wlat_minus_1])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_0 <= 1'b0;
                end
                
                if (doing_write_pipe[afi_wlat_minus_2])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (doing_write_pipe[afi_wlat_minus_3])
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    doing_write_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b0;
            rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b0;
        end
        else
        begin
            if (afi_wlat == 0)
            begin
                rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b0;
            end
            else if (afi_wlat == 1)
            begin
                if (bg_rdwr_data_valid)
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (bg_rdwr_data_valid) // we must disable int_cfg_output_regd when (afi_wlat < 2)
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else if (afi_wlat == 2)
            begin
                if (rdwr_data_valid_pipe[0])
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (bg_rdwr_data_valid)
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else
            begin
                if (rdwr_data_valid_pipe[afi_wlat_minus_2])
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (rdwr_data_valid_pipe[afi_wlat_minus_3])
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rdwr_data_valid_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            dataid_pipe_eq_afi_wlat_minus_1        <= 0;
            dataid_pipe_eq_afi_wlat_minus_2        <= 0;
            dataid_vector_pipe_eq_afi_wlat_minus_1 <= 0;
            dataid_vector_pipe_eq_afi_wlat_minus_2 <= 0;
        end
        else
        begin
            if (afi_wlat == 0)
            begin
                dataid_pipe_eq_afi_wlat_minus_1        <= 0;
                dataid_pipe_eq_afi_wlat_minus_2        <= 0;
                dataid_vector_pipe_eq_afi_wlat_minus_1 <= 0;
                dataid_vector_pipe_eq_afi_wlat_minus_2 <= 0;
            end
            else if (afi_wlat == 1)
            begin
                dataid_pipe_eq_afi_wlat_minus_1        <= dataid;
                dataid_pipe_eq_afi_wlat_minus_2        <= dataid;          // we must disable int_cfg_output_regd when (afi_wlat < 2)
                dataid_vector_pipe_eq_afi_wlat_minus_1 <= dataid_vector;
                dataid_vector_pipe_eq_afi_wlat_minus_2 <= dataid_vector;   // we must disable int_cfg_output_regd when (afi_wlat < 2)
            end
            else if (afi_wlat == 2)
            begin
                dataid_pipe_eq_afi_wlat_minus_1        <= dataid_pipe       [0];
                dataid_pipe_eq_afi_wlat_minus_2        <= dataid;
                dataid_vector_pipe_eq_afi_wlat_minus_1 <= dataid_vector_pipe[0];
                dataid_vector_pipe_eq_afi_wlat_minus_2 <= dataid_vector;
            end
            else
            begin
                dataid_pipe_eq_afi_wlat_minus_1        <= dataid_pipe       [afi_wlat_minus_2];
                dataid_pipe_eq_afi_wlat_minus_2        <= dataid_pipe       [afi_wlat_minus_3];
                dataid_vector_pipe_eq_afi_wlat_minus_1 <= dataid_vector_pipe[afi_wlat_minus_2];
                dataid_vector_pipe_eq_afi_wlat_minus_2 <= dataid_vector_pipe[afi_wlat_minus_3];
            end
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b0;
            rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b0;
            rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b0;
            rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b0;
        end
        else
        begin
            if (afi_wlat == 0)
            begin
                rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b0;
            end
            else if (afi_wlat == 1)
            begin
                if (int_do_rmw_correct)
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (int_do_rmw_partial)
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (int_do_rmw_correct) // we must disable int_cfg_output_regd when (afi_wlat < 2)
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
                
                if (int_do_rmw_partial) // we must disable int_cfg_output_regd when (afi_wlat < 2)
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else if (afi_wlat == 2)
            begin
                if (rmw_correct_pipe[0])
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (rmw_partial_pipe[0])
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (int_do_rmw_correct)
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
                
                if (int_do_rmw_partial)
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
            else
            begin
                if (rmw_correct_pipe[afi_wlat_minus_2])
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (rmw_partial_pipe[afi_wlat_minus_2])
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_1 <= 1'b0;
                end
                
                if (rmw_correct_pipe[afi_wlat_minus_3])
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_correct_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
                
                if (rmw_partial_pipe[afi_wlat_minus_3])
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b1;
                end
                else
                begin
                    rmw_partial_pipe_eq_afi_wlat_minus_2 <= 1'b0;
                end
            end
        end
    end
    
    always @ (*)
    begin
        if (CFG_WDATA_REG || CFG_ECC_ENC_REG)
        begin
            doing_write_pipe_eq_afi_wlat_minus_x     = doing_write_pipe_eq_afi_wlat_minus_2;
            rdwr_data_valid_pipe_eq_afi_wlat_minus_x = rdwr_data_valid_pipe_eq_afi_wlat_minus_2;
            dataid_pipe_eq_afi_wlat_minus_x          = dataid_pipe_eq_afi_wlat_minus_2;
            dataid_vector_pipe_eq_afi_wlat_minus_x   = dataid_vector_pipe_eq_afi_wlat_minus_2;
            rmw_correct_pipe_eq_afi_wlat_minus_x     = rmw_correct_pipe_eq_afi_wlat_minus_2;
            rmw_partial_pipe_eq_afi_wlat_minus_x     = rmw_partial_pipe_eq_afi_wlat_minus_2;
        end
        else
        begin
            doing_write_pipe_eq_afi_wlat_minus_x     = doing_write_pipe_eq_afi_wlat_minus_1;
            rdwr_data_valid_pipe_eq_afi_wlat_minus_x = rdwr_data_valid_pipe_eq_afi_wlat_minus_1;
            dataid_pipe_eq_afi_wlat_minus_x          = dataid_pipe_eq_afi_wlat_minus_1;
            dataid_vector_pipe_eq_afi_wlat_minus_x   = dataid_vector_pipe_eq_afi_wlat_minus_1;
            rmw_correct_pipe_eq_afi_wlat_minus_x     = rmw_correct_pipe_eq_afi_wlat_minus_1;
            rmw_partial_pipe_eq_afi_wlat_minus_x     = rmw_partial_pipe_eq_afi_wlat_minus_1;
        end
    end
    
    //*************************************************************************************************//
    //            afi_dqs_burst generation logic                                                       //
    //*************************************************************************************************//
    // high earlier than wdata_valid but ends the same
    // for writes only, where dqs should toggle, use doing_write_pipe
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (bg_doing_write || doing_write_pipe[0])
            begin
                int_dqs_burst =  1'b1;
            end
            else
            begin
                int_dqs_burst =  1'b0;
            end
        end
        else
        begin
            if (doing_write_pipe_eq_afi_wlat_minus_1 || doing_write_pipe_eq_afi_wlat_minus_0)
            begin
                int_dqs_burst =  1'b1;
            end
            else
            begin
                int_dqs_burst =  1'b0;
            end
        end
    end
    
    // registered output
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_dqs_burst_r    <=  1'b0;
        end
        else
        begin
            int_dqs_burst_r    <=  int_dqs_burst;
        end
    end
    
    always @ (*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (doing_write_pipe[0])
            begin
                int_dqs_burst_half_rate =  1'b1;
            end
            else
            begin
                int_dqs_burst_half_rate =  1'b0;
            end
        end
        else
        begin
            if (doing_write_pipe[afi_wlat])
            begin
                int_dqs_burst_half_rate =  1'b1;
            end
            else
            begin
                int_dqs_burst_half_rate =  1'b0;
            end
        end
    end
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_dqs_burst_half_rate_r <= 1'b0;
        end
        else
        begin
            int_dqs_burst_half_rate_r <= int_dqs_burst_half_rate;
        end
    end
    
    generate
        genvar K;
        if (CFG_DWIDTH_RATIO == 2) // fullrate
        begin
            for (K = 0; K < CFG_MEM_IF_DQS_WIDTH; K = K + 1)
            begin : C
                assign afi_dqs_burst[K] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_r : int_dqs_burst;
            end
        end
        else if (CFG_DWIDTH_RATIO == 4) // halfrate
        begin
            for (K = 0; K < CFG_MEM_IF_DQS_WIDTH; K = K + 1)
            begin : C
                assign afi_dqs_burst[K + CFG_MEM_IF_DQS_WIDTH] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_r           : int_dqs_burst          ;
                assign afi_dqs_burst[K                       ] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_half_rate_r : int_dqs_burst_half_rate;
            end
        end
        else if (CFG_DWIDTH_RATIO == 8) // quarterrate
        begin
            for (K = 0; K < CFG_MEM_IF_DQS_WIDTH; K = K + 1)
            begin : C
                assign afi_dqs_burst[K + CFG_MEM_IF_DQS_WIDTH * 3] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_r           : int_dqs_burst          ;
                assign afi_dqs_burst[K + CFG_MEM_IF_DQS_WIDTH * 2] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_half_rate_r : int_dqs_burst_half_rate;
                assign afi_dqs_burst[K + CFG_MEM_IF_DQS_WIDTH * 1] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_half_rate_r : int_dqs_burst_half_rate;
                assign afi_dqs_burst[K                           ] = (cfg_output_regd_for_afi_output == 1) ? int_dqs_burst_half_rate_r : int_dqs_burst_half_rate;
            end
        end
    endgenerate
    
    //*************************************************************************************************//
    //            afi_wdata_valid generation logic                                                     //
    //*************************************************************************************************//
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (doing_write_pipe[0])
            begin
                int_wdata_valid =  1'b1;
            end
            else
            begin
                int_wdata_valid =  1'b0;
            end
        end
        else
        begin
            if (doing_write_pipe_eq_afi_wlat_minus_0)
            begin
                int_wdata_valid =  1'b1;
            end
            else
            begin
                int_wdata_valid =  1'b0;
            end
        end
    end
    
    // registered output
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_wdata_valid_r    <=  1'b0;
        end
        else
        begin
            int_wdata_valid_r    <=  int_wdata_valid;
        end
    end
    
    generate
        genvar L;
        for (L = 0; L < CFG_MEM_IF_DQS_WIDTH*(CFG_DWIDTH_RATIO/2); L = L + 1)
        begin : D
            assign afi_wdata_valid[L]    =   (cfg_output_regd_for_afi_output) ? int_wdata_valid_r : int_wdata_valid;
        end
    endgenerate
    
    //*************************************************************************************************//
    //            afi_wdata generation logic                                                           //
    //*************************************************************************************************//
    // cycle earlier than wdata_valid
    // for writes only, where dqs should toggle, use doing_write_pipe
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (bg_rdwr_data_valid && bg_doing_write)
            begin
                int_ecc_wdata_fifo_read =  1'b1;
            end
            else
            begin
                int_ecc_wdata_fifo_read =  1'b0;
            end
        end
        else
        begin
            if (rdwr_data_valid_pipe_eq_afi_wlat_minus_x && doing_write_pipe_eq_afi_wlat_minus_x)
            begin
                int_ecc_wdata_fifo_read =  1'b1;
            end
            else
            begin
                int_ecc_wdata_fifo_read =  1'b0;
            end
        end
    end
    
    // registered output
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_ecc_wdata_fifo_read_r    <=  1'b0;
        end
        else
        begin
            int_ecc_wdata_fifo_read_r    <=  int_ecc_wdata_fifo_read;
        end
    end
    
    assign ecc_wdata_fifo_read    =   (cfg_output_regd_for_wdata_path) ? int_ecc_wdata_fifo_read_r : int_ecc_wdata_fifo_read;
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            ecc_wdata_fifo_read_r <= 1'b0;
        end
        else
        begin
            ecc_wdata_fifo_read_r <= ecc_wdata_fifo_read;
        end
    end
    
    // no data manipulation here
    assign  afi_wdata = ecc_wdata;
    
    // dataid generation
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (bg_rdwr_data_valid && bg_doing_write)
            begin
                int_ecc_wdata_fifo_dataid         =  dataid;
                int_ecc_wdata_fifo_dataid_vector  =  dataid_vector;
            end
            else
            begin
                int_ecc_wdata_fifo_dataid         =  {(CFG_DATA_ID_WIDTH){1'b0}};
                int_ecc_wdata_fifo_dataid_vector  =  {(CFG_DATAID_ARRAY_DEPTH){1'b0}};
            end
        end
        else
        begin
            if (rdwr_data_valid_pipe_eq_afi_wlat_minus_x && doing_write_pipe_eq_afi_wlat_minus_x)
            begin
                int_ecc_wdata_fifo_dataid         =  dataid_pipe_eq_afi_wlat_minus_x;
                int_ecc_wdata_fifo_dataid_vector  =  dataid_vector_pipe_eq_afi_wlat_minus_x;
            end
            else
            begin
                int_ecc_wdata_fifo_dataid         =  {(CFG_DATA_ID_WIDTH){1'b0}};
                int_ecc_wdata_fifo_dataid_vector  =  {(CFG_DATAID_ARRAY_DEPTH){1'b0}};
            end
        end
    end
    
    always @ (posedge ctl_clk, negedge ctl_reset_n) 
    begin
        if (~ctl_reset_n)
        begin
            int_ecc_wdata_fifo_dataid_r        <= 0;
            int_ecc_wdata_fifo_dataid_vector_r <= 0;
        end
        else
        begin
            int_ecc_wdata_fifo_dataid_r        <= int_ecc_wdata_fifo_dataid;
            int_ecc_wdata_fifo_dataid_vector_r <= int_ecc_wdata_fifo_dataid_vector;
        end
    end
    
    assign ecc_wdata_fifo_dataid           =   (cfg_output_regd_for_wdata_path) ? int_ecc_wdata_fifo_dataid_r        : int_ecc_wdata_fifo_dataid;
    assign ecc_wdata_fifo_dataid_vector    =   (cfg_output_regd_for_wdata_path) ? int_ecc_wdata_fifo_dataid_vector_r : int_ecc_wdata_fifo_dataid_vector;
    
    // rmw_correct generation
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (bg_rdwr_data_valid && bg_doing_write)
            begin
                int_ecc_wdata_fifo_rmw_correct =  int_do_rmw_correct;
            end
            else
            begin
                int_ecc_wdata_fifo_rmw_correct =  1'b0;
            end
        end
        else
        begin
            if (rdwr_data_valid_pipe_eq_afi_wlat_minus_x && doing_write_pipe_eq_afi_wlat_minus_x)
            begin
                int_ecc_wdata_fifo_rmw_correct  =  rmw_correct_pipe_eq_afi_wlat_minus_x;
            end
            else
            begin
                int_ecc_wdata_fifo_rmw_correct =  1'b0;
            end
        end
    end
    
    always @ (posedge ctl_clk, negedge ctl_reset_n) 
    begin
        if (~ctl_reset_n)
        begin
            int_ecc_wdata_fifo_rmw_correct_r <= 0;
        end
        else
        begin
            int_ecc_wdata_fifo_rmw_correct_r <= int_ecc_wdata_fifo_rmw_correct;
        end
    end
    
    assign ecc_wdata_fifo_rmw_correct    =   (cfg_output_regd_for_wdata_path) ? int_ecc_wdata_fifo_rmw_correct_r : int_ecc_wdata_fifo_rmw_correct;
    
    // rmw_partial generation
    always @(*)
    begin
        if (afi_wlat_eq_0)
        begin
            if (bg_rdwr_data_valid && bg_doing_write)
            begin
                int_ecc_wdata_fifo_rmw_partial =  int_do_rmw_partial;
            end
            else
            begin
                int_ecc_wdata_fifo_rmw_partial =  1'b0;
            end
        end
        else
        begin
            if (rdwr_data_valid_pipe_eq_afi_wlat_minus_x && doing_write_pipe_eq_afi_wlat_minus_x)
            begin
                int_ecc_wdata_fifo_rmw_partial  =  rmw_partial_pipe_eq_afi_wlat_minus_x;
            end
            else
            begin
                int_ecc_wdata_fifo_rmw_partial =  1'b0;
            end
        end
    end
    
    always @ (posedge ctl_clk, negedge ctl_reset_n) 
    begin
        if (~ctl_reset_n)
        begin
            int_ecc_wdata_fifo_rmw_partial_r <= 0;
        end
        else
        begin
            int_ecc_wdata_fifo_rmw_partial_r <= int_ecc_wdata_fifo_rmw_partial;
        end
    end
    
    assign ecc_wdata_fifo_rmw_partial    =   (cfg_output_regd_for_wdata_path) ? int_ecc_wdata_fifo_rmw_partial_r : int_ecc_wdata_fifo_rmw_partial;
    
    //*************************************************************************************************//
    //            afi_dm generation logic                                                              //
    //*************************************************************************************************//
    //Why do we need ecc_dm and rdwr_data_valid to determine DM
    // ecc_dm will not get updated till we read another data from wrfifo, so we need to drive DMs based on rdwr_data_valid
    //Output registered information already backed in ecc_wdata_fifo_read
    
    // data valid one clock cycle after read
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin
        if (!ctl_reset_n)
        begin
            int_real_wdata_valid <=  1'b0;
        end
        else
        begin
            if (CFG_WDATA_REG || CFG_ECC_ENC_REG)
                begin
                    int_real_wdata_valid <=  ecc_wdata_fifo_read_r;
                end
            else
                begin
                    int_real_wdata_valid <=  ecc_wdata_fifo_read;
                end
        end
    end
    
    generate
        if ((CFG_MEM_IF_DQ_WIDTH*CFG_DWIDTH_RATIO)/8 < CFG_MEM_IF_DM_WIDTH*CFG_DWIDTH_RATIO) // happens in x4 mode
        begin
            genvar J;
            for (J = 0; J < CFG_MEM_IF_DM_WIDTH*CFG_DWIDTH_RATIO; J = J + 1)
            begin : E
                if (J % 2 == 0) //even
                begin
                    assign afi_dm[J]    =   ~ecc_dm[J/2] | ~int_real_wdata_valid;
                end
                else //odd
                begin
                    assign afi_dm[J]    =   ~ecc_dm[(J-1)/2] | ~int_real_wdata_valid;
                end
            end
        end
        else
        begin
            genvar J;
            for (J = 0; J < CFG_MEM_IF_DM_WIDTH*CFG_DWIDTH_RATIO; J = J + 1)
            begin : F
                assign afi_dm[J]    =   ~ecc_dm[J] | ~int_real_wdata_valid;
            end
        end
    endgenerate
    
endmodule
