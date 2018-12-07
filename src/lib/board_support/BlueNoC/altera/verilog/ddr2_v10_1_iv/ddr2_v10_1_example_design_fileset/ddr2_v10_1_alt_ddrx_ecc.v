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

// altera message_off 10036 10230
module ddr2_v10_1_alt_ddrx_ecc #
    ( parameter
        LOCAL_DATA_WIDTH        = 128,
        DWIDTH_RATIO            = 2,
        CTL_ECC_ENABLED         = 0,
        CTL_ECC_RMW_ENABLED     = 0,
        CTL_ECC_CSR_ENABLED     = 0,
        CTL_ECC_MULTIPLES_40_72 = 1,
        CTL_ECC_RDATA_PATH_REGD = 0,            
        FAMILY                  = "Stratix",
        MEMORY_BURSTLENGTH      = 8,
        
        MEM_IF_CS_WIDTH         = 4,
        MEM_IF_CHIP_BITS        = 2,
        MEM_IF_ROW_WIDTH        = 13,
        MEM_IF_COL_WIDTH        = 10,
        MEM_IF_BA_WIDTH         = 3,
        MEM_IF_DQ_WIDTH         = 64
    )
    (
        ctl_clk,
        ctl_reset_n,
        
        do_read,                        
        do_write,
        do_ecc,                         
        do_partial,                     
        do_burst_chop,
        rdwr_data_valid,                
        ecc_fetch_error_addr,           
        to_chip,
        to_bank_addr,
        to_row_addr,
        to_col_addr,
        
        afi_rdata,
        afi_rdata_valid,
        
        ecc_wdata_fifo_read,
        
        write_req_to_wfifo,
        be_to_wfifo,
        
        wdata_fifo_be,                  
        wdata_fifo_wdata,
        
        ecc_enable,
        ecc_enable_auto_corr,
        ecc_gen_sbe,
        ecc_gen_dbe,
        ecc_enable_intr,
        ecc_mask_sbe_intr,
        ecc_mask_dbe_intr,
        ecc_clear,
        
        ecc_single_bit_error,
        ecc_error_chip_addr,
        ecc_error_bank_addr,
        ecc_error_row_addr,
        ecc_error_col_addr,
        rmw_data_ready,
        wdata_is_partial,
        
        ecc_interrupt,
        ecc_rdata_valid,
        ecc_rdata_error,
        ecc_rdata,
        ecc_be,
        ecc_wdata,
        
        wdata_fifo_read,
        
        ecc_sbe_error,
        ecc_dbe_error,
        ecc_sbe_count,
        ecc_dbe_count,
        ecc_error_addr
    );

localparam LOCAL_BE_WIDTH  = LOCAL_DATA_WIDTH / 8;
localparam ECC_DATA_WIDTH  = MEM_IF_DQ_WIDTH * DWIDTH_RATIO;
localparam ECC_BE_WIDTH    = ECC_DATA_WIDTH / 8;
localparam ECC_CODE_WIDTH  = 8; 

localparam NUMBER_OF_INSTANCE = DWIDTH_RATIO * CTL_ECC_MULTIPLES_40_72; 

localparam LOCAL_BE_PER_WORD_WIDTH   = (NUMBER_OF_INSTANCE != 0) ? (LOCAL_BE_WIDTH   / (NUMBER_OF_INSTANCE)) : 0;
localparam LOCAL_DATA_PER_WORD_WIDTH = (NUMBER_OF_INSTANCE != 0) ? (LOCAL_DATA_WIDTH / (NUMBER_OF_INSTANCE)) : 0;
localparam ECC_DATA_PER_WORD_WIDTH   = (NUMBER_OF_INSTANCE != 0) ? (ECC_DATA_WIDTH   / (NUMBER_OF_INSTANCE)) : 0;
localparam ECC_BE_PER_WORD_WIDTH     = (NUMBER_OF_INSTANCE != 0) ? (ECC_BE_WIDTH     / (NUMBER_OF_INSTANCE)) : 0;

localparam ADDR_FIFO_WIDTH = MEM_IF_CHIP_BITS + MEM_IF_BA_WIDTH + MEM_IF_ROW_WIDTH + MEM_IF_COL_WIDTH;

localparam RDWR_DATA_VALID_MAX_LENGTH = MEMORY_BURSTLENGTH / DWIDTH_RATIO;

input ctl_clk;
input ctl_reset_n;

input do_read;
input do_write;
input do_ecc;
input do_partial;
input do_burst_chop;
input rdwr_data_valid;
input ecc_fetch_error_addr;
input [MEM_IF_CS_WIDTH  - 1 : 0] to_chip;
input [MEM_IF_BA_WIDTH  - 1 : 0] to_bank_addr;
input [MEM_IF_ROW_WIDTH - 1 : 0] to_row_addr;
input [MEM_IF_COL_WIDTH - 1 : 0] to_col_addr;

input [ECC_DATA_WIDTH   - 1 : 0] afi_rdata;
input [DWIDTH_RATIO / 2 - 1 : 0] afi_rdata_valid;

input ecc_wdata_fifo_read;

input write_req_to_wfifo;
input [LOCAL_BE_WIDTH - 1 : 0] be_to_wfifo;

input [LOCAL_BE_WIDTH - 1 : 0]   wdata_fifo_be;
input [LOCAL_DATA_WIDTH - 1 : 0] wdata_fifo_wdata;

input ecc_enable;
input ecc_enable_auto_corr;
input ecc_gen_sbe;
input ecc_gen_dbe;
input ecc_enable_intr;
input ecc_mask_sbe_intr;
input ecc_mask_dbe_intr;
input ecc_clear;

output rmw_data_ready;
output ecc_single_bit_error;
output wdata_is_partial;
output [MEM_IF_CHIP_BITS - 1 : 0] ecc_error_chip_addr;
output [MEM_IF_BA_WIDTH  - 1 : 0] ecc_error_bank_addr;
output [MEM_IF_ROW_WIDTH - 1 : 0] ecc_error_row_addr;
output [MEM_IF_COL_WIDTH - 1 : 0] ecc_error_col_addr;

output ecc_interrupt;
output ecc_rdata_error;
output [DWIDTH_RATIO / 2 - 1 : 0] ecc_rdata_valid;
output [LOCAL_DATA_WIDTH - 1 : 0] ecc_rdata;
output [ECC_BE_WIDTH     - 1 : 0] ecc_be;
output [ECC_DATA_WIDTH   - 1 : 0] ecc_wdata;

output wdata_fifo_read;

output          ecc_sbe_error;
output          ecc_dbe_error;
output [7  : 0] ecc_sbe_count;
output [7  : 0] ecc_dbe_count;
output [31 : 0] ecc_error_addr;

wire rmw_data_ready;
wire ecc_single_bit_error;
wire wdata_is_partial;
wire [MEM_IF_CHIP_BITS - 1 : 0] ecc_error_chip_addr;
wire [MEM_IF_BA_WIDTH  - 1 : 0] ecc_error_bank_addr;
wire [MEM_IF_ROW_WIDTH - 1 : 0] ecc_error_row_addr;
wire [MEM_IF_COL_WIDTH - 1 : 0] ecc_error_col_addr;

wire ecc_interrupt;
wire ecc_rdata_error;
wire [DWIDTH_RATIO / 2 - 1 : 0] ecc_rdata_valid;
wire [LOCAL_DATA_WIDTH - 1 : 0] ecc_rdata;
wire [ECC_BE_WIDTH     - 1 : 0] ecc_be;
wire [ECC_DATA_WIDTH   - 1 : 0] ecc_wdata;

wire wdata_fifo_read;

wire          ecc_sbe_error;
wire          ecc_dbe_error;
wire [7  : 0] ecc_sbe_count;
wire [7  : 0] ecc_dbe_count;
wire [31 : 0] ecc_error_addr;

generate
    if (!CTL_ECC_ENABLED) 
    begin
        assign rmw_data_ready       = 0;
        assign ecc_single_bit_error = 0;
        assign ecc_error_chip_addr  = 0;
        assign ecc_error_bank_addr  = 0;
        assign ecc_error_row_addr   = 0;
        assign ecc_error_col_addr   = 0;
        assign wdata_is_partial     = 0;
        
        assign ecc_interrupt        = 0;
        assign ecc_rdata_valid      = afi_rdata_valid;
        assign ecc_rdata_error      = 0;
        assign ecc_rdata            = afi_rdata;
        assign ecc_be               = wdata_fifo_be;
        assign ecc_wdata            = wdata_fifo_wdata;
        
        assign wdata_fifo_read      = ecc_wdata_fifo_read;
        
        assign ecc_sbe_error        = 0;
        assign ecc_dbe_error        = 0;
        assign ecc_sbe_count        = 0;
        assign ecc_dbe_count        = 0;
        assign ecc_error_addr       = 0;
    end
    else 
    begin
        
        reg do_read_r1;
        reg do_read_r2;
        reg do_read_r3;
        reg do_burst_chop_r1;
        reg do_burst_chop_r2;
        reg do_burst_chop_r3;
        reg read_addr_fifo_rd;
        reg read_addr_fifo_wr;
        reg [ADDR_FIFO_WIDTH : 0] read_addr_fifo_wr_data;
        reg [ADDR_FIFO_WIDTH : 0] to_addr_r1;
        reg [ADDR_FIFO_WIDTH : 0] to_addr_r2;
        reg [ADDR_FIFO_WIDTH : 0] to_addr_r3;
        reg [2 : 0]               read_size_count;
        reg [2 : 0]               rdata_valid_count;
        
        wire scfifo_reset;
        
        wire read_addr_fifo_full;
        wire read_addr_fifo_empty;
        wire [ADDR_FIFO_WIDTH + 3 : 0] read_addr_fifo_rd_data; 
        
        reg  error_addr_fifo_wr;
        
        wire error_addr_fifo_rd;
        wire error_addr_fifo_full;
        wire error_addr_fifo_empty;
        wire [ADDR_FIFO_WIDTH - 1 : 0] error_addr_fifo_rd_data;
        wire [ADDR_FIFO_WIDTH - 1 : 0] error_addr_fifo_wr_data;
        
        reg  [LOCAL_DATA_WIDTH - 1 : 0] ecc_rdata_r1;
        
        reg  int_rmw_data_ready;
        reg  rmw_data_fifo_wr;
        reg  rmw_data_fifo_rd;
        reg  [NUMBER_OF_INSTANCE - 1 : 0]                  rmw_data_double_bit_error_r1;
        reg  [ECC_CODE_WIDTH * NUMBER_OF_INSTANCE - 1 : 0] rmw_data_ecc_code_r1;
        
        wire rmw_data_fifo_full;
        wire rmw_data_fifo_empty;
        wire [ECC_DATA_WIDTH + NUMBER_OF_INSTANCE - 1 : 0] rmw_data_fifo_rd_data;
        wire [ECC_DATA_WIDTH + NUMBER_OF_INSTANCE - 1 : 0] rmw_data_fifo_wr_data;
        wire [NUMBER_OF_INSTANCE                  - 1 : 0] rmw_data_double_bit_error;
        wire [ECC_CODE_WIDTH * NUMBER_OF_INSTANCE - 1 : 0] rmw_data_ecc_code;
        wire [NUMBER_OF_INSTANCE                  - 1 : 0] rmw_data_be;
        wire [LOCAL_DATA_WIDTH                    - 1 : 0] rmw_data;
        
        reg  [NUMBER_OF_INSTANCE - 1 : 0] int_single_bit_error;
        reg  [NUMBER_OF_INSTANCE - 1 : 0] int_double_bit_error;
        
        reg ecc_wdata_fifo_read_r1;
        reg ecc_wdata_fifo_read_r2;
        reg ecc_wdata_fifo_read_r3;
        
        reg int_afi_rdata_valid_r1; 
        
        reg afi_rdata_single_bit_error;
        
        reg [MEM_IF_CHIP_BITS - 1 : 0] to_chip_addr;
        
        reg [2 : 0] read_addr_fifo_read_size_count;
        
        reg int_ecc_rdata_valid;
        
        reg int_wdata_fifo_read;
        
        reg doing_partial_read;
        reg doing_partial_write;
        reg doing_partial_write_r1;
        reg doing_partial_write_r2;
        
        reg doing_ecc_read;
        reg doing_ecc_write;
        reg doing_ecc_write_r1;
        reg doing_ecc_write_r2;
        
        reg [LOCAL_DATA_WIDTH - 1 : 0] int_ecc_corrected_wdata;
        
        reg int_partial_write;
        
        reg int_ecc_interrupt;
        
        wire [NUMBER_OF_INSTANCE - 1 : 0] decoder_err_detected;
        wire [NUMBER_OF_INSTANCE - 1 : 0] decoder_err_corrected;
        wire [NUMBER_OF_INSTANCE - 1 : 0] decoder_err_fatal;
        
        wire zero = 1'b0;
        
        assign wdata_fifo_read = int_wdata_fifo_read;
        assign rmw_data_ready  = int_rmw_data_ready;
        assign ecc_rdata_valid = {{((DWIDTH_RATIO / 2) - 1){1'b0}}, int_ecc_rdata_valid};
        assign ecc_rdata_error = (ecc_enable) ? |decoder_err_fatal : 1'b0; 
        
        assign wdata_is_partial = int_partial_write;
        
        /*------------------------------------------------------------------------------
        
            AFI Read Data Path
        
        ------------------------------------------------------------------------------*/
        reg [ECC_DATA_WIDTH - 1 : 0] int_afi_rdata;
        reg [DWIDTH_RATIO/2 - 1 : 0] int_afi_rdata_valid;
        
        if (CTL_ECC_RDATA_PATH_REGD)
        begin
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    int_afi_rdata       <= 0;
                    int_afi_rdata_valid <= 0;
                end
                else
                begin
                    int_afi_rdata       <= afi_rdata;
                    int_afi_rdata_valid <= afi_rdata_valid;
                end
            end
        end
        else
        begin
            always @ (*)
            begin
                int_afi_rdata       = afi_rdata;
                int_afi_rdata_valid = afi_rdata_valid;
            end
        end
        
        /*------------------------------------------------------------------------------
        
            CSR Information
        
        ------------------------------------------------------------------------------*/
        reg          int_ecc_sbe_error;
        reg          int_ecc_dbe_error;
        reg [7  : 0] int_ecc_sbe_count;
        reg [7  : 0] int_ecc_dbe_count;
        reg [31 : 0] int_ecc_error_addr;
        
        if (!CTL_ECC_CSR_ENABLED) 
        begin
            assign ecc_sbe_error  = 0;
            assign ecc_dbe_error  = 0;
            assign ecc_sbe_count  = 0;
            assign ecc_dbe_count  = 0;
            assign ecc_error_addr = 0;
        end
        else
        begin
            assign ecc_sbe_error  = int_ecc_sbe_error;
            assign ecc_dbe_error  = int_ecc_dbe_error;
            assign ecc_sbe_count  = int_ecc_sbe_count;
            assign ecc_dbe_count  = int_ecc_dbe_count;
            assign ecc_error_addr = int_ecc_error_addr;
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    int_ecc_sbe_error <= 1'b0;
                    int_ecc_dbe_error <= 1'b0;
                end
                else
                begin
                    if (ecc_clear)
                        int_ecc_sbe_error <= 1'b0;
                    else if (|int_single_bit_error && int_afi_rdata_valid_r1)
                        int_ecc_sbe_error <= 1'b1;
                    
                    if (ecc_clear)
                        int_ecc_dbe_error <= 1'b0;
                    else if (|int_double_bit_error && int_afi_rdata_valid_r1)
                        int_ecc_dbe_error <= 1'b1;
                end
            end
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    int_ecc_sbe_count <= 0;
                    int_ecc_dbe_count <= 0;
                end
                else
                begin
                    if (ecc_clear)
                        int_ecc_sbe_count <= 0;
                    else if (|int_single_bit_error && int_afi_rdata_valid_r1) 
                        int_ecc_sbe_count <= int_ecc_sbe_count + 1;
                    
                    if (ecc_clear)
                        int_ecc_dbe_count <= 0;
                    else if (|int_double_bit_error && int_afi_rdata_valid_r1) 
                        int_ecc_dbe_count <= int_ecc_dbe_count + 1;
                end
            end
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    int_ecc_error_addr <= 0;
                end
                else
                begin
                    if (ecc_clear)
                        int_ecc_error_addr <= 0;
                    else if ((|int_single_bit_error || |int_double_bit_error) && int_afi_rdata_valid_r1) 
                        int_ecc_error_addr <= read_addr_fifo_rd_data [ADDR_FIFO_WIDTH - 1 : 0]; 
                end
            end
        end
        
        /*------------------------------------------------------------------------------
        
            Encoder and Decoder Instantiation
        
        ------------------------------------------------------------------------------*/
        reg [1 : 0] bit_error;
        reg [ECC_DATA_WIDTH - 1 : 0] int_afi_rdata_r1;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                bit_error <= 0;
            else
            begin
                if (ecc_gen_sbe)
                    bit_error <= 2'b01;
                else if (ecc_gen_dbe)
                    bit_error <= 2'b11;
                else
                    bit_error <= 2'b00;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_afi_rdata_r1 <= 0;
            else
                int_afi_rdata_r1 <= int_afi_rdata;
        end
        
        genvar z;
        for (z = 0;z < NUMBER_OF_INSTANCE;z = z + 1)
        begin : encoder_instantiation
            wire [LOCAL_DATA_PER_WORD_WIDTH                - 1 : 0] int_wdata_fifo_wdata;
            wire [LOCAL_DATA_PER_WORD_WIDTH                - 1 : 0] int_rmw_wdata;
            wire [ECC_DATA_PER_WORD_WIDTH                  - 1 : 0] int_ecc_wdata;
            wire [ECC_DATA_PER_WORD_WIDTH                  - 1 : 0] int_ecc_wdata_rmw;
            wire [LOCAL_BE_PER_WORD_WIDTH                  - 1 : 0] int_wdata_fifo_be;
            wire [LOCAL_DATA_PER_WORD_WIDTH                - 1 : 0] int_ecc_rdata;
            
            reg  [ECC_BE_PER_WORD_WIDTH                    - 1 : 0] int_ecc_be;
            reg  [ECC_CODE_WIDTH                           - 1 : 0] altered_ecc_code;
            reg  [LOCAL_DATA_PER_WORD_WIDTH                - 1 : 0] altered_ecc_rdata;
            reg  [ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : 0] altered_ecc_wdata;
            
            /*------------------------------------------------------------------------------
                Read Data
            ------------------------------------------------------------------------------*/
            assign ecc_rdata [(z + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : z * LOCAL_DATA_PER_WORD_WIDTH] = altered_ecc_rdata;
            
            always @ (*)
            begin
                if (ecc_enable)
                    altered_ecc_rdata = int_ecc_rdata;
                else
                    altered_ecc_rdata = int_afi_rdata_r1 [(z + 1) * ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : z * ECC_DATA_PER_WORD_WIDTH];
            end
            
            /*------------------------------------------------------------------------------
                Write Data
            ------------------------------------------------------------------------------*/
            assign ecc_wdata [(z + 1) * ECC_DATA_PER_WORD_WIDTH - 1 : z * ECC_DATA_PER_WORD_WIDTH + LOCAL_DATA_PER_WORD_WIDTH] = altered_ecc_code;
            
            assign ecc_wdata [(z + 1) * ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : z * ECC_DATA_PER_WORD_WIDTH]            = altered_ecc_wdata;
            
            assign ecc_be    [(z + 1) * ECC_BE_PER_WORD_WIDTH - 1 : z * ECC_BE_PER_WORD_WIDTH]                                 = int_ecc_be;
            
            always @ (*)
            begin
                if (CTL_ECC_RMW_ENABLED && doing_ecc_write_r2)
                    altered_ecc_wdata = {int_ecc_wdata_rmw [ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : 2], (bit_error [1] ^ int_ecc_wdata_rmw [1]), (bit_error [0] ^ int_ecc_wdata_rmw [0])};
                else if (doing_partial_write_r2)
                    altered_ecc_wdata = {int_ecc_wdata_rmw [ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : 2], (bit_error [1] ^ int_ecc_wdata_rmw [1]), (bit_error [0] ^ int_ecc_wdata_rmw [0])};
                else
                    altered_ecc_wdata = {int_ecc_wdata [ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH - 1 : 2], (bit_error [1] ^ int_ecc_wdata [1]), (bit_error [0] ^ int_ecc_wdata [0])};
            end
            
            always @ (*)
            begin
                if (CTL_ECC_RMW_ENABLED && doing_ecc_write_r2)
                begin
                    if (rmw_data_double_bit_error_r1 [z]) 
                        altered_ecc_code = rmw_data_ecc_code_r1 [(z + 1) * ECC_CODE_WIDTH - 1 : z * ECC_CODE_WIDTH];
                    else
                        altered_ecc_code = int_ecc_wdata_rmw [ECC_DATA_PER_WORD_WIDTH - 1 : LOCAL_DATA_PER_WORD_WIDTH];
                end
                else if (doing_partial_write_r2)
                    altered_ecc_code = int_ecc_wdata_rmw [ECC_DATA_PER_WORD_WIDTH - 1 : LOCAL_DATA_PER_WORD_WIDTH];
                else
                    altered_ecc_code = int_ecc_wdata [ECC_DATA_PER_WORD_WIDTH - 1 : LOCAL_DATA_PER_WORD_WIDTH];
            end
            
            assign int_rmw_wdata        = rmw_data [(z + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : z * LOCAL_DATA_PER_WORD_WIDTH];
            
            assign int_wdata_fifo_wdata = wdata_fifo_wdata [(z + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : z * LOCAL_DATA_PER_WORD_WIDTH];
            
            assign int_wdata_fifo_be    = wdata_fifo_be [(z + 1) * LOCAL_BE_PER_WORD_WIDTH - 1 : z * LOCAL_BE_PER_WORD_WIDTH];
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    int_ecc_be <= 0;
                end
                else
                begin
                    if (ecc_enable)
                    begin
                        int_ecc_be <= {ECC_BE_PER_WORD_WIDTH{1'b1}};
                    end
                    else
                        int_ecc_be <= {{(ECC_BE_PER_WORD_WIDTH - LOCAL_BE_PER_WORD_WIDTH){1'b0}}, int_wdata_fifo_be};
                end
            end
            
            
            ddr2_v10_1_alt_ddrx_encoder # (
                .INPUT_DATA_WIDTH  (LOCAL_DATA_PER_WORD_WIDTH),
                .OUTPUT_DATA_WIDTH (ECC_DATA_PER_WORD_WIDTH)
            ) encoder_corrected_inst (
                .ctl_clk     (ctl_clk),
                .input_data  (int_rmw_wdata),
                .output_data (int_ecc_wdata_rmw)
            );
            
            ddr2_v10_1_alt_ddrx_encoder # (
                .INPUT_DATA_WIDTH  (LOCAL_DATA_PER_WORD_WIDTH),
                .OUTPUT_DATA_WIDTH (ECC_DATA_PER_WORD_WIDTH)
            ) encoder_inst (
                .ctl_clk     (ctl_clk),
                .input_data  (int_wdata_fifo_wdata),
                .output_data (int_ecc_wdata)
            );
            
            ddr2_v10_1_alt_ddrx_decoder # (
                .INPUT_DATA_WIDTH  (ECC_DATA_PER_WORD_WIDTH),
                .OUTPUT_DATA_WIDTH (LOCAL_DATA_PER_WORD_WIDTH)
            ) decoder_inst (
                .ctl_clk       (ctl_clk),
                .input_data    (int_afi_rdata [(z + 1) * ECC_DATA_PER_WORD_WIDTH - 1 : z * ECC_DATA_PER_WORD_WIDTH]),
                .err_corrected (decoder_err_corrected [z]),
                .err_detected  (decoder_err_detected [z]),
                .err_fatal     (decoder_err_fatal [z]),
                .output_data   (int_ecc_rdata)
            );
            
            always @ (*)
            begin
                if (decoder_err_detected [z] && ecc_enable)
                begin
                    if (decoder_err_corrected [z])
                    begin
                        int_single_bit_error [z] = 1'b1;
                        int_double_bit_error [z] = 1'b0;
                    end
                    else if (decoder_err_fatal [z])
                    begin
                        int_single_bit_error [z] = 1'b0;
                        int_double_bit_error [z] = 1'b1;
                    end
                    else
                    begin
                        int_single_bit_error [z] = 1'b0;
                        int_double_bit_error [z] = 1'b0;
                    end
                end
                else
                begin
                    int_single_bit_error [z] = 1'b0;
                    int_double_bit_error [z] = 1'b0;
                end
            end
        end
        
        /*------------------------------------------------------------------------------
        
            Partial Information
        
        ------------------------------------------------------------------------------*/
        always @ (*)
        begin
            if (ecc_enable)
            begin
                if (be_to_wfifo == {LOCAL_BE_WIDTH{1'b1}})
                    int_partial_write = 1'b0;
                else
                    int_partial_write = 1'b1;
            end
            else
                int_partial_write = 1'b0;
        end
        
        /*------------------------------------------------------------------------------
        
            Address FIFO
        
        ------------------------------------------------------------------------------*/
        assign scfifo_reset = !ctl_reset_n; 
        
        if (MEM_IF_CS_WIDTH == 1)
        begin
            always @ (*)
            begin
                to_chip_addr = zero;
            end
        end
        else if (MEM_IF_CS_WIDTH == 2)
        begin
            always @ (*)
            begin
                if (to_chip [0])
                    to_chip_addr = 1'b0;
                else if (to_chip [1])
                    to_chip_addr = 1'b1;
                else
                    to_chip_addr = 1'b0;
            end
        end
        else if (MEM_IF_CS_WIDTH == 4)
        begin
            always @ (*)
            begin
                if (to_chip [0])
                    to_chip_addr = 2'b00;
                else if (to_chip [1])
                    to_chip_addr = 2'b01;
                else if (to_chip [2])
                    to_chip_addr = 2'b10;
                else if (to_chip [3])
                    to_chip_addr = 2'b11;
                else
                    to_chip_addr = 2'b00;
            end
        end
        else if (MEM_IF_CS_WIDTH == 8)
        begin
            always @ (*)
            begin
                if (to_chip [0])
                    to_chip_addr = 3'b000;
                else if (to_chip [1])
                    to_chip_addr = 3'b001;
                else if (to_chip [2])
                    to_chip_addr = 3'b010;
                else if (to_chip [3])
                    to_chip_addr = 3'b011;
                else if (to_chip [4])
                    to_chip_addr = 3'b100;
                else if (to_chip [5])
                    to_chip_addr = 3'b101;
                else if (to_chip [6])
                    to_chip_addr = 3'b110;
                else if (to_chip [7])
                    to_chip_addr = 3'b111;
                else
                    to_chip_addr = 3'b000;
            end
        end
        
        /*------------------------------------------------------------------------------
            Read Address FIFO
        ------------------------------------------------------------------------------*/
        always @ (*)
        begin
            if (read_addr_fifo_full)
            begin
				// synthesis translate_off
                $write($time);
                $write(" DDRX ECC Warning: Read address fifo overflow\n");
				// synthesis translate_on
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                do_burst_chop_r1 <= 1'b0;
                do_burst_chop_r2 <= 1'b0;
                do_burst_chop_r3 <= 1'b0;
            end
            else
            begin
                do_burst_chop_r1 <= do_burst_chop;
                do_burst_chop_r2 <= do_burst_chop_r1;
                do_burst_chop_r3 <= do_burst_chop_r2;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                do_read_r1 <= 1'b0;
                do_read_r2 <= 1'b0;
                do_read_r3 <= 1'b0;
            end
            else
            begin
                do_read_r1 <= do_read;
                do_read_r2 <= do_read_r1;
                do_read_r3 <= do_read_r2;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                read_size_count <= 0;
            end
            else
            begin
                if (do_read && rdwr_data_valid) 
                    read_size_count <= 1'b1;
                else if (do_read && !rdwr_data_valid) 
                    read_size_count <= 1'b0;
                else
                begin
                    if (RDWR_DATA_VALID_MAX_LENGTH == 4) 
                    begin
                        if (do_read_r1 && rdwr_data_valid)
                            read_size_count <= read_size_count + 1'b1;
                        else if (do_read_r2 && !do_burst_chop_r2 && rdwr_data_valid) 
                            read_size_count <= read_size_count + 1'b1;
                        else if (do_read_r3 && !do_burst_chop_r3 && rdwr_data_valid) 
                            read_size_count <= read_size_count + 1'b1;
                    end
                    else if (RDWR_DATA_VALID_MAX_LENGTH == 2) 
                    begin
                        if (do_read_r1 && !do_burst_chop_r1 && rdwr_data_valid) 
                            read_size_count <= read_size_count + 1'b1;
                    end
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                to_addr_r1 <= 0;
                to_addr_r2 <= 0;
                to_addr_r3 <= 0;
            end
            else
            begin
                to_addr_r1 <= {(do_ecc | do_partial), to_chip_addr, to_bank_addr, to_row_addr, to_col_addr}; 
                to_addr_r2 <= to_addr_r1;
                to_addr_r3 <= to_addr_r2;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                read_addr_fifo_wr      <= 1'b0;
                read_addr_fifo_wr_data <= 0;
            end
            else
            begin
                if (RDWR_DATA_VALID_MAX_LENGTH == 2) 
                begin
                    read_addr_fifo_wr      <= do_read_r1;
                    read_addr_fifo_wr_data <= to_addr_r1;
                end
                else if (RDWR_DATA_VALID_MAX_LENGTH == 4) 
                begin
                    read_addr_fifo_wr      <= do_read_r3;
                    read_addr_fifo_wr_data <= to_addr_r3;
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                rdata_valid_count <= 0;
            end
            else
            begin
                if (int_afi_rdata_valid [0] && read_addr_fifo_rd) 
                    rdata_valid_count <= 1'b1;
                else if (int_afi_rdata_valid [0])
                    rdata_valid_count <= rdata_valid_count + 1'b1;
                else
                    rdata_valid_count <= 0;
            end
        end
        
        always @ (*)
        begin
            if (rdata_valid_count != 0)
            begin
                if (rdata_valid_count == read_addr_fifo_rd_data [ADDR_FIFO_WIDTH + 3 : ADDR_FIFO_WIDTH + 1])
                    read_addr_fifo_rd = 1'b1;
                else
                    read_addr_fifo_rd = 1'b0;
            end
            else
                read_addr_fifo_rd = 1'b0;
        end
        
        /*
        reg [31 : 0] read_addr_fifo_rd_cnt;
        reg [31 : 0] read_addr_fifo_wr_cnt;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                read_addr_fifo_rd_cnt <= 0;
                read_addr_fifo_wr_cnt <= 0;
            end
            else
            begin
                if (read_addr_fifo_rd)
                    read_addr_fifo_rd_cnt <= read_addr_fifo_rd_cnt + 1'b1;
                
                if (read_addr_fifo_wr)
                    read_addr_fifo_wr_cnt <= read_addr_fifo_wr_cnt + 1'b1;
            end
        end
        */
        /*
        reg [31 : 0] ecc_read_count;
        reg [31 : 0] ecc_write_count;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                ecc_read_count  <= 0;
                ecc_write_count <= 0;
            end
            else
            begin
                if (do_ecc && do_read)
                    ecc_read_count <= ecc_read_count + 1'b1;
                
                if (do_ecc && do_write)
                    ecc_write_count <= ecc_write_count + 1'b1;
            end
        end
        */
        
        (* message_disable = "14320" *) scfifo #
            (
                .intended_device_family  (FAMILY),
                .lpm_width               (ADDR_FIFO_WIDTH + 4),         
                .lpm_numwords            (16),                          
                .lpm_widthu              (4),                           
                .almost_full_value       (16 - 4),                      
                .lpm_type                ("scfifo"),
                .lpm_showahead           ("ON"),                        
                .overflow_checking       ("OFF"),
                .underflow_checking      ("OFF"),
                .use_eab                 ("ON"),
                .add_ram_output_register ("ON")
            )
            read_addr_fifo
            (
                .rdreq                   (read_addr_fifo_rd),
                .aclr                    (scfifo_reset),
                .clock                   (ctl_clk),
                .wrreq                   (read_addr_fifo_wr),
                .data                    ({read_size_count, read_addr_fifo_wr_data}),
                .full                    (read_addr_fifo_full),
                .q                       (read_addr_fifo_rd_data),
                .sclr                    (1'b0),
                .usedw                   (),
                .empty                   (read_addr_fifo_empty),
                .almost_full             (),
                .almost_empty            ()
           );
        
        /*------------------------------------------------------------------------------
        
            RMW Data FIFO
        
        ------------------------------------------------------------------------------*/
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                ecc_rdata_r1 <= 0;
            else
                ecc_rdata_r1 <= ecc_rdata;
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                rmw_data_fifo_wr <= 1'b0;
            end
            else
            begin
                if (CTL_ECC_RMW_ENABLED && int_afi_rdata_valid_r1 && doing_ecc_read && read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                    rmw_data_fifo_wr <= 1'b1;
                else if (int_afi_rdata_valid_r1 && doing_partial_read && read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                    rmw_data_fifo_wr <= 1'b1;
                else
                    rmw_data_fifo_wr <= 1'b0;
            end
        end
        
        always @ (*)
        begin
            if (CTL_ECC_RMW_ENABLED && doing_ecc_write && ecc_wdata_fifo_read)
                rmw_data_fifo_rd = 1'b1;
            else if (doing_partial_write && ecc_wdata_fifo_read)
                rmw_data_fifo_rd = 1'b1;
            else
                rmw_data_fifo_rd = 1'b0;
        end
        
        genvar y;
        for (y = 0;y < NUMBER_OF_INSTANCE;y = y + 1)
        begin : ecc_code_per_word
            reg [ECC_CODE_WIDTH            - 1 : 0] original_ecc_code;
            reg [LOCAL_DATA_PER_WORD_WIDTH - 1 : 0] int_ecc_rdata_masked;
            reg [LOCAL_DATA_PER_WORD_WIDTH - 1 : 0] int_ecc_rdata;
            reg [LOCAL_BE_PER_WORD_WIDTH   - 1 : 0] int_wdata_fifo_be;
            reg [LOCAL_DATA_PER_WORD_WIDTH - 1 : 0] int_wdata_fifo_wdata;
            reg                                     int_double_bit_error_r1;
            
            /*------------------------------------------------------------------------------
                Inputs
            ------------------------------------------------------------------------------*/
            assign rmw_data_fifo_wr_data [(y + 1) * (ECC_DATA_PER_WORD_WIDTH + 1) - 1 : y * (ECC_DATA_PER_WORD_WIDTH + 1)] = {int_double_bit_error_r1, original_ecc_code, int_ecc_rdata_masked};
            
            always @ (*)
            begin
                int_ecc_rdata        = ecc_rdata_r1     [(y + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : y * LOCAL_DATA_PER_WORD_WIDTH];
                int_wdata_fifo_be    = wdata_fifo_be    [(y + 1) * LOCAL_BE_PER_WORD_WIDTH   - 1 : y * LOCAL_BE_PER_WORD_WIDTH];
                int_wdata_fifo_wdata = wdata_fifo_wdata [(y + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : y * LOCAL_DATA_PER_WORD_WIDTH];
            end
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                    int_double_bit_error_r1 <= 1'b0;
                else
                    int_double_bit_error_r1 <= int_double_bit_error [y];
            end
            
            genvar i;
            for (i = 0;i < LOCAL_BE_PER_WORD_WIDTH;i = i + 1)
            begin : wdata_mask_per_byte_enable
                always @ (*)
                begin
                    if (doing_partial_read)
                    begin
                        if (int_wdata_fifo_be [i]) 
                            int_ecc_rdata_masked [(i + 1) * 8 - 1 : i * 8] = int_wdata_fifo_wdata [(i + 1) * 8 - 1 : i * 8];
                        else 
                            int_ecc_rdata_masked [(i + 1) * 8 - 1 : i * 8] = int_ecc_rdata [(i + 1) * 8 - 1 : i * 8];
                    end
                    else
                        int_ecc_rdata_masked [(i + 1) * 8 - 1 : i * 8] = int_ecc_rdata [(i + 1) * 8 - 1 : i * 8];
                end
            end
            
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                    original_ecc_code <= 0;
                else
                    original_ecc_code <= int_afi_rdata_r1 [(y + 1) * ECC_DATA_PER_WORD_WIDTH - 1 : (y * ECC_DATA_PER_WORD_WIDTH) + (ECC_DATA_PER_WORD_WIDTH - ECC_CODE_WIDTH)];
            end
            
            /*------------------------------------------------------------------------------
                Outputs
            ------------------------------------------------------------------------------*/
            assign rmw_data_double_bit_error [y]                                                      = rmw_data_fifo_rd_data [(y + 1) * (ECC_DATA_PER_WORD_WIDTH + 1) - 1];
            assign rmw_data_ecc_code [(y + 1) * ECC_CODE_WIDTH - 1 : y * ECC_CODE_WIDTH]              = rmw_data_fifo_rd_data [(y + 1) * (ECC_DATA_PER_WORD_WIDTH + 1) - 2 : y * (ECC_DATA_PER_WORD_WIDTH + 1) + LOCAL_DATA_PER_WORD_WIDTH];
            assign rmw_data [(y + 1) * LOCAL_DATA_PER_WORD_WIDTH - 1 : y * LOCAL_DATA_PER_WORD_WIDTH] = rmw_data_fifo_rd_data [(y + 1) * (ECC_DATA_PER_WORD_WIDTH + 1) - ECC_CODE_WIDTH - 2 : y * (ECC_DATA_PER_WORD_WIDTH + 1)];
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                rmw_data_double_bit_error_r1 <= 0;
                rmw_data_ecc_code_r1         <= 0;
            end
            else
            begin
                rmw_data_double_bit_error_r1 <= rmw_data_double_bit_error;
                rmw_data_ecc_code_r1         <= rmw_data_ecc_code;
            end
        end
        
        (* message_disable = "14320" *) scfifo #
            (
                .intended_device_family  (FAMILY),
                .lpm_width               (ECC_DATA_WIDTH + NUMBER_OF_INSTANCE),           
                .lpm_numwords            (4),                                             
                .lpm_widthu              (2),                                             
                .almost_full_value       (4 - 2),                                         
                .lpm_type                ("scfifo"),
                .lpm_showahead           ("OFF"),
                .overflow_checking       ("OFF"),
                .underflow_checking      ("OFF"),
                .use_eab                 ("ON"),
                .add_ram_output_register ("ON")
            )
            rmw_data_fifo
            (
                .rdreq                   (rmw_data_fifo_rd),
                .aclr                    (scfifo_reset),
                .clock                   (ctl_clk),
                .wrreq                   (rmw_data_fifo_wr),
                .data                    (rmw_data_fifo_wr_data),
                .full                    (rmw_data_fifo_full),
                .q                       (rmw_data_fifo_rd_data),
                .sclr                    (1'b0),
                .usedw                   (),
                .empty                   (rmw_data_fifo_empty),
                .almost_full             (),
                .almost_empty            ()
           );
        
        /*------------------------------------------------------------------------------
            rmw_data_ready
        ------------------------------------------------------------------------------*/
        always @ (*)
        begin
            if (doing_ecc_write || doing_partial_write)
                int_rmw_data_ready = 1'b0;
            else
                int_rmw_data_ready = !rmw_data_fifo_empty;
        end
        
        /*------------------------------------------------------------------------------
        
            Others
        
        ------------------------------------------------------------------------------*/
        
        /*------------------------------------------------------------------------------
            wdata fifo read
        ------------------------------------------------------------------------------*/
        
        always @ (*)
        begin
            if (CTL_ECC_RMW_ENABLED && doing_ecc_write) 
                int_wdata_fifo_read = 1'b0; 
            else if (doing_partial_write)
                int_wdata_fifo_read = 1'b0; 
            else if (doing_partial_read && int_afi_rdata_valid_r1 && read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                int_wdata_fifo_read = 1'b1; 
            else
                int_wdata_fifo_read = ecc_wdata_fifo_read;
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                ecc_wdata_fifo_read_r1 <= 1'b0;
                ecc_wdata_fifo_read_r2 <= 1'b0;
                ecc_wdata_fifo_read_r3 <= 1'b0;
            end
            else
            begin
                ecc_wdata_fifo_read_r1 <= ecc_wdata_fifo_read;
                ecc_wdata_fifo_read_r2 <= ecc_wdata_fifo_read_r1;
                ecc_wdata_fifo_read_r3 <= ecc_wdata_fifo_read_r2;
            end
        end
        
        /*------------------------------------------------------------------------------
            doing partial read write
        ------------------------------------------------------------------------------*/
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                read_addr_fifo_read_size_count <= 0;
            end
            else
            begin
                if (read_addr_fifo_rd)
                begin
                    read_addr_fifo_read_size_count <= read_addr_fifo_rd_data [ADDR_FIFO_WIDTH + 3 : ADDR_FIFO_WIDTH + 1];
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                doing_partial_write <= 1'b0;
            end
            else
            begin
                if (do_write && do_partial)
                    doing_partial_write <= 1'b1;
                else if (read_addr_fifo_read_size_count == 1 && ecc_wdata_fifo_read)    
                    doing_partial_write <= 1'b0;
                else if (read_addr_fifo_read_size_count == 2 && ecc_wdata_fifo_read_r1) 
                    doing_partial_write <= 1'b0;
                else if (read_addr_fifo_read_size_count == 3 && ecc_wdata_fifo_read_r2) 
                    doing_partial_write <= 1'b0;
                else if (ecc_wdata_fifo_read_r3) 
                    doing_partial_write <= 1'b0;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                doing_partial_write_r1 <= 1'b0;
                doing_partial_write_r2 <= 1'b0;
            end
            else
            begin
                doing_partial_write_r1 <= doing_partial_write;
                doing_partial_write_r2 <= doing_partial_write_r1;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                doing_partial_read <= 1'b0;
            end
            else
            begin
                if (do_read && do_partial)
                    doing_partial_read <= 1'b1;
                else if (do_write && do_partial) 
                    doing_partial_read <= 1'b0;
            end
        end
        
        /*------------------------------------------------------------------------------
            ecc rdata valid
        ------------------------------------------------------------------------------*/
        always @ (*)
        begin
            if (CTL_ECC_RMW_ENABLED && int_afi_rdata_valid_r1 && doing_ecc_read && read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                int_ecc_rdata_valid = 1'b0;
            else if (int_afi_rdata_valid_r1 && doing_partial_read && read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                int_ecc_rdata_valid = 1'b0;
            else
                int_ecc_rdata_valid = int_afi_rdata_valid_r1;
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                int_afi_rdata_valid_r1 <= 1'b0;
            end
            else
            begin
                int_afi_rdata_valid_r1 <= int_afi_rdata_valid [0];
            end
        end
        
        /*------------------------------------------------------------------------------
            ECC Interrupt
        ------------------------------------------------------------------------------*/
        assign ecc_interrupt = int_ecc_interrupt;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                int_ecc_interrupt <= 1'b0;
            else
            begin
                if (!ecc_clear) 
                begin
                    if (ecc_enable_intr)
                    begin
                        if ((!ecc_mask_sbe_intr && |int_single_bit_error && int_afi_rdata_valid_r1) || (!ecc_mask_dbe_intr && |int_double_bit_error && int_afi_rdata_valid_r1)) 
                        begin
                            int_ecc_interrupt <= 1'b1;
                        end
                    end
                    else
                        int_ecc_interrupt <= 1'b0;
                end
                else
                    int_ecc_interrupt <= 1'b0;
            end
        end
        
        /*------------------------------------------------------------------------------
        
            Auto Correction Specifics
        
        ------------------------------------------------------------------------------*/
        if (!CTL_ECC_RMW_ENABLED)
        begin
            assign ecc_single_bit_error = 0;
            
            assign ecc_error_chip_addr  = 0;
            assign ecc_error_bank_addr  = 0;
            assign ecc_error_row_addr   = 0;
            assign ecc_error_col_addr   = 0;
        end
        else
        begin
            assign ecc_single_bit_error = !error_addr_fifo_empty;
            
            assign ecc_error_chip_addr = error_addr_fifo_rd_data [ADDR_FIFO_WIDTH - 1 : MEM_IF_BA_WIDTH + MEM_IF_ROW_WIDTH + MEM_IF_COL_WIDTH];
            assign ecc_error_bank_addr = error_addr_fifo_rd_data [MEM_IF_BA_WIDTH + MEM_IF_ROW_WIDTH + MEM_IF_COL_WIDTH - 1 : MEM_IF_ROW_WIDTH + MEM_IF_COL_WIDTH];
            assign ecc_error_row_addr  = error_addr_fifo_rd_data [MEM_IF_ROW_WIDTH + MEM_IF_COL_WIDTH - 1 : MEM_IF_COL_WIDTH];
            assign ecc_error_col_addr  = error_addr_fifo_rd_data [MEM_IF_COL_WIDTH - 1 : 0];
            
            /*------------------------------------------------------------------------------
            
                Error Address FIFO
            
            ------------------------------------------------------------------------------*/
            assign error_addr_fifo_rd      = ecc_fetch_error_addr;
            assign error_addr_fifo_wr_data = read_addr_fifo_rd_data [ADDR_FIFO_WIDTH - 1 : 0];
            
            always @ (*)
            begin
                if (ecc_enable_auto_corr) 
                begin
                    if (read_addr_fifo_rd && (|int_single_bit_error || afi_rdata_single_bit_error) && !read_addr_fifo_rd_data [ADDR_FIFO_WIDTH]) 
                        error_addr_fifo_wr = 1'b1;
                    else
                        error_addr_fifo_wr = 1'b0;
                end
                else
                    error_addr_fifo_wr = 1'b0;
            end
            
            always @ (*)
            begin
                if (error_addr_fifo_full)
                begin
					// synthesis translate_off
                    $write($time);
                    $write(" DDRX ECC Warning: Read address fifo overflow\n");
					// synthesis translate_on
                end
            end
            
            scfifo #
                (
                    .intended_device_family  (FAMILY),
                    .lpm_width               (ADDR_FIFO_WIDTH),
                    .lpm_numwords            (128),                         
                    .lpm_widthu              (7),                           
                    .almost_full_value       (16 - 4),                      
                    .lpm_type                ("scfifo"),
                    .lpm_showahead           ("ON"),
                    .overflow_checking       ("OFF"),
                    .underflow_checking      ("OFF"),
                    .use_eab                 ("ON"),
                    .add_ram_output_register ("ON")
                )
                error_addr_fifo
                (
                    .rdreq                   (error_addr_fifo_rd),
                    .aclr                    (scfifo_reset),
                    .clock                   (ctl_clk),
                    .wrreq                   (error_addr_fifo_wr),
                    .data                    (error_addr_fifo_wr_data),
                    .full                    (error_addr_fifo_full),
                    .q                       (error_addr_fifo_rd_data),
                    .sclr                    (1'b0),
                    .usedw                   (),
                    .empty                   (error_addr_fifo_empty),
                    .almost_full             (),
                    .almost_empty            ()
               );
            
            /*------------------------------------------------------------------------------
            
                Others
            
            ------------------------------------------------------------------------------*/
            
            /*------------------------------------------------------------------------------
                doing ecc read write
            ------------------------------------------------------------------------------*/
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    doing_ecc_write <= 1'b0;
                end
                else
                begin
                    if (do_write && do_ecc)
                        doing_ecc_write <= 1'b1;
                    else if (RDWR_DATA_VALID_MAX_LENGTH == 4 && ecc_wdata_fifo_read && ecc_wdata_fifo_read_r3) 
                        doing_ecc_write <= 1'b0;
                    else if (RDWR_DATA_VALID_MAX_LENGTH == 2 && ecc_wdata_fifo_read && ecc_wdata_fifo_read_r1) 
                        doing_ecc_write <= 1'b0;
                end
            end
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    doing_ecc_write_r1 <= 1'b0;
                    doing_ecc_write_r2 <= 1'b0;
                end
                else
                begin
                    doing_ecc_write_r1 <= doing_ecc_write;
                    doing_ecc_write_r2 <= doing_ecc_write_r1;
                end
            end
            
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    doing_ecc_read <= 1'b0;
                end
                else
                begin
                    if (do_read && do_ecc && !do_partial) 
                        doing_ecc_read <= 1'b1;
                    else if (do_write && do_ecc) 
                        doing_ecc_read <= 1'b0;
                end
            end
            
            /*------------------------------------------------------------------------------
                afi rdata single bit error
            ------------------------------------------------------------------------------*/
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                begin
                    afi_rdata_single_bit_error <= 1'b0;
                end
                else
                begin
                    if (read_addr_fifo_rd) 
                        afi_rdata_single_bit_error <= 1'b0;
                    else if (|int_single_bit_error && int_afi_rdata_valid_r1) 
                        afi_rdata_single_bit_error <= 1'b1;
                end
            end
        end
    end
endgenerate

/*------------------------------------------------------------------------------

    Debug Signals

------------------------------------------------------------------------------*/

reg do_ecc_r1;
reg do_partial_r1;

always @ (posedge ctl_clk or negedge ctl_reset_n)
begin
    if (!ctl_reset_n)
    begin
        do_ecc_r1     <= 1'b0;
        do_partial_r1 <= 1'b0;
    end
    else
    begin
        do_ecc_r1     <= do_ecc;
        do_partial_r1 <= do_partial;
    end
end

endmodule
