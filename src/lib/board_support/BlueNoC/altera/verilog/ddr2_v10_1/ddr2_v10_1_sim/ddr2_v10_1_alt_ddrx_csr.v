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
module ddr2_v10_1_alt_ddrx_csr #
    ( parameter
        DWIDTH_RATIO                = 2,
        CTL_CSR_ENABLED             = 0,
        CTL_ECC_CSR_ENABLED         = 0,
        CTL_CSR_READ_ONLY           = 0,
        CTL_ECC_CSR_READ_ONLY       = 0,
        CSR_ADDR_WIDTH              = 8,
        CSR_DATA_WIDTH              = 32,
        
        MEM_IF_CLK_PAIR_COUNT       = 1,
        MEM_IF_DQS_WIDTH            = 72,
        
        MEM_IF_CS_WIDTH             = 2,
        MEM_IF_CHIP_BITS            = 1,
        MEM_IF_ROW_WIDTH            = 13,      
        MEM_IF_COL_WIDTH            = 10,      
        MEM_IF_BA_WIDTH             = 3,       
        
        CTL_ECC_ENABLED             = 1,
        CTL_ECC_RMW_ENABLED         = 1,
        CTL_REGDIMM_ENABLED         = 0,
        
        CAS_WR_LAT_BUS_WIDTH        = 4,       
        ADD_LAT_BUS_WIDTH           = 3,       
        TCL_BUS_WIDTH               = 4,       
        TRRD_BUS_WIDTH              = 4,       
        TFAW_BUS_WIDTH              = 6,       
        TRFC_BUS_WIDTH              = 8,       
        TREFI_BUS_WIDTH             = 13,      
        TRCD_BUS_WIDTH              = 4,       
        TRP_BUS_WIDTH               = 4,       
        TWR_BUS_WIDTH               = 4,       
        TWTR_BUS_WIDTH              = 4,       
        TRTP_BUS_WIDTH              = 4,       
        TRAS_BUS_WIDTH              = 5,       
        TRC_BUS_WIDTH               = 6,       
        AUTO_PD_BUS_WIDTH           = 16,      
        
        MEM_CAS_WR_LAT              = 0,        
        MEM_ADD_LAT                 = 0,        
        MEM_TCL                     = 0,        
        MEM_TRRD                    = 0,        
        MEM_TFAW                    = 0,        
        MEM_TRFC                    = 0,        
        MEM_TREFI                   = 0,        
        MEM_TRCD                    = 0,        
        MEM_TRP                     = 0,        
        MEM_TWR                     = 0,        
        MEM_TWTR                    = 0,        
        MEM_TRTP                    = 0,        
        MEM_TRAS                    = 0,        
        MEM_TRC                     = 0,        
        MEM_AUTO_PD_CYCLES          = 0,        
        
        ADDR_ORDER                  = 1,        
        MEM_IF_CSR_COL_WIDTH        = 4,
        MEM_IF_CSR_ROW_WIDTH        = 5,
        MEM_IF_CSR_BANK_WIDTH       = 2,
        MEM_IF_CSR_CS_WIDTH         = 2
    )
    (
        ctl_clk,
        ctl_reset_n,
        
        csr_addr,
        csr_be,
        csr_write_req,
        csr_wdata,
        csr_read_req,
        csr_rdata,
        csr_rdata_valid,
        csr_waitrequest,
        
        ctl_cal_success,
        ctl_cal_fail,
        
        local_power_down_ack,
        local_self_rfsh_ack,
        
        ecc_sbe_error,
        ecc_dbe_error,
        ecc_sbe_count,
        ecc_dbe_count,
        ecc_error_addr,
        
        ctl_cal_req,
        ctl_mem_clk_disable,
        ctl_cal_byte_lane_sel_n,
        
        mem_cas_wr_lat,
        mem_add_lat,
        mem_tcl,
        mem_trrd,
        mem_tfaw,
        mem_trfc,
        mem_trefi,
        mem_trcd,
        mem_trp,
        mem_twr,
        mem_twtr,
        mem_trtp,
        mem_tras,
        mem_trc,
        mem_auto_pd_cycles,
        
        addr_order,
        col_width_from_csr,
        row_width_from_csr,
        bank_width_from_csr,
        cs_width_from_csr,
        
        ecc_enable,
        ecc_enable_auto_corr,
        ecc_gen_sbe,
        ecc_gen_dbe,
        ecc_enable_intr,
        ecc_mask_sbe_intr,
        ecc_mask_dbe_intr,
        ecc_clear,

        regdimm_enable
    );

input ctl_clk;
input ctl_reset_n;

input csr_write_req;
input csr_read_req;
input [CSR_ADDR_WIDTH - 1       : 0] csr_addr;
input [CSR_DATA_WIDTH - 1       : 0] csr_wdata;
input [(CSR_DATA_WIDTH / 8) - 1 : 0] csr_be;

output csr_waitrequest;
output csr_rdata_valid;
output [CSR_DATA_WIDTH - 1 : 0] csr_rdata;

input ctl_cal_success;
input ctl_cal_fail;

input local_power_down_ack;
input local_self_rfsh_ack;

input           ecc_sbe_error;
input           ecc_dbe_error;
input  [7  : 0] ecc_sbe_count;
input  [7  : 0] ecc_dbe_count;
input  [31 : 0] ecc_error_addr;

output                                              ctl_cal_req;
output [MEM_IF_CLK_PAIR_COUNT              - 1 : 0] ctl_mem_clk_disable;
output [MEM_IF_DQS_WIDTH * MEM_IF_CS_WIDTH - 1 : 0] ctl_cal_byte_lane_sel_n;

output [CAS_WR_LAT_BUS_WIDTH - 1 : 0] mem_cas_wr_lat;
output [ADD_LAT_BUS_WIDTH    - 1 : 0] mem_add_lat;
output [TCL_BUS_WIDTH        - 1 : 0] mem_tcl;
output [TRRD_BUS_WIDTH       - 1 : 0] mem_trrd;
output [TFAW_BUS_WIDTH       - 1 : 0] mem_tfaw;
output [TRFC_BUS_WIDTH       - 1 : 0] mem_trfc;
output [TREFI_BUS_WIDTH      - 1 : 0] mem_trefi;
output [TRCD_BUS_WIDTH       - 1 : 0] mem_trcd;
output [TRP_BUS_WIDTH        - 1 : 0] mem_trp;
output [TWR_BUS_WIDTH        - 1 : 0] mem_twr;
output [TWTR_BUS_WIDTH       - 1 : 0] mem_twtr;
output [TRTP_BUS_WIDTH       - 1 : 0] mem_trtp;
output [TRAS_BUS_WIDTH       - 1 : 0] mem_tras;
output [TRC_BUS_WIDTH        - 1 : 0] mem_trc;
output [AUTO_PD_BUS_WIDTH    - 1 : 0] mem_auto_pd_cycles;

output [1 : 0]                         addr_order;
output [MEM_IF_CSR_COL_WIDTH  - 1 : 0] col_width_from_csr;
output [MEM_IF_CSR_ROW_WIDTH  - 1 : 0] row_width_from_csr;
output [MEM_IF_CSR_BANK_WIDTH - 1 : 0] bank_width_from_csr;
output [MEM_IF_CSR_CS_WIDTH   - 1 : 0] cs_width_from_csr;

output ecc_enable;
output ecc_enable_auto_corr;
output ecc_gen_sbe;
output ecc_gen_dbe;
output ecc_enable_intr;
output ecc_mask_sbe_intr;
output ecc_mask_dbe_intr;
output ecc_clear;

output regdimm_enable;

wire csr_waitrequest;
wire csr_rdata_valid;
wire [CSR_DATA_WIDTH - 1 : 0] csr_rdata;

reg int_write_req;
reg int_read_req;
reg int_rdata_valid;
reg [8              - 1       : 0] int_addr; 
reg [CSR_DATA_WIDTH - 1       : 0] int_wdata;
reg [CSR_DATA_WIDTH - 1       : 0] int_rdata;
reg [(CSR_DATA_WIDTH / 8) - 1 : 0] int_be;

reg int_mask_csr_write_req;
reg int_mask_csr_read_req;
reg int_mask_ecc_csr_write_req;
reg int_mask_ecc_csr_read_req;

wire                                              ctl_cal_req;
wire [MEM_IF_CLK_PAIR_COUNT              - 1 : 0] ctl_mem_clk_disable;
wire [MEM_IF_DQS_WIDTH * MEM_IF_CS_WIDTH - 1 : 0] ctl_cal_byte_lane_sel_n;

wire [CAS_WR_LAT_BUS_WIDTH - 1 : 0] mem_cas_wr_lat;
wire [ADD_LAT_BUS_WIDTH    - 1 : 0] mem_add_lat;
wire [TCL_BUS_WIDTH        - 1 : 0] mem_tcl;
wire [TRRD_BUS_WIDTH       - 1 : 0] mem_trrd;
wire [TFAW_BUS_WIDTH       - 1 : 0] mem_tfaw;
wire [TRFC_BUS_WIDTH       - 1 : 0] mem_trfc;
wire [TREFI_BUS_WIDTH      - 1 : 0] mem_trefi;
wire [TRCD_BUS_WIDTH       - 1 : 0] mem_trcd;
wire [TRP_BUS_WIDTH        - 1 : 0] mem_trp;
wire [TWR_BUS_WIDTH        - 1 : 0] mem_twr;
wire [TWTR_BUS_WIDTH       - 1 : 0] mem_twtr;
wire [TRTP_BUS_WIDTH       - 1 : 0] mem_trtp;
wire [TRAS_BUS_WIDTH       - 1 : 0] mem_tras;
wire [TRC_BUS_WIDTH        - 1 : 0] mem_trc;
wire [AUTO_PD_BUS_WIDTH    - 1 : 0] mem_auto_pd_cycles;

wire [1 : 0]                         addr_order;
wire [MEM_IF_CSR_COL_WIDTH  - 1 : 0] col_width_from_csr;
wire [MEM_IF_CSR_ROW_WIDTH  - 1 : 0] row_width_from_csr;
wire [MEM_IF_CSR_BANK_WIDTH - 1 : 0] bank_width_from_csr;
wire [MEM_IF_CSR_CS_WIDTH   - 1 : 0] cs_width_from_csr;

wire ecc_enable;
wire ecc_enable_auto_corr;
wire ecc_gen_sbe;
wire ecc_gen_dbe;
wire ecc_enable_intr;
wire ecc_mask_sbe_intr;
wire ecc_mask_dbe_intr;
wire ecc_clear;

wire regdimm_enable;

reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_100;

reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_110;

reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_120;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_121;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_122;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_123;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_124;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_125;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_126;

reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_130;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_131;
reg [CSR_DATA_WIDTH - 1 : 0] read_csr_register_132;

/*------------------------------------------------------------------------------

   CSR Interface

------------------------------------------------------------------------------*/
assign csr_waitrequest = 1'b0;

generate
    if (!CTL_CSR_ENABLED && !CTL_ECC_CSR_ENABLED)
    begin
        assign csr_rdata       = 0;
        assign csr_rdata_valid = 0;
    end
    else
    begin
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                int_write_req <= 0;
                int_read_req  <= 0;
                int_addr      <= 0;
                int_wdata     <= 0;
                int_be        <= 0;
            end
            else
            begin
                int_addr  <= csr_addr [7 : 0]; 
                int_wdata <= csr_wdata;
                int_be    <= csr_be;
                
                if (csr_write_req)
                    int_write_req <= 1'b1;
                else
                    int_write_req <= 1'b0;
                
                if (csr_read_req)
                    int_read_req <= 1'b1;
                else
                    int_read_req <= 1'b0;
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                int_mask_csr_write_req     <= 1'b0;
                int_mask_csr_read_req      <= 1'b0;
                int_mask_ecc_csr_write_req <= 1'b0;
                int_mask_ecc_csr_read_req  <= 1'b0;
            end
            else
            begin
                if (CTL_CSR_READ_ONLY)
                begin
                    int_mask_csr_write_req <= 1'b1;
                    int_mask_csr_read_req  <= 1'b0;
                end
                else
                begin
                    int_mask_csr_write_req <= 1'b0;
                    int_mask_csr_read_req  <= 1'b0;
                end
                
                if (CTL_ECC_CSR_READ_ONLY)
                begin
                    int_mask_ecc_csr_write_req <= 1'b1;
                    int_mask_ecc_csr_read_req  <= 1'b0;
                end
                else
                begin
                    int_mask_ecc_csr_write_req <= 1'b0;
                    int_mask_ecc_csr_read_req  <= 1'b0;
                end
            end
        end
        
        /*------------------------------------------------------------------------------
        
           Read Interface
        
        ------------------------------------------------------------------------------*/
        assign csr_rdata       = int_rdata;
        assign csr_rdata_valid = int_rdata_valid;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                int_rdata       <= 0;
                int_rdata_valid <= 0;
            end
            else
            begin
                if (int_read_req)
                begin
                    if (int_addr == 8'h00)
                        int_rdata <= read_csr_register_100;
                    else if (int_addr == 8'h10)
                        int_rdata <= read_csr_register_110;
                    else if (int_addr == 8'h20)
                        int_rdata <= read_csr_register_120;
                    else if (int_addr == 8'h21)
                        int_rdata <= read_csr_register_121;
                    else if (int_addr == 8'h22)
                        int_rdata <= read_csr_register_122;
                    else if (int_addr == 8'h23)
                        int_rdata <= read_csr_register_123;
                    else if (int_addr == 8'h24)
                        int_rdata <= read_csr_register_124;
                    else if (int_addr == 8'h25)
                        int_rdata <= read_csr_register_125;
                    else if (int_addr == 8'h26)
                        int_rdata <= read_csr_register_126;
                    else if (int_addr == 8'h30)
                        int_rdata <= read_csr_register_130;
                    else if (int_addr == 8'h31)
                        int_rdata <= read_csr_register_131;
                    else if (int_addr == 8'h32)
                        int_rdata <= read_csr_register_132;
                end
                
                if (int_read_req)
                    int_rdata_valid <= 1'b1;
                else
                    int_rdata_valid <= 1'b0;
            end
        end
    end
endgenerate

/*------------------------------------------------------------------------------

   CSR Registers

------------------------------------------------------------------------------*/
generate
    genvar i;
    if (!CTL_CSR_ENABLED) 
    begin
        assign mem_cas_wr_lat          = MEM_CAS_WR_LAT;
        assign mem_add_lat             = MEM_ADD_LAT;
        assign mem_tcl                 = MEM_TCL;
        assign mem_trrd                = MEM_TRRD;
        assign mem_tfaw                = MEM_TFAW;
        assign mem_trfc                = MEM_TRFC;
        assign mem_trefi               = MEM_TREFI;
        assign mem_trcd                = MEM_TRCD;
        assign mem_trp                 = MEM_TRP;
        assign mem_twr                 = MEM_TWR;
        assign mem_twtr                = MEM_TWTR;
        assign mem_trtp                = MEM_TRTP;
        assign mem_tras                = MEM_TRAS;
        assign mem_trc                 = MEM_TRC;
        assign mem_auto_pd_cycles      = MEM_AUTO_PD_CYCLES;
        
        assign addr_order              = ADDR_ORDER;
        assign cs_width_from_csr       = MEM_IF_CS_WIDTH > 1 ? MEM_IF_CHIP_BITS : 0;
        assign bank_width_from_csr     = MEM_IF_BA_WIDTH;
        assign row_width_from_csr      = MEM_IF_ROW_WIDTH;
        assign col_width_from_csr      = MEM_IF_COL_WIDTH;
        
        assign ctl_cal_req             = 0;
        assign ctl_mem_clk_disable     = 0;
        assign ctl_cal_byte_lane_sel_n = 0;
        
        assign regdimm_enable          = 1'b1; 
    end
    else
    begin
        /*------------------------------------------------------------------------------
        
           0x100 ALTMEPHY Status and Control Register
        
        ------------------------------------------------------------------------------*/
        reg         csr_cal_success;
        reg         csr_cal_fail;
        reg         csr_cal_req;
        reg [5 : 0] csr_clock_off;
        
        assign ctl_cal_req         = csr_cal_req;
        assign ctl_mem_clk_disable = csr_clock_off [MEM_IF_CLK_PAIR_COUNT - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_cal_req     <= 0;
                csr_clock_off   <= 0;
            end
            else
            begin
                if (int_write_req && int_addr == 8'h00)
                begin
                    if (int_be [0])
                    begin
                        csr_cal_req   <= int_wdata [2]     ;
                    end
                    
                    if (int_be [1])
                    begin
                        csr_clock_off <= int_wdata [13 : 8];
                    end
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_cal_success <= 0;
                csr_cal_fail    <= 0;
            end
            else
            begin
                csr_cal_success <= ctl_cal_success;
                csr_cal_fail    <= ctl_cal_fail;
            end
        end
        
        always @ (*)
        begin
            read_csr_register_100 = 0;
            
            read_csr_register_100 [0]      = csr_cal_success;
            read_csr_register_100 [1]      = csr_cal_fail;
            read_csr_register_100 [2]      = csr_cal_req;
            read_csr_register_100 [13 : 8] = csr_clock_off;
        end
        
        /*------------------------------------------------------------------------------
        
           0x110 Controller Status and Control Register
        
        ------------------------------------------------------------------------------*/
        reg [15 : 0] csr_auto_pd_cycles;
        reg          csr_auto_pd_ack;
        reg          csr_self_rfsh;    
        reg          csr_self_rfsh_ack;
        reg          csr_ganged_arf;   
        reg [1  : 0] csr_addr_order;
        reg          csr_reg_dimm;     
        reg [1  : 0] csr_drate;
        
        assign mem_auto_pd_cycles = csr_auto_pd_cycles;
        assign addr_order         = csr_addr_order;
        assign regdimm_enable     = csr_reg_dimm;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_auto_pd_cycles <= MEM_AUTO_PD_CYCLES;  
                csr_self_rfsh      <= 0;
                csr_ganged_arf     <= 0;
                csr_addr_order     <= 2'b01;               
                csr_reg_dimm       <= CTL_REGDIMM_ENABLED; 
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h10)
                begin
                    if (int_be [0])
                    begin
                        csr_auto_pd_cycles [ 7 :  0] <= int_wdata [ 7 :  0];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_auto_pd_cycles [15 :  8] <= int_wdata [15 :  8];
                    end
                    
                    if (int_be [2])
                    begin
                        csr_self_rfsh      <= int_wdata [17]     ;
                        csr_ganged_arf     <= int_wdata [19]     ;
                        csr_addr_order     <= int_wdata [21 : 20];
                        csr_reg_dimm       <= int_wdata [22]     ;
                    end
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_auto_pd_ack   <= 0;
                csr_self_rfsh_ack <= 0;
                csr_drate         <= 0;
            end
            else
            begin
                csr_auto_pd_ack   <= local_power_down_ack;
                csr_self_rfsh_ack <= local_self_rfsh_ack;
                csr_drate         <= (DWIDTH_RATIO == 2) ? 2'b00 : 2'b01; 
            end
        end
        
        always @ (*)
        begin
            read_csr_register_110 = 0;
            
            read_csr_register_110 [15 : 0 ] = csr_auto_pd_cycles;
            read_csr_register_110 [16]      = csr_auto_pd_ack;
            read_csr_register_110 [17]      = csr_self_rfsh;
            read_csr_register_110 [18]      = csr_self_rfsh_ack;
            read_csr_register_110 [19]      = csr_ganged_arf;
            read_csr_register_110 [21 : 20] = csr_addr_order;
            read_csr_register_110 [22]      = csr_reg_dimm;
        end
        
        /*------------------------------------------------------------------------------
        
           0x120 Memory Address Sizes 0
        
        ------------------------------------------------------------------------------*/
        reg [7 : 0] csr_col_width;
        reg [7 : 0] csr_row_width;
        reg [3 : 0] csr_bank_width;
        reg [3 : 0] csr_chip_width;
        
        assign cs_width_from_csr    = csr_chip_width [MEM_IF_CSR_CS_WIDTH   - 1 : 0];
        assign bank_width_from_csr  = csr_bank_width [MEM_IF_CSR_BANK_WIDTH - 1 : 0];
        assign row_width_from_csr   = csr_row_width  [MEM_IF_CSR_ROW_WIDTH  - 1 : 0];
        assign col_width_from_csr   = csr_col_width  [MEM_IF_CSR_COL_WIDTH  - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_col_width  <= MEM_IF_COL_WIDTH;                           
                csr_row_width  <= MEM_IF_ROW_WIDTH;                           
                csr_bank_width <= MEM_IF_BA_WIDTH;                            
                csr_chip_width <= MEM_IF_CS_WIDTH > 1 ? MEM_IF_CHIP_BITS : 0; 
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h20)
                begin
                    if (int_be [0])
                    begin
                        if (int_wdata [7 : 0] <= MEM_IF_COL_WIDTH)
                        begin
                            csr_col_width  <= int_wdata [7  : 0 ];
                        end
                    end
                    
                    if (int_be [1])
                    begin
                        if (int_wdata [15 : 8] <= MEM_IF_ROW_WIDTH)
                        begin
                            csr_row_width  <= int_wdata [15 : 8 ];
                        end
                    end
                    
                    if (int_be [2])
                    begin
                        if (int_wdata [19 : 16] <= MEM_IF_BA_WIDTH)
                        begin
                            csr_bank_width <= int_wdata [19 : 16];
                        end
                        
                        if (int_wdata [23 : 20] <= (MEM_IF_CS_WIDTH > 1 ? MEM_IF_CHIP_BITS : 0))
                        begin
                            csr_chip_width <= int_wdata [23 : 20];
                        end
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_120 = 0;
            
            read_csr_register_120 [7  : 0 ] = csr_col_width;
            read_csr_register_120 [15 : 8 ] = csr_row_width;
            read_csr_register_120 [19 : 16] = csr_bank_width;
            read_csr_register_120 [23 : 20] = csr_chip_width;
        end
        
        /*------------------------------------------------------------------------------
        
           0x121 Memory Address Sizes 1
        
        ------------------------------------------------------------------------------*/
        reg [31 : 0] csr_data_binary_representation;
        reg [7 : 0] csr_chip_binary_representation;
        
        reg [MEM_IF_DQS_WIDTH * MEM_IF_CS_WIDTH - 1 : 0] cal_byte_lane;
        
        assign ctl_cal_byte_lane_sel_n = ~cal_byte_lane;
        
        for (i = 0;i < MEM_IF_CS_WIDTH;i = i + 1)
        begin : ctl_cal_byte_lane_per_chip
            always @ (posedge ctl_clk or negedge ctl_reset_n)
            begin
                if (!ctl_reset_n)
                    cal_byte_lane [(i + 1) * MEM_IF_DQS_WIDTH - 1 : i * MEM_IF_DQS_WIDTH] <= {MEM_IF_DQS_WIDTH{1'b1}}; 
                else
                begin
                    if (csr_chip_binary_representation[i])
                        cal_byte_lane [(i + 1) * MEM_IF_DQS_WIDTH - 1 : i * MEM_IF_DQS_WIDTH] <= csr_data_binary_representation [MEM_IF_DQS_WIDTH - 1 : 0];
                    else
                        cal_byte_lane [(i + 1) * MEM_IF_DQS_WIDTH - 1 : i * MEM_IF_DQS_WIDTH] <= 0;
                end
            end
        end
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_data_binary_representation <= {MEM_IF_DQS_WIDTH{1'b1}};
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h21)
                begin
                    if (int_be [0])
                    begin
                        csr_data_binary_representation [ 7 :  0] <= int_wdata [ 7 :  0];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_data_binary_representation [15 :  8] <= int_wdata [15 :  8];
                    end
                    
                    if (int_be [2])
                    begin
                        csr_data_binary_representation [23 : 16] <= int_wdata [23 : 16];
                    end
                    
                    if (int_be [3])
                    begin
                        csr_data_binary_representation [31 : 24] <= int_wdata [31 : 24];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_121 = 0;
            
            read_csr_register_121 [31 : 0 ] = csr_data_binary_representation;
        end
        
        /*------------------------------------------------------------------------------
        
           0x122 Memory Address Sizes 2
        
        ------------------------------------------------------------------------------*/
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_chip_binary_representation <= {MEM_IF_CS_WIDTH{1'b1}};
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h22)
                begin
                    if (int_be [0])
                    begin
                        csr_chip_binary_representation [ 7 :  0] <= int_wdata [7  : 0 ];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_122 = 0;
            
            read_csr_register_122 [7  : 0 ] = csr_chip_binary_representation;
        end
        
        /*------------------------------------------------------------------------------
        
           0x123 Memory Timing Parameters Registers 0
        
        ------------------------------------------------------------------------------*/
        reg [3 : 0] csr_trcd;
        reg [3 : 0] csr_trrd;
        reg [3 : 0] csr_trp;
        reg [3 : 0] csr_tmrd; 
        reg [7 : 0] csr_tras;
        reg [7 : 0] csr_trc;
        
        assign mem_trcd = csr_trcd [TRCD_BUS_WIDTH - 1 : 0];
        assign mem_trrd = csr_trrd [TRRD_BUS_WIDTH - 1 : 0];
        assign mem_trp  = csr_trp  [TRP_BUS_WIDTH  - 1 : 0];
        assign mem_tras = csr_tras [TRAS_BUS_WIDTH - 1 : 0];
        assign mem_trc  = csr_trc  [TRC_BUS_WIDTH  - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_trcd <= MEM_TRCD; 
                csr_trrd <= MEM_TRRD; 
                csr_trp  <= MEM_TRP;  
                csr_tmrd <= 0;        
                csr_tras <= MEM_TRAS; 
                csr_trc  <= MEM_TRC;  
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h23)
                begin
                    if (int_be [0])
                    begin
                        csr_trcd <= int_wdata [3  : 0 ];
                        csr_trrd <= int_wdata [7  : 4 ];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_trp  <= int_wdata [11 : 8 ];
                        csr_tmrd <= int_wdata [15 : 12];
                    end
                    
                    if (int_be [2])
                    begin
                        csr_tras <= int_wdata [23 : 16];
                    end
                    
                    if (int_be [3])
                    begin
                        csr_trc  <= int_wdata [31 : 24];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_123 = 0;
            
            read_csr_register_123 [3  : 0 ] = csr_trcd;
            read_csr_register_123 [7  : 4 ] = csr_trrd;
            read_csr_register_123 [11 : 8 ] = csr_trp;
            read_csr_register_123 [15 : 12] = csr_tmrd;
            read_csr_register_123 [23 : 16] = csr_tras;
            read_csr_register_123 [31 : 24] = csr_trc;
        end
        
        /*------------------------------------------------------------------------------
        
           0x124 Memory Timing Parameters Registers 1
        
        ------------------------------------------------------------------------------*/
        reg [3 : 0] csr_twtr;
        reg [3 : 0] csr_trtp;
        reg [7 : 0] csr_tfaw;
        
        assign mem_twtr = csr_twtr [TWTR_BUS_WIDTH - 1 : 0];
        assign mem_trtp = csr_trtp [TRTP_BUS_WIDTH - 1 : 0];
        assign mem_tfaw = csr_tfaw [TFAW_BUS_WIDTH - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_twtr <= MEM_TWTR;
                csr_trtp <= MEM_TRTP;
                csr_tfaw <= MEM_TFAW;
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h24)
                begin
                    if (int_be [0])
                    begin
                        csr_twtr <= int_wdata [3  : 0 ];
                        csr_trtp <= int_wdata [7  : 4 ];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_tfaw <= int_wdata [15 : 8 ];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_124 = 0;
            
            read_csr_register_124 [3  : 0 ] = csr_twtr;
            read_csr_register_124 [7  : 4 ] = csr_trtp;
            read_csr_register_124 [15 : 8 ] = csr_tfaw;
        end
        
        /*------------------------------------------------------------------------------
        
           0x125 Memory Timing Parameters Registers 2
        
        ------------------------------------------------------------------------------*/
        reg [15 : 0] csr_trefi;
        reg [7  : 0] csr_trfc;
        
        assign mem_trefi = csr_trefi [TREFI_BUS_WIDTH - 1 : 0];
        assign mem_trfc  = csr_trfc  [TRFC_BUS_WIDTH  - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_trefi <= MEM_TREFI;
                csr_trfc  <= MEM_TRFC;
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h25)
                begin
                    if (int_be [0])
                    begin
                        csr_trefi [ 7 :  0] <= int_wdata [ 7 :  0];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_trefi [15 :  8] <= int_wdata [15 :  8];
                    end
                    
                    if (int_be [2])
                    begin
                        csr_trfc  <= int_wdata [23 : 16];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_125 = 0;
            
            read_csr_register_125 [15 : 0 ] = csr_trefi;
            read_csr_register_125 [23 : 16] = csr_trfc;
        end
        
        /*------------------------------------------------------------------------------
        
           0x126 Memory Timing Parameters Registers 3
        
        ------------------------------------------------------------------------------*/
        reg [3 : 0] csr_tcl;
        reg [3 : 0] csr_al;
        reg [3 : 0] csr_cwl;
        reg [3 : 0] csr_twr;
        
        assign mem_tcl        = csr_tcl [TCL_BUS_WIDTH        - 1 : 0];
        assign mem_add_lat    = csr_al  [ADD_LAT_BUS_WIDTH    - 1 : 0];
        assign mem_cas_wr_lat = csr_cwl [CAS_WR_LAT_BUS_WIDTH - 1 : 0];
        assign mem_twr        = csr_twr [TWR_BUS_WIDTH        - 1 : 0];
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_tcl <= MEM_TCL;
                csr_al  <= MEM_ADD_LAT;
                csr_cwl <= MEM_CAS_WR_LAT;
                csr_twr <= MEM_TWR;
            end
            else
            begin
                if (!int_mask_csr_write_req && int_write_req && int_addr == 8'h26)
                begin
                    if (int_be [0])
                    begin
                        csr_tcl <= int_wdata [3  : 0 ];
                        csr_al  <= int_wdata [7  : 4 ];
                    end
                    
                    if (int_be [1])
                    begin
                        csr_cwl <= int_wdata [11 : 8 ];
                        csr_twr <= int_wdata [15 : 12];
                    end
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_126 = 0;
            
            read_csr_register_126 [3  : 0 ] = csr_tcl;
            read_csr_register_126 [7  : 4 ] = csr_al;
            read_csr_register_126 [11 : 8 ] = csr_cwl;
            read_csr_register_126 [15 : 12] = csr_twr;
        end
    end
    
    if (!CTL_ECC_CSR_ENABLED)
    begin
        assign ecc_enable              = 1'b1; 
        assign ecc_enable_auto_corr    = 1'b1; 
        assign ecc_gen_sbe             = 0;
        assign ecc_gen_dbe             = 0;
        assign ecc_enable_intr         = 1'b1; 
        assign ecc_mask_sbe_intr       = 0;
        assign ecc_mask_dbe_intr       = 0;
        assign ecc_clear               = 0;
    end
    else
    begin
        /*------------------------------------------------------------------------------
        
           0x130 ECC Control Register
        
        ------------------------------------------------------------------------------*/
        reg csr_enable_ecc;
        reg csr_enable_auto_corr;
        reg csr_gen_sbe;
        reg csr_gen_dbe;
        reg csr_enable_intr;
        reg csr_mask_sbe_intr;
        reg csr_mask_dbe_intr;
        reg csr_ecc_clear;
        
        assign ecc_enable           = csr_enable_ecc;
        assign ecc_enable_auto_corr = csr_enable_auto_corr;
        assign ecc_gen_sbe          = csr_gen_sbe;
        assign ecc_gen_dbe          = csr_gen_dbe;
        assign ecc_enable_intr      = csr_enable_intr;
        assign ecc_mask_sbe_intr    = csr_mask_sbe_intr;
        assign ecc_mask_dbe_intr    = csr_mask_dbe_intr;
        assign ecc_clear            = csr_ecc_clear;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_enable_ecc       <= CTL_ECC_ENABLED;
                csr_enable_auto_corr <= CTL_ECC_RMW_ENABLED;
                csr_gen_sbe          <= 0;
                csr_gen_dbe          <= 0;
                csr_enable_intr      <= 1'b1;
                csr_mask_sbe_intr    <= 0;
                csr_mask_dbe_intr    <= 0;
                csr_ecc_clear        <= 0;
            end
            else
            begin
                if (!int_mask_ecc_csr_write_req && int_write_req && int_addr == 8'h30)
                begin
                    if (int_be [0])
                    begin
                        csr_enable_ecc       <= int_wdata [0];
                        csr_enable_auto_corr <= int_wdata [1];
                        csr_gen_sbe          <= int_wdata [2];
                        csr_gen_dbe          <= int_wdata [3];
                        csr_enable_intr      <= int_wdata [4];
                        csr_mask_sbe_intr    <= int_wdata [5];
                        csr_mask_dbe_intr    <= int_wdata [6];
                        csr_ecc_clear        <= int_wdata [7];
                    end
                end
                
                if (csr_ecc_clear)
                    csr_ecc_clear     <= 1'b0;
            end
        end
        
        always @ (*)
        begin
            read_csr_register_130 = 0;
            
            read_csr_register_130 [0] = csr_enable_ecc;
            read_csr_register_130 [1] = csr_enable_auto_corr;
            read_csr_register_130 [2] = csr_gen_sbe;
            read_csr_register_130 [3] = csr_gen_dbe;
            read_csr_register_130 [4] = csr_enable_intr;
            read_csr_register_130 [5] = csr_mask_sbe_intr;
            read_csr_register_130 [6] = csr_mask_dbe_intr;
            read_csr_register_130 [7] = csr_ecc_clear;
        end
        
        /*------------------------------------------------------------------------------
        
           0x131 ECC Status Register (Read Only)
        
        ------------------------------------------------------------------------------*/
        reg         csr_sbe_error;
        reg         csr_dbe_error;
        reg [7 : 0] csr_sbe_count;
        reg [7 : 0] csr_dbe_count;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_sbe_error <= 0;
                csr_dbe_error <= 0;
                csr_sbe_count <= 0;
                csr_dbe_count <= 0;
            end
            else
            begin
                if (csr_ecc_clear)
                begin
                    csr_sbe_error <= 0;
                    csr_dbe_error <= 0;
                    csr_sbe_count <= 0;
                    csr_dbe_count <= 0;
                end
                else
                begin
                    csr_sbe_error <= ecc_sbe_error;
                    csr_dbe_error <= ecc_dbe_error;
                    csr_sbe_count <= ecc_sbe_count;
                    csr_dbe_count <= ecc_dbe_count;
                end
            end
        end
        
        always @ (*)
        begin
            read_csr_register_131 = 0;
            
            read_csr_register_131 [0      ] = csr_sbe_error;
            read_csr_register_131 [1      ] = csr_dbe_error;
            read_csr_register_131 [15 : 8 ] = csr_sbe_count;
            read_csr_register_131 [23 : 16] = csr_dbe_count;
        end
        
        /*------------------------------------------------------------------------------
        
           0x132 ECC Error Address Register (Read Only)
        
        ------------------------------------------------------------------------------*/
        reg [31 : 0] csr_error_addr;
        
        always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
            begin
                csr_error_addr <= 0;
            end
            else
            begin
                if (csr_ecc_clear)
                    csr_error_addr <= 0;
                else
                    csr_error_addr <= ecc_error_addr;
            end
        end
        
        always @ (*)
        begin
            read_csr_register_132 = csr_error_addr;
        end
    end
endgenerate
























endmodule
