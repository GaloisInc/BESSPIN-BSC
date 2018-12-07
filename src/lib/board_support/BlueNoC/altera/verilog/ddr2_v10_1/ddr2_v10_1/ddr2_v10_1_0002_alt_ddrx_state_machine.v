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
module ddr2_v10_1_0002_alt_ddrx_state_machine
    # (parameter
    
        CTL_LOOK_AHEAD_DEPTH    = 4,
        WDATA_BEATS_WIDTH       = 6, 
        MEM_TYPE                = "DDR3",
        DWIDTH_RATIO            = 4,
        MEMORY_BURSTLENGTH      = 8,
        CTL_CSR_ENABLED         = 0,
        CTL_ECC_ENABLED         = 0, 
        CTL_REGDIMM_ENABLED     = 0, 
        CTL_USR_REFRESH         = 0, 
        ENABLE_AUTO_AP_LOGIC    = 1, 
        CLOSE_PAGE_POLICY       = 0,
        LOW_LATENCY             = 1,
        MULTICAST_WR_EN         = 0,
        
        MEM_IF_CHIP_BITS        = 1,
        MEM_IF_CS_WIDTH         = 2,
        MEM_IF_BA_WIDTH         = 3,  
        MEM_IF_ROW_WIDTH        = 13, 
        MEM_IF_COL_WIDTH        = 10, 
        MEM_IF_CSR_CS_WIDTH     = 2,
        MEM_IF_CSR_BANK_WIDTH   = 3,
        MEM_IF_CSR_ROW_WIDTH    = 13,
        MEM_IF_CSR_COL_WIDTH    = 10
        
    )
    (
    
        ctl_clk,
        ctl_reset_n,
        ctl_cal_success,
        
        cmd_fifo_empty,
        cmd_fifo_wren,
        write_req_to_wfifo,
        fetch,
        
        cmd0_is_a_read,
        cmd0_is_a_write,
        cmd0_autopch_req,
        cmd0_multicast_req,
        cmd0_burstcount,
        cmd0_chip_addr,
        cmd0_bank_addr,
        cmd0_row_addr,
        cmd0_col_addr,
        cmd0_is_valid,
        
        cmd1_multicast_req,
        cmd1_chip_addr,
        cmd1_bank_addr,
        cmd1_row_addr,
        cmd1_is_valid,
        
        cmd2_multicast_req,
        cmd2_chip_addr,
        cmd2_bank_addr,
        cmd2_row_addr,
        cmd2_is_valid,
        
        cmd3_multicast_req,
        cmd3_chip_addr,
        cmd3_bank_addr,
        cmd3_row_addr,
        cmd3_is_valid,
        
        cmd4_multicast_req,
        cmd4_chip_addr,
        cmd4_bank_addr,
        cmd4_row_addr,
        cmd4_is_valid,
        
        cmd5_multicast_req,
        cmd5_chip_addr,
        cmd5_bank_addr,
        cmd5_row_addr,
        cmd5_is_valid,
        
        cmd6_multicast_req,
        cmd6_chip_addr,
        cmd6_bank_addr,
        cmd6_row_addr,
        cmd6_is_valid,
        
        cmd7_multicast_req,
        cmd7_chip_addr,
        cmd7_bank_addr,
        cmd7_row_addr,
        cmd7_is_valid,
        
        current_is_a_read,
        current_is_a_write,
        current_chip_addr,
        current_bank_addr,
        current_row_addr,
        current_multicast_req,
        
        all_banks_closed,
        bank_info_valid,
        bank_is_open,
        row_is_open,
        current_bank_info_valid,
        current_bank_is_open,
        current_row_is_open,
        
        zq_cal_req,
        add_lat_on,
        can_al_activate_write,
        can_al_activate_read,
        can_activate,            
        can_precharge,            
        can_read_current,
        can_write_current,
        can_activate_current,
        can_precharge_current,
        can_lmr,            
        can_precharge_all,
        can_refresh,
        can_enter_power_down,          
        can_exit_power_saving_mode,
        can_self_rfsh,           
        cam_full,
        
        auto_refresh_req,     
        auto_refresh_chip,
        
        local_refresh_req,       
        local_refresh_chip,      
        local_refresh_ack,
        
        power_down_req,          
        local_power_down_ack,
        
        local_self_rfsh_req,     
        local_self_rfsh_chip,    
        local_self_rfsh_ack,     
        
        do_ecc,
        do_partial,
        ecc_fetch_error_addr,
        wdata_is_partial,
        ecc_single_bit_error,
        rmw_data_ready,
        ecc_error_chip_addr,
        ecc_error_bank_addr,
        ecc_error_row_addr,
        ecc_error_col_addr,
        
        do_write_r,
        do_read_r,
        do_auto_precharge_r,
        do_burst_chop_r,
        do_activate_r,
        do_precharge_r,
        do_refresh_r,
        do_power_down_r,
        do_self_rfsh_r,
        do_lmr_r,
        do_precharge_all_r,
        do_zqcal_r,
        
        rdwr_data_valid_r,
        
        to_chip_r,
        to_bank_addr_r,
        to_row_addr_r,
        to_col_addr_r,
        
        do_write_var_r,
        do_read_var_r,
        do_auto_precharge_var_r,
        do_burst_chop_var_r,
        do_activate_var_r,
        do_precharge_var_r,
        do_refresh_var_r,
        do_power_down_var_r,
        do_self_rfsh_var_r,
        do_lmr_var_r,
        do_precharge_all_var_r,
        do_zqcal_var_r,
        
        rdwr_data_valid_var_r,
        
        to_chip_var_r,
        to_bank_addr_var_r,
        to_row_addr_var_r,
        to_col_addr_var_r,

        addr_order,
        regdimm_enable

    );
    
    localparam LOCAL_SIZE_WIDTH = 2; 
    
    input ctl_clk;
    input ctl_reset_n;
    input ctl_cal_success;
    
    input                               cmd_fifo_empty;
    input                               cmd_fifo_wren;
    input                               write_req_to_wfifo;
    output                              fetch;
    
    input                               cmd0_is_a_read;
    input                               cmd0_is_a_write;
    input                               cmd0_autopch_req;
    input                               cmd0_multicast_req;
    input   [LOCAL_SIZE_WIDTH-1:0]      cmd0_burstcount;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd0_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd0_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd0_row_addr;
    input   [MEM_IF_COL_WIDTH-1:0]      cmd0_col_addr;
    input                               cmd0_is_valid;
    
    input                               cmd1_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd1_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd1_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd1_row_addr;
    input                               cmd1_is_valid;
    
    input                               cmd2_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd2_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd2_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd2_row_addr;
    input                               cmd2_is_valid;
    
    input                               cmd3_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd3_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd3_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd3_row_addr;
    input                               cmd3_is_valid;
    
    input                               cmd4_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd4_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd4_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd4_row_addr;
    input                               cmd4_is_valid;
    
    input                               cmd5_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd5_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd5_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd5_row_addr;
    input                               cmd5_is_valid;
    
    input                               cmd6_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd6_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd6_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd6_row_addr;
    input                               cmd6_is_valid;
    
    input                               cmd7_multicast_req;
    input   [MEM_IF_CHIP_BITS-1:0]      cmd7_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       cmd7_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      cmd7_row_addr;
    input                               cmd7_is_valid;
    
    output                              current_is_a_read;
    output                              current_is_a_write;
    output  [MEM_IF_CHIP_BITS-1:0]      current_chip_addr;
    output  [MEM_IF_BA_WIDTH-1:0]       current_bank_addr;
    output  [MEM_IF_ROW_WIDTH-1:0]      current_row_addr;
    output                              current_multicast_req;
    
    input   [MEM_IF_CS_WIDTH-1:0]       all_banks_closed;
    input   [CTL_LOOK_AHEAD_DEPTH-1:0]  bank_info_valid;
    input   [CTL_LOOK_AHEAD_DEPTH-1:0]  bank_is_open;
    input   [CTL_LOOK_AHEAD_DEPTH-1:0]  row_is_open;
    input                               current_bank_info_valid;
    input                               current_bank_is_open;
    input                               current_row_is_open;
    
    input                               zq_cal_req;
    input                               add_lat_on;
    input                               can_al_activate_write;
    input                               can_al_activate_read;
    input   [CTL_LOOK_AHEAD_DEPTH-1:0]  can_activate;            
    input   [CTL_LOOK_AHEAD_DEPTH-1:0]  can_precharge;            
    input                               can_read_current;
    input                               can_write_current;
    input                               can_activate_current;
    input                               can_precharge_current;
    input   [MEM_IF_CS_WIDTH-1:0]       can_lmr;
    input   [MEM_IF_CS_WIDTH-1:0]       can_precharge_all;
    input   [MEM_IF_CS_WIDTH-1:0]       can_refresh;
    input   [MEM_IF_CS_WIDTH-1:0]       can_enter_power_down;          
    input   [MEM_IF_CS_WIDTH-1:0]       can_exit_power_saving_mode;    
    input   [MEM_IF_CS_WIDTH-1:0]       can_self_rfsh;           
    input                               cam_full;
    
    input                               auto_refresh_req;     
    input   [MEM_IF_CS_WIDTH-1:0]       auto_refresh_chip;      
    
    input                               local_refresh_req;       
    input   [MEM_IF_CS_WIDTH-1:0]       local_refresh_chip;      
    output                              local_refresh_ack;
    
    input                               power_down_req;          
    output                              local_power_down_ack;
    
    input                               local_self_rfsh_req;     
    input   [MEM_IF_CS_WIDTH-1:0]       local_self_rfsh_chip;    
    output                              local_self_rfsh_ack;     
    
    output                              do_ecc;
    output                              do_partial;
    output                              ecc_fetch_error_addr;
    input                               wdata_is_partial;
    input                               ecc_single_bit_error;
    input                               rmw_data_ready;
    input   [MEM_IF_CHIP_BITS-1:0]      ecc_error_chip_addr;
    input   [MEM_IF_BA_WIDTH-1:0]       ecc_error_bank_addr;
    input   [MEM_IF_ROW_WIDTH-1:0]      ecc_error_row_addr;
    input   [MEM_IF_COL_WIDTH-1:0]      ecc_error_col_addr;
    
    output do_write_r;
    output do_read_r;
    output do_auto_precharge_r;
    output do_burst_chop_r;
    output do_activate_r;
    output do_precharge_r;
    output do_refresh_r;
    output do_power_down_r;
    output do_self_rfsh_r;
    output do_lmr_r;
    output do_precharge_all_r;
    output do_zqcal_r;
    
    output rdwr_data_valid_r;
    
    output  [MEM_IF_CS_WIDTH-1:0]       to_chip_r;
    output  [MEM_IF_BA_WIDTH-1:0]       to_bank_addr_r;
    output  [MEM_IF_ROW_WIDTH-1:0]      to_row_addr_r;
    output  [MEM_IF_COL_WIDTH-1:0]      to_col_addr_r;
    
    output do_write_var_r;
    output do_read_var_r;
    output do_auto_precharge_var_r;
    output do_burst_chop_var_r;
    output do_activate_var_r;
    output do_precharge_var_r;
    output do_refresh_var_r;
    output do_power_down_var_r;
    output do_self_rfsh_var_r;
    output do_lmr_var_r;
    output do_precharge_all_var_r;
    output do_zqcal_var_r;
    
    output rdwr_data_valid_var_r;
    
    output  [MEM_IF_CS_WIDTH-1:0]       to_chip_var_r;
    output  [MEM_IF_BA_WIDTH-1:0]       to_bank_addr_var_r;
    output  [MEM_IF_ROW_WIDTH-1:0]      to_row_addr_var_r;
    output  [MEM_IF_COL_WIDTH-1:0]      to_col_addr_var_r;

    input   [1:0]                       addr_order; 
    input   regdimm_enable;
    
    localparam INIT         = 32'h696e6974;
    localparam FETCH        = 32'h66657468;
    localparam DO2          = 32'h646f3032;
    localparam READWRITE    = 32'h72647772;
    localparam PCHALL       = 32'h70636861;
    localparam REFRESH      = 32'h72667368;
    localparam PDOWN        = 32'h7064776e;
    localparam SELFRFSH     = 32'h736c7266;
    localparam ZQCAL        = 32'h7a63616c;
    localparam ECC_DO2      = 32'h65636377;
    
    
    localparam HIGH_LATENCY     = 0;
    localparam CHIP_BANK_BUS    = MEM_IF_CHIP_BITS + MEM_IF_BA_WIDTH;
    localparam INT_SIZE_WIDTH   = LOCAL_SIZE_WIDTH;
    localparam INT_LOOK_AHEAD_DEPTH = (CTL_LOOK_AHEAD_DEPTH < 4) ? 4 : CTL_LOOK_AHEAD_DEPTH;
    
    reg fetch;
    reg fetch_r;
    reg [31:0] state;
    reg do_write;
    reg do_read;
    reg do_auto_precharge;
    reg do_burst_chop;
    reg do_activate;
    reg do_precharge;
    reg do_refresh;
    reg do_power_down;
    reg do_self_rfsh;
    wire do_lmr;
    reg do_precharge_all;
    reg do_zqcal;
    
    wire    [INT_LOOK_AHEAD_DEPTH-1:0]  cmd_is_valid;
    
    wire    [MEM_IF_BA_WIDTH-1:0]       to_bank_addr;
    wire    [MEM_IF_ROW_WIDTH-1:0]      to_row_addr;
    wire    [MEM_IF_COL_WIDTH-1:0]      to_col_addr;
    
    wire                                current_is_a_read;
    wire                                current_is_a_write;
    reg                                 current_read;
    reg                                 current_write;
    reg                                 current_autopch_req;
    reg                                 current_multicast_req;
    reg                                 current_is_ecc;
    reg                                 current_diff_cs;
    reg     [INT_SIZE_WIDTH-1:0]        current_burstcount_counter_temp;
    reg     [INT_SIZE_WIDTH-1:0]        current_burstcount_counter;
    wire    [MEM_IF_CHIP_BITS-1:0]      current_chip_addr;
    wire    [MEM_IF_BA_WIDTH-1:0]       current_bank_addr;
    wire    [MEM_IF_ROW_WIDTH-1:0]      current_row_addr;
    reg     [MEM_IF_CHIP_BITS-1:0]      current_chip;
    reg     [MEM_IF_BA_WIDTH-1:0]       current_bank;
    reg     [MEM_IF_ROW_WIDTH-1:0]      current_row;
    reg     [MEM_IF_COL_WIDTH-1:0]      current_col;
    
    reg                                 current_copy_autopch_req;
    reg                                 current_copy_multicast_req;
    reg                                 current_copy_diff_cs;
    reg     [INT_SIZE_WIDTH-1:0]        current_copy_burstcount_counter;
    reg     [MEM_IF_CHIP_BITS-1:0]      current_copy_chip;
    reg     [MEM_IF_BA_WIDTH-1:0]       current_copy_bank;
    reg     [MEM_IF_ROW_WIDTH-1:0]      current_copy_row;
    reg     [MEM_IF_COL_WIDTH-1:0]      current_copy_col;
    reg                            current_copy_burst_delay;
    
    reg                            burst_delay_temp;
    reg                            burst_delay;
    
    reg     [MEM_IF_CS_WIDTH-1:0]       to_chip;
    reg     [MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:0] to_addr;
    wire    [MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:0] current_addr;
    
    reg     do_write_r;
    reg     do_read_r;
    reg     do_auto_precharge_r;
    reg     do_burst_chop_r;
    reg     do_activate_r;
    reg     do_precharge_r;
    reg     do_refresh_r;
    reg     do_power_down_r;
    reg     do_self_rfsh_r;
    reg     do_lmr_r;
    reg     do_precharge_all_r;
    reg     do_zqcal_r;
    reg     rdwr_data_valid_r;
    reg     [MEM_IF_CS_WIDTH-1:0]   to_chip_r;
    reg     [MEM_IF_BA_WIDTH-1:0]   to_bank_addr_r;
    reg     [MEM_IF_ROW_WIDTH-1:0]  to_row_addr_r;
    reg     [MEM_IF_COL_WIDTH-1:0]  to_col_addr_r;
    
    wire    do_write_var_r;
    wire    do_read_var_r;
    wire    do_auto_precharge_var_r;
    wire    do_burst_chop_var_r;
    wire    do_activate_var_r;
    wire    do_precharge_var_r;
    wire    do_refresh_var_r;
    wire    do_power_down_var_r;
    wire    do_self_rfsh_var_r;
    wire    do_lmr_var_r;
    wire    do_precharge_all_var_r;
    wire    do_zqcal_var_r;
    wire    rdwr_data_valid_var_r;
    wire    [MEM_IF_CS_WIDTH-1:0]   to_chip_var_r;
    wire    [MEM_IF_BA_WIDTH-1:0]   to_bank_addr_var_r;
    wire    [MEM_IF_ROW_WIDTH-1:0]  to_row_addr_var_r;
    wire    [MEM_IF_COL_WIDTH-1:0]  to_col_addr_var_r;
    
    wire rdwr_data_valid;
    
    reg  [MEM_IF_CS_WIDTH-1:0] for_chip;
    wire  [MEM_IF_CS_WIDTH-1:0] for_chip_current;
    wire  [MEM_IF_CS_WIDTH-1:0] for_chip_next;

    wire int_refresh_req;
    reg  int_refresh_ack;
    wire refresh_chip_ok;
    wire [MEM_IF_CS_WIDTH-1:0] refresh_chip;
    
    wire int_power_down_req;
    wire [MEM_IF_CS_WIDTH-1:0] power_down_chip;
    
    reg  int_self_rfsh_req;
    wire self_rfsh_chip_ok;
    reg  [MEM_IF_CS_WIDTH-1:0] self_rfsh_chip;
    reg  [MEM_IF_CS_WIDTH-1:0] for_chip_self_rfsh_saved;

    reg  for_chip_refresh_req;
    reg  for_chip_self_rfsh_req;
    reg  for_chip_power_down_req;
    reg  zq_cal_req_r;

    wire precharge_all_chip_ok;
    
    reg  [MEM_IF_CS_WIDTH-1:0] for_chip_saved;
    wire [MEM_IF_CS_WIDTH-1:0] for_chip_mask;
    reg  [MEM_IF_CS_WIDTH-1:0] for_chip_mask_gen;

    reg                                 current_is_ready;   
    reg                                 precharge_current;  
    reg                                 activate_current;   
    reg     [INT_LOOK_AHEAD_DEPTH-1:0]  precharge_cmd;
    reg     [INT_LOOK_AHEAD_DEPTH-1:0]  activate_cmd;
    
    wire    [INT_LOOK_AHEAD_DEPTH-1:0]  int_can_activate;
    wire    [INT_LOOK_AHEAD_DEPTH-1:0]  int_can_precharge;
    
    reg     just_did_activate;
    reg     just_did_precharge; 
    wire    [CHIP_BANK_BUS-1:0] current_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd0_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd1_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd2_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd3_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd4_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd5_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd6_bank_index;
    wire    [CHIP_BANK_BUS-1:0] cmd7_bank_index;
    reg [INT_LOOK_AHEAD_DEPTH-1:0]   lookahead_allowed_to_cmd;
    
    wire    enable_aap;
    wire    aap_default;
    reg auto_autopch_req_c; 
    reg auto_autopch_req_0; 
    reg auto_autopch_req;
    
    wire    support_writepause;
    reg [WDATA_BEATS_WIDTH-1:0]   proper_beats_in_fifo;
    reg [3:0]   simple_beats_info;
    reg enough_data_to_write;
    wire    decrement_normal;
    
    reg start_burst;
    reg start_burst_r;
    reg [1:0] bl_counter;
    reg burst_length_gen;
    wire burst_length;
    
    reg start_write_burst;
    reg start_write_burst_r;
    reg [1:0] bl_write_counter;
    reg write_burst_length_gen;
    wire write_burst_length;
    
    reg new_gen_rdwr_data_valid;
    
    wire is_diff_chip;
    reg is_diff_chip_r;
    
    wire    int_ecc_req;
    reg     do_ecc;
    reg     do_partial;
    reg     partial_write_read;
    wire    ecc_fetch_error_addr;
    reg     ecc_fetch;
    reg     ecc_fetch_r;
    reg     ecc_dummy_fetch;
    reg     ecc_internal_fetch;
    reg     ecc_crrctn_ready;
    
    reg     [255:0] partial_data_pipe;
    reg     [7:0]   partial_data_location;
    wire    [4:0]   short_partial_data_pipe;
    
    reg         partial_write;
    
    wire    proper_multicast_cmd0;
    wire    proper_multicast_cmd1;
    wire    proper_multicast_cmd2;
    wire    proper_multicast_cmd3;
    wire    proper_multicast_cmd4;
    wire    proper_multicast_cmd5;
    wire    proper_multicast_cmd6;
    wire    proper_multicast_cmd7;
    
    integer i;
    
    assign  do_lmr = 0;
    assign  current_is_a_read   =   current_read;
    assign  current_is_a_write  =   current_write;
    assign  current_chip_addr   =   current_chip;
    assign  current_bank_addr   =   current_bank;
    assign  current_row_addr    =   current_row;
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 0)
            begin
                assign  cmd_is_valid[0] = cmd0_is_valid;
                assign  cmd_is_valid[1] = cmd1_is_valid;
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 2)
            begin
                assign  cmd_is_valid[2] = cmd2_is_valid;
                assign  cmd_is_valid[3] = cmd3_is_valid;
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 4)
            begin
                assign  cmd_is_valid[4] = cmd4_is_valid;
                assign  cmd_is_valid[5] = cmd5_is_valid;
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 6)
            begin
                assign  cmd_is_valid[6] = cmd6_is_valid;
                assign  cmd_is_valid[7] = cmd7_is_valid;
            end
    endgenerate
    
    assign  to_bank_addr    =   to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH];
    assign  to_row_addr     =   to_addr[MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH];
    assign  to_col_addr     =   to_addr[MEM_IF_COL_WIDTH-1:0];
    assign  current_addr    =   {current_bank,current_row,current_col[MEM_IF_COL_WIDTH-1:3],3'b000};
    
    assign int_refresh_req = (CTL_USR_REFRESH == 1) ? local_refresh_req : auto_refresh_req;
    assign refresh_chip = (local_refresh_req) ? (MULTICAST_WR_EN ? {MEM_IF_CS_WIDTH{1'b1}}:local_refresh_chip):auto_refresh_chip;
    assign local_refresh_ack = int_refresh_ack;
    
    assign int_power_down_req = power_down_req;
    assign power_down_chip = {MEM_IF_CS_WIDTH{1'b1}};
    assign local_power_down_ack = do_power_down;
    
    assign local_self_rfsh_ack = do_self_rfsh;
    
    assign int_ecc_req = ecc_single_bit_error & ~cam_full & CTL_ECC_ENABLED == 1; 
    assign ecc_fetch_error_addr = ecc_fetch;
    
    assign rdwr_data_valid = new_gen_rdwr_data_valid;
    
    generate
        genvar I;
        for (I = 0; I < CTL_LOOK_AHEAD_DEPTH; I = I + 1)
            begin : A
                assign int_can_activate[I]  =   can_activate[I];
                assign int_can_precharge[I] =   can_precharge[I];
            end
    endgenerate
    
    assign  current_bank_index  =   {current_chip,current_bank};
    assign  cmd0_bank_index     =   {cmd0_chip_addr,cmd0_bank_addr};
    assign  cmd1_bank_index     =   {cmd1_chip_addr,cmd1_bank_addr};
    assign  cmd2_bank_index     =   {cmd2_chip_addr,cmd2_bank_addr};
    assign  cmd3_bank_index     =   {cmd3_chip_addr,cmd3_bank_addr};
    assign  cmd4_bank_index     =   {cmd4_chip_addr,cmd4_bank_addr};
    assign  cmd5_bank_index     =   {cmd5_chip_addr,cmd5_bank_addr};
    assign  cmd6_bank_index     =   {cmd6_chip_addr,cmd6_bank_addr};
    assign  cmd7_bank_index     =   {cmd7_chip_addr,cmd7_bank_addr};
    
    assign  enable_aap = (ENABLE_AUTO_AP_LOGIC == 1) ? 1'b1 : 1'b0;
    
    assign  aap_default = (CLOSE_PAGE_POLICY == 1) ? 1'b1 : 1'b0;
    
    assign  support_writepause  = 1'b1;
    
    assign  proper_multicast_cmd0   =   cmd0_multicast_req;
    assign  proper_multicast_cmd1   =   cmd1_multicast_req;
    assign  proper_multicast_cmd2   =   cmd2_multicast_req;
    assign  proper_multicast_cmd3   =   cmd3_multicast_req;
    assign  proper_multicast_cmd4   =   cmd4_multicast_req;
    assign  proper_multicast_cmd5   =   cmd5_multicast_req;
    assign  proper_multicast_cmd6   =   cmd6_multicast_req;
    assign  proper_multicast_cmd7   =   cmd7_multicast_req;
    
    always @ (posedge ctl_clk or negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    do_write_r          <=  1'b0;
                    do_read_r           <=  1'b0;
                    do_auto_precharge_r <=  1'b0;
                    do_burst_chop_r     <=  1'b0;
                    do_activate_r       <=  1'b0;
                    do_precharge_r      <=  1'b0;
                    do_refresh_r        <=  1'b0;
                    do_power_down_r     <=  1'b0;
                    do_self_rfsh_r      <=  1'b0;
                    do_lmr_r            <=  1'b0;
                    do_precharge_all_r  <=  1'b0;
                    do_zqcal_r          <=  1'b0;
                    rdwr_data_valid_r   <=  1'b0;
                    to_chip_r           <=  0;
                    to_bank_addr_r      <=  0;
                    to_row_addr_r       <=  0;
                    to_col_addr_r       <=  0;
                end
            else
                begin
                    do_write_r          <=  do_write;
                    do_read_r           <=  do_read;
                    do_auto_precharge_r <=  do_auto_precharge;
                    do_burst_chop_r     <=  do_burst_chop;
                    do_activate_r       <=  do_activate;
                    do_precharge_r      <=  do_precharge;
                    do_refresh_r        <=  do_refresh;
                    do_power_down_r     <=  do_power_down;
                    do_self_rfsh_r      <=  do_self_rfsh;
                    do_lmr_r            <=  do_lmr;
                    do_precharge_all_r  <=  do_precharge_all;
                    do_zqcal_r          <=  do_zqcal;
                    rdwr_data_valid_r   <=  rdwr_data_valid;
                    to_chip_r           <=  to_chip;
                    to_bank_addr_r      <=  to_bank_addr;
                    to_row_addr_r       <=  to_row_addr;
                    to_col_addr_r       <=  to_col_addr;
                end
        end
    
    generate
        if (LOW_LATENCY == 1) 
            begin
                assign  do_write_var_r          =   do_write;
                assign  do_read_var_r           =   do_read;
                assign  do_auto_precharge_var_r =   do_auto_precharge;
                assign  do_burst_chop_var_r     =   do_burst_chop;
                assign  do_activate_var_r       =   do_activate;
                assign  do_precharge_var_r      =   do_precharge;
                assign  do_refresh_var_r        =   do_refresh;
                assign  do_power_down_var_r     =   do_power_down;
                assign  do_self_rfsh_var_r      =   do_self_rfsh;
                assign  do_lmr_var_r            =   do_lmr;
                assign  do_precharge_all_var_r  =   do_precharge_all;
                assign  do_zqcal_var_r          =   do_zqcal;
                assign  rdwr_data_valid_var_r   =   rdwr_data_valid;
                assign  to_chip_var_r           =   to_chip;
                assign  to_bank_addr_var_r      =   to_bank_addr;
                assign  to_row_addr_var_r       =   to_row_addr;
                assign  to_col_addr_var_r       =   to_col_addr;
            end
        else 
            begin
                assign  do_write_var_r          =   do_write_r;
                assign  do_read_var_r           =   do_read_r;
                assign  do_auto_precharge_var_r =   do_auto_precharge_r;
                assign  do_burst_chop_var_r     =   do_burst_chop_r;
                assign  do_activate_var_r       =   do_activate_r;
                assign  do_precharge_var_r      =   do_precharge_r;
                assign  do_refresh_var_r        =   do_refresh_r;
                assign  do_power_down_var_r     =   do_power_down_r;
                assign  do_self_rfsh_var_r      =   do_self_rfsh_r;
                assign  do_lmr_var_r            =   do_lmr_r;
                assign  do_precharge_all_var_r  =   do_precharge_all_r;
                assign  do_zqcal_var_r          =   do_zqcal_r;
                assign  rdwr_data_valid_var_r   =   rdwr_data_valid_r;
                assign  to_chip_var_r           =   to_chip_r;
                assign  to_bank_addr_var_r      =   to_bank_addr_r;
                assign  to_row_addr_var_r       =   to_row_addr_r;
                assign  to_col_addr_var_r       =   to_col_addr_r;
            end
    endgenerate
    
    generate
        if (HIGH_LATENCY == 1)
            begin
                genvar I;
                for (I = 0; I < CTL_LOOK_AHEAD_DEPTH; I = I + 1)
                begin : A
                    always @ (posedge ctl_clk or negedge ctl_reset_n)
                        begin
                            if (!ctl_reset_n)
                                begin
                                    precharge_cmd[I]    <=  0;
                                    activate_cmd[I]     <=  0;
                                end
                            else
                                begin
                                    precharge_cmd[I]    <=  bank_info_valid[I] & bank_is_open[I] & ~row_is_open[I];
                                    activate_cmd[I]     <=  bank_info_valid[I] & ~bank_is_open[I];
                                end
                        end
                end
                
                always @ (posedge ctl_clk or negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            begin
                                current_is_ready    <=  0;
                                precharge_current   <=  0;
                                activate_current    <=  0;
                            end
                        else
                            begin
                                current_is_ready    <=  current_bank_info_valid & current_bank_is_open & current_row_is_open;
                                precharge_current   <=  current_bank_info_valid & current_bank_is_open & ~current_row_is_open;
                                activate_current    <=  current_bank_info_valid & ~current_bank_is_open;
                            end
                    end
            end
        else
            begin
                genvar I;
                for (I = 0; I < CTL_LOOK_AHEAD_DEPTH; I = I + 1)
                begin : B
                    always @ (*)
                        begin
                            precharge_cmd[I]    <=  bank_info_valid[I] & bank_is_open[I] & ~row_is_open[I];
                            activate_cmd[I]     <=  bank_info_valid[I] & ~bank_is_open[I];
                        end
                end
                
                always @ (*)
                    begin
                        current_is_ready    <=  current_bank_info_valid & current_bank_is_open & current_row_is_open;
                        precharge_current   <=  current_bank_info_valid & current_bank_is_open & ~current_row_is_open;
                        activate_current    <=  current_bank_info_valid & ~current_bank_is_open;
                    end
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 0)
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            lookahead_allowed_to_cmd[0]  <=  1'b0;
                        else
                            begin
                                if (fetch)
                                    lookahead_allowed_to_cmd[0] <=  lookahead_allowed_to_cmd[1];
                                else if (cmd_is_valid[0] &&
                                    (((!current_multicast_req && !proper_multicast_cmd0) && (current_bank_index != cmd0_bank_index)) || ((current_multicast_req || proper_multicast_cmd0) && (current_bank != cmd0_bank_addr)))
                                    )
                                    lookahead_allowed_to_cmd[0]  <=  1'b1;
                                else
                                    lookahead_allowed_to_cmd[0]  <=  1'b0;
                            end
                    end
                
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            lookahead_allowed_to_cmd[1]  <=  1'b0;
                        else
                            begin
                                if (fetch)
                                    lookahead_allowed_to_cmd[1] <=  lookahead_allowed_to_cmd[2];
                                else if (cmd_is_valid[1] &&
                                    (((!current_multicast_req && !proper_multicast_cmd1) && (current_bank_index != cmd1_bank_index)) || ((current_multicast_req || proper_multicast_cmd1) && (current_bank != cmd1_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd0 && !proper_multicast_cmd1) && (cmd0_bank_index != cmd1_bank_index)) || ((proper_multicast_cmd0 || proper_multicast_cmd1) && (cmd0_bank_addr != cmd1_bank_addr)))
                                    )
                                    lookahead_allowed_to_cmd[1]  <=  1'b1;
                                else
                                    lookahead_allowed_to_cmd[1]  <=  1'b0;
                            end
                    end
            end
        else
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            begin
                                lookahead_allowed_to_cmd[0]  <=  1'b0;
                                lookahead_allowed_to_cmd[1]  <=  1'b0;
                            end
                        else
                            begin
                                lookahead_allowed_to_cmd[0]  <=  1'b0;
                                lookahead_allowed_to_cmd[1]  <=  1'b0;
                            end
                    end
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 2)
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            lookahead_allowed_to_cmd[2]  <=  1'b0;
                        else
                            begin
                                if (fetch)
                                    lookahead_allowed_to_cmd[2] <=  lookahead_allowed_to_cmd[3];
                                else if (cmd_is_valid[2] &&
                                    (((!current_multicast_req && !proper_multicast_cmd2) && (current_bank_index != cmd2_bank_index)) || ((current_multicast_req || proper_multicast_cmd2) && (current_bank != cmd2_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd0 && !proper_multicast_cmd2) && (cmd0_bank_index != cmd2_bank_index)) || ((proper_multicast_cmd0 || proper_multicast_cmd2) && (cmd0_bank_addr != cmd2_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd1 && !proper_multicast_cmd2) && (cmd1_bank_index != cmd2_bank_index)) || ((proper_multicast_cmd1 || proper_multicast_cmd2) && (cmd1_bank_addr != cmd2_bank_addr)))
                                    )
                                    lookahead_allowed_to_cmd[2]  <=  1'b1;
                                else
                                    lookahead_allowed_to_cmd[2]  <=  1'b0;
                            end
                    end
                
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            lookahead_allowed_to_cmd[3]  <=  1'b0;
                        else
                            begin
                                if (fetch)
                                    lookahead_allowed_to_cmd[3] <=  1'b0;
                                else if (cmd_is_valid[3] &&
                                    (((!current_multicast_req && !proper_multicast_cmd3) && (current_bank_index != cmd3_bank_index)) || ((current_multicast_req || proper_multicast_cmd3) && (current_bank != cmd3_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd0 && !proper_multicast_cmd3) && (cmd0_bank_index != cmd3_bank_index)) || ((proper_multicast_cmd0 || proper_multicast_cmd3) && (cmd0_bank_addr != cmd3_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd1 && !proper_multicast_cmd3) && (cmd1_bank_index != cmd3_bank_index)) || ((proper_multicast_cmd1 || proper_multicast_cmd3) && (cmd1_bank_addr != cmd3_bank_addr)))
                                    &&
                                    (((!proper_multicast_cmd2 && !proper_multicast_cmd3) && (cmd2_bank_index != cmd3_bank_index)) || ((proper_multicast_cmd2 || proper_multicast_cmd3) && (cmd2_bank_addr != cmd3_bank_addr)))
                                    )
                                    lookahead_allowed_to_cmd[3]  <=  1'b1;
                                else
                                    lookahead_allowed_to_cmd[3]  <=  1'b0;
                            end
                    end
            end
        else
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            begin
                                lookahead_allowed_to_cmd[2]  <=  1'b0;
                                lookahead_allowed_to_cmd[3]  <=  1'b0;
                            end
                        else
                            begin
                                lookahead_allowed_to_cmd[2]  <=  1'b0;
                                lookahead_allowed_to_cmd[3]  <=  1'b0;
                            end
                    end
            end
    endgenerate
    
    generate
        if (CTL_LOOK_AHEAD_DEPTH > 6)
            begin
                always @(*)
                    begin
                                if (((!current_multicast_req && !proper_multicast_cmd0 && current_bank_index == cmd0_bank_index) || ((current_multicast_req || proper_multicast_cmd0) && current_bank == cmd0_bank_addr)) && cmd_is_valid[0])
                                    if (current_row == cmd0_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd0))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd1 && current_bank_index == cmd1_bank_index) || ((current_multicast_req || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (current_row == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd1))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd2 && current_bank_index == cmd2_bank_index) || ((current_multicast_req || proper_multicast_cmd2) && current_bank == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (current_row == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd2))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd3 && current_bank_index == cmd3_bank_index) || ((current_multicast_req || proper_multicast_cmd3) && current_bank == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (current_row == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd3))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd4 && current_bank_index == cmd4_bank_index) || ((current_multicast_req || proper_multicast_cmd4) && current_bank == cmd4_bank_addr)) && cmd_is_valid[4])
                                    if (current_row == cmd4_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd4))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd5 && current_bank_index == cmd5_bank_index) || ((current_multicast_req || proper_multicast_cmd5) && current_bank == cmd5_bank_addr)) && cmd_is_valid[5])
                                    if (current_row == cmd5_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd5))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd6 && current_bank_index == cmd6_bank_index) || ((current_multicast_req || proper_multicast_cmd6) && current_bank == cmd6_bank_addr)) && cmd_is_valid[6])
                                    if (current_row == cmd6_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd6))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd7 && current_bank_index == cmd7_bank_index) || ((current_multicast_req || proper_multicast_cmd7) && current_bank == cmd7_bank_addr)) && cmd_is_valid[7])
                                    if (current_row == cmd7_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd7))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else
                                    auto_autopch_req_c  <=  aap_default;
                    end
                    
                always @(*)
                    begin
                                if (((!proper_multicast_cmd0 && !proper_multicast_cmd1 && cmd0_bank_index == cmd1_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd1) && cmd0_bank_index == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (cmd0_row_addr == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd1))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd2 && cmd0_bank_index == cmd2_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd2) && cmd0_bank_index == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (cmd0_row_addr == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd2))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd3 && cmd0_bank_index == cmd3_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd3) && cmd0_bank_index == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (cmd0_row_addr == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd3))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd4 && cmd0_bank_index == cmd4_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd4) && cmd0_bank_index == cmd4_bank_addr)) && cmd_is_valid[4])
                                    if (cmd0_row_addr == cmd4_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd4))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd5 && cmd0_bank_index == cmd5_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd5) && cmd0_bank_index == cmd5_bank_addr)) && cmd_is_valid[5])
                                    if (cmd0_row_addr == cmd5_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd5))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd6 && cmd0_bank_index == cmd6_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd6) && cmd0_bank_index == cmd6_bank_addr)) && cmd_is_valid[6])
                                    if (cmd0_row_addr == cmd6_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd6))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd7 && cmd0_bank_index == cmd7_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd7) && cmd0_bank_index == cmd7_bank_addr)) && cmd_is_valid[7])
                                    if (cmd0_row_addr == cmd7_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd7))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else
                                    auto_autopch_req_0  <=  aap_default;
                    end
                    
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            auto_autopch_req    <=  1'b0;
                        else
                            begin
                                if (enable_aap)
                                    begin
                                        if (fetch)
                                            auto_autopch_req    <=  auto_autopch_req_0;
                                        else
                                            auto_autopch_req    <=  auto_autopch_req_c;
                                    end
                                else
                                    auto_autopch_req    <=  aap_default;
                            end
                    end
            end
        else if (CTL_LOOK_AHEAD_DEPTH > 4)
            begin
                always @(*)
                    begin
                                if (((!current_multicast_req && !proper_multicast_cmd0 && current_bank_index == cmd0_bank_index) || ((current_multicast_req || proper_multicast_cmd0) && current_bank == cmd0_bank_addr)) && cmd_is_valid[0])
                                    if (current_row == cmd0_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd0))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd1 && current_bank_index == cmd1_bank_index) || ((current_multicast_req || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (current_row == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd1))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd2 && current_bank_index == cmd2_bank_index) || ((current_multicast_req || proper_multicast_cmd2) && current_bank == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (current_row == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd2))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd3 && current_bank_index == cmd3_bank_index) || ((current_multicast_req || proper_multicast_cmd3) && current_bank == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (current_row == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd3))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd4 && current_bank_index == cmd4_bank_index) || ((current_multicast_req || proper_multicast_cmd4) && current_bank == cmd4_bank_addr)) && cmd_is_valid[4])
                                    if (current_row == cmd4_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd4))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd5 && current_bank_index == cmd5_bank_index) || ((current_multicast_req || proper_multicast_cmd5) && current_bank == cmd5_bank_addr)) && cmd_is_valid[5])
                                    if (current_row == cmd5_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd5))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else
                                    auto_autopch_req_c  <=  aap_default;
                    end
                    
                always @(*)
                    begin
                                if (((!proper_multicast_cmd0 && !proper_multicast_cmd1 && cmd0_bank_index == cmd1_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd1) && cmd0_bank_index == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (cmd0_row_addr == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd1))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd2 && cmd0_bank_index == cmd2_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd2) && cmd0_bank_index == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (cmd0_row_addr == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd2))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd3 && cmd0_bank_index == cmd3_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd3) && cmd0_bank_index == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (cmd0_row_addr == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd3))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd4 && cmd0_bank_index == cmd4_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd4) && cmd0_bank_index == cmd4_bank_addr)) && cmd_is_valid[4])
                                    if (cmd0_row_addr == cmd4_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd4))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd5 && cmd0_bank_index == cmd5_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd5) && cmd0_bank_index == cmd5_bank_addr)) && cmd_is_valid[5])
                                    if (cmd0_row_addr == cmd5_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd5))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else
                                    auto_autopch_req_0  <=  aap_default;
                    end
                    
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            auto_autopch_req    <=  1'b0;
                        else
                            begin
                                if (enable_aap)
                                    begin
                                        if (fetch)
                                            auto_autopch_req    <=  auto_autopch_req_0;
                                        else
                                            auto_autopch_req    <=  auto_autopch_req_c;
                                    end
                                else
                                    auto_autopch_req    <=  aap_default;
                            end
                    end
            end
        else if (CTL_LOOK_AHEAD_DEPTH > 2)
            begin
                always @(*)
                    begin
                                if (((!current_multicast_req && !proper_multicast_cmd0 && current_bank_index == cmd0_bank_index) || ((current_multicast_req || proper_multicast_cmd0) && current_bank == cmd0_bank_addr)) && cmd_is_valid[0])
                                    if (current_row == cmd0_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd0))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd1 && current_bank_index == cmd1_bank_index) || ((current_multicast_req || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (current_row == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd1))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd2 && current_bank_index == cmd2_bank_index) || ((current_multicast_req || proper_multicast_cmd2) && current_bank == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (current_row == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd2))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else if (((!current_multicast_req && !proper_multicast_cmd3 && current_bank_index == cmd3_bank_index) || ((current_multicast_req || proper_multicast_cmd3) && current_bank == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (current_row == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd3))
                                        auto_autopch_req_c  <=  1'b0;
                                    else
                                        auto_autopch_req_c  <=  1'b1;
                                else
                                    auto_autopch_req_c  <=  aap_default;
                    end
                    
                always @(*)
                    begin
                                if (((!proper_multicast_cmd0 && !proper_multicast_cmd1 && cmd0_bank_index == cmd1_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                                    if (cmd0_row_addr == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd1))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd2 && cmd0_bank_index == cmd2_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd2) && current_bank == cmd2_bank_addr)) && cmd_is_valid[2])
                                    if (cmd0_row_addr == cmd2_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd2))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else if (((!proper_multicast_cmd0 && !proper_multicast_cmd3 && cmd0_bank_index == cmd3_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd3) && current_bank == cmd3_bank_addr)) && cmd_is_valid[3])
                                    if (cmd0_row_addr == cmd3_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd3))
                                        auto_autopch_req_0  <=  1'b0;
                                    else
                                        auto_autopch_req_0  <=  1'b1;
                                else
                                    auto_autopch_req_0  <=  aap_default;
                    end
                    
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            auto_autopch_req    <=  1'b0;
                        else
                            begin
                                if (enable_aap)
                                    begin
                                        if (fetch)
                                            auto_autopch_req    <=  auto_autopch_req_0;
                                        else
                                            auto_autopch_req    <=  auto_autopch_req_c;
                                    end
                                else
                                    auto_autopch_req    <=  aap_default;
                            end
                    end
            end
        else if (CTL_LOOK_AHEAD_DEPTH > 0)
            begin
                always @(*)
                    begin
                        if (((!current_multicast_req && !proper_multicast_cmd0 && current_bank_index == cmd0_bank_index) || ((current_multicast_req || proper_multicast_cmd0) && current_bank == cmd0_bank_addr)) && cmd_is_valid[0])
                            if (current_row == cmd0_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd0))
                                auto_autopch_req_c  <=  1'b0;
                            else
                                auto_autopch_req_c  <=  1'b1;
                        else if (((!current_multicast_req && !proper_multicast_cmd1 && current_bank_index == cmd1_bank_index) || ((current_multicast_req || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                            if (current_row == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && current_multicast_req == proper_multicast_cmd1))
                                auto_autopch_req_c  <=  1'b0;
                            else
                                auto_autopch_req_c  <=  1'b1;
                        else
                            auto_autopch_req_c  <=  aap_default;
                    end
                    
                always @(*)
                    begin
                        if (((!proper_multicast_cmd0 && !proper_multicast_cmd1 && cmd0_bank_index == cmd1_bank_index) || ((proper_multicast_cmd0 || proper_multicast_cmd1) && current_bank == cmd1_bank_addr)) && cmd_is_valid[1])
                            if (cmd0_row_addr == cmd1_row_addr && (CLOSE_PAGE_POLICY == 1 && proper_multicast_cmd0 == proper_multicast_cmd1))
                                auto_autopch_req_0  <=  1'b0;
                            else
                                auto_autopch_req_0  <=  1'b1;
                        else
                            auto_autopch_req_0  <=  aap_default;
                    end
                    
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            auto_autopch_req    <=  1'b0;
                        else
                            begin
                                if (enable_aap)
                                    begin
                                        if (fetch)
                                            auto_autopch_req    <=  auto_autopch_req_0;
                                        else
                                            auto_autopch_req    <=  auto_autopch_req_c;
                                    end
                                else
                                    auto_autopch_req    <=  aap_default;
                            end
                    end
            end
        else 
            begin
                always @(posedge ctl_clk, negedge ctl_reset_n)
                    begin
                        if (!ctl_reset_n)
                            auto_autopch_req    <=  1'b0;
                        else
                            auto_autopch_req    <=  aap_default; 
                    end
            end
    endgenerate
    
    assign decrement_normal = current_burstcount_counter != 0 && burst_delay == 0;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                proper_beats_in_fifo <= 0;
            else if (write_req_to_wfifo)
                begin
                    if (write_burst_length && decrement_normal)
                        proper_beats_in_fifo <= proper_beats_in_fifo;
                    else
                        proper_beats_in_fifo <= proper_beats_in_fifo + 1'b1;
                end
            else if (write_burst_length && decrement_normal)
                proper_beats_in_fifo <= proper_beats_in_fifo - 1'b1;
        end
    
    always @(proper_beats_in_fifo)
        begin
            begin
                if (|proper_beats_in_fifo[WDATA_BEATS_WIDTH-1:3])
                    simple_beats_info <= 8;
                else
                    simple_beats_info <= {1'b0,proper_beats_in_fifo[2:0]};
            end
        end
    
    always @(*)
        begin
            if (support_writepause)
                begin
                    if (simple_beats_info > 2 || (!write_burst_length && simple_beats_info == 2))
                        enough_data_to_write <= 1'b1;
                    else if ((current_burstcount_counter == 1 && !fetch_r) || (current_burstcount_counter_temp == 1 && fetch_r))
                        enough_data_to_write <= 1'b1;
                    else
                        enough_data_to_write <= 1'b0;
                end
            else
                enough_data_to_write <= 1'b1;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                partial_data_location <= 0;
            else
                begin
                    if (write_req_to_wfifo)
                        begin
                            if (write_burst_length && decrement_normal)
                                partial_data_location <= partial_data_location;
                            else
                                partial_data_location <= partial_data_location + 1'b1;
                        end
                    else if (write_burst_length && decrement_normal)
                        partial_data_location <= partial_data_location - 1'b1;
                end
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                partial_data_pipe <= 0;
            else
                begin
                    if (write_burst_length && decrement_normal)
                    begin
                        if (write_req_to_wfifo)
                        begin
                            for (i=0;i < 255;i = i + 1)
                            begin : shift_loop
                                if (i == (partial_data_location - 1))
                                begin
                                    partial_data_pipe [i] <= wdata_is_partial;
                                end
                                else
                                begin
                                    if (i == 255) 
                                        partial_data_pipe [i] <= 1'b0;
                                    else
                                        partial_data_pipe [i] <= partial_data_pipe [i + 1];
                                end
                            end
                        end
                        else
                        begin
                            partial_data_pipe <= {1'b0,partial_data_pipe[255:1]};
                        end
                    end
                    else if (write_req_to_wfifo)
                    begin
                        partial_data_pipe[partial_data_location] <= wdata_is_partial;
                    end
                end
        end
    
    assign  short_partial_data_pipe = partial_data_pipe[4:0];
    
    always @(*)
        begin
            if (current_burstcount_counter == 1)
                partial_write <= short_partial_data_pipe[0];
            else
                partial_write <= |short_partial_data_pipe[1:0];
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    int_self_rfsh_req   <=  1'b0;
                    self_rfsh_chip      <=  0;
                end
            else
                begin
                    int_self_rfsh_req   <=  local_self_rfsh_req;
                    self_rfsh_chip      <=  local_self_rfsh_chip;
                end
        end
    
    generate
        genvar chip;
        wire [(MEM_IF_CS_WIDTH-1):0] for_chip_and_can_precharge_all_is_same;
        wire [(MEM_IF_CS_WIDTH-1):0] for_chip_and_can_refresh_is_same;
        wire [(MEM_IF_CS_WIDTH-1):0] for_chip_and_can_self_rfsh_is_same;
        for (chip = 0; chip < MEM_IF_CS_WIDTH; chip = chip + 1)
        begin : gen_chip_ok
            assign for_chip_and_can_precharge_all_is_same[chip] = ( for_chip[chip] & can_precharge_all[chip] ) ^~ for_chip[chip];
            assign for_chip_and_can_refresh_is_same[chip] = ( for_chip[chip] & can_refresh[chip] ) ^~ for_chip[chip];
            assign for_chip_and_can_self_rfsh_is_same[chip] = ( for_chip[chip] & can_self_rfsh[chip] ) ^~ for_chip[chip];
        end

        assign precharge_all_chip_ok = &for_chip_and_can_precharge_all_is_same;
        assign refresh_chip_ok = &for_chip_and_can_refresh_is_same;
        assign self_rfsh_chip_ok = &for_chip_and_can_self_rfsh_is_same;

    endgenerate


    generate
        if ( ( (CTL_CSR_ENABLED == 1) || (CTL_REGDIMM_ENABLED == 1) ) && (MEM_TYPE == "DDR3")) begin
            
            if (MEM_IF_CS_WIDTH == 1) 
            begin

                assign for_chip_mask = 1'b1;

            end
            else if (MEM_IF_CS_WIDTH == 2) 
            begin

                always @ (*)
                begin 
                    if (regdimm_enable)
                    begin
                        if (for_chip[0]) begin
                            for_chip_mask_gen = 2'b01;
                        end
                        else if (for_chip[1]) begin
                            for_chip_mask_gen = 2'b10;
                        end
                        else begin
                            for_chip_mask_gen = 2'b00;
                        end
                    end
                    else 
                        for_chip_mask_gen = {MEM_IF_CS_WIDTH{1'b1}};
                end

                assign for_chip_mask = for_chip_mask_gen;

            end
            else if (MEM_IF_CS_WIDTH == 4)
            begin

                always @ (*)
                begin
                    if (regdimm_enable)
                    begin
                        if (for_chip[0] | for_chip[3]) begin
                            for_chip_mask_gen = 4'b1001;
                        end
                        else if (for_chip[1] | for_chip[2]) begin
                            for_chip_mask_gen = 4'b0110;
                        end
                        else begin
                            for_chip_mask_gen = 4'b0000;
                        end
                    end
                    else 
                        for_chip_mask_gen = {MEM_IF_CS_WIDTH{1'b1}};
                end

                assign for_chip_mask = for_chip_mask_gen;

            end
            else if (MEM_IF_CS_WIDTH == 8)
            begin

                always @ (*)
                begin
                    if (regdimm_enable)
                    begin
                        if (for_chip[0] | for_chip[3] | for_chip[4] | for_chip[7]) begin
                            for_chip_mask_gen = 8'b10011001;
                        end
                        else if (for_chip[1] | for_chip[2] | for_chip[5] | for_chip[6]) begin
                            for_chip_mask_gen = 8'b01100110;
                        end
                        else begin
                            for_chip_mask_gen = 8'b00000000;
                        end
                    end
                    else 
                        for_chip_mask_gen = {MEM_IF_CS_WIDTH{1'b1}};
                end

                assign for_chip_mask = for_chip_mask_gen;

            end
            else
            begin
            end

        end
        else
        begin

            assign for_chip_mask = {MEM_IF_CS_WIDTH{1'b1}};

        end
    endgenerate

    assign for_chip_current = for_chip & for_chip_mask;
    assign for_chip_next = for_chip & ~for_chip_mask;

    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                fetch_r     <=  1'b0;
            else
                fetch_r     <=  fetch | ecc_fetch | ecc_dummy_fetch | ecc_internal_fetch;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    current_read            <=  1'b0;
                    current_write           <=  1'b0;
                    current_autopch_req     <=  1'b0;
                    current_multicast_req   <=  1'b0;
                    current_diff_cs         <=  1'b0;
                    current_is_ecc          <=  1'b0;
                    current_chip            <=  {MEM_IF_CHIP_BITS{1'b0}};
                    current_bank            <=  {MEM_IF_BA_WIDTH{1'b0}};
                    current_row             <=  {MEM_IF_ROW_WIDTH{1'b0}};
                    current_col             <=  {MEM_IF_COL_WIDTH{1'b0}};
                    burst_delay_temp             <=  0;
                end
            else
                if (fetch)
                    begin
                        current_is_ecc          <=  1'b0;
                        current_read            <=  cmd0_is_a_read;
                        current_write           <=  cmd0_is_a_write;
                        current_autopch_req     <=  cmd0_autopch_req;
                        current_multicast_req   <=  proper_multicast_cmd0;
                        current_diff_cs         <=  current_chip != cmd0_chip_addr;
                        current_chip            <=  cmd0_chip_addr;
                        current_bank            <=  cmd0_bank_addr;
                        current_row             <=  cmd0_row_addr;
                        current_col             <=  cmd0_col_addr;
                        if (DWIDTH_RATIO == 2)
                            burst_delay_temp      <=  cmd0_col_addr[1];
                        else
                            burst_delay_temp      <=  cmd0_col_addr[2];
                    end
                else if (ecc_internal_fetch)
                    begin
                        current_read            <=  1'b0;
                        current_write           <=  1'b1;
                        current_autopch_req     <=  current_copy_autopch_req;
                        current_multicast_req   <=  current_copy_multicast_req;
                        current_diff_cs         <=  current_copy_diff_cs;
                        current_chip            <=  current_copy_chip;
                        current_bank            <=  current_copy_bank;
                        current_row             <=  current_copy_row;
                        current_col             <=  current_copy_col;
                        burst_delay_temp             <=  current_copy_burst_delay;
                    end
                else if (ecc_fetch || ecc_dummy_fetch)
                    begin
                        if (ecc_fetch)
                            begin
                                current_read            <=  1'b1;
                                current_write           <=  1'b0;
                                current_chip            <=  ecc_error_chip_addr;
                                current_bank            <=  ecc_error_bank_addr;
                                current_row             <=  ecc_error_row_addr;
                                current_col             <=  ecc_error_col_addr;
                            end
                        else
                            begin
                                current_read            <=  1'b0;
                                current_write           <=  1'b1;
                            end
                        current_autopch_req     <=  1'b0;
                        current_multicast_req   <=  1'b0;
                        current_diff_cs         <=  1'b1;
                        current_is_ecc          <=  1'b1;
                        burst_delay_temp             <=  0;
                    end
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                burst_delay             <=  0;
            else
                if (fetch_r)
                    burst_delay      <=  burst_delay_temp;
                else
                    begin
                        if (burst_delay > 0 && burst_length)
                            burst_delay <=  burst_delay - 1'b1;
                    end
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                begin
                    current_copy_autopch_req            <=  1'b0;
                    current_copy_multicast_req          <=  1'b0;
                    current_copy_diff_cs                <=  1'b0;
                    current_copy_burstcount_counter     <=  0;
                    current_copy_chip                   <=  {MEM_IF_CHIP_BITS{1'b0}};
                    current_copy_bank                   <=  {MEM_IF_BA_WIDTH{1'b0}};
                    current_copy_row                    <=  {MEM_IF_ROW_WIDTH{1'b0}};
                    current_copy_col                    <=  {MEM_IF_COL_WIDTH{1'b0}};
                    current_copy_burst_delay            <=  0;
                end
            else
                if (state == READWRITE && do_partial)
                    begin
                        current_copy_autopch_req    <=  current_autopch_req;
                        current_copy_multicast_req  <=  current_multicast_req;
                        current_copy_diff_cs        <=  current_diff_cs;
                        current_copy_burstcount_counter  <=  current_burstcount_counter;
                        current_copy_burst_delay         <=  burst_delay;
                        current_copy_chip           <=  current_chip;
                        current_copy_bank           <=  current_bank;
                        current_copy_row            <=  current_row;
                        current_copy_col            <=  current_col;
                        
                    end
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                current_burstcount_counter_temp   <=  0;
            else if (fetch)
                current_burstcount_counter_temp   <=  cmd0_burstcount;
            else if (ecc_internal_fetch)
                current_burstcount_counter_temp  <=  current_copy_burstcount_counter;
            else if (ecc_fetch || ecc_dummy_fetch)
                current_burstcount_counter_temp  <=  2;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                current_burstcount_counter   <=  0;
            else if (fetch_r)
                current_burstcount_counter   <=  current_burstcount_counter_temp;
            else if (current_burstcount_counter != 0 && burst_length && burst_delay == 0)
                current_burstcount_counter <= current_burstcount_counter - 1'b1;
        end
    
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                bl_counter   <=  0;
            else
                if (bl_counter == 2)
                    bl_counter    <=  0;
                else if (bl_counter > 0)
                    bl_counter    <=  bl_counter + 1'b1;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                start_burst_r   <=  0;
            else
                start_burst_r   <=  start_burst;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                burst_length_gen         <=  1'b0;
            else
                if (start_burst)
                    burst_length_gen         <=  1'b1;
                else if (bl_counter > 0)
                    burst_length_gen         <=  1'b1;
                else
                    burst_length_gen         <=  1'b0;
        end
    
    assign burst_length = start_burst | start_burst_r | burst_length_gen;
    
    
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                bl_write_counter   <=  0;
            else
                if (bl_write_counter == 2)
                    bl_write_counter    <=  0;
                else if (bl_write_counter > 0)
                    bl_write_counter    <=  bl_write_counter + 1'b1;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                start_write_burst_r   <=  0;
            else
                start_write_burst_r   <=  start_burst & current_write & ~do_ecc;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                write_burst_length_gen         <=  1'b0;
            else
                if (start_burst && current_write && !do_ecc)
                    write_burst_length_gen         <=  1'b1;
                else if (bl_write_counter > 0)
                    write_burst_length_gen         <=  1'b1;
                else
                    write_burst_length_gen         <=  1'b0;
        end
    
    assign write_burst_length = (start_burst & current_write & ~do_ecc) | start_write_burst_r | write_burst_length_gen;
    
    
    always @(*)
        begin
                if (MEM_TYPE == "DDR3" && state == READWRITE && (burst_delay == 1 || current_burstcount_counter == 1) )
                    new_gen_rdwr_data_valid <=  1'b1;
                else if (burst_delay == 0 && burst_length && current_burstcount_counter > 0)
                    new_gen_rdwr_data_valid <=  1'b1;
                else
                    new_gen_rdwr_data_valid <=  1'b0;
        end
    
    assign is_diff_chip = fetch_r & current_diff_cs & burst_length;
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
        begin
            if (!ctl_reset_n)
                is_diff_chip_r  <=  1'b0;
            else
                is_diff_chip_r  <=  is_diff_chip;
        end
    
    always @(posedge ctl_clk, negedge ctl_reset_n)
    begin : FSM
    if (!ctl_reset_n)
        begin
            state <= INIT;
            fetch <= 1'b0;
            just_did_activate <= 1'b0;
            just_did_precharge <= 1'b0;
            start_burst <= 1'b0;
            do_ecc <= 1'b0;
            do_partial <= 1'b0;
            ecc_fetch <= 1'b0;
            ecc_fetch_r <= 1'b0;
            ecc_dummy_fetch <= 1'b0;
            ecc_internal_fetch <= 1'b0;
            partial_write_read <= 1'b0;
            for_chip_refresh_req <= 1'b0;
            for_chip_self_rfsh_req <= 1'b0;
            for_chip_power_down_req <= 1'b0;
            for_chip <= 0;
            for_chip_saved <= 0;
            int_refresh_ack <= 0;
            for_chip_self_rfsh_saved <= 0;
            zq_cal_req_r <= 0;
            ecc_crrctn_ready <= 0;
        end
    else
        case(state)
            INIT :
                if (ctl_cal_success == 1'b1)
                    begin
                        state <= FETCH;
                    end
                else
                    begin
                        state <= INIT;
                    end
            FETCH :
                begin
                    int_refresh_ack <= 0;

                    if (fetch || ecc_internal_fetch) 
                        begin
                            state <= DO2;
                            fetch <= 1'b0;
                            ecc_internal_fetch <= 1'b0;
                        end
                    else if (int_refresh_req && !do_refresh_r)
                        begin
                            if (refresh_chip == (refresh_chip & all_banks_closed))
                                state <= REFRESH;
                            else
                                begin
                                    state <= PCHALL;
                                    ecc_crrctn_ready <= 1'b0;
                                end

                            for_chip <= refresh_chip;
                            for_chip_saved <= refresh_chip;
                            for_chip_refresh_req <= 1'b1;
                            for_chip_self_rfsh_req <= 1'b0;
                            for_chip_power_down_req <= 1'b0;

                        end
                    else if (do_partial && partial_write_read)
                        begin
                            ecc_internal_fetch <= 1'b1;
                        end
                    else if (CTL_ECC_ENABLED == 1 && do_ecc)
                        begin
                            if (current_write)
                                do_ecc <= 1'b0;
                            else
                                begin
                                    ecc_dummy_fetch <= 1'b1;
                                    state <= ECC_DO2;
                                end
                        end
                    else if (CTL_ECC_ENABLED == 1 && int_ecc_req)
                        begin
                            do_ecc <= 1'b1;
                            ecc_fetch <= 1'b1;
                            ecc_crrctn_ready <= 1'b0;
                            state <= ECC_DO2;
                        end
                    else if (int_self_rfsh_req)
                        begin
                            if (&all_banks_closed)
                                state <= REFRESH;
                            else
                                state <= PCHALL;

                            for_chip <= self_rfsh_chip;
                            for_chip_saved <= self_rfsh_chip;
                            for_chip_refresh_req <= 1'b0;
                            for_chip_self_rfsh_req <= 1'b1;
                            for_chip_power_down_req <= 1'b0;

                        end
                    else if (int_power_down_req)
                        begin
                            if (&all_banks_closed)
                                state <= PDOWN;
                            else
                                state <= PCHALL;

                            for_chip <= power_down_chip;
                            for_chip_saved <= power_down_chip;
                            for_chip_refresh_req <= 1'b0;
                            for_chip_self_rfsh_req <= 1'b0;
                            for_chip_power_down_req <= 1'b1;

                        end
                    else if (!cmd_fifo_empty || cmd_fifo_wren) 
                        begin
                            fetch <= 1'b1;
                        end
                    else
                        begin
                            fetch <= 1'b0;
                        end
                    
                    if (do_partial && !partial_write_read)
                        do_partial <= 1'b0;
                end
            DO2 : 
                begin
                    if (do_partial && !partial_write_read)
                        do_partial <= 1'b0;
                    
                    if ((!(is_diff_chip || is_diff_chip_r)) && !(CTL_ECC_ENABLED == 1 && write_burst_length && |short_partial_data_pipe) && 
                        (
                            (add_lat_on && (
                                (activate_current && !just_did_activate && (can_activate_current) && !(current_multicast_req && (do_read_r || do_write_r)) && ( (current_read && !do_write_r && can_al_activate_read) || (current_write && !do_read_r && (enough_data_to_write && (CTL_ECC_ENABLED == 0 || (CTL_ECC_ENABLED == 1 && (!do_partial || (do_partial && rmw_data_ready)) ))) && ((can_al_activate_write && (CTL_ECC_ENABLED == 0 || do_partial || !partial_write)) || (can_al_activate_read && (CTL_ECC_ENABLED == 1 && !do_partial && partial_write)))) ))
                            )) 
                            ||
                            (
                                (current_is_ready && !(do_auto_precharge_r && to_chip_r[current_chip] && current_bank == to_bank_addr_r)) && ((current_read && !do_write_r && can_read_current) || (current_write && !do_read_r && (enough_data_to_write && (CTL_ECC_ENABLED == 0 || (CTL_ECC_ENABLED == 1 && (!do_partial || (do_partial && rmw_data_ready)) ))) && ((can_write_current && (CTL_ECC_ENABLED == 0 || do_partial || !partial_write)) || (can_read_current && (CTL_ECC_ENABLED == 1 && !do_partial && partial_write))) ))
                            )
                        )
                        )
                        begin
                            start_burst <= 1'b1;
                            state <= READWRITE;
                            if (CTL_ECC_ENABLED == 1 && current_write && partial_write)
                                begin
                                    partial_write_read <= 1'b1;
                                    do_partial <= 1'b1;
                                    do_ecc <= 1'b1;
                                end
                            if (do_partial)
                                begin
                                    partial_write_read <= 1'b0;
                                    do_ecc <= 1'b0;
                                end
                            
                            
                            if (CTL_ECC_ENABLED == 1 && ((do_partial && partial_write_read) || (current_write && partial_write))) 
                                fetch <= 1'b0;
                            else if (!int_refresh_req)
                                begin
                                    if ((!cmd_fifo_empty || cmd_fifo_wren) && !int_power_down_req && !int_self_rfsh_req && !int_ecc_req)
                                        fetch <= 1'b1;
                                    else
                                        fetch <= 1'b0;
                                end
                            else 
                                fetch <= 1'b0;
                        end
                    else
                        state <= DO2;
                    
                    just_did_activate <= 1'b0;
                    just_did_precharge <= 1'b0;
                    
                    if (CLOSE_PAGE_POLICY == 0 && precharge_current && !just_did_precharge && can_precharge_current && !do_auto_precharge_r && !(current_multicast_req && (do_read_r || do_write_r)))
                        just_did_precharge    <=  1'b1;
                    else if (activate_current && !just_did_activate && can_activate_current && (!add_lat_on || (add_lat_on && (can_al_activate_read || can_al_activate_write) )) && !(current_multicast_req && (do_read_r || do_write_r)))
                        just_did_activate               <=  1'b1;
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 0 && precharge_cmd[0] && !just_did_precharge && int_can_precharge[0] && lookahead_allowed_to_cmd[0])
                        just_did_precharge     <=  1'b1;
                    else if (CTL_LOOK_AHEAD_DEPTH > 0 && activate_cmd[0] && !just_did_activate && int_can_activate[0] && lookahead_allowed_to_cmd[0])
                        just_did_activate      <=  1'b1;
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 0 && precharge_cmd[1] && !just_did_precharge && int_can_precharge[1] && lookahead_allowed_to_cmd[1])
                        just_did_precharge     <=  1'b1;
                    else if (CTL_LOOK_AHEAD_DEPTH > 0 && activate_cmd[1] && !just_did_activate && int_can_activate[1] && lookahead_allowed_to_cmd[1])
                        just_did_activate      <=  1'b1;
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 2 && precharge_cmd[2] && !just_did_precharge && int_can_precharge[2] && lookahead_allowed_to_cmd[2])
                        just_did_precharge     <=  1'b1;
                    else if (CTL_LOOK_AHEAD_DEPTH > 2 && activate_cmd[2] && !just_did_activate && int_can_activate[2] && lookahead_allowed_to_cmd[2])
                        just_did_activate      <=  1'b1;
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 2 && precharge_cmd[3] && !just_did_precharge && int_can_precharge[3] && lookahead_allowed_to_cmd[3])
                        just_did_precharge     <=  1'b1;
                    else if (CTL_LOOK_AHEAD_DEPTH > 2 && activate_cmd[3] && !just_did_activate && int_can_activate[3] && lookahead_allowed_to_cmd[3])
                        just_did_activate      <=  1'b1;
                end
            READWRITE : 
                begin
                    fetch <= 1'b0;
                    ecc_internal_fetch <= 1'b0;
                    
                    start_burst <= 1'b0;
                    just_did_activate <= 1'b0;
                    just_did_precharge <= 1'b0;
                    
                    if (CTL_ECC_ENABLED == 1 && current_is_ecc) 
                        begin
                            if (int_refresh_req)
                                state <= FETCH;
                            else if (current_write)
                                state <= FETCH;
                            else
                                begin
                                    ecc_dummy_fetch <= 1'b1;
                                    state <= ECC_DO2;
                                end
                        end
                    else if (do_partial && partial_write_read) 
                        state <= FETCH;
                    else if (fetch)
                        state <= DO2;
                    else if (int_refresh_req)
                        state <= FETCH;
                    else
                        state <= FETCH;
                end
            ECC_DO2 :
                begin
                    ecc_fetch <= 1'b0;
                    ecc_dummy_fetch <= 1'b0;
                    ecc_fetch_r <=  ecc_fetch;
                    
                    if (!(ecc_fetch || ecc_fetch_r || ecc_dummy_fetch))
                        begin
                            if (
                                    (
                                        (current_read && ecc_crrctn_ready && can_read_current) || (current_write && ecc_crrctn_ready && can_write_current && rmw_data_ready)
                                    )
                                )
                                begin
                                    state <= READWRITE;
                                    start_burst <= 1'b1;
                                end
                            else
                                state <= ECC_DO2;
                            
                            if (!ecc_crrctn_ready && can_precharge_current && !just_did_precharge && !do_auto_precharge_r && current_is_ready)
                                begin
                                    just_did_precharge <=  1'b1;
                                end
                            else if (!ecc_crrctn_ready && can_activate_current && !just_did_activate && !do_precharge_r)
                                begin
                                    just_did_activate <=  1'b1;
                                    ecc_crrctn_ready <= 1'b1;
                                end
                        end
                    else
                        state <= ECC_DO2;
                end
            PCHALL :
                begin
                    if (for_chip_refresh_req | for_chip_self_rfsh_req | for_chip_power_down_req) 
                    begin

                        if ( |for_chip ) 
                        begin
                            if (precharge_all_chip_ok)
                            begin
                                for_chip <= for_chip_next;
                                state <= PCHALL;


                            end
                            else
                                state <= PCHALL;

                        end
                        else 
                        begin

                            for_chip <= for_chip_saved;

                            if (for_chip_refresh_req | for_chip_self_rfsh_req)
                            begin
                                state <= REFRESH;
                            end
                            else if (for_chip_power_down_req)
                            begin
                                state <= PDOWN;
                            end
                            else
                            begin
                                state <= FETCH;
                            end
                        end
                    end
                    else 
                    begin
                        state <= FETCH;
                        for_chip_refresh_req <= 1'b0;
                        for_chip_self_rfsh_req <= 1'b0;
                        for_chip_power_down_req <= 1'b0;
                    end

                end
            REFRESH :
                begin
                    int_refresh_ack <= 0;
                    for_chip_refresh_req <= 1'b0;

                    if ( |for_chip )
                    begin
                        if ( refresh_chip_ok && !do_precharge_all_r)
                        begin
                            for_chip <= for_chip_next;
                            state <= REFRESH;

                            if ( ~(|for_chip_next) )
                            begin
                                int_refresh_ack <= 1;
                            end
                        end
                        else
                             state <= REFRESH;
                    end
                    else
                    begin
            
                        int_refresh_ack <= 0;
                        for_chip <= for_chip_saved;

                        if (for_chip_self_rfsh_req)
                        begin
                            state <= SELFRFSH;
                        end
                        else if (int_power_down_req && &for_chip_saved && &all_banks_closed && !int_ecc_req && current_burstcount_counter == 0)
                        begin
                            state <= PDOWN;
                        end
                        else
                        begin
                            state <= FETCH;
                        end
                    end
                end
            PDOWN :
                begin
                    int_refresh_ack <= 0;
                    for_chip_power_down_req <= 1'b0;
                    if (int_refresh_req && !do_refresh_r && &can_exit_power_saving_mode)
                        begin
                            state <= REFRESH;
                            for_chip <= refresh_chip;
                        end
                    else if ((!int_power_down_req || int_ecc_req) && &can_exit_power_saving_mode)
                        begin
                            state <= FETCH;
                        end
                    else if (&can_enter_power_down && !do_precharge_all_r) 
                        begin
                            state <= PDOWN;
                        end
                    else
                        state <= PDOWN;
                end
            SELFRFSH :
                begin
                    int_refresh_ack <= 0;
                    for_chip_self_rfsh_req <= 1'b0;

                    if (!int_self_rfsh_req && &can_exit_power_saving_mode)
                        begin
                            for_chip_self_rfsh_saved <= 0;
                            for_chip <= for_chip_saved;

                            if (MEM_TYPE == "DDR3")
                                state <= ZQCAL;
                            else
                                state <= FETCH;
                        end
                    else if (self_rfsh_chip_ok && !do_precharge_all_r)
                        begin
                            for_chip_self_rfsh_saved <= (for_chip_current) | for_chip_self_rfsh_saved;
                            for_chip <= for_chip_next;
                            state <= SELFRFSH;
                        end
                    else
                        state <= SELFRFSH;
                end
            ZQCAL :
                begin

                    state <= ZQCAL;

                    if (zq_cal_req)
                    begin
                        zq_cal_req_r <= 1;
                    end

                    if (zq_cal_req_r)
                    begin
                        if (|for_chip)
                        begin
                            for_chip <= for_chip_next;
                        end
                        else 
                        begin
                            zq_cal_req_r <= 0;
                            state <= FETCH;
                        end
                    end
                end
            default : state <= FETCH;
        endcase
    end

    always @(*)
    begin : OUTPUT
        case(state)
            FETCH :
                begin
                    to_chip <= 0;
                    to_addr <= 0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_refresh <= 1'b0;
                end
            DO2 : 
                begin
                    to_addr <= 0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_precharge    <=  1'b0;
                    do_activate     <=  1'b0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                            
                    to_chip <=  {MEM_IF_CS_WIDTH{1'b0}};
                    
                    if (CLOSE_PAGE_POLICY == 0 && precharge_current && !just_did_precharge && can_precharge_current && !do_auto_precharge_r && !(current_multicast_req && (do_read_r || do_write_r)))
                        begin
                            do_precharge                    <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  current_bank;
                            if (current_multicast_req)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[current_chip]       <=  1'b1;
                        end
                    else if (activate_current && !just_did_activate && can_activate_current && (!add_lat_on || (add_lat_on && (can_al_activate_read || can_al_activate_write) )) && !(current_multicast_req && (do_read_r || do_write_r)))
                        begin
                            do_activate                     <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {current_bank,current_row};
                            if (current_multicast_req)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[current_chip]       <=  1'b1;
                        end
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 0 && precharge_cmd[0] && !just_did_precharge && int_can_precharge[0] && lookahead_allowed_to_cmd[0])
                        begin
                            do_precharge                <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  cmd0_bank_addr;
                            if (proper_multicast_cmd0)
                                to_chip                         <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd0_chip_addr] <=  1'b1;
                        end
                    else if (CTL_LOOK_AHEAD_DEPTH > 0 && activate_cmd[0] && !just_did_activate && int_can_activate[0] && lookahead_allowed_to_cmd[0])
                        begin
                            do_activate                 <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {cmd0_bank_addr,cmd0_row_addr};
                            if (proper_multicast_cmd0)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd0_chip_addr] <=  1'b1;
                        end
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 0 && precharge_cmd[1] && !just_did_precharge && int_can_precharge[1] && lookahead_allowed_to_cmd[1])
                        begin
                            do_precharge                <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  cmd1_bank_addr;
                            if (proper_multicast_cmd1)
                                to_chip                         <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd1_chip_addr] <=  1'b1;
                        end
                    else if (CTL_LOOK_AHEAD_DEPTH > 0 && activate_cmd[1] && !just_did_activate && int_can_activate[1] && lookahead_allowed_to_cmd[1])
                        begin
                            do_activate                 <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {cmd1_bank_addr,cmd1_row_addr};
                            if (proper_multicast_cmd1)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd1_chip_addr] <=  1'b1;
                        end
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 2 && precharge_cmd[2] && !just_did_precharge && int_can_precharge[2] && lookahead_allowed_to_cmd[2])
                        begin
                            do_precharge                <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  cmd2_bank_addr;
                            if (proper_multicast_cmd2)
                                to_chip                         <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd2_chip_addr] <=  1'b1;
                        end
                    else if (CTL_LOOK_AHEAD_DEPTH > 2 && activate_cmd[2] && !just_did_activate && int_can_activate[2] && lookahead_allowed_to_cmd[2])
                        begin
                            do_activate                 <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {cmd2_bank_addr,cmd2_row_addr};
                            if (proper_multicast_cmd2)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd2_chip_addr] <=  1'b1;
                        end
                    else if (CLOSE_PAGE_POLICY == 0 && CTL_LOOK_AHEAD_DEPTH > 2 && precharge_cmd[3] && !just_did_precharge && int_can_precharge[3] && lookahead_allowed_to_cmd[3])
                        begin
                            do_precharge                <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  cmd3_bank_addr;
                            if (proper_multicast_cmd3)
                                to_chip                         <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd3_chip_addr] <=  1'b1;
                        end
                    else if (CTL_LOOK_AHEAD_DEPTH > 2 && activate_cmd[3] && !just_did_activate && int_can_activate[3] && lookahead_allowed_to_cmd[3])
                        begin
                            do_activate                 <=  1'b1;
                            to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {cmd3_bank_addr,cmd3_row_addr};
                            if (proper_multicast_cmd3)
                                to_chip                     <=  {MEM_IF_CS_WIDTH{1'b1}};
                            else
                                to_chip[cmd3_chip_addr] <=  1'b1;
                        end
                end
            READWRITE : 
                begin
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    if (current_read)
                        do_read <= 1'b1;
                    else if (partial_write_read && current_write)
                        do_read <= 1'b1;
                    else
                        do_write <= 1'b1;
                    
                    do_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    to_addr <= current_addr;
                    to_chip <=  {MEM_IF_CS_WIDTH{1'b0}};
                    
                    if (current_multicast_req)
                        to_chip                 <=  {MEM_IF_CS_WIDTH{1'b1}};
                    else
                        to_chip[current_chip]   <=  1'b1;
                    
                    if (MEM_TYPE == "DDR3")
                        begin
                            if (burst_delay == 1 || (current_burstcount_counter == 1 && (CTL_ECC_ENABLED != 1 || (CTL_ECC_ENABLED == 1 && !do_partial && !short_partial_data_pipe[1]))) )
                                begin
                                    do_burst_chop <= 1'b1;
                                    to_addr[MEM_IF_COL_WIDTH-1:0] <= {current_col[MEM_IF_COL_WIDTH-1:2],2'b00};
                                end
                        end
                    else
                        begin
                            if (MEMORY_BURSTLENGTH == 4)
                                to_addr[MEM_IF_COL_WIDTH-1:0] <= {current_col[MEM_IF_COL_WIDTH-1:2],2'b00};
                        end
                    
                    if (!current_is_ecc && !(partial_write_read && current_write)) 
                        begin
                            if (CLOSE_PAGE_POLICY == 1)
                                begin
                                    if (fetch || !int_refresh_req)
                                        if (!auto_autopch_req) 
                                            do_auto_precharge <= 1'b0;
                                        else
                                            do_auto_precharge <= 1'b1;
                                    else
                                        do_auto_precharge <= 1'b1;
                                end
                            else
                                begin
                                    if (fetch || !int_refresh_req)
                                        if (current_autopch_req || auto_autopch_req) 
                                            do_auto_precharge <= 1'b1;
                                        else
                                            do_auto_precharge <= 1'b0;
                                    else
                                        do_auto_precharge <= 1'b0;
                                end
                        end
                    else
                        begin
                            if (current_is_ecc && current_write)
                                do_auto_precharge <= 1'b1;
                            else
                                do_auto_precharge <= 1'b0;
                        end
                end
            ECC_DO2 :
                begin
                    to_addr <= 0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_precharge <=  1'b0;
                    do_activate <=  1'b0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    
                    to_chip <=  {MEM_IF_CS_WIDTH{1'b0}};
                    
                    if (!(ecc_fetch || ecc_fetch_r || ecc_dummy_fetch))
                        begin
                            if (!ecc_crrctn_ready && can_precharge_current && !just_did_precharge && !do_auto_precharge_r && current_is_ready)
                                begin
                                    do_precharge            <=  1'b1;
                                    to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH]  <=  current_bank;
                                    to_chip[current_chip]   <=  1'b1;
                                end
                            else if (!ecc_crrctn_ready && can_activate_current && !just_did_activate && !do_precharge_r)
                                begin
                                    do_activate             <=  1'b1;
                                    to_addr[MEM_IF_BA_WIDTH+MEM_IF_ROW_WIDTH+MEM_IF_COL_WIDTH-1:MEM_IF_COL_WIDTH]   <=  {current_bank,current_row};
                                    to_chip[current_chip]   <=  1'b1;
                                end
                        end
                end
            PCHALL :
                begin
                    to_chip <= 0;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_precharge_all <= 1'b0;
                    if (for_chip_refresh_req | for_chip_self_rfsh_req | for_chip_power_down_req) 
                    begin

                        if ( |for_chip ) 
                        begin
                            if (precharge_all_chip_ok)
                            begin
                                do_precharge_all <= 1'b1;
                                to_chip <= for_chip_current;
                            end
                        end
                    end

                end
            REFRESH :
                begin
                    to_chip <= 0;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_precharge_all <= 1'b0;
                    do_refresh <= 1'b0;

                    if ( |for_chip )
                    begin
                        if ( refresh_chip_ok && !do_precharge_all_r)
                        begin
                            do_refresh <= 1'b1;
                            to_chip <= for_chip_current;
                        end
                    end
                end
            PDOWN :
                begin
                    to_chip <= to_chip_r;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_power_down <= do_power_down_r;
                    do_self_rfsh <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    do_precharge_all <= 1'b0;
                    do_refresh <= 1'b0;
                    if (int_refresh_req && !do_refresh_r && &can_exit_power_saving_mode)
                        begin
                            do_power_down <= 1'b0;
                        end
                    else if ((!int_power_down_req || int_ecc_req) && &can_exit_power_saving_mode)
                        begin
                            do_power_down <= 1'b0;
                        end
                    else if (&can_enter_power_down && !do_precharge_all_r) 
                        begin
                            do_power_down <= 1'b1;
                            to_chip <= for_chip;
                        end
                end
            SELFRFSH :
                begin
                    to_chip <= to_chip_r;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= do_self_rfsh_r;
                    do_zqcal <= 1'b0;
                    
                    do_precharge_all <= 1'b0;
                    
                    if (!int_self_rfsh_req && &can_exit_power_saving_mode)
                        begin
                            do_self_rfsh <= 1'b0;
                        end
                    else if (self_rfsh_chip_ok && !do_precharge_all_r)
                        begin
                            do_self_rfsh <= 1'b1;
                            to_chip <= (for_chip_current) | for_chip_self_rfsh_saved;
                        end
                end
            ZQCAL :
                begin
                    to_chip <= 0;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_zqcal <= 1'b0;
                    
                    if (zq_cal_req_r)
                    begin
                        if (|for_chip)
                        begin
                            do_zqcal <= 1;
                            to_chip <= for_chip_current;
                        end
                        else 
                        begin
                            do_zqcal <= 0;
                        end
                    end
                end
            default :
                begin
                    to_chip <= 0;
                    to_addr <= 0;
                    do_write <= 1'b0;
                    do_read <= 1'b0;
                    do_burst_chop <= 1'b0;
                    do_auto_precharge <= 1'b0;
                    do_activate <= 1'b0;
                    do_precharge <= 1'b0;
                    do_refresh <= 1'b0;
                    do_power_down <= 1'b0;
                    do_self_rfsh <= 1'b0;
                    do_precharge_all <= 1'b0;
                    do_zqcal <= 1'b0;
                end
        endcase
    end

endmodule
