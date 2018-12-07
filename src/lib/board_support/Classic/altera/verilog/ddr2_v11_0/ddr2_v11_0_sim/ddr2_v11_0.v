// ddr2_v11_0.v

// This file was auto-generated from alt_mem_if_ddr2_emif_hw.tcl.  If you edit it your changes
// will probably be lost.
// 
// Generated using SOPC Builder version 11.0 157 at 2011.06.21.09:54:50

`timescale 1 ps / 1 ps
module ddr2_v11_0 (
		input  wire         pll_ref_clk,       //     pll_ref_clk.clk
		input  wire         global_reset_n,    //    global_reset.reset_n
		input  wire         soft_reset_n,      //      soft_reset.reset_n
		output wire         afi_clk,           //         afi_clk.clk
		output wire         afi_half_clk,      //    afi_half_clk.clk
		output wire         afi_reset_n,       //       afi_reset.reset_n
		output wire [1:0]   mem_ck,            //          memory.mem_ck
		output wire [15:0]  mem_a,             //                .mem_a
		output wire [2:0]   mem_ba,            //                .mem_ba
		output wire         mem_we_n,          //                .mem_we_n
		output wire         mem_cas_n,         //                .mem_cas_n
		output wire [1:0]   mem_odt,           //                .mem_odt
		output wire [7:0]   mem_dm,            //                .mem_dm
		inout  wire [7:0]   mem_dqs,           //                .mem_dqs
		output wire [1:0]   mem_ck_n,          //                .mem_ck_n
		output wire         mem_ras_n,         //                .mem_ras_n
		output wire [1:0]   mem_cs_n,          //                .mem_cs_n
		inout  wire [7:0]   mem_dqs_n,         //                .mem_dqs_n
		inout  wire [63:0]  mem_dq,            //                .mem_dq
		output wire [1:0]   mem_cke,           //                .mem_cke
		output wire         avl_ready,         //             avl.waitrequest_n
		input  wire         avl_burstbegin,    //                .beginbursttransfer
		input  wire [29:0]  avl_addr,          //                .address
		output wire         avl_rdata_valid,   //                .readdatavalid
		output wire [255:0] avl_rdata,         //                .readdata
		input  wire [255:0] avl_wdata,         //                .writedata
		input  wire [31:0]  avl_be,            //                .byteenable
		input  wire         avl_read_req,      //                .read
		input  wire         avl_write_req,     //                .write
		input  wire [2:0]   avl_size,          //                .burstcount
		output wire         local_cal_fail,    //          status.local_cal_fail
		output wire         local_init_done,   //                .local_init_done
		output wire         local_cal_success, //                .local_cal_success
		input  wire         oct_rdn,           //             oct.rdn
		input  wire         oct_rup,           //                .rup
		input  wire         local_powerdn_req, // local_powerdown.local_powerdn_req
		output wire         local_powerdn_ack, //                .local_powerdn_ack
		output wire         csr_waitrequest,   //             csr.waitrequest
		output wire [31:0]  csr_readdata,      //                .readdata
		output wire         csr_readdatavalid, //                .readdatavalid
		input  wire         csr_burstcount,    //                .burstcount
		input  wire [31:0]  csr_writedata,     //                .writedata
		input  wire [18:0]  csr_address,       //                .address
		input  wire         csr_write,         //                .write
		input  wire         csr_read,          //                .read
		input  wire [3:0]   csr_byteenable,    //                .byteenable
		input  wire         csr_debugaccess    //                .debugaccess
	);

	wire    [3:0] c0_afi_afi_odt;                                                              // c0:afi_odt -> p0:afi_odt
	wire   [31:0] c0_afi_afi_addr;                                                             // c0:afi_addr -> p0:afi_addr
	wire          c0_afi_afi_cal_req;                                                          // c0:afi_cal_req -> p0:afi_cal_req
	wire    [5:0] p0_afi_afi_wlat;                                                             // p0:afi_wlat -> c0:afi_wlat
	wire    [1:0] p0_afi_afi_rdata_valid;                                                      // p0:afi_rdata_valid -> c0:afi_rdata_valid
	wire    [1:0] c0_afi_afi_rdata_en_full;                                                    // c0:afi_rdata_en_full -> p0:afi_rdata_en_full
	wire    [1:0] c0_afi_afi_we_n;                                                             // c0:afi_we_n -> p0:afi_we_n
	wire    [5:0] c0_afi_afi_ba;                                                               // c0:afi_ba -> p0:afi_ba
	wire  [255:0] c0_afi_afi_wdata;                                                            // c0:afi_wdata -> p0:afi_wdata
	wire    [3:0] c0_afi_afi_cs_n;                                                             // c0:afi_cs_n -> p0:afi_cs_n
	wire    [3:0] c0_afi_afi_cke;                                                              // c0:afi_cke -> p0:afi_cke
	wire    [1:0] c0_afi_afi_rdata_en;                                                         // c0:afi_rdata_en -> p0:afi_rdata_en
	wire    [1:0] c0_afi_afi_cas_n;                                                            // c0:afi_cas_n -> p0:afi_cas_n
	wire          p0_afi_afi_cal_success;                                                      // p0:afi_cal_success -> c0:afi_cal_success
	wire    [1:0] c0_afi_afi_ras_n;                                                            // c0:afi_ras_n -> p0:afi_ras_n
	wire    [5:0] p0_afi_afi_rlat;                                                             // p0:afi_rlat -> c0:afi_rlat
	wire  [255:0] p0_afi_afi_rdata;                                                            // p0:afi_rdata -> c0:afi_rdata
	wire          p0_afi_afi_cal_fail;                                                         // p0:afi_cal_fail -> c0:afi_cal_fail
	wire   [15:0] c0_afi_afi_wdata_valid;                                                      // c0:afi_wdata_valid -> p0:afi_wdata_valid
	wire   [15:0] c0_afi_afi_dqs_burst;                                                        // c0:afi_dqs_burst -> p0:afi_dqs_burst
	wire   [31:0] c0_afi_afi_dm;                                                               // c0:afi_dm -> p0:afi_dm
	wire    [0:0] bridge_0_m0_burstcount;                                                      // bridge_0:m0_burstcount -> bridge_0_m0_translator:av_burstcount
	wire          bridge_0_m0_waitrequest;                                                     // bridge_0_m0_translator:av_waitrequest -> bridge_0:m0_waitrequest
	wire   [18:0] bridge_0_m0_address;                                                         // bridge_0:m0_address -> bridge_0_m0_translator:av_address
	wire   [31:0] bridge_0_m0_writedata;                                                       // bridge_0:m0_writedata -> bridge_0_m0_translator:av_writedata
	wire          bridge_0_m0_write;                                                           // bridge_0:m0_write -> bridge_0_m0_translator:av_write
	wire          bridge_0_m0_read;                                                            // bridge_0:m0_read -> bridge_0_m0_translator:av_read
	wire   [31:0] bridge_0_m0_readdata;                                                        // bridge_0_m0_translator:av_readdata -> bridge_0:m0_readdata
	wire          bridge_0_m0_debugaccess;                                                     // bridge_0:m0_debugaccess -> bridge_0_m0_translator:av_debugaccess
	wire    [3:0] bridge_0_m0_byteenable;                                                      // bridge_0:m0_byteenable -> bridge_0_m0_translator:av_byteenable
	wire          bridge_0_m0_readdatavalid;                                                   // bridge_0_m0_translator:av_readdatavalid -> bridge_0:m0_readdatavalid
	wire          p0_csr_translator_avalon_anti_slave_0_waitrequest;                           // p0:csr_waitrequest -> p0_csr_translator:av_waitrequest
	wire   [31:0] p0_csr_translator_avalon_anti_slave_0_writedata;                             // p0_csr_translator:av_writedata -> p0:csr_wdata
	wire   [15:0] p0_csr_translator_avalon_anti_slave_0_address;                               // p0_csr_translator:av_address -> p0:csr_addr
	wire          p0_csr_translator_avalon_anti_slave_0_write;                                 // p0_csr_translator:av_write -> p0:csr_write_req
	wire          p0_csr_translator_avalon_anti_slave_0_read;                                  // p0_csr_translator:av_read -> p0:csr_read_req
	wire   [31:0] p0_csr_translator_avalon_anti_slave_0_readdata;                              // p0:csr_rdata -> p0_csr_translator:av_readdata
	wire          p0_csr_translator_avalon_anti_slave_0_readdatavalid;                         // p0:csr_rdata_valid -> p0_csr_translator:av_readdatavalid
	wire    [3:0] p0_csr_translator_avalon_anti_slave_0_byteenable;                            // p0_csr_translator:av_byteenable -> p0:csr_be
	wire          c0_csr_translator_avalon_anti_slave_0_waitrequest;                           // c0:csr_waitrequest -> c0_csr_translator:av_waitrequest
	wire          c0_csr_translator_avalon_anti_slave_0_burstcount;                            // c0_csr_translator:av_burstcount -> c0:csr_burst_count
	wire   [31:0] c0_csr_translator_avalon_anti_slave_0_writedata;                             // c0_csr_translator:av_writedata -> c0:csr_wdata
	wire   [15:0] c0_csr_translator_avalon_anti_slave_0_address;                               // c0_csr_translator:av_address -> c0:csr_addr
	wire          c0_csr_translator_avalon_anti_slave_0_write;                                 // c0_csr_translator:av_write -> c0:csr_write_req
	wire          c0_csr_translator_avalon_anti_slave_0_beginbursttransfer;                    // c0_csr_translator:av_beginbursttransfer -> c0:csr_beginbursttransfer
	wire          c0_csr_translator_avalon_anti_slave_0_read;                                  // c0_csr_translator:av_read -> c0:csr_read_req
	wire   [31:0] c0_csr_translator_avalon_anti_slave_0_readdata;                              // c0:csr_rdata -> c0_csr_translator:av_readdata
	wire          c0_csr_translator_avalon_anti_slave_0_readdatavalid;                         // c0:csr_rdata_valid -> c0_csr_translator:av_readdatavalid
	wire    [3:0] c0_csr_translator_avalon_anti_slave_0_byteenable;                            // c0_csr_translator:av_byteenable -> c0:csr_be
	wire          bridge_0_m0_translator_avalon_universal_master_0_waitrequest;                // bridge_0_m0_translator_avalon_universal_master_0_agent:av_waitrequest -> bridge_0_m0_translator:uav_waitrequest
	wire    [2:0] bridge_0_m0_translator_avalon_universal_master_0_burstcount;                 // bridge_0_m0_translator:uav_burstcount -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_burstcount
	wire   [31:0] bridge_0_m0_translator_avalon_universal_master_0_writedata;                  // bridge_0_m0_translator:uav_writedata -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_writedata
	wire   [18:0] bridge_0_m0_translator_avalon_universal_master_0_address;                    // bridge_0_m0_translator:uav_address -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_address
	wire          bridge_0_m0_translator_avalon_universal_master_0_lock;                       // bridge_0_m0_translator:uav_lock -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_lock
	wire          bridge_0_m0_translator_avalon_universal_master_0_write;                      // bridge_0_m0_translator:uav_write -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_write
	wire          bridge_0_m0_translator_avalon_universal_master_0_read;                       // bridge_0_m0_translator:uav_read -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_read
	wire   [31:0] bridge_0_m0_translator_avalon_universal_master_0_readdata;                   // bridge_0_m0_translator_avalon_universal_master_0_agent:av_readdata -> bridge_0_m0_translator:uav_readdata
	wire          bridge_0_m0_translator_avalon_universal_master_0_debugaccess;                // bridge_0_m0_translator:uav_debugaccess -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_debugaccess
	wire    [3:0] bridge_0_m0_translator_avalon_universal_master_0_byteenable;                 // bridge_0_m0_translator:uav_byteenable -> bridge_0_m0_translator_avalon_universal_master_0_agent:av_byteenable
	wire          bridge_0_m0_translator_avalon_universal_master_0_readdatavalid;              // bridge_0_m0_translator_avalon_universal_master_0_agent:av_readdatavalid -> bridge_0_m0_translator:uav_readdatavalid
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest;             // p0_csr_translator:uav_waitrequest -> p0_csr_translator_avalon_universal_slave_0_agent:m0_waitrequest
	wire    [2:0] p0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount;              // p0_csr_translator_avalon_universal_slave_0_agent:m0_burstcount -> p0_csr_translator:uav_burstcount
	wire   [31:0] p0_csr_translator_avalon_universal_slave_0_agent_m0_writedata;               // p0_csr_translator_avalon_universal_slave_0_agent:m0_writedata -> p0_csr_translator:uav_writedata
	wire   [18:0] p0_csr_translator_avalon_universal_slave_0_agent_m0_address;                 // p0_csr_translator_avalon_universal_slave_0_agent:m0_address -> p0_csr_translator:uav_address
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_write;                   // p0_csr_translator_avalon_universal_slave_0_agent:m0_write -> p0_csr_translator:uav_write
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_lock;                    // p0_csr_translator_avalon_universal_slave_0_agent:m0_lock -> p0_csr_translator:uav_lock
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_read;                    // p0_csr_translator_avalon_universal_slave_0_agent:m0_read -> p0_csr_translator:uav_read
	wire   [31:0] p0_csr_translator_avalon_universal_slave_0_agent_m0_readdata;                // p0_csr_translator:uav_readdata -> p0_csr_translator_avalon_universal_slave_0_agent:m0_readdata
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid;           // p0_csr_translator:uav_readdatavalid -> p0_csr_translator_avalon_universal_slave_0_agent:m0_readdatavalid
	wire          p0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess;             // p0_csr_translator_avalon_universal_slave_0_agent:m0_debugaccess -> p0_csr_translator:uav_debugaccess
	wire    [3:0] p0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable;              // p0_csr_translator_avalon_universal_slave_0_agent:m0_byteenable -> p0_csr_translator:uav_byteenable
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket;      // p0_csr_translator_avalon_universal_slave_0_agent:rf_source_endofpacket -> p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_endofpacket
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid;            // p0_csr_translator_avalon_universal_slave_0_agent:rf_source_valid -> p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_valid
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket;    // p0_csr_translator_avalon_universal_slave_0_agent:rf_source_startofpacket -> p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_startofpacket
	wire   [72:0] p0_csr_translator_avalon_universal_slave_0_agent_rf_source_data;             // p0_csr_translator_avalon_universal_slave_0_agent:rf_source_data -> p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_data
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready;            // p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_ready -> p0_csr_translator_avalon_universal_slave_0_agent:rf_source_ready
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket;   // p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_endofpacket -> p0_csr_translator_avalon_universal_slave_0_agent:rf_sink_endofpacket
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid;         // p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_valid -> p0_csr_translator_avalon_universal_slave_0_agent:rf_sink_valid
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket; // p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_startofpacket -> p0_csr_translator_avalon_universal_slave_0_agent:rf_sink_startofpacket
	wire   [72:0] p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data;          // p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_data -> p0_csr_translator_avalon_universal_slave_0_agent:rf_sink_data
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready;         // p0_csr_translator_avalon_universal_slave_0_agent:rf_sink_ready -> p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_ready
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid;       // p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_valid -> p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_valid
	wire   [31:0] p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data;        // p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_data -> p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_data
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready;       // p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_ready -> p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_ready
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid;       // p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_valid -> p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_valid
	wire   [31:0] p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data;        // p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_data -> p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_data
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready;       // p0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_ready -> p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_ready
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest;             // c0_csr_translator:uav_waitrequest -> c0_csr_translator_avalon_universal_slave_0_agent:m0_waitrequest
	wire    [2:0] c0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount;              // c0_csr_translator_avalon_universal_slave_0_agent:m0_burstcount -> c0_csr_translator:uav_burstcount
	wire   [31:0] c0_csr_translator_avalon_universal_slave_0_agent_m0_writedata;               // c0_csr_translator_avalon_universal_slave_0_agent:m0_writedata -> c0_csr_translator:uav_writedata
	wire   [18:0] c0_csr_translator_avalon_universal_slave_0_agent_m0_address;                 // c0_csr_translator_avalon_universal_slave_0_agent:m0_address -> c0_csr_translator:uav_address
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_write;                   // c0_csr_translator_avalon_universal_slave_0_agent:m0_write -> c0_csr_translator:uav_write
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_lock;                    // c0_csr_translator_avalon_universal_slave_0_agent:m0_lock -> c0_csr_translator:uav_lock
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_read;                    // c0_csr_translator_avalon_universal_slave_0_agent:m0_read -> c0_csr_translator:uav_read
	wire   [31:0] c0_csr_translator_avalon_universal_slave_0_agent_m0_readdata;                // c0_csr_translator:uav_readdata -> c0_csr_translator_avalon_universal_slave_0_agent:m0_readdata
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid;           // c0_csr_translator:uav_readdatavalid -> c0_csr_translator_avalon_universal_slave_0_agent:m0_readdatavalid
	wire          c0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess;             // c0_csr_translator_avalon_universal_slave_0_agent:m0_debugaccess -> c0_csr_translator:uav_debugaccess
	wire    [3:0] c0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable;              // c0_csr_translator_avalon_universal_slave_0_agent:m0_byteenable -> c0_csr_translator:uav_byteenable
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket;      // c0_csr_translator_avalon_universal_slave_0_agent:rf_source_endofpacket -> c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_endofpacket
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid;            // c0_csr_translator_avalon_universal_slave_0_agent:rf_source_valid -> c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_valid
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket;    // c0_csr_translator_avalon_universal_slave_0_agent:rf_source_startofpacket -> c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_startofpacket
	wire   [72:0] c0_csr_translator_avalon_universal_slave_0_agent_rf_source_data;             // c0_csr_translator_avalon_universal_slave_0_agent:rf_source_data -> c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_data
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready;            // c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:in_ready -> c0_csr_translator_avalon_universal_slave_0_agent:rf_source_ready
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket;   // c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_endofpacket -> c0_csr_translator_avalon_universal_slave_0_agent:rf_sink_endofpacket
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid;         // c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_valid -> c0_csr_translator_avalon_universal_slave_0_agent:rf_sink_valid
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket; // c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_startofpacket -> c0_csr_translator_avalon_universal_slave_0_agent:rf_sink_startofpacket
	wire   [72:0] c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data;          // c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_data -> c0_csr_translator_avalon_universal_slave_0_agent:rf_sink_data
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready;         // c0_csr_translator_avalon_universal_slave_0_agent:rf_sink_ready -> c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo:out_ready
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid;       // c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_valid -> c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_valid
	wire   [31:0] c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data;        // c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_data -> c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_data
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready;       // c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:in_ready -> c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_src_ready
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid;       // c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_valid -> c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_valid
	wire   [31:0] c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data;        // c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_data -> c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_data
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready;       // c0_csr_translator_avalon_universal_slave_0_agent:rdata_fifo_sink_ready -> c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo:out_ready
	wire          bridge_0_m0_translator_avalon_universal_master_0_agent_cp_endofpacket;       // bridge_0_m0_translator_avalon_universal_master_0_agent:cp_endofpacket -> addr_router:sink_endofpacket
	wire          bridge_0_m0_translator_avalon_universal_master_0_agent_cp_valid;             // bridge_0_m0_translator_avalon_universal_master_0_agent:cp_valid -> addr_router:sink_valid
	wire          bridge_0_m0_translator_avalon_universal_master_0_agent_cp_startofpacket;     // bridge_0_m0_translator_avalon_universal_master_0_agent:cp_startofpacket -> addr_router:sink_startofpacket
	wire   [71:0] bridge_0_m0_translator_avalon_universal_master_0_agent_cp_data;              // bridge_0_m0_translator_avalon_universal_master_0_agent:cp_data -> addr_router:sink_data
	wire          bridge_0_m0_translator_avalon_universal_master_0_agent_cp_ready;             // addr_router:sink_ready -> bridge_0_m0_translator_avalon_universal_master_0_agent:cp_ready
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket;             // p0_csr_translator_avalon_universal_slave_0_agent:rp_endofpacket -> id_router:sink_endofpacket
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rp_valid;                   // p0_csr_translator_avalon_universal_slave_0_agent:rp_valid -> id_router:sink_valid
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket;           // p0_csr_translator_avalon_universal_slave_0_agent:rp_startofpacket -> id_router:sink_startofpacket
	wire   [71:0] p0_csr_translator_avalon_universal_slave_0_agent_rp_data;                    // p0_csr_translator_avalon_universal_slave_0_agent:rp_data -> id_router:sink_data
	wire          p0_csr_translator_avalon_universal_slave_0_agent_rp_ready;                   // id_router:sink_ready -> p0_csr_translator_avalon_universal_slave_0_agent:rp_ready
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket;             // c0_csr_translator_avalon_universal_slave_0_agent:rp_endofpacket -> id_router_001:sink_endofpacket
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rp_valid;                   // c0_csr_translator_avalon_universal_slave_0_agent:rp_valid -> id_router_001:sink_valid
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket;           // c0_csr_translator_avalon_universal_slave_0_agent:rp_startofpacket -> id_router_001:sink_startofpacket
	wire   [71:0] c0_csr_translator_avalon_universal_slave_0_agent_rp_data;                    // c0_csr_translator_avalon_universal_slave_0_agent:rp_data -> id_router_001:sink_data
	wire          c0_csr_translator_avalon_universal_slave_0_agent_rp_ready;                   // id_router_001:sink_ready -> c0_csr_translator_avalon_universal_slave_0_agent:rp_ready
	wire          addr_router_src_endofpacket;                                                 // addr_router:src_endofpacket -> limiter:cmd_sink_endofpacket
	wire          addr_router_src_valid;                                                       // addr_router:src_valid -> limiter:cmd_sink_valid
	wire          addr_router_src_startofpacket;                                               // addr_router:src_startofpacket -> limiter:cmd_sink_startofpacket
	wire   [71:0] addr_router_src_data;                                                        // addr_router:src_data -> limiter:cmd_sink_data
	wire    [1:0] addr_router_src_channel;                                                     // addr_router:src_channel -> limiter:cmd_sink_channel
	wire          addr_router_src_ready;                                                       // limiter:cmd_sink_ready -> addr_router:src_ready
	wire          limiter_rsp_src_endofpacket;                                                 // limiter:rsp_src_endofpacket -> bridge_0_m0_translator_avalon_universal_master_0_agent:rp_endofpacket
	wire          limiter_rsp_src_valid;                                                       // limiter:rsp_src_valid -> bridge_0_m0_translator_avalon_universal_master_0_agent:rp_valid
	wire          limiter_rsp_src_startofpacket;                                               // limiter:rsp_src_startofpacket -> bridge_0_m0_translator_avalon_universal_master_0_agent:rp_startofpacket
	wire   [71:0] limiter_rsp_src_data;                                                        // limiter:rsp_src_data -> bridge_0_m0_translator_avalon_universal_master_0_agent:rp_data
	wire    [1:0] limiter_rsp_src_channel;                                                     // limiter:rsp_src_channel -> bridge_0_m0_translator_avalon_universal_master_0_agent:rp_channel
	wire          limiter_rsp_src_ready;                                                       // bridge_0_m0_translator_avalon_universal_master_0_agent:rp_ready -> limiter:rsp_src_ready
	wire          rst_controller_reset_out_reset;                                              // rst_controller:reset_out -> [addr_router:reset, bridge_0:reset, bridge_0_m0_translator:reset, bridge_0_m0_translator_avalon_universal_master_0_agent:reset, cmd_xbar_demux:reset, crosser:in_reset, crosser_001:in_reset, crosser_002:out_reset, crosser_003:out_reset, limiter:reset, rsp_xbar_mux:reset]
	wire          limiter_cmd_src_endofpacket;                                                 // limiter:cmd_src_endofpacket -> cmd_xbar_demux:sink_endofpacket
	wire          limiter_cmd_src_startofpacket;                                               // limiter:cmd_src_startofpacket -> cmd_xbar_demux:sink_startofpacket
	wire   [71:0] limiter_cmd_src_data;                                                        // limiter:cmd_src_data -> cmd_xbar_demux:sink_data
	wire    [1:0] limiter_cmd_src_channel;                                                     // limiter:cmd_src_channel -> cmd_xbar_demux:sink_channel
	wire          limiter_cmd_src_ready;                                                       // cmd_xbar_demux:sink_ready -> limiter:cmd_src_ready
	wire          rsp_xbar_mux_src_endofpacket;                                                // rsp_xbar_mux:src_endofpacket -> limiter:rsp_sink_endofpacket
	wire          rsp_xbar_mux_src_valid;                                                      // rsp_xbar_mux:src_valid -> limiter:rsp_sink_valid
	wire          rsp_xbar_mux_src_startofpacket;                                              // rsp_xbar_mux:src_startofpacket -> limiter:rsp_sink_startofpacket
	wire   [71:0] rsp_xbar_mux_src_data;                                                       // rsp_xbar_mux:src_data -> limiter:rsp_sink_data
	wire    [1:0] rsp_xbar_mux_src_channel;                                                    // rsp_xbar_mux:src_channel -> limiter:rsp_sink_channel
	wire          rsp_xbar_mux_src_ready;                                                      // limiter:rsp_sink_ready -> rsp_xbar_mux:src_ready
	wire          crosser_out_ready;                                                           // p0_csr_translator_avalon_universal_slave_0_agent:cp_ready -> crosser:out_ready
	wire          id_router_src_endofpacket;                                                   // id_router:src_endofpacket -> rsp_xbar_demux:sink_endofpacket
	wire          id_router_src_valid;                                                         // id_router:src_valid -> rsp_xbar_demux:sink_valid
	wire          id_router_src_startofpacket;                                                 // id_router:src_startofpacket -> rsp_xbar_demux:sink_startofpacket
	wire   [71:0] id_router_src_data;                                                          // id_router:src_data -> rsp_xbar_demux:sink_data
	wire    [1:0] id_router_src_channel;                                                       // id_router:src_channel -> rsp_xbar_demux:sink_channel
	wire          id_router_src_ready;                                                         // rsp_xbar_demux:sink_ready -> id_router:src_ready
	wire          crosser_001_out_ready;                                                       // c0_csr_translator_avalon_universal_slave_0_agent:cp_ready -> crosser_001:out_ready
	wire          id_router_001_src_endofpacket;                                               // id_router_001:src_endofpacket -> rsp_xbar_demux_001:sink_endofpacket
	wire          id_router_001_src_valid;                                                     // id_router_001:src_valid -> rsp_xbar_demux_001:sink_valid
	wire          id_router_001_src_startofpacket;                                             // id_router_001:src_startofpacket -> rsp_xbar_demux_001:sink_startofpacket
	wire   [71:0] id_router_001_src_data;                                                      // id_router_001:src_data -> rsp_xbar_demux_001:sink_data
	wire    [1:0] id_router_001_src_channel;                                                   // id_router_001:src_channel -> rsp_xbar_demux_001:sink_channel
	wire          id_router_001_src_ready;                                                     // rsp_xbar_demux_001:sink_ready -> id_router_001:src_ready
	wire          crosser_out_endofpacket;                                                     // crosser:out_endofpacket -> p0_csr_translator_avalon_universal_slave_0_agent:cp_endofpacket
	wire          crosser_out_valid;                                                           // crosser:out_valid -> p0_csr_translator_avalon_universal_slave_0_agent:cp_valid
	wire          crosser_out_startofpacket;                                                   // crosser:out_startofpacket -> p0_csr_translator_avalon_universal_slave_0_agent:cp_startofpacket
	wire   [71:0] crosser_out_data;                                                            // crosser:out_data -> p0_csr_translator_avalon_universal_slave_0_agent:cp_data
	wire    [1:0] crosser_out_channel;                                                         // crosser:out_channel -> p0_csr_translator_avalon_universal_slave_0_agent:cp_channel
	wire          cmd_xbar_demux_src0_endofpacket;                                             // cmd_xbar_demux:src0_endofpacket -> crosser:in_endofpacket
	wire          cmd_xbar_demux_src0_valid;                                                   // cmd_xbar_demux:src0_valid -> crosser:in_valid
	wire          cmd_xbar_demux_src0_startofpacket;                                           // cmd_xbar_demux:src0_startofpacket -> crosser:in_startofpacket
	wire   [71:0] cmd_xbar_demux_src0_data;                                                    // cmd_xbar_demux:src0_data -> crosser:in_data
	wire    [1:0] cmd_xbar_demux_src0_channel;                                                 // cmd_xbar_demux:src0_channel -> crosser:in_channel
	wire          cmd_xbar_demux_src0_ready;                                                   // crosser:in_ready -> cmd_xbar_demux:src0_ready
	wire          crosser_001_out_endofpacket;                                                 // crosser_001:out_endofpacket -> c0_csr_translator_avalon_universal_slave_0_agent:cp_endofpacket
	wire          crosser_001_out_valid;                                                       // crosser_001:out_valid -> c0_csr_translator_avalon_universal_slave_0_agent:cp_valid
	wire          crosser_001_out_startofpacket;                                               // crosser_001:out_startofpacket -> c0_csr_translator_avalon_universal_slave_0_agent:cp_startofpacket
	wire   [71:0] crosser_001_out_data;                                                        // crosser_001:out_data -> c0_csr_translator_avalon_universal_slave_0_agent:cp_data
	wire    [1:0] crosser_001_out_channel;                                                     // crosser_001:out_channel -> c0_csr_translator_avalon_universal_slave_0_agent:cp_channel
	wire          cmd_xbar_demux_src1_endofpacket;                                             // cmd_xbar_demux:src1_endofpacket -> crosser_001:in_endofpacket
	wire          cmd_xbar_demux_src1_valid;                                                   // cmd_xbar_demux:src1_valid -> crosser_001:in_valid
	wire          cmd_xbar_demux_src1_startofpacket;                                           // cmd_xbar_demux:src1_startofpacket -> crosser_001:in_startofpacket
	wire   [71:0] cmd_xbar_demux_src1_data;                                                    // cmd_xbar_demux:src1_data -> crosser_001:in_data
	wire    [1:0] cmd_xbar_demux_src1_channel;                                                 // cmd_xbar_demux:src1_channel -> crosser_001:in_channel
	wire          cmd_xbar_demux_src1_ready;                                                   // crosser_001:in_ready -> cmd_xbar_demux:src1_ready
	wire          crosser_002_out_endofpacket;                                                 // crosser_002:out_endofpacket -> rsp_xbar_mux:sink0_endofpacket
	wire          crosser_002_out_valid;                                                       // crosser_002:out_valid -> rsp_xbar_mux:sink0_valid
	wire          crosser_002_out_startofpacket;                                               // crosser_002:out_startofpacket -> rsp_xbar_mux:sink0_startofpacket
	wire   [71:0] crosser_002_out_data;                                                        // crosser_002:out_data -> rsp_xbar_mux:sink0_data
	wire    [1:0] crosser_002_out_channel;                                                     // crosser_002:out_channel -> rsp_xbar_mux:sink0_channel
	wire          crosser_002_out_ready;                                                       // rsp_xbar_mux:sink0_ready -> crosser_002:out_ready
	wire          rsp_xbar_demux_src0_endofpacket;                                             // rsp_xbar_demux:src0_endofpacket -> crosser_002:in_endofpacket
	wire          rsp_xbar_demux_src0_valid;                                                   // rsp_xbar_demux:src0_valid -> crosser_002:in_valid
	wire          rsp_xbar_demux_src0_startofpacket;                                           // rsp_xbar_demux:src0_startofpacket -> crosser_002:in_startofpacket
	wire   [71:0] rsp_xbar_demux_src0_data;                                                    // rsp_xbar_demux:src0_data -> crosser_002:in_data
	wire    [1:0] rsp_xbar_demux_src0_channel;                                                 // rsp_xbar_demux:src0_channel -> crosser_002:in_channel
	wire          rsp_xbar_demux_src0_ready;                                                   // crosser_002:in_ready -> rsp_xbar_demux:src0_ready
	wire          crosser_003_out_endofpacket;                                                 // crosser_003:out_endofpacket -> rsp_xbar_mux:sink1_endofpacket
	wire          crosser_003_out_valid;                                                       // crosser_003:out_valid -> rsp_xbar_mux:sink1_valid
	wire          crosser_003_out_startofpacket;                                               // crosser_003:out_startofpacket -> rsp_xbar_mux:sink1_startofpacket
	wire   [71:0] crosser_003_out_data;                                                        // crosser_003:out_data -> rsp_xbar_mux:sink1_data
	wire    [1:0] crosser_003_out_channel;                                                     // crosser_003:out_channel -> rsp_xbar_mux:sink1_channel
	wire          crosser_003_out_ready;                                                       // rsp_xbar_mux:sink1_ready -> crosser_003:out_ready
	wire          rsp_xbar_demux_001_src0_endofpacket;                                         // rsp_xbar_demux_001:src0_endofpacket -> crosser_003:in_endofpacket
	wire          rsp_xbar_demux_001_src0_valid;                                               // rsp_xbar_demux_001:src0_valid -> crosser_003:in_valid
	wire          rsp_xbar_demux_001_src0_startofpacket;                                       // rsp_xbar_demux_001:src0_startofpacket -> crosser_003:in_startofpacket
	wire   [71:0] rsp_xbar_demux_001_src0_data;                                                // rsp_xbar_demux_001:src0_data -> crosser_003:in_data
	wire    [1:0] rsp_xbar_demux_001_src0_channel;                                             // rsp_xbar_demux_001:src0_channel -> crosser_003:in_channel
	wire          rsp_xbar_demux_001_src0_ready;                                               // crosser_003:in_ready -> rsp_xbar_demux_001:src0_ready
	wire    [1:0] limiter_cmd_valid_data;                                                      // limiter:cmd_src_valid -> cmd_xbar_demux:sink_valid

	altera_mem_if_nextgen_ddr2_controller_0001 c0 (
		.afi_reset_n            (afi_reset_n),                                              //       afi_reset.reset_n
		.afi_clk                (afi_clk),                                                  //         afi_clk.clk
		.afi_half_clk           (afi_half_clk),                                             //    afi_half_clk.clk
		.local_cal_success      (local_cal_success),                                        //          status.local_cal_success
		.local_cal_fail         (local_cal_fail),                                           //                .local_cal_fail
		.local_init_done        (local_init_done),                                          //                .local_init_done
		.csr_write_req          (c0_csr_translator_avalon_anti_slave_0_write),              //             csr.write
		.csr_read_req           (c0_csr_translator_avalon_anti_slave_0_read),               //                .read
		.csr_waitrequest        (c0_csr_translator_avalon_anti_slave_0_waitrequest),        //                .waitrequest
		.csr_addr               (c0_csr_translator_avalon_anti_slave_0_address),            //                .address
		.csr_be                 (c0_csr_translator_avalon_anti_slave_0_byteenable),         //                .byteenable
		.csr_wdata              (c0_csr_translator_avalon_anti_slave_0_writedata),          //                .writedata
		.csr_rdata              (c0_csr_translator_avalon_anti_slave_0_readdata),           //                .readdata
		.csr_rdata_valid        (c0_csr_translator_avalon_anti_slave_0_readdatavalid),      //                .readdatavalid
		.csr_beginbursttransfer (c0_csr_translator_avalon_anti_slave_0_beginbursttransfer), //                .beginbursttransfer
		.csr_burst_count        (c0_csr_translator_avalon_anti_slave_0_burstcount),         //                .burstcount
		.afi_dm                 (c0_afi_afi_dm),                                            //             afi.afi_dm
		.afi_wlat               (p0_afi_afi_wlat),                                          //                .afi_wlat
		.afi_rdata_valid        (p0_afi_afi_rdata_valid),                                   //                .afi_rdata_valid
		.afi_rdata_en_full      (c0_afi_afi_rdata_en_full),                                 //                .afi_rdata_en_full
		.afi_addr               (c0_afi_afi_addr),                                          //                .afi_addr
		.afi_odt                (c0_afi_afi_odt),                                           //                .afi_odt
		.afi_rdata              (p0_afi_afi_rdata),                                         //                .afi_rdata
		.afi_wdata_valid        (c0_afi_afi_wdata_valid),                                   //                .afi_wdata_valid
		.afi_cal_req            (c0_afi_afi_cal_req),                                       //                .afi_cal_req
		.afi_rdata_en           (c0_afi_afi_rdata_en),                                      //                .afi_rdata_en
		.afi_rlat               (p0_afi_afi_rlat),                                          //                .afi_rlat
		.afi_cke                (c0_afi_afi_cke),                                           //                .afi_cke
		.afi_cal_fail           (p0_afi_afi_cal_fail),                                      //                .afi_cal_fail
		.afi_cas_n              (c0_afi_afi_cas_n),                                         //                .afi_cas_n
		.afi_we_n               (c0_afi_afi_we_n),                                          //                .afi_we_n
		.afi_dqs_burst          (c0_afi_afi_dqs_burst),                                     //                .afi_dqs_burst
		.afi_cal_success        (p0_afi_afi_cal_success),                                   //                .afi_cal_success
		.afi_ras_n              (c0_afi_afi_ras_n),                                         //                .afi_ras_n
		.afi_wdata              (c0_afi_afi_wdata),                                         //                .afi_wdata
		.afi_ba                 (c0_afi_afi_ba),                                            //                .afi_ba
		.afi_cs_n               (c0_afi_afi_cs_n),                                          //                .afi_cs_n
		.local_powerdn_req      (local_powerdn_req),                                        // local_powerdown.local_powerdn_req
		.local_powerdn_ack      (local_powerdn_ack),                                        //                .local_powerdn_ack
		.avl_ready              (avl_ready),                                                //             avl.waitrequest_n
		.avl_burstbegin         (avl_burstbegin),                                           //                .beginbursttransfer
		.avl_addr               (avl_addr),                                                 //                .address
		.avl_rdata_valid        (avl_rdata_valid),                                          //                .readdatavalid
		.avl_rdata              (avl_rdata),                                                //                .readdata
		.avl_wdata              (avl_wdata),                                                //                .writedata
		.avl_be                 (avl_be),                                                   //                .byteenable
		.avl_read_req           (avl_read_req),                                             //                .read
		.avl_write_req          (avl_write_req),                                            //                .write
		.avl_size               (avl_size)                                                  //                .burstcount
	);

	altera_mem_if_ddr2_phy_0001 p0 (
		.global_reset_n             (global_reset_n),                                      // global_reset.reset_n
		.soft_reset_n               (soft_reset_n),                                        //   soft_reset.reset_n
		.afi_reset_n                (afi_reset_n),                                         //    afi_reset.reset_n
		.afi_clk                    (afi_clk),                                             //      afi_clk.clk
		.afi_half_clk               (afi_half_clk),                                        // afi_half_clk.clk
		.pll_ref_clk                (pll_ref_clk),                                         //  pll_ref_clk.clk
		.afi_addr                   (c0_afi_afi_addr),                                     //          afi.afi_addr
		.afi_ba                     (c0_afi_afi_ba),                                       //             .afi_ba
		.afi_cke                    (c0_afi_afi_cke),                                      //             .afi_cke
		.afi_cs_n                   (c0_afi_afi_cs_n),                                     //             .afi_cs_n
		.afi_ras_n                  (c0_afi_afi_ras_n),                                    //             .afi_ras_n
		.afi_we_n                   (c0_afi_afi_we_n),                                     //             .afi_we_n
		.afi_cas_n                  (c0_afi_afi_cas_n),                                    //             .afi_cas_n
		.afi_odt                    (c0_afi_afi_odt),                                      //             .afi_odt
		.afi_dqs_burst              (c0_afi_afi_dqs_burst),                                //             .afi_dqs_burst
		.afi_wdata_valid            (c0_afi_afi_wdata_valid),                              //             .afi_wdata_valid
		.afi_wdata                  (c0_afi_afi_wdata),                                    //             .afi_wdata
		.afi_dm                     (c0_afi_afi_dm),                                       //             .afi_dm
		.afi_rdata                  (p0_afi_afi_rdata),                                    //             .afi_rdata
		.afi_rdata_en               (c0_afi_afi_rdata_en),                                 //             .afi_rdata_en
		.afi_rdata_en_full          (c0_afi_afi_rdata_en_full),                            //             .afi_rdata_en_full
		.afi_rdata_valid            (p0_afi_afi_rdata_valid),                              //             .afi_rdata_valid
		.afi_cal_success            (p0_afi_afi_cal_success),                              //             .afi_cal_success
		.afi_cal_fail               (p0_afi_afi_cal_fail),                                 //             .afi_cal_fail
		.afi_cal_req                (c0_afi_afi_cal_req),                                  //             .afi_cal_req
		.afi_wlat                   (p0_afi_afi_wlat),                                     //             .afi_wlat
		.afi_rlat                   (p0_afi_afi_rlat),                                     //             .afi_rlat
		.oct_rdn                    (oct_rdn),                                             //          oct.rdn
		.oct_rup                    (oct_rup),                                             //             .rup
		.mem_a                      (mem_a),                                               //       memory.mem_a
		.mem_ba                     (mem_ba),                                              //             .mem_ba
		.mem_ck                     (mem_ck),                                              //             .mem_ck
		.mem_ck_n                   (mem_ck_n),                                            //             .mem_ck_n
		.mem_cke                    (mem_cke),                                             //             .mem_cke
		.mem_cs_n                   (mem_cs_n),                                            //             .mem_cs_n
		.mem_dm                     (mem_dm),                                              //             .mem_dm
		.mem_ras_n                  (mem_ras_n),                                           //             .mem_ras_n
		.mem_cas_n                  (mem_cas_n),                                           //             .mem_cas_n
		.mem_we_n                   (mem_we_n),                                            //             .mem_we_n
		.mem_dq                     (mem_dq),                                              //             .mem_dq
		.mem_dqs                    (mem_dqs),                                             //             .mem_dqs
		.mem_dqs_n                  (mem_dqs_n),                                           //             .mem_dqs_n
		.mem_odt                    (mem_odt),                                             //             .mem_odt
		.csr_write_req              (p0_csr_translator_avalon_anti_slave_0_write),         //          csr.write
		.csr_read_req               (p0_csr_translator_avalon_anti_slave_0_read),          //             .read
		.csr_waitrequest            (p0_csr_translator_avalon_anti_slave_0_waitrequest),   //             .waitrequest
		.csr_addr                   (p0_csr_translator_avalon_anti_slave_0_address),       //             .address
		.csr_be                     (p0_csr_translator_avalon_anti_slave_0_byteenable),    //             .byteenable
		.csr_wdata                  (p0_csr_translator_avalon_anti_slave_0_writedata),     //             .writedata
		.csr_rdata                  (p0_csr_translator_avalon_anti_slave_0_readdata),      //             .readdata
		.csr_rdata_valid            (p0_csr_translator_avalon_anti_slave_0_readdatavalid), //             .readdatavalid
		.dll_delayctrl              (),                                                    //  (terminated)
		.seriesterminationcontrol   (),                                                    //  (terminated)
		.parallelterminationcontrol ()                                                     //  (terminated)
	);

	altera_avalon_mm_bridge #(
		.DATA_WIDTH        (32),
		.SYMBOL_WIDTH      (8),
		.ADDRESS_WIDTH     (19),
		.BURSTCOUNT_WIDTH  (1),
		.PIPELINE_COMMAND  (1),
		.PIPELINE_RESPONSE (1)
	) bridge_0 (
		.clk              (afi_clk),                        //   clk.clk
		.reset            (rst_controller_reset_out_reset), // reset.reset
		.s0_waitrequest   (csr_waitrequest),                //    s0.waitrequest
		.s0_readdata      (csr_readdata),                   //      .readdata
		.s0_readdatavalid (csr_readdatavalid),              //      .readdatavalid
		.s0_burstcount    (csr_burstcount),                 //      .burstcount
		.s0_writedata     (csr_writedata),                  //      .writedata
		.s0_address       (csr_address),                    //      .address
		.s0_write         (csr_write),                      //      .write
		.s0_read          (csr_read),                       //      .read
		.s0_byteenable    (csr_byteenable),                 //      .byteenable
		.s0_debugaccess   (csr_debugaccess),                //      .debugaccess
		.m0_waitrequest   (bridge_0_m0_waitrequest),        //    m0.waitrequest
		.m0_readdata      (bridge_0_m0_readdata),           //      .readdata
		.m0_readdatavalid (bridge_0_m0_readdatavalid),      //      .readdatavalid
		.m0_burstcount    (bridge_0_m0_burstcount),         //      .burstcount
		.m0_writedata     (bridge_0_m0_writedata),          //      .writedata
		.m0_address       (bridge_0_m0_address),            //      .address
		.m0_write         (bridge_0_m0_write),              //      .write
		.m0_read          (bridge_0_m0_read),               //      .read
		.m0_byteenable    (bridge_0_m0_byteenable),         //      .byteenable
		.m0_debugaccess   (bridge_0_m0_debugaccess)         //      .debugaccess
	);

	altera_merlin_master_translator #(
		.AV_ADDRESS_W                (19),
		.AV_DATA_W                   (32),
		.AV_BURSTCOUNT_W             (1),
		.AV_BYTEENABLE_W             (4),
		.UAV_ADDRESS_W               (19),
		.UAV_BURSTCOUNT_W            (3),
		.USE_READ                    (1),
		.USE_WRITE                   (1),
		.USE_BEGINBURSTTRANSFER      (0),
		.USE_BEGINTRANSFER           (0),
		.USE_CHIPSELECT              (0),
		.USE_BURSTCOUNT              (1),
		.USE_READDATAVALID           (1),
		.USE_WAITREQUEST             (1),
		.AV_SYMBOLS_PER_WORD         (4),
		.AV_ADDRESS_SYMBOLS          (1),
		.AV_BURSTCOUNT_SYMBOLS       (0),
		.AV_CONSTANT_BURST_BEHAVIOR  (0),
		.UAV_CONSTANT_BURST_BEHAVIOR (0),
		.AV_LINEWRAPBURSTS           (0),
		.AV_REGISTERINCOMINGSIGNALS  (0)
	) bridge_0_m0_translator (
		.clk                   (afi_clk),                                                        //                       clk.clk
		.reset                 (rst_controller_reset_out_reset),                                 //                     reset.reset
		.uav_address           (bridge_0_m0_translator_avalon_universal_master_0_address),       // avalon_universal_master_0.address
		.uav_burstcount        (bridge_0_m0_translator_avalon_universal_master_0_burstcount),    //                          .burstcount
		.uav_read              (bridge_0_m0_translator_avalon_universal_master_0_read),          //                          .read
		.uav_write             (bridge_0_m0_translator_avalon_universal_master_0_write),         //                          .write
		.uav_waitrequest       (bridge_0_m0_translator_avalon_universal_master_0_waitrequest),   //                          .waitrequest
		.uav_readdatavalid     (bridge_0_m0_translator_avalon_universal_master_0_readdatavalid), //                          .readdatavalid
		.uav_byteenable        (bridge_0_m0_translator_avalon_universal_master_0_byteenable),    //                          .byteenable
		.uav_readdata          (bridge_0_m0_translator_avalon_universal_master_0_readdata),      //                          .readdata
		.uav_writedata         (bridge_0_m0_translator_avalon_universal_master_0_writedata),     //                          .writedata
		.uav_lock              (bridge_0_m0_translator_avalon_universal_master_0_lock),          //                          .lock
		.uav_debugaccess       (bridge_0_m0_translator_avalon_universal_master_0_debugaccess),   //                          .debugaccess
		.av_address            (bridge_0_m0_address),                                            //      avalon_anti_master_0.address
		.av_waitrequest        (bridge_0_m0_waitrequest),                                        //                          .waitrequest
		.av_burstcount         (bridge_0_m0_burstcount),                                         //                          .burstcount
		.av_byteenable         (bridge_0_m0_byteenable),                                         //                          .byteenable
		.av_read               (bridge_0_m0_read),                                               //                          .read
		.av_readdata           (bridge_0_m0_readdata),                                           //                          .readdata
		.av_readdatavalid      (bridge_0_m0_readdatavalid),                                      //                          .readdatavalid
		.av_write              (bridge_0_m0_write),                                              //                          .write
		.av_writedata          (bridge_0_m0_writedata),                                          //                          .writedata
		.av_debugaccess        (bridge_0_m0_debugaccess),                                        //                          .debugaccess
		.av_beginbursttransfer (1'b0),                                                           //               (terminated)
		.av_begintransfer      (1'b0),                                                           //               (terminated)
		.av_chipselect         (1'b0),                                                           //               (terminated)
		.av_lock               (1'b0),                                                           //               (terminated)
		.uav_clken             (),                                                               //               (terminated)
		.av_clken              (1'b1)                                                            //               (terminated)
	);

	altera_merlin_slave_translator #(
		.AV_ADDRESS_W                   (16),
		.AV_DATA_W                      (32),
		.UAV_DATA_W                     (32),
		.AV_BURSTCOUNT_W                (1),
		.AV_BYTEENABLE_W                (4),
		.UAV_BYTEENABLE_W               (4),
		.UAV_ADDRESS_W                  (19),
		.UAV_BURSTCOUNT_W               (3),
		.AV_READLATENCY                 (0),
		.USE_READDATAVALID              (1),
		.USE_WAITREQUEST                (1),
		.USE_UAV_CLKEN                  (0),
		.AV_SYMBOLS_PER_WORD            (4),
		.AV_ADDRESS_SYMBOLS             (0),
		.AV_BURSTCOUNT_SYMBOLS          (0),
		.AV_CONSTANT_BURST_BEHAVIOR     (0),
		.UAV_CONSTANT_BURST_BEHAVIOR    (0),
		.AV_REQUIRE_UNALIGNED_ADDRESSES (0),
		.CHIPSELECT_THROUGH_READLATENCY (0),
		.AV_READ_WAIT_CYCLES            (1),
		.AV_WRITE_WAIT_CYCLES           (0),
		.AV_SETUP_WAIT_CYCLES           (0),
		.AV_DATA_HOLD_CYCLES            (0)
	) p0_csr_translator (
		.clk                   (afi_clk),                                                           //                      clk.clk
		.reset                 (~afi_reset_n),                                                      //                    reset.reset
		.uav_address           (p0_csr_translator_avalon_universal_slave_0_agent_m0_address),       // avalon_universal_slave_0.address
		.uav_burstcount        (p0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount),    //                         .burstcount
		.uav_read              (p0_csr_translator_avalon_universal_slave_0_agent_m0_read),          //                         .read
		.uav_write             (p0_csr_translator_avalon_universal_slave_0_agent_m0_write),         //                         .write
		.uav_waitrequest       (p0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest),   //                         .waitrequest
		.uav_readdatavalid     (p0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid), //                         .readdatavalid
		.uav_byteenable        (p0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable),    //                         .byteenable
		.uav_readdata          (p0_csr_translator_avalon_universal_slave_0_agent_m0_readdata),      //                         .readdata
		.uav_writedata         (p0_csr_translator_avalon_universal_slave_0_agent_m0_writedata),     //                         .writedata
		.uav_lock              (p0_csr_translator_avalon_universal_slave_0_agent_m0_lock),          //                         .lock
		.uav_debugaccess       (p0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess),   //                         .debugaccess
		.av_address            (p0_csr_translator_avalon_anti_slave_0_address),                     //      avalon_anti_slave_0.address
		.av_write              (p0_csr_translator_avalon_anti_slave_0_write),                       //                         .write
		.av_read               (p0_csr_translator_avalon_anti_slave_0_read),                        //                         .read
		.av_readdata           (p0_csr_translator_avalon_anti_slave_0_readdata),                    //                         .readdata
		.av_writedata          (p0_csr_translator_avalon_anti_slave_0_writedata),                   //                         .writedata
		.av_byteenable         (p0_csr_translator_avalon_anti_slave_0_byteenable),                  //                         .byteenable
		.av_readdatavalid      (p0_csr_translator_avalon_anti_slave_0_readdatavalid),               //                         .readdatavalid
		.av_waitrequest        (p0_csr_translator_avalon_anti_slave_0_waitrequest),                 //                         .waitrequest
		.av_begintransfer      (),                                                                  //              (terminated)
		.av_beginbursttransfer (),                                                                  //              (terminated)
		.av_burstcount         (),                                                                  //              (terminated)
		.av_writebyteenable    (),                                                                  //              (terminated)
		.av_lock               (),                                                                  //              (terminated)
		.av_chipselect         (),                                                                  //              (terminated)
		.av_clken              (),                                                                  //              (terminated)
		.uav_clken             (1'b0),                                                              //              (terminated)
		.av_debugaccess        (),                                                                  //              (terminated)
		.av_outputenable       ()                                                                   //              (terminated)
	);

	altera_merlin_slave_translator #(
		.AV_ADDRESS_W                   (16),
		.AV_DATA_W                      (32),
		.UAV_DATA_W                     (32),
		.AV_BURSTCOUNT_W                (1),
		.AV_BYTEENABLE_W                (4),
		.UAV_BYTEENABLE_W               (4),
		.UAV_ADDRESS_W                  (19),
		.UAV_BURSTCOUNT_W               (3),
		.AV_READLATENCY                 (0),
		.USE_READDATAVALID              (1),
		.USE_WAITREQUEST                (1),
		.USE_UAV_CLKEN                  (0),
		.AV_SYMBOLS_PER_WORD            (4),
		.AV_ADDRESS_SYMBOLS             (0),
		.AV_BURSTCOUNT_SYMBOLS          (0),
		.AV_CONSTANT_BURST_BEHAVIOR     (0),
		.UAV_CONSTANT_BURST_BEHAVIOR    (0),
		.AV_REQUIRE_UNALIGNED_ADDRESSES (0),
		.CHIPSELECT_THROUGH_READLATENCY (0),
		.AV_READ_WAIT_CYCLES            (1),
		.AV_WRITE_WAIT_CYCLES           (0),
		.AV_SETUP_WAIT_CYCLES           (0),
		.AV_DATA_HOLD_CYCLES            (0)
	) c0_csr_translator (
		.clk                   (afi_clk),                                                           //                      clk.clk
		.reset                 (~afi_reset_n),                                                      //                    reset.reset
		.uav_address           (c0_csr_translator_avalon_universal_slave_0_agent_m0_address),       // avalon_universal_slave_0.address
		.uav_burstcount        (c0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount),    //                         .burstcount
		.uav_read              (c0_csr_translator_avalon_universal_slave_0_agent_m0_read),          //                         .read
		.uav_write             (c0_csr_translator_avalon_universal_slave_0_agent_m0_write),         //                         .write
		.uav_waitrequest       (c0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest),   //                         .waitrequest
		.uav_readdatavalid     (c0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid), //                         .readdatavalid
		.uav_byteenable        (c0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable),    //                         .byteenable
		.uav_readdata          (c0_csr_translator_avalon_universal_slave_0_agent_m0_readdata),      //                         .readdata
		.uav_writedata         (c0_csr_translator_avalon_universal_slave_0_agent_m0_writedata),     //                         .writedata
		.uav_lock              (c0_csr_translator_avalon_universal_slave_0_agent_m0_lock),          //                         .lock
		.uav_debugaccess       (c0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess),   //                         .debugaccess
		.av_address            (c0_csr_translator_avalon_anti_slave_0_address),                     //      avalon_anti_slave_0.address
		.av_write              (c0_csr_translator_avalon_anti_slave_0_write),                       //                         .write
		.av_read               (c0_csr_translator_avalon_anti_slave_0_read),                        //                         .read
		.av_readdata           (c0_csr_translator_avalon_anti_slave_0_readdata),                    //                         .readdata
		.av_writedata          (c0_csr_translator_avalon_anti_slave_0_writedata),                   //                         .writedata
		.av_beginbursttransfer (c0_csr_translator_avalon_anti_slave_0_beginbursttransfer),          //                         .beginbursttransfer
		.av_burstcount         (c0_csr_translator_avalon_anti_slave_0_burstcount),                  //                         .burstcount
		.av_byteenable         (c0_csr_translator_avalon_anti_slave_0_byteenable),                  //                         .byteenable
		.av_readdatavalid      (c0_csr_translator_avalon_anti_slave_0_readdatavalid),               //                         .readdatavalid
		.av_waitrequest        (c0_csr_translator_avalon_anti_slave_0_waitrequest),                 //                         .waitrequest
		.av_begintransfer      (),                                                                  //              (terminated)
		.av_writebyteenable    (),                                                                  //              (terminated)
		.av_lock               (),                                                                  //              (terminated)
		.av_chipselect         (),                                                                  //              (terminated)
		.av_clken              (),                                                                  //              (terminated)
		.uav_clken             (1'b0),                                                              //              (terminated)
		.av_debugaccess        (),                                                                  //              (terminated)
		.av_outputenable       ()                                                                   //              (terminated)
	);

	altera_merlin_master_agent #(
		.PKT_PROTECTION_H          (71),
		.PKT_PROTECTION_L          (71),
		.PKT_BEGIN_BURST           (66),
		.PKT_BURSTWRAP_H           (65),
		.PKT_BURSTWRAP_L           (63),
		.PKT_BYTE_CNT_H            (62),
		.PKT_BYTE_CNT_L            (60),
		.PKT_ADDR_H                (54),
		.PKT_ADDR_L                (36),
		.PKT_TRANS_COMPRESSED_READ (55),
		.PKT_TRANS_POSTED          (56),
		.PKT_TRANS_WRITE           (57),
		.PKT_TRANS_READ            (58),
		.PKT_TRANS_LOCK            (59),
		.PKT_DATA_H                (31),
		.PKT_DATA_L                (0),
		.PKT_BYTEEN_H              (35),
		.PKT_BYTEEN_L              (32),
		.PKT_SRC_ID_H              (68),
		.PKT_SRC_ID_L              (67),
		.PKT_DEST_ID_H             (70),
		.PKT_DEST_ID_L             (69),
		.ST_DATA_W                 (72),
		.ST_CHANNEL_W              (2),
		.AV_BURSTCOUNT_W           (3),
		.SUPPRESS_0_BYTEEN_RSP     (0),
		.ID                        (1),
		.BURSTWRAP_VALUE           (7)
	) bridge_0_m0_translator_avalon_universal_master_0_agent (
		.clk              (afi_clk),                                                                 //       clk.clk
		.reset            (rst_controller_reset_out_reset),                                          // clk_reset.reset
		.av_address       (bridge_0_m0_translator_avalon_universal_master_0_address),                //        av.address
		.av_write         (bridge_0_m0_translator_avalon_universal_master_0_write),                  //          .write
		.av_read          (bridge_0_m0_translator_avalon_universal_master_0_read),                   //          .read
		.av_writedata     (bridge_0_m0_translator_avalon_universal_master_0_writedata),              //          .writedata
		.av_readdata      (bridge_0_m0_translator_avalon_universal_master_0_readdata),               //          .readdata
		.av_waitrequest   (bridge_0_m0_translator_avalon_universal_master_0_waitrequest),            //          .waitrequest
		.av_readdatavalid (bridge_0_m0_translator_avalon_universal_master_0_readdatavalid),          //          .readdatavalid
		.av_byteenable    (bridge_0_m0_translator_avalon_universal_master_0_byteenable),             //          .byteenable
		.av_burstcount    (bridge_0_m0_translator_avalon_universal_master_0_burstcount),             //          .burstcount
		.av_debugaccess   (bridge_0_m0_translator_avalon_universal_master_0_debugaccess),            //          .debugaccess
		.av_lock          (bridge_0_m0_translator_avalon_universal_master_0_lock),                   //          .lock
		.cp_valid         (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_valid),         //        cp.valid
		.cp_data          (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_data),          //          .data
		.cp_startofpacket (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_startofpacket), //          .startofpacket
		.cp_endofpacket   (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_endofpacket),   //          .endofpacket
		.cp_ready         (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_ready),         //          .ready
		.rp_valid         (limiter_rsp_src_valid),                                                   //        rp.valid
		.rp_data          (limiter_rsp_src_data),                                                    //          .data
		.rp_channel       (limiter_rsp_src_channel),                                                 //          .channel
		.rp_startofpacket (limiter_rsp_src_startofpacket),                                           //          .startofpacket
		.rp_endofpacket   (limiter_rsp_src_endofpacket),                                             //          .endofpacket
		.rp_ready         (limiter_rsp_src_ready)                                                    //          .ready
	);

	altera_merlin_slave_agent #(
		.PKT_DATA_H                (31),
		.PKT_DATA_L                (0),
		.PKT_BEGIN_BURST           (66),
		.PKT_SYMBOL_W              (8),
		.PKT_BYTEEN_H              (35),
		.PKT_BYTEEN_L              (32),
		.PKT_ADDR_H                (54),
		.PKT_ADDR_L                (36),
		.PKT_TRANS_COMPRESSED_READ (55),
		.PKT_TRANS_POSTED          (56),
		.PKT_TRANS_WRITE           (57),
		.PKT_TRANS_READ            (58),
		.PKT_TRANS_LOCK            (59),
		.PKT_SRC_ID_H              (68),
		.PKT_SRC_ID_L              (67),
		.PKT_DEST_ID_H             (70),
		.PKT_DEST_ID_L             (69),
		.PKT_BURSTWRAP_H           (65),
		.PKT_BURSTWRAP_L           (63),
		.PKT_BYTE_CNT_H            (62),
		.PKT_BYTE_CNT_L            (60),
		.PKT_PROTECTION_H          (71),
		.PKT_PROTECTION_L          (71),
		.ST_CHANNEL_W              (2),
		.ST_DATA_W                 (72),
		.AVS_BURSTCOUNT_W          (3),
		.SUPPRESS_0_BYTEEN_CMD     (0),
		.PREVENT_FIFO_OVERFLOW     (1)
	) p0_csr_translator_avalon_universal_slave_0_agent (
		.clk                     (afi_clk),                                                                     //             clk.clk
		.reset                   (~afi_reset_n),                                                                //       clk_reset.reset
		.m0_address              (p0_csr_translator_avalon_universal_slave_0_agent_m0_address),                 //              m0.address
		.m0_burstcount           (p0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount),              //                .burstcount
		.m0_byteenable           (p0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable),              //                .byteenable
		.m0_debugaccess          (p0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess),             //                .debugaccess
		.m0_lock                 (p0_csr_translator_avalon_universal_slave_0_agent_m0_lock),                    //                .lock
		.m0_readdata             (p0_csr_translator_avalon_universal_slave_0_agent_m0_readdata),                //                .readdata
		.m0_readdatavalid        (p0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid),           //                .readdatavalid
		.m0_read                 (p0_csr_translator_avalon_universal_slave_0_agent_m0_read),                    //                .read
		.m0_waitrequest          (p0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest),             //                .waitrequest
		.m0_writedata            (p0_csr_translator_avalon_universal_slave_0_agent_m0_writedata),               //                .writedata
		.m0_write                (p0_csr_translator_avalon_universal_slave_0_agent_m0_write),                   //                .write
		.rp_endofpacket          (p0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket),             //              rp.endofpacket
		.rp_ready                (p0_csr_translator_avalon_universal_slave_0_agent_rp_ready),                   //                .ready
		.rp_valid                (p0_csr_translator_avalon_universal_slave_0_agent_rp_valid),                   //                .valid
		.rp_data                 (p0_csr_translator_avalon_universal_slave_0_agent_rp_data),                    //                .data
		.rp_startofpacket        (p0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket),           //                .startofpacket
		.cp_ready                (crosser_out_ready),                                                           //              cp.ready
		.cp_valid                (crosser_out_valid),                                                           //                .valid
		.cp_data                 (crosser_out_data),                                                            //                .data
		.cp_startofpacket        (crosser_out_startofpacket),                                                   //                .startofpacket
		.cp_endofpacket          (crosser_out_endofpacket),                                                     //                .endofpacket
		.cp_channel              (crosser_out_channel),                                                         //                .channel
		.rf_sink_ready           (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready),         //         rf_sink.ready
		.rf_sink_valid           (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid),         //                .valid
		.rf_sink_startofpacket   (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket), //                .startofpacket
		.rf_sink_endofpacket     (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket),   //                .endofpacket
		.rf_sink_data            (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data),          //                .data
		.rf_source_ready         (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready),            //       rf_source.ready
		.rf_source_valid         (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid),            //                .valid
		.rf_source_startofpacket (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket),    //                .startofpacket
		.rf_source_endofpacket   (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket),      //                .endofpacket
		.rf_source_data          (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_data),             //                .data
		.rdata_fifo_sink_ready   (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready),       // rdata_fifo_sink.ready
		.rdata_fifo_sink_valid   (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid),       //                .valid
		.rdata_fifo_sink_data    (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data),        //                .data
		.rdata_fifo_src_ready    (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready),       //  rdata_fifo_src.ready
		.rdata_fifo_src_valid    (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid),       //                .valid
		.rdata_fifo_src_data     (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data)         //                .data
	);

	altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (73),
		.FIFO_DEPTH          (5),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (1),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (1),
		.USE_MEMORY_BLOCKS   (0),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo (
		.clk               (afi_clk),                                                                     //       clk.clk
		.reset             (~afi_reset_n),                                                                // clk_reset.reset
		.in_data           (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_data),             //        in.data
		.in_valid          (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid),            //          .valid
		.in_ready          (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready),            //          .ready
		.in_startofpacket  (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket),    //          .startofpacket
		.in_endofpacket    (p0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket),      //          .endofpacket
		.out_data          (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data),          //       out.data
		.out_valid         (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid),         //          .valid
		.out_ready         (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready),         //          .ready
		.out_startofpacket (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket), //          .startofpacket
		.out_endofpacket   (p0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket),   //          .endofpacket
		.csr_address       (2'b00),                                                                       // (terminated)
		.csr_read          (1'b0),                                                                        // (terminated)
		.csr_write         (1'b0),                                                                        // (terminated)
		.csr_readdata      (),                                                                            // (terminated)
		.csr_writedata     (32'b00000000000000000000000000000000),                                        // (terminated)
		.almost_full_data  (),                                                                            // (terminated)
		.almost_empty_data (),                                                                            // (terminated)
		.in_empty          (1'b0),                                                                        // (terminated)
		.out_empty         (),                                                                            // (terminated)
		.in_error          (1'b0),                                                                        // (terminated)
		.out_error         (),                                                                            // (terminated)
		.in_channel        (1'b0),                                                                        // (terminated)
		.out_channel       ()                                                                             // (terminated)
	);

	altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (32),
		.FIFO_DEPTH          (8),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (0),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (3),
		.USE_MEMORY_BLOCKS   (1),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo (
		.clk               (afi_clk),                                                               //       clk.clk
		.reset             (~afi_reset_n),                                                          // clk_reset.reset
		.in_data           (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data),  //        in.data
		.in_valid          (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid), //          .valid
		.in_ready          (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready), //          .ready
		.out_data          (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data),  //       out.data
		.out_valid         (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid), //          .valid
		.out_ready         (p0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready), //          .ready
		.csr_address       (2'b00),                                                                 // (terminated)
		.csr_read          (1'b0),                                                                  // (terminated)
		.csr_write         (1'b0),                                                                  // (terminated)
		.csr_readdata      (),                                                                      // (terminated)
		.csr_writedata     (32'b00000000000000000000000000000000),                                  // (terminated)
		.almost_full_data  (),                                                                      // (terminated)
		.almost_empty_data (),                                                                      // (terminated)
		.in_startofpacket  (1'b0),                                                                  // (terminated)
		.in_endofpacket    (1'b0),                                                                  // (terminated)
		.out_startofpacket (),                                                                      // (terminated)
		.out_endofpacket   (),                                                                      // (terminated)
		.in_empty          (1'b0),                                                                  // (terminated)
		.out_empty         (),                                                                      // (terminated)
		.in_error          (1'b0),                                                                  // (terminated)
		.out_error         (),                                                                      // (terminated)
		.in_channel        (1'b0),                                                                  // (terminated)
		.out_channel       ()                                                                       // (terminated)
	);

	altera_merlin_slave_agent #(
		.PKT_DATA_H                (31),
		.PKT_DATA_L                (0),
		.PKT_BEGIN_BURST           (66),
		.PKT_SYMBOL_W              (8),
		.PKT_BYTEEN_H              (35),
		.PKT_BYTEEN_L              (32),
		.PKT_ADDR_H                (54),
		.PKT_ADDR_L                (36),
		.PKT_TRANS_COMPRESSED_READ (55),
		.PKT_TRANS_POSTED          (56),
		.PKT_TRANS_WRITE           (57),
		.PKT_TRANS_READ            (58),
		.PKT_TRANS_LOCK            (59),
		.PKT_SRC_ID_H              (68),
		.PKT_SRC_ID_L              (67),
		.PKT_DEST_ID_H             (70),
		.PKT_DEST_ID_L             (69),
		.PKT_BURSTWRAP_H           (65),
		.PKT_BURSTWRAP_L           (63),
		.PKT_BYTE_CNT_H            (62),
		.PKT_BYTE_CNT_L            (60),
		.PKT_PROTECTION_H          (71),
		.PKT_PROTECTION_L          (71),
		.ST_CHANNEL_W              (2),
		.ST_DATA_W                 (72),
		.AVS_BURSTCOUNT_W          (3),
		.SUPPRESS_0_BYTEEN_CMD     (0),
		.PREVENT_FIFO_OVERFLOW     (1)
	) c0_csr_translator_avalon_universal_slave_0_agent (
		.clk                     (afi_clk),                                                                     //             clk.clk
		.reset                   (~afi_reset_n),                                                                //       clk_reset.reset
		.m0_address              (c0_csr_translator_avalon_universal_slave_0_agent_m0_address),                 //              m0.address
		.m0_burstcount           (c0_csr_translator_avalon_universal_slave_0_agent_m0_burstcount),              //                .burstcount
		.m0_byteenable           (c0_csr_translator_avalon_universal_slave_0_agent_m0_byteenable),              //                .byteenable
		.m0_debugaccess          (c0_csr_translator_avalon_universal_slave_0_agent_m0_debugaccess),             //                .debugaccess
		.m0_lock                 (c0_csr_translator_avalon_universal_slave_0_agent_m0_lock),                    //                .lock
		.m0_readdata             (c0_csr_translator_avalon_universal_slave_0_agent_m0_readdata),                //                .readdata
		.m0_readdatavalid        (c0_csr_translator_avalon_universal_slave_0_agent_m0_readdatavalid),           //                .readdatavalid
		.m0_read                 (c0_csr_translator_avalon_universal_slave_0_agent_m0_read),                    //                .read
		.m0_waitrequest          (c0_csr_translator_avalon_universal_slave_0_agent_m0_waitrequest),             //                .waitrequest
		.m0_writedata            (c0_csr_translator_avalon_universal_slave_0_agent_m0_writedata),               //                .writedata
		.m0_write                (c0_csr_translator_avalon_universal_slave_0_agent_m0_write),                   //                .write
		.rp_endofpacket          (c0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket),             //              rp.endofpacket
		.rp_ready                (c0_csr_translator_avalon_universal_slave_0_agent_rp_ready),                   //                .ready
		.rp_valid                (c0_csr_translator_avalon_universal_slave_0_agent_rp_valid),                   //                .valid
		.rp_data                 (c0_csr_translator_avalon_universal_slave_0_agent_rp_data),                    //                .data
		.rp_startofpacket        (c0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket),           //                .startofpacket
		.cp_ready                (crosser_001_out_ready),                                                       //              cp.ready
		.cp_valid                (crosser_001_out_valid),                                                       //                .valid
		.cp_data                 (crosser_001_out_data),                                                        //                .data
		.cp_startofpacket        (crosser_001_out_startofpacket),                                               //                .startofpacket
		.cp_endofpacket          (crosser_001_out_endofpacket),                                                 //                .endofpacket
		.cp_channel              (crosser_001_out_channel),                                                     //                .channel
		.rf_sink_ready           (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready),         //         rf_sink.ready
		.rf_sink_valid           (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid),         //                .valid
		.rf_sink_startofpacket   (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket), //                .startofpacket
		.rf_sink_endofpacket     (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket),   //                .endofpacket
		.rf_sink_data            (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data),          //                .data
		.rf_source_ready         (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready),            //       rf_source.ready
		.rf_source_valid         (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid),            //                .valid
		.rf_source_startofpacket (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket),    //                .startofpacket
		.rf_source_endofpacket   (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket),      //                .endofpacket
		.rf_source_data          (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_data),             //                .data
		.rdata_fifo_sink_ready   (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready),       // rdata_fifo_sink.ready
		.rdata_fifo_sink_valid   (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid),       //                .valid
		.rdata_fifo_sink_data    (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data),        //                .data
		.rdata_fifo_src_ready    (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready),       //  rdata_fifo_src.ready
		.rdata_fifo_src_valid    (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid),       //                .valid
		.rdata_fifo_src_data     (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data)         //                .data
	);

	altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (73),
		.FIFO_DEPTH          (5),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (1),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (1),
		.USE_MEMORY_BLOCKS   (0),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo (
		.clk               (afi_clk),                                                                     //       clk.clk
		.reset             (~afi_reset_n),                                                                // clk_reset.reset
		.in_data           (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_data),             //        in.data
		.in_valid          (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_valid),            //          .valid
		.in_ready          (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_ready),            //          .ready
		.in_startofpacket  (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_startofpacket),    //          .startofpacket
		.in_endofpacket    (c0_csr_translator_avalon_universal_slave_0_agent_rf_source_endofpacket),      //          .endofpacket
		.out_data          (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_data),          //       out.data
		.out_valid         (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_valid),         //          .valid
		.out_ready         (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_ready),         //          .ready
		.out_startofpacket (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_startofpacket), //          .startofpacket
		.out_endofpacket   (c0_csr_translator_avalon_universal_slave_0_agent_rsp_fifo_out_endofpacket),   //          .endofpacket
		.csr_address       (2'b00),                                                                       // (terminated)
		.csr_read          (1'b0),                                                                        // (terminated)
		.csr_write         (1'b0),                                                                        // (terminated)
		.csr_readdata      (),                                                                            // (terminated)
		.csr_writedata     (32'b00000000000000000000000000000000),                                        // (terminated)
		.almost_full_data  (),                                                                            // (terminated)
		.almost_empty_data (),                                                                            // (terminated)
		.in_empty          (1'b0),                                                                        // (terminated)
		.out_empty         (),                                                                            // (terminated)
		.in_error          (1'b0),                                                                        // (terminated)
		.out_error         (),                                                                            // (terminated)
		.in_channel        (1'b0),                                                                        // (terminated)
		.out_channel       ()                                                                             // (terminated)
	);

	altera_avalon_sc_fifo #(
		.SYMBOLS_PER_BEAT    (1),
		.BITS_PER_SYMBOL     (32),
		.FIFO_DEPTH          (8),
		.CHANNEL_WIDTH       (0),
		.ERROR_WIDTH         (0),
		.USE_PACKETS         (0),
		.USE_FILL_LEVEL      (0),
		.EMPTY_LATENCY       (3),
		.USE_MEMORY_BLOCKS   (1),
		.USE_STORE_FORWARD   (0),
		.USE_ALMOST_FULL_IF  (0),
		.USE_ALMOST_EMPTY_IF (0)
	) c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo (
		.clk               (afi_clk),                                                               //       clk.clk
		.reset             (~afi_reset_n),                                                          // clk_reset.reset
		.in_data           (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_data),  //        in.data
		.in_valid          (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_valid), //          .valid
		.in_ready          (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_src_ready), //          .ready
		.out_data          (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_data),  //       out.data
		.out_valid         (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_valid), //          .valid
		.out_ready         (c0_csr_translator_avalon_universal_slave_0_agent_rdata_fifo_out_ready), //          .ready
		.csr_address       (2'b00),                                                                 // (terminated)
		.csr_read          (1'b0),                                                                  // (terminated)
		.csr_write         (1'b0),                                                                  // (terminated)
		.csr_readdata      (),                                                                      // (terminated)
		.csr_writedata     (32'b00000000000000000000000000000000),                                  // (terminated)
		.almost_full_data  (),                                                                      // (terminated)
		.almost_empty_data (),                                                                      // (terminated)
		.in_startofpacket  (1'b0),                                                                  // (terminated)
		.in_endofpacket    (1'b0),                                                                  // (terminated)
		.out_startofpacket (),                                                                      // (terminated)
		.out_endofpacket   (),                                                                      // (terminated)
		.in_empty          (1'b0),                                                                  // (terminated)
		.out_empty         (),                                                                      // (terminated)
		.in_error          (1'b0),                                                                  // (terminated)
		.out_error         (),                                                                      // (terminated)
		.in_channel        (1'b0),                                                                  // (terminated)
		.out_channel       ()                                                                       // (terminated)
	);

	altera_merlin_router_0001 addr_router (
		.sink_ready         (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_ready),         //      sink.ready
		.sink_valid         (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_valid),         //          .valid
		.sink_data          (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_data),          //          .data
		.sink_startofpacket (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_startofpacket), //          .startofpacket
		.sink_endofpacket   (bridge_0_m0_translator_avalon_universal_master_0_agent_cp_endofpacket),   //          .endofpacket
		.clk                (afi_clk),                                                                 //       clk.clk
		.reset              (rst_controller_reset_out_reset),                                          // clk_reset.reset
		.src_ready          (addr_router_src_ready),                                                   //       src.ready
		.src_valid          (addr_router_src_valid),                                                   //          .valid
		.src_data           (addr_router_src_data),                                                    //          .data
		.src_channel        (addr_router_src_channel),                                                 //          .channel
		.src_startofpacket  (addr_router_src_startofpacket),                                           //          .startofpacket
		.src_endofpacket    (addr_router_src_endofpacket)                                              //          .endofpacket
	);

	altera_merlin_router_0002 id_router (
		.sink_ready         (p0_csr_translator_avalon_universal_slave_0_agent_rp_ready),         //      sink.ready
		.sink_valid         (p0_csr_translator_avalon_universal_slave_0_agent_rp_valid),         //          .valid
		.sink_data          (p0_csr_translator_avalon_universal_slave_0_agent_rp_data),          //          .data
		.sink_startofpacket (p0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket), //          .startofpacket
		.sink_endofpacket   (p0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket),   //          .endofpacket
		.clk                (afi_clk),                                                           //       clk.clk
		.reset              (~afi_reset_n),                                                      // clk_reset.reset
		.src_ready          (id_router_src_ready),                                               //       src.ready
		.src_valid          (id_router_src_valid),                                               //          .valid
		.src_data           (id_router_src_data),                                                //          .data
		.src_channel        (id_router_src_channel),                                             //          .channel
		.src_startofpacket  (id_router_src_startofpacket),                                       //          .startofpacket
		.src_endofpacket    (id_router_src_endofpacket)                                          //          .endofpacket
	);

	altera_merlin_router_0002 id_router_001 (
		.sink_ready         (c0_csr_translator_avalon_universal_slave_0_agent_rp_ready),         //      sink.ready
		.sink_valid         (c0_csr_translator_avalon_universal_slave_0_agent_rp_valid),         //          .valid
		.sink_data          (c0_csr_translator_avalon_universal_slave_0_agent_rp_data),          //          .data
		.sink_startofpacket (c0_csr_translator_avalon_universal_slave_0_agent_rp_startofpacket), //          .startofpacket
		.sink_endofpacket   (c0_csr_translator_avalon_universal_slave_0_agent_rp_endofpacket),   //          .endofpacket
		.clk                (afi_clk),                                                           //       clk.clk
		.reset              (~afi_reset_n),                                                      // clk_reset.reset
		.src_ready          (id_router_001_src_ready),                                           //       src.ready
		.src_valid          (id_router_001_src_valid),                                           //          .valid
		.src_data           (id_router_001_src_data),                                            //          .data
		.src_channel        (id_router_001_src_channel),                                         //          .channel
		.src_startofpacket  (id_router_001_src_startofpacket),                                   //          .startofpacket
		.src_endofpacket    (id_router_001_src_endofpacket)                                      //          .endofpacket
	);

	altera_merlin_traffic_limiter #(
		.PKT_DEST_ID_H             (70),
		.PKT_DEST_ID_L             (69),
		.PKT_TRANS_POSTED          (56),
		.MAX_OUTSTANDING_RESPONSES (12),
		.PIPELINED                 (0),
		.ST_DATA_W                 (72),
		.ST_CHANNEL_W              (2),
		.VALID_WIDTH               (2),
		.ENFORCE_ORDER             (1),
		.PKT_BYTE_CNT_H            (62),
		.PKT_BYTE_CNT_L            (60),
		.PKT_BYTEEN_H              (35),
		.PKT_BYTEEN_L              (32)
	) limiter (
		.clk                    (afi_clk),                        //       clk.clk
		.reset                  (rst_controller_reset_out_reset), // clk_reset.reset
		.cmd_sink_ready         (addr_router_src_ready),          //  cmd_sink.ready
		.cmd_sink_valid         (addr_router_src_valid),          //          .valid
		.cmd_sink_data          (addr_router_src_data),           //          .data
		.cmd_sink_channel       (addr_router_src_channel),        //          .channel
		.cmd_sink_startofpacket (addr_router_src_startofpacket),  //          .startofpacket
		.cmd_sink_endofpacket   (addr_router_src_endofpacket),    //          .endofpacket
		.cmd_src_ready          (limiter_cmd_src_ready),          //   cmd_src.ready
		.cmd_src_data           (limiter_cmd_src_data),           //          .data
		.cmd_src_channel        (limiter_cmd_src_channel),        //          .channel
		.cmd_src_startofpacket  (limiter_cmd_src_startofpacket),  //          .startofpacket
		.cmd_src_endofpacket    (limiter_cmd_src_endofpacket),    //          .endofpacket
		.rsp_sink_ready         (rsp_xbar_mux_src_ready),         //  rsp_sink.ready
		.rsp_sink_valid         (rsp_xbar_mux_src_valid),         //          .valid
		.rsp_sink_channel       (rsp_xbar_mux_src_channel),       //          .channel
		.rsp_sink_data          (rsp_xbar_mux_src_data),          //          .data
		.rsp_sink_startofpacket (rsp_xbar_mux_src_startofpacket), //          .startofpacket
		.rsp_sink_endofpacket   (rsp_xbar_mux_src_endofpacket),   //          .endofpacket
		.rsp_src_ready          (limiter_rsp_src_ready),          //   rsp_src.ready
		.rsp_src_valid          (limiter_rsp_src_valid),          //          .valid
		.rsp_src_data           (limiter_rsp_src_data),           //          .data
		.rsp_src_channel        (limiter_rsp_src_channel),        //          .channel
		.rsp_src_startofpacket  (limiter_rsp_src_startofpacket),  //          .startofpacket
		.rsp_src_endofpacket    (limiter_rsp_src_endofpacket),    //          .endofpacket
		.cmd_src_valid          (limiter_cmd_valid_data)          // cmd_valid.data
	);

	altera_reset_controller #(
		.NUM_RESET_INPUTS        (1),
		.OUTPUT_RESET_SYNC_EDGES ("deassert"),
		.SYNC_DEPTH              (2)
	) rst_controller (
		.reset_in0  (~afi_reset_n),                   // reset_in0.reset
		.clk        (afi_clk),                        //       clk.clk
		.reset_out  (rst_controller_reset_out_reset), // reset_out.reset
		.reset_in1  (1'b0),                           // (terminated)
		.reset_in2  (1'b0),                           // (terminated)
		.reset_in3  (1'b0),                           // (terminated)
		.reset_in4  (1'b0),                           // (terminated)
		.reset_in5  (1'b0),                           // (terminated)
		.reset_in6  (1'b0),                           // (terminated)
		.reset_in7  (1'b0),                           // (terminated)
		.reset_in8  (1'b0),                           // (terminated)
		.reset_in9  (1'b0),                           // (terminated)
		.reset_in10 (1'b0),                           // (terminated)
		.reset_in11 (1'b0),                           // (terminated)
		.reset_in12 (1'b0),                           // (terminated)
		.reset_in13 (1'b0),                           // (terminated)
		.reset_in14 (1'b0),                           // (terminated)
		.reset_in15 (1'b0)                            // (terminated)
	);

	altera_merlin_demultiplexer_0001 cmd_xbar_demux (
		.clk                (afi_clk),                           //        clk.clk
		.reset              (rst_controller_reset_out_reset),    //  clk_reset.reset
		.sink_ready         (limiter_cmd_src_ready),             //       sink.ready
		.sink_channel       (limiter_cmd_src_channel),           //           .channel
		.sink_data          (limiter_cmd_src_data),              //           .data
		.sink_startofpacket (limiter_cmd_src_startofpacket),     //           .startofpacket
		.sink_endofpacket   (limiter_cmd_src_endofpacket),       //           .endofpacket
		.sink_valid         (limiter_cmd_valid_data),            // sink_valid.data
		.src0_ready         (cmd_xbar_demux_src0_ready),         //       src0.ready
		.src0_valid         (cmd_xbar_demux_src0_valid),         //           .valid
		.src0_data          (cmd_xbar_demux_src0_data),          //           .data
		.src0_channel       (cmd_xbar_demux_src0_channel),       //           .channel
		.src0_startofpacket (cmd_xbar_demux_src0_startofpacket), //           .startofpacket
		.src0_endofpacket   (cmd_xbar_demux_src0_endofpacket),   //           .endofpacket
		.src1_ready         (cmd_xbar_demux_src1_ready),         //       src1.ready
		.src1_valid         (cmd_xbar_demux_src1_valid),         //           .valid
		.src1_data          (cmd_xbar_demux_src1_data),          //           .data
		.src1_channel       (cmd_xbar_demux_src1_channel),       //           .channel
		.src1_startofpacket (cmd_xbar_demux_src1_startofpacket), //           .startofpacket
		.src1_endofpacket   (cmd_xbar_demux_src1_endofpacket)    //           .endofpacket
	);

	altera_merlin_demultiplexer_0002 rsp_xbar_demux (
		.clk                (afi_clk),                           //       clk.clk
		.reset              (~afi_reset_n),                      // clk_reset.reset
		.sink_ready         (id_router_src_ready),               //      sink.ready
		.sink_channel       (id_router_src_channel),             //          .channel
		.sink_data          (id_router_src_data),                //          .data
		.sink_startofpacket (id_router_src_startofpacket),       //          .startofpacket
		.sink_endofpacket   (id_router_src_endofpacket),         //          .endofpacket
		.sink_valid         (id_router_src_valid),               //          .valid
		.src0_ready         (rsp_xbar_demux_src0_ready),         //      src0.ready
		.src0_valid         (rsp_xbar_demux_src0_valid),         //          .valid
		.src0_data          (rsp_xbar_demux_src0_data),          //          .data
		.src0_channel       (rsp_xbar_demux_src0_channel),       //          .channel
		.src0_startofpacket (rsp_xbar_demux_src0_startofpacket), //          .startofpacket
		.src0_endofpacket   (rsp_xbar_demux_src0_endofpacket)    //          .endofpacket
	);

	altera_merlin_demultiplexer_0002 rsp_xbar_demux_001 (
		.clk                (afi_clk),                               //       clk.clk
		.reset              (~afi_reset_n),                          // clk_reset.reset
		.sink_ready         (id_router_001_src_ready),               //      sink.ready
		.sink_channel       (id_router_001_src_channel),             //          .channel
		.sink_data          (id_router_001_src_data),                //          .data
		.sink_startofpacket (id_router_001_src_startofpacket),       //          .startofpacket
		.sink_endofpacket   (id_router_001_src_endofpacket),         //          .endofpacket
		.sink_valid         (id_router_001_src_valid),               //          .valid
		.src0_ready         (rsp_xbar_demux_001_src0_ready),         //      src0.ready
		.src0_valid         (rsp_xbar_demux_001_src0_valid),         //          .valid
		.src0_data          (rsp_xbar_demux_001_src0_data),          //          .data
		.src0_channel       (rsp_xbar_demux_001_src0_channel),       //          .channel
		.src0_startofpacket (rsp_xbar_demux_001_src0_startofpacket), //          .startofpacket
		.src0_endofpacket   (rsp_xbar_demux_001_src0_endofpacket)    //          .endofpacket
	);

	altera_merlin_multiplexer_0001 rsp_xbar_mux (
		.clk                 (afi_clk),                        //       clk.clk
		.reset               (rst_controller_reset_out_reset), // clk_reset.reset
		.src_ready           (rsp_xbar_mux_src_ready),         //       src.ready
		.src_valid           (rsp_xbar_mux_src_valid),         //          .valid
		.src_data            (rsp_xbar_mux_src_data),          //          .data
		.src_channel         (rsp_xbar_mux_src_channel),       //          .channel
		.src_startofpacket   (rsp_xbar_mux_src_startofpacket), //          .startofpacket
		.src_endofpacket     (rsp_xbar_mux_src_endofpacket),   //          .endofpacket
		.sink0_ready         (crosser_002_out_ready),          //     sink0.ready
		.sink0_valid         (crosser_002_out_valid),          //          .valid
		.sink0_channel       (crosser_002_out_channel),        //          .channel
		.sink0_data          (crosser_002_out_data),           //          .data
		.sink0_startofpacket (crosser_002_out_startofpacket),  //          .startofpacket
		.sink0_endofpacket   (crosser_002_out_endofpacket),    //          .endofpacket
		.sink1_ready         (crosser_003_out_ready),          //     sink1.ready
		.sink1_valid         (crosser_003_out_valid),          //          .valid
		.sink1_channel       (crosser_003_out_channel),        //          .channel
		.sink1_data          (crosser_003_out_data),           //          .data
		.sink1_startofpacket (crosser_003_out_startofpacket),  //          .startofpacket
		.sink1_endofpacket   (crosser_003_out_endofpacket)     //          .endofpacket
	);

	altera_avalon_st_handshake_clock_crosser #(
		.DATA_WIDTH          (72),
		.BITS_PER_SYMBOL     (72),
		.USE_PACKETS         (1),
		.USE_CHANNEL         (1),
		.CHANNEL_WIDTH       (2),
		.USE_ERROR           (0),
		.ERROR_WIDTH         (1),
		.VALID_SYNC_DEPTH    (2),
		.READY_SYNC_DEPTH    (2),
		.USE_OUTPUT_PIPELINE (0)
	) crosser (
		.in_clk            (afi_clk),                           //        in_clk.clk
		.in_reset          (rst_controller_reset_out_reset),    //  in_clk_reset.reset
		.out_clk           (afi_clk),                           //       out_clk.clk
		.out_reset         (~afi_reset_n),                      // out_clk_reset.reset
		.in_ready          (cmd_xbar_demux_src0_ready),         //            in.ready
		.in_valid          (cmd_xbar_demux_src0_valid),         //              .valid
		.in_startofpacket  (cmd_xbar_demux_src0_startofpacket), //              .startofpacket
		.in_endofpacket    (cmd_xbar_demux_src0_endofpacket),   //              .endofpacket
		.in_channel        (cmd_xbar_demux_src0_channel),       //              .channel
		.in_data           (cmd_xbar_demux_src0_data),          //              .data
		.out_ready         (crosser_out_ready),                 //           out.ready
		.out_valid         (crosser_out_valid),                 //              .valid
		.out_startofpacket (crosser_out_startofpacket),         //              .startofpacket
		.out_endofpacket   (crosser_out_endofpacket),           //              .endofpacket
		.out_channel       (crosser_out_channel),               //              .channel
		.out_data          (crosser_out_data),                  //              .data
		.in_empty          (1'b0),                              //   (terminated)
		.in_error          (1'b0),                              //   (terminated)
		.out_empty         (),                                  //   (terminated)
		.out_error         ()                                   //   (terminated)
	);

	altera_avalon_st_handshake_clock_crosser #(
		.DATA_WIDTH          (72),
		.BITS_PER_SYMBOL     (72),
		.USE_PACKETS         (1),
		.USE_CHANNEL         (1),
		.CHANNEL_WIDTH       (2),
		.USE_ERROR           (0),
		.ERROR_WIDTH         (1),
		.VALID_SYNC_DEPTH    (2),
		.READY_SYNC_DEPTH    (2),
		.USE_OUTPUT_PIPELINE (0)
	) crosser_001 (
		.in_clk            (afi_clk),                           //        in_clk.clk
		.in_reset          (rst_controller_reset_out_reset),    //  in_clk_reset.reset
		.out_clk           (afi_clk),                           //       out_clk.clk
		.out_reset         (~afi_reset_n),                      // out_clk_reset.reset
		.in_ready          (cmd_xbar_demux_src1_ready),         //            in.ready
		.in_valid          (cmd_xbar_demux_src1_valid),         //              .valid
		.in_startofpacket  (cmd_xbar_demux_src1_startofpacket), //              .startofpacket
		.in_endofpacket    (cmd_xbar_demux_src1_endofpacket),   //              .endofpacket
		.in_channel        (cmd_xbar_demux_src1_channel),       //              .channel
		.in_data           (cmd_xbar_demux_src1_data),          //              .data
		.out_ready         (crosser_001_out_ready),             //           out.ready
		.out_valid         (crosser_001_out_valid),             //              .valid
		.out_startofpacket (crosser_001_out_startofpacket),     //              .startofpacket
		.out_endofpacket   (crosser_001_out_endofpacket),       //              .endofpacket
		.out_channel       (crosser_001_out_channel),           //              .channel
		.out_data          (crosser_001_out_data),              //              .data
		.in_empty          (1'b0),                              //   (terminated)
		.in_error          (1'b0),                              //   (terminated)
		.out_empty         (),                                  //   (terminated)
		.out_error         ()                                   //   (terminated)
	);

	altera_avalon_st_handshake_clock_crosser #(
		.DATA_WIDTH          (72),
		.BITS_PER_SYMBOL     (72),
		.USE_PACKETS         (1),
		.USE_CHANNEL         (1),
		.CHANNEL_WIDTH       (2),
		.USE_ERROR           (0),
		.ERROR_WIDTH         (1),
		.VALID_SYNC_DEPTH    (2),
		.READY_SYNC_DEPTH    (2),
		.USE_OUTPUT_PIPELINE (0)
	) crosser_002 (
		.in_clk            (afi_clk),                           //        in_clk.clk
		.in_reset          (~afi_reset_n),                      //  in_clk_reset.reset
		.out_clk           (afi_clk),                           //       out_clk.clk
		.out_reset         (rst_controller_reset_out_reset),    // out_clk_reset.reset
		.in_ready          (rsp_xbar_demux_src0_ready),         //            in.ready
		.in_valid          (rsp_xbar_demux_src0_valid),         //              .valid
		.in_startofpacket  (rsp_xbar_demux_src0_startofpacket), //              .startofpacket
		.in_endofpacket    (rsp_xbar_demux_src0_endofpacket),   //              .endofpacket
		.in_channel        (rsp_xbar_demux_src0_channel),       //              .channel
		.in_data           (rsp_xbar_demux_src0_data),          //              .data
		.out_ready         (crosser_002_out_ready),             //           out.ready
		.out_valid         (crosser_002_out_valid),             //              .valid
		.out_startofpacket (crosser_002_out_startofpacket),     //              .startofpacket
		.out_endofpacket   (crosser_002_out_endofpacket),       //              .endofpacket
		.out_channel       (crosser_002_out_channel),           //              .channel
		.out_data          (crosser_002_out_data),              //              .data
		.in_empty          (1'b0),                              //   (terminated)
		.in_error          (1'b0),                              //   (terminated)
		.out_empty         (),                                  //   (terminated)
		.out_error         ()                                   //   (terminated)
	);

	altera_avalon_st_handshake_clock_crosser #(
		.DATA_WIDTH          (72),
		.BITS_PER_SYMBOL     (72),
		.USE_PACKETS         (1),
		.USE_CHANNEL         (1),
		.CHANNEL_WIDTH       (2),
		.USE_ERROR           (0),
		.ERROR_WIDTH         (1),
		.VALID_SYNC_DEPTH    (2),
		.READY_SYNC_DEPTH    (2),
		.USE_OUTPUT_PIPELINE (0)
	) crosser_003 (
		.in_clk            (afi_clk),                               //        in_clk.clk
		.in_reset          (~afi_reset_n),                          //  in_clk_reset.reset
		.out_clk           (afi_clk),                               //       out_clk.clk
		.out_reset         (rst_controller_reset_out_reset),        // out_clk_reset.reset
		.in_ready          (rsp_xbar_demux_001_src0_ready),         //            in.ready
		.in_valid          (rsp_xbar_demux_001_src0_valid),         //              .valid
		.in_startofpacket  (rsp_xbar_demux_001_src0_startofpacket), //              .startofpacket
		.in_endofpacket    (rsp_xbar_demux_001_src0_endofpacket),   //              .endofpacket
		.in_channel        (rsp_xbar_demux_001_src0_channel),       //              .channel
		.in_data           (rsp_xbar_demux_001_src0_data),          //              .data
		.out_ready         (crosser_003_out_ready),                 //           out.ready
		.out_valid         (crosser_003_out_valid),                 //              .valid
		.out_startofpacket (crosser_003_out_startofpacket),         //              .startofpacket
		.out_endofpacket   (crosser_003_out_endofpacket),           //              .endofpacket
		.out_channel       (crosser_003_out_channel),               //              .channel
		.out_data          (crosser_003_out_data),                  //              .data
		.in_empty          (1'b0),                                  //   (terminated)
		.in_error          (1'b0),                                  //   (terminated)
		.out_empty         (),                                      //   (terminated)
		.out_error         ()                                       //   (terminated)
	);

endmodule
