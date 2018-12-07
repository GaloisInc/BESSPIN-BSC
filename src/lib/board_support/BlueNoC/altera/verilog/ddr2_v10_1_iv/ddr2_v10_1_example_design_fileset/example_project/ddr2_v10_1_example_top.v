// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

// synthesis translate_off
`timescale 1ps / 1ps
// synthesis translate_on

module ddr2_v10_1_example_top (
    mem_a,
    mem_ba,
    mem_ck,
    mem_ck_n,
    mem_cke,
    mem_cs_n,
    mem_dm,
    mem_odt,
    mem_ras_n,
    mem_cas_n,
    mem_we_n,
    mem_dq,
    mem_dqs,
    mem_dqs_n,
`ifdef PNF_PER_BIT_OUTPUT
	pnf_per_bit,
	pnf_per_bit_persist,
`endif
	reset_request_n,
	pass,
	fail,
	test_complete,
    local_init_done,
	afi_cal_success,
	afi_cal_fail,





	pll_ref_clk,
	global_reset_n,
	oct_rdn,
	oct_rup
);


input			pll_ref_clk;
input			global_reset_n;
input			oct_rdn;
input			oct_rup;

`ifdef PNF_PER_BIT_OUTPUT
output	[256-1:0]		pnf_per_bit;
output	[256-1:0]		pnf_per_bit_persist;
`endif
output	reset_request_n;
output  [16-1:0]  mem_a;
output  [3-1:0]  mem_ba;
output  [2-1:0]	mem_ck;
output  [2-1:0] 	mem_ck_n;
output  [2-1:0] mem_cke;
output  [2-1:0] mem_cs_n;
output  [8-1:0]  mem_dm;
output  [2-1:0] mem_odt;
output  [1-1:0] mem_ras_n;
output  [1-1:0] mem_cas_n;
output  [1-1:0] mem_we_n;
inout   [64-1:0]  mem_dq;
inout   [8-1:0] mem_dqs;
inout   [8-1:0] mem_dqs_n;
wire	[31:0] afi_cal_debug_info;
output	pass;
output	fail;
output	test_complete;
output  local_init_done;
output	afi_cal_success;
output	afi_cal_fail;





parameter  CSR_ADDR_WIDTH      = 16; 
parameter  CSR_BE_WIDTH        = 4;
parameter  CSR_DATA_WIDTH      = 32;





// PLL/DLL Interface
wire  pll_locked;

// OCT interface

wire	pll_afi_clk;        
wire	pll_mem_clk;
wire	pll_write_clk;
wire	pll_addr_cmd_clk;
wire	pll_afi_half_clk;
wire	pll_avl_clk;
wire	pll_config_clk;
wire    [6-1:0]  dll_delayctrl;

wire			afi_clk;
wire			afi_half_clk;
wire			afi_reset_n;
wire	soft_reset_n;
`ifdef ENABLE_SOFT_RESET
dut_iss_source #(
	.WIDTH(1)
) iss_soft_reset (
	.source(soft_reset_n)
);
`else
assign soft_reset_n = 1'b1;
`endif

wire			avl_ready;
wire			avl_write_req;
wire			avl_read_req;
wire			avl_burstbegin;
wire	[30-1:0]	avl_addr;
wire	[3-1:0]	avl_size;
wire	[256-1:0]	avl_wdata;
wire			avl_rdata_valid;
wire	[256-1:0]	avl_rdata;
wire	[32-1:0]	avl_be;

wire    [2-1:0]    cs_n;

wire    [CSR_ADDR_WIDTH - 1: 0]    csr_addr;
wire    [CSR_BE_WIDTH - 1: 0]      csr_be;
wire                               csr_read_req;
wire    [CSR_DATA_WIDTH - 1: 0]    csr_wdata;
wire                               csr_write_req;
wire   [CSR_DATA_WIDTH - 1: 0]     csr_rdata;
wire                               csr_rdata_valid;
wire                               csr_waitrequest;




assign mem_cs_n = cs_n;


  assign csr_write_req = 1'b0;
  assign csr_read_req = 1'b0;
  assign csr_addr = 16'b0;
  assign csr_be = 4'b0;
  assign csr_wdata = 32'b0;



ddr2_v10_1 mem_if (
    .pll_ref_clk(pll_ref_clk),
	// when PHY is the PLL/DLL master, these will be outputs that can be shared with other components of the chip
	// when PHY is the PLL/DLL slave, these will be inputs from the PLL/DLL instantiations below 
    .pll_afi_clk            (pll_afi_clk),
    .pll_addr_cmd_clk       (pll_addr_cmd_clk),
    .pll_mem_clk            (pll_mem_clk),
    .pll_write_clk          (pll_write_clk),
	.pll_afi_half_clk		(pll_afi_half_clk),
	.pll_avl_clk			(pll_avl_clk),
	.pll_config_clk			(pll_config_clk),
    .pll_locked             (pll_locked),
    .dll_delayctrl      	(dll_delayctrl),
     .csr_addr (csr_addr),
     .csr_be (csr_be),
     .csr_read_req (csr_read_req),
     .csr_wdata (csr_wdata),
     .csr_write_req (csr_write_req),
     .csr_rdata (csr_rdata),
     .csr_rdata_valid (csr_rdata_valid),
     .csr_waitrequest (csr_waitrequest),
	.global_reset_n(global_reset_n),
	.soft_reset_n(soft_reset_n), // reset input intended for SOPC builder user or to be controlled by other system reset logic
	.oct_rdn(oct_rdn),
	.oct_rup(oct_rup),
    .parallelterminationcontrol     (),
    .seriesterminationcontrol       (),

	.reset_request_n(reset_request_n),
	.afi_clk(afi_clk),
	.afi_half_clk(afi_half_clk),
	.afi_reset_n(afi_reset_n),
	.afi_cal_debug_info(afi_cal_debug_info),
    .local_init_done(local_init_done),
	.afi_cal_success(afi_cal_success),
	.afi_cal_fail(afi_cal_fail),
    .mem_a          (mem_a),
    .mem_ba         (mem_ba),
    .mem_ck         (mem_ck),
    .mem_ck_n       (mem_ck_n),
    .mem_cke        (mem_cke),
    .mem_cs_n       (cs_n),
    .mem_dm         (mem_dm),
    .mem_odt        (mem_odt),
    .mem_ras_n      (mem_ras_n),
    .mem_cas_n      (mem_cas_n),
    .mem_we_n       (mem_we_n),
    .mem_dq         (mem_dq),
    .mem_dqs        (mem_dqs),
    .mem_dqs_n      (mem_dqs_n),
	.avl_ready(avl_ready),
	.avl_write_req(avl_write_req),
	.avl_read_req(avl_read_req),
    .avl_burstbegin(avl_burstbegin),
	.avl_addr(avl_addr),
	.avl_size(avl_size),
    .avl_be(avl_be),
	.avl_wdata(avl_wdata),
	.avl_rdata_valid(avl_rdata_valid),
	.avl_rdata(avl_rdata)
);


`ifdef ENABLE_DRIVER_RESET
wire driver_reset_n;
wire driver_reset_sync_n;

dut_iss_source #(
	.WIDTH(1)
) iss_driver_reset (
	.source(driver_reset_n)
);


dut_reset_sync	driver_clk_sync (
	.reset_n		(driver_reset_n),
	.clk			(afi_clk),
	.reset_n_sync	(driver_reset_sync_n)
);

`endif


ddr2_v10_1_example_driver driver (
    .clk(afi_clk),
`ifdef ENABLE_DRIVER_RESET
	.reset_n(driver_reset_sync_n),
`else
	.reset_n(afi_reset_n),
`endif
	.avl_ready(avl_ready),
	.avl_write_req(avl_write_req),
	.avl_read_req(avl_read_req),
	.avl_burstbegin(avl_burstbegin),
	.avl_addr(avl_addr),
	.avl_size(avl_size),
	.avl_be(avl_be),
	.avl_wdata(avl_wdata),
	.avl_rdata(avl_rdata),
	.avl_rdata_valid(avl_rdata_valid),
`ifdef PNF_PER_BIT_OUTPUT
	.pnf_per_bit(pnf_per_bit),
	.pnf_per_bit_persist(pnf_per_bit_persist),
`endif
	.pass(pass),
	.fail(fail),
	.test_complete(test_complete)
);




// On chip termination (OCT) control block








`ifdef ENABLE_STP_PORT
reg stp_driver_test_complete  /* synthesis syn_noprune syn_preserve = 1 */;
reg stp_driver_pass  /* synthesis syn_noprune syn_preserve = 1 */;
reg stp_driver_fail  /* synthesis syn_noprune syn_preserve = 1 */;
reg stp_afi_cal_fail  /* synthesis syn_noprune syn_preserve = 1 */;
reg stp_afi_cal_success  /* synthesis syn_noprune syn_preserve = 1 */;
reg stp_trigger  /* synthesis syn_noprune syn_preserve = 1 */;

reg	[256-1:0]		stp_pnf_per_bit /* synthesis syn_noprune syn_preserve = 1 */;
reg	[256-1:0]		stp_pnf_per_bit_persist /* synthesis syn_noprune syn_preserve = 1 */;

reg [31:0]         stp_afi_cal_debug_info /* synthesis syn_noprune syn_preserve = 1 */;

 
always @ (posedge pll_afi_clk) begin
		stp_driver_test_complete <= test_complete;
		stp_driver_pass <= pass;
		stp_driver_fail <= fail;
		stp_afi_cal_fail <= afi_cal_fail;
		stp_afi_cal_success <= afi_cal_success;
		stp_trigger <= test_complete | afi_cal_fail;
`ifdef PNF_PER_BIT_OUTPUT
		stp_pnf_per_bit <= pnf_per_bit;
		stp_pnf_per_bit_persist <= pnf_per_bit_persist;
`endif
		stp_afi_cal_debug_info <= afi_cal_debug_info;
end
`endif


endmodule


