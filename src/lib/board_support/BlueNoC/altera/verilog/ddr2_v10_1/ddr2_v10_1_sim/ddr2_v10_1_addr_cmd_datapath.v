// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_addr_cmd_datapath(
    clk,
    reset_n,
    afi_address,
	afi_bank,
	afi_cs_n,
	afi_cke,
	afi_odt,
	afi_ras_n,
	afi_cas_n,
	afi_we_n,
    phy_ddio_address_l,
    phy_ddio_address_h,
	phy_ddio_bank_l,
	phy_ddio_bank_h,
	phy_ddio_cs_n_l,
	phy_ddio_cs_n_h,
	phy_ddio_cke_l,
	phy_ddio_cke_h,
	phy_ddio_odt_l,
	phy_ddio_odt_h,
	phy_ddio_we_n_l,
	phy_ddio_we_n_h,
	phy_ddio_ras_n_l,
	phy_ddio_ras_n_h,
	phy_ddio_cas_n_l,
	phy_ddio_cas_n_h
);



parameter MEM_ADDRESS_WIDTH     = ""; 
parameter MEM_BANK_WIDTH        = ""; 
parameter MEM_CHIP_SELECT_WIDTH = ""; 
parameter MEM_CLK_EN_WIDTH 		= ""; 
parameter MEM_ODT_WIDTH 		= ""; 
parameter MEM_DM_WIDTH          = ""; 
parameter MEM_CONTROL_WIDTH     = ""; 
parameter MEM_DQ_WIDTH          = ""; 
parameter MEM_READ_DQS_WIDTH    = ""; 
parameter MEM_WRITE_DQS_WIDTH   = ""; 

parameter AFI_ADDRESS_WIDTH         = ""; 
parameter AFI_BANK_WIDTH            = ""; 
parameter AFI_CHIP_SELECT_WIDTH     = ""; 
parameter AFI_CLK_EN_WIDTH     		= ""; 
parameter AFI_ODT_WIDTH     		= ""; 
parameter AFI_DATA_MASK_WIDTH       = ""; 
parameter AFI_CONTROL_WIDTH         = ""; 
parameter AFI_DATA_WIDTH            = ""; 





input	reset_n;
input	clk;

input	[AFI_ADDRESS_WIDTH-1:0]	afi_address;
input   [AFI_BANK_WIDTH-1:0]    afi_bank;
input   [AFI_CONTROL_WIDTH-1:0] afi_cas_n;
input   [AFI_CLK_EN_WIDTH-1:0] afi_cke;
input   [AFI_CHIP_SELECT_WIDTH-1:0] afi_cs_n;
input   [AFI_ODT_WIDTH-1:0] afi_odt;
input   [AFI_CONTROL_WIDTH-1:0] afi_ras_n;
input   [AFI_CONTROL_WIDTH-1:0] afi_we_n;
output	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_l;
output	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_h;
output    [MEM_BANK_WIDTH-1:0]    phy_ddio_bank_l;
output    [MEM_BANK_WIDTH-1:0]    phy_ddio_bank_h;
output    [MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_l;
output    [MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_h;
output    [MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_l;
output    [MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_h;
output    [MEM_ODT_WIDTH-1:0] phy_ddio_odt_l;
output    [MEM_ODT_WIDTH-1:0] phy_ddio_odt_h;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_l;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_h;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_l;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_h;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_l;
output    [MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_h;


wire	[MEM_ADDRESS_WIDTH-1:0]	afi_address_high;
wire	[MEM_ADDRESS_WIDTH-1:0]	afi_address_low;
wire	[MEM_BANK_WIDTH-1:0]    afi_bank_high;
wire	[MEM_BANK_WIDTH-1:0]    afi_bank_low;
wire	[MEM_CONTROL_WIDTH-1:0] afi_cas_n_high;
wire	[MEM_CONTROL_WIDTH-1:0] afi_cas_n_low;
wire	[MEM_CLK_EN_WIDTH-1:0] afi_cke_high;
wire	[MEM_CLK_EN_WIDTH-1:0] afi_cke_low;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] afi_cs_n_high;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] afi_cs_n_low;
wire	[MEM_ODT_WIDTH-1:0] afi_odt_high;
wire	[MEM_ODT_WIDTH-1:0] afi_odt_low;
wire	[MEM_CONTROL_WIDTH-1:0] afi_ras_n_high;
wire	[MEM_CONTROL_WIDTH-1:0] afi_ras_n_low;
wire	[MEM_CONTROL_WIDTH-1:0] afi_we_n_high;
wire	[MEM_CONTROL_WIDTH-1:0] afi_we_n_low;

	// each signal has a high and a low portion,
	// connecting to the high and low inputs of the DDIO_OUT,
	// for the purpose of creating double data rate

    assign afi_address_low = afi_address[MEM_ADDRESS_WIDTH-1:0];
    assign afi_bank_low = afi_bank[MEM_BANK_WIDTH-1:0];
    assign afi_cke_low = afi_cke[MEM_CLK_EN_WIDTH-1:0];
    assign afi_odt_low = afi_odt[MEM_ODT_WIDTH-1:0];
    assign afi_cs_n_low = afi_cs_n[MEM_CHIP_SELECT_WIDTH-1:0];
    assign afi_we_n_low = afi_we_n[MEM_CONTROL_WIDTH-1:0];
    assign afi_ras_n_low = afi_ras_n[MEM_CONTROL_WIDTH-1:0];
    assign afi_cas_n_low = afi_cas_n[MEM_CONTROL_WIDTH-1:0];

    assign afi_address_high = afi_address[AFI_ADDRESS_WIDTH-1:MEM_ADDRESS_WIDTH];
    assign afi_bank_high = afi_bank[AFI_BANK_WIDTH-1:MEM_BANK_WIDTH];
    assign afi_cke_high = afi_cke[AFI_CLK_EN_WIDTH-1:MEM_CLK_EN_WIDTH];
    assign afi_odt_high = afi_odt[AFI_ODT_WIDTH-1:MEM_ODT_WIDTH];
    assign afi_cs_n_high = afi_cs_n[AFI_CHIP_SELECT_WIDTH-1:MEM_CHIP_SELECT_WIDTH];
    assign afi_we_n_high = afi_we_n[AFI_CONTROL_WIDTH-1:MEM_CONTROL_WIDTH];
    assign afi_ras_n_high = afi_ras_n[AFI_CONTROL_WIDTH-1:MEM_CONTROL_WIDTH];
    assign afi_cas_n_high = afi_cas_n[AFI_CONTROL_WIDTH-1:MEM_CONTROL_WIDTH];


wire	[MEM_ADDRESS_WIDTH-1:0]	afi_address_high_r;
wire	[MEM_ADDRESS_WIDTH-1:0]	afi_address_low_r;
wire	[MEM_BANK_WIDTH-1:0]    afi_bank_high_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_cas_n_high_r;
wire	[MEM_CLK_EN_WIDTH-1:0] afi_cke_high_r;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] afi_cs_n_high_r;
wire	[MEM_ODT_WIDTH-1:0] afi_odt_high_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_ras_n_high_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_we_n_high_r;

wire	[MEM_BANK_WIDTH-1:0]    afi_bank_low_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_cas_n_low_r;
wire	[MEM_CLK_EN_WIDTH-1:0] afi_cke_low_r;
wire	[MEM_CHIP_SELECT_WIDTH-1:0] afi_cs_n_low_r;
wire	[MEM_ODT_WIDTH-1:0] afi_odt_low_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_ras_n_low_r;
wire	[MEM_CONTROL_WIDTH-1:0] afi_we_n_low_r;

	assign afi_address_high_r = afi_address_high;
	assign afi_address_low_r = afi_address_low;
	assign afi_bank_high_r = afi_bank_high;
	assign afi_cas_n_high_r = afi_cas_n_high;
	assign afi_cke_high_r = afi_cke_high;
	assign afi_cs_n_high_r = afi_cs_n_high;
	assign afi_odt_high_r = afi_odt_high;
	assign afi_ras_n_high_r = afi_ras_n_high;
	assign afi_we_n_high_r = afi_we_n_high;

	assign afi_bank_low_r = afi_bank_low;
	assign afi_cas_n_low_r = afi_cas_n_low;
	assign afi_cke_low_r = afi_cke_low;
	assign afi_cs_n_low_r = afi_cs_n_low;
	assign afi_odt_low_r = afi_odt_low;
	assign afi_ras_n_low_r = afi_ras_n_low;
	assign afi_we_n_low_r = afi_we_n_low;
 


 
reg	[MEM_ADDRESS_WIDTH-1:0]	afi_address_high_rr;
reg	[MEM_BANK_WIDTH-1:0]    afi_bank_high_rr;
reg	[MEM_CONTROL_WIDTH-1:0] afi_cas_n_high_rr;
reg	[MEM_CLK_EN_WIDTH-1:0] afi_cke_high_rr;
reg	[MEM_CHIP_SELECT_WIDTH-1:0] afi_cs_n_high_rr;
reg	[MEM_ODT_WIDTH-1:0] afi_odt_high_rr;
reg	[MEM_CONTROL_WIDTH-1:0] afi_ras_n_high_rr;
reg	[MEM_CONTROL_WIDTH-1:0] afi_we_n_high_rr;

	always @(posedge clk or negedge reset_n)
		if (~reset_n) begin
			afi_address_high_rr <= {MEM_ADDRESS_WIDTH{1'b0}};
			afi_bank_high_rr <= {MEM_BANK_WIDTH{1'b0}};
			afi_cke_high_rr <= {MEM_CLK_EN_WIDTH{1'b0}};
			afi_cs_n_high_rr <= {MEM_CHIP_SELECT_WIDTH{1'b1}};
			afi_odt_high_rr <= {MEM_ODT_WIDTH{1'b0}};
			afi_ras_n_high_rr <= {MEM_CONTROL_WIDTH{1'b1}};
			afi_cas_n_high_rr <= {MEM_CONTROL_WIDTH{1'b1}};
			afi_we_n_high_rr <= {MEM_CONTROL_WIDTH{1'b1}};
		end
		else begin
			afi_address_high_rr <= afi_address_high_r;
			afi_bank_high_rr <= afi_bank_high_r;
			afi_cke_high_rr <= afi_cke_high_r;
			afi_cs_n_high_rr <= afi_cs_n_high_r;
			afi_odt_high_rr <= afi_odt_high_r;
			afi_ras_n_high_rr <= afi_ras_n_high_r;
			afi_cas_n_high_rr <= afi_cas_n_high_r;
			afi_we_n_high_rr <= afi_we_n_high_r;
    end


    assign phy_ddio_address_h = afi_address_low_r;
    assign phy_ddio_address_l = afi_address_high_rr;
	assign phy_ddio_bank_h = afi_bank_low_r;
	assign phy_ddio_cke_h = afi_cke_low_r;
	assign phy_ddio_cs_n_h = afi_cs_n_low_r;
	assign phy_ddio_odt_h = afi_odt_low_r;
	assign phy_ddio_ras_n_h = afi_ras_n_low_r;
	assign phy_ddio_cas_n_h = afi_cas_n_low_r;
	assign phy_ddio_we_n_h = afi_we_n_low_r;

	assign phy_ddio_bank_l = afi_bank_high_rr;
	assign phy_ddio_cke_l = afi_cke_high_rr;
	assign phy_ddio_cs_n_l = afi_cs_n_high_rr;
	assign phy_ddio_odt_l = afi_odt_high_rr;
	assign phy_ddio_ras_n_l = afi_ras_n_high_rr;
	assign phy_ddio_cas_n_l = afi_cas_n_high_rr;
	assign phy_ddio_we_n_l = afi_we_n_high_rr;

endmodule
