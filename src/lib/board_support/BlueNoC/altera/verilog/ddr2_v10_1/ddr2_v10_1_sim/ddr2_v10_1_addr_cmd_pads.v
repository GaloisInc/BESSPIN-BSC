// Copyright 2009 Altera Corporation. All rights reserved.  
// Your use of Altera Corporation's design tools, logic functions and other software and tools, and its AMPP partner logic 
// functions, and any output files any of the foregoing (including device programming or simulation files), and any associated 
// documentation or information are expressly subject to the terms and conditions of the Altera Program License Subscription 
// Agreement, Altera MegaCore Function License Agreement, or other applicable license agreement, including, without limitation, 
// that your use is for the sole purpose of programming logic devices manufactured by Altera and sold by Altera or its authorized 
// distributors.  Please refer to the applicable agreement for further details.

module ddr2_v10_1_addr_cmd_pads(
    reset_n,
    reset_n_mem_clk,
	pll_mem_clk,
    phy_ddio_addr_cmd_clk,
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
    phy_ddio_cas_n_h,
    phy_mem_address,
    phy_mem_bank,
    phy_mem_cs_n,
    phy_mem_cke,
    phy_mem_odt,
    phy_mem_we_n,
    phy_mem_ras_n,
    phy_mem_cas_n,
	phy_mem_ck,
	phy_mem_ck_n
 
);

parameter DEVICE_FAMILY = "";
parameter MEM_ADDRESS_WIDTH     = ""; 
parameter MEM_BANK_WIDTH        = ""; 
parameter MEM_CHIP_SELECT_WIDTH = ""; 
parameter MEM_CLK_EN_WIDTH 		= ""; 
parameter MEM_CK_WIDTH 			= ""; 
parameter MEM_ODT_WIDTH 		= ""; 
parameter MEM_CONTROL_WIDTH     = ""; 


input	reset_n;
input	reset_n_mem_clk;
input	pll_mem_clk;
input	phy_ddio_addr_cmd_clk;
input	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_l;
input	[MEM_ADDRESS_WIDTH-1:0]	phy_ddio_address_h;



input   [MEM_BANK_WIDTH-1:0]    phy_ddio_bank_l;
input   [MEM_BANK_WIDTH-1:0]    phy_ddio_bank_h;
input   [MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_l;
input   [MEM_CHIP_SELECT_WIDTH-1:0] phy_ddio_cs_n_h;
input   [MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_l;
input   [MEM_CLK_EN_WIDTH-1:0] phy_ddio_cke_h;
input   [MEM_ODT_WIDTH-1:0] phy_ddio_odt_l;
input   [MEM_ODT_WIDTH-1:0] phy_ddio_odt_h;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_l;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_ras_n_h;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_l;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_cas_n_h;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_l;
input   [MEM_CONTROL_WIDTH-1:0] phy_ddio_we_n_h;

output  [MEM_ADDRESS_WIDTH-1:0] phy_mem_address;
output  [MEM_BANK_WIDTH-1:0]    phy_mem_bank;
output  [MEM_CHIP_SELECT_WIDTH-1:0] phy_mem_cs_n;
output  [MEM_CLK_EN_WIDTH-1:0] phy_mem_cke;
output  [MEM_ODT_WIDTH-1:0] phy_mem_odt;
output  [MEM_CONTROL_WIDTH-1:0] phy_mem_we_n;
output  [MEM_CONTROL_WIDTH-1:0] phy_mem_ras_n;
output  [MEM_CONTROL_WIDTH-1:0] phy_mem_cas_n;
output  [MEM_CK_WIDTH-1:0]	phy_mem_ck;
output  [MEM_CK_WIDTH-1:0]	phy_mem_ck_n;


    altddio_out	uaddress_pad(
		.aclr	    (~reset_n),
		.aset	    (1'b0),
		.datain_h   (phy_ddio_address_l),
		.datain_l   (phy_ddio_address_h),
		.dataout    (phy_mem_address),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		uaddress_pad.extend_oe_disable = "UNUSED",
		uaddress_pad.intended_device_family = DEVICE_FAMILY,
		uaddress_pad.invert_output = "OFF",
		uaddress_pad.lpm_hint = "UNUSED",
		uaddress_pad.lpm_type = "altddio_out",
		uaddress_pad.oe_reg = "UNUSED",
		uaddress_pad.power_up_high = "OFF",
		uaddress_pad.width = MEM_ADDRESS_WIDTH;






    altddio_out	ubank_pad(
		.aclr	    (~reset_n),
		.aset	    (1'b0),
		.datain_h   (phy_ddio_bank_l),
		.datain_l   (phy_ddio_bank_h),
		.dataout    (phy_mem_bank),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		ubank_pad.extend_oe_disable = "UNUSED",
		ubank_pad.intended_device_family = DEVICE_FAMILY,
		ubank_pad.invert_output = "OFF",
		ubank_pad.lpm_hint = "UNUSED",
		ubank_pad.lpm_type = "altddio_out",
		ubank_pad.oe_reg = "UNUSED",
		ubank_pad.power_up_high = "OFF",
		ubank_pad.width = MEM_BANK_WIDTH;



    altddio_out	ucs_n_pad(
		.aclr	    (1'b0),
		.aset	    (~reset_n),
		.datain_h   (phy_ddio_cs_n_l),
		.datain_l   (phy_ddio_cs_n_h),
		.dataout    (phy_mem_cs_n),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		ucs_n_pad.extend_oe_disable = "UNUSED",
		ucs_n_pad.intended_device_family = DEVICE_FAMILY,
		ucs_n_pad.invert_output = "OFF",
		ucs_n_pad.lpm_hint = "UNUSED",
		ucs_n_pad.lpm_type = "altddio_out",
		ucs_n_pad.oe_reg = "UNUSED",
		ucs_n_pad.power_up_high = "OFF",
		ucs_n_pad.width = MEM_CHIP_SELECT_WIDTH;


    altddio_out ucke_pad(
        .aclr       (~reset_n),
        .aset       (1'b0),
        .datain_h   (phy_ddio_cke_l),
        .datain_l   (phy_ddio_cke_h),
        .dataout    (phy_mem_cke),
        .oe         (1'b1),
        .outclock   (phy_ddio_addr_cmd_clk),
        .outclocken (1'b1)
    );

    defparam
        ucke_pad.extend_oe_disable = "UNUSED",
        ucke_pad.intended_device_family = DEVICE_FAMILY,
        ucke_pad.invert_output = "OFF",
        ucke_pad.lpm_hint = "UNUSED",
        ucke_pad.lpm_type = "altddio_out",
        ucke_pad.oe_reg = "UNUSED",
        ucke_pad.power_up_high = "OFF",
        ucke_pad.width = MEM_CLK_EN_WIDTH;


    altddio_out uodt_pad(
        .aclr       (~reset_n),
        .aset       (1'b0),
        .datain_h   (phy_ddio_odt_l),
        .datain_l   (phy_ddio_odt_h),
        .dataout    (phy_mem_odt),
        .oe         (1'b1),
        .outclock   (phy_ddio_addr_cmd_clk),
        .outclocken (1'b1)
    );

    defparam
        uodt_pad.extend_oe_disable = "UNUSED",
        uodt_pad.intended_device_family = DEVICE_FAMILY,
        uodt_pad.invert_output = "OFF",
        uodt_pad.lpm_hint = "UNUSED",
        uodt_pad.lpm_type = "altddio_out",
        uodt_pad.oe_reg = "UNUSED",
        uodt_pad.power_up_high = "OFF",
        uodt_pad.width = MEM_ODT_WIDTH;


    altddio_out	uwe_n_pad(
		.aclr	    (1'b0),
		.aset	    (~reset_n),
		.datain_h   (phy_ddio_we_n_l),
		.datain_l   (phy_ddio_we_n_h),
		.dataout    (phy_mem_we_n),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		uwe_n_pad.extend_oe_disable = "UNUSED",
		uwe_n_pad.intended_device_family = DEVICE_FAMILY,
		uwe_n_pad.invert_output = "OFF",
		uwe_n_pad.lpm_hint = "UNUSED",
		uwe_n_pad.lpm_type = "altddio_out",
		uwe_n_pad.oe_reg = "UNUSED",
		uwe_n_pad.power_up_high = "OFF",
		uwe_n_pad.width = MEM_CONTROL_WIDTH;


    altddio_out	uras_n_pad(
		.aclr	    (1'b0),
		.aset	    (~reset_n),
		.datain_h   (phy_ddio_ras_n_l),
		.datain_l   (phy_ddio_ras_n_h),
		.dataout    (phy_mem_ras_n),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		uras_n_pad.extend_oe_disable = "UNUSED",
		uras_n_pad.intended_device_family = DEVICE_FAMILY,
		uras_n_pad.invert_output = "OFF",
		uras_n_pad.lpm_hint = "UNUSED",
		uras_n_pad.lpm_type = "altddio_out",
		uras_n_pad.oe_reg = "UNUSED",
		uras_n_pad.power_up_high = "OFF",
		uras_n_pad.width = MEM_CONTROL_WIDTH;



    altddio_out	ucas_n_pad(
		.aclr	    (1'b0),
		.aset	    (~reset_n),
		.datain_h   (phy_ddio_cas_n_l),
		.datain_l   (phy_ddio_cas_n_h),
		.dataout    (phy_mem_cas_n),
		.oe	    	(1'b1),
		.outclock   (phy_ddio_addr_cmd_clk),
		.outclocken (1'b1)
    );

    defparam 
		ucas_n_pad.extend_oe_disable = "UNUSED",
		ucas_n_pad.intended_device_family = DEVICE_FAMILY,
		ucas_n_pad.invert_output = "OFF",
		ucas_n_pad.lpm_hint = "UNUSED",
		ucas_n_pad.lpm_type = "altddio_out",
		ucas_n_pad.oe_reg = "UNUSED",
		ucas_n_pad.power_up_high = "OFF",
		ucas_n_pad.width = MEM_CONTROL_WIDTH;



wire	[MEM_CK_WIDTH-1:0] mem_ck;
generate
genvar clock_width;
    for (clock_width=0; clock_width<MEM_CK_WIDTH; clock_width=clock_width+1)
    begin: clock_gen

    altddio_out umem_ck_pad(
    	.aclr       (1'b0),
    	.aset       (1'b0),
    	.datain_h   (1'b1),
    	.datain_l   (1'b0),
    	.dataout    (mem_ck[clock_width]),
    	.oe     	(1'b1),
    	.outclock   (pll_mem_clk),
    	.outclocken (1'b1)
    );

    defparam
    	umem_ck_pad.extend_oe_disable = "UNUSED",
    	umem_ck_pad.intended_device_family = DEVICE_FAMILY,
    	umem_ck_pad.invert_output = "OFF",
    	umem_ck_pad.lpm_hint = "UNUSED",
    	umem_ck_pad.lpm_type = "altddio_out",
    	umem_ck_pad.oe_reg = "UNUSED",
    	umem_ck_pad.power_up_high = "OFF",
    	umem_ck_pad.width = 1;


    ddr2_v10_1_clock_pair_generator    uclk_generator(
        .datain     (mem_ck[clock_width]),
        .dataout    (phy_mem_ck[clock_width]),
        .dataout_b  (phy_mem_ck_n[clock_width])
    );
	end
endgenerate


endmodule
