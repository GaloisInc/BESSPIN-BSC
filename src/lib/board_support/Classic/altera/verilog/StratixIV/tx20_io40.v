`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_tx20_io40 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/

module tx20_io40 (
                  input        I_clk, // NOT USED
                  input        I_rst_n, // NOT USED
                  input [1:0]  I_sel,   // NOT USED
                  input        I_pll_areset,
                  input        I_tx_inclock,

                  input [9:0] i0_p,
                  input [9:0] i1_p,
                  input [9:0] i2_p,
                  input [9:0] i3_p,
                  input [9:0] i4_p,
                  input [9:0] i5_p,
                  input [9:0] i6_p,
                  input [9:0] i7_p,
                  input [9:0] i8_p,
                  input [9:0] i9_p,
                  input [9:0] i10_p,
                  input [9:0] i11_p,
                  input [9:0] i12_p,
                  input [9:0] i13_p,
                  input [9:0] i14_p,
                  input [9:0] i15_p,
                  input [9:0] i16_p,
                  input [9:0] i17_p,
                  input [9:0] i18_p,
                  input [9:0] i19_p,
                  output [19:0] O_tx20_out,
                  output        O_tx_locked,
                  output        O_tx_outclock,
                  output        O_tx_coreclock
                  );

   wire [199:0]                 tx_in;

wire                            tx_locked;

generate 
   begin : L0
   genvar i;
   for (i=o; i<=19; i = i+1)
    begin u
        reg_mux_40_10 reg_mux_40_10_inst[i] (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i[i]_p),
				      .o_p       (tx_in[(9+(i*10)):(10*i)]),
				      );
    end // for (i=o; i<=59; i = i+1)
   end // block: L0
endgenerate



`ifdef  BOARD_7406
 s4_lvds_rx60
`else
 s3_lvds_rx60
`endif
   lvds_tx20_inst(
	                      .pll_areset     (I_pll_areset),
	                      .tx_in          (tx_in),
	                      .tx_inclock     (I_tx_inclock),
	                      .tx_coreclock   (O_tx_coreclock),
                              .tx_locked      (tx_locked),
	                      .tx_out         (O_tx20_out),
                              .tx_outclock    (O_tx_outclock)
                              );

assign O_tx_locked = tx_locked;

endmodule
