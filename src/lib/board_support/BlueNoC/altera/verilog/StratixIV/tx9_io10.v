`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_tx9_io10 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/


module tx9_io10 (
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
                  output [8:0] O_tx9_out,
                  output        O_tx_locked,
                  output        O_tx_outclock,
                  output        O_tx_coreclock
                  );

   wire [89:0]                 tx_in;


`ifdef  BOARD_7406
 s4_lvds_tx9
`else
 s3_lvds_tx9
`endif

 lvds_tx9_inst(
	                      .pll_areset     (I_pll_areset),
	                      .tx_in          (tx_in),
	                      .tx_inclock     (I_tx_inclock),
	                      .tx_coreclock   (O_tx_coreclock),
                              .tx_locked      (O_tx_locked),
	                      .tx_out         (O_tx9_out),
                              .tx_outclock    (O_tx_outclock)
                              );
   assign tx_in[9:0] = i0_p;
   assign tx_in[19:10] = i1_p;
   assign tx_in[29:20] = i2_p;
   assign tx_in[39:30] = i3_p;
   assign tx_in[49:40] = i4_p;
   assign tx_in[59:50] = i5_p;
   assign tx_in[69:60] = i6_p;
   assign tx_in[79:70] = i7_p;
   assign tx_in[89:80] = i8_p;

endmodule
