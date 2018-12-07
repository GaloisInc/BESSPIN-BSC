`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_tx20_io10 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/


module tx20_io10 (
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
                              .tx_locked      (O_tx_locked),
	                      .tx_out         (O_tx20_out),
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
   assign tx_in[99:90] = i9_p;
   assign tx_in[109:100] =  i10_p;
   assign tx_in[119:110] =  i11_p;
   assign tx_in[129:120] =  i12_p;
   assign tx_in[139:130] =  i13_p;
   assign tx_in[149:140] =  i14_p;
   assign tx_in[159:150] =  i15_p;
   assign tx_in[169:160] =  i16_p;
   assign tx_in[179:170] =  i17_p;
   assign tx_in[189:180] =  i18_p;
   assign tx_in[199:190] =  i19_p;

endmodule
