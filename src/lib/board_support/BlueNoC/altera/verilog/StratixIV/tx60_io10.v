`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_tx60_io10 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/

module tx60_io10 (
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
                  input [9:0] i20_p,
                  input [9:0] i21_p,
                  input [9:0] i22_p,
                  input [9:0] i23_p,
                  input [9:0] i24_p,
                  input [9:0] i25_p,
                  input [9:0] i26_p,
                  input [9:0] i27_p,
                  input [9:0] i28_p,
                  input [9:0] i29_p,
                  input [9:0] i30_p,
                  input [9:0] i31_p,
                  input [9:0] i32_p,
                  input [9:0] i33_p,
                  input [9:0] i34_p,
                  input [9:0] i35_p,
                  input [9:0] i36_p,
                  input [9:0] i37_p,
                  input [9:0] i38_p,
                  input [9:0] i39_p,
                  input [9:0] i40_p,
                  input [9:0] i41_p,
                  input [9:0] i42_p,
                  input [9:0] i43_p,
                  input [9:0] i44_p,
                  input [9:0] i45_p,
                  input [9:0] i46_p,
                  input [9:0] i47_p,
                  input [9:0] i48_p,
                  input [9:0] i49_p,
                  input [9:0] i50_p,
                  input [9:0] i51_p,
                  input [9:0] i52_p,
                  input [9:0] i53_p,
                  input [9:0] i54_p,
                  input [9:0] i55_p,
                  input [9:0] i56_p,
                  input [9:0] i57_p,
                  input [9:0] i58_p,
                  input [9:0] i59_p,

                  output [59:0] O_tx60_out,
                  output        O_tx_locked,
                  output        O_tx_outclock,
                  output        O_tx_coreclock
                  );

   wire [599:0]                 tx_in;


`ifdef  BOARD_7406
 s4_lvds_tx60
`else
 s3_lvds_tx60
`endif
     lvds_tx60_inst(
	                      .pll_areset     (I_pll_areset),
	                      .tx_in          (tx_in),
	                      .tx_inclock     (I_tx_inclock),
	                      .tx_coreclock   (O_tx_coreclock),
                              .tx_locked      (O_tx_locked),
	                      .tx_out         (O_tx60_out),
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
   assign tx_in[209:200] =  i20_p;
   assign tx_in[219:210] =  i21_p;
   assign tx_in[229:220] =  i22_p;
   assign tx_in[239:230] =  i23_p;
   assign tx_in[249:240] =  i24_p;
   assign tx_in[259:250] =  i25_p;
   assign tx_in[269:260] =  i26_p;
   assign tx_in[279:270] =  i27_p;
   assign tx_in[289:280] =  i28_p;
   assign tx_in[299:290] =  i29_p;
   assign tx_in[309:300] =  i30_p;
   assign tx_in[319:310] =  i31_p;
   assign tx_in[329:320] =  i32_p;
   assign tx_in[339:330] =  i33_p;
   assign tx_in[349:340] =  i34_p;
   assign tx_in[359:350] =  i35_p;
   assign tx_in[369:360] =  i36_p;
   assign tx_in[379:370] =  i37_p;
   assign tx_in[389:380] =  i38_p;
   assign tx_in[399:390] =  i39_p;
   assign tx_in[409:400] =  i40_p;
   assign tx_in[419:410] =  i41_p;
   assign tx_in[429:420] =  i42_p;
   assign tx_in[439:430] =  i43_p;
   assign tx_in[449:440] =  i44_p;
   assign tx_in[459:450] =  i45_p;
   assign tx_in[469:460] =  i46_p;
   assign tx_in[479:470] =  i47_p;
   assign tx_in[489:480] =  i48_p;
   assign tx_in[499:490] =  i49_p;
   assign tx_in[509:500] =  i50_p;
   assign tx_in[519:510] =  i51_p;
   assign tx_in[529:520] =  i52_p;
   assign tx_in[539:530] =  i53_p;
   assign tx_in[549:540] =  i54_p;
   assign tx_in[559:550] =  i55_p;
   assign tx_in[569:560] =  i56_p;
   assign tx_in[579:570] =  i57_p;
   assign tx_in[589:580] =  i58_p;
   assign tx_in[599:590] =  i59_p;



endmodule
