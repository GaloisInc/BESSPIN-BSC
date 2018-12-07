module d7002_rx60_io10 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [59:0] I_rx_cda_reset,
                  input [59:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,

                  input [59:0]   I_rx60_in,

                  output [9:0] o0_p,
                  output [9:0] o1_p,
                  output [9:0] o2_p,
                  output [9:0] o3_p,
                  output [9:0] o4_p,
                  output [9:0] o5_p,
                  output [9:0] o6_p,
                  output [9:0] o7_p,
                  output [9:0] o8_p,
                  output [9:0] o9_p,
                  output [9:0] o10_p,
                  output [9:0] o11_p,
                  output [9:0] o12_p,
                  output [9:0] o13_p,
                  output [9:0] o14_p,
                  output [9:0] o15_p,
                  output [9:0] o16_p,
                  output [9:0] o17_p,
                  output [9:0] o18_p,
                  output [9:0] o19_p,
                  output [9:0] o20_p,
                  output [9:0] o21_p,
                  output [9:0] o22_p,
                  output [9:0] o23_p,
                  output [9:0] o24_p,
                  output [9:0] o25_p,
                  output [9:0] o26_p,
                  output [9:0] o27_p,
                  output [9:0] o28_p,
                  output [9:0] o29_p,
                  output [9:0] o30_p,
                  output [9:0] o31_p,
                  output [9:0] o32_p,
                  output [9:0] o33_p,
                  output [9:0] o34_p,
                  output [9:0] o35_p,
                  output [9:0] o36_p,
                  output [9:0] o37_p,
                  output [9:0] o38_p,
                  output [9:0] o39_p,
                  output [9:0] o40_p,
                  output [9:0] o41_p,
                  output [9:0] o42_p,
                  output [9:0] o43_p,
                  output [9:0] o44_p,
                  output [9:0] o45_p,
                  output [9:0] o46_p,
                  output [9:0] o47_p,
                  output [9:0] o48_p,
                  output [9:0] o49_p,
                  output [9:0] o50_p,
                  output [9:0] o51_p,
                  output [9:0] o52_p,
                  output [9:0] o53_p,
                  output [9:0] o54_p,
                  output [9:0] o55_p,
                  output [9:0] o56_p,
                  output [9:0] o57_p,
                  output [9:0] o58_p,
                  output [9:0] o59_p,

                  output        O_rx_locked,
                  output        O_rx_outclock
                  );

   wire [599:0]                 rx_out;


   s3_lvds_rx60 lvds_rx60_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx60_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (O_rx_locked),
	                      .rx_outclock            (O_rx_outclock)
                              );

   assign     o0_p  = rx_out[9:0];
   assign     o1_p  = rx_out[19:10];
   assign     o2_p  = rx_out[29:20];
   assign     o3_p  = rx_out[39:30];
   assign     o4_p  = rx_out[49:40];
   assign     o5_p  = rx_out[59:50];
   assign     o6_p  = rx_out[69:60];
   assign     o7_p  = rx_out[79:70];
   assign     o8_p  = rx_out[89:80];
   assign     o9_p  = rx_out[99:90];
   assign     o10_p = rx_out[109:100];
   assign     o11_p = rx_out[119:110];
   assign     o12_p = rx_out[129:120];
   assign     o13_p = rx_out[139:130];
   assign     o14_p = rx_out[149:140];
   assign     o15_p = rx_out[159:150];
   assign     o16_p = rx_out[169:160];
   assign     o17_p = rx_out[179:170];
   assign     o18_p = rx_out[189:180];
   assign     o19_p = rx_out[199:190];
   assign     o20_p = rx_out[209:200];
   assign     o21_p = rx_out[219:210];
   assign     o22_p = rx_out[229:220];
   assign     o23_p = rx_out[239:230];
   assign     o24_p = rx_out[249:240];
   assign     o25_p = rx_out[259:250];
   assign     o26_p = rx_out[269:260];
   assign     o27_p = rx_out[279:270];
   assign     o28_p = rx_out[289:280];
   assign     o29_p = rx_out[299:290];
   assign     o30_p = rx_out[309:300];
   assign     o31_p = rx_out[319:310];
   assign     o32_p = rx_out[329:320];
   assign     o33_p = rx_out[339:330];
   assign     o34_p = rx_out[349:340];
   assign     o35_p = rx_out[359:350];
   assign     o36_p = rx_out[369:360];
   assign     o37_p = rx_out[379:370];
   assign     o38_p = rx_out[389:380];
   assign     o39_p = rx_out[399:390];
   assign     o40_p = rx_out[409:400];
   assign     o41_p = rx_out[419:410];
   assign     o42_p = rx_out[429:420];
   assign     o43_p = rx_out[439:430];
   assign     o44_p = rx_out[449:440];
   assign     o45_p = rx_out[459:450];
   assign     o46_p = rx_out[469:460];
   assign     o47_p = rx_out[479:470];
   assign     o48_p = rx_out[489:480];
   assign     o49_p = rx_out[499:490];
   assign     o50_p = rx_out[509:500];
   assign     o51_p = rx_out[519:510];
   assign     o52_p = rx_out[529:520];
   assign     o53_p = rx_out[539:530];
   assign     o54_p = rx_out[549:540];
   assign     o55_p = rx_out[559:550];
   assign     o56_p = rx_out[569:560];
   assign     o57_p = rx_out[579:570];
   assign     o58_p = rx_out[589:580];
   assign     o59_p = rx_out[599:590];


endmodule
