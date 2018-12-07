module rx30_io10 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [29:0] I_rx_cda_reset,
                  input [29:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,

                  input [29:0]   I_rx30_in,

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

                  output        O_rx_locked,
                  output        O_rx_outclock
                  );

   wire [299:0]                 rx_out;


   lvds_rx30 lvds_rx30_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx30_in),
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


endmodule
