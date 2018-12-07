`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_rx60_io40 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/

module rx60_io40 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [59:0] I_rx_cda_reset,
                  input [59:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,

                  input [59:0]   I_rx60_in,

                  output [39:0] o0_p,
                  output [39:0] o1_p,
                  output [39:0] o2_p,
                  output [39:0] o3_p,
                  output [39:0] o4_p,
                  output [39:0] o5_p,
                  output [39:0] o6_p,
                  output [39:0] o7_p,
                  output [39:0] o8_p,
                  output [39:0] o9_p,
                  output [39:0] o10_p,
                  output [39:0] o11_p,
                  output [39:0] o12_p,
                  output [39:0] o13_p,
                  output [39:0] o14_p,
                  output [39:0] o15_p,
                  output [39:0] o16_p,
                  output [39:0] o17_p,
                  output [39:0] o18_p,
                  output [39:0] o19_p,
                  output [39:0] o20_p,
                  output [39:0] o21_p,
                  output [39:0] o22_p,
                  output [39:0] o23_p,
                  output [39:0] o24_p,
                  output [39:0] o25_p,
                  output [39:0] o26_p,
                  output [39:0] o27_p,
                  output [39:0] o28_p,
                  output [39:0] o29_p,
                  output [39:0] o30_p,
                  output [39:0] o31_p,
                  output [39:0] o32_p,
                  output [39:0] o33_p,
                  output [39:0] o34_p,
                  output [39:0] o35_p,
                  output [39:0] o36_p,
                  output [39:0] o37_p,
                  output [39:0] o38_p,
                  output [39:0] o39_p,
                  output [39:0] o40_p,
                  output [39:0] o41_p,
                  output [39:0] o42_p,
                  output [39:0] o43_p,
                  output [39:0] o44_p,
                  output [39:0] o45_p,
                  output [39:0] o46_p,
                  output [39:0] o47_p,
                  output [39:0] o48_p,
                  output [39:0] o49_p,
                  output [39:0] o50_p,
                  output [39:0] o51_p,
                  output [39:0] o52_p,
                  output [39:0] o53_p,
                  output [39:0] o54_p,
                  output [39:0] o55_p,
                  output [39:0] o56_p,
                  output [39:0] o57_p,
                  output [39:0] o58_p,
                  output [39:0] o59_p,

                  output        O_rx_locked,
                  output        O_rx_data_locked,
                  output        O_rx_outclock
                  );


wire 				rx_outclock;
wire [599:0]                    rx_out;
//
wire [1:0] 			pattern_mux_sel;
wire 				rx_pattern_err;
wire 				rx_pattern_found;

      demux_10_40 demux_10_40_inst0 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[9:0]),
				  .o_p       (o0_p)
				  );

      demux_10_40 demux_10_40_inst1 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[19:10]),
				  .o_p       (o1_p)
				  );

      demux_10_40 demux_10_40_inst2 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[29:20]),
				  .o_p       (o2_p)
				  );

      demux_10_40 demux_10_40_inst3 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[39:30]),
				  .o_p       (o3_p)
				  );

      demux_10_40 demux_10_40_inst4 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[49:40]),
				  .o_p       (o4_p)
				  );

      demux_10_40 demux_10_40_inst5 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[59:50]),
				  .o_p       (o5_p)
				  );

      demux_10_40 demux_10_40_inst6 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[69:60]),
				  .o_p       (o6_p)
				  );

      demux_10_40 demux_10_40_inst7 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[79:70]),
				  .o_p       (o7_p)
				  );

      demux_10_40 demux_10_40_inst8 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[89:80]),
				  .o_p       (o8_p)
				  );

      demux_10_40 demux_10_40_inst9 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[99:90]),
				  .o_p       (o9_p)
				  );
      demux_10_40 demux_10_40_inst10 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[109:100]),
				  .o_p       (o10_p)
				  );

      demux_10_40 demux_10_40_inst11 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[119:110]),
				  .o_p       (o11_p)
				  );

      demux_10_40 demux_10_40_inst12 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[129:120]),
				  .o_p       (o12_p)
				  );

      demux_10_40 demux_10_40_inst13 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[139:130]),
				  .o_p       (o13_p)
				  );

      demux_10_40 demux_10_40_inst14 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[149:140]),
				  .o_p       (o14_p)
				  );

      demux_10_40 demux_10_40_inst15 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[159:150]),
				  .o_p       (o15_p)
				  );

      demux_10_40 demux_10_40_inst16 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[169:160]),
				  .o_p       (o16_p)
				  );

      demux_10_40 demux_10_40_inst17 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[179:170]),
				  .o_p       (o17_p)
				  );

      demux_10_40 demux_10_40_inst18 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[189:180]),
				  .o_p       (o18_p)
				  );

      demux_10_40 demux_10_40_inst19 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[199:190]),
				  .o_p       (o19_p)
				  );
      demux_10_40 demux_10_40_inst20 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[209:200]),
				  .o_p       (o20_p)
				  );

      demux_10_40 demux_10_40_inst21 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[219:210]),
				  .o_p       (o21_p)
				  );

      demux_10_40 demux_10_40_inst22 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[229:220]),
				  .o_p       (o22_p)
				  );

      demux_10_40 demux_10_40_inst23 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[239:230]),
				  .o_p       (o23_p)
				  );

      demux_10_40 demux_10_40_inst24 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[249:240]),
				  .o_p       (o24_p)
				  );

      demux_10_40 demux_10_40_inst25 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[259:250]),
				  .o_p       (o25_p)
				  );

      demux_10_40 demux_10_40_inst26 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[269:260]),
				  .o_p       (o26_p)
				  );

      demux_10_40 demux_10_40_inst27 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[279:270]),
				  .o_p       (o27_p)
				  );

      demux_10_40 demux_10_40_inst28 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[289:280]),
				  .o_p       (o28_p)
				  );

      demux_10_40 demux_10_40_inst29 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[299:290]),
				  .o_p       (o29_p)
				  );
      demux_10_40 demux_10_40_inst30 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[309:300]),
				  .o_p       (o30_p)
				  );

      demux_10_40 demux_10_40_inst31 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[319:310]),
				  .o_p       (o31_p)
				  );

      demux_10_40 demux_10_40_inst32 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[329:320]),
				  .o_p       (o32_p)
				  );

      demux_10_40 demux_10_40_inst33 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[339:330]),
				  .o_p       (o33_p)
				  );

      demux_10_40 demux_10_40_inst34 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[349:340]),
				  .o_p       (o34_p)
				  );

      demux_10_40 demux_10_40_inst35 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[359:350]),
				  .o_p       (o35_p)
				  );

      demux_10_40 demux_10_40_inst36 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[369:360]),
				  .o_p       (o36_p)
				  );

      demux_10_40 demux_10_40_inst37 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[379:370]),
				  .o_p       (o37_p)
				  );

      demux_10_40 demux_10_40_inst38 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[389:380]),
				  .o_p       (o38_p)
				  );

      demux_10_40 demux_10_40_inst39 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[399:390]),
				  .o_p       (o39_p)
				  );
      demux_10_40 demux_10_40_inst40 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[409:400]),
				  .o_p       (o40_p)
				  );

      demux_10_40 demux_10_40_inst41 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[419:410]),
				  .o_p       (o41_p)
				  );

      demux_10_40 demux_10_40_inst42 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[429:420]),
				  .o_p       (o42_p)
				  );

      demux_10_40 demux_10_40_inst43 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[439:430]),
				  .o_p       (o43_p)
				  );

      demux_10_40 demux_10_40_inst44 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[449:440]),
				  .o_p       (o44_p)
				  );

      demux_10_40 demux_10_40_inst45 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[459:450]),
				  .o_p       (o45_p)
				  );

      demux_10_40 demux_10_40_inst46 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[469:460]),
				  .o_p       (o46_p)
				  );

      demux_10_40 demux_10_40_inst47 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[479:470]),
				  .o_p       (o47_p)
				  );

      demux_10_40 demux_10_40_inst48 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[489:480]),
				  .o_p       (o48_p)
				  );

      demux_10_40 demux_10_40_inst49 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[499:490]),
				  .o_p       (o49_p)
				  );
      demux_10_40 demux_10_40_inst50 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[509:500]),
				  .o_p       (o50_p)
				  );

      demux_10_40 demux_10_40_inst51 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[519:510]),
				  .o_p       (o51_p)
				  );

      demux_10_40 demux_10_40_inst52 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[529:520]),
				  .o_p       (o52_p)
				  );

      demux_10_40 demux_10_40_inst53 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[539:530]),
				  .o_p       (o53_p)
				  );

      demux_10_40 demux_10_40_inst54 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[549:540]),
				  .o_p       (o54_p)
				  );

      demux_10_40 demux_10_40_inst55 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[559:550]),
				  .o_p       (o55_p)
				  );

      demux_10_40 demux_10_40_inst56 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[569:560]),
				  .o_p       (o56_p)
				  );

      demux_10_40 demux_10_40_inst57 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[579:570]),
				  .o_p       (o57_p)
				  );

      demux_10_40 demux_10_40_inst58 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[589:580]),
				  .o_p       (o58_p)
				  );

      demux_10_40 demux_10_40_inst59 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
                                  .pattern_mux_sel (pattern_mux_sel),
				  .sel       (I_sel),
				  .i_p       (rx_out[599:590]),
				  .o_p       (o59_p)
				  );

//////
init_sm init_sm_int (
                                 .clk               (rx_outclock),
        			 .rst_n             (I_rst_n),
                                 .cda_rdy           (I_rx_cda_rdy),
				 .i_p               (o0_p),
                                 .O_pattern_mux_sel (pattern_mux_sel),
                                 .O_pattern_found   (rx_pattern_found),
                                 .O_pattern_err     (rx_pattern_err)
                     );



/////



`ifdef BOARD_7406
   s4_lvds_rx60
`else
   s3_lvds_rx60
`endif			       
     lvds_rx60_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx60_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (O_rx_locked),
	                      .rx_outclock            (rx_outclock)
                              );

assign O_rx_outclock = rx_outclock;
assign O_rx_data_locked = rx_pattern_found;


endmodule
