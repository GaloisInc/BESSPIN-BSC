module d7002_tx60_io40 (
                  input        I_clk, // NOT USED
                  input        I_rst_n, // NOT USED
                  input [1:0]  I_sel,   // NOT USED
                  input        I_pll_areset,
                  input        I_tx_inclock,

                  input [39:0] i0_p,
                  input [39:0] i1_p,
                  input [39:0] i2_p,
                  input [39:0] i3_p,
                  input [39:0] i4_p,
                  input [39:0] i5_p,
                  input [39:0] i6_p,
                  input [39:0] i7_p,
                  input [39:0] i8_p,
                  input [39:0] i9_p,
                  input [39:0] i10_p,
                  input [39:0] i11_p,
                  input [39:0] i12_p,
                  input [39:0] i13_p,
                  input [39:0] i14_p,
                  input [39:0] i15_p,
                  input [39:0] i16_p,
                  input [39:0] i17_p,
                  input [39:0] i18_p,
                  input [39:0] i19_p,
                  input [39:0] i20_p,
                  input [39:0] i21_p,
                  input [39:0] i22_p,
                  input [39:0] i23_p,
                  input [39:0] i24_p,
                  input [39:0] i25_p,
                  input [39:0] i26_p,
                  input [39:0] i27_p,
                  input [39:0] i28_p,
                  input [39:0] i29_p,
                  input [39:0] i30_p,
                  input [39:0] i31_p,
                  input [39:0] i32_p,
                  input [39:0] i33_p,
                  input [39:0] i34_p,
                  input [39:0] i35_p,
                  input [39:0] i36_p,
                  input [39:0] i37_p,
                  input [39:0] i38_p,
                  input [39:0] i39_p,
                  input [39:0] i40_p,
                  input [39:0] i41_p,
                  input [39:0] i42_p,
                  input [39:0] i43_p,
                  input [39:0] i44_p,
                  input [39:0] i45_p,
                  input [39:0] i46_p,
                  input [39:0] i47_p,
                  input [39:0] i48_p,
                  input [39:0] i49_p,
                  input [39:0] i50_p,
                  input [39:0] i51_p,
                  input [39:0] i52_p,
                  input [39:0] i53_p,
                  input [39:0] i54_p,
                  input [39:0] i55_p,
                  input [39:0] i56_p,
                  input [39:0] i57_p,
                  input [39:0] i58_p,
                  input [39:0] i59_p,

                  output [59:0] O_tx60_out,
                  output        O_tx_locked,
                  output        O_tx_outclock,
                  output        O_tx_coreclock
                  );

   wire [599:0]                 tx_in;

wire                            tx_locked;


        reg_mux_40_10 reg_mux_40_10_inst0 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i0_p),
				      .o_p       (tx_in[9:0])
				      );

        reg_mux_40_10 reg_mux_40_10_inst1 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i1_p),
				      .o_p       (tx_in[19:10])
				      );

        reg_mux_40_10 reg_mux_40_10_inst2 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i2_p),
				      .o_p       (tx_in[29:20])
				      );

        reg_mux_40_10 reg_mux_40_10_inst3 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i3_p),
				      .o_p       (tx_in[39:30])
				      );

        reg_mux_40_10 reg_mux_40_10_inst4 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i4_p),
				      .o_p       (tx_in[49:40])
				      );

        reg_mux_40_10 reg_mux_40_10_inst5 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i5_p),
				      .o_p       (tx_in[59:50])
				      );

        reg_mux_40_10 reg_mux_40_10_inst6 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i6_p),
				      .o_p       (tx_in[69:60])
				      );

        reg_mux_40_10 reg_mux_40_10_inst7 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i7_p),
				      .o_p       (tx_in[79:70])
				      );

        reg_mux_40_10 reg_mux_40_10_inst8 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i8_p),
				      .o_p       (tx_in[89:80])
				      );

        reg_mux_40_10 reg_mux_40_10_inst9 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i9_p),
				      .o_p       (tx_in[99:90])
				      );

        reg_mux_40_10 reg_mux_40_10_inst10 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i10_p),
				      .o_p       (tx_in[109:100])
				      );

        reg_mux_40_10 reg_mux_40_10_inst11 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i11_p),
				      .o_p       (tx_in[119:110])
				      );

        reg_mux_40_10 reg_mux_40_10_inst12 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i12_p),
				      .o_p       (tx_in[129:120])
				      );

        reg_mux_40_10 reg_mux_40_10_inst13 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i13_p),
				      .o_p       (tx_in[139:130])
				      );

        reg_mux_40_10 reg_mux_40_10_inst14 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i14_p),
				      .o_p       (tx_in[149:140])
				      );

        reg_mux_40_10 reg_mux_40_10_inst15 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i15_p),
				      .o_p       (tx_in[159:150])
				      );

        reg_mux_40_10 reg_mux_40_10_inst16 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i16_p),
				      .o_p       (tx_in[169:160])
				      );

        reg_mux_40_10 reg_mux_40_10_inst17 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i17_p),
				      .o_p       (tx_in[179:170])
				      );

        reg_mux_40_10 reg_mux_40_10_inst18 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i18_p),
				      .o_p       (tx_in[189:180])
				      );

        reg_mux_40_10 reg_mux_40_10_inst19 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i19_p),
				      .o_p       (tx_in[199:190])
				      );

        reg_mux_40_10 reg_mux_40_10_inst20 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i20_p),
				      .o_p       (tx_in[209:200])
				      );

        reg_mux_40_10 reg_mux_40_10_inst21 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i21_p),
				      .o_p       (tx_in[219:210])
				      );

        reg_mux_40_10 reg_mux_40_10_inst22 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i22_p),
				      .o_p       (tx_in[229:220])
				      );

        reg_mux_40_10 reg_mux_40_10_inst23 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i23_p),
				      .o_p       (tx_in[239:230])
				      );

        reg_mux_40_10 reg_mux_40_10_inst24 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i24_p),
				      .o_p       (tx_in[249:240])
				      );

        reg_mux_40_10 reg_mux_40_10_inst25 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i25_p),
				      .o_p       (tx_in[259:250])
				      );

        reg_mux_40_10 reg_mux_40_10_inst26 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i26_p),
				      .o_p       (tx_in[269:260])
				      );

        reg_mux_40_10 reg_mux_40_10_inst27 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i27_p),
				      .o_p       (tx_in[279:270])
				      );

        reg_mux_40_10 reg_mux_40_10_inst28 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i28_p),
				      .o_p       (tx_in[289:280])
				      );

        reg_mux_40_10 reg_mux_40_10_inst29 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i29_p),
				      .o_p       (tx_in[299:290])
				      );

        reg_mux_40_10 reg_mux_40_10_inst30 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i30_p),
				      .o_p       (tx_in[309:300])
				      );

        reg_mux_40_10 reg_mux_40_10_inst31 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i31_p),
				      .o_p       (tx_in[319:310])
				      );

        reg_mux_40_10 reg_mux_40_10_inst32 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i32_p),
				      .o_p       (tx_in[329:320])
				      );

        reg_mux_40_10 reg_mux_40_10_inst33 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i33_p),
				      .o_p       (tx_in[339:330])
				      );

        reg_mux_40_10 reg_mux_40_10_inst34 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i34_p),
				      .o_p       (tx_in[349:340])
				      );

        reg_mux_40_10 reg_mux_40_10_inst35 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i35_p),
				      .o_p       (tx_in[359:350])
				      );

        reg_mux_40_10 reg_mux_40_10_inst36 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i36_p),
				      .o_p       (tx_in[369:360])
				      );

        reg_mux_40_10 reg_mux_40_10_inst37 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i37_p),
				      .o_p       (tx_in[379:370])
				      );

        reg_mux_40_10 reg_mux_40_10_inst38 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i38_p),
				      .o_p       (tx_in[389:380])
				      );

        reg_mux_40_10 reg_mux_40_10_inst39 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i39_p),
				      .o_p       (tx_in[399:390])
				      );

        reg_mux_40_10 reg_mux_40_10_inst40 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i40_p),
				      .o_p       (tx_in[409:400])
				      );

        reg_mux_40_10 reg_mux_40_10_inst41 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i41_p),
				      .o_p       (tx_in[419:410])
				      );

        reg_mux_40_10 reg_mux_40_10_inst42 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i42_p),
				      .o_p       (tx_in[429:420])
				      );

        reg_mux_40_10 reg_mux_40_10_inst43 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i43_p),
				      .o_p       (tx_in[439:430])
				      );

        reg_mux_40_10 reg_mux_40_10_inst44 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i44_p),
				      .o_p       (tx_in[449:440])
				      );

        reg_mux_40_10 reg_mux_40_10_inst45 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i45_p),
				      .o_p       (tx_in[459:450])
				      );

        reg_mux_40_10 reg_mux_40_10_inst46 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i46_p),
				      .o_p       (tx_in[469:460])
				      );

        reg_mux_40_10 reg_mux_40_10_inst47 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i47_p),
				      .o_p       (tx_in[479:470])
				      );

        reg_mux_40_10 reg_mux_40_10_inst48 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i48_p),
				      .o_p       (tx_in[489:480])
				      );

        reg_mux_40_10 reg_mux_40_10_inst49 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i49_p),
				      .o_p       (tx_in[499:490])
				      );

        reg_mux_40_10 reg_mux_40_10_inst50 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i50_p),
				      .o_p       (tx_in[509:500])
				      );

        reg_mux_40_10 reg_mux_40_10_inst51 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i51_p),
				      .o_p       (tx_in[519:510])
				      );

        reg_mux_40_10 reg_mux_40_10_inst52 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i52_p),
				      .o_p       (tx_in[529:520])
				      );

        reg_mux_40_10 reg_mux_40_10_inst53 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i53_p),
				      .o_p       (tx_in[539:530])
				      );

        reg_mux_40_10 reg_mux_40_10_inst54 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i54_p),
				      .o_p       (tx_in[549:540])
				      );

        reg_mux_40_10 reg_mux_40_10_inst55 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i55_p),
				      .o_p       (tx_in[559:550])
				      );

        reg_mux_40_10 reg_mux_40_10_inst56 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i56_p),
				      .o_p       (tx_in[569:560])
				      );

        reg_mux_40_10 reg_mux_40_10_inst57 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i57_p),
				      .o_p       (tx_in[579:570])
				      );

        reg_mux_40_10 reg_mux_40_10_inst58 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i58_p),
				      .o_p       (tx_in[589:580])
				      );

        reg_mux_40_10 reg_mux_40_10_inst59 (
				      .clk       (I_clk),
				      .rst       (I_rst_n),
				      .sel       (I_sel),
                                      .tx_locked (tx_locked),
				      .i_p       (i59_p),
				      .o_p       (tx_in[599:590])
				      );




   s3_lvds_tx60 lvds_tx60_inst(
	                      .pll_areset     (I_pll_areset),
	                      .tx_in          (tx_in),
	                      .tx_inclock     (I_tx_inclock),
	                      .tx_coreclock   (O_tx_coreclock),
                              .tx_locked      (tx_locked),
	                      .tx_out         (O_tx60_out),
                              .tx_outclock    (O_tx_outclock)
                              );

assign O_tx_locked = tx_locked;

endmodule
