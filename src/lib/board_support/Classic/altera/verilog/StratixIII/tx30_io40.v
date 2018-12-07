module tx30_io40 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
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

                  output [29:0] O_tx30_out,
                  output        O_tx_locked,
                  output        O_tx_outclock,
                  output        O_tx_coreclock

                  );

   wire [299:0]                 tx_in;
   wire [29:0]                  tx30_out;

wire 				tx_locked;

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



   lvds_tx30 lvds_tx30_inst(
	                      .pll_areset     (I_pll_areset),
	                      .tx_in          (tx_in),
	                      .tx_inclock     (I_tx_inclock),
	                      .tx_coreclock   (O_tx_coreclock),
                              .tx_locked      (tx_locked),
	                      .tx_out         (O_tx30_out),
                              .tx_outclock    (O_tx_outclock)
                              );
assign O_tx_locked = tx_locked;

endmodule
