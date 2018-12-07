module rx30_io40 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [29:0] I_rx_cda_reset,
                  input [29:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,

                  input [29:0]   I_rx30_in,

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


                  output        O_rx_locked,
                  output        O_rx_outclock
                  );

   wire [299:0]                 rx_out;
   wire [29:0]                  rx_in;

reg 				clk_d2, clk_d4;
reg [1:0]                       sel;
wire rx_outclock;



   always @(posedge clk_d4 or negedge I_rst_n) begin
      if (!I_rst_n) begin
         sel = 0;
      end
      else begin
         sel = sel + 1'b1;
      end
   end





   demux_10_40 demux_10_40_inst0 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[9:0]),
				  .o_p       (o0_p)
				  );
   demux_10_40 demux_10_40_inst1 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[19:10]),
				  .o_p       (o1_p)
				  );
   demux_10_40 demux_10_40_inst2 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[29:20]),
				  .o_p       (o2_p)
				  );
   demux_10_40 demux_10_40_inst3 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[39:30]),
				  .o_p       (o3_p)
				  );
   demux_10_40 demux_10_40_inst4 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[49:40]),
				  .o_p       (o4_p)
				  );
   demux_10_40 demux_10_40_inst5 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[59:50]),
				  .o_p       (o5_p)
				  );
   demux_10_40 demux_10_40_inst6 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[69:60]),
				  .o_p       (o6_p)
				  );
   demux_10_40 demux_10_40_inst7 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[79:70]),
				  .o_p       (o7_p)
				  );
   demux_10_40 demux_10_40_inst8 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[89:80]),
				  .o_p       (o8_p)
				  );

   demux_10_40 demux_10_40_inst9 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[99:90]),
				  .o_p       (o9_p)
				  );

   demux_10_40 demux_10_40_inst10 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[109:100]),
				   .o_p       (o10_p)
				   );
   demux_10_40 demux_10_40_inst11 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[119:110]),
				   .o_p       (o11_p)
				   );
   demux_10_40 demux_10_40_inst12 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[129:120]),
				   .o_p       (o12_p)
				   );
   demux_10_40 demux_10_40_inst13 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[139:130]),
				   .o_p       (o13_p)
				   );
   demux_10_40 demux_10_40_inst14 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[149:140]),
				   .o_p       (o14_p)
				   );
   demux_10_40 demux_10_40_inst15 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[159:150]),
				   .o_p       (o15_p)
				   );
   demux_10_40 demux_10_40_inst16 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[169:160]),
				   .o_p       (o16_p)
				   );
   demux_10_40 demux_10_40_inst17 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[179:170]),
				   .o_p       (o17_p)
				   );
   demux_10_40 demux_10_40_inst18 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[189:180]),
				   .o_p       (o18_p)
				   );
   demux_10_40 demux_10_40_inst19 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[199:190]),
	                    	   .o_p       (o19_p)
				   );

   demux_10_40 demux_10_40_inst20 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[209:200]),
	                    	   .o_p       (o20_p)
				   );

   demux_10_40 demux_10_40_inst21 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[219:210]),
	                    	   .o_p       (o21_p)
				   );

   demux_10_40 demux_10_40_inst22 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[229:220]),
	                    	   .o_p       (o22_p)
				   );

   demux_10_40 demux_10_40_inst23 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[239:230]),
	                    	   .o_p       (o23_p)
				   );

   demux_10_40 demux_10_40_inst24 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[249:240]),
	                    	   .o_p       (o24_p)
				   );

   demux_10_40 demux_10_40_inst25 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[259:250]),
	                    	   .o_p       (o25_p)
				   );

   demux_10_40 demux_10_40_inst26 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[269:260]),
	                    	   .o_p       (o26_p)
				   );

   demux_10_40 demux_10_40_inst27 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[279:270]),
	                    	   .o_p       (o27_p)
				   );

   demux_10_40 demux_10_40_inst28 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[289:280]),
	                    	   .o_p       (o28_p)
				   );

   demux_10_40 demux_10_40_inst29 (
				   .clk       (rx_outclock),
				   .rst_n     (I_rst_n),
                                   .cda_rdy   (I_rx_cda_rdy),
				   .sel       (sel),
				   .i_p       (rx_out[299:290]),
	                    	   .o_p       (o29_p)
				   );




   lvds_rx30 lvds_rx30_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx30_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (O_rx_locked),
	                      .rx_outclock            (rx_outclock)
                              );


assign O_rx_outclock = rx_outclock;

endmodule
