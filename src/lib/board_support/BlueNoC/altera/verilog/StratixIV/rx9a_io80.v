`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_rx9_io80 Top level
*
* function: 
* history:
* date         author  rev     modification
* 06/15/11     aa      0.0        Initial release R01

******************************************************************************/

module rx9a_io80 (
                  input        I_clk,
                  input        I_rst_n,
//                  input [8:0] I_rx_cda_reset,
//                  input [8:0] I_rx_cda,
//                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,
                  input [8:0]   I_rx9_in,
                  output [79:0] O_d0_p,
                  output [79:0] O_d1_p,
                  output [79:0] O_d2_p,
                  output [79:0] O_d3_p,
                  output [79:0] O_d4_p,
                  output [79:0] O_d5_p,
                  output [79:0] O_d6_p,
                  output [79:0] O_d7_p,
                  output [79:0] O_d8_p,
                  output        O_rx_locked,
                  output        O_rx_data_locked,
                  output        O_rx_outclock,
                  output        O_rx_cda_rdy
                  );

   wire [89:0]                 rx_out;

wire 			       rx_outclock, rx_locked;
//
wire [2:0] 			pattern_mux_sel;
wire 				pattern_err;
wire 				pattern_found;
wire 				mux_go, mux_done;
wire [79:0] 			o0_p, o1_p;
wire [79:0] 			o2_p, o3_p;
wire [79:0] 			o4_p, o5_p;
wire [79:0] 			o6_p, o7_p;
wire [79:0] 			o8_p;
//
wire [8:0] 			cda_reset, cda;
wire 				cda_rdy;



cda9_init80_sm cda9_init80(
               .I_clk              (I_clk),
               .I_reset_n          (I_rst_n),
               .I_rx_locked        (rx_locked),
               .I_i_p              (o0_p),
               .I_mux_done         (mux_done),
               .O_pattern_found    (pattern_found),
               .O_pattern_err      (pattern_err),
               .O_cda_reset        (cda_reset),
               .O_cda              (cda),
               .O_rx_cda_rdy       (cda_rdy),
               .O_pattern_mux_sel  (pattern_mux_sel),
               .O_mux_go           (mux_go)
	       );



      demux_10_80 demux_10_80_inst0 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[9:0]),
				  .o_p       (o0_p),
                                  .mux_done  (mux_done)
				  );

      demux_10_80 demux_10_80_inst1 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[19:10]),
				  .o_p       (o1_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst2 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[29:20]),
				  .o_p       (o2_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst3 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[39:30]),
				  .o_p       (o3_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst4 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[49:40]),
				  .o_p       (o4_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst5 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[59:50]),
				  .o_p       (o5_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst6 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[69:60]),
				  .o_p       (o6_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst7 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[79:70]),
				  .o_p       (o7_p),
                                  .mux_done  ()
				  );

      demux_10_80 demux_10_80_inst8 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .mux_go    (mux_go),
				  .pattern_mux_sel       (pattern_mux_sel),
				  .i_p       (rx_out[89:80]),
				  .o_p       (o8_p),
                                  .mux_done  ()
				  );


//////
/*
init80a_sm init80a_sm_int (
                                 .clk               (rx_outclock),
        			 .rst_n             (I_rst_n),
                                 .cda_rdy           (I_rx_cda_rdy),
				 .i_p               (o0_p),
                                 .O_pattern_mux_sel (pattern_mux_sel),
                                 .O_pattern_found   (rx_pattern_found),
                                 .O_pattern_err     (rx_pattern_err)
                     );

*/
 /////
//

`ifdef  BOARD_7406
 s4_lvds_rx9
`else
 s3_lvds_rx9
`endif
   lvds_rx9_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (cda_reset),
	                      .rx_channel_data_align  (cda),
	                      .rx_in                  (I_rx9_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (rx_locked),
	                      .rx_outclock            (rx_outclock)
                              );

assign O_rx_locked  = rx_locked;
assign O_rx_outclock = rx_outclock;
assign O_rx_data_locked = pattern_found;
assign O_d0_p = o0_p;
assign O_d1_p = o1_p;
assign O_d2_p = o2_p;
assign O_d3_p = o3_p;
assign O_d4_p = o4_p;
assign O_d5_p = o5_p;
assign O_d6_p = o6_p;
assign O_d7_p = o7_p;
assign O_d8_p = o8_p;
assign O_rx_cda_rdy = cda_rdy;

endmodule
