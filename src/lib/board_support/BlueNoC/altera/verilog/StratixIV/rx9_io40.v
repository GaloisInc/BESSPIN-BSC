`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_rx9_io40 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/

module rx9_io40 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [8:0] I_rx_cda_reset,
                  input [8:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,
                  input [8:0]   I_rx9_in,
                  output [39:0] o0_p,
                  output [39:0] o1_p,
                  output [39:0] o2_p,
                  output [39:0] o3_p,
                  output [39:0] o4_p,
                  output [39:0] o5_p,
                  output [39:0] o6_p,
                  output [39:0] o7_p,
                  output [39:0] o8_p,
                  output        O_rx_locked,
                  output        O_rx_outclock
                  );

   wire [89:0]                 rx_out;

wire 			       rx_outclock;

assign O_rx_outclock = rx_outclock;

      demux_10_40 demux_10_40_inst0 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[9:0]),
				  .o_p       (o0_p)
				  );

      demux_10_40 demux_10_40_inst1 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[19:10]),
				  .o_p       (o1_p)
				  );

      demux_10_40 demux_10_40_inst2 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[29:20]),
				  .o_p       (o2_p)
				  );

      demux_10_40 demux_10_40_inst3 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[39:30]),
				  .o_p       (o3_p)
				  );

      demux_10_40 demux_10_40_inst4 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[49:40]),
				  .o_p       (o4_p)
				  );

      demux_10_40 demux_10_40_inst5 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[59:50]),
				  .o_p       (o5_p)
				  );

      demux_10_40 demux_10_40_inst6 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[69:60]),
				  .o_p       (o6_p)
				  );

      demux_10_40 demux_10_40_inst7 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[79:70]),
				  .o_p       (o7_p)
				  );

      demux_10_40 demux_10_40_inst8 (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (I_sel),
				  .i_p       (rx_out[89:80]),
				  .o_p       (o8_p)
				  );



//



`ifdef  BOARD_7406
 s4_lvds_rx9
`else
 s3_lvds_rx9
`endif
   lvds_rx9_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx9_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (O_rx_locked),
	                      .rx_outclock            (rx_outclock)
                              );


endmodule
