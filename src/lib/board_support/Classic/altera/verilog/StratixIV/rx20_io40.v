`timescale 1ns / 100ps
/*****************************************************************************
* Bluespec Inc.
* Proprietary and Confidential Material.
* Copyright
******************************************************************************
* 7x06_rx20_io40 Top level
*
* function: 
* history:
* date         author  rev     modification
* 03/02/11     aa              Initial release R01

******************************************************************************/


module rx20_io40 (
                  input        I_clk,
                  input        I_rst_n,
                  input [1:0]  I_sel,
                  input [19:0] I_rx_cda_reset,
                  input [19:0] I_rx_cda,
                  input        I_rx_cda_rdy,
                  input        I_pll_areset,
                  input        I_rx_inclock,

                  input [19:0]   I_rx20_in,

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
                  output        O_rx_locked,
                  output        O_rx_outclock
                  );

   wire [199:0]                 rx_out;


generate 
   begin : L0
   genvar i;
   for (i=o; i<=19; i = i+1)
    begin u
      demux_10_40 demux_10_40_inst (
				  .clk       (rx_outclock),
				  .rst_n     (I_rst_n),
                                  .cda_rdy   (I_rx_cda_rdy),
				  .sel       (sel),
				  .i_p       (rx_out[9+(i*10):10*i]),
				  .o_p       (o[i]_p)
				  );
    end // for (i=o; i<=19; i = i+1)
   end // block: L0
endgenerate



`ifdef  BOARD_7406
 s4_lvds_rx20
`else
 s3_lvds_rx20
`endif
 lvds_rx20_inst(
	                      .pll_areset             (I_pll_areset),
	                      .rx_cda_reset           (I_rx_cda_reset),
	                      .rx_channel_data_align  (I_rx_cda),
	                      .rx_in                  (I_rx20_in),
	                      .rx_inclock             (I_rx_inclock),
	                      .rx_out                 (rx_out),
	                      .rx_locked              (O_rx_locked),
	                      .rx_outclock            (O_rx_outclock)
                              );


endmodule
