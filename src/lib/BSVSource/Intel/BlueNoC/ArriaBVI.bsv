////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
package ArriaBVI;

interface Pcie_sim;
   // following methods are sim only:
   method Action sim_pipe_pclk_in (Bit#(1) x);
   method Bit#(2) sim_pipe_rate;
   method Bit#(5) sim_ltssmstate;
   method Bit#(3) eidleinfersel0;
   method Bit#(3) eidleinfersel1;
   method Bit#(3) eidleinfersel2;
   method Bit#(3) eidleinfersel3;
   method Bit#(3) eidleinfersel4;
   method Bit#(3) eidleinfersel5;
   method Bit#(3) eidleinfersel6;
   method Bit#(3) eidleinfersel7;
   method Bit#(2) powerdown0;
   method Bit#(2) powerdown1;
   method Bit#(2) powerdown2;
   method Bit#(2) powerdown3;
   method Bit#(2) powerdown4;
   method Bit#(2) powerdown5;
   method Bit#(2) powerdown6;
   method Bit#(2) powerdown7;
   method Bit#(1) rxpolarity0;
   method Bit#(1) rxpolarity1;
   method Bit#(1) rxpolarity2;
   method Bit#(1) rxpolarity3;
   method Bit#(1) rxpolarity4;
   method Bit#(1) rxpolarity5;
   method Bit#(1) rxpolarity6;
   method Bit#(1) rxpolarity7;
   method Bit#(1) txcompl0;
   method Bit#(1) txcompl1;
   method Bit#(1) txcompl2;
   method Bit#(1) txcompl3;
   method Bit#(1) txcompl4;
   method Bit#(1) txcompl5;
   method Bit#(1) txcompl6;
   method Bit#(1) txcompl7;
   method Bit#(32) txdata0;
   method Bit#(32) txdata1;
   method Bit#(32) txdata2;
   method Bit#(32) txdata3;
   method Bit#(32) txdata4;
   method Bit#(32) txdata5;
   method Bit#(32) txdata6;
   method Bit#(32) txdata7;
   method Bit#(4) txdatak0;
   method Bit#(4) txdatak1;
   method Bit#(4) txdatak2;
   method Bit#(4) txdatak3;
   method Bit#(4) txdatak4;
   method Bit#(4) txdatak5;
   method Bit#(4) txdatak6;
   method Bit#(4) txdatak7;
   method Bit#(1) txdetectrx0;
   method Bit#(1) txdetectrx1;
   method Bit#(1) txdetectrx2;
   method Bit#(1) txdetectrx3;
   method Bit#(1) txdetectrx4;
   method Bit#(1) txdetectrx5;
   method Bit#(1) txdetectrx6;
   method Bit#(1) txdetectrx7;
   method Bit#(1) txelecidle0;
   method Bit#(1) txelecidle1;
   method Bit#(1) txelecidle2;
   method Bit#(1) txelecidle3;
   method Bit#(1) txelecidle4;
   method Bit#(1) txelecidle5;
   method Bit#(1) txelecidle6;
   method Bit#(1) txelecidle7;
   method Bit#(1) txdeemph0;
   method Bit#(1) txdeemph1;
   method Bit#(1) txdeemph2;
   method Bit#(1) txdeemph3;
   method Bit#(1) txdeemph4;
   method Bit#(1) txdeemph5;
   method Bit#(1) txdeemph6;
   method Bit#(1) txdeemph7;
   method Bit#(3) txmargin0;
   method Bit#(3) txmargin1;
   method Bit#(3) txmargin2;
   method Bit#(3) txmargin3;
   method Bit#(3) txmargin4;
   method Bit#(3) txmargin5;
   method Bit#(3) txmargin6;
   method Bit#(3) txmargin7;
   method Bit#(1) txswing0;
   method Bit#(1) txswing1;
   method Bit#(1) txswing2;
   method Bit#(1) txswing3;
   method Bit#(1) txswing4;
   method Bit#(1) txswing5;
   method Bit#(1) txswing6;
   method Bit#(1) txswing7;
   method Action phystatus0 (Bit#(1) x);
   method Action phystatus1 (Bit#(1) x);
   method Action phystatus2 (Bit#(1) x);
   method Action phystatus3 (Bit#(1) x);
   method Action phystatus4 (Bit#(1) x);
   method Action phystatus5 (Bit#(1) x);
   method Action phystatus6 (Bit#(1) x);
   method Action phystatus7 (Bit#(1) x);
   method Action rxdata0 (Bit#(32) x);
   method Action rxdata1 (Bit#(32) x);
   method Action rxdata2 (Bit#(32) x);
   method Action rxdata3 (Bit#(32) x);
   method Action rxdata4 (Bit#(32) x);
   method Action rxdata5 (Bit#(32) x);
   method Action rxdata6 (Bit#(32) x);
   method Action rxdata7 (Bit#(32) x);
   method Action rxdatak0 (Bit#(4) x);
   method Action rxdatak1 (Bit#(4) x);
   method Action rxdatak2 (Bit#(4) x);
   method Action rxdatak3 (Bit#(4) x);
   method Action rxdatak4 (Bit#(4) x);
   method Action rxdatak5 (Bit#(4) x);
   method Action rxdatak6 (Bit#(4) x);
   method Action rxdatak7 (Bit#(4) x);
   method Action rxelecidle0 (Bit#(1) x);
   method Action rxelecidle1 (Bit#(1) x);
   method Action rxelecidle2 (Bit#(1) x);
   method Action rxelecidle3 (Bit#(1) x);
   method Action rxelecidle4 (Bit#(1) x);
   method Action rxelecidle5 (Bit#(1) x);
   method Action rxelecidle6 (Bit#(1) x);
   method Action rxelecidle7 (Bit#(1) x);
   method Action rxstatus0 (Bit#(3) x);
   method Action rxstatus1 (Bit#(3) x);
   method Action rxstatus2 (Bit#(3) x);
   method Action rxstatus3 (Bit#(3) x);
   method Action rxstatus4 (Bit#(3) x);
   method Action rxstatus5 (Bit#(3) x);
   method Action rxstatus6 (Bit#(3) x);
   method Action rxstatus7 (Bit#(3) x);
   method Action rxvalid0 (Bit#(1) x);
   method Action rxvalid1 (Bit#(1) x);
   method Action rxvalid2 (Bit#(1) x);
   method Action rxvalid3 (Bit#(1) x);
   method Action rxvalid4 (Bit#(1) x);
   method Action rxvalid5 (Bit#(1) x);
   method Action rxvalid6 (Bit#(1) x);
   method Action rxvalid7 (Bit#(1) x);
   method Action rxdataskip0 (Bit#(1) x);
   method Action rxdataskip1 (Bit#(1) x);
   method Action rxdataskip2 (Bit#(1) x);
   method Action rxdataskip3 (Bit#(1) x);
   method Action rxdataskip4 (Bit#(1) x);
   method Action rxdataskip5 (Bit#(1) x);
   method Action rxdataskip6 (Bit#(1) x);
   method Action rxdataskip7 (Bit#(1) x);
   method Action rxblkst0 (Bit#(1) x);
   method Action rxblkst1 (Bit#(1) x);
   method Action rxblkst2 (Bit#(1) x);
   method Action rxblkst3 (Bit#(1) x);
   method Action rxblkst4 (Bit#(1) x);
   method Action rxblkst5 (Bit#(1) x);
   method Action rxblkst6 (Bit#(1) x);
   method Action rxblkst7 (Bit#(1) x);
   method Action rxsynchd0 (Bit#(2) x);
   method Action rxsynchd1 (Bit#(2) x);
   method Action rxsynchd2 (Bit#(2) x);
   method Action rxsynchd3 (Bit#(2) x);
   method Action rxsynchd4 (Bit#(2) x);
   method Action rxsynchd5 (Bit#(2) x);
   method Action rxsynchd6 (Bit#(2) x);
   method Action rxsynchd7 (Bit#(2) x);
   method Bit#(18) currentcoeff0;
   method Bit#(18) currentcoeff1;
   method Bit#(18) currentcoeff2;
   method Bit#(18) currentcoeff3;
   method Bit#(18) currentcoeff4;
   method Bit#(18) currentcoeff5;
   method Bit#(18) currentcoeff6;
   method Bit#(18) currentcoeff7;
   method Bit#(3) currentrxpreset0;
   method Bit#(3) currentrxpreset1;
   method Bit#(3) currentrxpreset2;
   method Bit#(3) currentrxpreset3;
   method Bit#(3) currentrxpreset4;
   method Bit#(3) currentrxpreset5;
   method Bit#(3) currentrxpreset6;
   method Bit#(3) currentrxpreset7;
   method Bit#(2) txsynchd0;
   method Bit#(2) txsynchd1;
   method Bit#(2) txsynchd2;
   method Bit#(2) txsynchd3;
   method Bit#(2) txsynchd4;
   method Bit#(2) txsynchd5;
   method Bit#(2) txsynchd6;
   method Bit#(2) txsynchd7;
   method Bit#(1) txblkst0;
   method Bit#(1) txblkst1;
   method Bit#(1) txblkst2;
   method Bit#(1) txblkst3;
   method Bit#(1) txblkst4;
   method Bit#(1) txblkst5;
   method Bit#(1) txblkst6;
   method Bit#(1) txblkst7;
   method Bit#(1) txdataskip0;
   method Bit#(1) txdataskip1;
   method Bit#(1) txdataskip2;
   method Bit#(1) txdataskip3;
   method Bit#(1) txdataskip4;
   method Bit#(1) txdataskip5;
   method Bit#(1) txdataskip6;
   method Bit#(1) txdataskip7;
   method Bit#(2) rate0;
   method Bit#(2) rate1;
   method Bit#(2) rate2;
   method Bit#(2) rate3;
   method Bit#(2) rate4;
   method Bit#(2) rate5;
   method Bit#(2) rate6;
   method Bit#(2) rate7;
   // end of sim-only methods
endinterface: Pcie_sim

(* always_ready, always_enabled *)
interface Pcie;
   // input clocks: refclk,
   // input resets: npor, pin_perst
   method Action rx_in0 (Bit#(1) x);
   method Action rx_in1 (Bit#(1) x);
   method Action rx_in2 (Bit#(1) x);
   method Action rx_in3 (Bit#(1) x);
   method Action rx_in4 (Bit#(1) x);
   method Action rx_in5 (Bit#(1) x);
   method Action rx_in6 (Bit#(1) x);
   method Action rx_in7 (Bit#(1) x);
   method Bit#(1) tx_out0;
   method Bit#(1) tx_out1;
   method Bit#(1) tx_out2;
   method Bit#(1) tx_out3;
   method Bit#(1) tx_out4;
   method Bit#(1) tx_out5;
   method Bit#(1) tx_out6;
   method Bit#(1) tx_out7;
endinterface: Pcie

// Interface for 'import "BVI"' module:
(* always_ready, always_enabled *)
interface Arria10GXPCIExpress;
   // input clocks: refclk,
   // input resets: npor, pin_perst

   interface Clock coreclkout_hip;

   //interface Pcie_sim  pcie_sim;
   interface Pcie      pcie;
   /*
   method Bit#(256) devkit_status;
   method Action devkit_ctrl (Bit#(256) x);
   */

   method Action test_in (Bit#(32) x);//T.O.
   method Action simu_mode_pipe (Bit#(1) x);//T.O.

   method Action hpg_ctrler (Bit#(5) x);//T.O.
   method Bit#(4) tl_cfg_add;
   method Bit#(32) tl_cfg_ctl;
   method Bit#(53) tl_cfg_sts;
   method Action cpl_err (Bit#(7) x);//T.O.
   method Action cpl_pending (Bit#(1) x);//T.O.

   method Bit#(2) currentspeed; // Gen1, 2, 3

   method Action pld_core_ready (Bit#(1) x); // drive with serdes_pll_locked;
   method Bool    pld_clk_inuse;  // do reset if not asserted
   method Bool    reset_status;   // active high
   method Bit#(1) serdes_pll_locked;
   method Bit#(1) testin_zero;

   method Bit#(1) derr_cor_ext_rcv; //)
   method Bit#(1) derr_cor_ext_rpl; //) error reports (for debug only)
   method Bit#(1) derr_rpl;         //)
   method Bit#(1) dlup;
   method Bool    dlup_exit;     // do reset when low (at least 32 cycles)
   method Bit#(1) ev128ns;     // pulse every 128ns
   method Bit#(1) ev1us;       // pulse every microsecond
   method Bool    hotrst_exit;   // do reset when low
   method Bit#(4) int_status;
   method Bool    l2_exit;       // do reset when low
   method Bit#(4) lane_act;   // number of lanes currently active
   method Bit#(5) ltssmstate;
   method Bit#(1) rx_par_err;
   method Bit#(2) tx_par_err;
   method Bit#(1) cfg_par_err;
   method Bit#(8) ko_cpl_spc_header;
   method Bit#(12) ko_cpl_spc_data;

   method Action app_int_sts (Bit#(1) x);
   method Bit#(1) app_int_ack;
   method Action app_msi_num (Bit#(5) x);
   method Action app_msi_req (Bit#(1) x);
   method Action app_msi_tc (Bit#(3) x);
   method Bit#(1) app_msi_ack;

   method Action pm_auxpwr (Bit#(1) x);//T.O.
   method Action pm_data (Bit#(10) x);//T.O.
   method Action pme_to_cr (Bit#(1) x);//T.O.
   method Action pm_event (Bit#(1) x);//T.O.
   method Bit#(1) pme_to_sr;

   method Bit#(8) rx_st_bar;
   method Action rx_st_mask (Bit#(1) x);

   method Bool    rx_st_sop;
   method Bool    rx_st_eop;
   method Bool    rx_st_empty;
   method Bool    rx_st_err;
   method Bool    rx_st_valid;
   method Action rx_st_ready (Bool    x);
   method Bit#(64) rx_st_data;

   method Bit#(12) tx_cred_data_fc;
   method Bit#(6) tx_cred_fc_hip_cons;
   method Bit#(6) tx_cred_fc_infinite;
   method Bit#(8) tx_cred_hdr_fc;
   method Action tx_cred_fc_sel (Bit#(2) x);//T.O.

   method Action tx_st_sop (Bool    x);
   method Action tx_st_eop (Bool    x);
   method Action tx_st_empty (Bool    x);
   method Action tx_st_err (Bool    x);
   method Action tx_st_valid (Bool    x);
   method Bool    tx_st_ready;
   method Action tx_st_data (Bit#(64) x);
endinterface

(* always_ready, always_enabled *)
interface Arria10GX128PCIExpress;
   // input clocks: refclk,
   // input resets: npor, pin_perst

   interface Clock coreclkout_hip;

   //interface Pcie_sim  pcie_sim;
   interface Pcie      pcie;

   method Action test_in (Bit#(32) x);//T.O.
   method Action simu_mode_pipe (Bit#(1) x);//T.O.

   method Action hpg_ctrler (Bit#(5) x);//T.O.
   method Bit#(4) tl_cfg_add;
   method Bit#(32) tl_cfg_ctl;
   method Bit#(53) tl_cfg_sts;
   method Action cpl_err (Bit#(7) x);//T.O.
   method Action cpl_pending (Bit#(1) x);//T.O.

   method Bit#(2) currentspeed; // Gen1, 2, 3

   method Action pld_core_ready (Bit#(1) x); // drive with serdes_pll_locked;
   method Bool    pld_clk_inuse;  // do reset if not asserted
   method Bool    reset_status;   // active high
   method Bit#(1) serdes_pll_locked;
   method Bit#(1) testin_zero;

   method Bit#(1) derr_cor_ext_rcv; //)
   method Bit#(1) derr_cor_ext_rpl; //) error reports (for debug only)
   method Bit#(1) derr_rpl;         //)
   method Bit#(1) dlup;
   method Bool    dlup_exit;     // do reset when low (at least 32 cycles)
   method Bit#(1) ev128ns;     // pulse every 128ns
   method Bit#(1) ev1us;       // pulse every microsecond
   method Bool    hotrst_exit;   // do reset when low
   method Bit#(4) int_status;
   method Bool    l2_exit;       // do reset when low
   method Bit#(4) lane_act;   // number of lanes currently active
   method Bit#(5) ltssmstate;
   method Bit#(1) rx_par_err;
   method Bit#(2) tx_par_err;
   method Bit#(1) cfg_par_err;
   method Bit#(8) ko_cpl_spc_header;
   method Bit#(12) ko_cpl_spc_data;

   method Action app_int_sts (Bit#(1) x);
   method Bit#(1) app_int_ack;
   method Action app_msi_num (Bit#(5) x);
   method Action app_msi_req (Bit#(1) x);
   method Action app_msi_tc (Bit#(3) x);
   method Bit#(1) app_msi_ack;

   method Action pm_auxpwr (Bit#(1) x);//T.O.
   method Action pm_data (Bit#(10) x);//T.O.
   method Action pme_to_cr (Bit#(1) x);//T.O.
   method Action pm_event (Bit#(1) x);//T.O.
   method Bit#(1) pme_to_sr;

   method Bit#(8) rx_st_bar;
   method Action rx_st_mask (Bit#(1) x);

   method Bool    rx_st_sop;
   method Bool    rx_st_eop;
   method Bool    rx_st_empty;
   method Bool    rx_st_err;
   method Bool    rx_st_valid;
   method Action rx_st_ready (Bool    x);
   method Bit#(128) rx_st_data;

   method Bit#(12) tx_cred_data_fc;
   method Bit#(6) tx_cred_fc_hip_cons;
   method Bit#(6) tx_cred_fc_infinite;
   method Bit#(8) tx_cred_hdr_fc;
   method Action tx_cred_fc_sel (Bit#(2) x);//T.O.

   method Action tx_st_sop (Bool    x);
   method Action tx_st_eop (Bool    x);
   method Action tx_st_empty (Bool    x);
   method Action tx_st_err (Bool    x);
   method Action tx_st_valid (Bool    x);
   method Bool    tx_st_ready;
   method Action tx_st_data (Bit#(128) x);
endinterface

import "BVI" p1b =
module vMkArria10GX128PCIExpress#(Clock pld_clk, Reset pin_perst, Reset npor) (Arria10GX128PCIExpress);

   //let sys_reset <- invertCurrentReset;

   default_clock clk(refclk); // 100 MHz refclk
   no_reset;

   input_clock (pld_clk) = pld_clk;

   input_reset (pin_perst) = pin_perst;
   input_reset npor (npor) = pin_perst;

   output_clock coreclkout_hip (coreclkout_hip);

   interface Pcie pcie;
      method rx_in0(rx_in0)     		 enable((*inhigh*)en079)  reset_by(no_reset);
      method rx_in1(rx_in1)     		 enable((*inhigh*)en080)  reset_by(no_reset);
      method rx_in2(rx_in2)     		 enable((*inhigh*)en081)  reset_by(no_reset);
      method rx_in3(rx_in3)     		 enable((*inhigh*)en082)  reset_by(no_reset);
      method rx_in4(rx_in4)     		 enable((*inhigh*)en083)  reset_by(no_reset);
      method rx_in5(rx_in5)     		 enable((*inhigh*)en084)  reset_by(no_reset);
      method rx_in6(rx_in6)     		 enable((*inhigh*)en085)  reset_by(no_reset);
      method rx_in7(rx_in7)     		 enable((*inhigh*)en086)  reset_by(no_reset);

      method tx_out0 tx_out0        		 reset_by(no_reset);
      method tx_out1 tx_out1        		 reset_by(no_reset);
      method tx_out2 tx_out2        		 reset_by(no_reset);
      method tx_out3 tx_out3        		 reset_by(no_reset);
      method tx_out4 tx_out4        		 reset_by(no_reset);
      method tx_out5 tx_out5        		 reset_by(no_reset);
      method tx_out6 tx_out6        		 reset_by(no_reset);
      method tx_out7 tx_out7        		 reset_by(no_reset);
   endinterface

      method pm_auxpwr(pm_auxpwr)     		 enable((*inhigh*)en093)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method pm_data(pm_data)     		 enable((*inhigh*)en094)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method pme_to_cr(pme_to_cr)     		 enable((*inhigh*)en095)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method pm_event(pm_event)     		 enable((*inhigh*)en096)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method rx_st_mask(rx_st_mask)     	 enable((*inhigh*)en097)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method rx_st_ready(rx_st_ready)     	 enable((*inhigh*)en098)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_cred_fc_sel(tx_cred_fc_sel)      enable((*inhigh*)en099)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_sop(tx_st_sop)     		 enable((*inhigh*)en100)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_eop(tx_st_eop)     		 enable((*inhigh*)en101)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_empty(tx_st_empty)  		 enable((*inhigh*)en801)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_err(tx_st_err)     		 enable((*inhigh*)en102)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_valid(tx_st_valid)     	 enable((*inhigh*)en103)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method tx_st_data(tx_st_data)     	 enable((*inhigh*)en104)  clocked_by(coreclkout_hip)  reset_by(no_reset);

      method app_int_sts(app_int_sts)     	 enable((*inhigh*)en087)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method app_msi_num(app_msi_num)     	 enable((*inhigh*)en088)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method app_msi_req(app_msi_req)     	 enable((*inhigh*)en089)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method app_msi_tc(app_msi_tc)     	 enable((*inhigh*)en090)  clocked_by(coreclkout_hip)  reset_by(no_reset);

      method pld_core_ready(pld_core_ready)      enable((*inhigh*)en078)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method hpg_ctrler(hpg_ctrler)              enable((*inhigh*)en000)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method cpl_err(cpl_err)                    enable((*inhigh*)en001)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method cpl_pending(cpl_pending)            enable((*inhigh*)en002)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method test_in(test_in)                    enable((*inhigh*)en003)  clocked_by(coreclkout_hip)  reset_by(no_reset);
      method simu_mode_pipe(simu_mode_pipe)      enable((*inhigh*)en004)  clocked_by(coreclkout_hip)  reset_by(no_reset);

   method tl_cfg_add tl_cfg_add                       clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tl_cfg_ctl tl_cfg_ctl        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tl_cfg_sts tl_cfg_sts        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method currentspeed currentspeed        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method pld_clk_inuse pld_clk_inuse        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method reset_status reset_status        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method serdes_pll_locked serdes_pll_locked         clocked_by(coreclkout_hip)  reset_by(no_reset);
   method testin_zero testin_zero        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method derr_cor_ext_rcv derr_cor_ext_rcv           clocked_by(coreclkout_hip)  reset_by(no_reset);
   method derr_cor_ext_rpl derr_cor_ext_rpl           clocked_by(coreclkout_hip)  reset_by(no_reset);
   method derr_rpl derr_rpl        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method dlup dlup        			      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method dlup_exit dlup_exit        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method ev128ns ev128ns        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method ev1us ev1us        			      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method hotrst_exit hotrst_exit        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method int_status int_status        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method l2_exit l2_exit        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method lane_act lane_act        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method ltssmstate ltssmstate        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_par_err rx_par_err        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_par_err tx_par_err        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method cfg_par_err cfg_par_err        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method ko_cpl_spc_header ko_cpl_spc_header         clocked_by(coreclkout_hip)  reset_by(no_reset);
   method ko_cpl_spc_data ko_cpl_spc_data             clocked_by(coreclkout_hip)  reset_by(no_reset);
   method app_int_ack app_int_ack        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method app_msi_ack app_msi_ack        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method pme_to_sr pme_to_sr        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_bar rx_st_bar        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_sop rx_st_sop        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_eop rx_st_eop      		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_empty rx_st_empty      		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_err rx_st_err        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_valid rx_st_valid        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method rx_st_data rx_st_data        		      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_cred_data_fc tx_cred_data_fc             clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_cred_fc_hip_cons tx_cred_fc_hip_cons     clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_cred_fc_infinite tx_cred_fc_infinite     clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_cred_hdr_fc tx_cred_hdr_fc        	      clocked_by(coreclkout_hip)  reset_by(no_reset);
   method tx_st_ready tx_st_ready              	      clocked_by(coreclkout_hip)  reset_by(no_reset);
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////



schedule (tl_cfg_add, tl_cfg_ctl, tl_cfg_sts, currentspeed,
	  pld_clk_inuse, reset_status,
          serdes_pll_locked, testin_zero,
          pcie_tx_out0, pcie_tx_out1, pcie_tx_out2, pcie_tx_out3,
          pcie_tx_out4, pcie_tx_out5, pcie_tx_out6, pcie_tx_out7,
          derr_cor_ext_rcv, derr_cor_ext_rpl, derr_rpl,
          dlup, dlup_exit, ev128ns, ev1us,
          hotrst_exit, int_status, l2_exit, lane_act,
          ltssmstate, rx_par_err, tx_par_err, cfg_par_err,
          ko_cpl_spc_header, ko_cpl_spc_data, app_int_ack,
          app_msi_ack, pme_to_sr, rx_st_bar, rx_st_sop,
          rx_st_eop, rx_st_empty, rx_st_err, rx_st_valid, rx_st_data,
          tx_cred_data_fc, tx_cred_fc_hip_cons,
          tx_cred_fc_infinite, tx_cred_hdr_fc, tx_st_ready,
          hpg_ctrler, cpl_err, cpl_pending, test_in,
          simu_mode_pipe,
	  pld_core_ready, pcie_rx_in0,
          pcie_rx_in1, pcie_rx_in2, pcie_rx_in3, pcie_rx_in4, pcie_rx_in5,
          pcie_rx_in6, pcie_rx_in7, app_int_sts, app_msi_num,
          app_msi_req, app_msi_tc,
          pm_auxpwr, pm_data, pme_to_cr, pm_event,
          rx_st_mask, rx_st_ready, tx_cred_fc_sel,
          tx_st_sop, tx_st_eop, tx_st_empty, tx_st_err, tx_st_valid,
          tx_st_data ) CF
         (tl_cfg_add, tl_cfg_ctl, tl_cfg_sts, currentspeed,
	  pld_clk_inuse, reset_status,
          serdes_pll_locked, testin_zero,
          pcie_tx_out0, pcie_tx_out1, pcie_tx_out2, pcie_tx_out3,
          pcie_tx_out4, pcie_tx_out5, pcie_tx_out6, pcie_tx_out7,
          derr_cor_ext_rcv, derr_cor_ext_rpl, derr_rpl,
          dlup, dlup_exit, ev128ns, ev1us,
          hotrst_exit, int_status, l2_exit, lane_act,
          ltssmstate, rx_par_err, tx_par_err, cfg_par_err,
          ko_cpl_spc_header, ko_cpl_spc_data, app_int_ack,
          app_msi_ack, pme_to_sr, rx_st_bar, rx_st_sop,
          rx_st_eop, rx_st_empty, rx_st_err, rx_st_valid, rx_st_data,
          tx_cred_data_fc, tx_cred_fc_hip_cons,
          tx_cred_fc_infinite, tx_cred_hdr_fc, tx_st_ready,
          hpg_ctrler, cpl_err, cpl_pending, test_in,
          simu_mode_pipe,
	  pld_core_ready, pcie_rx_in0,
          pcie_rx_in1, pcie_rx_in2, pcie_rx_in3, pcie_rx_in4, pcie_rx_in5,
          pcie_rx_in6, pcie_rx_in7, app_int_sts, app_msi_num,
          app_msi_req, app_msi_tc,
          pm_auxpwr, pm_data, pme_to_cr, pm_event,
          rx_st_mask, rx_st_ready, tx_cred_fc_sel,
          tx_st_sop, tx_st_eop, tx_st_empty, tx_st_err, tx_st_valid,
	  tx_st_data );
endmodule

endpackage
