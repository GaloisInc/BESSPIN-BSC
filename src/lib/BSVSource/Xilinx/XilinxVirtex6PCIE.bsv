////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtex6PCIE.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtex6PCIE;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Clocks            ::*;
import Vector            ::*;
import Connectable       ::*;
import GetPut            ::*;
import Reserved          ::*;
import TieOff            ::*;
import DefaultValue      ::*;
import DReg              ::*;
import Gearbox           ::*;
import FIFO              ::*;
import ClientServer      ::*;
import BUtils            ::*;

import XilinxClocks      ::*;
import PCIE              ::*;

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_ready, always_enabled *)
interface PCIE_TRN_V6;
   interface Clock            clk;
   interface Clock            clk2;
   interface Reset            reset_n;
   method    Bit#(1)          lnk_up_n;
   method    Bit#(8)          fc_ph;
   method    Bit#(12)         fc_pd;
   method    Bit#(8)          fc_nph;
   method    Bit#(12)         fc_npd;
   method    Bit#(8)          fc_cplh;
   method    Bit#(12)         fc_cpld;
   method    Action           fc_sel(FlowControlInfoSelect i);
endinterface

(* always_ready, always_enabled *)
interface PCIE_TRN_TX_V6;
   method    Action           tsof_n(Bit#(1) i);
   method    Action           teof_n(Bit#(1) i);
   method    Action           td(Bit#(64) i);
   method    Action           trem_n(Bit#(1) i);
   method    Action           tsrc_rdy_n(Bit#(1) i);
   method    Bit#(1)          tdst_rdy_n;
   method    Action           tsrc_dsc_n(Bit#(1) i);
   method    Bit#(6)          tbuf_av;
   method    Bit#(1)          terr_drop_n;
   method    Action           tstr_n(Bit#(1) i);
   method    Bit#(1)          tcfg_req_n;
   method    Action           tcfg_gnt_n(Bit#(1) i);
   method    Action           terrfwd_n(Bit#(1) i);
endinterface

(* always_ready, always_enabled *)
interface PCIE_TRN_RX_V6;
   method    Bit#(1)     rsof_n;
   method    Bit#(1)     reof_n;
   method    Bit#(64)    rd;
   method    Bit#(1)     rrem_n;
   method    Bit#(1)     rerrfwd_n;
   method    Bit#(1)     rsrc_rdy_n;
   method    Action      rdst_rdy_n(Bit#(1) i);
   method    Bit#(1)     rsrc_dsc_n;
   method    Action      rnp_ok_n(Bit#(1) i);
   method    Bit#(7)     rbar_hit_n;
endinterface

(* always_ready, always_enabled *)
interface PCIE_PL_V6;
   method    Bit#(2)     initial_link_width;
   method    Bit#(2)     lane_reversal_mode;
   method    Bit#(1)     link_gen2_capable;
   method    Bit#(1)     link_partner_gen2_supported;
   method    Bit#(1)     link_upcfg_capable;
   method    Bit#(1)     sel_link_rate;
   method    Bit#(2)     sel_link_width;
   method    Bit#(6)     ltssm_state;
   method    Action      directed_link_auton(Bit#(1) i);
   method    Action      directed_link_change(Bit#(2) i);
   method    Action      directed_link_speed(Bit#(1) i);
   method    Action      directed_link_width(Bit#(2) i);
   method    Action      upstream_prefer_deemph(Bit#(1) i);
   method    Bit#(1)     received_hot_rst;
endinterface

(* always_ready, always_enabled *)
interface PCIE_CFG_V6;
   method    Bit#(32)    dout;
   method    Bit#(1)     rd_wr_done_n;
   method    Action      di(Bit#(32) i);
   method    Action      dwaddr(Bit#(10) i);
   method    Action      byte_en_n(Bit#(4) i);
   method    Action      wr_en_n(Bit#(1) i);
   method    Action      rd_en_n(Bit#(1) i);
   method    Bit#(8)     bus_number;
   method    Bit#(5)     device_number;
   method    Bit#(3)     function_number;
   method    Bit#(16)    status;
   method    Bit#(16)    command;
   method    Bit#(16)    dstatus;
   method    Bit#(16)    dcommand;
   method    Bit#(16)    dcommand2;
   method    Bit#(16)    lstatus;
   method    Bit#(16)    lcommand;
   method    Bit#(1)     to_turnoff_n;
   method    Action      turnoff_ok_n(Bit#(1) i);
   method    Action      pm_wake_n(Bit#(1) i);
   method    Bit#(3)     pcie_link_state_n;
   method    Action      trn_pending_n(Bit#(1) i);
   method    Action      dsn(Bit#(64) i);
   method    Bit#(1)     pmcsr_pme_en;
   method    Bit#(1)     pmcsr_pme_status;
   method    Bit#(2)     pmcsr_powerstate;
endinterface

(* always_ready, always_enabled *)
interface PCIE_INT_V6;
   method    Action      req_n(Bit#(1) i);
   method    Bit#(1)     rdy_n;
   method    Action      assert_n(Bit#(1) i);
   method    Action      di(Bit#(8) i);
   method    Bit#(8)     dout;
   method    Bit#(3)     mmenable;
   method    Bit#(1)     msienable;
   method    Bit#(1)     msixenable;
   method    Bit#(1)     msixfm;
endinterface

(* always_ready, always_enabled *)
interface PCIE_ERR_V6;
   method    Action      ecrc_n(Bit#(1) i);
   method    Action      ur_n(Bit#(1) i);
   method    Action      cpl_timeout_n(Bit#(1) i);
   method    Action      cpl_unexpect_n(Bit#(1) i);
   method    Action      cpl_abort_n(Bit#(1) i);
   method    Action      posted_n(Bit#(1) i);
   method    Action      cor_n(Bit#(1) i);
   method    Action      tlp_cpl_header(Bit#(48) i);
   method    Bit#(1)     cpl_rdy_n;
   method    Action      locked_n(Bit#(1) i);
endinterface

interface PCIE_V6#(numeric type lanes);
   interface PCIE_EXP#(lanes) pcie;
   interface PCIE_TRN_TX_V6   trn_tx;
   interface PCIE_TRN_RX_V6   trn_rx;
   interface PCIE_TRN_V6      trn;
   interface PCIE_PL_V6       pl;
   interface PCIE_CFG_V6      cfg;
   interface PCIE_INT_V6      cfg_interrupt;
   interface PCIE_ERR_V6      cfg_err;
endinterface: PCIE_V6

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" xilinx_v6_pcie_wrapper =
module vMkVirtex6PCIExpress#(PCIEParams params)(PCIE_V6#(lanes))
   provisos(Add#(1, z, lanes));

   default_clock clk(sys_clk);
   default_reset rstn(sys_reset_n);

   parameter PL_FAST_TRAIN = (params.fast_train_sim_only) ? "TRUE" : "FALSE";

   interface PCIE_EXP pcie;
      method                            rxp(pci_exp_rxp) enable((*inhigh*)en0)                              reset_by(no_reset);
      method                            rxn(pci_exp_rxn) enable((*inhigh*)en1)                              reset_by(no_reset);
      method pci_exp_txp                txp                                                                 reset_by(no_reset);
      method pci_exp_txn                txn                                                                 reset_by(no_reset);
   endinterface

   interface PCIE_TRN_V6 trn;
      output_clock                      clk(trn_clk);
      output_clock                      clk2(trn2_clk);
      output_reset                      reset_n(trn_reset_n)                                                clocked_by(trn_clk);
      method trn_lnk_up_n               lnk_up_n                                                            clocked_by(no_clock) reset_by(no_reset); /* semi-static */
      method trn_fc_ph                  fc_ph                                                               clocked_by(trn_clk)  reset_by(no_reset);
      method trn_fc_pd                  fc_pd                                                               clocked_by(trn_clk)  reset_by(no_reset);
      method trn_fc_nph                 fc_nph                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method trn_fc_npd                 fc_npd                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method trn_fc_cplh                fc_cplh                                                             clocked_by(trn_clk)  reset_by(no_reset);
      method trn_fc_cpld                fc_cpld                                                             clocked_by(trn_clk)  reset_by(no_reset);
      method                            fc_sel(trn_fc_sel)                           enable((*inhigh*)en01) clocked_by(trn_clk)  reset_by(no_reset);
   endinterface

   interface PCIE_TRN_TX_V6 trn_tx;
      method                            tsof_n(trn_tsof_n)                           enable((*inhigh*)en02) clocked_by(trn_clk)  reset_by(no_reset);
      method                            teof_n(trn_teof_n)                           enable((*inhigh*)en03) clocked_by(trn_clk)  reset_by(no_reset);
      method                            td(trn_td)                                   enable((*inhigh*)en04) clocked_by(trn_clk)  reset_by(no_reset);
      method                            trem_n(trn_trem_n)                           enable((*inhigh*)en05) clocked_by(trn_clk)  reset_by(no_reset);
      method                            tsrc_rdy_n(trn_tsrc_rdy_n)                   enable((*inhigh*)en06) clocked_by(trn_clk)  reset_by(no_reset);
      method trn_tdst_rdy_n             tdst_rdy_n                                                          clocked_by(trn_clk)  reset_by(no_reset);
      method                            tsrc_dsc_n(trn_tsrc_dsc_n)                   enable((*inhigh*)en07) clocked_by(trn_clk)  reset_by(no_reset);
      method trn_tbuf_av                tbuf_av                                                             clocked_by(trn_clk)  reset_by(no_reset);
      method trn_terr_drop_n            terr_drop_n                                                         clocked_by(trn_clk)  reset_by(no_reset);
      method                            tstr_n(trn_tstr_n)                           enable((*inhigh*)en08) clocked_by(trn_clk)  reset_by(no_reset);
      method trn_tcfg_req_n             tcfg_req_n                                                          clocked_by(trn_clk)  reset_by(no_reset);
      method                            tcfg_gnt_n(trn_tcfg_gnt_n)                   enable((*inhigh*)en09) clocked_by(trn_clk)  reset_by(no_reset);
      method                            terrfwd_n(trn_terrfwd_n)                     enable((*inhigh*)en10) clocked_by(trn_clk)  reset_by(no_reset);
   endinterface

   interface PCIE_TRN_RX_V6 trn_rx;
      method trn_rsof_n                 rsof_n                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method trn_reof_n                 reof_n                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rd                     rd                                                                  clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rrem_n                 rrem_n                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rerrfwd_n              rerrfwd_n                                                           clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rsrc_rdy_n             rsrc_rdy_n                                                          clocked_by(trn_clk)  reset_by(no_reset);
      method                            rdst_rdy_n(trn_rdst_rdy_n)                   enable((*inhigh*)en11) clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rsrc_dsc_n             rsrc_dsc_n                                                          clocked_by(trn_clk)  reset_by(no_reset);
      method                            rnp_ok_n(trn_rnp_ok_n)                       enable((*inhigh*)en12) clocked_by(trn_clk)  reset_by(no_reset);
      method trn_rbar_hit_n             rbar_hit_n                                                          clocked_by(trn_clk)  reset_by(no_reset);
   endinterface

   interface PCIE_PL_V6 pl;
      method pl_initial_link_width      initial_link_width                                                       clocked_by(trn_clk)  reset_by(no_reset);
      method pl_lane_reversal_mode      lane_reversal_mode                                                       clocked_by(trn_clk)  reset_by(no_reset);
      method pl_link_gen2_capable       link_gen2_capable                                                        clocked_by(trn_clk)  reset_by(no_reset);
      method pl_link_partner_gen2_supported link_partner_gen2_supported                                          clocked_by(trn_clk)  reset_by(no_reset);
      method pl_link_upcfg_capable      link_upcfg_capable                                                       clocked_by(trn_clk)  reset_by(no_reset);
      method pl_sel_link_rate           sel_link_rate                                                            clocked_by(trn_clk)  reset_by(no_reset);
      method pl_sel_link_width          sel_link_width                                                           clocked_by(trn_clk)  reset_by(no_reset);
      method pl_ltssm_state             ltssm_state                                                              clocked_by(trn_clk)  reset_by(no_reset);
      method                            directed_link_auton(pl_directed_link_auton)       enable((*inhigh*)en13) clocked_by(trn_clk)  reset_by(no_reset);
      method                            directed_link_change(pl_directed_link_change)     enable((*inhigh*)en14) clocked_by(trn_clk)  reset_by(no_reset);
      method                            directed_link_speed(pl_directed_link_speed)       enable((*inhigh*)en15) clocked_by(trn_clk)  reset_by(no_reset);
      method                            directed_link_width(pl_directed_link_width)       enable((*inhigh*)en16) clocked_by(trn_clk)  reset_by(no_reset);
      method                            upstream_prefer_deemph(pl_upstream_prefer_deemph) enable((*inhigh*)en17) clocked_by(trn_clk)  reset_by(no_reset);
      method pl_received_hot_rst        received_hot_rst                                                         clocked_by(trn_clk)  reset_by(no_reset);
   endinterface

   interface PCIE_CFG_V6 cfg;
      method cfg_do                     dout                                                                     clocked_by(trn_clk) reset_by(no_reset);
      method cfg_rd_wr_done_n           rd_wr_done_n                                                             clocked_by(trn_clk) reset_by(no_reset);
      method                            di(cfg_di)                                        enable((*inhigh*)en18) clocked_by(trn_clk) reset_by(no_reset);
      method                            dwaddr(cfg_dwaddr)                                enable((*inhigh*)en19) clocked_by(trn_clk) reset_by(no_reset);
      method                            byte_en_n(cfg_byte_en_n)                          enable((*inhigh*)en20) clocked_by(trn_clk) reset_by(no_reset);
      method                            wr_en_n(cfg_wr_en_n)                              enable((*inhigh*)en21) clocked_by(trn_clk) reset_by(no_reset);
      method                            rd_en_n(cfg_rd_en_n)                              enable((*inhigh*)en22) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_bus_number             bus_number                                                               clocked_by(no_clock) reset_by(no_reset);
      method cfg_device_number          device_number                                                            clocked_by(no_clock) reset_by(no_reset);
      method cfg_function_number        function_number                                                          clocked_by(no_clock) reset_by(no_reset);
      method cfg_status                 status                                                                   clocked_by(trn_clk) reset_by(no_reset);
      method cfg_command                command                                                                  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dstatus                dstatus                                                                  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dcommand               dcommand                                                                 clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dcommand2              dcommand2                                                                clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lstatus                lstatus                                                                  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lcommand               lcommand                                                                 clocked_by(trn_clk) reset_by(no_reset);
      method cfg_to_turnoff_n           to_turnoff_n                                                             clocked_by(trn_clk) reset_by(no_reset);
      method                            turnoff_ok_n(cfg_turnoff_ok_n)                    enable((*inhigh*)en23) clocked_by(trn_clk) reset_by(no_reset);
      method                            pm_wake_n(cfg_pm_wake_n)                          enable((*inhigh*)en24) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pcie_link_state_n      pcie_link_state_n                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            trn_pending_n(cfg_trn_pending_n)                  enable((*inhigh*)en25) clocked_by(trn_clk) reset_by(no_reset);
      method                            dsn(cfg_dsn)                                      enable((*inhigh*)en26) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pmcsr_pme_en           pmcsr_pme_en                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pmcsr_pme_status       pmcsr_pme_status                                                         clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pmcsr_powerstate       pmcsr_powerstate                                                         clocked_by(trn_clk) reset_by(no_reset);
   endinterface

   interface PCIE_INT_V6 cfg_interrupt;
      method                            req_n(cfg_interrupt_n)                            enable((*inhigh*)en27) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_rdy_n        rdy_n                                                                    clocked_by(trn_clk) reset_by(no_reset);
      method                            assert_n(cfg_interrupt_assert_n)                  enable((*inhigh*)en28) clocked_by(trn_clk) reset_by(no_reset);
      method                            di(cfg_interrupt_di)                              enable((*inhigh*)en29) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_do           dout                                                                     clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_mmenable     mmenable                                                                 clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_msienable    msienable                                                                clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_msixenable   msixenable                                                               clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_msixfm       msixfm                                                                   clocked_by(trn_clk) reset_by(no_reset);
   endinterface

   interface PCIE_ERR_V6 cfg_err;
      method                            ecrc_n(cfg_err_ecrc_n)                       enable((*inhigh*)en30) clocked_by(trn_clk) reset_by(no_reset);
      method                            ur_n(cfg_err_ur_n)                           enable((*inhigh*)en31) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_timeout_n(cfg_err_cpl_timeout_n)         enable((*inhigh*)en32) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_unexpect_n(cfg_err_cpl_unexpect_n)       enable((*inhigh*)en33) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_abort_n(cfg_err_cpl_abort_n)             enable((*inhigh*)en34) clocked_by(trn_clk) reset_by(no_reset);
      method                            posted_n(cfg_err_posted_n)                   enable((*inhigh*)en35) clocked_by(trn_clk) reset_by(no_reset);
      method                            cor_n(cfg_err_cor_n)                         enable((*inhigh*)en36) clocked_by(trn_clk) reset_by(no_reset);
      method                            tlp_cpl_header(cfg_err_tlp_cpl_header)       enable((*inhigh*)en37) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_err_cpl_rdy_n          cpl_rdy_n                                                           clocked_by(trn_clk) reset_by(no_reset);
      method                            locked_n(cfg_err_locked_n)                   enable((*inhigh*)en38) clocked_by(trn_clk) reset_by(no_reset);
   endinterface

   schedule (
             pcie_rxp, pcie_rxn, pcie_txp, pcie_txn,
             trn_lnk_up_n, trn_fc_ph, trn_fc_pd, trn_fc_nph, trn_fc_npd, trn_fc_cplh, trn_fc_cpld, trn_fc_sel,
             trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n,
             trn_tx_tsrc_dsc_n, trn_tx_tbuf_av, trn_tx_terr_drop_n, trn_tx_tstr_n, trn_tx_tcfg_req_n, trn_tx_tcfg_gnt_n,
             trn_tx_terrfwd_n, trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n,
             trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rbar_hit_n,
             pl_initial_link_width, pl_lane_reversal_mode, pl_link_gen2_capable, pl_link_partner_gen2_supported,
             pl_link_upcfg_capable, pl_sel_link_rate, pl_sel_link_width, pl_ltssm_state, pl_directed_link_auton,
             pl_directed_link_change, pl_directed_link_speed, pl_directed_link_width, pl_upstream_prefer_deemph,
             pl_received_hot_rst, cfg_dout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_byte_en_n, cfg_wr_en_n,
             cfg_rd_en_n, cfg_bus_number, cfg_device_number, cfg_function_number, cfg_status, cfg_command,
             cfg_dstatus, cfg_dcommand, cfg_dcommand2, cfg_lstatus, cfg_lcommand, cfg_to_turnoff_n,
             cfg_turnoff_ok_n, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn, cfg_pmcsr_pme_en,
             cfg_pmcsr_pme_status, cfg_pmcsr_powerstate, cfg_interrupt_req_n, cfg_interrupt_rdy_n,
             cfg_interrupt_assert_n, cfg_interrupt_di, cfg_interrupt_dout, cfg_interrupt_mmenable,
             cfg_interrupt_msienable, cfg_interrupt_msixenable, cfg_interrupt_msixfm, cfg_err_ecrc_n, cfg_err_ur_n,
             cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n,
             cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n
             )
            CF
            (
             pcie_rxp, pcie_rxn, pcie_txp, pcie_txn,
             trn_lnk_up_n, trn_fc_ph, trn_fc_pd, trn_fc_nph, trn_fc_npd, trn_fc_cplh, trn_fc_cpld, trn_fc_sel,
             trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n,
             trn_tx_tsrc_dsc_n, trn_tx_tbuf_av, trn_tx_terr_drop_n, trn_tx_tstr_n, trn_tx_tcfg_req_n, trn_tx_tcfg_gnt_n,
             trn_tx_terrfwd_n, trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n,
             trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rbar_hit_n,
             pl_initial_link_width, pl_lane_reversal_mode, pl_link_gen2_capable, pl_link_partner_gen2_supported,
             pl_link_upcfg_capable, pl_sel_link_rate, pl_sel_link_width, pl_ltssm_state, pl_directed_link_auton,
             pl_directed_link_change, pl_directed_link_speed, pl_directed_link_width, pl_upstream_prefer_deemph,
             pl_received_hot_rst, cfg_dout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_byte_en_n, cfg_wr_en_n,
             cfg_rd_en_n, cfg_bus_number, cfg_device_number, cfg_function_number, cfg_status, cfg_command,
             cfg_dstatus, cfg_dcommand, cfg_dcommand2, cfg_lstatus, cfg_lcommand, cfg_to_turnoff_n,
             cfg_turnoff_ok_n, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn, cfg_pmcsr_pme_en,
             cfg_pmcsr_pme_status, cfg_pmcsr_powerstate, cfg_interrupt_req_n, cfg_interrupt_rdy_n,
             cfg_interrupt_assert_n, cfg_interrupt_di, cfg_interrupt_dout, cfg_interrupt_mmenable,
             cfg_interrupt_msienable, cfg_interrupt_msixenable, cfg_interrupt_msixfm, cfg_err_ecrc_n, cfg_err_ur_n,
             cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n,
             cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n
             );

endmodule: vMkVirtex6PCIExpress

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
interface PCIE_TRN_COMMON_V6;
   interface Clock       clk;
   interface Clock       clk2;
   interface Reset       reset_n;
   method    Bool        link_up;
endinterface

interface PCIE_TRN_XMIT_V6;
   method    Action      xmit(Bool discontinue, TLPData#(8) data);
   method    Bool        dropped;
   method    Bit#(6)     buffers_available;
   method    Action      cut_through_mode(Bool i);
   method    Bool        configuration_completion_ready;
   method    Action      configuration_completion_grant(Bool i);
   method    Action      error_forward(Bool i);
endinterface

interface PCIE_TRN_RECV_V6;
   method    ActionValue#(TLPData#(8)) recv();
   method    Bool        error_forward;
   method    Bool        source_discontinue;
   method    Action      non_posted_ready(Bool i);
endinterface


interface PCIExpressV6#(numeric type lanes);
   interface PCIE_EXP#(lanes)   pcie;
   interface PCIE_TRN_COMMON_V6 trn;
   interface PCIE_TRN_XMIT_V6   trn_tx;
   interface PCIE_TRN_RECV_V6   trn_rx;
   interface PCIE_CFG_V6        cfg;
   interface PCIE_INT_V6        cfg_interrupt;
   interface PCIE_ERR_V6        cfg_err;
   interface PCIE_PL_V6         pl;
   interface XilinxClkServer    clks;
   interface Clock              scemi_clk;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
typeclass SelectVirtex6PCIE#(numeric type lanes);
   module selectVirtex6PCIE(PCIEParams params, PCIE_V6#(lanes) ifc);
endtypeclass

instance SelectVirtex6PCIE#(8);
   module selectVirtex6PCIE(PCIEParams params, PCIE_V6#(8) ifc);
      let _ifc <- vMkVirtex6PCIExpress(params);
      return _ifc;
   endmodule
endinstance

module mkPCIExpressEndpointV6#(PCIEParams params)(PCIExpressV6#(lanes))
   provisos(Add#(1, z, lanes), SelectVirtex6PCIE#(lanes));

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   PCIE_V6#(lanes)                           pcie_ep             <- selectVirtex6PCIE(params);

   Clock                                     trnclk               = pcie_ep.trn.clk;    // 250 MHz
   Clock                                     trn2clk              = pcie_ep.trn.clk2;   // 125 MHz trn_clk/2
   Reset                                     trnrst_n             = pcie_ep.trn.reset_n;

   PulseWire                                 pwTrnTx             <- mkPulseWire(clocked_by trnclk, reset_by noReset);
   PulseWire                                 pwTrnRx             <- mkPulseWire(clocked_by trnclk, reset_by noReset);
   
   Wire#(Bit#(1))                            wTSofN              <- mkDWire(1, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(1))                            wTEofN              <- mkDWire(1, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(1))                            wTSrcDscN           <- mkDWire(1, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(1))                            wTRemN              <- mkDWire(1, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(64))                           wTd                 <- mkDWire(0, clocked_by trnclk, reset_by noReset);

   Reset                                     trn2rst_n           <- mkAsyncReset(1, trnrst_n, trn2clk);
   
   XilinxClockParams                         scemiclk_params      = defaultValue;
   scemiclk_params.clkin1_period    = 8.000;
   scemiclk_params.clkfbout_mult_f  = 8.000;
   scemiclk_params.clkout0_divide_f = params.clock_period;
   XilinxClockController                     scemi_clkgen        <- mkXilinxClockController(scemiclk_params, trn2clk, clocked_by trn2clk, reset_by trn2rst_n);
   
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule connect_trn_tx;
      pcie_ep.trn_tx.tsrc_rdy_n(pack(!pwTrnTx));
   endrule

   rule connect_trn_rx;
      pcie_ep.trn_rx.rdst_rdy_n(pack(!pwTrnRx));
   endrule
   
   rule tie_off_fc_sel;
      pcie_ep.trn.fc_sel(RECEIVE_BUFFER_AVAILABLE_SPACE);
   endrule
   
   rule transmit_connection;
      pcie_ep.trn_tx.tsof_n(wTSofN);
      pcie_ep.trn_tx.teof_n(wTEofN);
      pcie_ep.trn_tx.tsrc_dsc_n(wTSrcDscN);
      pcie_ep.trn_tx.trem_n(wTRemN);
      pcie_ep.trn_tx.td(wTd);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface pcie       = pcie_ep.pcie;

   interface PCIE_TRN_COMMON_V6 trn;
      interface clk           = pcie_ep.trn.clk;
      interface clk2          = pcie_ep.trn.clk2;
      interface reset_n       = pcie_ep.trn.reset_n;
      method Bool link_up     = (pcie_ep.trn.lnk_up_n == 0);
   endinterface

   interface PCIE_TRN_XMIT_V6 trn_tx;
      method Action xmit(discontinue, data) if (pcie_ep.trn_tx.tdst_rdy_n == 0);
	 wTSofN    <= pack(!data.sof);
	 wTEofN    <= pack(!data.eof);
	 wTSrcDscN <= pack(!discontinue);
	 wTRemN    <= pack(data.be != '1);
	 wTd       <= data.data;
         pwTrnTx.send;
      endmethod
      method dropped                           = (pcie_ep.trn_tx.terr_drop_n == 0);
      method buffers_available                 = pcie_ep.trn_tx.tbuf_av;
      method cut_through_mode(i)               = pcie_ep.trn_tx.tstr_n(pack(!i));
      method configuration_completion_ready    = (pcie_ep.trn_tx.tcfg_req_n == 0);
      method configuration_completion_grant(i) = pcie_ep.trn_tx.tcfg_gnt_n(pack(!i));
      method error_forward(i)                  = pcie_ep.trn_tx.terrfwd_n(pack(!i));
   endinterface

   interface PCIE_TRN_RECV_V6 trn_rx;
      method ActionValue#(TLPData#(8)) recv() if (pcie_ep.trn_rx.rsrc_rdy_n == 0);
         TLPData#(8) retval = defaultValue;
         retval.sof  = (pcie_ep.trn_rx.rsof_n == 0);
         retval.eof  = (pcie_ep.trn_rx.reof_n == 0);
         retval.hit  = ~pcie_ep.trn_rx.rbar_hit_n;
         retval.be   = {4'b1111, ~pack(replicate(pcie_ep.trn_rx.rrem_n))};
         retval.data = pcie_ep.trn_rx.rd;
         pwTrnRx.send;
         return retval;
      endmethod
      method error_forward         = (pcie_ep.trn_rx.rerrfwd_n == 0);
      method source_discontinue    = (pcie_ep.trn_rx.rsrc_dsc_n == 0);
      method non_posted_ready(i)   = pcie_ep.trn_rx.rnp_ok_n(pack(!i));
   endinterface

   interface pl            = pcie_ep.pl;
   interface cfg           = pcie_ep.cfg;
   interface cfg_interrupt = pcie_ep.cfg_interrupt;
   interface cfg_err       = pcie_ep.cfg_err;
   interface scemi_clk = scemi_clkgen.clkout0;
   interface XilinxClkServer clks;
      interface Put request;
	 method Action put(Bit#(32) x);
	    let request = XilinxClockRequest {
	       rnw:  unpack(x[31]),
	       addr: x[20:16],
	       data: x[15:0]
	       };
	    scemi_clkgen.csr.request.put(request);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue#(Bit#(32)) get;
	    let response <- scemi_clkgen.csr.response.get;
	    return cExtend(response);
	 endmethod
      endinterface
   endinterface
endmodule: mkPCIExpressEndpointV6

////////////////////////////////////////////////////////////////////////////////
/// Connection Instances
////////////////////////////////////////////////////////////////////////////////

// Basic TLPData#(8) connections to PCIE endpoint

instance Connectable#(Get#(TLPData#(8)), PCIE_TRN_XMIT_V6);
   module mkConnection#(Get#(TLPData#(8)) g, PCIE_TRN_XMIT_V6 p)(Empty);
      rule every;
         p.cut_through_mode(False);
         p.configuration_completion_grant(True);  // Core gets to choose
         p.error_forward(False);
      endrule
      rule connect;
         let data <- g.get;
         p.xmit(False, data);
      endrule
   endmodule
endinstance

instance Connectable#(PCIE_TRN_XMIT_V6, Get#(TLPData#(8)));
   module mkConnection#(PCIE_TRN_XMIT_V6 p, Get#(TLPData#(8)) g)(Empty);
      mkConnection(g, p);
   endmodule
endinstance

instance Connectable#(Put#(TLPData#(8)), PCIE_TRN_RECV_V6);
   module mkConnection#(Put#(TLPData#(8)) p, PCIE_TRN_RECV_V6 r)(Empty);
      (* no_implicit_conditions, fire_when_enabled *)
      rule every;
         r.non_posted_ready(True);
      endrule
      rule connect;
         let data <- r.recv;
         p.put(data);
      endrule
   endmodule
endinstance

instance Connectable#(PCIE_TRN_RECV_V6, Put#(TLPData#(8)));
   module mkConnection#(PCIE_TRN_RECV_V6 r, Put#(TLPData#(8)) p)(Empty);
      mkConnection(p, r);
   endmodule
endinstance

// Connections between TLPData#(16) and a PCIE endpoint.
// These are all using the same clock, so the TLPData#(16) accesses
// will not be back-to-back.

instance Connectable#(Get#(TLPData#(16)), PCIE_TRN_XMIT_V6);
   module mkConnection#(Get#(TLPData#(16)) g, PCIE_TRN_XMIT_V6 t)(Empty);
      FIFO#(TLPData#(8)) outFifo <- mkFIFO();

      (* no_implicit_conditions, fire_when_enabled *)
      rule every;
         t.cut_through_mode(False);
         t.configuration_completion_grant(True);  // True means core gets to choose
         t.error_forward(False);
      endrule

      rule connect;
         let data = outFifo.first; outFifo.deq;
         if (data.be != 0)
            t.xmit(False, data);
      endrule

      Put#(TLPData#(8)) p = fifoToPut(outFifo);
      mkConnection(g,p);
   endmodule
endinstance

instance Connectable#(PCIE_TRN_XMIT_V6, Get#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_XMIT_V6 p, Get#(TLPData#(16)) g)(Empty);
      mkConnection(g, p);
   endmodule
endinstance

instance Connectable#(Put#(TLPData#(16)), PCIE_TRN_RECV_V6);
   module mkConnection#(Put#(TLPData#(16)) p, PCIE_TRN_RECV_V6 r)(Empty);
      FIFO#(TLPData#(8)) inFifo <- mkFIFO();

      (* no_implicit_conditions, fire_when_enabled *)
      rule every;
         r.non_posted_ready(True);
      endrule

      rule connect;
         let data <- r.recv;
         inFifo.enq(data);
      endrule

      Get#(TLPData#(8)) g = fifoToGet(inFifo);
      mkConnection(g,p);
   endmodule
endinstance

instance Connectable#(PCIE_TRN_RECV_V6, Put#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_RECV_V6 r, Put#(TLPData#(16)) p)(Empty);
      mkConnection(p, r);
   endmodule
endinstance

// Connections between TLPData#(16) and a PCIE endpoint, using a gearbox
// to match data rates between the endpoint and design clocks.

instance ConnectableWithClocks#(PCIE_TRN_XMIT_V6, Get#(TLPData#(16)));
   module mkConnectionWithClocks#(PCIE_TRN_XMIT_V6 p, Get#(TLPData#(16)) g,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);

      ////////////////////////////////////////////////////////////////////////////////
      /// Design Elements
      ////////////////////////////////////////////////////////////////////////////////
      FIFO#(TLPData#(8))                     outFifo             <- mkFIFO(clocked_by fastClock, reset_by fastReset);
      Gearbox#(2, 1, TLPData#(8))            fifoTxData          <- mkNto1Gearbox(slowClock, slowReset, fastClock, fastReset);

      ////////////////////////////////////////////////////////////////////////////////
      /// Rules
      ////////////////////////////////////////////////////////////////////////////////
      (* no_implicit_conditions, fire_when_enabled *)
      rule every;
         p.cut_through_mode(False);
         p.configuration_completion_grant(True);  // Means the core gets to choose
         p.error_forward(False);
      endrule

      rule get_data;
         function Vector#(2, TLPData#(8)) split(TLPData#(16) in);
            Vector#(2, TLPData#(8)) v = defaultValue;
            v[0].sof  = in.sof;
            v[0].eof  = (in.be[7:0] == 0) ? in.eof : False;
            v[0].hit  = in.hit;
            v[0].be   = in.be[15:8];
            v[0].data = in.data[127:64];
            v[1].sof  = False;
            v[1].eof  = in.eof;
            v[1].hit  = in.hit;
            v[1].be   = in.be[7:0];
            v[1].data = in.data[63:0];
            return v;
         endfunction

         let data <- g.get;
         fifoTxData.enq(split(data));
      endrule

      rule process_outgoing_packets;
         let data = fifoTxData.first; fifoTxData.deq;
         outFifo.enq(head(data));
      endrule

      rule send_data;
         let data = outFifo.first; outFifo.deq;
         // filter out TLPs with 00 byte enable
         if (data.be != 0)
            p.xmit(False, data);
      endrule

   endmodule
endinstance

instance ConnectableWithClocks#(Get#(TLPData#(16)), PCIE_TRN_XMIT_V6);
   module mkConnectionWithClocks#(Get#(TLPData#(16)) g, PCIE_TRN_XMIT_V6 p,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);

      mkConnectionWithClocks(p, g, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

instance ConnectableWithClocks#(Put#(TLPData#(16)), PCIE_TRN_RECV_V6);
   module mkConnectionWithClocks#(Put#(TLPData#(16)) p, PCIE_TRN_RECV_V6 g,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);

      ////////////////////////////////////////////////////////////////////////////////
      /// Design Elements
      ////////////////////////////////////////////////////////////////////////////////
      FIFO#(TLPData#(8))                        inFifo              <- mkFIFO(clocked_by fastClock, reset_by fastReset);
      Gearbox#(1, 2, TLPData#(8))               fifoRxData          <- mk1toNGearbox(fastClock, fastReset, slowClock, slowReset);

      Reg#(Bool)                                rOddBeat            <- mkRegA(False, clocked_by fastClock, reset_by fastReset);
      Reg#(Bool)                                rSendInvalid        <- mkRegA(False, clocked_by fastClock, reset_by fastReset);

      ////////////////////////////////////////////////////////////////////////////////
      /// Rules
      ////////////////////////////////////////////////////////////////////////////////
      (* no_implicit_conditions, fire_when_enabled *)
      rule every;
         g.non_posted_ready(True);
      endrule

      rule accept_data;
         let data <- g.recv;
         inFifo.enq(data);
      endrule

      rule process_incoming_packets(!rSendInvalid);
         let data = inFifo.first; inFifo.deq;
         rOddBeat     <= !rOddBeat;
         rSendInvalid <= !rOddBeat && data.eof;
         Vector#(1, TLPData#(8)) v = defaultValue;
         v[0] = data;
         fifoRxData.enq(v);
      endrule

      rule send_invalid_packets(rSendInvalid);
         rOddBeat     <= !rOddBeat;
         rSendInvalid <= False;
         Vector#(1, TLPData#(8)) v = defaultValue;
         v[0].eof = True;
         v[0].be  = 0;
         fifoRxData.enq(v);
      endrule

      rule send_data;
         function TLPData#(16) combine(Vector#(2, TLPData#(8)) in);
            return TLPData {
                            sof:   in[0].sof,
                            eof:   in[1].eof,
                            hit:   in[0].hit,
                            be:    { in[0].be,   in[1].be },
                            data:  { in[0].data, in[1].data }
                            };
         endfunction

         fifoRxData.deq;
         p.put(combine(fifoRxData.first));
      endrule

   endmodule
endinstance

instance ConnectableWithClocks#(PCIE_TRN_RECV_V6, Put#(TLPData#(16)));
   module mkConnectionWithClocks#(PCIE_TRN_RECV_V6 g, Put#(TLPData#(16)) p,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);
      mkConnectionWithClocks(p, g, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// interface tie-offs

instance TieOff#(PCIE_CFG_V6);
   module mkTieOff#(PCIE_CFG_V6 ifc)(Empty);
      rule tie_off_inputs;
         ifc.di('0);
         ifc.dwaddr('0);
         ifc.byte_en_n('1);
         ifc.wr_en_n('1);
         ifc.rd_en_n('1);
         ifc.turnoff_ok_n('1);
         ifc.pm_wake_n('1);
         ifc.trn_pending_n('1);
         ifc.dsn({ 32'h0000_0001, { { 8'h1}, 24'h000A35}});
      endrule
   endmodule
endinstance

instance TieOff#(PCIE_INT_V6);
   module mkTieOff#(PCIE_INT_V6 ifc)(Empty);
      rule tie_off_inputs;
         ifc.req_n('1);
         ifc.assert_n('1);
         ifc.di('0);
      endrule
   endmodule
endinstance

instance TieOff#(PCIE_ERR_V6);
   module mkTieOff#(PCIE_ERR_V6 ifc)(Empty);
      rule tie_off_inputs;
         ifc.ecrc_n('1);
         ifc.ur_n('1);
         ifc.cpl_timeout_n('1);
         ifc.cpl_unexpect_n('1);
         ifc.cpl_abort_n('1);
         ifc.posted_n('1);
         ifc.cor_n('1);
         ifc.tlp_cpl_header('0);
         ifc.locked_n('1);
      endrule
   endmodule
endinstance

instance TieOff#(PCIE_PL_V6);
   module mkTieOff#(PCIE_PL_V6 ifc)(Empty);
      rule tie_off_inputs;
         ifc.directed_link_auton('0);
         ifc.directed_link_change('0);
         ifc.directed_link_speed('0);
         ifc.directed_link_width('1);
         ifc.upstream_prefer_deemph('0);
      endrule
   endmodule
endinstance

endpackage: XilinxVirtex6PCIE

