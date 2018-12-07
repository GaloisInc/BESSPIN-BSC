////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtex5PCIE.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtex5PCIE;

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

import PCIE              ::*;

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_ready, always_enabled *)
interface PCIE_CFG;
   method    Bit#(32)    dataout;
   method    Bit#(1)     rd_wr_done_n;
   method    Action      di(Bit#(32) i);
   method    Action      dwaddr(Bit#(10) i);
   method    Action      wr_en_n(Bit#(1) i);
   method    Action      rd_en_n(Bit#(1) i);
   method    Bit#(1)     to_turnoff_n;
   method    Action      byte_en_n(Bit#(4) i);
   method    Bit#(8)     bus_number;
   method    Bit#(5)     device_number;
   method    Bit#(3)     function_number;
   method    Bit#(16)    status;
   method    Bit#(16)    command;
   method    Bit#(16)    dstatus;
   method    Bit#(16)    dcommand;
   method    Bit#(16)    lstatus;
   method    Bit#(16)    lcommand;
   method    Action      pm_wake_n(Bit#(1) i);
   method    Bit#(3)     pcie_link_state_n;
   method    Action      trn_pending_n(Bit#(1) i);
   method    Action      dsn(Bit#(64) i);
endinterface: PCIE_CFG

(* always_ready, always_enabled *)
interface PCIE_INT;
   method    Action      interrupt_n(Bit#(1) i);
   method    Bit#(1)     interrupt_rdy_n;
   method    Bit#(3)     interrupt_mmenable;
   method    Bit#(1)     interrupt_msienable;
   method    Action      interrupt_di(Bit#(8) i);
   method    Bit#(8)     interrupt_do;
   method    Action      interrupt_assert_n(Bit#(1) i);
endinterface: PCIE_INT

(* always_ready, always_enabled *)
interface PCIE_ERR;
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
endinterface: PCIE_ERR

(* always_ready, always_enabled *)
interface PCIE_TRN_RX;
   method    Bit#(1)     rsof_n;
   method    Bit#(1)     reof_n;
   method    Bit#(64)    rd;
   method    Bit#(8)     rrem_n;
   method    Bit#(1)     rerrfwd_n;
   method    Bit#(1)     rsrc_rdy_n;
   method    Action      rdst_rdy_n(Bit#(1) i);
   method    Bit#(1)     rsrc_dsc_n;
   method    Action      rnp_ok_n(Bit#(1) i);
   method    Action      rcpl_streaming_n(Bit#(1) i);
   method    Bit#(7)     rbar_hit_n;
   method    Bit#(8)     rfc_ph_av;
   method    Bit#(12)    rfc_pd_av;
   method    Bit#(8)     rfc_nph_av;
   method    Bit#(12)    rfc_npd_av;
endinterface: PCIE_TRN_RX

(* always_ready, always_enabled *)
interface PCIE_TRN;
   interface Clock       clk;
   interface Clock       clk2;
   interface Reset       reset_n;
   method    Bit#(1)     lnk_up_n;
endinterface: PCIE_TRN

(* always_ready, always_enabled *)
interface PCIE_TRN_TX;
   method    Action      tsof_n(Bit#(1) i);
   method    Action      teof_n(Bit#(1) i);
   method    Action      td(Bit#(64) i);
   method    Action      trem_n(Bit#(8) i);
   method    Action      tsrc_rdy_n(Bit#(1) i);
   method    Bit#(1)     tdst_rdy_n;
   method    Action      tsrc_dsc_n(Bit#(1) i);
   method    Bit#(1)     tdst_dsc_n;
   method    Bit#(4)     tbuf_av;
   method    Action      terrfwd_n(Bit#(1) i);
endinterface: PCIE_TRN_TX

interface PCIE#(numeric type lanes);
   interface Clock            clkout;
   interface PCIE_EXP#(lanes) pcie;
   interface PCIE_CFG         cfg;
   interface PCIE_INT         cfg_irq;
   interface PCIE_ERR         cfg_err;
   interface PCIE_TRN_TX      trn_tx;
   interface PCIE_TRN_RX      trn_rx;
   interface PCIE_TRN         trn;
endinterface: PCIE

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" xilinx_v5_pcie_wrapper =
module vMkVirtex5PCIExpressWithDCM#(PCIEParams params)(PCIE#(lanes))
   provisos(Add#(1, z, lanes));

   default_clock clk(sys_clk);
   default_reset rstn(sys_reset_n);

   port fast_train_simulation_only = params.fast_train_sim_only;

   output_clock  clkout(refclkout);

   interface PCIE_EXP pcie;
      method                  rxp(pci_exp_rxp) enable((*inhigh*)en0) reset_by(no_reset);
      method                  rxn(pci_exp_rxn) enable((*inhigh*)en1) reset_by(no_reset);
      method pci_exp_txp      txp                                    reset_by(no_reset);
      method pci_exp_txn      txn                                    reset_by(no_reset);
   endinterface: pcie

   interface PCIE_CFG cfg;
      method cfg_do                     dataout                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_rd_wr_done_n           rd_wr_done_n                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            di(cfg_di)                                   enable((*inhigh*)en2)  clocked_by(trn_clk) reset_by(no_reset);
      method                            dwaddr(cfg_dwaddr)                           enable((*inhigh*)en3)  clocked_by(trn_clk) reset_by(no_reset);
      method                            wr_en_n(cfg_wr_en_n)                         enable((*inhigh*)en4)  clocked_by(trn_clk) reset_by(no_reset);
      method                            rd_en_n(cfg_rd_en_n)                         enable((*inhigh*)en5)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_to_turnoff_n           to_turnoff_n                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            byte_en_n(cfg_byte_en_n)                     enable((*inhigh*)en9)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_bus_number             bus_number                                                          clocked_by(no_clock) reset_by(no_reset);
      method cfg_device_number          device_number                                                       clocked_by(no_clock) reset_by(no_reset);
      method cfg_function_number        function_number                                                     clocked_by(no_clock) reset_by(no_reset);
      method cfg_status                 status                                                              clocked_by(trn_clk) reset_by(no_reset);
      method cfg_command                command                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dstatus                dstatus                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dcommand               dcommand                                                            clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lstatus                lstatus                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lcommand               lcommand                                                            clocked_by(trn_clk) reset_by(no_reset);
      method                            pm_wake_n(cfg_pm_wake_n)                     enable((*inhigh*)en10) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pcie_link_state_n      pcie_link_state_n                                                   clocked_by(trn_clk) reset_by(no_reset);
      method                            trn_pending_n(cfg_trn_pending_n)             enable((*inhigh*)en11) clocked_by(trn_clk) reset_by(no_reset);
      method                            dsn(cfg_dsn)                                 enable((*inhigh*)en12) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg

   interface PCIE_INT cfg_irq;
      method                            interrupt_n(cfg_interrupt_n)                 enable((*inhigh*)en6)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_rdy_n        interrupt_rdy_n                                                     clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_mmenable     interrupt_mmenable                                                  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_msienable    interrupt_msienable                                                 clocked_by(trn_clk) reset_by(no_reset);
      method                            interrupt_di(cfg_interrupt_di)               enable((*inhigh*)en7)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_do           interrupt_do                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            interrupt_assert_n(cfg_interrupt_assert_n)   enable((*inhigh*)en8)  clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg_irq

   interface PCIE_ERR cfg_err;
      method                            ecrc_n(cfg_err_ecrc_n)                       enable((*inhigh*)en13) clocked_by(trn_clk) reset_by(no_reset);
      method                            ur_n(cfg_err_ur_n)                           enable((*inhigh*)en14) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_timeout_n(cfg_err_cpl_timeout_n)         enable((*inhigh*)en15) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_unexpect_n(cfg_err_cpl_unexpect_n)       enable((*inhigh*)en16) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_abort_n(cfg_err_cpl_abort_n)             enable((*inhigh*)en17) clocked_by(trn_clk) reset_by(no_reset);
      method                            posted_n(cfg_err_posted_n)                   enable((*inhigh*)en18) clocked_by(trn_clk) reset_by(no_reset);
      method                            cor_n(cfg_err_cor_n)                         enable((*inhigh*)en19) clocked_by(trn_clk) reset_by(no_reset);
      method                            tlp_cpl_header(cfg_err_tlp_cpl_header)       enable((*inhigh*)en20) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_err_cpl_rdy_n          cpl_rdy_n                                                           clocked_by(trn_clk) reset_by(no_reset);
      method                            locked_n(cfg_err_locked_n)                   enable((*inhigh*)en21) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg_err

   interface PCIE_TRN trn;
      output_clock                      clk(trn_clk);
      output_clock                      clk2(trn2_clk);
      output_reset                      reset_n(trn_reset_n)                                                clocked_by(trn_clk);
      method trn_lnk_up_n               lnk_up_n                                                            clocked_by(no_clock) reset_by(no_reset); /* semi-static */
   endinterface: trn

   interface PCIE_TRN_TX trn_tx;
      method                            tsof_n(trn_tsof_n)                           enable((*inhigh*)en22) clocked_by(trn_clk) reset_by(no_reset);
      method                            teof_n(trn_teof_n)                           enable((*inhigh*)en23) clocked_by(trn_clk) reset_by(no_reset);
      method                            td(trn_td)                                   enable((*inhigh*)en24) clocked_by(trn_clk) reset_by(no_reset);
      method                            trem_n(trn_trem_n)                           enable((*inhigh*)en25) clocked_by(trn_clk) reset_by(no_reset);
      method                            tsrc_rdy_n(trn_tsrc_rdy_n)                   enable((*inhigh*)en26) clocked_by(trn_clk) reset_by(no_reset);
      method trn_tdst_rdy_n             tdst_rdy_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            tsrc_dsc_n(trn_tsrc_dsc_n)                   enable((*inhigh*)en27) clocked_by(trn_clk) reset_by(no_reset);
      method trn_tdst_dsc_n             tdst_dsc_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_tbuf_av                tbuf_av                                                             clocked_by(trn_clk) reset_by(no_reset);
      method                            terrfwd_n(trn_terrfwd_n)                     enable((*inhigh*)en31) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: trn_tx

   interface PCIE_TRN_RX trn_rx;
      method trn_rsof_n                 rsof_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_reof_n                 reof_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_rd                     rd                                                                  clocked_by(trn_clk) reset_by(no_reset);
      method trn_rrem_n                 rrem_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_rerrfwd_n              rerrfwd_n                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rsrc_rdy_n             rsrc_rdy_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            rdst_rdy_n(trn_rdst_rdy_n)                   enable((*inhigh*)en28) clocked_by(trn_clk) reset_by(no_reset);
      method trn_rsrc_dsc_n             rsrc_dsc_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            rnp_ok_n(trn_rnp_ok_n)                       enable((*inhigh*)en29) clocked_by(trn_clk) reset_by(no_reset);
      method                            rcpl_streaming_n(trn_rcpl_streaming_n)       enable((*inhigh*)en30) clocked_by(trn_clk) reset_by(no_reset);
      method trn_rbar_hit_n             rbar_hit_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_ph_av              rfc_ph_av                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_pd_av              rfc_pd_av                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_nph_av             rfc_nph_av                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_npd_av             rfc_npd_av                                                          clocked_by(trn_clk) reset_by(no_reset);
   endinterface: trn_rx

   schedule (pcie_rxp, pcie_rxn, pcie_txp, pcie_txn, cfg_dataout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_wr_en_n, cfg_rd_en_n, cfg_irq_interrupt_n, cfg_irq_interrupt_rdy_n, cfg_irq_interrupt_mmenable, cfg_irq_interrupt_msienable,
             cfg_irq_interrupt_di, cfg_irq_interrupt_do, cfg_irq_interrupt_assert_n, cfg_to_turnoff_n, cfg_byte_en_n, cfg_bus_number, cfg_device_number, cfg_function_number,
             cfg_status, cfg_command, cfg_dstatus, cfg_dcommand, cfg_lstatus, cfg_lcommand, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn,
             cfg_err_ecrc_n, cfg_err_ur_n, cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n, cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n,
             trn_lnk_up_n, trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n, trn_tx_tsrc_dsc_n, trn_tx_tdst_dsc_n, trn_tx_tbuf_av, trn_tx_terrfwd_n,
             trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n, trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rcpl_streaming_n, trn_rx_rbar_hit_n, trn_rx_rfc_ph_av, trn_rx_rfc_pd_av, trn_rx_rfc_nph_av, trn_rx_rfc_npd_av) CF
            (pcie_rxp, pcie_rxn, pcie_txp, pcie_txn, cfg_dataout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_wr_en_n, cfg_rd_en_n, cfg_irq_interrupt_n, cfg_irq_interrupt_rdy_n, cfg_irq_interrupt_mmenable, cfg_irq_interrupt_msienable,
             cfg_irq_interrupt_di, cfg_irq_interrupt_do, cfg_irq_interrupt_assert_n, cfg_to_turnoff_n, cfg_byte_en_n, cfg_bus_number, cfg_device_number, cfg_function_number,
             cfg_status, cfg_command, cfg_dstatus, cfg_dcommand, cfg_lstatus, cfg_lcommand, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn,
             cfg_err_ecrc_n, cfg_err_ur_n, cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n, cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n,
             trn_lnk_up_n, trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n, trn_tx_tsrc_dsc_n, trn_tx_tdst_dsc_n, trn_tx_tbuf_av, trn_tx_terrfwd_n,
             trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n, trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rcpl_streaming_n, trn_rx_rbar_hit_n, trn_rx_rfc_ph_av, trn_rx_rfc_pd_av, trn_rx_rfc_nph_av, trn_rx_rfc_npd_av);

endmodule: vMkVirtex5PCIExpressWithDCM


// Specialized for a single-lane endpoint (trn.clk = 62.5 MHz, trn.clk2 == noClock)
import "BVI" endpoint_blk_plus_v1_14 /* pcie_endpoint */ =
module vMkVirtex5PCIExpressSingleLane#(PCIEParams params)(PCIE#(1));

   default_clock clk(sys_clk);
   default_reset rstn(sys_reset_n);

   port fast_train_simulation_only = params.fast_train_sim_only;

   output_clock  clkout(refclkout);

   interface PCIE_EXP pcie;
      method                  rxp(pci_exp_rxp) enable((*inhigh*)en0) reset_by(no_reset);
      method                  rxn(pci_exp_rxn) enable((*inhigh*)en1) reset_by(no_reset);
      method pci_exp_txp      txp                                    reset_by(no_reset);
      method pci_exp_txn      txn                                    reset_by(no_reset);
   endinterface: pcie

   interface PCIE_CFG cfg;
      method cfg_do                     dataout                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_rd_wr_done_n           rd_wr_done_n                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            di(cfg_di)                                   enable((*inhigh*)en2)  clocked_by(trn_clk) reset_by(no_reset);
      method                            dwaddr(cfg_dwaddr)                           enable((*inhigh*)en3)  clocked_by(trn_clk) reset_by(no_reset);
      method                            wr_en_n(cfg_wr_en_n)                         enable((*inhigh*)en4)  clocked_by(trn_clk) reset_by(no_reset);
      method                            rd_en_n(cfg_rd_en_n)                         enable((*inhigh*)en5)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_to_turnoff_n           to_turnoff_n                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            byte_en_n(cfg_byte_en_n)                     enable((*inhigh*)en9)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_bus_number             bus_number                                                          clocked_by(no_clock) reset_by(no_reset);
      method cfg_device_number          device_number                                                       clocked_by(no_clock) reset_by(no_reset);
      method cfg_function_number        function_number                                                     clocked_by(no_clock) reset_by(no_reset);
      method cfg_status                 status                                                              clocked_by(trn_clk) reset_by(no_reset);
      method cfg_command                command                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dstatus                dstatus                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_dcommand               dcommand                                                            clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lstatus                lstatus                                                             clocked_by(trn_clk) reset_by(no_reset);
      method cfg_lcommand               lcommand                                                            clocked_by(trn_clk) reset_by(no_reset);
      method                            pm_wake_n(cfg_pm_wake_n)                     enable((*inhigh*)en10) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_pcie_link_state_n      pcie_link_state_n                                                   clocked_by(trn_clk) reset_by(no_reset);
      method                            trn_pending_n(cfg_trn_pending_n)             enable((*inhigh*)en11) clocked_by(trn_clk) reset_by(no_reset);
      method                            dsn(cfg_dsn)                                 enable((*inhigh*)en12) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg

   interface PCIE_INT cfg_irq;
      method                            interrupt_n(cfg_interrupt_n)                 enable((*inhigh*)en6)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_rdy_n        interrupt_rdy_n                                                     clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_mmenable     interrupt_mmenable                                                  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_msienable    interrupt_msienable                                                 clocked_by(trn_clk) reset_by(no_reset);
      method                            interrupt_di(cfg_interrupt_di)               enable((*inhigh*)en7)  clocked_by(trn_clk) reset_by(no_reset);
      method cfg_interrupt_do           interrupt_do                                                        clocked_by(trn_clk) reset_by(no_reset);
      method                            interrupt_assert_n(cfg_interrupt_assert_n)   enable((*inhigh*)en8)  clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg_irq

   interface PCIE_ERR cfg_err;
      method                            ecrc_n(cfg_err_ecrc_n)                       enable((*inhigh*)en13) clocked_by(trn_clk) reset_by(no_reset);
      method                            ur_n(cfg_err_ur_n)                           enable((*inhigh*)en14) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_timeout_n(cfg_err_cpl_timeout_n)         enable((*inhigh*)en15) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_unexpect_n(cfg_err_cpl_unexpect_n)       enable((*inhigh*)en16) clocked_by(trn_clk) reset_by(no_reset);
      method                            cpl_abort_n(cfg_err_cpl_abort_n)             enable((*inhigh*)en17) clocked_by(trn_clk) reset_by(no_reset);
      method                            posted_n(cfg_err_posted_n)                   enable((*inhigh*)en18) clocked_by(trn_clk) reset_by(no_reset);
      method                            cor_n(cfg_err_cor_n)                         enable((*inhigh*)en19) clocked_by(trn_clk) reset_by(no_reset);
      method                            tlp_cpl_header(cfg_err_tlp_cpl_header)       enable((*inhigh*)en20) clocked_by(trn_clk) reset_by(no_reset);
      method cfg_err_cpl_rdy_n          cpl_rdy_n                                                           clocked_by(trn_clk) reset_by(no_reset);
      method                            locked_n(cfg_err_locked_n)                   enable((*inhigh*)en21) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: cfg_err

   interface PCIE_TRN trn;
      output_clock                      clk(trn_clk);
      output_clock                      clk2(trn_clk); /* not connected */
      output_reset                      reset_n(trn_reset_n)                                                clocked_by(trn_clk);
      method trn_lnk_up_n               lnk_up_n                                                            clocked_by(no_clock) reset_by(no_reset); /* semi-static */
   endinterface: trn

   interface PCIE_TRN_TX trn_tx;
      method                            tsof_n(trn_tsof_n)                           enable((*inhigh*)en22) clocked_by(trn_clk) reset_by(no_reset);
      method                            teof_n(trn_teof_n)                           enable((*inhigh*)en23) clocked_by(trn_clk) reset_by(no_reset);
      method                            td(trn_td)                                   enable((*inhigh*)en24) clocked_by(trn_clk) reset_by(no_reset);
      method                            trem_n(trn_trem_n)                           enable((*inhigh*)en25) clocked_by(trn_clk) reset_by(no_reset);
      method                            tsrc_rdy_n(trn_tsrc_rdy_n)                   enable((*inhigh*)en26) clocked_by(trn_clk) reset_by(no_reset);
      method trn_tdst_rdy_n             tdst_rdy_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            tsrc_dsc_n(trn_tsrc_dsc_n)                   enable((*inhigh*)en27) clocked_by(trn_clk) reset_by(no_reset);
      method trn_tdst_dsc_n             tdst_dsc_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_tbuf_av                tbuf_av                                                             clocked_by(trn_clk) reset_by(no_reset);
      method                            terrfwd_n(trn_terrfwd_n)                     enable((*inhigh*)en31) clocked_by(trn_clk) reset_by(no_reset);
   endinterface: trn_tx

   interface PCIE_TRN_RX trn_rx;
      method trn_rsof_n                 rsof_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_reof_n                 reof_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_rd                     rd                                                                  clocked_by(trn_clk) reset_by(no_reset);
      method trn_rrem_n                 rrem_n                                                              clocked_by(trn_clk) reset_by(no_reset);
      method trn_rerrfwd_n              rerrfwd_n                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rsrc_rdy_n             rsrc_rdy_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            rdst_rdy_n(trn_rdst_rdy_n)                   enable((*inhigh*)en28) clocked_by(trn_clk) reset_by(no_reset);
      method trn_rsrc_dsc_n             rsrc_dsc_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method                            rnp_ok_n(trn_rnp_ok_n)                       enable((*inhigh*)en29) clocked_by(trn_clk) reset_by(no_reset);
      method                            rcpl_streaming_n(trn_rcpl_streaming_n)       enable((*inhigh*)en30) clocked_by(trn_clk) reset_by(no_reset);
      method trn_rbar_hit_n             rbar_hit_n                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_ph_av              rfc_ph_av                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_pd_av              rfc_pd_av                                                           clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_nph_av             rfc_nph_av                                                          clocked_by(trn_clk) reset_by(no_reset);
      method trn_rfc_npd_av             rfc_npd_av                                                          clocked_by(trn_clk) reset_by(no_reset);
   endinterface: trn_rx

   schedule (pcie_rxp, pcie_rxn, pcie_txp, pcie_txn, cfg_dataout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_wr_en_n, cfg_rd_en_n, cfg_irq_interrupt_n, cfg_irq_interrupt_rdy_n, cfg_irq_interrupt_mmenable, cfg_irq_interrupt_msienable,
             cfg_irq_interrupt_di, cfg_irq_interrupt_do, cfg_irq_interrupt_assert_n, cfg_to_turnoff_n, cfg_byte_en_n, cfg_bus_number, cfg_device_number, cfg_function_number,
             cfg_status, cfg_command, cfg_dstatus, cfg_dcommand, cfg_lstatus, cfg_lcommand, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn,
             cfg_err_ecrc_n, cfg_err_ur_n, cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n, cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n,
             trn_lnk_up_n, trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n, trn_tx_tsrc_dsc_n, trn_tx_tdst_dsc_n, trn_tx_tbuf_av, trn_tx_terrfwd_n,
             trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n, trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rcpl_streaming_n, trn_rx_rbar_hit_n, trn_rx_rfc_ph_av, trn_rx_rfc_pd_av, trn_rx_rfc_nph_av, trn_rx_rfc_npd_av) CF
            (pcie_rxp, pcie_rxn, pcie_txp, pcie_txn, cfg_dataout, cfg_rd_wr_done_n, cfg_di, cfg_dwaddr, cfg_wr_en_n, cfg_rd_en_n, cfg_irq_interrupt_n, cfg_irq_interrupt_rdy_n, cfg_irq_interrupt_mmenable, cfg_irq_interrupt_msienable,
             cfg_irq_interrupt_di, cfg_irq_interrupt_do, cfg_irq_interrupt_assert_n, cfg_to_turnoff_n, cfg_byte_en_n, cfg_bus_number, cfg_device_number, cfg_function_number,
             cfg_status, cfg_command, cfg_dstatus, cfg_dcommand, cfg_lstatus, cfg_lcommand, cfg_pm_wake_n, cfg_pcie_link_state_n, cfg_trn_pending_n, cfg_dsn,
             cfg_err_ecrc_n, cfg_err_ur_n, cfg_err_cpl_timeout_n, cfg_err_cpl_unexpect_n, cfg_err_cpl_abort_n, cfg_err_posted_n, cfg_err_cor_n, cfg_err_tlp_cpl_header, cfg_err_cpl_rdy_n, cfg_err_locked_n,
             trn_lnk_up_n, trn_tx_tsof_n, trn_tx_teof_n, trn_tx_td, trn_tx_trem_n, trn_tx_tsrc_rdy_n, trn_tx_tdst_rdy_n, trn_tx_tsrc_dsc_n, trn_tx_tdst_dsc_n, trn_tx_tbuf_av, trn_tx_terrfwd_n,
             trn_rx_rsof_n, trn_rx_reof_n, trn_rx_rd, trn_rx_rrem_n, trn_rx_rerrfwd_n, trn_rx_rsrc_rdy_n, trn_rx_rdst_rdy_n, trn_rx_rsrc_dsc_n, trn_rx_rnp_ok_n, trn_rx_rcpl_streaming_n, trn_rx_rbar_hit_n, trn_rx_rfc_ph_av, trn_rx_rfc_pd_av, trn_rx_rfc_nph_av, trn_rx_rfc_npd_av);

endmodule: vMkVirtex5PCIExpressSingleLane

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
interface PCIE_TRN_COMMON;
   interface Clock       clk;
   interface Clock       clk2;
   interface Reset       reset_n;
   method    Bool        link_up;
endinterface: PCIE_TRN_COMMON

interface PCIE_TRN_XMIT;
   method    Action      xmit(Bool discontinue, TLPData#(8) data);
   method    Bool        discontinued;
   method    Bit#(4)     buffers_available;
endinterface: PCIE_TRN_XMIT

interface PCIE_TRN_RECV;
   method    ActionValue#(TLPData#(8)) recv();
   method    Bool        error_forward;
   method    Bool        source_discontinue;
   method    Action      non_posted_ready(Bool i);
   method    Action      completion_streaming(Bool i);
   method    Bit#(8)     posted_header_credits;
   method    Bit#(12)    posted_data_credits;
   method    Bit#(8)     non_posted_header_credits;
   method    Bit#(12)    non_posted_data_credits;
endinterface: PCIE_TRN_RECV

interface PCIExpress#(numeric type lanes);
   interface Clock            clkout;
   interface PCIE_EXP#(lanes) pcie;
   interface PCIE_TRN_COMMON  trn;
   interface PCIE_TRN_XMIT    trn_tx;
   interface PCIE_TRN_RECV    trn_rx;
   interface PCIE_CFG         cfg;
   interface PCIE_INT         cfg_irq;
   interface PCIE_ERR         cfg_err;
endinterface: PCIExpress

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
typeclass SelectVirtex5PCIE#(numeric type lanes);
   module selectVirtex5PCIE(PCIEParams params, PCIE#(lanes) ifc);
endtypeclass

instance SelectVirtex5PCIE#(1);
   module selectVirtex5PCIE(PCIEParams params, PCIE#(1) ifc);
      let _ifc <- vMkVirtex5PCIExpressSingleLane(params);
      return _ifc;
   endmodule
endinstance

instance SelectVirtex5PCIE#(8);
   module selectVirtex5PCIE(PCIEParams params, PCIE#(8) ifc);
      let _ifc <- vMkVirtex5PCIExpressWithDCM(params);
      return _ifc;
   endmodule
endinstance

module mkPCIExpressEndpoint#(PCIEParams params)(PCIExpress#(lanes))
   provisos(Add#(1,z,lanes), SelectVirtex5PCIE#(lanes));

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ///////////////////////////////////////////////////////////////////////////////
   PCIE#(lanes)                              pcie_ep             <- selectVirtex5PCIE(params);

   Clock                                     trnclk               = pcie_ep.trn.clk;
   Clock                                     trn2clk              = pcie_ep.trn.clk2;
   Reset                                     trnrst_n             = pcie_ep.trn.reset_n;

   PulseWire                                 pwTrnTx             <- mkPulseWire(clocked_by trnclk, reset_by noReset);
   Wire#(Bool)                               wTrnTxSof           <- mkDWire(True, clocked_by trnclk, reset_by noReset);
   Wire#(Bool)                               wTrnTxEof           <- mkDWire(True, clocked_by trnclk, reset_by noReset);
   Wire#(Bool)                               wTrnTxDsc           <- mkDWire(True, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(8))                            wTrnTxRem           <- mkDWire(8'h00, clocked_by trnclk, reset_by noReset);
   Wire#(Bit#(64))                           wTrnTxDat           <- mkDWire(64'h00, clocked_by trnclk, reset_by noReset);

   PulseWire                                 pwTrnRx             <- mkPulseWire(clocked_by trnclk, reset_by noReset);
   Wire#(Bool)                               wTrnRxNpOk          <- mkDWire(True, clocked_by trnclk, reset_by noReset);
   Wire#(Bool)                               wTrnRxCplS          <- mkDWire(True, clocked_by trnclk, reset_by noReset);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule connect_trn_tx;
      pcie_ep.trn_tx.tsof_n(pack(wTrnTxSof));
      pcie_ep.trn_tx.teof_n(pack(wTrnTxEof));
      pcie_ep.trn_tx.tsrc_dsc_n(pack(wTrnTxDsc));
      pcie_ep.trn_tx.trem_n(~wTrnTxRem);
      pcie_ep.trn_tx.td(wTrnTxDat);
      pcie_ep.trn_tx.tsrc_rdy_n(pack(!pwTrnTx));
      pcie_ep.trn_tx.terrfwd_n('1);
   endrule

   rule connect_trn_rx;
      pcie_ep.trn_rx.rdst_rdy_n(pack(!pwTrnRx));
      pcie_ep.trn_rx.rnp_ok_n(pack(wTrnRxNpOk));
      pcie_ep.trn_rx.rcpl_streaming_n(pack(wTrnRxCplS));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface clkout  = pcie_ep.clkout;
   interface pcie    = pcie_ep.pcie;

   interface PCIE_TRN_COMMON trn;
      interface clk           = pcie_ep.trn.clk;
      interface clk2          = pcie_ep.trn.clk2;
      interface reset_n       = pcie_ep.trn.reset_n;
      method Bool link_up     = (pcie_ep.trn.lnk_up_n == 0);
   endinterface: trn

   interface PCIE_TRN_XMIT trn_tx;
      method Action xmit(discontinue, data) if (pcie_ep.trn_tx.tdst_rdy_n == 0);
         wTrnTxSof <= !data.sof;
         wTrnTxEof <= !data.eof;
         wTrnTxDsc <= !discontinue;
         wTrnTxRem <= data.be;
         wTrnTxDat <= data.data;
         pwTrnTx.send;
      endmethod
      method discontinued      = (pcie_ep.trn_tx.tdst_dsc_n == 0);
      method buffers_available = pcie_ep.trn_tx.tbuf_av;
   endinterface: trn_tx

   interface PCIE_TRN_RECV trn_rx;
      method ActionValue#(TLPData#(8)) recv() if (pcie_ep.trn_rx.rsrc_rdy_n == 0);
         TLPData#(8) retval = defaultValue;
         retval.sof  = (pcie_ep.trn_rx.rsof_n == 0);
         retval.eof  = (pcie_ep.trn_rx.reof_n == 0);
         retval.hit  = ~pcie_ep.trn_rx.rbar_hit_n;
         retval.be   = ~pcie_ep.trn_rx.rrem_n;
         retval.data = pcie_ep.trn_rx.rd;
         pwTrnRx.send;
         return retval;
      endmethod
      method error_forward      = (pcie_ep.trn_rx.rerrfwd_n == 0);
      method source_discontinue = (pcie_ep.trn_rx.rsrc_dsc_n == 0);
      method Action non_posted_ready(i);
         wTrnRxNpOk <= !i;
      endmethod
      method Action completion_streaming(i);
         wTrnRxCplS <= !i;
      endmethod
      method posted_header_credits     = pcie_ep.trn_rx.rfc_ph_av;
      method posted_data_credits       = pcie_ep.trn_rx.rfc_pd_av;
      method non_posted_header_credits = pcie_ep.trn_rx.rfc_nph_av;
      method non_posted_data_credits   = pcie_ep.trn_rx.rfc_npd_av;
   endinterface: trn_rx

   interface cfg        = pcie_ep.cfg;
   interface cfg_irq    = pcie_ep.cfg_irq;
   interface cfg_err    = pcie_ep.cfg_err;
endmodule: mkPCIExpressEndpoint

////////////////////////////////////////////////////////////////////////////////
/// Connection Instances
////////////////////////////////////////////////////////////////////////////////

// Basic TLPData#(8) connections to PCIE endpoint

instance Connectable#(Get#(TLPData#(8)), PCIE_TRN_XMIT);
   module mkConnection#(Get#(TLPData#(8)) g, PCIE_TRN_XMIT p)(Empty);
      rule connect;
         let data <- g.get;
         p.xmit(False, data);
      endrule
   endmodule
endinstance

instance Connectable#(PCIE_TRN_XMIT, Get#(TLPData#(8)));
   module mkConnection#(PCIE_TRN_XMIT p, Get#(TLPData#(8)) g)(Empty);
      mkConnection(g, p);
   endmodule
endinstance

instance Connectable#(Put#(TLPData#(8)), PCIE_TRN_RECV);
   module mkConnection#(Put#(TLPData#(8)) p, PCIE_TRN_RECV r)(Empty);
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

instance Connectable#(PCIE_TRN_RECV, Put#(TLPData#(8)));
   module mkConnection#(PCIE_TRN_RECV r, Put#(TLPData#(8)) p)(Empty);
      mkConnection(p, r);
   endmodule
endinstance

// Connections between TLPData#(16) and a PCIE endpoint.
// These are all using the same clock, so the TLPData#(16) accesses
// will not be back-to-back.

instance Connectable#(Get#(TLPData#(16)), PCIE_TRN_XMIT);
   module mkConnection#(Get#(TLPData#(16)) g, PCIE_TRN_XMIT t)(Empty);
      FIFO#(TLPData#(8)) outFifo <- mkFIFO();

      rule connect;
         let data = outFifo.first; outFifo.deq;
         if (data.be != 0)
            t.xmit(False, data);
      endrule

      Put#(TLPData#(8)) p = fifoToPut(outFifo);
      mkConnection(g,p);
   endmodule
endinstance

instance Connectable#(PCIE_TRN_XMIT, Get#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_XMIT p, Get#(TLPData#(16)) g)(Empty);
      mkConnection(g, p);
   endmodule
endinstance

instance Connectable#(Put#(TLPData#(16)), PCIE_TRN_RECV);
   module mkConnection#(Put#(TLPData#(16)) p, PCIE_TRN_RECV r)(Empty);
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

instance Connectable#(PCIE_TRN_RECV, Put#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_RECV r, Put#(TLPData#(16)) p)(Empty);
      mkConnection(p, r);
   endmodule
endinstance

// Connections between TLPData#(16) and a PCIE endpoint, using a gearbox
// to match data rates between the endpoint and design clocks.

instance ConnectableWithClocks#(Put#(TLPData#(16)), PCIE_TRN_RECV);
   module mkConnectionWithClocks#(Put#(TLPData#(16)) p, PCIE_TRN_RECV g,
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

instance ConnectableWithClocks#(PCIE_TRN_RECV, Put#(TLPData#(16)));
   module mkConnectionWithClocks#(PCIE_TRN_RECV g, Put#(TLPData#(16)) p,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);
      mkConnectionWithClocks(p, g, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

instance ConnectableWithClocks#(PCIE_TRN_XMIT, Get#(TLPData#(16)));
   module mkConnectionWithClocks#(PCIE_TRN_XMIT p, Get#(TLPData#(16)) g,
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

instance ConnectableWithClocks#(Get#(TLPData#(16)), PCIE_TRN_XMIT);
   module mkConnectionWithClocks#(Get#(TLPData#(16)) g, PCIE_TRN_XMIT p,
                                  Clock fastClock, Reset fastReset,
                                  Clock slowClock, Reset slowReset)(Empty);

      mkConnectionWithClocks(p, g, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// interface tie-offs
instance TieOff#(PCIE_CFG);
   module mkTieOff#(PCIE_CFG ifc)(Empty);
      rule tie_off_inputs;
         ifc.di('0);
         ifc.wr_en_n('1);
         ifc.rd_en_n('1);
         ifc.dwaddr('0);
         ifc.byte_en_n('1);
         ifc.pm_wake_n('1);
         ifc.trn_pending_n('1);
         ifc.dsn({ 32'h0000_0001, { { 8'h1}, 24'h000A35}});
      endrule
   endmodule
endinstance

instance TieOff#(PCIE_ERR);
   module mkTieOff#(PCIE_ERR ifc)(Empty);
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

instance TieOff#(PCIE_INT);
   module mkTieOff#(PCIE_INT ifc)(Empty);
      rule tie_off_inputs;
         ifc.interrupt_n('1);
         ifc.interrupt_di('0);
         ifc.interrupt_assert_n('1);
      endrule
   endmodule
endinstance

endpackage: XilinxVirtex5PCIE

