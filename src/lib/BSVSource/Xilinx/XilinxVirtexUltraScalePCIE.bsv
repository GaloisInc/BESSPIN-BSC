////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012-2016  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision: 34744 $
// $Date: 2016-01-22 09:41:34 -0500 (Fri, 22 Jan 2016) $
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtexUltraScalePCIE.bsv
//  Description   :
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtexUltraScalePCIE;

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
import FIFOF             ::*;
import SpecialFIFOs      ::*;
import BRAMFIFO          ::*;
import ClientServer      ::*;
import BUtils            ::*;

import XilinxCells       ::*;
import XilinxClocks      ::*;
import PCIE              ::*;

// ================================================================
// Types of PCIE3 info connecting to bridge (mkPCIE3toBNocFull) via gearboxes etc.

typedef struct {
   Bit #(64)     data;
   Bool          sop;
   Bool          eop;
   Bit #(2)      keep;
   TLPFirstDWBE  first_be;
   TLPFirstDWBE  last_be;
} AxiStCq deriving (Bits, Eq);

typedef struct {
   Bit #(64)     data;
   Bit #(2)      keep;
   Bool          last;
} AxiStCc deriving (Bits, Eq);

typedef struct {
   Bit #(64)     data;
   Bool          last;
   Bit #(2)      keep;
   Bit #(4)      first_be;
   Bit #(4)      last_be;
} AxiStRq deriving (Bits, Eq);

typedef struct {
   Bit #(64)     data;
   Bool          sop;
   Bool          eop;
   Bit #(2)      keep;
   Bit #(8)      be;
} AxiStRc deriving (Bits, Eq);

// ================================================================
/// Raw interface from wrapped verilog PCIE3 endpoint

(* always_ready, always_enabled *)
interface PCIE3_VU#(numeric type lanes);
   interface PCIE_EXP#(lanes) pcie;
   interface Clock            user_clk;
   interface Reset            user_reset;
   interface PCIE3_STATUS_VU  status;
   interface PCIE3_AXI_RQ_VU  axi_rq;
   interface PCIE3_AXI_RC_VU  axi_rc;
   interface PCIE3_AXI_CQ_VU  axi_cq;
   interface PCIE3_AXI_CC_VU  axi_cc;
   interface PCIE3_INT_VU     cfg_interrupt;
   interface PCIE3_INT_MSIX_VU cfg_interrupt_msix;
endinterface

(* always_ready, always_enabled *)
interface PCIE3_STATUS_VU;
   method    Bool             lnk_up;
   method    Bit#(3)          max_payload;
   method    Bit#(3)          max_read_req;
   method    Bit#(4)          rcb_status;
   method    Bit#(16)         function_status;
endinterface

(* always_ready, always_enabled *)
interface PCIE3_AXI_RQ_VU;
   method    Action           tlast(Bool i);
   method    Action           tdata(Bit#(64) i);
   method    Action           tuser(Bit#(60) i);
   method    Action           tkeep(Bit#(2) i);
   method    Bit#(4)          tready();
   method    Action           tvalid(Bool i);
endinterface

(* always_ready, always_enabled *)
interface PCIE3_AXI_RC_VU;
   method    Bit#(64)         tdata();
   method    Bit#(75)         tuser();
   method    Bool             tlast();
   method    Bit#(2)          tkeep();
   method    Bool             tvalid();
   method    Action           tready(Bit#(1) i);
endinterface

(* always_ready, always_enabled *)
interface PCIE3_AXI_CQ_VU;
   method    Bit#(64)         tdata();
   method    Bit#(85)         tuser();
   method    Bool             tlast();
   method    Bit#(2)          tkeep();
   method    Bool             tvalid();
   method    Action           tready(Bit#(1) i);
endinterface

(* always_ready, always_enabled *)
interface PCIE3_AXI_CC_VU;
   method    Action           tlast(Bool i);
   method    Action           tdata(Bit#(64) i);
   method    Action           tuser(Bit#(33) i);
   method    Action           tkeep(Bit#(2) i);
   method    Bit#(4)          tready();
   method    Action           tvalid(Bool i);
endinterface

(* always_ready, always_enabled *)
interface PCIE3_INT_VU;
   method    Action           int_vect(Bit#(4) i);
   method    Action           pending(Bit#(4) i);
   method    Bool             sent();
endinterface

(* always_ready, always_enabled *)
interface PCIE3_INT_MSIX_VU;
   method     Bit#(4)         enabled();
   method     Bit#(4)         mask();
   method     Bit#(8)         vf_enable();
   method     Bit#(8)         vf_mask();
   method     Action          data(Bit#(32) i);
   method     Action          address(Bit#(64) i);
   method     Action          valid(Bit#(1) i); // int
   method     Bool            sent();
   method     Bool            fail();
endinterface

// ================================================================
// Immediate wrapper for imported Verilog PCIE3 endpoint

import "BVI" xilinx_ultrascale_pcie3_wrapper =
module vMkVirtexUltraScalePCIExpress3#(PCIEParams params)(Clock sys_clk_gt, PCIE3_VU#(lanes) ifc)
   provisos( Add#(1, z, lanes) );

   let sys_reset <- exposeCurrentReset;

   input_clock sys_clk_gt (sys_clk_gt) = sys_clk_gt;

   default_clock sys_clk(sys_clk);  // 100 MHz refclk
   default_reset sys_rstn(sys_reset) = sys_reset;

   interface PCIE_EXP pcie;
      method                            rxp(pci_exp_rxp) enable((*inhigh*)en0)                              reset_by(no_reset);
      method                            rxn(pci_exp_rxn) enable((*inhigh*)en1)                              reset_by(no_reset);
      method pci_exp_txp                txp                                                                 reset_by(no_reset);
      method pci_exp_txn                txn                                                                 reset_by(no_reset);
   endinterface

   output_clock                         user_clk(user_clk);
   output_reset                         user_reset(user_reset);

   interface PCIE3_STATUS_VU status;
      method user_lnk_up                lnk_up                                                              clocked_by(no_clock) reset_by(no_reset); /* semi-static */
      method cfg_max_payload            max_payload                                                         clocked_by(user_clk) reset_by(no_reset);
      method cfg_max_read_req           max_read_req                                                        clocked_by(user_clk) reset_by(no_reset);
      method cfg_rcb_status             rcb_status                                                          clocked_by(user_clk) reset_by(no_reset);
      method cfg_function_status        function_status                                                     clocked_by(user_clk) reset_by(no_reset);
   endinterface

   interface PCIE3_AXI_RQ_VU axi_rq;
      method                            tlast(s_axis_rq_tlast)                       enable((*inhigh*)en01) clocked_by(user_clk)  reset_by(no_reset);
      method                            tdata(s_axis_rq_tdata)                       enable((*inhigh*)en02) clocked_by(user_clk)  reset_by(no_reset);
      method                            tuser(s_axis_rq_tuser)                       enable((*inhigh*)en03) clocked_by(user_clk)  reset_by(no_reset);
      method                            tkeep(s_axis_rq_tkeep)                       enable((*inhigh*)en04) clocked_by(user_clk)  reset_by(no_reset);
      method s_axis_rq_tready           tready                                                              clocked_by(user_clk) reset_by(no_reset);
      method                            tvalid(s_axis_rq_tvalid)                     enable((*inhigh*)en05) clocked_by(user_clk)  reset_by(no_reset);
   endinterface

   interface PCIE3_AXI_RC_VU axi_rc;
      method m_axis_rc_tdata            tdata                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_rc_tuser            tuser                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_rc_tlast            tlast                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_rc_tkeep            tkeep                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_rc_tvalid           tvalid                                                              clocked_by(user_clk) reset_by(no_reset);
      method                            tready(m_axis_rc_tready)                     enable((*inhigh*)en06) clocked_by(user_clk) reset_by(no_reset);
   endinterface

   interface PCIE3_AXI_CQ_VU axi_cq;
      method m_axis_cq_tdata            tdata                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_cq_tuser            tuser                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_cq_tlast            tlast                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_cq_tkeep            tkeep                                                               clocked_by(user_clk) reset_by(no_reset);
      method m_axis_cq_tvalid           tvalid                                                              clocked_by(user_clk) reset_by(no_reset);
      method                            tready(m_axis_cq_tready)                     enable((*inhigh*)en07) clocked_by(user_clk) reset_by(no_reset);
   endinterface

   interface PCIE3_AXI_CC_VU axi_cc;
      method                            tlast(s_axis_cc_tlast)                       enable((*inhigh*)en08) clocked_by(user_clk)  reset_by(no_reset);
      method                            tdata(s_axis_cc_tdata)                       enable((*inhigh*)en09) clocked_by(user_clk)  reset_by(no_reset);
      method                            tuser(s_axis_cc_tuser)                       enable((*inhigh*)en10) clocked_by(user_clk)  reset_by(no_reset);
      method                            tkeep(s_axis_cc_tkeep)                       enable((*inhigh*)en11) clocked_by(user_clk)  reset_by(no_reset);
      method s_axis_cc_tready           tready                                                              clocked_by(user_clk) reset_by(no_reset);
      method                            tvalid(s_axis_cc_tvalid)                     enable((*inhigh*)en12) clocked_by(user_clk)  reset_by(no_reset);
   endinterface

   interface PCIE3_INT_VU cfg_interrupt;
      method                            int_vect(cfg_interrupt_int)                  enable((*inhigh*)en13) clocked_by(user_clk)  reset_by(no_reset);
      method                            pending(cfg_interrupt_pending)               enable((*inhigh*)en14) clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_sent         sent                                                                clocked_by(user_clk) reset_by(no_reset);
   endinterface

   interface PCIE3_INT_MSIX_VU cfg_interrupt_msix;
      method cfg_interrupt_msix_enable  enabled                                                             clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_msix_mask    mask                                                                clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_msix_vf_enable vf_enable                                                         clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_msix_vf_mask vf_mask                                                             clocked_by(user_clk)  reset_by(no_reset);
      method                            data(cfg_interrupt_msix_data)                enable((*inhigh*)en23) clocked_by(user_clk)  reset_by(no_reset);
      method                            address(cfg_interrupt_msix_address)          enable((*inhigh*)en24) clocked_by(user_clk)  reset_by(no_reset);
      method                            valid(cfg_interrupt_msix_int)                enable((*inhigh*)en25) clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_msix_sent    sent                                                                clocked_by(user_clk)  reset_by(no_reset);
      method cfg_interrupt_msix_fail    fail                                                                clocked_by(user_clk)  reset_by(no_reset);
   endinterface

   schedule (status_lnk_up, axi_rq_tlast, axi_rq_tdata, axi_rq_tuser, axi_rq_tkeep, axi_rq_tready,
	     axi_rq_tvalid, axi_rc_tdata, axi_rc_tuser, axi_rc_tlast, axi_rc_tkeep, axi_rc_tvalid, axi_rc_tready,
	     axi_cq_tdata, axi_cq_tuser, axi_cq_tlast, axi_cq_tkeep, axi_cq_tvalid, axi_cq_tready, axi_cc_tlast,
	     axi_cc_tdata, axi_cc_tuser, axi_cc_tkeep, axi_cc_tready, axi_cc_tvalid, cfg_interrupt_int_vect,
	     cfg_interrupt_pending, cfg_interrupt_sent, cfg_interrupt_msix_enabled, cfg_interrupt_msix_mask, cfg_interrupt_msix_vf_enable,
	     cfg_interrupt_msix_vf_mask, cfg_interrupt_msix_data, cfg_interrupt_msix_address, cfg_interrupt_msix_valid,
	     cfg_interrupt_msix_sent, cfg_interrupt_msix_fail, pcie_txp, pcie_txn, pcie_rxp, pcie_rxn,
	     status_max_payload, status_max_read_req, status_rcb_status, status_function_status) CF
            (status_lnk_up, axi_rq_tlast, axi_rq_tdata, axi_rq_tuser, axi_rq_tkeep, axi_rq_tready,
	     axi_rq_tvalid, axi_rc_tdata, axi_rc_tuser, axi_rc_tlast, axi_rc_tkeep, axi_rc_tvalid, axi_rc_tready,
	     axi_cq_tdata, axi_cq_tuser, axi_cq_tlast, axi_cq_tkeep, axi_cq_tvalid, axi_cq_tready, axi_cc_tlast,
	     axi_cc_tdata, axi_cc_tuser, axi_cc_tkeep, axi_cc_tready, axi_cc_tvalid, cfg_interrupt_int_vect,
	     cfg_interrupt_pending, cfg_interrupt_sent, cfg_interrupt_msix_enabled, cfg_interrupt_msix_mask, cfg_interrupt_msix_vf_enable,
	     cfg_interrupt_msix_vf_mask, cfg_interrupt_msix_data, cfg_interrupt_msix_address, cfg_interrupt_msix_valid,
	     cfg_interrupt_msix_sent, cfg_interrupt_msix_fail, pcie_txp, pcie_txn, pcie_rxp, pcie_rxn,
	     status_max_payload, status_max_read_req, status_rcb_status, status_function_status);
endmodule

// ================================================================
// Interfaces PCIE3 for bridge

interface PCIExpress3VU #(numeric type lanes);
   interface PCIE_EXP#(lanes)    pcie;
   interface Clock               uclk;
   interface Reset               ureset;
   interface Clock               uclk_half;
   interface Reset               ureset_half;
   interface PCIE3_STATUS_VU     status;
   interface Get #(AxiStCq)      cq_recv;
   interface Put #(AxiStCc)      cc_xmit;
   interface Put #(AxiStRq)      rq_xmit;
   interface Get #(AxiStRc)      rc_recv;
   interface PCIE3_INT_VU        cfg_interrupt;
   interface PCIE3_INT_MSIX_VU   cfg_interrupt_msix;
   interface XilinxClkServer     clks;
   interface Clock               cclk;
endinterface

////////////////////////////////////////////////////////////////////////////////
/// For PCIE3
/// Typeclass to select vMkVirtexUltraScalePCIExpress3() for 1, 4, 8 lanes
////////////////////////////////////////////////////////////////////////////////

typeclass SelectVirtexUltraScalePCIE3#(numeric type lanes);
   module selectVirtexUltraScalePCIE3(PCIEParams params, Clock sys_clk_gt, PCIE3_VU#(lanes) ifc);
endtypeclass

instance SelectVirtexUltraScalePCIE3#(8);
   module selectVirtexUltraScalePCIE3(PCIEParams params, Clock sys_clk_gt, PCIE3_VU#(8) ifc);
      let _ifc <- vMkVirtexUltraScalePCIExpress3(params, sys_clk_gt);
      return _ifc;
   endmodule
endinstance

instance SelectVirtexUltraScalePCIE3#(4);
   module selectVirtexUltraScalePCIE3(PCIEParams params, Clock sys_clk_gt, PCIE3_VU#(4) ifc);
      let _ifc <- vMkVirtexUltraScalePCIExpress3(params, sys_clk_gt);
      return _ifc;
   endmodule
endinstance

instance SelectVirtexUltraScalePCIE3#(1);
   module selectVirtexUltraScalePCIE3(PCIEParams params, Clock sys_clk_gt, PCIE3_VU#(1) ifc);
      let _ifc <- vMkVirtexUltraScalePCIExpress3(params, sys_clk_gt);
      return _ifc;
   endmodule
endinstance

////////////////////////////////////////////////////////////////
// The BSV PCIE3 endpoint
////////////////////////////////////////////////////////////////

module mkPCIExpress3EndpointVU #(PCIEParams params) (Clock sys_clk_gt, PCIExpress3VU #(lanes) ifc)
   provisos(Add #(1, z, lanes), SelectVirtexUltraScalePCIE3 #(lanes));

   // Instantiate Vivado-generated, wrapped Verilog endpoint
   PCIE3_VU #(lanes)                         pcie_ep             <- selectVirtexUltraScalePCIE3 (params, sys_clk_gt);

   Clock                                     user_clk             = pcie_ep.user_clk;
   Reset                                     user_reset_raw      <- mkResetInverter (pcie_ep.user_reset, clocked_by user_clk);
   Reset                                     user_reset          <- mkAsyncReset(4, user_reset_raw, user_clk);

   ClockGeneratorUParams                     clk_params           = defaultValue;
   clk_params.clkin1_period    = 4.000;
   clk_params.clkin_buffer     = False;
   clk_params.clkfbout_mult_f  = 4.000;
   clk_params.clkout0_divide_f = 8.000;
   ClockGeneratorU                           clkgen              <- mkClockGeneratorU (clk_params,
										       clocked_by user_clk,
										       reset_by user_reset_raw);
   Clock                                     user_clk_half        = clkgen.clkout0;
   Reset                                     user_reset_half     <- mkAsyncReset(4, user_reset_raw, user_clk_half);

   XilinxClockParams                         cclk_params          = defaultValue;
   cclk_params.e_type           = E3;
   cclk_params.clkin1_period    = 8.000;
   cclk_params.clkfbout_mult_f  = 8.000;
   cclk_params.clkout0_divide_f = params.clock_period;
   // XXX Can we use "user_reset_half" here?
   Reset                                     cclk_clkgen_reset   <- mkAsyncReset(1, user_reset_raw, user_clk_half);
   XilinxClockController                     cclk_clkgen         <- mkXilinxClockController (cclk_params,
											     user_clk_half,
											     clocked_by user_clk_half,
											     reset_by cclk_clkgen_reset);

   // ----------------
   // FIFOs that drain the CQ and RC AXI Stream interfaces

   FIFOF #(AxiStCq)  fAxiCq          <- mkBypassFIFOF (clocked_by user_clk, reset_by user_reset);

   // RC
   FIFOF #(AxiStRc)  fAxiRc          <- mkBypassFIFOF (clocked_by user_clk, reset_by user_reset);

   // ----------------
   // FIFOs that feed the CC and RQ AXI Stream interfaces

   // CC
   FIFO#(AxiStCc)    fAxiCc          <- mkCCBuffer(clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(64))   wAxiCcData      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiCcLast      <- mkDWire(False, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(2))    wAxiCcKeep      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit #(33))  wAxiCcUser      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiCcValid     <- mkDWire(False, clocked_by user_clk, reset_by user_reset);

   // RQ
   FIFO#(AxiStRq)    fAxiRq          <- mkRQBuffer(clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(64))   wAxiRqData      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiRqLast      <- mkDWire(False, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(2))    wAxiRqKeep      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(60))   wAxiRqUser      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiRqValid     <- mkDWire(False, clocked_by user_clk, reset_by user_reset);

   // ----------------------------------------------------------------
   // RULES

   // ----------------
   // CQ (requests from host): here at 250 MHz we only collect the
   // info we need and pass it on; the actual parsing of descriptors
   // and payloads happens in the 125 MHz domain which can accommodate
   // more complex circuits.

   // Collect cq from AXI into FIFO fAxiCq
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_cq_ready;
      pcie_ep.axi_cq.tready (pack (fAxiCq.notFull));
   endrule

   rule process_ax_cq_beat (pcie_ep.axi_cq.tvalid && fAxiCq.notFull);
      let cq = AxiStCq {data:     pcie_ep.axi_cq.tdata,
			sop:      unpack (pcie_ep.axi_cq.tuser [40]),  // tuser.sop
			eop:      pcie_ep.axi_cq.tlast,
			keep:     pcie_ep.axi_cq.tkeep,
			first_be: pcie_ep.axi_cq.tuser [3:0],    // tuser.first_be,
			last_be:  pcie_ep.axi_cq.tuser [7:4]};   // tuser.last_be
      fAxiCq.enq (cq);
   endrule

   // ----------------
   // RC (completions from host): here at 250 MHz we only collect the
   // info we need and pass it on; the actual parsing of descriptors
   // and payloads happens in the 125 MHz domain which can accommodate
   // more complex circuits

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_rc_ready;
      pcie_ep.axi_rc.tready (pack (fAxiRc.notFull));
   endrule

   rule process_ax_rc_beat (pcie_ep.axi_rc.tvalid && fAxiRc.notFull);
      let rc = AxiStRc {data:pcie_ep.axi_rc.tdata,
			sop: unpack (pcie_ep.axi_rc.tuser [32]),         // tuser.is_sof_0
			eop: pcie_ep.axi_rc.tlast,
			keep:pcie_ep.axi_rc.tkeep,
			be:  truncate (pcie_ep.axi_rc.tuser [31:0])};    // tuser.byte_en
      fAxiRc.enq (rc);
   endrule

   // Move rc from FIFO fAxiRc_a to fAxiRc_b, injecting an 'empty'
   // pad, if necessary, to ensure that sop is on an even enq

   // ----------------
   // CC (completions to host): here at 250 MHz we just pass on the
   // info we get from the bridge, which was created at 125 MHz.

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_cc;
      pcie_ep.axi_cc.tdata  (wAxiCcData);
      pcie_ep.axi_cc.tkeep  (wAxiCcKeep);
      pcie_ep.axi_cc.tlast  (wAxiCcLast);
      pcie_ep.axi_cc.tuser  (wAxiCcUser);
      pcie_ep.axi_cc.tvalid (wAxiCcValid);
   endrule

   (* fire_when_enabled *)
   rule drive_axi_cc_info (pcie_ep.axi_cc.tready != 0);
      let cc <- toGet (fAxiCc).get;
      wAxiCcValid <= True;
      wAxiCcLast  <= cc.last;
      wAxiCcData  <= cc.data;
      wAxiCcKeep  <= cc.keep;
      wAxiCcUser  <= 0;        // tuser.discontinue and tuser.parity[31:0]
   endrule

   // ----------------
   // RQ (requests to host): here at 250 MHz we just pass on the
   // info we get from the bridge, which was created at 125 MHz.

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_rq;
      pcie_ep.axi_rq.tdata  (wAxiRqData);
      pcie_ep.axi_rq.tkeep  (wAxiRqKeep);
      pcie_ep.axi_rq.tlast  (wAxiRqLast);
      pcie_ep.axi_rq.tuser  (wAxiRqUser);
      pcie_ep.axi_rq.tvalid (wAxiRqValid);
   endrule

   (* fire_when_enabled *)
   rule drive_axi_rq_info (pcie_ep.axi_rq.tready != 0);
      let rq <- toGet (fAxiRq).get;
      wAxiRqValid <= True;
      wAxiRqData  <= rq.data;
      wAxiRqLast  <= rq.last;
      wAxiRqKeep  <= rq.keep;
      wAxiRqUser  <= { 0, rq.last_be, rq.first_be};
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface pcie = pcie_ep.pcie;

   interface uclk        = user_clk;
   interface ureset      = user_reset;

   interface uclk_half   = user_clk_half;
   interface ureset_half = user_reset_half;

   interface PCIE3_STATUS_VU status;
      method    lnk_up = pcie_ep.status.lnk_up;
      method    max_payload = pcie_ep.status.max_payload;
      method    max_read_req = pcie_ep.status.max_read_req;
      method    rcb_status = pcie_ep.status.rcb_status;
      method    function_status = pcie_ep.status.function_status;
   endinterface

   interface cq_recv = toGet (fAxiCq);
   interface rc_recv = toGet (fAxiRc);

   interface cc_xmit = toPut (fAxiCc);
   interface rq_xmit = toPut (fAxiRq);

   interface cfg_interrupt      = pcie_ep.cfg_interrupt;
   interface cfg_interrupt_msix = pcie_ep.cfg_interrupt_msix;

   interface cclk = cclk_clkgen.clkout0;

   interface XilinxClkServer clks;
      interface Put request;
	 method Action put(Bit#(32) x);
	    let request = XilinxClockRequest {
	       rnw:  unpack(x[31]),
	       addr: x[20:16],
	       data: x[15:0]
	       };
	    cclk_clkgen.csr.request.put(request);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue#(Bit#(32)) get;
	    let response <- cclk_clkgen.csr.response.get;
	    return cExtend(response);
	 endmethod
      endinterface
   endinterface
endmodule

module mkPCIExpress3EndpointNoClkVU #(PCIEParams params) (Clock sys_clk_gt, PCIExpress3VU #(lanes) ifc)
   provisos(Add #(1, z, lanes), SelectVirtexUltraScalePCIE3 #(lanes));

   // Instantiate Vivado-generated, wrapped Verilog endpoint
   PCIE3_VU #(lanes)                         pcie_ep             <- selectVirtexUltraScalePCIE3 (params, sys_clk_gt);

   Clock                                     user_clk             = pcie_ep.user_clk;
   Reset                                     user_reset_raw      <- mkResetInverter (pcie_ep.user_reset, clocked_by user_clk);
   Reset                                     user_reset          <- mkAsyncReset(4, user_reset_raw, user_clk);

   ClockGeneratorUParams                     clk_params           = defaultValue;
   clk_params.clkin1_period    = 4.000;
   clk_params.clkin_buffer     = False;
   clk_params.clkfbout_mult_f  = 4.000;
   clk_params.clkout0_divide_f = 8.000;

   clk_params.clkout0n_buffer  = False;
   clk_params.clkout1_buffer   = False;
   clk_params.clkout1n_buffer  = False;
   clk_params.clkout2_buffer   = False;
   clk_params.clkout2n_buffer  = False;
   clk_params.clkout3_buffer   = False;
   clk_params.clkout3n_buffer  = False;
   clk_params.clkout4_buffer   = False;
   clk_params.clkout5_buffer   = False;
   clk_params.clkout6_buffer   = False;

   ClockGeneratorU                           clkgen              <- mkClockGeneratorU (clk_params,
										       clocked_by user_clk,
										       reset_by user_reset_raw);
   Clock                                     user_clk_half        = clkgen.clkout0;
   Reset                                     user_reset_half     <- mkAsyncReset(4, user_reset_raw, user_clk_half);

   // ----------------
   // FIFOs that drain the CQ and RC AXI Stream interfaces

   FIFOF #(AxiStCq)  fAxiCq          <- mkBypassFIFOF (clocked_by user_clk, reset_by user_reset);

   // RC
   FIFOF #(AxiStRc)  fAxiRc          <- mkBypassFIFOF (clocked_by user_clk, reset_by user_reset);

   // ----------------
   // FIFOs that feed the CC and RQ AXI Stream interfaces

   // CC
   FIFO#(AxiStCc)    fAxiCc          <- mkCCBuffer(clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(64))   wAxiCcData      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiCcLast      <- mkDWire(False, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(2))    wAxiCcKeep      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit #(33))  wAxiCcUser      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiCcValid     <- mkDWire(False, clocked_by user_clk, reset_by user_reset);

   // RQ
   FIFO#(AxiStRq)    fAxiRq          <- mkRQBuffer(clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(64))   wAxiRqData      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiRqLast      <- mkDWire(False, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(2))    wAxiRqKeep      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bit#(60))   wAxiRqUser      <- mkDWire(0, clocked_by user_clk, reset_by user_reset);
   Wire#(Bool)       wAxiRqValid     <- mkDWire(False, clocked_by user_clk, reset_by user_reset);

   // ----------------------------------------------------------------
   // RULES

   // ----------------
   // CQ (requests from host): here at 250 MHz we only collect the
   // info we need and pass it on; the actual parsing of descriptors
   // and payloads happens in the 125 MHz domain which can accommodate
   // more complex circuits.

   // Collect cq from AXI into FIFO fAxiCq
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_cq_ready;
      pcie_ep.axi_cq.tready (pack (fAxiCq.notFull));
   endrule

   rule process_ax_cq_beat (pcie_ep.axi_cq.tvalid && fAxiCq.notFull);
      let cq = AxiStCq {data:     pcie_ep.axi_cq.tdata,
			sop:      unpack (pcie_ep.axi_cq.tuser [40]),  // tuser.sop
			eop:      pcie_ep.axi_cq.tlast,
			keep:     pcie_ep.axi_cq.tkeep,
			first_be: pcie_ep.axi_cq.tuser [3:0],    // tuser.first_be,
			last_be:  pcie_ep.axi_cq.tuser [7:4]};   // tuser.last_be
      fAxiCq.enq (cq);
   endrule

   // ----------------
   // RC (completions from host): here at 250 MHz we only collect the
   // info we need and pass it on; the actual parsing of descriptors
   // and payloads happens in the 125 MHz domain which can accommodate
   // more complex circuits

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_rc_ready;
      pcie_ep.axi_rc.tready (pack (fAxiRc.notFull));
   endrule

   rule process_ax_rc_beat (pcie_ep.axi_rc.tvalid && fAxiRc.notFull);
      let rc = AxiStRc {data:pcie_ep.axi_rc.tdata,
			sop: unpack (pcie_ep.axi_rc.tuser [32]),         // tuser.is_sof_0
			eop: pcie_ep.axi_rc.tlast,
			keep:pcie_ep.axi_rc.tkeep,
			be:  truncate (pcie_ep.axi_rc.tuser [31:0])};    // tuser.byte_en
      fAxiRc.enq (rc);
   endrule

   // Move rc from FIFO fAxiRc_a to fAxiRc_b, injecting an 'empty'
   // pad, if necessary, to ensure that sop is on an even enq

   // ----------------
   // CC (completions to host): here at 250 MHz we just pass on the
   // info we get from the bridge, which was created at 125 MHz.

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_cc;
      pcie_ep.axi_cc.tdata  (wAxiCcData);
      pcie_ep.axi_cc.tkeep  (wAxiCcKeep);
      pcie_ep.axi_cc.tlast  (wAxiCcLast);
      pcie_ep.axi_cc.tuser  (wAxiCcUser);
      pcie_ep.axi_cc.tvalid (wAxiCcValid);
   endrule

   (* fire_when_enabled *)
   rule drive_axi_cc_info (pcie_ep.axi_cc.tready != 0);
      let cc <- toGet (fAxiCc).get;
      wAxiCcValid <= True;
      wAxiCcLast  <= cc.last;
      wAxiCcData  <= cc.data;
      wAxiCcKeep  <= cc.keep;
      wAxiCcUser  <= 0;        // tuser.discontinue and tuser.parity[31:0]
   endrule

   // ----------------
   // RQ (requests to host): here at 250 MHz we just pass on the
   // info we get from the bridge, which was created at 125 MHz.

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_rq;
      pcie_ep.axi_rq.tdata  (wAxiRqData);
      pcie_ep.axi_rq.tkeep  (wAxiRqKeep);
      pcie_ep.axi_rq.tlast  (wAxiRqLast);
      pcie_ep.axi_rq.tuser  (wAxiRqUser);
      pcie_ep.axi_rq.tvalid (wAxiRqValid);
   endrule

   (* fire_when_enabled *)
   rule drive_axi_rq_info (pcie_ep.axi_rq.tready != 0);
      let rq <- toGet (fAxiRq).get;
      wAxiRqValid <= True;
      wAxiRqData  <= rq.data;
      wAxiRqLast  <= rq.last;
      wAxiRqKeep  <= rq.keep;
      wAxiRqUser  <= { 0, rq.last_be, rq.first_be};
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface pcie = pcie_ep.pcie;

   interface uclk        = user_clk;
   interface ureset      = user_reset;

   interface uclk_half   = user_clk_half;
   interface ureset_half = user_reset_half;

   interface PCIE3_STATUS_VU status;
      method    lnk_up = pcie_ep.status.lnk_up;
      method    max_payload = pcie_ep.status.max_payload;
      method    max_read_req = pcie_ep.status.max_read_req;
      method    rcb_status = pcie_ep.status.rcb_status;
      method    function_status = pcie_ep.status.function_status;
   endinterface

   interface cq_recv = toGet (fAxiCq);
   interface rc_recv = toGet (fAxiRc);

   interface cc_xmit = toPut (fAxiCc);
   interface rq_xmit = toPut (fAxiRq);

   interface cfg_interrupt      = pcie_ep.cfg_interrupt;
   interface cfg_interrupt_msix = pcie_ep.cfg_interrupt_msix;
endmodule

// ================================================================
// Connecting/converting stream AxiCq ==> stream TLPData#(16)

instance ConnectableWithClocks #(Get #(AxiStCq), Put #(TLPData #(16)));
   module mkConnectionWithClocks #(Get #(AxiStCq) g, Put #(TLPData #(16)) p,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);

      // Data path: g -> gearbox_1_2 -> f_cq -> p

      Reg #(Bool) rg_even_enq     <- mkReg (True, clocked_by fastClock, reset_by fastReset);
      Reg #(Bool) rg_pad_odd_tail <- mkReg (False, clocked_by fastClock, reset_by fastReset);

      // Buffer incoming messages for timing
      FIFO #(AxiStCq) in_buf <- mkFIFO(clocked_by fastClock, reset_by fastReset);
      mkConnection(g, toPut(in_buf));
      let g_buf = toGet(in_buf);

      Gearbox #(1, 2, AxiStCq) gearbox <- mk1toNGearbox (fastClock, fastReset, slowClock, slowReset);

      // f_cq provides one extra level of buffering, allowing us to
      // examine 3 AxiStCqs (two at head of f_cq, and one at head of
      // gearbox). This is needed for write packets, where we have to
      // look descriptor (64b,64b) + 32b data (1st. half of 3rd 64b)

      FIFOF #(Vector #(2, AxiStCq)) f_cq   <- mkPipelineFIFOF (clocked_by slowClock,
							       reset_by slowReset);

      // ----------------
      // g -> gearbox

      // If eop happens on an even enq, we insert a dummy odd enq, for two reasons:
      // (1) sop is always an even enq, so it is in element [0] of the gearbox output
      // (2) to avoid deadlock/delay: the gearbox output does not show
      //       up until it has received both an even and an odd enq

      // Incoming AxiStCq: g ==> gearbox (fast clock)
      rule rl_g_to_gearbox (! rg_pad_odd_tail);
	 let cq <- g_buf.get;
	 // Assert: cq.sop => rg_even_enq
	 Vector #(1, AxiStCq) v1 = replicate (cq);
	 gearbox.enq (v1);
	 rg_pad_odd_tail <= (cq.eop && rg_even_enq);
	 rg_even_enq     <= ! rg_even_enq;
      endrule

      rule rl_g_to_gearbox_pad_odd_tail (rg_pad_odd_tail);
	 AxiStCq cq = unpack (0);
	 Vector #(1, AxiStCq) v1 = replicate (cq);
	 gearbox.enq (v1);
	 rg_pad_odd_tail <= False;
	 rg_even_enq     <= True;
      endrule

      // ----------------
      // gearbox -> f_cq (slow clock)

      rule rl_slowclock;
	 Vector #(2, AxiStCq) v2 = gearbox.first;
	 gearbox.deq;
	 f_cq.enq (v2);
      endrule

      Reg #(DWCount)     rg_dwcount <- mkRegU (clocked_by slowClock, reset_by slowReset);

      CQDescriptor cq_desc = unpack ({ f_cq.first [1].data, f_cq.first [0].data });

      // 'Write' headers
      rule rl_wr_header (f_cq.first [0].sop
			 && ((cq_desc.reqtype == MEMORY_WRITE) || (cq_desc.reqtype == IO_WRITE)));

	 // Consume 1st 32b of data of next axi beat (head of gearbox FIFO)
	 // Note: gearbox.first will move into f_cq, where remaining 32 bits will be consumed
	 Bit #(32) data = gearbox.first [0].data [31:0];

	 // this takes the data in the Xilinx byte order and converts it
	 TLPData #(16) tlp16 = convertCQDescriptorToTLP16 (cq_desc,
							   data,
							   f_cq.first [0].first_be,
							   f_cq.first [0].last_be);
	 p.put (tlp16);

	 rg_dwcount  <= cq_desc.dwcount - 1;    // Since first DW is in the tlp16
	 f_cq.deq;
      endrule

      // 'Read' headers
      rule rl_rd_header (f_cq.first [0].sop
			 && ((cq_desc.reqtype == MEMORY_READ) || (cq_desc.reqtype == IO_READ)));

	 Bit #(32) data = 0;    // don't care, but set to 0 for deterministic debugging

	 TLPData #(16) tlp16 = convertCQDescriptorToTLP16 (cq_desc,
							   data,
							   f_cq.first [0].first_be,
							   f_cq.first [0].last_be);
	 p.put (tlp16);

	 f_cq.deq;
      endrule

      // 'Write' data payload; no data remaining
      rule rl_data_0 ((! f_cq.first [0].sop) && (rg_dwcount == 0));
	 // f_cq.first [0].data [31:0] already consumed
	 // f_cq.first [0].data [64:32] and f_cq.first [1].data [64:0] are just padding
	 f_cq.deq;
      endrule

      // 'Write' data payload 1 to 3 DWs remaining
      rule rl_data_1_to_3 ((! f_cq.first [0].sop) && (rg_dwcount > 0) && (rg_dwcount < 4));
	 Bit #(16) be16 = 0;
	 case (rg_dwcount)
	    1: be16 = 16'hF000;
	    2: be16 = 16'hFF00;
	    3: be16 = 16'hFFF0;
	 endcase
	 Vector#(4, Bit#(32)) data_vec = replicate(0);
	 data_vec[3] = convertDW(f_cq.first [0].data [63:32]) ;
	 data_vec[2] = convertDW(f_cq.first [1].data [31:0]) ;
	 data_vec[1] = convertDW(f_cq.first [1].data [63:32]) ;
	 TLPData #(16) tlp16 = TLPData {sof: False,
					eof: True,
					hit: 0,
					be:  be16,
					data: pack(data_vec)};
	 p.put (tlp16);
	 f_cq.deq;
	 rg_dwcount <= 0;
      endrule

      // 'Write' data payload >= 4 DWs remaining
      rule rl_data_4 ((! f_cq.first [0].sop) && (rg_dwcount > 3));
	 Vector#(4, Bit#(32)) data_vec;
	 data_vec[3] = convertDW(f_cq.first [0].data [63:32]) ;
	 data_vec[2] = convertDW(f_cq.first [1].data [31:0]) ;
	 data_vec[1] = convertDW(f_cq.first [1].data [63:32]) ;
	 data_vec[0] = convertDW(gearbox.first [0].data [31:0]) ;
	 TLPData #(16) tlp16 = TLPData {sof: False,
					eof: (rg_dwcount == 4),
					hit: 0,
					be:  '1,
					data: pack(data_vec)};
	 p.put (tlp16);
	 f_cq.deq;
	 rg_dwcount <= rg_dwcount - 4;
      endrule

   endmodule
endinstance

instance ConnectableWithClocks #(Put #(TLPData #(16)), Get #(AxiStCq));
   module mkConnectionWithClocks #(Put #(TLPData #(16)) p, Get #(AxiStCq) g,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);
      mkConnectionWithClocks (g, p, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// ================================================================
// Connecting/converting stream AxiCc <== stream TLPData#(16)

instance ConnectableWithClocks #(Get #(TLPData #(16)), Put #(AxiStCc));
   module mkConnectionWithClocks #(Get #(TLPData #(16)) g, Put #(AxiStCc) p,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);

      // Data path: p <- gearbox_1_2 <- f_tlps <- g

      FIFOF #(TLPData #(16)) f_tlps <- mkPipelineFIFOF (clocked_by slowClock, reset_by slowReset);
      Gearbox #(2, 1, AxiStCc) gearbox <- mkNto1Gearbox (slowClock, slowReset, fastClock, fastReset);

      Reg #(DWCount) rg_dwcount <- mkReg (0, clocked_by slowClock, reset_by slowReset);

      // ----------------
      // f_tlps <== g
      // This step would not be necessary if we could examine the .sof
      // of g.get tlp without dequeueing.
      // TODO: is this redundant with rg_dwcount == 0? i.e., does sof <=> (rg_dwcount == 0)?

      rule rl_get_tlps;    // (slow clock)
	 let tlp <- g.get;
	 f_tlps.enq (tlp);
      endrule

      // ----------------
      // gearbox <== f_tlps    for completion header (slow clock)

      rule rl_header (f_tlps.first.sof);
	 Vector #(2, AxiStCc) v2 = newVector;

	 // this returns the data in the Xilinx byte order
	 match { .cc_desc, .dw } = convertTLP16ToCCDescriptor (f_tlps.first);
	 rg_dwcount <= cc_desc.dwcount - 1;    // since AxiStCC contains first DW

	 v2[0] = AxiStCc {data: pack (cc_desc) [63:0],
			  last: False,
			  keep: 2'b11 };
	 v2[1] = AxiStCc {data: { dw, pack (cc_desc) [95:64] },
			  last: f_tlps.first.eof,
			  keep: 2'b11 };
	 gearbox.enq (v2);
	 f_tlps.deq;
      endrule

      // ----------------
      // gearbox <== f_tlps    for data (slow clock)

      rule rl_data ((! f_tlps.first.sof) && (rg_dwcount != 0));
	 Vector #(2, AxiStCc) v2 = newVector;
	 Bit #(128) x = f_tlps.first.data;
	 v2[0] = AxiStCc {data: { convertDW(x[95:64]), convertDW(x[127:96]) },
			  last: (rg_dwcount <= 2),
			  keep: ((rg_dwcount == 1) ? 2'b01 : 2'b11) };
	 v2[1] = AxiStCc {data: { convertDW(x[31:0]), convertDW(x[63:32]) },
			  last: (rg_dwcount <= 4),
			  keep: ((rg_dwcount <= 2) ? 2'b00
				 : ((rg_dwcount == 3) ? 2'b01 : 2'b11)) };
	 gearbox.enq (v2);
	 rg_dwcount <= ((rg_dwcount < 4) ? 0 : (rg_dwcount - 4));
	 f_tlps.deq;
      endrule

      // ----------------
      // Move out of head of gearbox (fast clock)

      rule rl_fastclock;
	 AxiStCc x = gearbox.first[0];
	 gearbox.deq;
	 // do not propagate empty beats
	 if (x.keep != 0)
	   p.put (x);
      endrule

   endmodule
endinstance

instance ConnectableWithClocks #(Put #(AxiStCc), Get #(TLPData #(16)));
   module mkConnectionWithClocks #(Put #(AxiStCc) p, Get #(TLPData #(16)) g,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);
      mkConnectionWithClocks (g, p, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// ================================================================
// Connecting/converting stream AxiRq <== stream TLPData#(16)

instance ConnectableWithClocks #(Get #(TLPData #(16)), Put #(AxiStRq));
   module mkConnectionWithClocks #(Get #(TLPData #(16)) g, Put #(AxiStRq) p,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);

      // Data path: p <- gearbox_1_2 <- f_tlps <- g

      FIFOF #(TLPData #(16)) f_tlps <- mkPipelineFIFOF (clocked_by slowClock, reset_by   slowReset);
      Gearbox #(2, 1, AxiStRq) gearbox <- mkNto1Gearbox (slowClock, slowReset, fastClock, fastReset);

      Reg #(DWCount)             rg_dwcount <- mkReg (0, clocked_by slowClock, reset_by slowReset);
      // This stores an extra data word, in the converted Xilinx byte order
      Reg #(Maybe #(Bit #(32)))  rg_mdw     <- mkRegU (clocked_by slowClock, reset_by slowReset);

      // Record the first and last BE from header
      // so that we hold the value for the entire packet
      Reg #(Bit# (4))            rg_first_be <- mkRegU (clocked_by slowClock, reset_by slowReset);
      Reg #(Bit# (4))            rg_last_be <- mkRegU (clocked_by slowClock, reset_by slowReset);

      // ----------------
      // Move tlps from g into f_tlps
      // This step would not be necessary if we could examine the .sof
      // of g.get tlp without dequeueing.
      // TODO: is this redundant with rg_dwcount == 0? i.e., does sof <=> (rg_dwcount == 0)?

      rule rl_get_tlps;    // (slow clock)
	 let tlp <- g.get;
	 f_tlps.enq (tlp);
      endrule

      // ----------------
      // Move header into gearbox (slow clock)

      rule rl_header (rg_mdw matches tagged Invalid &&& f_tlps.first.sof);
	 Vector #(2, AxiStRq) v2 = newVector;

	 // this returns the data in the Xilinx byte order
	 match { .rq_desc, .first_be, .last_be, .mdata } = convertTLP16ToRQDescriptor (f_tlps.first);
	 rg_dwcount  <= ((rq_desc.reqtype == MEMORY_WRITE) ? rq_desc.dwcount : 0);
	 rg_mdw      <= mdata;
	 rg_first_be <= first_be;
	 rg_last_be  <= last_be;

	 v2[0] = AxiStRq {data: pack (rq_desc) [63:0],
			  last: False,
			  keep: 2'b11,
			  first_be: first_be,
			  last_be: last_be };
	 v2[1] = AxiStRq {data: pack (rq_desc) [127:64],
			  last: f_tlps.first.eof && !isValid(mdata),
			  keep: 2'b11,
			  first_be: first_be,
			  last_be: last_be };
	 gearbox.enq (v2);
	 f_tlps.deq;
      endrule

      // ----------------
      // Move write-payload into gearbox (slow clock)

      // rg_mdw contains last DW
      rule rl_data_a (rg_mdw matches tagged Valid .dw &&& (rg_dwcount == 1));
	 Vector #(2, AxiStRq) v2 = newVector;
	 v2[0] = AxiStRq {data: { 32'b0, dw },
			  last: True,
			  keep: 2'b01,
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 v2[1] = AxiStRq {data: 0,
			  last: True,
			  keep: 2'b00,
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 gearbox.enq (v2);
	 rg_dwcount <= 0;
	 rg_mdw <= tagged Invalid;
      endrule

      // rg_mdw contains DW, and there are more DWs
      rule rl_data_b (rg_mdw matches tagged Valid .dw &&& (rg_dwcount != 1));
	 Vector #(2, AxiStRq) v2 = newVector;
	 Bit #(128) x = f_tlps.first.data;
	 v2[0] = AxiStRq {data: { convertDW(x[127:96]), dw },
			  last: (rg_dwcount == 2),
			  keep: 2'b11,
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 v2[1] = AxiStRq {data: { convertDW(x[63:32]), convertDW(x[95:64]) },
			  last: (rg_dwcount <= 4),
			  keep: ((rg_dwcount == 2) ? 2'b00
				 : ((rg_dwcount == 3) ? 2'b01 : 2'b11)),
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 gearbox.enq (v2);
	 if (rg_dwcount <= 4) begin
	   rg_dwcount <= 0;
	   rg_mdw <= tagged Invalid;
	 end
         else begin
	   rg_dwcount <= rg_dwcount - 4;
	   rg_mdw <= tagged Valid convertDW(x [31:0]);
         end
	 f_tlps.deq;
      endrule

      // rg_mdw is Invalid, and there are more DWs
      rule rl_data_c (rg_mdw matches tagged Invalid &&& (! f_tlps.first.sof)
                      &&& (rg_dwcount != 0));
	 Vector #(2, AxiStRq) v2 = newVector;
	 Bit #(128) x = f_tlps.first.data;
	 v2[0] = AxiStRq {data: { convertDW(x[95:64]), convertDW(x[127:96]) },
			  last: (rg_dwcount <= 2),
			  keep: ((rg_dwcount == 1) ? 2'b01 : 2'b11),
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 v2[1] = AxiStRq {data: { convertDW(x[31:0]), convertDW(x[63:32]) },
			  last: (rg_dwcount <= 4),
			  keep: ((rg_dwcount <= 2) ? 2'b00
				 : ((rg_dwcount == 3) ? 2'b01 : 2'b11)),
			  first_be: rg_first_be,
			  last_be: rg_last_be };
	 gearbox.enq (v2);
	 rg_dwcount <= ((rg_dwcount < 4) ? 0 : (rg_dwcount - 4));
	 f_tlps.deq;
      endrule

      // ----------------
      // Move out of head of gearbox (fast clock)

      rule rl_fastclock;
	 AxiStRq x = gearbox.first[0];
	 gearbox.deq;
	 // do not propagate empty beats
	 if (x.keep != 0)
	   p.put (x);
      endrule

   endmodule
endinstance

instance ConnectableWithClocks #(Put #(AxiStRq), Get #(TLPData #(16)));
   module mkConnectionWithClocks #( Put #(AxiStRq) p, Get #(TLPData #(16)) g,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);
      mkConnectionWithClocks (g, p, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// ================================================================
// Connecting/converting stream AxiRc ==> stream TLPData#(16)

instance ConnectableWithClocks #(Get #(AxiStRc), Put #(TLPData #(16)));
   module mkConnectionWithClocks #(Get #(AxiStRc) g, Put #(TLPData #(16)) p,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);

      // Data path: g -> gearbox_1_2 -> p

      Reg #(Bool) rg_even_enq     <- mkReg (True, clocked_by fastClock, reset_by fastReset);
      Reg #(Bool) rg_pad_odd_tail <- mkReg (False, clocked_by fastClock, reset_by fastReset);

      // Buffer incoming messages for timing
      FIFO #(AxiStRc) in_buf <- mkFIFO(clocked_by fastClock, reset_by fastReset);
      mkConnection(g, toPut(in_buf));
      let g_buf = toGet(in_buf);

      Gearbox #(1, 2, AxiStRc) gearbox <- mk1toNGearbox (fastClock, fastReset, slowClock, slowReset);

      // ----------------
      // g -> gearbox

      // If eop happens on an even enq, we insert a dummy odd enq, for two reasons:
      // (1) sop is always an even enq, so it is in element [0] of the gearbox output
      // (2) to avoid deadlock/delay: the gearbox output does not show
      //       up until it has received both an even and an odd enq

      // Incoming AxiStRc: g ==> gearbox (fast clock)
      rule rl_g_to_gearbox (! rg_pad_odd_tail);
	 let rc <- g_buf.get;
	 // Assert: rc.sop => rg_even_enq
	 Vector #(1, AxiStRc) v1 = replicate (rc);
	 gearbox.enq (v1);
	 rg_pad_odd_tail <= (rc.eop && rg_even_enq);
	 rg_even_enq     <= ! rg_even_enq;
      endrule

      rule rl_g_to_gearbox_pad_odd_tail (rg_pad_odd_tail);
	 AxiStRc rc = unpack (0);
	 Vector #(1, AxiStRc) v1 = replicate (rc);
	 gearbox.enq (v1);
	 rg_pad_odd_tail <= False;
	 rg_even_enq     <= True;
      endrule

      // ----------------
      // gearbox -> f_cq (slow clock)

      Reg #(DWCount)     rg_dwcount <- mkRegU (clocked_by slowClock, reset_by slowReset);

      // 3DW Header + 1DW data: gearbox ==> p
      rule rl_header (gearbox.first [0].sop);
	 RCDescriptor rc_desc = unpack ({ gearbox.first [1].data [31:0], gearbox.first [0].data });
	 Bit #(32) data = gearbox.first [1].data [63:32];

	 // this takes the data in Xilinx byte order and converts it
	 TLPData #(16) tlp16 = convertRCDescriptorToTLP16 (rc_desc, data);
	 p.put (tlp16);

	 rg_dwcount <= (rc_desc.dwcount == 0) ? 0 : rc_desc.dwcount - 1;
	 gearbox.deq;
      endrule

      // Data payload: gearbox => p
      rule rl_data_a ((! (gearbox.first [0].sop)) && (rg_dwcount != 0));
	 Bit #(16) be16;
	 case (rg_dwcount)
	    1: be16 = 16'hF000;
	    2: be16 = 16'hFF00;
	    3: be16 = 16'hFFF0;
	    default: be16 = 16'hFFFF;
	 endcase
	 Vector#(4, Bit#(32)) data_vec;
	 data_vec[3] = convertDW(gearbox.first [0].data [31:0]);
	 data_vec[2] = convertDW(gearbox.first [0].data [63:32]);
	 data_vec[1] = convertDW(gearbox.first [1].data [31:0]);
	 data_vec[0] = convertDW(gearbox.first [1].data [63:32]);
	 TLPData #(16) tlp16 = TLPData {sof: False,
					eof: (rg_dwcount <= 4),
					hit: 0,
					be: be16,
					data: pack(data_vec)};
	 p.put (tlp16);
	 gearbox.deq;
	 rg_dwcount <= ((rg_dwcount < 4) ? 0 : rg_dwcount - 4);
      endrule

   endmodule
endinstance

instance ConnectableWithClocks #(Put #(TLPData #(16)), Get #(AxiStRc));
   module mkConnectionWithClocks #(Put #(TLPData #(16)) p, Get #(AxiStRc) g,
				   Clock fastClock, Reset fastReset,
				   Clock slowClock, Reset slowReset) (Empty);
      mkConnectionWithClocks (g, p, fastClock, fastReset, slowClock, slowReset);
   endmodule
endinstance

// ================================================================
// Tie-offs for unused interfaces

instance TieOff #(PCIE3_INT_VU);
   module mkTieOff #(PCIE3_INT_VU ifc) (Empty);
      rule tie_off_inputs;
	 ifc.int_vect (0);
	 ifc.pending (0);
      endrule
   endmodule
endinstance

instance TieOff #(PCIE3_INT_MSIX_VU);
   module mkTieOff #(PCIE3_INT_MSIX_VU ifc) (Empty);
      rule tie_off_inputs;
	 ifc.valid (0);
	 ifc.data (0);
	 ifc.address (0);
      endrule
   endmodule
endinstance

// ================================================================
// PCIE3 AXI-Stream Descriptor Formats

typedef struct {
   ReservedZero#(1)         r1;
   TLPAttrIDBasedOrdering   idbased;
   TLPAttrRelaxedOrdering   relaxed;
   TLPAttrNoSnoop           nosnoop;
   TLPTrafficClass          tclass;
   BARAperture              aperture;
   BARID                    barid;
   TargetFunction           targetfn;
   TLPTag                   tag;
   PciId                    reqid;
   ReservedZero#(1)         r2;
   RequestType              reqtype;
   DWCount                  dwcount;
   DWAddress64              address;
   TLPAddressType           addrtype;
} CQDescriptor deriving (Bits, Eq);

typedef struct {
   Bool                     forceecrc;
   TLPAttrIDBasedOrdering   idbased;
   TLPAttrRelaxedOrdering   relaxed;
   TLPAttrNoSnoop           nosnoop;
   TLPTrafficClass          tclass;
   Bool                     compliden;
   PciId                    complid;
   TLPTag                   tag;
   PciId                    reqid;
   ReservedZero#(1)         r1;
   TLPPoison                poisoned;
   TLPCompletionStatus      status;
   DWCount                  dwcount;
   ReservedZero#(2)         r2;
   Bool                     lockedcmpl;
   ByteCount                bytecount;
   ReservedZero#(6)         r3;
   TLPAddressType           addrtype;
   ReservedZero#(1)         r4;
   TLPLowerAddr             loweraddr;
} CCDescriptor deriving (Bits, Eq);

typedef struct {
   Bool                     forceecrc;
   TLPAttrIDBasedOrdering   idbased;
   TLPAttrRelaxedOrdering   relaxed;
   TLPAttrNoSnoop           nosnoop;
   TLPTrafficClass          tclass;
   Bool                     reqiden;
   PciId                    complid;
   TLPTag                   tag;
   PciId                    reqid;
   TLPPoison                poisoned;
   RequestType              reqtype;
   DWCount                  dwcount;
   DWAddress64              address;
   TLPAddressType           addrtype;
} RQDescriptor deriving (Bits, Eq);

typedef struct {
   ReservedZero#(1)         r1;
   TLPAttrIDBasedOrdering   idbased;
   TLPAttrRelaxedOrdering   relaxed;
   TLPAttrNoSnoop           nosnoop;
   TLPTrafficClass          tclass;
   ReservedZero#(1)         r2;
   PciId                    complid;
   TLPTag                   tag;
   PciId                    reqid;
   ReservedZero#(1)         r3;
   TLPPoison                poisoned;
   TLPCompletionStatus      status;
   DWCount                  dwcount;
   ReservedZero#(1)         r4;
   Bool                     reqcompleted;
   Bool                     lockedcmpl;
   ByteCount                bytecount;
   ErrorCode                errcode;
   Bit#(12)                 loweraddr;
} RCDescriptor deriving (Bits, Eq);

// -------------------------

// Conversion functions for PCIE3 AXI-Stream descriptors.  One thing to note
// here is that the TLPData#(n).be field is only ever set in the logic that
// utilizes these functions.  It was a required field for the original PCIE
// design by Xilinx, but is no longer used for PCIE3.  Therefore, the .be
// field will not be assigned in the TLPData#(n) type for traffic going to
// the DMA and CSR blocks.

function TLPData#(16) convertCQDescriptorToTLP16(CQDescriptor desc, Bit#(32) data, TLPFirstDWBE first, TLPLastDWBE last);
   TLPMemoryIO3DWHeader header = defaultValue;
   header.format     = tpl_1(convertCQReqTypeToTLPFmtType(desc.reqtype));
   header.pkttype    = tpl_2(convertCQReqTypeToTLPFmtType(desc.reqtype));
   header.tclass     = desc.tclass;
   header.relaxed    = desc.relaxed;
   header.nosnoop    = desc.nosnoop;
   header.length     = (desc.dwcount == 1024) ? 0 : truncate(desc.dwcount);
   header.reqid      = desc.reqid;
   header.tag        = desc.tag;
   header.lastbe     = last;
   header.firstbe    = first;
   header.addr       = truncate(desc.address);
   header.data       = convertDW(data);

   Bool is3DW = isReadReqType(desc.reqtype);
   Bool is3Or4DW = isReadReqType(desc.reqtype) || (desc.dwcount == 1);

   TLPData#(16) retval = defaultValue;
   retval.sof   = True;
   retval.eof   = is3Or4DW;
   retval.hit   = (1 << pack(desc.barid));
   retval.data  = pack(header);
   retval.be    = (is3DW ? 16'hFFF0 : 16'hFFFF);

   return retval;
endfunction

// this only expects Memory and IO types
function Bool isReadReqType(RequestType t);
   return ((t == MEMORY_READ) || (t == IO_READ));
endfunction

// this only expects Memory and IO types
function Tuple2#(TLPPacketFormat,TLPPacketType) convertCQReqTypeToTLPFmtType(RequestType t);
   case (t)
     MEMORY_READ : return tuple2(MEM_READ_3DW_NO_DATA, MEMORY_READ_WRITE);
     MEMORY_WRITE : return tuple2(MEM_WRITE_3DW_DATA, MEMORY_READ_WRITE);
     IO_READ : return tuple2(MEM_READ_3DW_NO_DATA, IO_REQUEST);
     IO_WRITE : return tuple2(MEM_WRITE_3DW_DATA, IO_REQUEST);
     default : return ?;
   endcase
endfunction

function Tuple2#(CCDescriptor, Bit#(32)) convertTLP16ToCCDescriptor(TLPData#(16) header);
   TLPCompletionHeader cmplheader = unpack(header.data);
   CCDescriptor desc = unpack(0);
   desc.relaxed      = cmplheader.relaxed;
   desc.nosnoop      = cmplheader.nosnoop;
   desc.tclass       = cmplheader.tclass;
   desc.compliden    = False;
   desc.complid      = cmplheader.cmplid;
   desc.tag          = cmplheader.tag;
   desc.reqid        = cmplheader.reqid;
   desc.poisoned     = cmplheader.poison;
   desc.status       = cmplheader.cstatus;
   desc.dwcount      = (cmplheader.length == 0) ? 1024 : zeroExtend(cmplheader.length);
   desc.lockedcmpl   = False;
   desc.bytecount    = (cmplheader.bytecount == 0) ? 4096 : zeroExtend(cmplheader.bytecount);
   desc.loweraddr    = cmplheader.loweraddr;

   return tuple2(desc, convertDW(cmplheader.data));
endfunction

function TLPData#(16) convertRCDescriptorToTLP16(RCDescriptor desc, Bit#(32) data);
   TLPCompletionHeader header = defaultValue;
   header.tclass    = desc.tclass;
   header.relaxed   = desc.relaxed;
   header.nosnoop   = desc.nosnoop;
   header.cmplid    = desc.complid;
   header.tag       = desc.tag;
   header.reqid     = desc.reqid;
   header.poison    = desc.poisoned;
   header.cstatus   = desc.status;
   header.length    = (desc.dwcount == 1024) ? 0 : truncate(desc.dwcount);
   header.bytecount = (desc.bytecount == 4096) ? 0 : truncate(desc.bytecount);
   header.loweraddr = truncate(desc.loweraddr);
   header.data      = convertDW(data);

   Bool is3DW = (desc.dwcount == 0);
   Bool is3Or4DW = (desc.dwcount == 0) || (desc.dwcount == 1);
   TLPData#(16) retval = defaultValue;
   retval.sof   = True;
   retval.eof   = is3Or4DW;
   retval.hit   = 1; // XXX
   retval.data  = pack(header);
   retval.be    = (is3DW ? 16'hFFF0 : 16'hFFFF);

   return retval;
endfunction

function Tuple4 #(RQDescriptor,
		  TLPFirstDWBE,
		  TLPLastDWBE,
		  Maybe#(Bit#(32)))
         convertTLP16ToRQDescriptor (TLPData #(16) header);

   // Note: other than .addr and .data, remaining fields are same for
   // the two header formats below
   TLPMemoryIO3DWHeader header3dw = unpack(header.data);
   TLPMemory4DWHeader   header4dw = unpack(header.data);
   RQDescriptor desc = unpack(0);
   Maybe#(Bit#(32)) data = tagged Invalid;

   desc.relaxed      = header4dw.relaxed;
   desc.nosnoop      = header4dw.nosnoop;
   desc.tclass       = header4dw.tclass;
   desc.reqiden      = False;
   desc.tag          = header4dw.tag;
   desc.reqid        = header4dw.reqid;
   desc.poisoned     = header4dw.poison;
   case(header4dw.format)
      MEM_READ_3DW_NO_DATA: desc.reqtype = MEMORY_READ;
      MEM_READ_4DW_NO_DATA: desc.reqtype = MEMORY_READ;
      MEM_WRITE_3DW_DATA:   desc.reqtype = MEMORY_WRITE;
      MEM_WRITE_4DW_DATA:   desc.reqtype = MEMORY_WRITE;
      default:              desc.reqtype = MEMORY_READ;
   endcase
   desc.dwcount      = (header4dw.length == 0) ? 1024 : zeroExtend(header4dw.length);

   if (header4dw.format == MEM_WRITE_4DW_DATA || header4dw.format == MEM_READ_4DW_NO_DATA) begin
      desc.address      = header4dw.addr;
   end
   else begin
      desc.address      = zeroExtend(header3dw.addr);
      if (header3dw.format == MEM_WRITE_3DW_DATA) begin
	 data           = tagged Valid convertDW(header3dw.data);
      end
   end

   return tuple4 (desc, header4dw.firstbe, header4dw.lastbe, data);
endfunction


// Functions to convert between the byte order inside data words of
// Xilinx AXI packets and PCIe TLP packets

function Bit#(32) convertDW(Bit#(32) dw);
  Vector#(4, Bit#(8)) bytes = unpack(dw);
  return pack(reverse(bytes));
endfunction

// -------------------------

// Buffer to hold at least one maximum size CC packet.
// Max size = 3 Dword header + 1024 Dwords = 1027 Dwords.
// At 64-bit beats, a buffer of 514 beats is needed.
//
module mkCCBuffer(FIFO#(AxiStCc));
  function Bool isEOF(AxiStCc x) = x.last;
  (* hide *)
  let _buf <- mkAXISBuffer(516, isEOF);
  return _buf;
endmodule

// Buffer to hold at least one maximum size RQ packet.
// Max size = 4 Dword header + 256 Dwords = 260 Dwords.
// At 64-bit beats, a buffer of 130 beats is needed.
//
module mkRQBuffer(FIFO#(AxiStRq));
  function Bool isEOF(AxiStRq x) = x.last;
  (* hide *)
  let _buf <- mkAXISBuffer(132, isEOF);
  return _buf;
endmodule

// The timing is tight through the core module, so we try to reduce the
// path length with this wrapper that adds a buffer on both the
// input and output.
module mkAXISBuffer#( Integer depth
                    , function Bool isEOF(t x) )
                   (FIFO#(t))
                   provisos (Bits#(t,tsz), Add#(1, j, tsz));

  (* hide *)
  FIFO#(t)  _core   <- mkAXISBufferCore(depth, isEOF);
  FIFO#(t)  in_buf  <- mkFIFO;
  FIFO#(t)  out_buf <- mkFIFO;

  (* fire_when_enabled *)
  rule moveIn;
    _core.enq(in_buf.first);
    in_buf.deq;
  endrule

  (* fire_when_enabled *)
  rule moveOut;
    out_buf.enq(_core.first);
    _core.deq;
  endrule

  method enq(x)  = in_buf.enq(x);

  method first() = out_buf.first;
  method deq()   = out_buf.deq;

  method Action clear();
    _core.clear;
    in_buf.clear;
    out_buf.clear;
  endmethod

endmodule

module mkAXISBufferCore#( Integer depth
                        , function Bool isEOF(t x) )
                       (FIFO#(t))
                       provisos (Bits#(t,tsz), Add#(1, j, tsz));

  FIFO#(t)      data_buf <- mkSizedBRAMFIFO(depth);
  FIFOF#(void)  eof_buf  <- mkSizedFIFOF(4);

  Bool has_data = eof_buf.notEmpty;

  method Action enq(t x);
    data_buf.enq(x);
    if (isEOF(x))
      eof_buf.enq(?);
  endmethod

  method t first() if (has_data) = data_buf.first;

  method Action deq() if (has_data);
    data_buf.deq;
    if (isEOF(data_buf.first))
      eof_buf.deq();
  endmethod

  method Action clear();
    data_buf.clear;
    eof_buf.clear;
  endmethod

endmodule

// -------------------------

endpackage: XilinxVirtexUltraScalePCIE
