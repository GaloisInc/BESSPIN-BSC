// Copyright (c) 2008- 2016 Bluespec, Inc.  All rights reserved.
// $Revision: 34889 $
// $Date: 2016-12-07 08:07:03 -0500 (Wed, 07 Dec 2016) $

package SceMiVirtexUltraScalePCIE;

// This is an implementation of SceMi over PCI-Express for Virtex UltraScale
// FPGAs.

import Clocks       :: *;
import Vector       :: *;
import Connectable  :: *;
import TieOff       :: *;
import DefaultValue :: *;
import XilinxPCIE   :: *;
import XilinxCells  :: *;
import BlueNoC      :: *;
import ClientServer :: *;
import XilinxClocks :: *;

import SceMiDefines   :: *;
import SceMiInternals :: *;
import SceMiNoC       :: *;

// Interface wrapper for PCIE
interface SceMiVUPCIEIfc#(type i, numeric type lanes);
   interface i                orig_ifc;
   interface PCIE_EXP#(lanes) pcie;
   interface MsgPort#(BPB)    noc_cont;
   (* always_ready *)
   method Bool isLinkUp();
   (* always_ready *)
   method Bool isOutOfReset();
   (* always_ready *)
   method Bool isClockAdvancing();
   (* always_enabled, always_ready *)
   method Action isDDRReady(Bool i);
endinterface

// Argument structure used for passing in PCIE clocks
typedef struct {
   Clock         pci_sys_clk_p;
   Clock         pci_sys_clk_n;
   Reset         pci_sys_reset;
   Real          clock_period;
   SceMiLinkType link_type;
} SceMiVUPCIE3Args;

typedef struct {
   Clock         pci_sys_clk_p;
   Clock         pci_sys_clk_n;
   Reset         pci_sys_reset;
   Clock         ref_clk;
   SceMiLinkType link_type;
} SceMiVUPCIE3NoClkArgs;

typedef struct {
   Clock         pci_sys_clk_p;
   Clock         pci_sys_clk_n;
   Reset         pci_sys_reset;
   Clock         ref_clk;
   SceMiLinkType link_type;
} SceMiVUPCIE3NoClkArgsNoClock;

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIE3VU#( SceMiModule#(i) mod
                                  , Clock pci_sys_clk_p
                                  , Clock pci_sys_clk_n
                                  , Reset pci_sys_reset
                                  , Real  clock_period
                                  , SceMiLinkType link_type
                                  )
                                  (SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   // Buffer clocks and reset before they are used
   Vector#(2, Clock) pci_sys_clks_buf <- mkClockIBUFDS_GTE3(True, pci_sys_clk_p, pci_sys_clk_n);
   Clock pci_sys_clk_gt_buf = pci_sys_clks_buf[0];
   Clock pci_sys_clk_buf    = pci_sys_clks_buf[1];

   // Instantiate the PCIE endpoint
   PCIEParams params = defaultValue;
   params.clock_period = clock_period;
   PCIExpress3VU#(lanes) _ep <- mkPCIExpress3EndpointVU( params
                                                       , pci_sys_clk_gt_buf
                                                       , clocked_by pci_sys_clk_buf
                                                       , reset_by pci_sys_reset
                                                       );

   mkTieOff(_ep.cfg_interrupt);

   // The PCIE endpoint is processing TLPWord#(8)s at 250MHz.  The
   // NoC bridge is accepting TLPWord#(16)s at 125 MHz. The
   // connection between the endpoint and the NoC contains GearBox
   // instances for the TLPWord#(8)@250 <--> TLPWord#(16)@125
   // conversion.

   // The PCIe endpoint exports full (250MHz) and half-speed (125MHz) clocks
   Clock epClock250 = _ep.uclk;
   Reset epReset250 = _ep.ureset;
   Clock epClock125 = _ep.uclk_half;
   Reset epReset125 = _ep.ureset_half;

   // Extract some status info from the PCIE endpoint. These values are
   // all in the epClock250 domain, so we have to cross them into the
   // epClock125 domain.

   Bool link_is_up = _ep.status.lnk_up();
   UInt#(13) max_read_req_bytes_250       = 128 << _ep.status.max_read_req;
   UInt#(13) max_payload_bytes_250        = 128 << _ep.status.max_payload;
   UInt#(8)  read_completion_boundary_250 = 64 << _ep.status.rcb_status[0];
   Bool      msix_enable_250              = (_ep.cfg_interrupt_msix.enabled()[0] == 1);
   Bool      msix_masked_250              = (_ep.cfg_interrupt_msix.mask()[0]    == 1);

   CrossingReg#(UInt#(13)) max_rd_req_cr  <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(13)) max_payload_cr <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(8))  rcb_cr         <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_enable_cr <- mkNullCrossingReg(epClock125, False, clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_masked_cr <- mkNullCrossingReg(epClock125, True,  clocked_by epClock250, reset_by epReset250);

   Reg#(UInt#(13)) max_read_req_bytes <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(UInt#(13)) max_payload_bytes  <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(Bit#(7))   rcb_mask           <- mkReg(7'h3f, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_enable        <- mkReg(False, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_masked        <- mkReg(True,  clocked_by epClock125, reset_by epReset125);

   Wire#(Bool)     memory_enabled     <- mkBypassWire(clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule cross_config_values;
      max_rd_req_cr  <= max_read_req_bytes_250;
      max_payload_cr <= max_payload_bytes_250;
      rcb_cr         <= read_completion_boundary_250;
      msix_enable_cr <= msix_enable_250;
      msix_masked_cr <= msix_masked_250;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule register_config_values;
      max_read_req_bytes <= max_rd_req_cr.crossed();
      max_payload_bytes  <= max_payload_cr.crossed();
      rcb_mask           <= (rcb_cr.crossed() == 64) ? 7'h3f : 7'h7f;
      msix_enable        <= msix_enable_cr.crossed();
      msix_masked        <= msix_masked_cr.crossed();
   endrule

   CrossingReg#(Bool) intr_on <- mkNullCrossingReg( epClock125, False, clocked_by epClock250, reset_by epReset250 );

   // setup PCIe interrupt for MSI-X
   // this rule executes in the epClock250 domain
   (* fire_when_enabled, no_implicit_conditions *)
   rule intr_ifc_ctl;
      // tie off the unused portion of the MSI-X interface
      _ep.cfg_interrupt_msix.valid (0);
      _ep.cfg_interrupt_msix.address (0);
      _ep.cfg_interrupt_msix.data (0);

      intr_on <= msix_enable_cr && (_ep.status.function_status()[2] == 1);
   endrule: intr_ifc_ctl

   // Build the PCIe-to-NoC bridge
   PCIE3toBNoCFull#(BPB) bridge   <- mkPCIE3toBNoCFull_4( 64'h05ce_0006_0008_0000
 						       , max_read_req_bytes
						       , max_payload_bytes
						       , rcb_mask
						       , msix_enable
						       , msix_masked
						       , False // no MSI, only MSI-X
						       , clocked_by epClock125, reset_by epReset125
						       );

   let cq <- mkConnectionWithClocks (_ep.cq_recv, bridge.cq_tlps, epClock250, epReset250, epClock125, epReset125);
   let cc <- mkConnectionWithClocks (_ep.cc_xmit, bridge.cc_tlps, epClock250, epReset250, epClock125, epReset125);

   let rq <- mkConnectionWithClocks (_ep.rq_xmit, bridge.rq_tlps, epClock250, epReset250, epClock125, epReset125);
   let rc <- mkConnectionWithClocks (_ep.rc_recv, bridge.rc_tlps, epClock250, epReset250, epClock125, epReset125);

   mkConnection (_ep.clks.request, bridge.clocks.request);
   mkConnection (_ep.clks.response, bridge.clocks.response);

   Clock scemiClock = _ep.cclk;

   // Propagate loss-of-partner as a reset to the SCE-MI
   MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule reset_scemi_if_network_is_inactive if (!bridge.is_activated());
      network_status.assertReset();
   endrule

   Reset scemiReset = network_status.new_rst;

   // Build the design with a NoC connection
   SceMiNoCIfc#(i) _dut <- buildSceMiNoC( mod
                                        , epClock125
                                        , epReset125
                                        , bridge.is_activated()
                                        , link_type
                                        , clocked_by scemiClock
                                        , reset_by scemiReset
                                        );
   mkConnection (_dut.noc_src, bridge.noc);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_pcie_link_up;
      bridge.status_pcie_link_is_up(_ep.status.lnk_up);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_bluenoc_link_up;
      bridge.status_bluenoc_link_is_up(bridge.is_activated());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_interrupts_enabled;
      bridge.status_interrupts_enabled(intr_on.crossed());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_memory_enabled;
      bridge.status_memory_enabled(memory_enabled);
   endrule

   ReadOnly#(Bool) wIsOutOfReset <- mkNullCrossing(epClock125, _dut.isOutOfReset, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_out_of_reset;
      bridge.status_out_of_reset(wIsOutOfReset);
   endrule

   // Pass along the required interface elements
   interface orig_ifc = _dut.orig_ifc;
   interface pcie     = _ep.pcie;
   interface noc_cont = _dut.noc_cont;
   method Bool isLinkUp         = link_is_up;
   method Bool isOutOfReset     = _dut.isOutOfReset;
   method Bool isClockAdvancing = _dut.isClockAdvancing;
   method Action isDDRReady(i)  = memory_enabled._write(i);
endmodule: buildSceMiPCIE3VU

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIE3NoClkVU#( SceMiModule#(i) mod
                                  , Clock pci_sys_clk_p
                                  , Clock pci_sys_clk_n
                                  , Reset pci_sys_reset
                                  , Clock ref_clk
                                  , SceMiLinkType link_type
                                  )
                                  (SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   // Buffer clocks and reset before they are used
   Vector#(2, Clock) pci_sys_clks_buf <- mkClockIBUFDS_GTE3(True, pci_sys_clk_p, pci_sys_clk_n);
   Clock pci_sys_clk_gt_buf = pci_sys_clks_buf[0];
   Clock pci_sys_clk_buf    = pci_sys_clks_buf[1];

   // Instantiate the PCIE endpoint
   PCIEParams params = defaultValue;
   PCIExpress3VU#(lanes) _ep <- mkPCIExpress3EndpointNoClkVU( params
                                                       , pci_sys_clk_gt_buf
                                                       , clocked_by pci_sys_clk_buf
                                                       , reset_by pci_sys_reset
                                                       );

   mkTieOff(_ep.cfg_interrupt);

   // The PCIE endpoint is processing TLPWord#(8)s at 250MHz.  The
   // NoC bridge is accepting TLPWord#(16)s at 125 MHz. The
   // connection between the endpoint and the NoC contains GearBox
   // instances for the TLPWord#(8)@250 <--> TLPWord#(16)@125
   // conversion.

   // The PCIe endpoint exports full (250MHz) and half-speed (125MHz) clocks
   Clock epClock250 = _ep.uclk;
   Reset epReset250 = _ep.ureset;
   Clock epClock125 = _ep.uclk_half;
   Reset epReset125 = _ep.ureset_half;

   // Extract some status info from the PCIE endpoint. These values are
   // all in the epClock250 domain, so we have to cross them into the
   // epClock125 domain.

   Bool link_is_up = _ep.status.lnk_up();
   UInt#(13) max_read_req_bytes_250       = 128 << _ep.status.max_read_req;
   UInt#(13) max_payload_bytes_250        = 128 << _ep.status.max_payload;
   UInt#(8)  read_completion_boundary_250 = 64 << _ep.status.rcb_status[0];
   Bool      msix_enable_250              = (_ep.cfg_interrupt_msix.enabled()[0] == 1);
   Bool      msix_masked_250              = (_ep.cfg_interrupt_msix.mask()[0]    == 1);

   CrossingReg#(UInt#(13)) max_rd_req_cr  <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(13)) max_payload_cr <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(8))  rcb_cr         <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_enable_cr <- mkNullCrossingReg(epClock125, False, clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_masked_cr <- mkNullCrossingReg(epClock125, True,  clocked_by epClock250, reset_by epReset250);

   Reg#(UInt#(13)) max_read_req_bytes <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(UInt#(13)) max_payload_bytes  <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(Bit#(7))   rcb_mask           <- mkReg(7'h3f, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_enable        <- mkReg(False, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_masked        <- mkReg(True,  clocked_by epClock125, reset_by epReset125);

   Wire#(Bool)     memory_enabled     <- mkBypassWire(clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule cross_config_values;
      max_rd_req_cr  <= max_read_req_bytes_250;
      max_payload_cr <= max_payload_bytes_250;
      rcb_cr         <= read_completion_boundary_250;
      msix_enable_cr <= msix_enable_250;
      msix_masked_cr <= msix_masked_250;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule register_config_values;
      max_read_req_bytes <= max_rd_req_cr.crossed();
      max_payload_bytes  <= max_payload_cr.crossed();
      rcb_mask           <= (rcb_cr.crossed() == 64) ? 7'h3f : 7'h7f;
      msix_enable        <= msix_enable_cr.crossed();
      msix_masked        <= msix_masked_cr.crossed();
   endrule

   CrossingReg#(Bool) intr_on <- mkNullCrossingReg( epClock125, False, clocked_by epClock250, reset_by epReset250 );

   // setup PCIe interrupt for MSI-X
   // this rule executes in the epClock250 domain
   (* fire_when_enabled, no_implicit_conditions *)
   rule intr_ifc_ctl;
      // tie off the unused portion of the MSI-X interface
      _ep.cfg_interrupt_msix.valid (0);
      _ep.cfg_interrupt_msix.address (0);
      _ep.cfg_interrupt_msix.data (0);

      intr_on <= msix_enable_cr && (_ep.status.function_status()[2] == 1);
   endrule: intr_ifc_ctl

   // Build the PCIe-to-NoC bridge
   PCIE3toBNoCFull#(BPB) bridge   <- mkPCIE3toBNoCFull_4( 64'h05ce_0006_0008_0000
 						       , max_read_req_bytes
						       , max_payload_bytes
						       , rcb_mask
						       , msix_enable
						       , msix_masked
						       , False // no MSI, only MSI-X
						       , clocked_by epClock125, reset_by epReset125
						       );

   let cq <- mkConnectionWithClocks (_ep.cq_recv, bridge.cq_tlps, epClock250, epReset250, epClock125, epReset125);
   let cc <- mkConnectionWithClocks (_ep.cc_xmit, bridge.cc_tlps, epClock250, epReset250, epClock125, epReset125);

   let rq <- mkConnectionWithClocks (_ep.rq_xmit, bridge.rq_tlps, epClock250, epReset250, epClock125, epReset125);
   let rc <- mkConnectionWithClocks (_ep.rc_recv, bridge.rc_tlps, epClock250, epReset250, epClock125, epReset125);

   mkConnection (_ep.clks.request, bridge.clocks.request);
   mkConnection (_ep.clks.response, bridge.clocks.response);

   Clock scemiClock = ref_clk;

   // Propagate loss-of-partner as a reset to the SCE-MI
   MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule reset_scemi_if_network_is_inactive if (!bridge.is_activated());
      network_status.assertReset();
   endrule

   Reset scemiReset = network_status.new_rst;

   // Build the design with a NoC connection
   SceMiNoCIfc#(i) _dut <- buildSceMiNoC( mod
                                        , epClock125
                                        , epReset125
                                        , bridge.is_activated()
                                        , link_type
                                        , clocked_by scemiClock
                                        , reset_by scemiReset
                                        );
   mkConnection (_dut.noc_src, bridge.noc);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_pcie_link_up;
      bridge.status_pcie_link_is_up(_ep.status.lnk_up);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_bluenoc_link_up;
      bridge.status_bluenoc_link_is_up(bridge.is_activated());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_interrupts_enabled;
      bridge.status_interrupts_enabled(intr_on.crossed());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_memory_enabled;
      bridge.status_memory_enabled(memory_enabled);
   endrule

   ReadOnly#(Bool) wIsOutOfReset <- mkNullCrossing(epClock125, _dut.isOutOfReset, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_out_of_reset;
      bridge.status_out_of_reset(wIsOutOfReset);
   endrule

   // Pass along the required interface elements
   interface orig_ifc = _dut.orig_ifc;
   interface pcie     = _ep.pcie;
   interface noc_cont = _dut.noc_cont;
   method Bool isLinkUp         = link_is_up;
   method Bool isOutOfReset     = _dut.isOutOfReset;
   method Bool isClockAdvancing = _dut.isClockAdvancing;
   method Action isDDRReady(i)  = memory_enabled._write(i);
endmodule: buildSceMiPCIE3NoClkVU

(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIE3NoClkVUNoClock#( SceMiModule#(i) mod
                                  , Clock pci_sys_clk_p
                                  , Clock pci_sys_clk_n
                                  , Reset pci_sys_reset
                                  , Clock ref_clk
                                  , SceMiLinkType link_type
                                  )
                                  (SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   // Buffer clocks and reset before they are used
   Vector#(2, Clock) pci_sys_clks_buf <- mkClockIBUFDS_GTE3(True, pci_sys_clk_p, pci_sys_clk_n);
   Clock pci_sys_clk_gt_buf = pci_sys_clks_buf[0];
   Clock pci_sys_clk_buf    = pci_sys_clks_buf[1];

   // Instantiate the PCIE endpoint
   PCIEParams params = defaultValue;
   PCIExpress3VU#(lanes) _ep <- mkPCIExpress3EndpointNoClkVU( params
                                                       , pci_sys_clk_gt_buf
                                                       , clocked_by pci_sys_clk_buf
                                                       , reset_by pci_sys_reset
                                                       );

   mkTieOff(_ep.cfg_interrupt);

   // The PCIE endpoint is processing TLPWord#(8)s at 250MHz.  The
   // NoC bridge is accepting TLPWord#(16)s at 125 MHz. The
   // connection between the endpoint and the NoC contains GearBox
   // instances for the TLPWord#(8)@250 <--> TLPWord#(16)@125
   // conversion.

   // The PCIe endpoint exports full (250MHz) and half-speed (125MHz) clocks
   Clock epClock250 = _ep.uclk;
   Reset epReset250 = _ep.ureset;
   Clock epClock125 = _ep.uclk_half;
   Reset epReset125 = _ep.ureset_half;

   // Extract some status info from the PCIE endpoint. These values are
   // all in the epClock250 domain, so we have to cross them into the
   // epClock125 domain.

   Bool link_is_up = _ep.status.lnk_up();
   UInt#(13) max_read_req_bytes_250       = 128 << _ep.status.max_read_req;
   UInt#(13) max_payload_bytes_250        = 128 << _ep.status.max_payload;
   UInt#(8)  read_completion_boundary_250 = 64 << _ep.status.rcb_status[0];
   Bool      msix_enable_250              = (_ep.cfg_interrupt_msix.enabled()[0] == 1);
   Bool      msix_masked_250              = (_ep.cfg_interrupt_msix.mask()[0]    == 1);

   CrossingReg#(UInt#(13)) max_rd_req_cr  <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(13)) max_payload_cr <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(UInt#(8))  rcb_cr         <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_enable_cr <- mkNullCrossingReg(epClock125, False, clocked_by epClock250, reset_by epReset250);
   CrossingReg#(Bool)      msix_masked_cr <- mkNullCrossingReg(epClock125, True,  clocked_by epClock250, reset_by epReset250);

   Reg#(UInt#(13)) max_read_req_bytes <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(UInt#(13)) max_payload_bytes  <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
   Reg#(Bit#(7))   rcb_mask           <- mkReg(7'h3f, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_enable        <- mkReg(False, clocked_by epClock125, reset_by epReset125);
   Reg#(Bool)      msix_masked        <- mkReg(True,  clocked_by epClock125, reset_by epReset125);

   Wire#(Bool)     memory_enabled     <- mkBypassWire(clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule cross_config_values;
      max_rd_req_cr  <= max_read_req_bytes_250;
      max_payload_cr <= max_payload_bytes_250;
      rcb_cr         <= read_completion_boundary_250;
      msix_enable_cr <= msix_enable_250;
      msix_masked_cr <= msix_masked_250;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule register_config_values;
      max_read_req_bytes <= max_rd_req_cr.crossed();
      max_payload_bytes  <= max_payload_cr.crossed();
      rcb_mask           <= (rcb_cr.crossed() == 64) ? 7'h3f : 7'h7f;
      msix_enable        <= msix_enable_cr.crossed();
      msix_masked        <= msix_masked_cr.crossed();
   endrule

   CrossingReg#(Bool) intr_on <- mkNullCrossingReg( epClock125, False, clocked_by epClock250, reset_by epReset250 );

   // setup PCIe interrupt for MSI-X
   // this rule executes in the epClock250 domain
   (* fire_when_enabled, no_implicit_conditions *)
   rule intr_ifc_ctl;
      // tie off the unused portion of the MSI-X interface
      _ep.cfg_interrupt_msix.valid (0);
      _ep.cfg_interrupt_msix.address (0);
      _ep.cfg_interrupt_msix.data (0);

      intr_on <= msix_enable_cr && (_ep.status.function_status()[2] == 1);
   endrule: intr_ifc_ctl

   // Build the PCIe-to-NoC bridge
   PCIE3toBNoCFull#(BPB) bridge   <- mkPCIE3toBNoCFull_4( 64'h05ce_0006_0008_0000
 						       , max_read_req_bytes
						       , max_payload_bytes
						       , rcb_mask
						       , msix_enable
						       , msix_masked
						       , False // no MSI, only MSI-X
						       , clocked_by epClock125, reset_by epReset125
						       );

   let cq <- mkConnectionWithClocks (_ep.cq_recv, bridge.cq_tlps, epClock250, epReset250, epClock125, epReset125);
   let cc <- mkConnectionWithClocks (_ep.cc_xmit, bridge.cc_tlps, epClock250, epReset250, epClock125, epReset125);

   let rq <- mkConnectionWithClocks (_ep.rq_xmit, bridge.rq_tlps, epClock250, epReset250, epClock125, epReset125);
   let rc <- mkConnectionWithClocks (_ep.rc_recv, bridge.rc_tlps, epClock250, epReset250, epClock125, epReset125);

   mkConnection (_ep.clks.request, bridge.clocks.request);
   mkConnection (_ep.clks.response, bridge.clocks.response);

   Clock scemiClock = ref_clk;

   // Propagate loss-of-partner as a reset to the SCE-MI
   MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule reset_scemi_if_network_is_inactive if (!bridge.is_activated());
      network_status.assertReset();
   endrule

   Reset scemiReset = network_status.new_rst;

   // Build the design with a NoC connection
   SceMiNoCIfc#(i) _dut <- buildSceMiNoCNoClock( mod
                                        , epClock125
                                        , epReset125
                                        , bridge.is_activated()
                                        , link_type
                                        , clocked_by scemiClock
                                        , reset_by scemiReset
                                        );
   mkConnection (_dut.noc_src, bridge.noc);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_pcie_link_up;
      bridge.status_pcie_link_is_up(_ep.status.lnk_up);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_bluenoc_link_up;
      bridge.status_bluenoc_link_is_up(bridge.is_activated());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_interrupts_enabled;
      bridge.status_interrupts_enabled(intr_on.crossed());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_memory_enabled;
      bridge.status_memory_enabled(memory_enabled);
   endrule

   ReadOnly#(Bool) wIsOutOfReset <- mkNullCrossing(epClock125, _dut.isOutOfReset, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_out_of_reset;
      bridge.status_out_of_reset(wIsOutOfReset);
   endrule

   // Pass along the required interface elements
   interface orig_ifc = _dut.orig_ifc;
   interface pcie     = _ep.pcie;
   interface noc_cont = _dut.noc_cont;
   method Bool isLinkUp         = link_is_up;
   method Bool isOutOfReset     = _dut.isOutOfReset;
   method Bool isClockAdvancing = _dut.isClockAdvancing;
   method Action isDDRReady(i)  = memory_enabled._write(i);
endmodule: buildSceMiPCIE3NoClkVUNoClock

endpackage: SceMiVirtexUltraScalePCIE
