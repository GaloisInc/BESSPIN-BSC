// Copyright (c) 2018 Bluespec, Inc.  All rights reserved.

package SceMiArria10PCIE;

// This is an implementation of BlueNoC over PCI-Express for the Arria 10GX
// FPGA

import Clocks       :: *;
import Connectable  :: *;
import TieOff       :: *;
import DefaultValue :: *;
import XilinxPCIE   :: *;
import BlueNoC      :: *;
import SceMiNoC     :: *;
import SceMiDefines :: *;
import SceMiInternals :: *;

import Arria10PCIE  :: *;
import ArriaBVI     ::*;

typedef 4 BPB;

// Interface wrapper for PCIE
interface SceMiA10PCIEIfc#(type i, numeric type lanes);
   interface i     orig_ifc;
   interface Pcie  pcie;
   interface MsgPort#(BPB) noc_cont;
   interface Clock ep125_clk;
   interface Reset ep125_rst;
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
   Clock         ref_clk;
   Reset         pin_perst;
   Real          clock_period;
   SceMiLinkType link_type;
} SceMiA10PCIEArgs;

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIEA10#(  SceMiModule#(i) mod
				   , Clock    ref_clk
				   , Reset    pin_perst
				   , Real     clock_period
				   , SceMiLinkType link_type
				   )
				   (SceMiA10PCIEIfc#(i, lanes));

   if (valueOf(lanes) != 8)
      errorM("Only 8-lane PCIe is supported on A10.");

   // Instantiate the PCIE endpoint
   PCIExpressA#(lanes) _ep <- mkPCIExpressEndpointA( ref_clk, pin_perst );

   // note our PCI ID
   PciId my_id = _ep.trn.pci_id;

   Clock epClock125 = _ep.trn.clk;
   Reset epReset125 = _ep.trn.reset_n;

   // Extract some status info from the PCIE endpoint:
   Bool link_is_up = _ep.trn.link_up();
   UInt#(13) max_read_req_bytes       = 128 << _ep.trn.max_rd_req_bytes;
   UInt#(13) max_payload_bytes        = 128 << _ep.trn.max_payload_bytes;
   Bit#(7)   rcb_mask                 = (_ep.trn.read_completion_boundary ? 7'h7F : 7'h3F);
   Bool      msix_enable              = _ep.trn.msix_enable;
   Bool      msix_masked              = _ep.trn.msix_mask;
   Wire#(Bool)     memory_enabled     <- mkBypassWire(clocked_by epClock125, reset_by epReset125);

   // Build the PCIe-to-NoC bridge
   PCIEtoBNoCFull#(BPB) bbridge <- mkPCIEtoBNoCFull_4(  64'h49 // content_id
						      , my_id
						      , max_read_req_bytes
						      , max_payload_bytes
						      , rcb_mask
						      , msix_enable
						      , msix_masked
						      , False // no MSI, only MSI-X
						      , clocked_by epClock125
						      , reset_by epReset125
						      );
   let rx <- mkEPTOBNoC(tpl_2(bbridge.tlps), clocked_by epClock125, reset_by epReset125);
   mkConnection(rx, _ep.trn_rx, clocked_by epClock125, reset_by epReset125);
   let tx <- mkBNoCToEP(tpl_1(bbridge.tlps), clocked_by epClock125, reset_by epReset125);
   mkConnection(_ep.trn_tx, tx, clocked_by epClock125, reset_by epReset125);

   // Create Sce-Mi refclock:
   let iopll <- mkSceMiClock(clock_period, clocked_by epClock125, reset_by epReset125);
   Clock scemiClock = iopll.clkout;

   // Propagate loss-of-partner as a reset to the SCE-MI
   MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock125, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule reset_scemi_if_network_is_inactive if (!(bbridge.is_activated() && iopll.locked));
      network_status.assertReset();
   endrule

   Reset scemiReset = network_status.new_rst;

   // Build the design with a NoC connection
   SceMiNoCIfc#(i) _dut <- buildSceMiNoC( mod
                                        , epClock125
                                        , epReset125
                                        , bbridge.is_activated()
                                        , link_type
                                        , clocked_by scemiClock
                                        , reset_by scemiReset
                                        );
   mkConnection(_dut.noc_src,bbridge.noc);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_pcie_link_up;
      bbridge.status_pcie_link_is_up(_ep.trn.link_up);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_bluenoc_link_up;
      bbridge.status_bluenoc_link_is_up(bbridge.is_activated());
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_interrupts_enabled;
      bbridge.status_interrupts_enabled(msix_enable);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_memory_enabled;
      bbridge.status_memory_enabled(memory_enabled);
   endrule

   ReadOnly#(Bool) wIsOutOfReset <- mkNullCrossing(epClock125, _dut.isOutOfReset, reset_by epReset125);

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_status_out_of_reset;
      bbridge.status_out_of_reset(wIsOutOfReset);
   endrule

   // Pass along the required interface elements
   interface orig_ifc = _dut.orig_ifc;
   interface pcie     = _ep.pcie;
   interface noc_cont = _dut.noc_cont;
   interface ep125_clk = epClock125;
   interface ep125_rst = epReset125;
   method Bool isLinkUp         = link_is_up;
   method Bool isOutOfReset     = _dut.isOutOfReset;
   method Bool isClockAdvancing = _dut.isClockAdvancing;
   method Action isDDRReady(i)  = memory_enabled._write(i);
endmodule

(*always_ready*)
interface IOPll;
   interface Clock clkout;
   method    Bool  locked;
endinterface

import "BVI" vArriaSceMiClock =
module mkSceMiClock#(parameter Real outClockPeriod)(IOPll);
   let hi = outClockPeriod/2;

   default_clock clk(CLK);
   default_reset rstn(RST_N);

   parameter hi = hi;
   parameter lo = hi;
   parameter outClockPeriod = realToString(outClockPeriod) + " ns";

   output_clock clkout(CLK_clkout);
   method locked locked;
   schedule locked CF locked;
endmodule

endpackage
