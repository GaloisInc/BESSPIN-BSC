// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiVirtex5PCIE;

// This is an implementation of SceMi over PCI-Express for Virtex 5
// FPGAs.

import Clocks       :: *;
import Connectable  :: *;
import TieOff       :: *;
import DefaultValue :: *;
import XilinxPCIE   :: *;
import XilinxVirtex5PCIE::*;
import XilinxCells  :: *;
import BlueNoC      :: *;

import SceMiDefines   :: *;
import SceMiInternals :: *;
import SceMiNoC       :: *;

// Interface wrapper for PCIE
interface SceMiV5PCIEIfc#(type i, numeric type lanes);
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
   Clock         ref_clk;
   SceMiLinkType link_type;
} SceMiV5PCIEArgs;

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIEV5#( SceMiModule#(i) mod
                                 , Clock pci_sys_clk_p
                                 , Clock pci_sys_clk_n
                                 , Reset pci_sys_reset
                                 , Clock ref_clk
                                 , SceMiLinkType link_type
                                 )
                                 (SceMiV5PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex5PCIE#(lanes));

   // Buffer clocks and reset before they are used
   Clock sys_clk_buf <- mkClockIBUFDS(defaultValue, pci_sys_clk_p, pci_sys_clk_n);

   // Instantiate the PCIE endpoint
   PCIExpress#(lanes) _ep <- mkPCIExpressEndpoint( defaultValue
                                                 , clocked_by sys_clk_buf
                                                 , reset_by pci_sys_reset
                                                 );
   mkTieOff(_ep.cfg);
   mkTieOff(_ep.cfg_err);

   // note our PCI ID
   PciId my_id = PciId { bus:  _ep.cfg.bus_number()
                       , dev:  _ep.cfg.device_number()
                       , func: _ep.cfg.function_number()
                       };

   i original_ifc       = ?;
   Bool out_of_reset    = False;
   Bool clock_advancing = False;
   SceMiNoCIfc#(i) _dut = ?;
   
   Wire#(Bool) memory_enabled = ?;
   
   if (valueOf(lanes) == 1) begin
      // The PCIE endpoint is processing TLPWord#(8)s at 62.5MHz.  The
      // PCIE_to_PIO converter works with TLPWord#(16)s at 62.5MHz
      // on one side and presents a PIO interface at uClock speeds on
      // the other.  The connection between the endpoint and the
      // converter is simple in this case and does not require a
      // GearBox, only a TLPData#(16) <--> TLPData#(8) conversion.

      // The PCIe endpoint exports the full (62.5MHz) clock, and clk2 is unconnected.
      Clock epClock62 = _ep.trn.clk;
      Reset epReset62 <- mkAsyncReset(4, _ep.trn.reset_n, epClock62);

      // Extract some status info from the PCIE endpoint. These values are
      // all in the epClock62 domain, so they can be registered without
      // a clock domain crossing.

      Reg#(UInt#(13)) max_read_req_bytes <- mkReg(128,   clocked_by epClock62, reset_by epReset62);
      Reg#(UInt#(13)) max_payload_bytes  <- mkReg(128,   clocked_by epClock62, reset_by epReset62);
      Reg#(Bit#(7))   rcb_mask           <- mkReg(7'h3f, clocked_by epClock62, reset_by epReset62);
      Reg#(Bool)      msi_enable         <- mkReg(False, clocked_by epClock62, reset_by epReset62);
      Reg#(Bool)      bus_master_enable  <- mkReg(False, clocked_by epClock62, reset_by epReset62);
                      memory_enabled     <- mkBypassWire(clocked_by epClock62, reset_by epReset62);
      Reg#(Bool)      intr_on            <- mkReg(False, clocked_by epClock62, reset_by epReset62);
      
      (* fire_when_enabled, no_implicit_conditions *)
      rule register_config_values;
         max_read_req_bytes <= 128 << _ep.cfg.dcommand[14:12];
         max_payload_bytes  <= 128 << _ep.cfg.dcommand[7:5];
         rcb_mask           <= (_ep.cfg.lcommand[3] == 0) ? 7'h3f : 7'h7f;
         msi_enable         <= (_ep.cfg_irq.interrupt_msienable() == 1)
                            && (_ep.cfg_irq.interrupt_mmenable == 0);
         bus_master_enable  <= (_ep.cfg.command[2] == 1);
      endrule

      // setup PCIe interrupt for MSI
      // this rule executes in the epClock62 domain
      (* fire_when_enabled, no_implicit_conditions *)
      rule intr_ifc_ctl;
         _ep.cfg_irq.interrupt_di('0);        // tied off for single-vector MSI
         _ep.cfg_irq.interrupt_assert_n('1);  // tied off for MSI
	 intr_on <= msi_enable && bus_master_enable;
      endrule: intr_ifc_ctl

      // Build the PCIe-to-NoC bridge
      PCIEtoBNoCFull#(BPB)   bridge <-   mkPCIEtoBNoCFull_4( 64'h05ce_0006_0008_0000
							   , my_id
                                              		   , max_read_req_bytes
							   , max_payload_bytes
                                              		   , rcb_mask
							   , False // no MSI-X, only MSI
                                              		   , True
							   , msi_enable
                                              		   , clocked_by epClock62, reset_by epReset62
							   );
      mkConnection(_ep.trn_rx, tpl_2(bridge.tlps), clocked_by epClock62, reset_by epReset62);
      mkConnection(_ep.trn_tx, tpl_1(bridge.tlps), clocked_by epClock62, reset_by epReset62);

      // pass MSI interrupt requests

      Reg#(Bool) msi_intr_active <- mkReg(False, clocked_by epClock62, reset_by epReset62);

      (* fire_when_enabled *) // in epClock62 domain
      rule pass_msi_req if (!msi_intr_active && bridge.msi_interrupt_req());
         msi_intr_active <= True;
      endrule

      (* fire_when_enabled, no_implicit_conditions *) // in epClock62 domain
      rule trigger_msi_intr;
         _ep.cfg_irq.interrupt_n((bridge.msi_interrupt_req() || msi_intr_active) ? 0 : 1);
      endrule

      (* fire_when_enabled *) // in epClock62 domain
      rule clear_msi_intr if (msi_intr_active && (_ep.cfg_irq.interrupt_rdy_n() == 0));
         msi_intr_active <= False;
         bridge.msi_interrupt_clear();
      endrule

      // The reference clock is the base SCE-MI clock
      Clock scemiClock = ref_clk;

      // Propagate loss-of-partner as a reset to the SCE-MI
      MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock62, reset_by epReset62);

      (* fire_when_enabled, no_implicit_conditions *)
      rule reset_scemi_if_network_is_inactive if (!bridge.is_activated());
         network_status.assertReset();
      endrule

      Reset scemiReset = network_status.new_rst;

      // Build the design with a NoC connection
      _dut <- buildSceMiNoC( mod
			   , epClock62
			   , epReset62
                           , bridge.is_activated()
			   , link_type
                           , clocked_by scemiClock
			   , reset_by scemiReset
                           );
      mkConnection(_dut.noc_src,bridge.noc);
      
      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_pcie_link_up;
	 bridge.status_pcie_link_is_up(_ep.trn.link_up);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_bluenoc_link_up;
	 bridge.status_bluenoc_link_is_up(bridge.is_activated());
      endrule
      
      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_interrupts_enabled;
	 bridge.status_interrupts_enabled(intr_on);
      endrule
      
      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_memory_enabled;
	 bridge.status_memory_enabled(memory_enabled);
      endrule      
      
      ReadOnly#(Bool) wIsOutOfReset <- mkNullCrossing(epClock62, _dut.isOutOfReset, reset_by epReset62);

      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_out_of_reset;
         bridge.status_out_of_reset(wIsOutOfReset);
      endrule

      original_ifc    = _dut.orig_ifc;
      out_of_reset    = _dut.isOutOfReset;
      clock_advancing = _dut.isClockAdvancing;
   end
    else begin
      // The PCIE endpoint is processing TLPWord#(8)s at 250MHz.  The
      // NoC bridge is accepting TLPWord#(16)s at 125 MHz. The
      // connection between the endpoint and the NoC contains GerBox
      // instances for the TLPWord#(8)@250 <--> TLPWord#(16)@125
      // conversion.

      // The PCIe endpoint exports full (250MHz) and half-speed (125MHz) clocks
      Clock epClock250 = _ep.trn.clk;
      Reset epReset250 <- mkAsyncReset(4, _ep.trn.reset_n, epClock250);
      Clock epClock125 = _ep.trn.clk2;
      Reset epReset125 <- mkAsyncReset(4, _ep.trn.reset_n, epClock125);

      // Extract some status info from the PCIE endpoint. These values are
      // all in the epClock250 domain, so we have to cross them into the
      // epClock125 domain.

      UInt#(13) max_read_req_bytes_250       = 128 << _ep.cfg.dcommand[14:12];
      UInt#(13) max_payload_bytes_250        = 128 << _ep.cfg.dcommand[7:5];
      UInt#(8)  read_completion_boundary_250 = 64 << _ep.cfg.lcommand[3];
      Bool      msi_enable_250               =  (_ep.cfg_irq.interrupt_msienable() == 1)
                                             && (_ep.cfg_irq.interrupt_mmenable == 0);
      Bool      bus_master_enable_250        = (_ep.cfg.command[2] == 1);

      CrossingReg#(UInt#(13)) max_rd_req_cr        <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
      CrossingReg#(UInt#(13)) max_payload_cr       <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
      CrossingReg#(UInt#(8))  rcb_cr               <- mkNullCrossingReg(epClock125, 128,   clocked_by epClock250, reset_by epReset250);
      CrossingReg#(Bool)      msi_enable_cr        <- mkNullCrossingReg(epClock125, False, clocked_by epClock250, reset_by epReset250);
      CrossingReg#(Bool)      bus_master_enable_cr <- mkNullCrossingReg(epClock125, False,  clocked_by epClock250, reset_by epReset250);

      Reg#(UInt#(13)) max_read_req_bytes <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
      Reg#(UInt#(13)) max_payload_bytes  <- mkReg(128,   clocked_by epClock125, reset_by epReset125);
      Reg#(Bit#(7))   rcb_mask           <- mkReg(7'h3f, clocked_by epClock125, reset_by epReset125);
      Reg#(Bool)      msi_enable         <- mkReg(False, clocked_by epClock125, reset_by epReset125);
      Reg#(Bool)      bus_master_enable  <- mkReg(False, clocked_by epClock125, reset_by epReset125);
       
                      memory_enabled     <- mkBypassWire(clocked_by epClock125, reset_by epReset125);
      CrossingReg#(Bool) intr_on         <- mkNullCrossingReg(epClock125, False, clocked_by epClock250, reset_by epReset250);
       
       
      (* fire_when_enabled, no_implicit_conditions *)
      rule cross_config_values;
         max_rd_req_cr        <= max_read_req_bytes_250;
         max_payload_cr       <= max_payload_bytes_250;
         rcb_cr               <= read_completion_boundary_250;
         msi_enable_cr        <= msi_enable_250;
         bus_master_enable_cr <= bus_master_enable_250;
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule register_config_values;
         max_read_req_bytes <= max_rd_req_cr.crossed();
         max_payload_bytes  <= max_payload_cr.crossed();
         rcb_mask           <= (rcb_cr.crossed() == 64) ? 7'h3f : 7'h7f;
         msi_enable         <= msi_enable_cr.crossed();
         bus_master_enable  <= bus_master_enable_cr.crossed();
      endrule

      // setup PCIe interrupt for MSI
      // this rule executes in the epClock250 domain
      (* fire_when_enabled, no_implicit_conditions *)
      rule intr_ifc_ctl;
         _ep.cfg_irq.interrupt_di('0);        // tied off for single-vector MSI
         _ep.cfg_irq.interrupt_assert_n('1);  // tied off for MSI
	 intr_on <= msi_enable_cr && bus_master_enable_cr;
      endrule: intr_ifc_ctl

      // Build the PCIe-to-NoC bridge
      PCIEtoBNoCFull#(BPB)  bridge <- mkPCIEtoBNoCFull_4( 64'h05ce_0006_0008_0000
						      	, my_id
						      	, max_read_req_bytes
						      	, max_payload_bytes
						      	, rcb_mask
						      	, False // no MSI-X, only MSI
						      	, True
						      	, msi_enable
						      	, clocked_by epClock125, reset_by epReset125
						      	);
      mkConnectionWithClocks(_ep.trn_rx, tpl_2(bridge.tlps), epClock250, epReset250, epClock125, epReset125);
      mkConnectionWithClocks(_ep.trn_tx, tpl_1(bridge.tlps), epClock250, epReset250, epClock125, epReset125);

      // pass MSI interrupt requests

      Reg#(Bool)   msi_intr_active <- mkReg(False, clocked_by epClock250, reset_by epReset250);
      SyncPulseIfc msi_req         <- mkSyncHandshake(epClock125, epReset125, epClock250);
      SyncPulseIfc msi_clr         <- mkSyncHandshake(epClock250, epReset250, epClock125);
      PulseWire    cleared_intr    <- mkPulseWire(clocked_by epClock250, reset_by epReset250);

      (* fire_when_enabled *) // in epClock125 domain
      rule pass_msi_req if (bridge.msi_interrupt_req());
         msi_req.send();
      endrule

      (* fire_when_enabled, no_implicit_conditions *) // in epClock62 domain
      rule trigger_msi_intr;
         _ep.cfg_irq.interrupt_n((msi_req.pulse() || msi_intr_active) ? 0 : 1);
      endrule

      (* fire_when_enabled *) // in epClock250 domain
      rule clear_msi_intr if (msi_intr_active && (_ep.cfg_irq.interrupt_rdy_n() == 0));
         msi_clr.send();
         cleared_intr.send();
      endrule

      (* fire_when_enabled *) // in epClock125 domain
      rule pass_msi_clear if (msi_clr.pulse());
         bridge.msi_interrupt_clear();
      endrule

      (* fire_when_enabled, no_implicit_conditions *) // in epClock250 domain
      rule track_msi_req_status;
         if (msi_req.pulse())
            msi_intr_active <= True;
         else if (cleared_intr)
            msi_intr_active <= False;
      endrule

      // The reference clock is the base SCE-MI clock
      Clock scemiClock = ref_clk;

      // Propagate loss-of-partner as a reset to the SCE-MI
      MakeResetIfc network_status <- mkReset(4, True, scemiClock, clocked_by epClock125, reset_by epReset125);

      (* fire_when_enabled, no_implicit_conditions *)
      rule reset_scemi_if_network_is_inactive if (!bridge.is_activated());
         network_status.assertReset();
      endrule

      Reset scemiReset = network_status.new_rst;

      // Build the design with a NoC connection
      _dut <- buildSceMiNoC( mod
			   , epClock125
                           , epReset125
                           , bridge.is_activated()
                           , link_type
                           , clocked_by scemiClock
                           , reset_by scemiReset
                           );
      mkConnection(_dut.noc_src,bridge.noc);

      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_pcie_link_up;
	 bridge.status_pcie_link_is_up(_ep.trn.link_up);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_bluenoc_link_up;
	 bridge.status_bluenoc_link_is_up(bridge.is_activated());
      endrule
      
      (* fire_when_enabled, no_implicit_conditions *)
      rule drive_status_interrupts_enabled;
	 bridge.status_interrupts_enabled(intr_on.crossed);
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
       
      original_ifc    = _dut.orig_ifc;
      out_of_reset    = _dut.isOutOfReset;
      clock_advancing = _dut.isClockAdvancing;
   end

   // Pass along the required interface elements
   interface orig_ifc = original_ifc;
   interface pcie     = _ep.pcie;
   interface noc_cont = _dut.noc_cont;
   method Bool isLinkUp         = _ep.trn.link_up();
   method Bool isOutOfReset     = out_of_reset;
   method Bool isClockAdvancing = clock_advancing;
   method Action isDDRReady(i)  = memory_enabled._write(i);
endmodule: buildSceMiPCIEV5

endpackage: SceMiVirtex5PCIE
