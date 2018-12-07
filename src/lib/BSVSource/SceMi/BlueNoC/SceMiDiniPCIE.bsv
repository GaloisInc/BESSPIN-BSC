// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiDiniPCIE;

// This is an implementation of SceMi over PCI-Express using the
// BlueNoC connection through FPGA Q.

import Clocks       :: *;
import Connectable  :: *;
import TieOff       :: *;
import DefaultValue :: *;
import AlteraCells  :: *;
import BlueNoC      :: *;
import NoCUtils     :: *;

import SceMiDefines   :: *;
import SceMiInternals :: *;
import SceMiNoC       :: *;

// Interface wrapper for PCIE
interface SceMiDiniPCIEIfc#(type i);
   interface i                orig_ifc;
   interface TrainingSignals  training;
   interface FPGASource#(BPB) source;
   interface FPGASink#(BPB)   sink;
   interface MsgPort#(BPB)    noc_cont;
   (* always_ready *)
   method Bool isLinkUp();
   (* always_ready *)
   method Bool isOutOfReset();
   (* always_ready *)
   method Bool isClockAdvancing();
endinterface

// Argument structure used for passing in clocks and resets
typedef struct {
   Clock         fpga_clk;
   Reset         fpga_rst;
   Clock         noc_q_clk;
   Clock         noc_a_clk;
   Reset         noc_reset_n;
   SceMiLinkType link_type;
} SceMiDiniPCIEArgs;

// This module builds the transactor hierarchy, the clock
// generation logic and the PCIE-to-port logic.
(* no_default_clock, no_default_reset *)
module [Module] buildSceMiPCIEDini#( SceMiModule#(i) mod
                                   , Clock fpga_clk
                                   , Reset fpga_rst
                                   , Clock noc_q_clk
				   , Clock noc_a_clk
				   , Reset noc_reset_n
                                   , SceMiLinkType link_type
                                   )
                                   (SceMiDiniPCIEIfc#(i));

   // Create a reset to use with the NoC Q clock domain
   Reset noc_q_rst <- mkAsyncReset(4,noc_reset_n,noc_q_clk);
   Reset noc_a_rst <- mkAsyncReset(4,noc_reset_n,noc_a_clk);
   
   // Instantiate the NoC connection to FPGA Q
   FPGAMsgPort#(BPB) to_fpga_q <- mkFPGAMsgPort(noc_q_clk, clocked_by noc_a_clk, reset_by noc_a_rst);

   // The fpga clock (G0) is the base SCE-MI clock, but we want to
   // propagate loss-of-partner as a reset to the SCE-MI system
   MakeResetIfc network_status <- mkReset(0, True, fpga_clk, clocked_by noc_a_clk, reset_by noc_a_rst);

   (* fire_when_enabled, no_implicit_conditions *)
   rule reset_scemi_if_fpga_q_is_not_present if (!to_fpga_q.status.partner_detected());
      network_status.assertReset();
   endrule

   Clock scemiClock = fpga_clk;
   Reset scemiReset <- mkResetEither(fpga_rst, network_status.new_rst, clocked_by fpga_clk);

   // Build the design with a NoC connection
   SceMiNoCIfc#(i) _dut <- buildSceMiNoC( mod
                                        , noc_a_clk
                                        , noc_a_rst
                                        , to_fpga_q.status.partner_detected()
                                        , link_type
                                        , clocked_by scemiClock
                                        , reset_by scemiReset
                                        );
   mkConnection(_dut.noc_src,to_fpga_q.noc);

   // Pass along the required interface elements
   interface orig_ifc           = _dut.orig_ifc;
   interface training           = to_fpga_q.pins.training;
   interface source             = to_fpga_q.pins.source;
   interface sink               = to_fpga_q.pins.sink;
   interface noc_cont           = _dut.noc_cont;
   method Bool isLinkUp         = to_fpga_q.status.partner_detected();
   method Bool isOutOfReset     = _dut.isOutOfReset();
   method Bool isClockAdvancing = _dut.isClockAdvancing();

endmodule: buildSceMiPCIEDini

endpackage: SceMiDiniPCIE
