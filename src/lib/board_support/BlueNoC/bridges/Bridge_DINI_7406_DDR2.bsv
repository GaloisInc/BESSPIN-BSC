////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$

import SceMi      :: *;
import SceMiLayer :: *;
import NoCUtils   :: *;

// Setup for SCE-MI over PCIE to a Dini board
import Dini        :: *;
import Clocks      :: *;
import DummyDriver :: *;
import CommitIfc   :: *;
import Connectable :: *;
import TieOff      :: *;

// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #( Clock g0_clk
                 , Reset g0_rstn
                 , Clock noc_q_clk
		 , Clock noc_a_clk
                 , Reset noc_reset_n
                 , Clock g1_clk
                 , Reset g1_rstn
                 )
                 (DN7406_FPGA_A_DDR2);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.fpga_clk  	 = g0_clk;
   pcie_args.fpga_rst  	 = g0_rstn;
   pcie_args.noc_q_clk 	 = noc_q_clk;
   pcie_args.noc_a_clk   = noc_a_clk;
   pcie_args.noc_reset_n = noc_reset_n;
   pcie_args.link_type 	 = PCIE_DINI;

   SceMiDiniPCIEIfc#(MemSceMiLayerIfc) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   
   mkTieOff(scemi.noc_cont);
   
   // ddr2_ctrl.user interface must be connected up to user logic
   DDR2_Controller ddr2_ctrl <- mkDDR2Controller(clocked_by g1_clk, reset_by g1_rstn);
   let connectDDR2 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr2_ctrl.user);

   // use a stub of the the RS232 pins
   RS232_Pins unused_rs232 <- mkStub(clocked_by g1_clk, reset_by g1_rstn);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr2_ctrl.user.init_done);

   interface training = scemi.training;
   interface source   = scemi.source;
   interface sink     = scemi.sink;

   interface rs232 = unused_rs232;

   method leds = zeroExtend({pack(_isCalibrated && _isLinkUp)
                            ,pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            });

   interface ddr2 = ddr2_ctrl.ddr2;
   method isDDRReady = ddr2_ctrl.user.init_done;
   method isOutOfReset = _isOutOfReset;
endmodule: mkBridge

(* no_default_clock, no_default_reset *)
module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule
