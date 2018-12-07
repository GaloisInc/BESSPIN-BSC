////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$

// Local Import
import SceMiLayer::*;

import SceMi::*;
import Dini::*;
import DiniPCIE::*;
import Clocks::*;
import DummyDriver::*;
import CommitIfc::*;
import Connectable::*;

SceMiLinkType lt = PCIE_DINI;

// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface


(* synthesize *)
module mkBridge #(Clock sample_clk
                  ,Reset sample_rstn
                  ,Clock g1_clk
                  ,Reset g1_rstn
                  ) (DN7002_FPGA_A_DDR2);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.pci_sample_clk = sample_clk;
   pcie_args.pci_sample_rstn = sample_rstn;
   pcie_args.link_type = lt;
   SceMiDiniPCIEIfc#(MemSceMiLayerIfc) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;

   // ddr2_ctrl.user interface must be connected up to user logic
   DDR2_Controller ddr2_ctrl <- mkDDR2Controller(clocked_by g1_clk, reset_by g1_rstn);
   let connectDDR2 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr2_ctrl.user);

   // use a stub of the the RS232 pins
   RS232_Pins unused_rs232 <- mkStub();

   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr2_ctrl.user.init_done);

   interface pcie  = scemi.pcie;
   interface rs232 = unused_rs232;
   method leds = zeroExtend({pack(_isCalibrated)
                             ,pack(_isClockAdvancing)
                             ,pack(_isOutOfReset)
                             });

   interface ddr2 = ddr2_ctrl.ddr2;
endmodule: mkBridge

module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule
