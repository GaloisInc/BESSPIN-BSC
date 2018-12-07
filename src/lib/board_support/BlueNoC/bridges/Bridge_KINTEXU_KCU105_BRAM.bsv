import SceMi      :: *;
import SceMiLayer :: *;

import Xilinx       :: *;
import XilinxPCIE   :: *;
import Clocks       :: *;
import DefaultValue :: *;
import TieOff       :: *;
import Connectable  :: *;
import ClientServer :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
                  Reset pci_sys_reset_n)
                 (KCU105_FPGA);

   SceMiVUPCIE3Args pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEXU;

   SceMiVUPCIEIfc#(MemSceMiLayerIfc, 8) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;

   let mem <- mkMemModel(clocked_by uclock, reset_by ureset);
   let mkMemCnx <- mkConnection(mem, scemiOrig.scemiLayer);

   mkTieOff(scemi.noc_cont);

   rule drive_memory_calibration;
      scemi.isDDRReady(True);
   endrule

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, True);

   interface pcie = scemi.pcie;
   method leds = zeroExtend({ pack(_isCalibrated)
			     ,pack(_isClockAdvancing)
			     ,pack(_isOutOfReset)
			     ,pack(_isLinkUp)
			     });
endmodule: mkBridge

// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule
