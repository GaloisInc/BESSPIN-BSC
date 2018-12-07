import SceMi      :: *;
import SceMiLayer :: *;

import Xilinx       :: *;
import XilinxPCIE   :: *;
import Clocks       :: *;
import DefaultValue :: *;
import TieOff       :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
                  Reset pci_sys_reset_n)
                 (KLVUF4A_FPGA);

   SceMiVUPCIE3Args pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEXU;

   SceMiVUPCIEIfc#(Empty, 4) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   mkTieOff(scemi.noc_cont);

   rule drive_memory_calibration;
      scemi.isDDRReady(False);
   endrule

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;
   method cprsnt = 1;
   method cwake  = 0;
   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
