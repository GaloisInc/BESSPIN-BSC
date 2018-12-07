import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx       :: *;
import XilinxPCIE   :: *;
import Clocks       :: *;
import DefaultValue :: *;
import TieOff       :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
                  Clock sys_clk_p, Clock sys_clk_n,
                  Reset pci_sys_reset_n)
                 (VC709_FPGA);

   Clock sys_clk <- mkClockIBUFDS(defaultValue, sys_clk_p, sys_clk_n);

   SceMiV7PCIE3Args pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEX7;

   SceMiV7PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);
   
   mkTieOff(scemi.noc_cont);
   
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   rule drive_memory_calibration;
      scemi.isDDRReady(False);
   endrule

   interface pcie = scemi.pcie;

   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
