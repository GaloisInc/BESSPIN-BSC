import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx       :: *;
import XilinxPCIE   :: *;
import XilinxVirtex6PCIE :: *;
import Clocks       :: *;
import DefaultValue :: *;
import TieOff       :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
                  Clock sgmii_clk_p, Clock sgmii_clk_n,
                  Reset pci_sys_reset_n)
                 (ML605_FPGA);

   Clock sgmii_clk <- mkClockIBUFDS_GTXE1(defaultValue, True, sgmii_clk_p, sgmii_clk_n);

   SceMiV6PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEX6;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute clock_signal of scemi_sys_clk_buf is \"yes\";" *)
   SceMiV6PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);
   
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
