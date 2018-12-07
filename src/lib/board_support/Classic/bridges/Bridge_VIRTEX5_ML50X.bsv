import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over PCIE to a Virtex5
import Xilinx::*;
import XilinxPCIE::*;
import Clocks::*;
import DefaultValue::*;

SceMiLinkType lt = PCIE_VIRTEX5;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		  Clock refclk_100, Clock clk200, Reset pci_sys_reset_n)
                 (ML50x_FPGA);

   ClockGeneratorParams clk_params = defaultValue();
   clk_params.feedback_mul = 10; // 1000 MHz VCO frequency
   clk_params.clk0_div     = `SCEMI_CLOCK_PERIOD;
   ClockGenerator clk_gen <- mkClockGenerator(clk_params, clocked_by refclk_100, reset_by pci_sys_reset_n);

   SceMiV5PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = lt;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute keep of scemi_pcie_ep_trn2_clk is \"true\";" *)
   SceMiV5PCIEIfc#(Empty, 1) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;

   method leds = zeroExtend({pack(_isClockAdvancing)
			    ,pack(_isOutOfReset)
			    ,pack(_isLinkUp)
			    });
endmodule: mkBridge
