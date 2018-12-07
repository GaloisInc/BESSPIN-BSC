import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx::*;
import XilinxPCIE::*;
import Clocks::*;
import DefaultValue::*;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		  Clock sgmii_clk_p, Clock sgmii_clk_n,
		  Reset pci_sys_reset_n)
                 (ML605_FPGA);

   Clock sgmii_clk <- mkClockIBUFDS_GTXE1(defaultValue, True, sgmii_clk_p, sgmii_clk_n);

   ClockGeneratorParams clk_params = defaultValue();
   clk_params.clkin1_period     = 8.000;       // 125 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.feedback_mul      = 8;           // 1000 MHz VCO
   clk_params.clk0_div          = `SCEMI_CLOCK_PERIOD;

   ClockGenerator clk_gen <- mkClockGenerator(clk_params, clocked_by sgmii_clk, reset_by pci_sys_reset_n);

   SceMiV6PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = PCIE_VIRTEX6;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute clock_signal of scemi_sys_clk_buf is \"yes\";" *)
   SceMiV6PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;

   method leds = zeroExtend({pack(_isClockAdvancing)
			    ,pack(_isOutOfReset)
			    ,pack(_isLinkUp)
			    });
endmodule: mkBridge
