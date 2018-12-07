import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx::*;
import XilinxPCIE::*;
import Clocks::*;
import DefaultValue::*;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		  Clock sys_clk_p, Clock sys_clk_n,
		  Reset pci_sys_reset_n)
                 (KC705_FPGA);

   Clock sys_clk <- mkClockIBUFDS(defaultValue, sys_clk_p, sys_clk_n);

   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 5.000;       // 200 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 5.000;       // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD;

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by sys_clk, reset_by pci_sys_reset_n);

   SceMiK7PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = PCIE_KINTEX7;

   SceMiK7PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;

   method leds = zeroExtend({pack(_isClockAdvancing)
			    ,pack(_isOutOfReset)
			    ,pack(_isLinkUp)
			    });
endmodule: mkBridge
