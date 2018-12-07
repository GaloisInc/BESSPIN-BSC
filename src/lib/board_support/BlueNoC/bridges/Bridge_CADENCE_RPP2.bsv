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
		  Clock sys_clk_0_p,   Clock sys_clk_0_n,
                  Reset pci_sys_reset)
                 (RPP2_FPGA);

   Clock sys_clk_0 <- mkClockIBUFGDS(defaultValue, sys_clk_0_p, sys_clk_0_n);
   Reset pci_sys_reset_n <- mkResetInverter(pci_sys_reset, clocked_by sys_clk_0);

   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 5.000;       // 200 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 5.000;       // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD;

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by sys_clk_0, reset_by pci_sys_reset_n);

   SceMiV7PCIENoClkArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = PCIE_VIRTEX7;

   SceMiV7PCIEIfc#(Empty, 4) scemi <- buildSceMi(mkSceMiLayer, pcie_args);

   mkTieOff(scemi.noc_cont);

   rule drive_memory_calibration;
      scemi.isDDRReady(False);
   endrule

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie = scemi.pcie;
   method cprsnt      = 1;
   method cwake       = 0;
   method osc_oe_real = 1;
   method osc_fs      = 3;
   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
