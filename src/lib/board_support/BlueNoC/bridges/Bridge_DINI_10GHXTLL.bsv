import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx       :: *;
import XilinxPCIE   :: *;
import XilinxVirtex6PCIE :: *;
import Clocks       :: *;
import DefaultValue :: *;
import Connectable  :: *;
import TieOff       :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		  Clock sfp0_refclk_p, Clock sfp0_refclk_n,
		  Clock sfp1_refclk_p, Clock sfp1_refclk_n,
		  Clock sfp2_refclk_p, Clock sfp2_refclk_n,
		  Clock clk200_p,      Clock clk200_n,
                  Reset pci_sys_reset_n)
                 (DN10GHXTLL_FPGA);

   Clock clk200 <- mkClockIBUFDS(defaultValue, clk200_p, clk200_n);

   ClockGeneratorParams clk_params = defaultValue();
   clk_params.clkin1_period     = 5.000;       // 200 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.feedback_mul      = 5;           // 1000 MHz VCO
   clk_params.clk0_buffer       = False;       // Instance BUFG for clk0
   clk_params.clk0_div          = 20;          // 50 MHz scemi clock / 25 MHz uclock/cclock (adjust as needed)
   clk_params.clk1_div          = 20;          // 50 MHz scemi clock / 25 MHz uclock/cclock (adjust as needed)

   ClockGenerator clk_gen <- mkClockGenerator(clk_params, clocked_by clk200, reset_by pci_sys_reset_n);
   Reset sysrst_n <- mkAsyncReset(0, pci_sys_reset_n, clk_gen.clkout1);
   
   SceMiV6PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = PCIE_VIRTEX6;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute clock_signal of scemi_sys_clk_buf is \"yes\";" *)
   SceMiV6PCIEIfc#(Empty, 8) scemi <- buildSceMi(mkSceMiLayer, pcie_args);
   
   mkTieOff(scemi.noc_cont);
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   
   let sfp0_xgmii <- mkXGMIITransceiver(0, sfp0_refclk_p, sfp0_refclk_n, clocked_by clk_gen.clkout1, reset_by sysrst_n);
   let sfp1_xgmii <- mkXGMIITransceiver(1, sfp1_refclk_p, sfp1_refclk_n, clocked_by clk_gen.clkout1, reset_by sysrst_n);
   let sfp2_xgmii <- mkXGMIITransceiver(2, sfp2_refclk_p, sfp2_refclk_n, clocked_by clk_gen.clkout1, reset_by sysrst_n);
   
   interface pcie = scemi.pcie;
   interface dclk = clk_gen.clkout1;
   interface sfp0 = sfp0_xgmii.sfp;
   interface sfp1 = sfp1_xgmii.sfp;
   interface sfp2 = sfp2_xgmii.sfp;
   method leds = ~zeroExtend({pack(_isClockAdvancing)
                             ,pack(_isOutOfReset)
                             ,pack(_isLinkUp)
                             });
endmodule: mkBridge
