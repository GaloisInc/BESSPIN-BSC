import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Virtex5
import Xilinx       :: *;
import XilinxPCIE   :: *;
import XilinxVirtex5PCIE :: *;
import Clocks       :: *;
import DefaultValue :: *;
import TieOff       :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		          Clock refclk_100, Clock clk200, Reset pci_sys_reset_n)
                 (ML50x_FPGA_DDR2);

   ClockGeneratorParams clk_params = defaultValue();
   clk_params.feedback_mul = 10; // 1000 MHz VCO frequency
   clk_params.clk0_div     = `SCEMI_CLOCK_PERIOD;
   clk_params.clk1_div     = 8;  // 125 MHz
   clk_params.clk1_buffer  = False;
   ClockGenerator clk_gen <- mkClockGenerator(clk_params, clocked_by refclk_100, reset_by pci_sys_reset_n);

   Reset scemi_reset <- mkAsyncReset( 3, pci_sys_reset_n, clk_gen.clkout0 );

   Clock g1_clk = clk_gen.clkout1;
   Reset g1_rstn <- mkAsyncReset(0, scemi_reset, g1_clk);

   DDR2_Configure ddr2_cfg;
   ddr2_cfg.clk_period_in_ps    = 8000;
   ddr2_cfg.num_reads_in_flight = 2;
   ddr2_cfg.fast_train_sim_only = False; // set to true for faster simulations with ddr2

   DDR2_Controller ddr2_ctrl <- mkDDR2Controller( ddr2_cfg, clk200, clocked_by g1_clk, reset_by g1_rstn );

   SceMiV5PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk_gen.clkout0;
   pcie_args.link_type     = PCIE_VIRTEX5;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute keep of scemi_pcie_ep_trn2_clk is \"true\";" *)
   SceMiV5PCIEIfc#(Empty, 1) scemi <- buildSceMi(mkSceMiLayer, pcie_args);
   
   mkTieOff(scemi.noc_cont);
   
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   
   rule drive_memory_calibration;
      scemi.isDDRReady(ddr2_ctrl.user.init_done);
   endrule
   
   interface pcie = scemi.pcie;
   interface ddr2 = ddr2_ctrl.ddr2;

   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
