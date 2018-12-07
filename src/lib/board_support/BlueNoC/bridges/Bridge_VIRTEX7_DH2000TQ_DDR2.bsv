import SceMi      :: *;
import SceMiLayer :: *;

import Xilinx       :: *;
import XilinxPCIE   :: *;
import XilinxDDR2   :: *;
import Clocks       :: *;
import Connectable  :: *;
import CommitIfc    :: *;
import DefaultValue :: *;
import TieOff       :: *;

interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
                  Clock sys_clk,
                  Reset pci_sys_reset_n)
                 (DH2000TQ_FPGA_DDR2);

   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 20.000;      // 50 MHz reference
   clk_params.clkin_buffer      = False;
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 20.000;      // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD; 
   clk_params.clkout1_divide    = 5;           // 200 MHz ddr3 system clock

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by sys_clk, reset_by pci_sys_reset_n);
   
   Clock clk = clk_gen.clkout0;
   Reset rst_n <- mkAsyncReset( 1, pci_sys_reset_n, clk );
   Reset ddr2ref_rst_n <- mkAsyncReset( 1, rst_n, clk_gen.clkout1 );
   
   DDR2_Configure_V7 ddr2_cfg;
   ddr2_cfg.num_reads_in_flight = 8;
   ddr2_cfg.fast_train_sim_only = False;
   
   DDR2_Controller_V7 ddr2_ctrl <- mkDDR2ControllerV7( ddr2_cfg, clocked_by clk_gen.clkout1, reset_by ddr2ref_rst_n );
   
   SceMiV7PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEX7;

   SceMiV7PCIEIfc#(MemSceMiLayerIfc, 4) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   
   mkTieOff(scemi.noc_cont);
   
   let connectDDR2 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr2_ctrl.user);
   
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr2_ctrl.user.init_done);

   rule drive_memory_calibration;
      scemi.isDDRReady(ddr2_ctrl.user.init_done);
   endrule
   
   interface pcie = scemi.pcie;
   interface ddr2 = ddr2_ctrl.ddr2;
   
   method leds = zeroExtend({pack(_isCalibrated)
			    ,pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge

module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule

