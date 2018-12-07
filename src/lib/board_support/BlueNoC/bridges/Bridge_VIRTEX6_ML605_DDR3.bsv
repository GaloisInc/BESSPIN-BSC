import SceMi      :: *;
import SceMiLayer :: *;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx       :: *;
import Clocks       :: *;
import DefaultValue :: *;
import Connectable  :: *;
import CommitIfc    :: *;
import TieOff       :: *;

// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		          Clock clk200_p, Clock clk200_n,
		          Clock sgmii_clk_p, Clock sgmii_clk_n,
		          Reset pci_sys_reset_n)
                 (ML605_FPGA_DDR3);

   Clock sgmii_clk <- mkClockIBUFDS_GTXE1(defaultValue, True, sgmii_clk_p, sgmii_clk_n);

   ClockGeneratorParams clk_params = defaultValue();
   clk_params.clkin1_period     = 8.000;       // 125 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.feedback_mul      = 8;           // 1000 MHz VCO
   clk_params.clk0_div          = `SCEMI_CLOCK_PERIOD;

   ClockGenerator clk_gen <- mkClockGenerator(clk_params, clocked_by sgmii_clk, reset_by pci_sys_reset_n);

   Clock clk = clk_gen.clkout0;
   Reset rst_n <- mkAsyncReset( 1, pci_sys_reset_n, clk );
   
   DDR3_Configure ddr3_cfg = defaultValue;
   DDR3_Controller_ML605 ddr3_ctrl <- mkDDR3Controller_ML605(ddr3_cfg, clk200_p, clk200_n, clocked_by clk, reset_by rst_n);

   // ddr3_ctrl.user needs to connect to user logic and should use ddr3clk and ddr3rstn
   Clock ddr3clk = ddr3_ctrl.user.clock;
   Reset ddr3rstn = ddr3_ctrl.user.reset_n;
   
   SceMiV6PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEX6;

   (* doc = "synthesis attribute buffer_type of scemi_pcie_ep_trn_clk is \"none\"" *)
   (* doc = "synthesis attribute clock_signal of scemi_sys_clk_buf is \"yes\";" *)
   SceMiV6PCIEIfc#(MemSceMiLayerIfc, 8) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   
   mkTieOff(scemi.noc_cont);
   
   let connectDDR3 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr3_ctrl.user);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr3_ctrl.user.init_done);
   
   rule drive_memory_calibration;
      scemi.isDDRReady(ddr3_ctrl.user.init_done);
   endrule
   
   interface pcie = scemi.pcie;
   interface ddr3 = ddr3_ctrl.ddr3;
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

