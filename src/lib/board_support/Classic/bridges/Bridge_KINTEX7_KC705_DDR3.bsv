import SceMi::*;
import SceMiLayer::*;

// Setup for SCE-MI over PCIE to a Virtex6
import Xilinx::*;
import XilinxPCIE::*;
import Clocks::*;
import DefaultValue::*;
import Connectable::*;
import CommitIfc::*;

// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #(Clock pci_sys_clk_p, Clock pci_sys_clk_n,
		  Clock sys_clk_p,     Clock sys_clk_n,
		  Reset pci_sys_reset_n)
                 (KC705_FPGA_DDR3);

   Clock sgmii_clk <- mkClockIBUFDS(defaultValue, sys_clk_p, sys_clk_n);

   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 8.000;       // 125 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 8.000;       // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD;
   clk_params.clkout1_divide    = 5;

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by sys_clk, reset_by pci_sys_reset_n);

   Clock clk = clk_gen.clkout0;
   Reset rst_n <- mkAsyncReset( 1, pci_sys_reset_n, clk );
   Reset ddr3ref_rst_n <- mkAsyncReset( 1, rst_n, clk_gen.clkout1 );
   
   DDR3_Configure_K7 ddr3_cfg;
   ddr3_cfg.num_reads_in_flight = 2;   // adjust as needed
   ddr3_cfg.fast_train_sim_only = False; // adjust if simulating
   
   DDR3_Controller_K7 ddr3_ctrl <- mkKintex7DDR3Controller(ddr3_cfg, clocked_by clk_gen.clkout1, reset_by ddr3ref_rst_n);

   // ddr3_ctrl.user needs to connect to user logic and should use ddr3clk and ddr3rstn
   Clock ddr3clk = ddr3_ctrl.user.clock;
   Reset ddr3rstn = ddr3_ctrl.user.reset_n;
   
   SceMiK7PCIEArgs pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.ref_clk       = clk;
   pcie_args.link_type     = PCIE_KINTEX7;

   SceMiK7PCIEIfc#(MemSceMiLayerIfc, 8) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   
   let connectDDR3 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr3_ctrl.user);

   
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr3_ctrl.user.init_done);

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

