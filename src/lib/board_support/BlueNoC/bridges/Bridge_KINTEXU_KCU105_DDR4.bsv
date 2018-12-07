import SceMi      :: *;
import SceMiLayer :: *;

import Xilinx       :: *;
import DDR4         :: *;
import XilinxKCU105DDR4 :: *;
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
		  Clock sys_clk_p,     Clock sys_clk_n,
		  Clock user_clk_p,     Clock user_clk_n,
                  Reset pci_sys_reset_n)
                 (KCU105_FPGA_DDR4);
/*
   Clock sys_clk <- mkClockIBUFDS(defaultValue, sys_clk_p, sys_clk_n);

   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 8;       // 125 MHz reference
   clk_params.clkin_buffer      = False;       // necessary buffer is instanced above
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 8.000;       // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD;
   //clk_params.clkout1_divide    = 10;           // ddr4 reference clock (100 MHz) // TODO: make faster?

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by user_clk, reset_by pci_sys_reset_n);

   Clock clk = clk_gen.clkout0;
   Reset rst_n <- mkAsyncReset( 1, pci_sys_reset_n, clk );
*/
   Clock user_clk <- mkClockIBUFDS(defaultValue, user_clk_p, user_clk_n);
   Reset ddr_rst <- mkResetInverter(pci_sys_reset_n, clocked_by user_clk);
   //Reset ddr4ref_rst_n <- mkAsyncReset( 1, ddr_rst, sys_clk );
   //Reset ddr4ref_rst_n <- mkAsyncReset( 1, rst_n, sys_clk );

   DDR4_Controller_KCU105 ddr4_ctrl <- mkDDR4Controller_KCU105(defaultValue, sys_clk_p, sys_clk_n, reset_by ddr_rst);

   // ddr4_ctrl.user needs to connect to user logic and should use ddr4clk and ddr4rstn
   Clock ddr4clk = ddr4_ctrl.user.clock;
   Reset ddr4rstn = ddr4_ctrl.user.reset_n;

   SceMiVUPCIE3Args pcie_args;
   pcie_args.pci_sys_clk_p = pci_sys_clk_p;
   pcie_args.pci_sys_clk_n = pci_sys_clk_n;
   pcie_args.pci_sys_reset = pci_sys_reset_n;
   pcie_args.clock_period  = `SCEMI_CLOCK_PERIOD;
   pcie_args.link_type     = PCIE_VIRTEXU;

   SceMiVUPCIEIfc#(MemSceMiLayerIfc, 8) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;

   let connectDDR4 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr4_ctrl.user);

   mkTieOff(scemi.noc_cont);

   rule drive_memory_calibration;
      scemi.isDDRReady(ddr4_ctrl.user.init_done);
   endrule

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr4_ctrl.user.init_done);

   interface pcie = scemi.pcie;
   interface ddr4 = ddr4_ctrl.ddr4;
   method leds = zeroExtend({ pack(_isCalibrated)
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
