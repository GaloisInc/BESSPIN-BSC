////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$

import SceMi      :: *;
import SceMiPCIE  :: *;
import SceMiLayer :: *;
import NoCUtils   :: *;

// Setup for SCE-MI over PCIE to a Xilinx board
import Xilinx      :: *;
import Clocks      :: *;
import DefaultValue:: *;
import TieOff      :: *;
import CommitIfc   :: *;
import Connectable :: *;


// We need to get access to the uncontrolled clock and reset to hook up the DDR2
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #( Clock sys_clk
                 , Reset sys_rstn
		 , Clock fpga_clk
                 , Clock noc_q_clk
		 , Clock noc_a_clk
		 , Reset noc_reset_n
                 )
                 (RPP2SPLIT_FPGA_DDR3);
   
   ClockGenerator7Params clk_params = defaultValue();
   clk_params.clkin1_period     = 5.000;       // 200 MHz reference
   clk_params.clkin_buffer      = False;
   clk_params.reset_stages      = 0;           // no sync on reset so input clock has pll as only load
   clk_params.clkfbout_mult_f   = 5.000;       // 1000 MHz VCO
   clk_params.clkout0_divide_f  = `SCEMI_CLOCK_PERIOD;
   clk_params.clkout1_divide    = 4;           // 250 MHz ddr3 clk
   clk_params.clkout2_divide    = 5;           // 200 MHz ddr3 reference clock

   ClockGenerator7 clk_gen <- mkClockGenerator7(clk_params, clocked_by sys_clk, reset_by sys_rstn);
   
   Reset fpga_rstn <- mkAsyncReset(2, sys_rstn, fpga_clk);
   Reset clkgen_rstn <- mkAsyncReset(2, sys_rstn, clk_gen.clkout0);
   Reset ddr3ref_rstn <- mkAsyncReset(0, sys_rstn, clk_gen.clkout1);
   
   DDR3_Configure ddr3_cfg = defaultValue;
   DDR3_Controller_DNV7F2A ddr3_ctrl <- mkDDR3Controller_DNV7F2A(ddr3_cfg, clk_gen.clkout2, clocked_by clk_gen.clkout1, reset_by ddr3ref_rstn);
   Clock ddr3clk = ddr3_ctrl.user.clock;
   Reset ddr3rstn = ddr3_ctrl.user.reset_n;
   
   SceMiV7Args args;
   args.fpga_clk    = fpga_clk;
   args.fpga_rst    = fpga_rstn;
   args.noc_q_clk   = noc_q_clk;
   args.noc_a_clk   = noc_a_clk;
   args.noc_reset_n = noc_reset_n;
   args.link_type   = PCIE_VIRTEX7;
   
   SceMiV7Ifc#(MemSceMiLayerIfc) scemi <- buildSceMi(mkMemSceMiLayerWrapper, args);
   MemSceMiLayerIfc scemiOrig = scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
      
   mkTieOff(scemi.noc_cont);
   
   let connectDDR3 <- mkConnection(clocked_by uclock, reset_by ureset, scemiLayer, ddr3_ctrl.user);
   
   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   ReadOnly#(Bool) _isCalibrated     <- mkNullCrossing(noClock, ddr3_ctrl.user.init_done);
   
   interface training = scemi.training;
   interface source   = scemi.source;
   interface sink     = scemi.sink;
   interface ddr3     = ddr3_ctrl.ddr3;
   method leds = zeroExtend({pack(_isCalibrated)
			    ,pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
   method isDDRReady = ddr3_ctrl.user.init_done;
   method isOutOfReset = _isOutOfReset;
endmodule: mkBridge

module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule
