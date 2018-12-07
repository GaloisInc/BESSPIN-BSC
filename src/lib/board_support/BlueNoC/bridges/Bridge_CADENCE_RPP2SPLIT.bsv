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

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #( Clock sys_clk
                 , Reset sys_rstn
		 , Clock fpga_clk
                 , Clock noc_q_clk
		 , Clock noc_a_clk
		 , Reset noc_reset_n
                 )
                 (RPP2SPLIT_FPGA);

   Reset fpga_rstn <- mkAsyncReset(2, sys_rstn, fpga_clk);

   SceMiV7Args args;
   args.fpga_clk    = fpga_clk;
   args.fpga_rst    = fpga_rstn;
   args.noc_q_clk   = noc_q_clk;
   args.noc_a_clk   = noc_a_clk;
   args.noc_reset_n = noc_reset_n;
   args.link_type   = PCIE_VIRTEX7;

   SceMiV7Ifc#(Empty) scemi <- buildSceMi(mkSceMiLayer, args);

   mkTieOff(scemi.noc_cont);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface training = scemi.training;
   interface source   = scemi.source;
   interface sink     = scemi.sink;

   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
			    });
   method isDDRReady = False;
   method isOutOfReset = _isOutOfReset;
endmodule: mkBridge
