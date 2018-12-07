////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$

import SceMi      :: *;
import SceMiLayer :: *;
import NoCUtils   :: *;

// Setup for SCE-MI over PCIE to a Dini board
import Dini        :: *;
import Clocks      :: *;
import DummyDriver :: *;
import TieOff      :: *;

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #( Clock g0_clk
                 , Reset g0_rstn
                 , Clock noc_q_clk
		 , Clock noc_a_clk
		 , Reset noc_reset_n
                 , Clock g1_clk
                 , Reset g1_rstn
                 )
                 (DN7002_FPGA_A);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.fpga_clk    = g0_clk;
   pcie_args.fpga_rst    = g0_rstn;
   pcie_args.noc_q_clk   = noc_q_clk;
   pcie_args.noc_a_clk   = noc_a_clk;
   pcie_args.noc_reset_n = noc_reset_n;
   pcie_args.link_type   = PCIE_DINI;

   SceMiDiniPCIEIfc#(Empty) scemi <- buildSceMi(mkSceMiLayer, pcie_args);
   
   mkTieOff(scemi.noc_cont);
   
   // use a stub of the the RS232 pins
   RS232_Pins unused_rs232 <- mkStub(clocked_by g1_clk, reset_by g1_rstn);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);
   
   interface training = scemi.training;
   interface source   = scemi.source;
   interface sink     = scemi.sink;
      
   interface rs232 = unused_rs232;
      
   method leds = zeroExtend({pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            ,pack(_isLinkUp)
                            });
endmodule: mkBridge
