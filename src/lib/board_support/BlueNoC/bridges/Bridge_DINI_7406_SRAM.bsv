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
import CommitIfc   :: *;
import Connectable :: *;
import TieOff      :: *;

// We need to get access to the uncontrolled clock and reset to hook up the SRAM
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize, no_default_clock, no_default_reset *)
module mkBridge #( Clock g0_clk
                 , Reset g0_rstn
                 , Clock noc_q_clk
		 , Clock noc_a_clk
                 , Reset noc_reset_n
		 , Clock g1_clk
                 , Reset g1_rstn
                 )
                 (DN7406_FPGA_A_SRAM);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.fpga_clk  	 = g0_clk;
   pcie_args.fpga_rst  	 = g0_rstn;
   pcie_args.noc_q_clk 	 = noc_q_clk;
   pcie_args.noc_a_clk   = noc_a_clk;
   pcie_args.noc_reset_n = noc_reset_n;
   pcie_args.link_type 	 = PCIE_DINI;

   SceMiDiniPCIEIfc#(MemSceMiLayerIfc) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   
   mkTieOff(scemi.noc_cont);

   let sramController <- mkSRAMController(clocked_by g1_clk, reset_by g1_rstn);
   let memConnect     <- mkMemConnection(g1_clk, g1_rstn
                                        ,scemiLayer
                                        ,sramController.user
                                        ,clocked_by uclock, reset_by ureset);


   // use a stub of the the RS232 pins
   RS232_Pins unused_rs232 <- mkStub(clocked_by g1_clk, reset_by g1_rstn);

   ReadOnly#(Bool) _isLinkUp         <- mkNullCrossing(noClock, scemi.isLinkUp);
   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface training = scemi.training;
   interface source   = scemi.source;
   interface sink     = scemi.sink;

   interface rs232 = unused_rs232;

   method leds = zeroExtend({pack(_isLinkUp)
                            ,pack(_isClockAdvancing)
                            ,pack(_isOutOfReset)
                            });

   interface sram  = sramController.sram;
   method isDDRReady = False;
endmodule: mkBridge

(* no_default_clock, no_default_reset *)
module [SceMiModule] mkMemSceMiLayerWrapper(MemSceMiLayerIfc);

    (*hide*) let _m <- mkSceMiLayer();
    Clock uclk <- sceMiGetUClock;
    Reset urst <- sceMiGetUReset;

    interface scemiLayer = _m;
    interface uclock = uclk;
    interface ureset = urst;
endmodule


module mkMemConnection#(Clock memClk, Reset memRst
                        ,RAM_ClientCmt#(Mem_addr_t, Mem_data_t) ramclient
                        ,SRAM_User s
                        ) (Empty)
   provisos (Add#(21, _1, SizeOf#(Mem_addr_t)),
	     Alias#(Mem_data_t, Bit#(64)));

   SyncFIFOIfc#(RAM_Request#(Mem_addr_t, Mem_data_t)
                )  requestFIFO <- mkSyncFIFOFromCC(4, memClk);
   SyncFIFOIfc#(RAM_Response#(Mem_data_t)
                )  responseFIFO <- mkSyncFIFOToCC(4, memClk, memRst);

   let connSyncRamRequest <- mkConnection (requestFIFO, ramclient.request);
   let connSyncRamResponse <- mkConnection (responseFIFO, ramclient.response);
   
   rule connectRequests;
      let x = requestFIFO.first;
      requestFIFO.deq;
      s.put((x.read ? 0 : '1), truncate(x.address), x.data);
   endrule

   rule connectResponses;
      let x <- s.read();
      responseFIFO.enq(x);
   endrule
endmodule
