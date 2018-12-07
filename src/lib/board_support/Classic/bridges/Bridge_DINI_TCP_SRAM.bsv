////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$

// Local Import
import SceMiLayer::*;

import SceMi::*;
import Dini::*;
import Clocks::*;
import ClientServer::*;
import GetPut::*;
import DummyDriver :: *;
import CommitIfc::*;
import Connectable::*;
import Vector::*;

// We need to get access to the uncontrolled clock and reset to hook up the SRAM
interface MemSceMiLayerIfc;
    interface SceMiLayer scemiLayer;
    interface Clock uclock;
    interface Reset ureset;
endinterface

(* synthesize *)
module mkBridge #(Clock sample_clk
                  , Reset sample_rstn
                  , Clock g1_clk
                  , Reset g1_rstn
                  ) (Empty);
   
   
   Clock defClock <- exposeCurrentClock;
   Reset defReset <- exposeCurrentReset;
      
   MemSceMiLayerIfc scemi <- buildSceMi(mkMemSceMiLayerWrapper, TCP);
   Clock uclock = scemi.uclock;
   Reset ureset = scemi.ureset;
   SceMiLayer scemiLayer = scemi.scemiLayer;
   
   Vector#(2,Clock) sram_clks = replicate(g1_clk);
   SRAM_Memory_Ifc sramModel <- mkSRAMMemory(sram_clks);
   
   let sramController <- mkSRAMController(clocked_by g1_clk, reset_by g1_rstn);
   let memConnect     <- mkMemConnection(g1_clk, g1_rstn
                                         ,scemiLayer
                                         ,sramController.user
                                         ,clocked_by uclock, reset_by ureset);
   let connMemPins <- mkConnection(sramModel, sramController.sram);

endmodule: mkBridge

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
