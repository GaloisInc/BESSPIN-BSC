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
SceMiLinkType lt = PCIE_DINI;

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
                  ) (DN7406_FPGA_A_SRAM);

   SceMiDiniPCIEArgs pcie_args;
   pcie_args.pci_sample_clk = sample_clk;
   pcie_args.pci_sample_rstn = sample_rstn;
   pcie_args.link_type = lt;
   SceMiDiniPCIEIfc#(MemSceMiLayerIfc) scemi <- buildSceMi(mkMemSceMiLayerWrapper, pcie_args);
   MemSceMiLayerIfc scemiOrig =  scemi.orig_ifc;
   let uclock = scemiOrig.uclock;
   let ureset = scemiOrig.ureset;
   SceMiLayer scemiLayer = scemiOrig.scemiLayer;
   let sramController <- mkSRAMController(clocked_by uclock, reset_by ureset);
   let memConnect     <- mkMemConnection(scemiLayer, sramController.user
                                 , clocked_by uclock, reset_by ureset);

   RS232_Pins unused_rs232 <- mkStub();

   ReadOnly#(Bool) _isOutOfReset     <- mkNullCrossing(noClock, scemi.isOutOfReset);
   ReadOnly#(Bool) _isClockAdvancing <- mkNullCrossing(noClock, scemi.isClockAdvancing);

   interface pcie  = scemi.pcie;
   interface rs232 = unused_rs232;
   interface sram  = sramController.sram;
   method leds = zeroExtend({pack(_isClockAdvancing)
			    ,pack(_isOutOfReset)
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


module mkMemConnection#(RAM_ClientCmt#(Mem_addr_t, Mem_data_t) ramclient, SRAM_User s)(Empty)
   provisos (Add#(21, _1, SizeOf#(Mem_addr_t)),
	     Alias#(Mem_data_t, Bit#(64)));

   RAM_Client#(Mem_addr_t, Mem_data_t) clientConv <- mkClientFromClientCommit(ramclient);
   let c = clientConv;
   rule connectRequests;
      let x <- c.request.get();
      s.put((x.read ? 0 : '1), truncate(x.address), x.data);
   endrule

   rule connectResponses;
      let x <- s.read();
      c.response.put(x);
   endrule
endmodule
