// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAhb;

import TLM3::*;
import Ahb::*;
import Clocks::*;
import Connectable::*;
import FIFOF::*;
import GetPut::*;
import ClientServer::*;
import XactorsFlow::*;
import XactorsDefines::*;

`include "TLM.defines"



// Stream
interface AhbSlaveStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AhbXtorMaster#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Server#(Chunk, Chunk)  stream;
endinterface

interface AhbMasterStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AhbXtorSlave#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Client#(Chunk, Chunk) stream;
endinterface


///////////////////////////////////////////////////////////////////////////
module mkAhbSlaveStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                                AhbSlaveStreamXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   AhbMasterXActor#(`TLM_XTR) master_xactor <- mkAhbMaster(args.max_flight);
   let master_tlm    <- addFlowControl(args.flow_depth, master_xactor.tlm);

   ApiRecvIFC#(`TLM_RR) receiver <- mkApiReceiverXL (args.max_flight, -1
                                                     , clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, cclk, crst_n, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, cclk);

   mkConnection(master_tlm.tx, toPut(response_fifo));
   mkConnection(master_tlm.rx, toGet(request_fifo));

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   interface bus    = master_xactor.fabric;
   interface stream = receiver.scemi;
endmodule

//////////////////////////////////////////////////////////////////////////
// Slave
///////////////////////////////////////////////////////////////////////////
module mkAhbMasterStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AhbMasterStreamXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   FIFOF#(resp_t) fifo_rx <- mkSizedFIFOF(1);
   FIFOF#(req_t)  fifo_tx <- mkSizedFIFOF(1);
   let slave_xactor <- mkAhbSlaveP(args.write_bypass, True, args.keep_bursts, args.big_endian, fifo_rx, fifo_tx);
   // "True" above allows early reads

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, cclk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, cclk, crst_n, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   mkConnection(toGet(response_fifo), slave_xactor.tlm.rx);
   mkConnection(slave_xactor.tlm.tx, toPut(request_fifo));

   interface bus    = slave_xactor.fabric;
   interface stream = sender.scemi;

endmodule

endpackage
