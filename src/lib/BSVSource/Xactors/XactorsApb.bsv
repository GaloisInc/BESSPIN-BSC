// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsApb;

import TLM3::*;
import Apb::*;
import Clocks::*;
import Connectable::*;
import FIFOF::*;
import GetPut::*;
import ClientServer::*;
import XactorsFlow::*;
import XactorsDefines::*;

`include "TLM.defines"

// Stream
interface ApbSlaveStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface ApbXtorMaster#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Server#(Chunk, Chunk)  stream;
endinterface

interface ApbMasterStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface ApbXtorSlave#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Client#(Chunk, Chunk) stream;
endinterface

///////////////////////////////////////////////////////////////////////////
module mkApbSlaveStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               ApbSlaveStreamXactor#(`TLM_PRM) ignore)
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

   ApbMasterXActor#(`TLM_XTR) master_xactor <- mkApbMaster(args.big_endian);

   ApiRecvIFC#(`TLM_RR) receiver <- mkApiReceiver(5, clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, cclk, crst_n, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, cclk);

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   mkConnection(toGet(request_fifo), master_xactor.tlm.rx);
   mkConnection(master_xactor.tlm.tx, toPut(response_fifo));

   interface bus    = master_xactor.fabric;
   interface stream = receiver.scemi;


endmodule

//////////////////////////////////////////////////////////////////////////
// Slave
///////////////////////////////////////////////////////////////////////////
module mkApbMasterStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               ApbMasterStreamXactor#(`TLM_PRM) ignore)
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

   ApbSlaveXActor#(`TLM_XTR)   slave_xactor <- mkApbSlave();

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
