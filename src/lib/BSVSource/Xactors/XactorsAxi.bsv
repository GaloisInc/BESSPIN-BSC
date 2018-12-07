// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAxi;

import TLM3::*;
import Axi::*;
import Clocks::*;
import Connectable::*;
import GetPut::*;
import ClientServer::*;
import XactorsFlow::*;
import XactorsDefines::*;

`include "TLM.defines"


// Stream
interface AxiRdSlaveStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Server#(Chunk, Chunk)  stream;
endinterface

interface AxiRdMasterStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Client#(Chunk, Chunk) stream;
endinterface

interface AxiWrSlaveStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Server#(Chunk, Chunk)  stream;
endinterface

interface AxiWrMasterStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) bus;
   (* prefix = "" *)
   interface Client#(Chunk, Chunk) stream;
endinterface

interface AxiRdWrSlaveStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) write;
   (* prefix = "" *)
   interface Server#(Chunk, Chunk)  stream;
endinterface

interface AxiRdWrMasterStreamXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) write;
   (* prefix = "" *)
   interface Client#(Chunk, Chunk) stream;
endinterface


///////////////////////////////////////////////////////////////////////////
module mkAxiRdSlaveStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AxiRdSlaveStreamXactor#(`TLM_PRM) ignore)
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

   AxiRdMasterXActorIFC#(`TLM_XTR) master_xactor <- mkAxiRdMaster(args.max_flight);
   let master_tlm = master_xactor.tlm;

   ApiRecvIFC#(`TLM_RR_NSTD) receiver <- mkApiReceiverXL(args.max_flight, 4, clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, cclk, crst_n, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, cclk);

   mkConnection(master_tlm.tx, toPut(response_fifo));
   mkConnection(master_tlm.rx, toGet(request_fifo));

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   interface bus       = master_xactor.fabric.bus;
   interface stream    = receiver.scemi;

endmodule

//////////////////////////////////////////////////////////////////////////
module mkAxiWrSlaveStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AxiWrSlaveStreamXactor#(`TLM_PRM) ignore)
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

   AxiWrMasterXActorIFC#(`TLM_XTR) master_xactor <- mkAxiWrMaster (args.max_flight, args.big_endian);
   TLMRecvIFC#(`TLM_RR) master_tlm = master_xactor.tlm;

   ApiRecvIFC#(`TLM_RR_NSTD) receiver <- mkApiReceiverXL(args.max_flight, 4, clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, cclk, crst_n, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, cclk);

   mkConnection(master_tlm.tx, toPut(response_fifo));
   mkConnection(master_tlm.rx, toGet(request_fifo));

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   interface bus       = master_xactor.fabric.bus;
   interface stream    = receiver.scemi;

endmodule


//////////////////////////////////////////////////////////////////////////
module mkAxiRdWrSlaveStreamXactor (TLMXActorArgs args,
                                 (* osc="cclock", gate_inhigh *) Clock cclk,
                                 (* reset="creset_n" *) Reset crst_n,
                                 AxiRdWrSlaveStreamXactor#(`TLM_PRM) ignore)
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

   AxiRdMasterXActorIFC#(`TLM_XTR) master_rd_xactor <- mkAxiRdMaster(args.max_flight);
   AxiWrMasterXActorIFC#(`TLM_XTR) master_wr_xactor <- mkAxiWrMaster(args.max_flight, args.big_endian);

   TLMTransformIFC#(`TLM_RR) buffer <- mkFlowBuffer(args.flow_depth);

   ApiRecvIFC#(`TLM_RR_NSTD) receiver <- mkApiReceiverXL(args.max_flight, 4, clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, cclk, crst_n, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, cclk);

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   mkConnection(toGet(request_fifo), buffer.in.rx);
   mkConnection(toPut(response_fifo), buffer.in.tx);

   let split <- mkTLMSplit(args.max_flight, buffer.out);

   mkConnection(split.read,  master_rd_xactor.tlm);
   mkConnection(split.write, master_wr_xactor.tlm);

   interface read      = master_rd_xactor.fabric.bus;
   interface write     = master_wr_xactor.fabric.bus;
   interface stream    = receiver.scemi;

endmodule

//////////////////////////////////////////////////////////////////////////
// Slaves
///////////////////////////////////////////////////////////////////////////
module mkAxiRdMasterStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AxiRdMasterStreamXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   AxiRdSlaveXActorIFC#(`TLM_XTR) slave_xactor <- mkAxiRdSlaveSynth(args.keep_bursts);

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, cclk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, cclk, crst_n, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   mkConnection(toGet(response_fifo), slave_xactor.tlm.rx);
   mkConnection(slave_xactor.tlm.tx, toPut(request_fifo));

   interface bus    = slave_xactor.fabric.bus;
   interface stream = sender.scemi;
endmodule

///////////////////////////////////////////////////////////////////////////
module mkAxiWrMasterStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AxiWrMasterStreamXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   AxiWrSlaveXActorIFC#(`TLM_XTR) slave_xactor <- mkAxiWrSlaveSynth(args.keep_bursts, args.interleave_depth);

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, cclk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, cclk, crst_n, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   mkConnection(toGet(response_fifo), slave_xactor.tlm.rx);
   mkConnection(slave_xactor.tlm.tx, toPut(request_fifo));

   interface bus    = slave_xactor.fabric.bus;
   interface stream = sender.scemi;
endmodule

///////////////////////////////////////////////////////////////////////////
module mkAxiRdWrMasterStreamXactor (TLMXActorArgs args,
                               (* osc="cclock", gate_inhigh *) Clock cclk,
                               (* reset="creset_n" *) Reset crst_n,
                               AxiRdWrMasterStreamXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   AxiRdSlaveXActorIFC#(`TLM_XTR) slave_rd_xactor <- mkAxiRdSlaveSynth(args.keep_bursts);
   AxiWrSlaveXActorIFC#(`TLM_XTR) slave_wr_xactor <- mkAxiWrSlaveSynth(args.keep_bursts, args.interleave_depth);

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by cclk, reset_by crst_n);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, cclk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, cclk, crst_n, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   TLMRecvIFC#(`TLM_RR) ifc = (interface TLMRecvIFC
                                  interface tx = toGet(response_fifo);
                                  interface rx = toPut(request_fifo);
                               endinterface);

   let joiner <- mkTLMJoin(ifc);

   mkConnection(joiner.read,  slave_rd_xactor.tlm);
   mkConnection(joiner.write, slave_wr_xactor.tlm);

   interface read      = slave_rd_xactor.fabric.bus;
   interface write     = slave_wr_xactor.fabric.bus;
   interface stream    = sender.scemi;

endmodule

endpackage
