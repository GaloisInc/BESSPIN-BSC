////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2016 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : SemuServer.bsv
//  Description   : Wrapper around the Readback core that adds Semu controls,
//                  which can be connected to any controlled clock generater
//                  and communication link (SceMi, JTAG, etc).
////////////////////////////////////////////////////////////////////////////////
package SemuServer;

import Clocks::*;
import ConfigReg::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;

import ReadbackCore::*;
import ReadbackDefines::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {SEdges, SQuery, SStop, SResume, RdBackOn, RdBackOff, RdBackCmd, RdBackStore } RdBkCommand deriving (Eq,Bits,FShow);
typedef Bit#(32) RdBackControlReq;

typedef struct {
   RdBackCycleStamp cycle;
   Bool             running;
   Bool             free_running;
   Bool             rdback_on;
   EdgeCount        edges;
} RdBackStatusResp deriving (Eq, Bits, FShow);

typedef UInt#(29) EdgeCount;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function RdBkCommand command(RdBackControlReq req);
   return unpack(req[31:29]);
endfunction

function UInt#(29) getData(RdBackControlReq req);
   return unpack(req[28:0]);
endfunction

function Bool isBrkCode(RdBackControlReq req);
   return (command(req) == RdBackCmd && pack(getData(req))[28] == 1);
endfunction

function Bool isFinish(RdBackControlReq req);
   return (command(req) == RdBackCmd && pack(getData(req))[28] != 1 && pack(getData(req))[27] == 1);
endfunction

function Bool isClear(RdBackControlReq req);
   return (command(req) == RdBackCmd && getData(req) == 0);
endfunction

function Bool isQuery(RdBackControlReq req);
   return (command(req) == SQuery);
endfunction

function Bool isStore(RdBackControlReq req);
   return (command(req) == RdBackStore);
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

interface SemuServer;
   interface Put#(RdBackControlReq)  cmd;
   interface GetS#(RdBackStatusResp) status;
   interface GetS#(Bit#(31))         rdback;

   (* always_enabled *)
   method Action preedge(Bool val);
   (* always_ready *)
   method Bool allow_edge();
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSemuServer #( parameter XilinxFamily family
		     , parameter Bool is_dummy
		     )
		     ( Clock      cclock
		     , Reset      creset
		     , SemuServer ifc
		     );
   
   Clock uclock <- exposeCurrentClock;
   
   CrossingReg#(RdBackCycleStamp)            stamp
     <- mkNullCrossingRegA(uclock, unpack(0), clocked_by cclock, reset_by creset);

   Reg#(ConfigNum)                           cfg        <- mkReg(maxBound);

   FIFO#(RdBackControlReq)                   fCmd       <- mkFIFO;
   FIFO#(RdBackStatusResp)                   fStatus    <- mkFIFO;

   Wire#(Bool)              detected_edge  <- mkBypassWire;

   Reg#(Bool)               stopped        <- mkReg(True);
   Reg#(EdgeCount)          edges_to_allow <- mkReg(0);
   Reg#(Bool)               send_status    <- mkReg(False);
   Reg#(Bool)               free_running   <- mkReg(False);
   Reg#(Bool)               rdback_on      <- mkReg(False);
   FIFOF#(RdBackControlReq) fifo_req       <- mkFIFOF;
   let req = fifo_req.first; 
   Reg#(Bool)               pending_clear  <- mkReg(False);
   Reg#(Bool)               extraRdBack    <- mkReg(False);
   Reg#(Bool)               prevStop       <- mkConfigReg(False);
   Reg#(Bool)               store_empty    <- mkReg(True);
   ReadBackCore             cntrl          <- mkReadbackCore(family, is_dummy);
   PulseWire                set_prev       <- mkPulseWire;
   PulseWire                unset_prev     <- mkPulseWire;
   
   Bool doing_rdback    = rdback_on && cntrl.storeNotEmpty;
   Bool break_condition = cntrl.brkSignal && doing_rdback;
   
   rule do_set (set_prev && !unset_prev);
      prevStop <= True;
   endrule
   
   rule do_uvset (unset_prev);
      prevStop <= False;
   endrule
   
   rule update_cycle;
      stamp <= stamp + 1;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule every_in;
      cntrl.stamp(stamp.crossed);
      cntrl.cfg(cfg);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// 
   ////////////////////////////////////////////////////////////////////////////////
   
   Bool active = !stopped && (edges_to_allow != 0) && cntrl.state == Done &&
   ((!extraRdBack && cntrl.storeIsReady && !pending_clear) || !doing_rdback) && !break_condition;

   rule do_read_query (isQuery(fCmd.first) && !send_status);
      send_status <= True;
      fCmd.deq;
   endrule
   
   rule do_read_clear (isClear(fCmd.first) && !pending_clear);
      pending_clear <= True;
      fCmd.deq;
   endrule
   
   rule do_read_store (isStore(fCmd.first) && !pending_clear);
      fifo_req.enq(fCmd.first);
      fCmd.deq;
   endrule
   
   rule do_read_rest (!isQuery(fCmd.first) && 
		      !isClear(fCmd.first) && 
      		      !isStore(fCmd.first) && 
		      !break_condition);
      fifo_req.enq(fCmd.first);
      // $display("(%0d) RECV: ", $time, fshow(command(fCmd.first)));
      fCmd.deq;
   endrule

   // Edges command now has additional features.
   //  If the number of edges received:
   //   - 'h1FFFFFFF the simulation will be free_running mode (nonstop)
   (* aggressive_implicit_conditions *)
   rule handle_edge_request if (command(req) == SEdges && !active);
      fifo_req.deq;
      EdgeCount numedges = getData(req);
      stopped  <= False;
      free_running <=  (numedges == maxBound);
      edges_to_allow <= numedges;
   endrule

   (* aggressive_implicit_conditions *)
   rule handle_stop_request if (!send_status && command(req) == SStop);
      fifo_req.deq;
      send_status <= True;
      if (!prevStop) 
	 begin 
	    stopped <= True;
	    cntrl.brkClear;
	 end
      set_prev.send;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_resume_request if (!send_status && (command(req) == SResume || command(req) == RdBackOn || command(req) == RdBackOff));
      fifo_req.deq;
      send_status <= True;
      case (command(req))
	 SResume:   stopped    <= False;
	 RdBackOn:  rdback_on  <= True;
	 RdBackOff: rdback_on  <= False;
      endcase
   endrule

   (* aggressive_implicit_conditions *)
   rule handle_clear_request (pending_clear);
      pending_clear <= False;
      cntrl.clear;
      store_empty <= True;
      // $display("(%0d) CLEAR RECEIVED", $time);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_finish_request (isFinish(req));
//      ConfigNum cfg = extend(getData(req));
      fifo_req.deq;
      cntrl.cmd(tagged Finish);
      extraRdBack <= !store_empty;
      cfg <= cfg + 1;
      // $display("(%0d) FINISH RECEIVED (%d)", $time, cfg);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_code_request (isBrkCode(req));
      fifo_req.deq;
      cntrl.brkCode(pack(getData(req))[15:0]);
      // $display("(%0d) CODE RECEIVED", $time);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_store_request (command(req) == RdBackStore);
      store_empty <= False;
      fifo_req.deq;
      UInt#(29) addr = getData(req);
      RdBackStoreCmd cmd = unpack(truncate(pack(addr)));
//      $display("(%0d) STORE! ", $time, fshow(cmd));
      cntrl.cmd(tagged Insert cmd);
   endrule

   (* aggressive_implicit_conditions *)
   rule count_edge if (active && detected_edge);
      // If it is free running then count down to 1
      //  and then let edges allowed go back up to max
//      if (edges_to_allow == 1)   send_status <= True;
      unset_prev.send;
      if (free_running && edges_to_allow == 1)
         edges_to_allow <= maxBound;
      else begin
         edges_to_allow <= edges_to_allow - 1;
      end
   endrule
   
   (* aggressive_implicit_conditions *)
   rule do_break (!send_status && cntrl.state == Done && break_condition);
      RdBackControlReq r = {pack(SStop), 0};
      fifo_req.enq(r);
      if (prevStop) cntrl.brkClear;
   endrule
   
   (* preempts = "handle_clear_request, start_readback" *)
   (* aggressive_implicit_conditions *)
   rule start_readback ((detected_edge || extraRdBack) && doing_rdback && !stopped);
      extraRdBack <= False;
      cntrl.startReadback;
   endrule
     
   (* aggressive_implicit_conditions *)
   rule send_response if (send_status);
      let resp = RdBackStatusResp {
                    cycle: stamp.crossed,
		    running: !stopped,
		    free_running: free_running,
		    rdback_on: rdback_on,
		    edges: edges_to_allow
		 };
      // When the free running mode is stopped, clear the model and edge counter after sending the response
      (*split*)
      if ( stopped && free_running ) begin
         free_running <= False;
         edges_to_allow <= 0;
      end
      fStatus.enq(resp);
      send_status <= False;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface cmd    = toPut(fCmd);
   interface status = fifoToGetS(fStatus);
   interface rdback = cntrl.rdback;

   method preedge = detected_edge._write;
   method allow_edge = active;
   
endmodule: mkSemuServer

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: SemuServer
