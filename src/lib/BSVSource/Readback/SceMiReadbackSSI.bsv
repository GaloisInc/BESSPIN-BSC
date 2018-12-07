// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision: 33005 $
// $Date: 2014-01-09 13:07:45 -0500 (Thu, 09 Jan 2014) $

package SceMiReadbackSSI;

import Clocks::*;
import ConfigReg::*;
import Connectable::*;
import DefaultValue::*;
import FIFOF::*;
import GetPut::*;
import ReadbackCoreSSI::*;
import ReadbackCoreSlice::*; // XXX Needed for FrameCmd type
import ReadbackDefines::*;
import SceMiCore::*;
import SceMiXactors::*;
import Vector::*;
import FShow::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {SEdges, SQuery, SStop, SResume, RdBackOn, RdBackOff, RdBackCmd, RdBackStore } RdBkCommand deriving (Eq,Bits,FShow);
typedef Bit#(32) RdBackControlReq;
typedef Bit#(32) RdBackStatusResp;

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
///
////////////////////////////////////////////////////////////////////////////////

function RdBackStatusResp mkStatus(Bool running, Bool freerunning, Bool rdback_on, UInt#(29) edge_count);
   return {pack(running), pack(freerunning) ,pack(rdback_on), pack(edge_count)};
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkRdBackControls#(Integer num,
				       Integer lcl,
				       parameter XilinxFamily family,
				       parameter Bool is_dummy,
				       parameter SceMiClockConfiguration conf)
   (Empty ifc);
   
   Clock uclock <- exposeCurrentClock;
   
   SceMiClockPortIfc               clk_port            <- mkSceMiClockPort(conf);
   let cclock = clk_port.cclock;
   let creset = clk_port.creset;

   CrossingReg#(RdBackCycleStamp)            stamp  
     <- mkNullCrossingRegA(uclock, unpack(0), clocked_by cclock, reset_by creset);
//   ReadOnly#(SceMiCycleStamp)                stamp      <- sceMiCycleStamp;      
   SceMiMessageInPortIfc#(RdBackControlReq)  req_in     <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(RdBackStatusResp) resp_out   <- mkSceMiMessageOutPort();
   
   SceMiOutputPipeParameters args = defaultValue;
   args.vis             = Fifo;
   args.accumulateTimer = 255;
   args.capacity        = 1023;
   SceMiOutputPipeIfc#(1,Bit#(31))           rdback_out <- mkSceMiOutputPipeP(args);
   
   Reg#(Bool)               stopped        <- mkReg(True);
   Reg#(UInt#(29))          edges_to_allow <- mkReg(0);
   Reg#(Bool)               send_status    <- mkReg(False);
   Reg#(Bool)               free_running   <- mkReg(False);
   Reg#(Bool)               rdback_on      <- mkReg(False);
   FIFOF#(RdBackControlReq) fifo_req       <- mkFIFOF;
   let req = fifo_req.first; 
   Reg#(Bool)               pending_clear  <- mkReg(False);
   Reg#(Bool)               extraRdBack    <- mkReg(False);
   Reg#(Bool)               alreadyStopped <- mkConfigReg(True);
   Reg#(Bool)               store_empty    <- mkReg(True);
   ReadBackCore             cntrl          <- mkReadbackCoresInner(num, lcl, family, is_dummy);
   PulseWire                set_prev       <- mkPulseWire;
   PulseWire                unset_prev     <- mkPulseWire;
   Reg#(Bool)               break_done     <- mkReg(True);
   
   Bool doing_rdback    = rdback_on && cntrl.storeNotEmpty;
   Bool break_condition = cntrl.brkSignal && doing_rdback && !alreadyStopped;
   
   rule do_set (set_prev);
      alreadyStopped <= True;
   endrule
   
   rule do_unset (unset_prev && !set_prev);
      alreadyStopped <= False;
   endrule
   
   rule update_cycle;
      stamp <= stamp + 1;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule every_in;
      cntrl.stamp(stamp.crossed);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   (* aggressive_implicit_conditions *)
   rule putData (rdback_out.can_send != 0);
      let cnt <- rdback_out.try_send (1, replicate(cntrl.rdback.first), False);
      if (cnt == 1) begin
   	 // $display("(%0d) ITEM IN: %8x", $time, cntrl.rdback.first);
         cntrl.rdback.deq;
      end
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// 
   ////////////////////////////////////////////////////////////////////////////////
   
   Bool canStart = break_done && cntrl.dataDone;
   
   Bool active = !stopped && (edges_to_allow != 0) && cntrl.isDone && canStart &&
   ((!extraRdBack && cntrl.storeIsReady && !pending_clear) || !doing_rdback) && !break_condition;

   SceMiClockControlIfc    clk_cntrl <- mkSceMiClockControl(conf.clockNum, active, active);
   
   rule do_read_query (isQuery(req_in.read) && !send_status);
      send_status <= True;
      req_in.ack;
   endrule
   
   rule do_read_clear (isClear(req_in.read) && !pending_clear);
      pending_clear <= True;
      req_in.ack;
   endrule

   rule do_read_store (isStore(req_in.read) && !pending_clear);
      fifo_req.enq(req_in.read);
      req_in.ack;
   endrule
   
   rule do_read_rest (!isQuery(req_in.read) && 
   		      !isClear(req_in.read) && 
      		      !isStore(req_in.read) && 
   		      !break_condition);
      fifo_req.enq(req_in.read);
      // $display("(%0d) RECV: ", $time, fshow(command(req_in.read)));
      req_in.ack;
   endrule
      
   // Calling request every cycle for read above
   (* aggressive_implicit_conditions *)
   rule request;
      req_in.request();
   endrule: request

   // Edges command now has additional features.
   //  If the number of edges received:
   //   - 'h1FFFFFFF the simulation will be free_running mode (nonstop)
   (* aggressive_implicit_conditions *)
   rule handle_edge_request if (command(req) == SEdges && !active);
      fifo_req.deq;
      UInt#(29) numedges = getData(req);
      stopped  <= False;
      free_running <=  (numedges == maxBound);
      edges_to_allow <= numedges;
   endrule

   (* aggressive_implicit_conditions *)
   rule handle_stop_request if (!send_status && command(req) == SStop);
      fifo_req.deq;
      send_status <= True;
      stopped <= True;
      break_done <= True;
      cntrl.brkClear;
      set_prev.send;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_last_edge if (!pending_clear && 
			     !fifo_req.notEmpty && 
			     !send_status && edges_to_allow == 0 && !stopped);
      send_status <= True;
      stopped <= True;
      break_done <= True;
      cntrl.brkClear;
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
   rule handle_clear_request (pending_clear && canStart);
      pending_clear <= False;
      cntrl.clear;
      store_empty <= True;
      // $display("(%0d) CLEAR RECEIVED", $time);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_finish_request (isFinish(req));
      fifo_req.deq;
      cntrl.cmd(tagged Finish);
      extraRdBack <= !store_empty;
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

   Bool detected_posedge = (conf.dutyLo != 0) && clk_cntrl.pre_posedge();
   Bool detected_negedge = (conf.dutyHi != 0) && clk_cntrl.pre_negedge();
   
   (* aggressive_implicit_conditions *)
   rule count_edge if (active && (detected_posedge || detected_negedge));
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
   rule tracking_done (!break_done && cntrl.isDone && cntrl.brkIsValid && !break_condition && cntrl.dataDone);
      break_done <= True;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule do_break (!send_status && cntrl.isDone && 
		  break_condition && !break_done &&
		  edges_to_allow != 0);
      RdBackControlReq r = {pack(SStop), 0};
      fifo_req.enq(r);
   endrule
   
   (* preempts = "handle_clear_request, start_readback" *)
   (* preempts = "handle_stop_request,  start_readback" *)
   (* aggressive_implicit_conditions *)
   rule start_readback ((detected_posedge || extraRdBack) && doing_rdback 
      && (!stopped || extraRdBack)
      && canStart
      && !break_condition);
      break_done <= False;
      extraRdBack <= False;
      cntrl.startReadback;
   endrule
     
   (* aggressive_implicit_conditions *)
   rule send_response if (send_status);
      RdBackStatusResp resp = mkStatus(!stopped,free_running,rdback_on,edges_to_allow);
      // When the free running mode is stopped, clear the model and edge counter after sending the response
      (*split*)
      if ( stopped && free_running ) begin
         free_running <= False;
         edges_to_allow <= 0;
      end
      resp_out.send(resp);
      send_status <= False;
   endrule

endmodule

endpackage
