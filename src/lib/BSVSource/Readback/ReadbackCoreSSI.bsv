// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision: 33005 $
// $Date: 2014-01-09 13:07:45 -0500 (Thu, 09 Jan 2014) $

package ReadbackCoreSSI;

import Break::*;
import Collector::*;
import Connectable::*;
import DefaultValue::*;
import FIFO::*;
import FIFOF::*;
import FShow::*;
import GetPut::*;
import OInt::*;
import ReadbackDefines::*;
import ReadbackCoreSlice::*;
import ReadbackPayload::*;
import Shuffle::*;
import SFIFO::*;
import Store::*;
import Vector::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface ReadBackCore;
   interface GetS#(Bit#(31)) rdback;
   method Action startReadback;
   (* always_ready, always_enabled *)
   method Action stamp (RdBackCycleStamp value);
   method Bool   isDone;
   method Bool   dataDone;
   method Action clear;
   method Action cmd (FrameStoreCmd#(RdBackStoreCmd) value);
   (* always_ready *)
   method Bool   brkSignal;
   (* always_ready *)
   method Bool   brkIsValid;
   method Bool   flush;
   method Action brkClear;
   method Action brkCode(Bit#(16) value);
   (* always_ready *)
   method Bool   storeIsReady;
   method Bool   storeNotEmpty;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkReadbackCoresInner#(Integer num, Integer lcl, XilinxFamily family, Bool is_dummy) (ReadBackCore);
   
   FIFO#(Bit#(31))                  fifo_rdback   <- mkFIFO;
   BreakTracker#(4)                 break_tracker <- mkBreakTracker;
   Reg#(Bool)                       no_sends      <- mkReg(True);
   Reg#(Maybe#(BitConstraints#(4))) bc_current    <- mkReg(tagged Invalid);
   Reg#(CompAddrType)               slr_current   <- mkReg(0);
   
   ToPayload#(30, Bit#(1))          to_words      <- toPayload;
   Reg#(Bool)                       flush_words   <- mkReg(False);
   Reg#(Bool)                       data_done     <- mkReg(True);
//   Reg#(Bool)                       can_start     <- mkReg(True);
   Reg#(Bool)                       preamble_sent <- mkReg(False);
   
   Bool break_condition = break_tracker.isValid && break_tracker.value;

   ReadBackCoreSlice slices[num];
   
   Get#(BitConstraints#(4)) ifc_min <- mkBCDummy;
   Get#(BitConstraints#(4)) ifc_max <- mkBCDummy;
   Get#(BitConstraints#(4)) ifc_pre  = ifc_min;
   Get#(BitConstraints#(4)) ifc_post = ifc_max;
   
   for (Integer i = 0; i < num; i = i + 1) begin
      if (i < lcl) begin
	 slices[i] <- mkReadbackCoreSlice(i, False, True, family, is_dummy);
	 if (i == 0) begin
	    mkConnection(ifc_min, slices[i].bc.rx);
	 end 
	 if (i != 0) begin
	    mkConnection(slices[i-1].bc.tx, slices[i].bc.rx);
	 end
	 ifc_pre = slices[i].bc.tx;
      end
   end
   
   for (Integer j = num; j > 0; j = j - 1) begin
      let i = j -1;
      if (i > lcl) begin
	 slices[i] <- mkReadbackCoreSlice(i, False, False, family, is_dummy);
	 if (j == num) begin
	    mkConnection(ifc_max, slices[i].bc.rx);
	 end 
	 if (j != num) begin
	    mkConnection(slices[i+1].bc.tx, slices[i].bc.rx);
	 end
	 ifc_post = slices[i].bc.tx;
      end
   end
   
   slices[lcl] <- mkReadbackCoreSlice(lcl, True, False, family, is_dummy);
   mkConnection(slices[lcl].bc.rx, ifc_post);
   Shuffle#(BitConstraints#(4)) reorder_lcl <- mkShuffleSwitch(True);
   mkConnection(ifc_pre, reorder_lcl.rx_0);
   mkConnection(slices[lcl].bc.tx, reorder_lcl.rx_1);
   
   RxTx#(BitConstraints#(4)) cleanup <- mkBCCleanup;
   
   rule grab_preamble (!to_words.notEmpty && data_done);
      let value = slices[lcl].rdback.first;
      Bit#(2) kind = truncateLSB(pack(value));
      if (kind == 3) preamble_sent <= True;
      if (kind == 3) data_done     <= False;
      slices[lcl].rdback.deq;
//      $display("(%0d) GRABBING PREAMBLE: %8h", $time, value);
      if (!no_sends) fifo_rdback.enq(value);
   endrule
   
   rule feed_cleanup;
      let bc <- reorder_lcl.tx.get;
      cleanup.rx.put(bc);
   endrule
   
   rule grab (!flush_words && preamble_sent);
      let bc <- cleanup.tx.get;
//      $display("(%0d) GRABBING BC: %d ", $time, bc.num, fshow(bc));
      if (bc.isLast) flush_words   <= True;
      if (bc.isLast) preamble_sent <= False;
      if (bc.isSent && !bc.isDummy) to_words.rx.put(bc.value);
      if (!bc.isDummy)
	 begin
	    break_tracker.rx.put(bc);
	 end
   endrule
   
   (* aggressive_implicit_conditions *)
    rule do_flush (flush_words);
       to_words.flush;
    endrule
   
   rule do_finish_flush (flush_words && !to_words.notEmpty);
      flush_words <= False;
      data_done   <= True;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule drain_words (to_words.notEmpty);
      Vector#(30, Maybe#(Bit#(1))) value <- to_words.tx.get;
      let f = fromMaybe(0);
      Bit#(30) data = pack(map(f, value)); 
      // Tag data words with 1'b0
      fifo_rdback.enq({1'b0, data});
//      $display("(%0d) GRABBING WORD: %8h", $time, pack(map(f, value)));
   endrule
   
   function Bool allDone ();
      Bool done = True;
      for (Integer i = 0; i < num; i = i + 1)
	 done = done && slices[i].isDone;
      return done;
   endfunction

   function Bool allReady ();
      Bool ready = True;
      for (Integer i = 0; i < num; i = i + 1)
	 ready = ready && slices[i].storeIsReady;
      return ready;
   endfunction
   
   function Bool allEmpty ();
      Bool empty = True;
      for (Integer i = 0; i < num; i = i + 1)
	 empty = empty && !slices[i].storeNotEmpty;
      return empty;
   endfunction
   
   interface rdback = fifoToGetS(fifo_rdback);
   
   method Action startReadback if (!allEmpty && data_done);
      break_tracker.clear;
      for (Integer i = 0; i < num; i = i + 1)
	 slices[i].startReadback;
   endmethod
    
   method Action stamp (RdBackCycleStamp value);
      for (Integer i = 0; i < num; i = i + 1) begin
	 RdBackCycleStamp value_lcl = (i == lcl) ? value : 0;
	 slices[i].stamp(value_lcl);
      end
   endmethod

   method Bool isDone ();
      return allDone;
   endmethod
   
   method Bool dataDone ();
      return data_done;
   endmethod

   // clear
   method Action clear;
      no_sends <= True;
     for (Integer i = 0; i < num; i = i + 1)
	slices[i].clear;
   endmethod
   
   method Action cmd (value);
      if (value matches tagged Insert (tagged Slr .s))
	 slr_current <= s;
      else if (value matches tagged Insert .i)
	 slices[slr_current].cmd(value);
      if (value matches tagged Finish) begin
	 for (Integer i = 0; i < num; i = i + 1)
	    slices[i].cmd(value);
      end
      if (value matches tagged Insert (tagged Offset .o))
	 begin
       	    if (isConstraint(o))
       	       begin
       		  BitConstraints#(4) bc = toBitConstraints(o);
       		  if (bc.isSent) no_sends <= False;
       		  bc_current <= tagged Valid bc;
       	       end
       	    else 
       	       begin
       		  if (bc_current matches tagged Invalid)
       		     no_sends <= False;
       		  bc_current <= tagged Invalid;
       	      end
       	 end
   endmethod

   method Bool brkSignal;
      return break_condition;
   endmethod
   
   method Bool brkIsValid;
      return break_tracker.isValid;
   endmethod
   
   method Bool flush;
      return flush_words;
   endmethod
   
   method Action brkClear;
      break_tracker.clear;
   endmethod
   
   method Action brkCode(Bit#(16) value);
      break_tracker.code.put(value);
   endmethod
   
   method storeIsReady  = allReady;
   method storeNotEmpty = !allEmpty;
   
endmodule

module mkBCDummy (Get#(BitConstraints#(4)));
   
   method ActionValue#(BitConstraints#(4)) get;
      BitConstraints#(4) bc = unpack(0);
      bc.isLast  = True;
      bc.isDummy = True;
      bc.num     = unpack('1);
      return bc;
   endmethod
   
endmodule

module mkBCCleanup (RxTx#(BitConstraints#(4)));
   
   FIFOF#(BitConstraints#(4)) fifo_0 <- mkFIFOF;
   FIFOF#(BitConstraints#(4)) fifo_1 <- mkFIFOF;
   FIFOF#(BitConstraints#(4)) fifo_2 <- mkFIFOF;
   
   rule x0 (!fifo_0.first.isDummy);
      fifo_1.enq(fifo_0.first);
      fifo_0.deq;
   endrule
   
   rule x1 (!fifo_0.first.isDummy);
      fifo_2.enq(fifo_1.first);
      fifo_1.deq;
   endrule

   rule x2 (fifo_1.first.isLast);
      fifo_2.enq(fifo_1.first);
      fifo_1.deq;
   endrule
   
   rule transfer_last (fifo_0.first.isDummy && !fifo_1.first.isLast);
      let value = fifo_1.first;
      value.isLast = True;
      fifo_2.enq(value);
      fifo_0.deq;
      fifo_1.deq;
   endrule
	     
   interface tx = toGet(fifo_2);

   interface Put rx;
      method Action put (value);
	 if (!value.isDummy || value.isLast)
	    fifo_0.enq(value);
      endmethod
   endinterface
   
endmodule

endpackage

