// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SFIFO;

import FIFO::*;
import FIFOF::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSafeDepthParamFIFO#(parameter UInt#(32) depth) (FIFO#(a))
   provisos(Bits#(a, sa));
   (* hide *)
   let _ifc <- mkDepthParamFIFO(max(2,depth));
   return _ifc;
endmodule

module mkSafeDepthParamFIFOF#(parameter UInt#(32) depth) (FIFOF#(a))
   provisos(Bits#(a, sa));
   (* hide *)
   let _ifc <- mkDepthParamFIFOF(max(2,depth));
   return _ifc;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface SFIFO#(type a, type b);
   method Action  enq (a value);
   method Action  deq;
   method a       first;
   method Action  clear;
endinterface

module mkSizedSFIFO#(Integer n) (SFIFO#(a, b))
   provisos (Bits#(a,sa), Bits#(b, sb), Arith#(b), Eq#(b));

   // If the queue contains n elements, they are in q[0]..q[n-1].  The head of
   // the queue (the "first" element) is in q[0], the tail in q[n-1].
   
   Tuple2#(b, a) dflt = tuple2(0, ?);
   
   Reg#(Tuple2#(b, a)) q[n];
   for (Integer i=0; i<n; i=i+1)
      q[i] <- mkReg(dflt);
   TCounter c <- mkTCounter(n);

   PulseWire enqueueing <- mkPulseWire;
   Wire#(Tuple2#(b, a)) x_wire <- mkWire;
   PulseWire dequeueing <- mkPulseWire;
   
   let empty = c.isEq(0);
   let full  = c.isEq(n);
   
   (* aggressive_implicit_conditions *)
   rule incCtr (enqueueing && !dequeueing);
      c.incr;
      c.setNext(x_wire, q);
   endrule

   (* aggressive_implicit_conditions *)
   rule decCtr (dequeueing && !enqueueing);
      for (Integer i=0; i<n; i=i+1)
	 q[i] <= (i==(n - 1) ? dflt : q[i + 1]);
      c.decr;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule both (dequeueing && enqueueing);
      for (Integer i=0; i<n; i=i+1)
	 if (!c.isEq(i + 1)) q[i] <= (i==(n - 1) ? dflt : q[i + 1]);
      c.set(x_wire, q);
   endrule

   method Action deq if (!empty);
      match {.n, .d} = q[0];
      if (n == 1)
	 dequeueing.send;
      else
	 q[0] <= tuple2(n-1, d);
   endmethod

   method first if (!empty);
      match {.n, .d} = q[0];
      return d;
   endmethod

   method Action enq(x) if (!full);
      match {.n, .d} =  c.get(q);
      if (n == 0 || (n + 1) == 0 || pack(x) != pack(d))
	 begin
	    enqueueing.send;
	    x_wire <= tuple2(1, x);
	 end
      else
	 c.set(tuple2(n+1, d), q);
   endmethod

//   method notEmpty = !empty;
//   method notFull  = !full;

   method Action clear;
      c.clear;
   endmethod
endmodule

interface TCounter;
   method Action incr;
   method Action decr;
   method Bool isEq(Integer n);
   method Action setNext (b value, Reg#(b) as[]);
   method Action set (b value, Reg#(b) as[]);
   method b getNext(Reg#(b) as[]);
   method b get(Reg#(b) as[]);
   method Action clear;
endinterface

module mkTCtr#(Reg#(UInt#(s)) c)(TCounter);
   method Action incr; c <= c+1; endmethod
   method Action decr; c <= c-1; endmethod
   method isEq(n) = (c==fromInteger(n));
   method Action setNext (b value, Reg#(b) as[]); as[c] <= value; endmethod
   method Action set (b value, Reg#(b) as[]); as[c-1] <= value; endmethod
   method b getNext(Reg#(b) as[]);
      return as[c]._read;
   endmethod
   method b get(Reg#(b) as[]);
      return as[c-1]._read;
   endmethod
   method Action clear; c <= 0; endmethod
endmodule

// A counter which can count up to m inclusive (m known at compile time):
module mkTCounter#(Integer m)(TCounter);
   let _i = ?;
   if      (m<2)      begin Reg#(UInt#(1))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<4)      begin Reg#(UInt#(2))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<8)      begin Reg#(UInt#(3))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<16)     begin Reg#(UInt#(4))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<32)     begin Reg#(UInt#(5))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<64)     begin Reg#(UInt#(6))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<128)    begin Reg#(UInt#(7))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<256)    begin Reg#(UInt#(8))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<512)    begin Reg#(UInt#(9))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<1024)   begin Reg#(UInt#(10)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<2048)   begin Reg#(UInt#(11)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<4096)   begin Reg#(UInt#(12)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<8192)   begin Reg#(UInt#(13)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<16384)  begin Reg#(UInt#(14)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<32768)  begin Reg#(UInt#(15)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<65536)  begin Reg#(UInt#(16)) r <- mkReg(0); _i <- mkTCtr(r); end
   return _i;
endmodule


module mkSafeDepthParamSFIFO#(parameter UInt#(32)  depth) (SFIFO#(a, b))
   provisos (Bits#(a,sa), Bits#(b, sb), Arith#(b), Bounded#(b), Eq#(b), Eq#(a));

   Reg#(Maybe#(Tuple2#(b, a))) head_reg <- mkReg(tagged Invalid);
   Reg#(Maybe#(Tuple2#(b, a))) tail_reg <- mkReg(tagged Invalid);
   FIFOF#(Tuple2#(b, a))       fifo     <- mkSafeDepthParamFIFOF(depth);

   PulseWire deq_pw <- mkPulseWire;
   PulseWire deq_tail_count <- mkPulseWire;

   Wire#(a)             enqValue <- mkWire;
   RWire#(a) deqValue  <- mkRWire;

   Bool can_enq = fifo.notFull;
   Bool normalOp = fifo.notEmpty || isValid(head_reg);

   // Noraml enqueue operations
   (* aggressive_implicit_conditions *)
   rule nEnqMove (normalOp &&& tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt == maxBound) || (dt != enqValue) ) );
      fifo.enq(tuple2(nt,dt));
      tail_reg <= tagged Valid tuple2(1,enqValue);
   endrule
   (* aggressive_implicit_conditions *)
   rule nEnqIncr (normalOp &&& tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt != maxBound) && (dt == enqValue) ) );
      tail_reg <= tagged Valid tuple2(nt+1,enqValue);
   endrule

   // Normal Deq operation
   (* aggressive_implicit_conditions *)
   rule nDeqDecr (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh != 1);
      head_reg <= tagged Valid tuple2 (nh-1,dh);
   endrule
   (*preempts = "nDeqMove, nDeqLast"*)
   (* aggressive_implicit_conditions *)
   rule nDeqMove (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh == 1);
      head_reg <= tagged Valid (fifo.first);
      fifo.deq;
   endrule
   (* aggressive_implicit_conditions *)
   rule nDeqLast (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh == 1 );
      head_reg <= tagged Invalid;
   endrule

   // Special case rules

   // Enq and Deq with last element in head and fifo is empty
   (* preempts = "sEnqDeqLast, (nEnqMove, nDeqLast)" *)
   (* aggressive_implicit_conditions *)
   rule sEnqDeqLast ((! fifo.notEmpty) &&& deq_pw &&&
                     head_reg matches tagged Valid { .nh, .dh} &&&
                     nh == 1 &&&
                     tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt == maxBound) || (dt != enqValue) ));
      tail_reg <= tagged Valid tuple2(1,enqValue);
      head_reg <= tail_reg;
   endrule

   (* aggressive_implicit_conditions *)
   rule sEnqFirst (tail_reg matches tagged Invalid);
      tail_reg <= tagged Valid tuple2(1, enqValue);
   endrule
   (* aggressive_implicit_conditions *)
   rule sEnqMove ( (!normalOp) && (!deq_pw) &&&
                  tail_reg matches tagged Valid {.nt, .dt} &&&
                  ( (nt == maxBound) || (dt != enqValue) ) );
      head_reg <= tail_reg;
      tail_reg <= tagged Valid tuple2(1,enqValue);
   endrule
   (* aggressive_implicit_conditions *)
   rule sEnqIncr ( (!normalOp) && (!deq_pw) &&&
                  tail_reg matches tagged Valid {.nt, .dt} &&&
                  ( (nt != maxBound) && (dt == enqValue) ) );
      tail_reg <= tagged Valid tuple2(nt+1,enqValue);
   endrule

   (*preempts = "sEnqDeq, (sDeqLast,sDeqDecr)"*)
   (* aggressive_implicit_conditions *)
   rule sEnqDeq ( (!normalOp) && deq_pw &&&
                 tail_reg matches tagged Valid {.nt, .dt});
      if (dt != enqValue) begin
         if (nt != 1) begin
            head_reg <= tagged Valid tuple2 (nt-1, dt);
         end                    // else head_reg <= tagged Invalid (same as hold)
         tail_reg <= tagged Valid tuple2 (1,enqValue);
      end
      else begin
         noAction; // +1 - 1
      end
   endrule

   (* aggressive_implicit_conditions *)
   rule sDeqDecr ( (!normalOp) && deq_pw &&&
                  tail_reg matches tagged Valid { .nt, .dt } &&&
                  nt != 1 );
      tail_reg <= tagged Valid tuple2(nt-1, dt);
   endrule
   (* aggressive_implicit_conditions *)
   rule sDeqLast ( (!normalOp) && deq_pw &&&
                  tail_reg matches tagged Valid { .nt, .dt } &&&
                  nt == 1 );
      tail_reg <= tagged Invalid;
   endrule


   // Rule to move deq value to a wire
   (* aggressive_implicit_conditions *)
   rule firstFromTail (head_reg matches tagged Invalid &&&
                           tail_reg matches tagged Valid {.n0, .d0});
      deqValue.wset (d0);
   endrule

   (* aggressive_implicit_conditions *)
   rule firstFromHead (head_reg matches tagged Valid {.n0, .d0});
      deqValue.wset (d0);
   endrule


   method Action enq(a x) if (can_enq);
      enqValue <= x;
   endmethod

   method Action deq() if (deqValue.wget matches tagged Valid .d);
      deq_pw.send;
   endmethod

   method first() if (deqValue.wget matches tagged Valid .d);
      return d;
   endmethod

   method Action clear;
      fifo.clear;
      head_reg <= tagged Invalid;
      tail_reg <= tagged Invalid;
   endmethod

   //method Bool notFull = can_enq;
   //method Bool notEmpty = (deqValue matches tagged Valid .d);

endmodule


function FIFO#(a) fromSFIFO (SFIFO#(a, b) ifc);
   
   return (interface FIFO;
	      method enq   = ifc.enq;
	      method deq   = ifc.deq;
	      method first = ifc.first;
	      method clear = ifc.clear;
	   endinterface);

endfunction

endpackage

