// Copyright (c) 2013-2016 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package ReadbackPayload;

import GetPut::*;
import Vector::*;
import BUtils::*;
import FIFOF::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface DataBuffer#(type a, type b);
   interface Get#(b)  tx;
   interface Put#(a)  rx;
   method Action      flush;
   method Action      clear;
   method Bool        notEmpty;
endinterface

typedef DataBuffer#(a, Vector#(m, Maybe#(a))) ToPayload#(numeric type m, type a);
typedef DataBuffer#(Vector#(m, Maybe#(a)), a) FromPayload#(numeric type m, type a);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module toPayload (ToPayload#(m, a))
   provisos(Bits#(a, sa));
   
   ToPayload#(m, a)_ifc = ?;
   if (valueOf(m) == 1)
      _ifc <- toPayload1;
   else
      _ifc <- toPayloadM;
   return _ifc;
   
endmodule

module toPayload1 (ToPayload#(m, a))
   provisos(Bits#(a, sa));
   
   FIFOF#(a) fifo <- mkFIFOF;

   interface rx    = toPut(fifo);
   interface Get tx;
      method ActionValue#(Vector#(m, Maybe#(a))) get;
	 let value = replicate(tagged Valid fifo.first);
	 fifo.deq;
	 return value;
      endmethod
   endinterface
   method    clear    = fifo.clear;
   method    flush    = noAction;
   method    notEmpty = fifo.notEmpty;
endmodule   

module toPayloadM (ToPayload#(m, a))
   provisos(Bits#(a, sa));
   
   Vector#(m, Maybe#(a))         init      = replicate(tagged Invalid);
   
   Reg#(Vector#(m, Maybe#(a)))   current      <- mkReg(init);
   RWire#(Vector#(m, Maybe#(a))) current_wire <- mkRWire;
   Reg#(LBit#(m))                count        <- mkReg(0);     
   PulseWire                     do_enq       <- mkPulseWire;
   PulseWire                     do_deq       <- mkPulseWire;

   Reg#(Bool)                    asap         <- mkReg(False);

   PulseWire                     clear_pw     <- mkPulseWire;
   PulseWire                     flush_pw     <- mkPulseWire;

   Bool full    = count == fromInteger(valueOf(m));
   Bool empty   = count == 0;
   Bool deq_ok  = full || (!empty && asap);
   
   (* preempts="do_clear, enq_update_payload" *)
   (* preempts="do_clear, update_asap" *)
   (* preempts="do_clear, deq_update" *)
   rule do_clear (clear_pw);
      current <= init;
      asap <= False;
   endrule
   
   (* preempts="enq_update_payload, deq_update" *)
   rule enq_update_payload (current_wire.wget matches tagged Valid .v);
      current <= v;
   endrule

   (* preempts="deq_update, update_asap" *)
   rule deq_update (do_deq);
      current <= init;
      asap <= False;
   endrule

   rule update_asap (flush_pw);
      asap <= True;
   endrule
   
   rule update_count_0 (do_enq && do_deq);
      count <= 1;
   endrule
   
   rule update_count_1 (do_enq && !do_deq);
      count <= count + 1;
   endrule
   
   rule update_count_2 (!do_enq && do_deq);
      count <= 0;
   endrule
   
   interface Put rx;		    
      method Action put(a value) if (!full || do_deq);
	 do_enq.send;
	 let prev = (do_deq) ? init : current;
	 prev = shiftInAt0(prev, tagged Valid value);
	 current_wire.wset(prev);
      endmethod
   endinterface
   
   interface Get tx;
      method ActionValue#(Vector#(m, Maybe#(a))) get if (deq_ok);
	 do_deq.send;
         return current;
      endmethod
   endinterface

   method Action flush;
      if (!asap && !empty)
	 begin
	    // XYZ $display("(%0d) FLUSHX with count = %d", $time, count);
	    flush_pw.send;
	 end
   endmethod
   
   method Action clear;
      clear_pw.send;
   endmethod
   
   method Bool notEmpty;
      return !empty;
   endmethod
   
endmodule




endpackage
