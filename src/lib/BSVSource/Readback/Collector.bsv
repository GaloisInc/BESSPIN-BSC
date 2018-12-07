// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Collector;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Vector::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Collector#(numeric type n, type a);
   interface Put#(Vector#(n, Maybe#(a))) rx;
   interface Get#(a)                     tx;
   method Bool                           isEmpty;
endinterface

function Vector#(n, Maybe#(a)) shift_over (Integer m, Vector#(n, Maybe#(a)) values);
   Bool empty = True;
   for (Integer i = 0; i < m; i = i + 1)
      if (isValid(values[i])) empty = False;
   if (empty)
      begin
	 Vector#(n, Maybe#(a)) mod = ?;
	 for (Integer i = 0; i < valueOf(n); i = i + 1)
	    begin
	       let j = i + m;
	       Maybe#(a) v = tagged Invalid;
	       if (j < valueOf(n)) v = values[j];
	       mod[i] = v;
	    end
	 return mod;
      end
   else
      return values;
endfunction

function Vector#(n, Maybe#(a)) eliminate (Integer m, Vector#(n, Maybe#(a)) values);

   Bool gap = !isValid(values[m]);
   Vector#(n, Maybe#(a)) mod = ?;
   for (Integer i = 0; i < valueOf(n); i = i + 1)
      begin
	 if (gap && i >= m)
	    mod[i] = (i == (valueOf(n) - 1)) ? tagged Invalid : values[i + 1];
	 else
	    mod[i] = values[i];
      end
   return mod;
endfunction

module mkCollectorOrig#(Integer depth) (Collector#(n, a))
   provisos(Bits#(a, sa), Eq#(a));
   
   FIFOF#(Vector#(n, Maybe#(a))) fifo_in  <- mkLFIFOF;
   FIFOF#(Vector#(n, Maybe#(a))) fifo_0   <- mkLFIFOF;
   FIFOF#(Vector#(n, Maybe#(a))) fifo_1   <- mkLFIFOF;
   FIFOF#(Vector#(n, Maybe#(a))) fifo_2   <- mkLFIFOF;
   FIFOF#(a)                     fifo_out <- mkLFIFOF;
   
   Reg#(Vector#(n, Maybe#(a)))   data_reg <- mkReg(replicate(tagged Invalid));
   
   Bool is_empty = !(fifo_in.notEmpty || fifo_0.notEmpty || fifo_1.notEmpty || fifo_2.notEmpty || fifo_out.notEmpty) && data_reg == replicate(tagged Invalid);
   
   (* preempts = "drop_empty, pass_on_0" *)
   (* aggressive_implicit_conditions *)
   rule drop_empty (fifo_in.first == replicate(tagged Invalid));
      fifo_in.deq;
   endrule

   (* aggressive_implicit_conditions *)
   rule pass_on_0;
      fifo_in.deq;
      fifo_0.enq(shift_over(8, fifo_in.first));
   endrule
   
   (* aggressive_implicit_conditions *)
   rule pass_on_1;
      fifo_0.deq;
      fifo_1.enq(shift_over(4, fifo_0.first));
   endrule
   
   (* aggressive_implicit_conditions *)
   rule pass_on_2;
      fifo_1.deq;
      fifo_2.enq(shift_over(2, fifo_1.first));
   endrule

   (* preempts = "shift_one_0, shift_one_1" *)   
   (* aggressive_implicit_conditions *)
   rule shift_one_0;
      if (data_reg[0] matches tagged Valid .v) fifo_out.enq(v);
      let next = shiftInAtN(data_reg, tagged Invalid);
      if (next == replicate(tagged Invalid)) begin
	 next = shift_over(1, fifo_2.first);
	 fifo_2.deq;
      end
      data_reg <= next;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule shift_one_1;
      if (data_reg[0] matches tagged Valid .v) fifo_out.enq(v);
      data_reg <= shiftInAtN(data_reg, tagged Invalid);
   endrule

   interface rx = toPut(fifo_in);
   interface Get tx;
      method ActionValue#(a) get if (!is_empty);
	 fifo_out.deq;
	 return fifo_out.first;
      endmethod
   endinterface
      
   method Bool isEmpty;
      return is_empty;
   endmethod
   
endmodule

module mkCollector#(Integer depth) (Collector#(n, a))
   provisos(Bits#(a, sa), Eq#(a));
   
   FIFOF#(Vector#(n, Maybe#(a))) fifo_in  <- mkLFIFOF;
   Slider#(n, a)                 slider   <- mkSlider;
   FIFOF#(a)                     fifo_out <- mkLFIFOF;
   
   Reg#(Vector#(n, Maybe#(a)))   data_reg <- mkReg(replicate(tagged Invalid));
   
   Bool is_empty = !fifo_in.notEmpty && slider.isEmpty && !fifo_out.notEmpty && data_reg == replicate(tagged Invalid);
   PulseWire   do_shift <- mkPulseWire;
   
   (* preempts = "drop_empty, grab_data" *)   
   (* aggressive_implicit_conditions *)
   rule drop_empty (fifo_in.first == replicate(tagged Invalid));
      fifo_in.deq;
   endrule
   
   (* preempts = "grab_data, default_data" *)   
   (* aggressive_implicit_conditions *)
   rule grab_data (do_shift);
      slider.in(fifo_in.first);
      fifo_in.deq;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule default_data;
      slider.in(replicate(tagged Invalid));
   endrule

   (* aggressive_implicit_conditions *)
   rule shift_one;
      if (data_reg[0] matches tagged Valid .v) fifo_out.enq(v);
      let next = shiftInAtN(data_reg, tagged Invalid);
      if (next == replicate(tagged Invalid)) begin
	 do_shift.send;
	 next = slider.out;
	 slider.shift;
      end
      data_reg <= next;
   endrule

   interface rx = toPut(fifo_in);
   interface Get tx;
      method ActionValue#(a) get if (!is_empty);
	 fifo_out.deq;
	 return fifo_out.first;
      endmethod
   endinterface
      
   method Bool isEmpty;
      return is_empty;
   endmethod
   
endmodule

interface Slider#(numeric type n, type a);
   method Action shift();
   (* always_ready, always_enabled *)
   method Action in (Vector#(n, Maybe#(a)) value);
   (* always_ready *)
   method Vector#(n, Maybe#(a)) out;
   method Bool isEmpty;
endinterface

module mkSlider(Slider#(n, a))
   provisos(Bits#(a, sa), Eq#(a));
   
   PulseWire  do_shift <- mkPulseWire;
   
   Bool is_empty = True;
   Vector#(n, Reg#(Vector#(n, Maybe#(a)))) rs = ?;
   for (Integer i = 0; i < valueOf(n); i = i + 1)
      begin
	 rs[i] <- mkReg(replicate(tagged Invalid));
	 is_empty = is_empty && rs[i] == replicate(tagged Invalid);
	 if (i != 0)
	    begin
	       (* aggressive_implicit_conditions *)
	       rule shift_rl (do_shift);
		  Vector#(n, Bool) x = map(isValid, eliminate(valueOf(n) - i - 1, rs[i - 1]));
//		  if (pack(x) != 0) $display("(%0d) RS %d %16b", $time, i, x);
		  rs[i] <= eliminate(valueOf(n) - i - 1, rs[i - 1]);
	       endrule
	    end
      end
   
   method shift = do_shift.send;
   
   method Action in (Vector#(n, Maybe#(a)) value);
      if (do_shift) rs[0] <= value;
   endmethod
   
   method out = rs[valueOf(n) - 1];
   
   method Bool isEmpty = is_empty;
   
   
endmodule


endpackage
