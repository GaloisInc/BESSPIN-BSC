package Shuffle;

import Break::*;
import GetPut::*;
import FIFO::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass HasLast#(type a);
   function Bool islast(a value);
   function a setlast(a value, Bool last);
endtypeclass

instance HasLast#(BitConstraints#(n));
   function Bool islast(BitConstraints#(n) value);
      return value.isLast;
   endfunction
   function BitConstraints#(n) setlast(BitConstraints#(n) value, Bool last);
      value.isLast = last;
      return value;
   endfunction
endinstance

interface Shuffle#(type a);
   interface Put#(a) rx_0;
   interface Put#(a) rx_1;
   interface Get#(a) tx;
endinterface

module mkShuffleSwitch#(parameter Bool zero_first) (Shuffle#(a))
   provisos(Bits#(a, sa),  HasLast#(a));
   
   Reg#(Bool) draining_zero <- mkReg(zero_first);
   FIFO#(a)   fifo          <- mkLFIFO;
   
   interface Put rx_0;
      method Action put (a value) if (draining_zero);
	 let v = value;
	 if (islast(v)) begin
	    draining_zero <= False;
	    if (zero_first)
	       v = setlast(v, False);
	 end
	 fifo.enq(v);
      endmethod
   endinterface
   
   interface Put rx_1;
      method Action put (a value) if (!draining_zero);
   	 let v = value;
	 if (islast(v)) begin
	    draining_zero <= True;
	    if (!zero_first)
	       v = setlast(v, False);
	 end
	 fifo.enq(v);
      endmethod
   endinterface
   
   interface tx = toGet(fifo);
   
endmodule


endpackage