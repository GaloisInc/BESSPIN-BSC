// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package DiniRS232;

import DummyDriver::*;
import TriState::*;

export RS232_Pins(..);

// This is the set of signals which routes to the FPGA boundary
// and out to the RS232 connector.
interface RS232_Pins;
   (* always_enabled, prefix="" *)
   method Action rx((* port="RX" *) Bit#(1) x);
   (* prefix="TX" *)
   interface Inout#(Bit#(1)) tx;
endinterface: RS232_Pins

instance DummyDriver#(RS232_Pins);
   module mkStub(RS232_Pins ifc);
      Wire#(Bool) dummyW <- mkWire;
      // never output
      TriState#(Bit#(1)) t <- mkTriState(False,0);

      method Action rx(Bit#(1) x);
	 dummyW <= True;
	 // ignore input
      endmethod
      interface tx = t.io;
   endmodule
endinstance

endpackage: DiniRS232
