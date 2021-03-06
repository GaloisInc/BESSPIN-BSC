// Copyright (c) 2008--2012 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Dbg;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Probe::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module dbgProbe (Probe#(a))
   provisos(FShow#(a));

   Probe#(Bit#(640)) _probe <- mkProbe;
  
   method Action _write(a value);
      let ascii <- $swriteAV(fshow(value));
      _probe <= ascii;
   endmethod

endmodule

module dbg#(a value) (Empty)
   provisos(FShow#(a));
      
   Probe#(a) _probe <- dbgProbe;
      
   rule every;
      _probe <= value;
   endrule

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage

