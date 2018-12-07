// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision: 17899 $
// $Date: 2009-09-21 09:39:55 -0400 (Mon, 21 Sep 2009) $

package AxiRam;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import AxiDefines::*;
import AxiSlave::*;
import Connectable::*;
import FShow::*;
import TLM::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRam#(parameter Bit#(4) id, function Bool addr_match(AxiAddr#(`TLM_TYPES) addr))
		    (AxiSlave#(`TLM_TYPES))
   provisos(Bits#(TLMRequest#(`TLM_TYPES), s0),
	    Bits#(TLMResponse#(`TLM_TYPES), s1),
	    Bits#(RequestDescriptor#(`TLM_TYPES), s2), 
	    FShow#(TLMRequest#(`TLM_TYPES)),
	    FShow#(TLMResponse#(`TLM_TYPES)));
   
   AxiWrSlaveXActorIFC#(`TLM_TYPES) xactor_sw <- mkAxiWrSlave(addr_match);
   AxiRdSlaveXActorIFC#(`TLM_TYPES) xactor_sr <- mkAxiRdSlave(addr_match);

   TLMReadWriteRecvIFC#(`TLM_TYPES) mem <- mkTLMReadWriteRam(id);
		    
   mkConnection(xactor_sw.tlm, mem.write);
   mkConnection(xactor_sr.tlm, mem.read);
		    
   interface AxiRdSlave read  = xactor_sr.bus;
   interface AxiWrSlave write = xactor_sw.bus;
   
endmodule

endpackage
