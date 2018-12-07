// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

import TLM3::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;

`include "TLM.defines"

////////////////////////////////////
module mkFlowBuffer#(parameter UInt#(32) flow_depth
                     ) (TLMTransformIFC#(req_t, resp_t))
   provisos (TLMRequestTC#(req_t, `TLM_PRM),
	     TLMResponseTC#(resp_t, `TLM_PRM),
             FlowPayload#(req_t),
	     Bits#(req_t, s0),
	     Bits#(resp_t, s1));

   TLMFlow#(req_t)           flow <- mkTLMFlow(flow_depth);
   FIFOF#(resp_t)            fifo <- mkLFIFOF;

   interface TLMRecvIFC in;
      interface Get tx = toGet(fifo);
      interface Put rx = flow.rx;
   endinterface

   interface TLMSendIFC out;
      interface Get tx = flow.tx;
      interface Put rx = toPut(fifo);
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
module addFlowControl#(parameter UInt#(32) flow_depth,
		       TLMRecvIFC#(req_t, resp_t) ifc
                       ) (TLMRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
            FlowPayload#(req_t),
	    Bits#(req_t, s0));

   TLMFlow#(req_t) flow <- mkTLMFlow(flow_depth);

   mkConnection(flow.tx, ifc.rx);

   interface tx = ifc.tx;
   interface rx = flow.rx;

endmodule
