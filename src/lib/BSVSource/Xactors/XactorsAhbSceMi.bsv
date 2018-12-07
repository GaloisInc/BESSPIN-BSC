// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAhbSceMi;

import Connectable::*;
import TLM3::*;
import SceMi::*;
import Ahb::*;
import XactorsAhb::*;
import XactorsDefines::*;
import XactorsCommon::*;

`include "TLM.defines"

// SceMi
interface AhbMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AhbXtorSlave#(`TLM_PRM) bus;
endinterface

interface AhbSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AhbXtorMaster#(`TLM_PRM) bus;
endinterface

instance Connectable#(AhbMasterSceMiXactor#(`TLM_PRM),  AhbXtorMaster#(`TLM_PRM));
   module mkConnection#(AhbMasterSceMiXactor#(`TLM_PRM) x,  AhbXtorMaster#(`TLM_PRM) b)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance

instance Connectable#(  AhbXtorMaster#(`TLM_PRM), AhbMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( AhbXtorMaster#(`TLM_PRM) b, AhbMasterSceMiXactor#(`TLM_PRM) x)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance


instance Connectable#(AhbSlaveSceMiXactor#(`TLM_PRM),  AhbXtorSlave#(`TLM_PRM));
   module mkConnection#(AhbSlaveSceMiXactor#(`TLM_PRM) x,  AhbXtorSlave#(`TLM_PRM) b)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance

instance Connectable#(  AhbXtorSlave#(`TLM_PRM), AhbSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( AhbXtorSlave#(`TLM_PRM) b, AhbSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAhbMasterSceMiXactor (TLMXActorArgs args,
                               AhbMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
   AhbMasterStreamXactor#(`TLM_PRM)  _busX <- mkAhbMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAhbSlaveSceMiXactor (TLMXActorArgs args,
                               AhbSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   AhbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAhbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule



endpackage
