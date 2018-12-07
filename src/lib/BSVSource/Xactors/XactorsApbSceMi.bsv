// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsApbSceMi;

import Connectable::*;
import TLM3::*;
import SceMi::*;
import Apb::*;
import XactorsApb::*;
import XactorsDefines::*;
import XactorsCommon::*;

`include "TLM.defines"


// SceMi
interface ApbMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface ApbXtorSlave#(`TLM_PRM) bus;
endinterface

interface ApbSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface ApbXtorMaster#(`TLM_PRM) bus;
endinterface

instance Connectable#(ApbMasterSceMiXactor#(`TLM_PRM),  ApbXtorMaster#(`TLM_PRM));
   module mkConnection#(ApbMasterSceMiXactor#(`TLM_PRM) x,  ApbXtorMaster#(`TLM_PRM) b)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance

instance Connectable#(  ApbXtorMaster#(`TLM_PRM), ApbMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( ApbXtorMaster#(`TLM_PRM) b, ApbMasterSceMiXactor#(`TLM_PRM) x)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance


instance Connectable#(ApbSlaveSceMiXactor#(`TLM_PRM),  ApbXtorSlave#(`TLM_PRM));
   module mkConnection#(ApbSlaveSceMiXactor#(`TLM_PRM) x,  ApbXtorSlave#(`TLM_PRM) b)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance

instance Connectable#(  ApbXtorSlave#(`TLM_PRM), ApbSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( ApbXtorSlave#(`TLM_PRM) b, ApbSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
   (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkApbMasterSceMiXactor (TLMXActorArgs args,
                               ApbMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
   ApbMasterStreamXactor#(`TLM_PRM)  _busX <- mkApbMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkApbSlaveSceMiXactor (TLMXActorArgs args,
                               ApbSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   ApbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkApbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule



endpackage
