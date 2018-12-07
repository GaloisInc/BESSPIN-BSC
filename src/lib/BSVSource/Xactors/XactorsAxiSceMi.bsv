// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAxiSceMi;

import Connectable::*;
import TLM3::*;
import SceMi::*;
import Axi::*;
import XactorsAxi::*;
import XactorsDefines::*;
import XactorsCommon::*;

`include "TLM.defines"


// SceMi
interface AxiRdMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) bus;
endinterface

interface AxiRdSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) bus;
endinterface

interface AxiWrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) bus;
endinterface

interface AxiWrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) bus;
endinterface

interface AxiRdWrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) write;
endinterface

interface AxiRdWrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) write;
endinterface

// 12 Connectable instances required...
instance Connectable#(AxiRdMasterSceMiXactor#(`TLM_PRM),  AxiRdMaster#(`TLM_PRM));
   module mkConnection#(AxiRdMasterSceMiXactor#(`TLM_PRM) x,  AxiRdMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  AxiRdMaster#(`TLM_PRM), AxiRdMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiRdMaster#(`TLM_PRM) b, AxiRdMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(AxiRdSlaveSceMiXactor#(`TLM_PRM),  AxiRdSlave#(`TLM_PRM));
   module mkConnection#(AxiRdSlaveSceMiXactor#(`TLM_PRM) x,  AxiRdSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  AxiRdSlave#(`TLM_PRM), AxiRdSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiRdSlave#(`TLM_PRM) b, AxiRdSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance

//
instance Connectable#(AxiWrMasterSceMiXactor#(`TLM_PRM),  AxiWrMaster#(`TLM_PRM));
   module mkConnection#(AxiWrMasterSceMiXactor#(`TLM_PRM) x,  AxiWrMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  AxiWrMaster#(`TLM_PRM), AxiWrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiWrMaster#(`TLM_PRM) b, AxiWrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(AxiWrSlaveSceMiXactor#(`TLM_PRM),  AxiWrSlave#(`TLM_PRM));
   module mkConnection#(AxiWrSlaveSceMiXactor#(`TLM_PRM) x,  AxiWrSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  AxiWrSlave#(`TLM_PRM), AxiWrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiWrSlave#(`TLM_PRM) b, AxiWrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance

//
instance Connectable#(AxiRdWrMasterSceMiXactor#(`TLM_PRM),  AxiRdWrMaster#(`TLM_PRM));
   module mkConnection#(AxiRdWrMasterSceMiXactor#(`TLM_PRM) x,  AxiRdWrMaster#(`TLM_PRM) s)(Empty);
      let rdConn <- mkConnection(x.read,s.read);
      let wrConn <- mkConnection(x.write,s.write);
   endmodule
endinstance
instance Connectable#(  AxiRdWrMaster#(`TLM_PRM), AxiRdWrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiRdWrMaster#(`TLM_PRM) b, AxiRdWrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(AxiRdWrSlaveSceMiXactor#(`TLM_PRM),  AxiRdWrSlave#(`TLM_PRM));
   module mkConnection#(AxiRdWrSlaveSceMiXactor#(`TLM_PRM) x,  AxiRdWrSlave#(`TLM_PRM) m)(Empty);
      let rdConn <- mkConnection(x.read,m.read);
      let wrConn <- mkConnection(x.write,m.write);
   endmodule
endinstance
instance Connectable#(  AxiRdWrSlave#(`TLM_PRM), AxiRdWrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( AxiRdWrSlave#(`TLM_PRM) b, AxiRdWrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance



///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiRdMasterSceMiXactor (TLMXActorArgs args,
                               AxiRdMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
   AxiRdMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxiRdMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiWrMasterSceMiXactor (TLMXActorArgs args,
                               AxiWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
   AxiWrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxiWrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiRdWrMasterSceMiXactor (TLMXActorArgs args,
                               AxiRdWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
   AxiRdWrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxiRdWrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiRdSlaveSceMiXactor (TLMXActorArgs args,
                               AxiRdSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   AxiRdSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxiRdSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiWrSlaveSceMiXactor (TLMXActorArgs args,
                               AxiWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   AxiWrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxiWrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxiRdWrSlaveSceMiXactor (TLMXActorArgs args,
                               AxiRdWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)

      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   AxiRdWrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxiRdWrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


endpackage
