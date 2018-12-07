// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAxi4LSceMi;

import Connectable::*;
import TLM3::*;
import SceMi::*;
import Axi4::*;

import XactorsAxi4L::*;
import XactorsDefines::*;
import XactorsCommon::*;

`include "TLM.defines"

// SceMi
interface Axi4LRdMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdSlave#(`TLM_PRM) bus;
endinterface

interface Axi4LRdSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdMaster#(`TLM_PRM) bus;
endinterface

interface Axi4LWrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LWrSlave#(`TLM_PRM) bus;
endinterface

interface Axi4LWrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LWrMaster#(`TLM_PRM) bus;
endinterface

interface Axi4LRdWrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrSlave#(`TLM_PRM) write;
endinterface

interface Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrMaster#(`TLM_PRM) write;
endinterface

// 12 Connectable instances required...
instance Connectable#(Axi4LRdMasterSceMiXactor#(`TLM_PRM),  Axi4LRdMaster#(`TLM_PRM));
   module mkConnection#(Axi4LRdMasterSceMiXactor#(`TLM_PRM) x,  Axi4LRdMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4LRdMaster#(`TLM_PRM), Axi4LRdMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LRdMaster#(`TLM_PRM) b, Axi4LRdMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4LRdSlaveSceMiXactor#(`TLM_PRM),  Axi4LRdSlave#(`TLM_PRM));
   module mkConnection#(Axi4LRdSlaveSceMiXactor#(`TLM_PRM) x,  Axi4LRdSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4LRdSlave#(`TLM_PRM), Axi4LRdSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LRdSlave#(`TLM_PRM) b, Axi4LRdSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance

//
instance Connectable#(Axi4LWrMasterSceMiXactor#(`TLM_PRM),  Axi4LWrMaster#(`TLM_PRM));
   module mkConnection#(Axi4LWrMasterSceMiXactor#(`TLM_PRM) x,  Axi4LWrMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4LWrMaster#(`TLM_PRM), Axi4LWrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LWrMaster#(`TLM_PRM) b, Axi4LWrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4LWrSlaveSceMiXactor#(`TLM_PRM),  Axi4LWrSlave#(`TLM_PRM));
   module mkConnection#(Axi4LWrSlaveSceMiXactor#(`TLM_PRM) x,  Axi4LWrSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4LWrSlave#(`TLM_PRM), Axi4LWrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LWrSlave#(`TLM_PRM) b, Axi4LWrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance

//
instance Connectable#(Axi4LRdWrMasterSceMiXactor#(`TLM_PRM),  Axi4LRdWrMaster#(`TLM_PRM));
   module mkConnection#(Axi4LRdWrMasterSceMiXactor#(`TLM_PRM) x,  Axi4LRdWrMaster#(`TLM_PRM) s)(Empty);
      let rdConn <- mkConnection(x.read,s.read);
      let wrConn <- mkConnection(x.write,s.write);
   endmodule
endinstance
instance Connectable#(  Axi4LRdWrMaster#(`TLM_PRM), Axi4LRdWrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LRdWrMaster#(`TLM_PRM) b, Axi4LRdWrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM),  Axi4LRdWrSlave#(`TLM_PRM));
   module mkConnection#(Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM) x,  Axi4LRdWrSlave#(`TLM_PRM) m)(Empty);
      let rdConn <- mkConnection(x.read,m.read);
      let wrConn <- mkConnection(x.write,m.write);
   endmodule
endinstance
instance Connectable#(  Axi4LRdWrSlave#(`TLM_PRM), Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4LRdWrSlave#(`TLM_PRM) b, Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LRdMasterSceMiXactor (TLMXActorArgs args,
                               Axi4LRdMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4LRdMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LRdMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LWrMasterSceMiXactor (TLMXActorArgs args,
                               Axi4LWrMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4LWrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LWrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LRdWrMasterSceMiXactor (TLMXActorArgs args,
                               Axi4LRdWrMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4LRdWrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LRdWrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LRdSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4LRdSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4LRdSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LRdSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LWrSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4LWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4LWrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LWrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4LRdWrSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)

      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4LRdWrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4LRdWrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


endpackage
