// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XactorsAxi4SceMi;

import Connectable::*;
import TLM3::*;
import SceMi::*;
import Axi4::*;

import XactorsAxi4::*;
import XactorsDefines::*;
import XactorsCommon::*;

`include "TLM.defines"



// SceMi
interface Axi4RdMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdSlave#(`TLM_PRM) bus;
endinterface

interface Axi4RdSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdMaster#(`TLM_PRM) bus;
endinterface

interface Axi4WrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4WrSlave#(`TLM_PRM) bus;
endinterface

interface Axi4WrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4WrMaster#(`TLM_PRM) bus;
endinterface

interface Axi4RdWrMasterSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrSlave#(`TLM_PRM) write;
endinterface

interface Axi4RdWrSlaveSceMiXactor#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrMaster#(`TLM_PRM) write;
endinterface

// 12 Connectable instances required...
instance Connectable#(Axi4RdMasterSceMiXactor#(`TLM_PRM),  Axi4RdMaster#(`TLM_PRM));
   module mkConnection#(Axi4RdMasterSceMiXactor#(`TLM_PRM) x,  Axi4RdMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4RdMaster#(`TLM_PRM), Axi4RdMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4RdMaster#(`TLM_PRM) b, Axi4RdMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4RdSlaveSceMiXactor#(`TLM_PRM),  Axi4RdSlave#(`TLM_PRM));
   module mkConnection#(Axi4RdSlaveSceMiXactor#(`TLM_PRM) x,  Axi4RdSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4RdSlave#(`TLM_PRM), Axi4RdSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4RdSlave#(`TLM_PRM) b, Axi4RdSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance

//
instance Connectable#(Axi4WrMasterSceMiXactor#(`TLM_PRM),  Axi4WrMaster#(`TLM_PRM));
   module mkConnection#(Axi4WrMasterSceMiXactor#(`TLM_PRM) x,  Axi4WrMaster#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4WrMaster#(`TLM_PRM), Axi4WrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4WrMaster#(`TLM_PRM) b, Axi4WrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4WrSlaveSceMiXactor#(`TLM_PRM),  Axi4WrSlave#(`TLM_PRM));
   module mkConnection#(Axi4WrSlaveSceMiXactor#(`TLM_PRM) x,  Axi4WrSlave#(`TLM_PRM) b)(Empty);
      (*hide*)  let _x <- mkConnection(x.bus,b);
   endmodule
endinstance
instance Connectable#(  Axi4WrSlave#(`TLM_PRM), Axi4WrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4WrSlave#(`TLM_PRM) b, Axi4WrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4RdWrMasterSceMiXactor#(`TLM_PRM),  Axi4RdWrMaster#(`TLM_PRM));
   module mkConnection#(Axi4RdWrMasterSceMiXactor#(`TLM_PRM) x,  Axi4RdWrMaster#(`TLM_PRM) s)(Empty);
      let rdConn <- mkConnection(x.read,s.read);
      let wrConn <- mkConnection(x.write,s.write);
   endmodule
endinstance
instance Connectable#(  Axi4RdWrMaster#(`TLM_PRM), Axi4RdWrMasterSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4RdWrMaster#(`TLM_PRM) b, Axi4RdWrMasterSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


instance Connectable#(Axi4RdWrSlaveSceMiXactor#(`TLM_PRM),  Axi4RdWrSlave#(`TLM_PRM));
   module mkConnection#(Axi4RdWrSlaveSceMiXactor#(`TLM_PRM) x,  Axi4RdWrSlave#(`TLM_PRM) m)(Empty);
      let rdConn <- mkConnection(x.read,m.read);
      let wrConn <- mkConnection(x.write,m.write);
   endmodule
endinstance
instance Connectable#(  Axi4RdWrSlave#(`TLM_PRM), Axi4RdWrSlaveSceMiXactor#(`TLM_PRM));
   module mkConnection#( Axi4RdWrSlave#(`TLM_PRM) b, Axi4RdWrSlaveSceMiXactor#(`TLM_PRM) x)(Empty);
      (*hide*)  let _x <- mkConnection(x,b);
   endmodule
endinstance


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4RdMasterSceMiXactor (TLMXActorArgs args,
                               Axi4RdMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4RdMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4RdMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4WrMasterSceMiXactor (TLMXActorArgs args,
                               Axi4WrMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4WrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4WrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4RdWrMasterSceMiXactor (TLMXActorArgs args,
                               Axi4RdWrMasterSceMiXactor#(`TLM_PRM) ignore)
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
   Axi4RdWrMasterStreamXactor#(`TLM_PRM)  _busX <- mkAxi4RdWrMasterStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4RdSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4RdSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4RdSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4RdSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4WrSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4WrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4WrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4WrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface bus = _busX.bus;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkAxi4RdWrSlaveSceMiXactor (TLMXActorArgs args,
                               Axi4RdWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)

      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
   Axi4RdWrSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAxi4RdWrSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, _busX.stream);

   interface read = _busX.read;
   interface write = _busX.write;
endmodule


endpackage
