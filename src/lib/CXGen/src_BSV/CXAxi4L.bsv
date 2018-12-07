package CXAxi4L;

import SceMi::*;
import Clocks::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;
import TLM3::*;
import Axi4::*;
import Axi::*;
import XactorsAxi4LSceMi::*;
import XactorsDefines::*;
import CXCommon::*;

`include "TLM.defines"

// ================

(* always_ready, always_enabled *)
interface CXAxi4LRdMaster#(`TLM_PRM_DCL);
   (* result = "ARADDR" *)  method AxiAddr#(`TLM_PRM) arADDR;
   (* result = "ARPROT" *)  method Bit#(3)            arPROT;
   (* result = "ARVALID" *) method Bit#(1)            arVALID;

   (* prefix = "", result = "unusedrm0" *) method Action arREADY((* port = "ARREADY" *) Bit#(1) value);

   (* result = "RREADY" *)  method Bit#(1)            rREADY;

   (* prefix = "", result = "unusedrm1" *) method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *) method Action rRESP((* port = "RRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedrm3" *) method Action rVALID((* port = "RVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxi4LWrMaster#(`TLM_PRM_DCL);
   (* result = "AWADDR" *)  method AxiAddr#(`TLM_PRM) awADDR;
   (* result = "AWPROT" *)  method Bit#(3)            awPROT;
   (* result = "AWVALID" *) method Bit#(1)            awVALID;

   (* prefix = "", result = "unusedwm0" *) method Action awREADY((* port = "AWREADY" *) Bit#(1) value);

   (* result = "WDATA" *) method AxiData#(`TLM_PRM)   wDATA;
   (* result = "WSTRB" *) method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WVALID" *) method Bit#(1)             wVALID;

   (* prefix = "", result = "unusedwm1" *) method Action wREADY((* port = "WREADY" *) Bit#(1) value);

   (* result = "BREADY" *) method Bit#(1)             bREADY;

   (* prefix = "", result = "unusedwm2" *) method Action bRESP((* port = "BRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedwm3" *) method Action bVALID((* port = "BVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxi4LRdWrMaster#(`TLM_PRM_DCL);
   (* result = "ARADDR" *)  method AxiAddr#(`TLM_PRM) arADDR;
   (* result = "ARPROT" *)  method Bit#(3)            arPROT;
   (* result = "ARVALID" *) method Bit#(1)            arVALID;
   (* prefix = "", result = "unusedrm0" *) method Action arREADY((* port = "ARREADY" *) Bit#(1) value);
   (* result = "RREADY" *)  method Bit#(1)            rREADY;
   (* prefix = "", result = "unusedrm1" *) method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *) method Action rRESP((* port = "RRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedrm3" *) method Action rVALID((* port = "RVALID" *) Bit#(1) value);

   (* result = "AWADDR" *)  method AxiAddr#(`TLM_PRM) awADDR;
   (* result = "AWPROT" *)  method Bit#(3)            awPROT;
   (* result = "AWVALID" *) method Bit#(1)            awVALID;
   (* prefix = "", result = "unusedwm0" *) method Action awREADY((* port = "AWREADY" *) Bit#(1) value);
   (* result = "WDATA" *) method AxiData#(`TLM_PRM)   wDATA;
   (* result = "WSTRB" *) method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WVALID" *) method Bit#(1)             wVALID;
   (* prefix = "", result = "unusedwm1" *) method Action wREADY((* port = "WREADY" *) Bit#(1) value);
   (* result = "BREADY" *) method Bit#(1)             bREADY;
   (* prefix = "", result = "unusedwm2" *) method Action bRESP((* port = "BRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedwm3" *) method Action bVALID((* port = "BVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxi4LWrSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedws0" *) method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *) method Action awPROT((* port = "AWPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedws2" *) method Action awVALID((* port = "AWVALID" *) Bit#(1) value);

   (* result = "AWREADY" *) method Bit#(1)               awREADY;

   (* prefix = "", result = "unusedws3" *) method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws4" *) method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws5" *) method Action wVALID((* port = "WVALID" *) Bit#(1) value);

   (* result = "WREADY" *) method Bit#(1)                wREADY;

   (* prefix = "", result = "unusedws6" *) method Action bREADY((* port = "BREADY" *) Bit#(1) value);

   (* result = "BRESP" *)  method Bit#(2)                bRESP;
   (* result = "BVALID" *) method Bit#(1)                bVALID;
endinterface

(* always_ready, always_enabled *)
interface CXAxi4LRdSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedrs0" *) method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *) method Action arPROT((* port = "ARPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs2" *) method Action arVALID((* port = "ARVALID" *) Bit#(1) value);

   (* result = "ARREADY" *)  method Bit#(1)               arREADY;

   (* prefix = "", result = "unusedrs3" *) method Action rREADY((* port = "RREADY" *) Bit#(1) value);

   (* result = "RDATA" *)  method AxiData#(`TLM_PRM)      rDATA;
   (* result = "RRESP" *)  method Bit#(2)                 rRESP;
   (* result = "RVALID" *) method Bit#(1)                 rVALID;
endinterface

(* always_ready, always_enabled *)
interface CXAxi4LRdWrSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedws0" *) method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *) method Action awPROT((* port = "AWPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedws2" *) method Action awVALID((* port = "AWVALID" *) Bit#(1) value);
   (* result = "AWREADY" *) method Bit#(1)               awREADY;
   (* prefix = "", result = "unusedws3" *) method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws4" *) method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws5" *) method Action wVALID((* port = "WVALID" *) Bit#(1) value);
   (* result = "WREADY" *) method Bit#(1)                wREADY;
   (* prefix = "", result = "unusedws6" *) method Action bREADY((* port = "BREADY" *) Bit#(1) value);
   (* result = "BRESP" *)  method Bit#(2)                bRESP;
   (* result = "BVALID" *) method Bit#(1)                bVALID;

   (* prefix = "", result = "unusedrs0" *) method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *) method Action arPROT((* port = "ARPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs2" *) method Action arVALID((* port = "ARVALID" *) Bit#(1) value);
   (* result = "ARREADY" *)  method Bit#(1)               arREADY;
   (* prefix = "", result = "unusedrs3" *) method Action rREADY((* port = "RREADY" *) Bit#(1) value);
   (* result = "RDATA" *)  method AxiData#(`TLM_PRM)      rDATA;
   (* result = "RRESP" *)  method Bit#(2)                 rRESP;
   (* result = "RVALID" *) method Bit#(1)                 rVALID;
endinterface

// ================

typeclass CXify#(type t, type cxt)
   dependencies (t determines cxt);

   module mkCXify#(t ifc_in, Bool xtor_enable)(cxt);
endtypeclass

instance CXify#(Axi4LWrSlave#(`TLM_PRM), CXAxi4LWrSlave#(`TLM_PRM));
   module mkCXify#(Axi4LWrSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LWrSlave#(`TLM_PRM));
      let aw <- mkReceiverRV(xtor_enable, ifc_in.awREADY, ifc_in.awVALID);
      let  w <- mkReceiverRV(xtor_enable, ifc_in.wREADY,  ifc_in.wVALID);
      let  b <- mkSenderRV  (xtor_enable, ifc_in.bREADY,  ifc_in.bVALID);
      let sx = mkSenderX(xtor_enable);

      let m_bRESP   <- sx(ifc_in.bRESP);

      method awADDR  = ifc_in.awADDR;
      method awPROT  = compose(ifc_in.awPROT, unpack);
      method awVALID = compose(aw.valid, unpack);
      method awREADY = pack(aw.ready);

      method wDATA   = ifc_in.wDATA;
      method wSTRB   = ifc_in.wSTRB;
      method wVALID  = compose(w.valid, unpack);
      method wREADY  = pack(w.ready);

      method bRESP   = pack(m_bRESP);
      method bVALID  = pack(b.valid);
      method bREADY  = compose(b.ready, unpack);
   endmodule
endinstance

instance CXify#(Axi4LRdSlave#(`TLM_PRM), CXAxi4LRdSlave#(`TLM_PRM));
   module mkCXify#(Axi4LRdSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdSlave#(`TLM_PRM));
      let ar <- mkReceiverRV(xtor_enable, ifc_in.arREADY, ifc_in.arVALID);
      let  r <- mkSenderRV  (xtor_enable, ifc_in.rREADY,  ifc_in.rVALID);
      let sx = mkSenderX(xtor_enable);

      let m_rDATA   <- sx(ifc_in.rDATA);
      let m_rRESP   <- sx(ifc_in.rRESP);

      method arADDR  = ifc_in.arADDR;
      method arPROT  = compose(ifc_in.arPROT, unpack);
      method arVALID = compose(ar.valid, unpack);
      method arREADY = pack(ar.ready);

      method rDATA   = m_rDATA;
      method rRESP   = pack(m_rRESP);
      method rVALID  = pack(r.valid);
      method rREADY  = compose(r.ready, unpack);
   endmodule
endinstance

instance CXify#(Axi4LRdWrSlave#(`TLM_PRM), CXAxi4LRdWrSlave#(`TLM_PRM));
   module mkCXify#(Axi4LRdWrSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdWrSlave#(`TLM_PRM));
      let rd <- mkCXify(ifc_in.read, xtor_enable);
      let wr <- mkCXify(ifc_in.write, xtor_enable);

      method awADDR = wr.awADDR;
      method awPROT = wr.awPROT;
      method awVALID = wr.awVALID;
      method awREADY = wr.awREADY;
      method wDATA = wr.wDATA;
      method wSTRB = wr.wSTRB;
      method wVALID = wr.wVALID;
      method wREADY = wr.wREADY;
      method bREADY = wr.bREADY;
      method bRESP = wr.bRESP;
      method bVALID = wr.bVALID;

      method arADDR = rd.arADDR;
      method arPROT = rd.arPROT;
      method arVALID = rd.arVALID;
      method arREADY = rd.arREADY;
      method rREADY = rd.rREADY;
      method rDATA = rd.rDATA;
      method rRESP = rd.rRESP;
      method rVALID = rd.rVALID;
   endmodule
endinstance

instance CXify#(Axi4LWrMaster#(`TLM_PRM), CXAxi4LWrMaster#(`TLM_PRM));
   module mkCXify#(Axi4LWrMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LWrMaster#(`TLM_PRM));
      let aw <- mkSenderRV   (xtor_enable, ifc_in.awREADY, ifc_in.awVALID);
      let  w <- mkSenderRV   (xtor_enable, ifc_in.wREADY,  ifc_in.wVALID);
      let  b <- mkReceiverRV (xtor_enable, ifc_in.bREADY,  ifc_in.bVALID);
      let sx = mkSenderX(xtor_enable);

      let m_awADDR  <- sx(ifc_in.awADDR);
      let m_awPROT  <- sx(ifc_in.awPROT);
      let m_wDATA   <- sx(ifc_in.wDATA);
      let m_wSTRB   <- sx(ifc_in.wSTRB);

      method awADDR  = m_awADDR;
      method awPROT  = pack(m_awPROT);
      method awVALID = pack(aw.valid);
      method awREADY = compose(aw.ready, unpack);

      method wDATA   = m_wDATA;
      method wSTRB   = m_wSTRB;
      method wVALID  = pack(w.valid);
      method wREADY  = compose(w.ready, unpack);

      method bRESP   = compose(ifc_in.bRESP, unpack);
      method bVALID  = compose(b.valid, unpack);
      method bREADY  = pack(b.ready);
   endmodule
endinstance

instance CXify#(Axi4LRdMaster#(`TLM_PRM), CXAxi4LRdMaster#(`TLM_PRM));
   module mkCXify#(Axi4LRdMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdMaster#(`TLM_PRM));
      let ar <- mkSenderRV   (xtor_enable, ifc_in.arREADY, ifc_in.arVALID);
      let  r <- mkReceiverRV (xtor_enable, ifc_in.rREADY,  ifc_in.rVALID);
      let sx = mkSenderX(xtor_enable);

      let m_arADDR  <- sx(ifc_in.arADDR);
      let m_arPROT  <- sx(ifc_in.arPROT);

      method arADDR  = m_arADDR;
      method arPROT  = pack(m_arPROT);
      method arVALID = pack(ar.valid);
      method arREADY = compose(ar.ready, unpack);

      method rDATA   = ifc_in.rDATA;
      method rRESP   = compose(ifc_in.rRESP, unpack);
      method rVALID  = compose(r.valid, unpack);
      method rREADY  = pack(r.ready);
   endmodule
endinstance

instance CXify#(Axi4LRdWrMaster#(`TLM_PRM), CXAxi4LRdWrMaster#(`TLM_PRM));
   module mkCXify#(Axi4LRdWrMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdWrMaster#(`TLM_PRM));
      let rd <- mkCXify(ifc_in.read,  xtor_enable);
      let wr <- mkCXify(ifc_in.write, xtor_enable);

      method awADDR = wr.awADDR;
      method awPROT = wr.awPROT;
      method awVALID = wr.awVALID;
      method awREADY = wr.awREADY;

      method wDATA = wr.wDATA;
      method wSTRB = wr.wSTRB;
      method wVALID = wr.wVALID;
      method wREADY = wr.wREADY;

      method bRESP = wr.bRESP;
      method bVALID = wr.bVALID;
      method bREADY = wr.bREADY;

      method arADDR = rd.arADDR;
      method arPROT = rd.arPROT;
      method arVALID = rd.arVALID;
      method arREADY = rd.arREADY;

      method rDATA = rd.rDATA;
      method rRESP = rd.rRESP;
      method rVALID = rd.rVALID;
      method rREADY = rd.rREADY;
   endmodule
endinstance

// ======

// SceMi
typedef CXAxi4LRdSlave#(`TLM_PRM)    CXAxi4LRdMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxi4LRdMaster#(`TLM_PRM)   CXAxi4LRdSlaveSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxi4LWrSlave#(`TLM_PRM)    CXAxi4LWrMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxi4LWrMaster#(`TLM_PRM)   CXAxi4LWrSlaveSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxi4LRdWrSlave#(`TLM_PRM)  CXAxi4LRdWrMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxi4LRdWrMaster#(`TLM_PRM) CXAxi4LRdWrSlaveSceMiXactor#(`TLM_PRM_DCL);

//; ======

instance CXify#(Axi4LRdMasterSceMiXactor#(`TLM_PRM), CXAxi4LRdMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LRdMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdMasterSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(Axi4LRdSlaveSceMiXactor#(`TLM_PRM), CXAxi4LRdSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LRdSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdSlaveSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(Axi4LWrMasterSceMiXactor#(`TLM_PRM), CXAxi4LWrMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LWrMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LWrMasterSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(Axi4LWrSlaveSceMiXactor#(`TLM_PRM), CXAxi4LWrSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LWrSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LWrSlaveSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(Axi4LRdWrMasterSceMiXactor#(`TLM_PRM), CXAxi4LRdWrMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LRdWrMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdWrMasterSceMiXactor#(`TLM_PRM));
      let sl = (interface Axi4LRdWrSlave ;
		   interface read  = ifc_in.read;
		   interface write = ifc_in.write;
		endinterface);
      let masc <- mkCXify(sl, xtor_enable);
      return masc;
   endmodule
endinstance

instance CXify#(Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM), CXAxi4LRdWrSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(Axi4LRdWrSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxi4LRdWrSlaveSceMiXactor#(`TLM_PRM));
      let ma = (interface Axi4LRdWrMaster ;
		   interface read  = ifc_in.read;
		   interface write = ifc_in.write;
		endinterface);
      let slsc <- mkCXify(ma, xtor_enable);
      return slsc;
   endmodule
endinstance

// ==========================

module [SceMiModule] mkCXAxi4LRdMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LRdMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   let _ifc0 <- mkAxi4LRdMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxi4LWrMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   let _ifc0 <- mkAxi4LWrMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxi4LRdWrMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LRdWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );


   let _ifc0 <- mkAxi4LRdWrMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxi4LRdSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LRdSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   let _ifc0 <- mkAxi4LRdSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxi4LWrSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   let _ifc0 <- mkAxi4LWrSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxi4LRdWrSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxi4LRdWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)

      );

   let _ifc0 <- mkAxi4LRdWrSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

endpackage
