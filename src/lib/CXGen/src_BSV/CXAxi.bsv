package CXAxi;

import SceMi::*;
import Clocks::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;
import TLM3::*;
import Axi::*;
import XactorsAxiSceMi::*;
import XactorsDefines::*;
import CXCommon::*;

`include "TLM.defines"

// ================

(* always_ready, always_enabled *)
interface CXAxiRdMaster#(`TLM_PRM_DCL);
   (* result = "ARID" *)    method AxiId#(`TLM_PRM)   arID;
   (* result = "ARADDR" *)  method AxiAddr#(`TLM_PRM) arADDR;
   (* result = "ARLEN" *)   method Bit#(4)            arLEN;
   (* result = "ARSIZE" *)  method Bit#(3)            arSIZE;
   (* result = "ARBURST" *) method Bit#(2)            arBURST;
   (* result = "ARLOCK" *)  method Bit#(2)            arLOCK;
   (* result = "ARCACHE" *) method Bit#(4)            arCACHE;
   (* result = "ARPROT" *)  method Bit#(3)            arPROT;
   (* result = "ARVALID" *) method Bit#(1)            arVALID;

   (* prefix = "", result = "unusedrm0" *)  method Action arREADY((* port = "ARREADY" *) Bit#(1) value);

   (* result = "RREADY" *)  method Bit#(1)            rREADY;

   (* prefix = "", result = "unusedrm1" *)  method Action rID((* port = "RID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *)  method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm3" *)  method Action rRESP((* port = "RRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedrm4" *)  method Action rLAST((* port = "RLAST" *) Bit#(1) value);
   (* prefix = "", result = "unusedrm5" *)  method Action rVALID((* port = "RVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxiWrMaster#(`TLM_PRM_DCL);
   (* result = "AWID" *)  method AxiId#(`TLM_PRM)     awID;
   (* result = "AWADDR" *)  method AxiAddr#(`TLM_PRM) awADDR;
   (* result = "AWLEN" *)   method Bit#(4)            awLEN;
   (* result = "AWSIZE" *)  method Bit#(3)            awSIZE;
   (* result = "AWBURST" *) method Bit#(2)            awBURST;
   (* result = "AWLOCK" *)  method Bit#(2)            awLOCK;
   (* result = "AWCACHE" *) method Bit#(4)            awCACHE;
   (* result = "AWPROT" *)  method Bit#(3)            awPROT;
   (* result = "AWVALID" *) method Bit#(1)            awVALID;

   (* prefix = "", result = "unusedwm0" *)  method Action awREADY((* port = "AWREADY" *) Bit#(1) value);

   (* result = "WID" *)   method AxiId#(`TLM_PRM)     wID;
   (* result = "WDATA" *) method AxiData#(`TLM_PRM)   wDATA;
   (* result = "WSTRB" *) method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WLAST" *) method Bit#(1)              wLAST;
   (* result = "WVALID" *) method Bit#(1)             wVALID;

   (* prefix = "", result = "unusedwm1" *) method Action wREADY((* port = "WREADY" *) Bit#(1) value);

   (* result = "BREADY" *) method Bit#(1)             bREADY;

   (* prefix = "", result = "unusedwm2" *) method Action bID((* port = "BID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedwm3" *) method Action bRESP((* port = "BRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedwm4" *) method Action bVALID((* port = "BVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxiRdWrMaster#(`TLM_PRM_DCL);
   (* result = "ARID" *)    method AxiId#(`TLM_PRM)   arID;
   (* result = "ARADDR" *)  method AxiAddr#(`TLM_PRM) arADDR;
   (* result = "ARLEN" *)   method Bit#(4)            arLEN;
   (* result = "ARSIZE" *)  method Bit#(3)            arSIZE;
   (* result = "ARBURST" *) method Bit#(2)            arBURST;
   (* result = "ARLOCK" *)  method Bit#(2)            arLOCK;
   (* result = "ARCACHE" *) method Bit#(4)            arCACHE;
   (* result = "ARPROT" *)  method Bit#(3)            arPROT;
   (* result = "ARVALID" *) method Bit#(1)            arVALID;
   (* prefix = "", result = "unusedrm0" *)  method Action arREADY((* port = "ARREADY" *) Bit#(1) value);
   (* result = "RREADY" *)  method Bit#(1)            rREADY;
   (* prefix = "", result = "unusedrm1" *)  method Action rID((* port = "RID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *)  method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm3" *)  method Action rRESP((* port = "RRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedrm4" *)  method Action rLAST((* port = "RLAST" *) Bit#(1) value);
   (* prefix = "", result = "unusedrm5" *)  method Action rVALID((* port = "RVALID" *) Bit#(1) value);

   (* result = "AWID" *)  method AxiId#(`TLM_PRM)     awID;
   (* result = "AWADDR" *)  method AxiAddr#(`TLM_PRM) awADDR;
   (* result = "AWLEN" *)   method Bit#(4)            awLEN;
   (* result = "AWSIZE" *)  method Bit#(3)            awSIZE;
   (* result = "AWBURST" *) method Bit#(2)            awBURST;
   (* result = "AWLOCK" *)  method Bit#(2)            awLOCK;
   (* result = "AWCACHE" *) method Bit#(4)            awCACHE;
   (* result = "AWPROT" *)  method Bit#(3)            awPROT;
   (* result = "AWVALID" *) method Bit#(1)            awVALID;
   (* prefix = "", result = "unusedwm0" *)  method Action awREADY((* port = "AWREADY" *) Bit#(1) value);
   (* result = "WID" *)   method AxiId#(`TLM_PRM)     wID;
   (* result = "WDATA" *) method AxiData#(`TLM_PRM)   wDATA;
   (* result = "WSTRB" *) method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WLAST" *) method Bit#(1)              wLAST;
   (* result = "WVALID" *) method Bit#(1)             wVALID;
   (* prefix = "", result = "unusedwm1" *) method Action wREADY((* port = "WREADY" *) Bit#(1) value);
   (* result = "BREADY" *) method Bit#(1)             bREADY;

   (* prefix = "", result = "unusedwm2" *) method Action bID((* port = "BID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedwm3" *) method Action bRESP((* port = "BRESP" *) Bit#(2) value);
   (* prefix = "", result = "unusedwm4" *) method Action bVALID((* port = "BVALID" *) Bit#(1) value);
endinterface

(* always_ready, always_enabled *)
interface CXAxiWrSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedws0" *) method Action awID((* port = "AWID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *) method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws2" *) method Action awLEN((* port = "AWLEN" *) Bit#(4) value);
   (* prefix = "", result = "unusedws3" *) method Action awSIZE((* port = "AWSIZE" *) Bit#(3) value);
   (* prefix = "", result = "unusedws4" *) method Action awBURST((* port = "AWBURST" *) Bit#(2)  value);
   (* prefix = "", result = "unusedws5" *) method Action awLOCK((* port = "AWLOCK" *) Bit#(2) value);
   (* prefix = "", result = "unusedws6" *) method Action awCACHE((* port = "AWCACHE" *) Bit#(4)  value);
   (* prefix = "", result = "unusedws7" *) method Action awPROT((* port = "AWPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedws8" *) method Action awVALID((* port = "AWVALID" *) Bit#(1) value);

   (* result = "AWREADY" *) method Bit#(1)               awREADY;

   (* prefix = "", result = "unusedws9" *)  method Action wID((* port = "WID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws10" *) method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws11" *) method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws12" *) method Action wLAST((* port = "WLAST" *) Bit#(1) value);
   (* prefix = "", result = "unusedws13" *) method Action wVALID((* port = "WVALID" *) Bit#(1) value);

   (* result = "WREADY" *) method Bit#(1)                wREADY;

   (* prefix = "", result = "unusedws14" *) method Action bREADY((* port = "BREADY" *) Bit#(1) value);

   (* result = "BID" *)    method AxiId#(`TLM_PRM)       bID;
   (* result = "BRESP" *)  method Bit#(2)                bRESP;
   (* result = "BVALID" *) method Bit#(1)                bVALID;
endinterface

(* always_ready, always_enabled *)
interface CXAxiRdSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedrs0" *)  method Action arID((* port = "ARID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *)  method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs2" *)  method Action arLEN((* port = "ARLEN" *) Bit#(4) value);
   (* prefix = "", result = "unusedrs3" *)  method Action arSIZE((* port = "ARSIZE" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs4" *)  method Action arBURST((* port = "ARBURST" *) Bit#(2)  value);
   (* prefix = "", result = "unusedrs5" *)  method Action arLOCK((* port = "ARLOCK" *) Bit#(2) value);
   (* prefix = "", result = "unusedrs6" *)  method Action arCACHE((* port = "ARCACHE" *) Bit#(4)  value);
   (* prefix = "", result = "unusedrs7" *)  method Action arPROT((* port = "ARPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs8" *)  method Action arVALID((* port = "ARVALID" *) Bit#(1) value);

   (* result = "ARREADY" *)  method Bit#(1)               arREADY;

   (* prefix = "", result = "unusedrs9" *)  method Action rREADY((* port = "RREADY" *) Bit#(1) value);

   (* result = "RID" *)    method AxiId#(`TLM_PRM)        rID;
   (* result = "RDATA" *)  method AxiData#(`TLM_PRM)      rDATA;
   (* result = "RRESP" *)  method Bit#(2)                 rRESP;
   (* result = "RLAST" *)  method Bit#(1)                 rLAST;
   (* result = "RVALID" *) method Bit#(1)                 rVALID;
endinterface

(* always_ready, always_enabled *)
interface CXAxiRdWrSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unusedws0" *) method Action awID((* port = "AWID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *) method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws2" *) method Action awLEN((* port = "AWLEN" *) Bit#(4) value);
   (* prefix = "", result = "unusedws3" *) method Action awSIZE((* port = "AWSIZE" *) Bit#(3) value);
   (* prefix = "", result = "unusedws4" *) method Action awBURST((* port = "AWBURST" *) Bit#(2)  value);
   (* prefix = "", result = "unusedws5" *) method Action awLOCK((* port = "AWLOCK" *) Bit#(2) value);
   (* prefix = "", result = "unusedws6" *) method Action awCACHE((* port = "AWCACHE" *) Bit#(4)  value);
   (* prefix = "", result = "unusedws7" *) method Action awPROT((* port = "AWPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedws8" *) method Action awVALID((* port = "AWVALID" *) Bit#(1) value);
   (* result = "AWREADY" *) method Bit#(1)               awREADY;
   (* prefix = "", result = "unusedws9" *)  method Action wID((* port = "WID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws10" *) method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws11" *) method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws12" *) method Action wLAST((* port = "WLAST" *) Bit#(1) value);
   (* prefix = "", result = "unusedws13" *) method Action wVALID((* port = "WVALID" *) Bit#(1) value);
   (* result = "WREADY" *) method Bit#(1)                wREADY;
   (* prefix = "", result = "unusedws14" *) method Action bREADY((* port = "BREADY" *) Bit#(1) value);
   (* result = "BID" *)    method AxiId#(`TLM_PRM)       bID;
   (* result = "BRESP" *)  method Bit#(2)                bRESP;
   (* result = "BVALID" *) method Bit#(1)                bVALID;

   (* prefix = "", result = "unusedrs0" *)  method Action arID((* port = "ARID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *)  method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs2" *)  method Action arLEN((* port = "ARLEN" *) Bit#(4) value);
   (* prefix = "", result = "unusedrs3" *)  method Action arSIZE((* port = "ARSIZE" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs4" *)  method Action arBURST((* port = "ARBURST" *) Bit#(2)  value);
   (* prefix = "", result = "unusedrs5" *)  method Action arLOCK((* port = "ARLOCK" *) Bit#(2) value);
   (* prefix = "", result = "unusedrs6" *)  method Action arCACHE((* port = "ARCACHE" *) Bit#(4)  value);
   (* prefix = "", result = "unusedrs7" *)  method Action arPROT((* port = "ARPROT" *) Bit#(3) value);
   (* prefix = "", result = "unusedrs8" *)  method Action arVALID((* port = "ARVALID" *) Bit#(1) value);
   (* result = "ARREADY" *)  method Bit#(1)               arREADY;
   (* prefix = "", result = "unusedrs9" *)  method Action rREADY((* port = "RREADY" *) Bit#(1) value);
   (* result = "RID" *)    method AxiId#(`TLM_PRM)        rID;
   (* result = "RDATA" *)  method AxiData#(`TLM_PRM)      rDATA;
   (* result = "RRESP" *)  method Bit#(2)                 rRESP;
   (* result = "RLAST" *)  method Bit#(1)                 rLAST;
   (* result = "RVALID" *) method Bit#(1)                 rVALID;
endinterface

// ================

typeclass CXify#(type t, type cxt)
   dependencies (t determines cxt);

   module mkCXify#(t ifc_in, Bool xtor_enable)(cxt);
endtypeclass

instance CXify#(AxiWrSlave#(`TLM_PRM), CXAxiWrSlave#(`TLM_PRM));
   module mkCXify#(AxiWrSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiWrSlave#(`TLM_PRM));
      let aw <- mkReceiverRV(xtor_enable, ifc_in.awREADY, ifc_in.awVALID);
      let  w <- mkReceiverRV(xtor_enable, ifc_in.wREADY,  ifc_in.wVALID);
      let  b <- mkSenderRV  (xtor_enable, ifc_in.bREADY,  ifc_in.bVALID);
      let sx = mkSenderX(xtor_enable);

      let m_bID     <- sx(ifc_in.bID);
      let m_bRESP   <- sx(ifc_in.bRESP);

      method awID    = ifc_in.awID;
      method awADDR  = ifc_in.awADDR;
      method awLEN   = compose(ifc_in.awLEN, unpack);
      method awSIZE  = compose(ifc_in.awSIZE, unpack);
      method awBURST = compose(ifc_in.awBURST, unpack);
      method awLOCK  = compose(ifc_in.awLOCK, unpack);
      method awCACHE = compose(ifc_in.awCACHE, unpack);
      method awPROT  = compose(ifc_in.awPROT, unpack);
      method awVALID = compose(aw.valid, unpack);
      method awREADY = pack(aw.ready);

      method wID     = ifc_in.wID;
      method wDATA   = ifc_in.wDATA;
      method wSTRB   = ifc_in.wSTRB;
      method wLAST   = compose(ifc_in.wLAST, unpack);
      method wVALID  = compose(w.valid, unpack);
      method wREADY  = pack(w.ready);

      method bID     = m_bID;
      method bRESP   = pack(m_bRESP);
      method bVALID  = pack(b.valid);
      method bREADY  = compose(b.ready, unpack);
   endmodule
endinstance

instance CXify#(AxiRdSlave#(`TLM_PRM), CXAxiRdSlave#(`TLM_PRM));
   module mkCXify#(AxiRdSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdSlave#(`TLM_PRM));
      let ar <- mkReceiverRV(xtor_enable, ifc_in.arREADY, ifc_in.arVALID);
      let  r <- mkSenderRV  (xtor_enable, ifc_in.rREADY,  ifc_in.rVALID);
      let sx = mkSenderX(xtor_enable);

      let m_rID     <- sx(ifc_in.rID);
      let m_rDATA   <- sx(ifc_in.rDATA);
      let m_rRESP   <- sx(ifc_in.rRESP);
      let m_rLAST   <- sx(ifc_in.rLAST);

      method arID    = ifc_in.arID;
      method arADDR  = ifc_in.arADDR;
      method arLEN   = compose(ifc_in.arLEN, unpack);
      method arSIZE  = compose(ifc_in.arSIZE, unpack);
      method arBURST = compose(ifc_in.arBURST, unpack);
      method arLOCK  = compose(ifc_in.arLOCK, unpack);
      method arCACHE = compose(ifc_in.arCACHE, unpack);
      method arPROT  = compose(ifc_in.arPROT, unpack);
      method arVALID = compose(ar.valid, unpack);
      method arREADY = pack(ar.ready);

      method rID     = m_rID;
      method rDATA   = m_rDATA;
      method rRESP   = pack(m_rRESP);
      method rLAST   = pack(m_rLAST);
      method rVALID  = pack(r.valid);
      method rREADY  = compose(r.ready, unpack);
   endmodule
endinstance

instance CXify#(AxiRdWrSlave#(`TLM_PRM), CXAxiRdWrSlave#(`TLM_PRM));
   module mkCXify#(AxiRdWrSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdWrSlave#(`TLM_PRM));
      let rd <- mkCXify(ifc_in.read, xtor_enable);
      let wr <- mkCXify(ifc_in.write, xtor_enable);

      method awID = wr.awID;
      method awADDR = wr.awADDR;
      method awLEN = wr.awLEN;
      method awSIZE = wr.awSIZE;
      method awBURST = wr.awBURST;
      method awLOCK = wr.awLOCK;
      method awCACHE = wr.awCACHE;
      method awPROT = wr.awPROT;
      method awVALID = wr.awVALID;
      method awREADY = wr.awREADY;
      method wID = wr.wID;
      method wDATA = wr.wDATA;
      method wSTRB = wr.wSTRB;
      method wLAST = wr.wLAST;
      method wVALID = wr.wVALID;
      method wREADY = wr.wREADY;
      method bREADY = wr.bREADY;
      method bID = wr.bID;
      method bRESP = wr.bRESP;
      method bVALID = wr.bVALID;

      method arID = rd.arID;
      method arADDR = rd.arADDR;
      method arLEN = rd.arLEN;
      method arSIZE = rd.arSIZE;
      method arBURST = rd.arBURST;
      method arLOCK = rd.arLOCK;
      method arCACHE = rd.arCACHE;
      method arPROT = rd.arPROT;
      method arVALID = rd.arVALID;
      method arREADY = rd.arREADY;
      method rREADY = rd.rREADY;
      method rID = rd.rID;
      method rDATA = rd.rDATA;
      method rRESP = rd.rRESP;
      method rLAST = rd.rLAST;
      method rVALID = rd.rVALID;
   endmodule
endinstance

instance CXify#(AxiWrMaster#(`TLM_PRM), CXAxiWrMaster#(`TLM_PRM));
   module mkCXify#(AxiWrMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiWrMaster#(`TLM_PRM));
      let aw <- mkSenderRV   (xtor_enable, ifc_in.awREADY, ifc_in.awVALID);
      let  w <- mkSenderRV   (xtor_enable, ifc_in.wREADY,  ifc_in.wVALID);
      let  b <- mkReceiverRV (xtor_enable, ifc_in.bREADY,  ifc_in.bVALID);
      let sx = mkSenderX(xtor_enable);

      let m_awID    <- sx(ifc_in.awID);
      let m_awADDR  <- sx(ifc_in.awADDR);
      let m_awLEN   <- sx(ifc_in.awLEN);
      let m_awSIZE  <- sx(ifc_in.awSIZE);
      let m_awBURST <- sx(ifc_in.awBURST);
      let m_awLOCK  <- sx(ifc_in.awLOCK);
      let m_awCACHE <- sx(ifc_in.awCACHE);
      let m_awPROT  <- sx(ifc_in.awPROT);
      let m_wID     <- sx(ifc_in.wID);
      let m_wDATA   <- sx(ifc_in.wDATA);
      let m_wSTRB   <- sx(ifc_in.wSTRB);
      let m_wLAST   <- sx(ifc_in.wLAST);

      method awID    = m_awID;
      method awADDR  = m_awADDR;
      method awLEN   = pack(m_awLEN);
      method awSIZE  = pack(m_awSIZE);
      method awBURST = pack(m_awBURST);
      method awLOCK  = pack(m_awLOCK);
      method awCACHE = pack(m_awCACHE);
      method awPROT  = pack(m_awPROT);
      method awVALID = pack(aw.valid);
      method awREADY = compose(aw.ready, unpack);

      method wID     = m_wID;
      method wDATA   = m_wDATA;
      method wSTRB   = m_wSTRB;
      method wLAST   = pack(m_wLAST);
      method wVALID  = pack(w.valid);
      method wREADY  = compose(w.ready, unpack);

      method bID     = ifc_in.bID;
      method bRESP   = compose(ifc_in.bRESP, unpack);
      method bVALID  = compose(b.valid, unpack);
      method bREADY  = pack(b.ready);
   endmodule
endinstance

instance CXify#(AxiRdMaster#(`TLM_PRM), CXAxiRdMaster#(`TLM_PRM));
   module mkCXify#(AxiRdMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdMaster#(`TLM_PRM));
      let ar <- mkSenderRV   (xtor_enable, ifc_in.arREADY, ifc_in.arVALID);
      let  r <- mkReceiverRV (xtor_enable, ifc_in.rREADY,  ifc_in.rVALID);
      let sx = mkSenderX(xtor_enable);

      let m_arID    <- sx(ifc_in.arID);
      let m_arADDR  <- sx(ifc_in.arADDR);
      let m_arLEN   <- sx(ifc_in.arLEN);
      let m_arSIZE  <- sx(ifc_in.arSIZE);
      let m_arBURST <- sx(ifc_in.arBURST);
      let m_arLOCK  <- sx(ifc_in.arLOCK);
      let m_arCACHE <- sx(ifc_in.arCACHE);
      let m_arPROT  <- sx(ifc_in.arPROT);

      method arID    = m_arID;
      method arADDR  = m_arADDR;
      method arLEN   = pack(m_arLEN);
      method arSIZE  = pack(m_arSIZE);
      method arBURST = pack(m_arBURST);
      method arLOCK  = pack(m_arLOCK);
      method arCACHE = pack(m_arCACHE);
      method arPROT  = pack(m_arPROT);
      method arVALID = pack(ar.valid);
      method arREADY = compose(ar.ready, unpack);

      method rID     = ifc_in.rID;
      method rDATA   = ifc_in.rDATA;
      method rRESP   = compose(ifc_in.rRESP, unpack);
      method rLAST   = compose(ifc_in.rLAST, unpack);
      method rVALID  = compose(r.valid, unpack);
      method rREADY  = pack(r.ready);
   endmodule
endinstance

instance CXify#(AxiRdWrMaster#(`TLM_PRM), CXAxiRdWrMaster#(`TLM_PRM));
   module mkCXify#(AxiRdWrMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdWrMaster#(`TLM_PRM));
      let rd <- mkCXify(ifc_in.read,  xtor_enable);
      let wr <- mkCXify(ifc_in.write, xtor_enable);

      method awID = wr.awID;
      method awADDR = wr.awADDR;
      method awLEN = wr.awLEN;
      method awSIZE = wr.awSIZE;
      method awBURST = wr.awBURST;
      method awLOCK = wr.awLOCK;
      method awCACHE = wr.awCACHE;
      method awPROT = wr.awPROT;
      method awVALID = wr.awVALID;
      method awREADY = wr.awREADY;

      method wID = wr.wID;
      method wDATA = wr.wDATA;
      method wSTRB = wr.wSTRB;
      method wLAST = wr.wLAST;
      method wVALID = wr.wVALID;
      method wREADY = wr.wREADY;

      method bID = wr.bID;
      method bRESP = wr.bRESP;
      method bVALID = wr.bVALID;
      method bREADY = wr.bREADY;

      method arID = rd.arID;
      method arADDR = rd.arADDR;
      method arLEN = rd.arLEN;
      method arSIZE = rd.arSIZE;
      method arBURST = rd.arBURST;
      method arLOCK = rd.arLOCK;
      method arCACHE = rd.arCACHE;
      method arPROT = rd.arPROT;
      method arVALID = rd.arVALID;
      method arREADY = rd.arREADY;

      method rID = rd.rID;
      method rDATA = rd.rDATA;
      method rRESP = rd.rRESP;
      method rLAST = rd.rLAST;
      method rVALID = rd.rVALID;
      method rREADY = rd.rREADY;
   endmodule
endinstance

// ======

// SceMi
typedef CXAxiRdSlave#(`TLM_PRM)    CXAxiRdMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxiRdMaster#(`TLM_PRM)   CXAxiRdSlaveSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxiWrSlave#(`TLM_PRM)    CXAxiWrMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxiWrMaster#(`TLM_PRM)   CXAxiWrSlaveSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxiRdWrSlave#(`TLM_PRM)  CXAxiRdWrMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAxiRdWrMaster#(`TLM_PRM) CXAxiRdWrSlaveSceMiXactor#(`TLM_PRM_DCL);

//; ======

instance CXify#(AxiRdMasterSceMiXactor#(`TLM_PRM), CXAxiRdMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiRdMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdMasterSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(AxiRdSlaveSceMiXactor#(`TLM_PRM), CXAxiRdSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiRdSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdSlaveSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(AxiWrMasterSceMiXactor#(`TLM_PRM), CXAxiWrMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiWrMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiWrMasterSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(AxiWrSlaveSceMiXactor#(`TLM_PRM), CXAxiWrSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiWrSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiWrSlaveSceMiXactor#(`TLM_PRM));
      let bs <- mkCXify(ifc_in.bus, xtor_enable);
      return bs;
   endmodule
endinstance

instance CXify#(AxiRdWrMasterSceMiXactor#(`TLM_PRM), CXAxiRdWrMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiRdWrMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdWrMasterSceMiXactor#(`TLM_PRM));
      let sl = (interface AxiRdWrSlave ;
		   interface read  = ifc_in.read;
		   interface write = ifc_in.write;
		endinterface);
      let masc <- mkCXify(sl, xtor_enable);
      return masc;
   endmodule
endinstance

instance CXify#(AxiRdWrSlaveSceMiXactor#(`TLM_PRM), CXAxiRdWrSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(AxiRdWrSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAxiRdWrSlaveSceMiXactor#(`TLM_PRM));
      let ma = (interface AxiRdWrMaster ;
		   interface read  = ifc_in.read;
		   interface write = ifc_in.write;
		endinterface);
      let slsc <- mkCXify(ma, xtor_enable);
      return slsc;
   endmodule
endinstance

// ==========================

module [SceMiModule] mkCXAxiRdMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiRdMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   let _ifc0 <- mkAxiRdMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxiWrMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );

   let _ifc0 <- mkAxiWrMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxiRdWrMasterSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiRdWrMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size)
      );


   let _ifc0 <- mkAxiRdWrMasterSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule


///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxiRdSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiRdSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   let _ifc0 <- mkAxiRdSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxiWrSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   let _ifc0 <- mkAxiWrSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

///////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkCXAxiRdWrSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               CXAxiRdWrSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)

      );

   let _ifc0 <- mkAxiRdWrSlaveSceMiXactor (args);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

endpackage
