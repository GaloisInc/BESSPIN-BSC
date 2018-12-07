// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision: 18220 $
// $Date: 2009-10-27 11:19:11 -0400 (Tue, 27 Oct 2009) $

package AxiDefines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Arbiter::*;
import Bus::*;
import Connectable::*;
import FShow::*;
import Probe::*;
import TLM::*;
import BUtils::*;
import Vector::*;

`include "TLM.defines"

 ////////////////////////////////////////////////////////////////////////////////
/// Data Structures
////////////////////////////////////////////////////////////////////////////////

typedef Bit#(addr_size)                  AxiAddr#(`TLM_TYPE_PRMS);
typedef Bit#(data_size)                  AxiData#(`TLM_TYPE_PRMS);
typedef Bit#(TDiv#(data_size, 8))        AxiByteEn#(`TLM_TYPE_PRMS);

typedef Bit#(id_size)                        AxiId#(`TLM_TYPE_PRMS); // Unique id
typedef Bit#(4)                              AxiLen;  // 1 - 16
typedef Bit#(3)                              AxiSize; // width in bytes
typedef Bit#(4)                              AxiCache;
typedef Bit#(3)                              AxiProt;

typedef enum {FIXED, INCR, WRAP}             AxiBurst `dv;
typedef enum {NORMAL, EXCLUSIVE, LOCKED}     AxiLock  `dv;
typedef enum {OKAY, EXOKAY, SLVERR, DECERR } AxiResp `dv;


typedef struct {
                AxiId#(`TLM_TYPES)   id;
                AxiLen               len;
                AxiSize              size;
                AxiBurst             burst;
                AxiLock              lock;
                AxiCache             cache;
                AxiProt              prot;
                AxiAddr#(`TLM_TYPES) addr;
		} AxiAddrCmd#(`TLM_TYPE_PRMS) `dv;

typedef struct {
		AxiId#(`TLM_TYPES)     id;
		AxiData#(`TLM_TYPES)   data;
		AxiByteEn#(`TLM_TYPES) strb;
		Bool                   last;
		} AxiWrData#(`TLM_TYPE_PRMS) `dv;


typedef struct {
		AxiId#(`TLM_TYPES)   id;
		AxiData#(`TLM_TYPES) data;
		AxiResp              resp;
		Bool                 last;
		} AxiRdResp#(`TLM_TYPE_PRMS) `dv;

typedef struct {
		AxiId#(`TLM_TYPES)   id;
		AxiResp              resp;
		} AxiWrResp#(`TLM_TYPE_PRMS) `dv;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance BusPayload#(AxiAddrCmd#(`TLM_TYPES), TLMId#(`TLM_TYPES));
   function Bool isLast (payload);
      return True;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiWrData#(`TLM_TYPES), TLMId#(`TLM_TYPES));
   function Bool isLast (payload);
      return payload.last;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiWrResp#(`TLM_TYPES), TLMId#(`TLM_TYPES));
   function Bool isLast (payload);
      return True;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiRdResp#(`TLM_TYPES), TLMId#(`TLM_TYPES));
   function Bool isLast (payload);
      return payload.last;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface AxiWrMaster#(`TLM_TYPE_PRMS);

   // Address Outputs
   (* result = "AWID" *)
   method AxiId#(`TLM_TYPES)   awID;
   (* result = "AWADDR" *)
   method AxiAddr#(`TLM_TYPES) awADDR;
   (* result = "AWLEN" *)
   method AxiLen               awLEN;
   (* result = "AWSIZE" *)
   method AxiSize              awSIZE;
   (* result = "AWBURST" *)
   method AxiBurst             awBURST;
   (* result = "AWLOCK" *)
   method AxiLock              awLOCK;
   (* result = "AWCACHE" *)
   method AxiCache             awCACHE;
   (* result = "AWPROT" *)
   method AxiProt              awPROT;
   (* result = "AWVALID" *)
   method Bool                 awVALID;

   // Address Inputs
   (* prefix = "", result = "unusedwm0" *)
   method Action awREADY((* port = "AWREADY" *) Bool value);

   // Data Outputs
   (* result = "WID" *)
   method AxiId#(`TLM_TYPES)     wID;
   (* result = "WDATA" *)
   method AxiData#(`TLM_TYPES)   wDATA;
   (* result = "WSTRB" *)
   method AxiByteEn#(`TLM_TYPES) wSTRB;
   (* result = "WLAST" *)
   method Bool                   wLAST;
   (* result = "WVALID" *)
   method Bool                   wVALID;

   // Data Inputs
   (* prefix = "", result = "unusedwm1" *)
   method Action wREADY((* port = "WREADY" *) Bool value);

   // Response Outputs
   (* result = "BREADY" *)
   method Bool                   bREADY;

   // Response Inputs
   (* prefix = "", result = "unusedwm2" *)
   method Action bID((* port = "BID" *) AxiId#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedwm3" *)
   method Action bRESP((* port = "BRESP" *) AxiResp value);
   (* prefix = "", result = "unusedwm4" *)
   method Action bVALID((* port = "BVALID" *) Bool value);

endinterface

(* always_ready, always_enabled *)
interface AxiRdMaster#(`TLM_TYPE_PRMS);

   // Address Outputs
   (* result = "ARID" *)
   method AxiId#(`TLM_TYPES)   arID;
   (* result = "ARADDR" *)
   method AxiAddr#(`TLM_TYPES) arADDR;
   (* result = "ARLEN" *)
   method AxiLen               arLEN;
   (* result = "ARSIZE" *)
   method AxiSize              arSIZE;
   (* result = "ARBURST" *)
   method AxiBurst             arBURST;
   (* result = "ARLOCK" *)
   method AxiLock              arLOCK;
   (* result = "ARCACHE" *)
   method AxiCache             arCACHE;
   (* result = "ARPROT" *)
   method AxiProt              arPROT;
   (* result = "ARVALID" *)
   method Bool                 arVALID;

   // Address Inputs
   (* prefix = "", result = "unusedrm0" *)
   method Action arREADY((* port = "ARREADY" *) Bool value);

   // Response Outputs
   (* result = "RREADY" *)
   method Bool                   rREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrm1" *)
   method Action rID((* port = "RID" *) AxiId#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedrm2" *)
   method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedrm3" *)
   method Action rRESP((* port = "RRESP" *) AxiResp value);
   (* prefix = "", result = "unusedrm4" *)
   method Action rLAST((* port = "RLAST" *) Bool value);
   (* prefix = "", result = "unusedrm5" *)
   method Action rVALID((* port = "RVALID" *) Bool value);

endinterface


(* always_ready, always_enabled *)
interface AxiWrSlave#(`TLM_TYPE_PRMS);

   // Address Inputs
   (* prefix = "", result = "unusedws0" *)
   method Action awID((* port = "AWID" *) AxiId#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedws1" *)
   method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedws2" *)
   method Action awLEN((* port = "AWLEN" *) AxiLen value);
   (* prefix = "", result = "unusedws3" *)
   method Action awSIZE((* port = "AWSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedws4" *)
   method Action awBURST((* port = "AWBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedws5" *)
   method Action awLOCK((* port = "AWLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedws6" *)
   method Action awCACHE((* port = "AWCACHE" *) AxiCache value);
   (* prefix = "", result = "unusedws7" *)
   method Action awPROT((* port = "AWPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws8" *)
   method Action awVALID((* port = "AWVALID" *) Bool value);

   // Address Outputs
   (* result = "AWREADY" *)
   method Bool                  awREADY;

   // Data Inputs
   (* prefix = "", result = "unusedws9" *)
   method Action wID((* port = "WID" *) AxiId#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedws10" *)
   method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedws11" *)
   method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedws12" *)
   method Action wLAST((* port = "WLAST" *) Bool value);
   (* prefix = "", result = "unusedws13" *)
   method Action wVALID((* port = "WVALID" *) Bool value);

   // Data Ouptuts
   (* result = "WREADY" *)
   method Bool                   wREADY;

   // Response Inputs
   (* prefix = "", result = "unusedws14" *)
   method Action bREADY((* port = "BREADY" *) Bool value);

   // Response Outputs
   (* result = "BID" *)
   method AxiId#(`TLM_TYPES)     bID;
   (* result = "BRESP" *)
   method AxiResp                bRESP;
   (* result = "BVALID" *)
   method Bool                   bVALID;

endinterface

(* always_ready, always_enabled *)
interface AxiRdSlave#(`TLM_TYPE_PRMS);

   // Address Inputs
   (* prefix = "", result = "unusedrs0" *)
   method Action arID((* port = "ARID" *) AxiId#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedrs1" *)
   method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_TYPES) value);
   (* prefix = "", result = "unusedrs2" *)
   method Action arLEN((* port = "ARLEN" *) AxiLen value);
   (* prefix = "", result = "unusedrs3" *)
   method Action arSIZE((* port = "ARSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedrs4" *)
   method Action arBURST((* port = "ARBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedrs5" *)
   method Action arLOCK((* port = "ARLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedrs6" *)
   method Action arCACHE((* port = "ARCACHE" *) AxiCache value);
   (* prefix = "", result = "unusedrs7" *)
   method Action arPROT((* port = "ARPROT" *) AxiProt value);
   (* prefix = "", result = "unusedrs8" *)
   method Action arVALID((* port = "ARVALID" *) Bool value);

   // Address Outputs
   (* result = "ARREADY" *)
   method Bool                  arREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrs9" *)
   method Action rREADY((* port = "RREADY" *) Bool value);

   // Response Outputs
   (* result = "RID" *)
   method AxiId#(`TLM_TYPES)     rID;
   (* result = "RDATA" *)
   method AxiData#(`TLM_TYPES)   rDATA;
   (* result = "RRESP" *)
   method AxiResp                rRESP;
   (* result = "RLAST" *)
   method Bool                   rLAST;
   (* result = "RVALID" *)
   method Bool                   rVALID;

endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiRdBusMaster#(`TLM_TYPE_PRMS);
   interface BusSend#(AxiAddrCmd#(`TLM_TYPES)) addr;
   interface BusRecv#(AxiRdResp#(`TLM_TYPES))  resp;
endinterface

interface AxiWrBusMaster#(`TLM_TYPE_PRMS);
   interface BusSend#(AxiAddrCmd#(`TLM_TYPES)) addr;
   interface BusSend#(AxiWrData#(`TLM_TYPES))  data;
   interface BusRecv#(AxiWrResp#(`TLM_TYPES))  resp;
endinterface

interface AxiRdBusSlave#(`TLM_TYPE_PRMS);
   interface BusRecv#(AxiAddrCmd#(`TLM_TYPES)) addr;
   interface BusSend#(AxiRdResp#(`TLM_TYPES))  resp;
endinterface

interface AxiWrBusSlave#(`TLM_TYPE_PRMS);
   interface BusRecv#(AxiAddrCmd#(`TLM_TYPES)) addr;
   interface BusRecv#(AxiWrData#(`TLM_TYPES))  data;
   interface BusSend#(AxiWrResp#(`TLM_TYPES))  resp;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiRdFabricMaster#(`TLM_TYPE_PRMS);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_TYPES) bus;
endinterface

interface AxiRdFabricSlave#(`TLM_TYPE_PRMS);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_TYPES) bus;
   method Bool addrMatch(AxiAddr#(`TLM_TYPES) value);
endinterface

interface AxiWrFabricMaster#(`TLM_TYPE_PRMS);
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_TYPES) bus;
endinterface

interface AxiWrFabricSlave#(`TLM_TYPE_PRMS);
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_TYPES) bus;
   method Bool addrMatch(AxiAddr#(`TLM_TYPES) value);
endinterface

interface AxiRdMasterXActorIFC#(`TLM_TYPE_PRMS);
   interface TLMRecvIFC#(`TLM_TYPES)        tlm;
   (* prefix = "" *)
   interface AxiRdFabricMaster#(`TLM_TYPES) fabric;
endinterface

interface AxiWrMasterXActorIFC#(`TLM_TYPE_PRMS);
   interface TLMRecvIFC#(`TLM_TYPES)        tlm;
   (* prefix = "" *)
   interface AxiWrFabricMaster#(`TLM_TYPES) fabric;
endinterface

interface AxiRdSlaveXActorIFC#(`TLM_TYPE_PRMS);
   interface TLMSendIFC#(`TLM_TYPES)       tlm;
   (* prefix = "" *)
   interface AxiRdFabricSlave#(`TLM_TYPES) fabric;
endinterface

interface AxiWrSlaveXActorIFC#(`TLM_TYPE_PRMS);
   interface TLMSendIFC#(`TLM_TYPES)       tlm;
   (* prefix = "" *)
   interface AxiWrFabricSlave#(`TLM_TYPES) fabric;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(AxiWrMaster#(`TLM_TYPES), AxiWrSlave#(`TLM_TYPES));
   module mkConnection#(AxiWrMaster#(`TLM_TYPES) m, AxiWrSlave#(`TLM_TYPES) s )(Empty);

      rule master_to_slave_addr_data;
	 // Address Signals
	 s.awID(m.awID);
	 s.awADDR(m.awADDR);
	 s.awLEN(m.awLEN);
	 s.awSIZE(m.awSIZE);
	 s.awBURST(m.awBURST);
	 s.awLOCK(m.awLOCK);
	 s.awCACHE(m.awCACHE);
	 s.awPROT(m.awPROT);
	 s.awVALID(m.awVALID);
	 // Data Signals
	 s.wID(m.wID);
	 s.wDATA(m.wDATA);
	 s.wSTRB(m.wSTRB);
	 s.wLAST(m.wLAST);
	 s.wVALID(m.wVALID);
      endrule

      rule master_to_slave_response;
	 // Response Signals
	 s.bREADY(m.bREADY);
      endrule

      rule slave_to_master_addr_data;
	 // Address Signals
	 m.awREADY(s.awREADY);
	 // Data Signals
	 m.wREADY(s.wREADY);
      endrule

      rule slave_to_master_response;
	 // Response Signals
	 m.bID(s.bID);
	 m.bRESP(s.bRESP);
	 m.bVALID(s.bVALID);
      endrule

   endmodule
endinstance

instance Connectable#(AxiRdMaster#(`TLM_TYPES), AxiRdSlave#(`TLM_TYPES));
   module mkConnection#(AxiRdMaster#(`TLM_TYPES) m, AxiRdSlave#(`TLM_TYPES) s )(Empty);

      rule master_to_slave_addr;
	 // Address Signals
	 s.arID(m.arID);
	 s.arADDR(m.arADDR);
	 s.arLEN(m.arLEN);
	 s.arSIZE(m.arSIZE);
	 s.arBURST(m.arBURST);
	 s.arLOCK(m.arLOCK);
	 s.arCACHE(m.arCACHE);
	 s.arPROT(m.arPROT);
	 s.arVALID(m.arVALID);
      endrule

      rule master_to_slave_response;
	 // Response Signals
	 s.rREADY(m.rREADY);
      endrule

      rule slave_to_master_addr;
	 // Address Signals
	 m.arREADY(s.arREADY);
      endrule

      rule slave_to_master_response;
	 // Response Signals
	 m.rID(s.rID);
	 m.rDATA(s.rDATA);
	 m.rRESP(s.rRESP);
	 m.rLAST(s.rLAST);
	 m.rVALID(s.rVALID);
      endrule

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
/// TLM conversion functions
/// TLM to AXI:
////////////////////////////////////////////////////////////////////////////////

function AxiAddrCmd#(`TLM_TYPES) getAxiAddrCmd (RequestDescriptor#(`TLM_TYPES) tlm_descriptor);

   AxiAddrCmd#(`TLM_TYPES) addr_cmd = unpack(0);
   addr_cmd.id    = getAxiId(tlm_descriptor.transaction_id);
   addr_cmd.len   = getAxiLen(tlm_descriptor.burst_length);
   addr_cmd.size  = getAxiSize(tlm_descriptor.burst_size);
   addr_cmd.burst = getAxiBurst(tlm_descriptor.burst_mode);
   addr_cmd.lock  = NORMAL;
   addr_cmd.cache = 0;
   addr_cmd.prot  = 0;
   addr_cmd.addr  = tlm_descriptor.addr;

   return addr_cmd;

endfunction

function AxiWrData#(`TLM_TYPES) getFirstAxiWrData (RequestDescriptor#(`TLM_TYPES) tlm_descriptor);

   AxiWrData#(`TLM_TYPES) wr_data = unpack(0);
   wr_data.id   = getAxiId(tlm_descriptor.transaction_id);
   wr_data.data = tlm_descriptor.data;
//   wr_data.strb = tlm_descriptor.byte_enable;
   wr_data.strb = getAxiByteEn(tlm_descriptor);
   wr_data.last = (tlm_descriptor.burst_length == 1);

   return wr_data;

endfunction

function AxiByteEn#(`TLM_TYPES) getAxiByteEn (RequestDescriptor#(`TLM_TYPES) tlm_descriptor);
   Bit#(TLog#(SizeOf#(AxiByteEn#(`TLM_TYPES)))) addr = zExtend(tlm_descriptor.addr);
   AxiByteEn#(`TLM_TYPES) all_ones = unpack('1);
   let mask = ~(all_ones << ({1'b0,tlm_descriptor.burst_size} + 1));

   return (mask << addr);
endfunction


function AxiLen getAxiLen(TLMUInt#(`TLM_TYPES) burst_length);
   AxiLen length = cExtend(burst_length - 1);
   return length;
endfunction

function AxiSize getAxiSize(TLMBurstSize#(`TLM_TYPES) incr);
   Bit#(8) value = cExtend(incr);
   case (value)
      (  1 - 1): return 0;
      (  2 - 1): return 1;
      (  4 - 1): return 2;
      (  8 - 1): return 3;
      ( 16 - 1): return 4;
      ( 32 - 1): return 5;
      ( 64 - 1): return 6;
      (128 - 1): return 7;
   endcase
endfunction

function AxiBurst getAxiBurst(TLMBurstMode burst_mode);
   case (burst_mode)
      INCR: return INCR;
      CNST: return FIXED;
      WRAP:      return WRAP;
   endcase
endfunction

function AxiId#(`TLM_TYPES) getAxiId(TLMId#(`TLM_TYPES) transaction_id);
   return cExtend(transaction_id);
endfunction

function AxiResp getAxiResp(TLMStatus status);
   case (status)
      SUCCESS:     return OKAY;
      ERROR:       return SLVERR;
      NO_RESPONSE: return SLVERR;
     endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
/// AXI to TLM:
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_TYPES) fromAxiAddrCmd (AxiAddrCmd#(`TLM_TYPES) addr_cmd)
   provisos(Bits#(RequestDescriptor#(`TLM_TYPES), s0));

   RequestDescriptor#(`TLM_TYPES) desc = unpack(0);
   desc.command         = READ; // added later
   desc.mode            = REGULAR;
   desc.addr            = addr_cmd.addr;
   desc.data            = 0; // added later
   desc.burst_length    = fromAxiLen(addr_cmd.len);
   desc.byte_enable     = '1; // added later
   desc.burst_mode      = fromAxiBurst(addr_cmd.burst);
   desc.burst_size      = fromAxiSize(addr_cmd.size);
   desc.prty = 0;
   desc.thread_id = 0;
   desc.transaction_id = fromAxiId(addr_cmd.id);
   desc.export_id = 0;

   return desc;

endfunction

function TLMUInt#(`TLM_TYPES) fromAxiLen(AxiLen len);
   let burst_length = unpack(zExtend(len) + 1);
   return burst_length;
endfunction

function TLMBurstMode fromAxiBurst(AxiBurst burst);
   case (burst)
      INCR:  return INCR;
      FIXED: return CNST;
      WRAP:  return WRAP;
   endcase
endfunction

function TLMBurstSize#(`TLM_TYPES) fromAxiSize(AxiSize size);
   Bit#(TAdd#(SizeOf#(TLMBurstSize#(`TLM_TYPES)), 1)) incr;
   incr = (1 << size) - 1;
   return zExtend(incr);
endfunction

function TLMId#(`TLM_TYPES) fromAxiId(AxiId#(`TLM_TYPES) id);
   return cExtend(id);
endfunction

function TLMStatus fromAxiResp(AxiResp resp);
   case (resp)
      OKAY:    return SUCCESS;
      EXOKAY:  return SUCCESS;
      default: return ERROR;
     endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Arbitable#(AxiWrBusMaster#(`TLM_TYPES));
   module mkArbiterRequest#(AxiWrBusMaster#(`TLM_TYPES) master) (ArbiterRequest_IFC);

      let addr_req <- mkArbiterRequest(master.addr);

      method Bool request;
	 return addr_req.request;
      endmethod
      method Bool lock;
	 return False;
      endmethod
      method Action grant;
	 // a no-op in this case
      endmethod

   endmodule
endinstance


instance Arbitable#(AxiRdBusMaster#(`TLM_TYPES));
   module mkArbiterRequest#(AxiRdBusMaster#(`TLM_TYPES) master) (ArbiterRequest_IFC);

      let addr_req <- mkArbiterRequest(master.addr);

      method Bool request;
	 return addr_req.request;
      endmethod
      method Bool lock;
	 return False;
      endmethod
      method Action grant;
	 // a no-op in this case
      endmethod

   endmodule
endinstance

instance Monitorable#(AxiRdMaster#(`TLM_TYPES));
   module mkMonitor#(AxiRdMaster#(`TLM_TYPES) ifc) (Empty);

      Probe#(AxiId#(`TLM_TYPES))     ar_id    <- mkProbe;
      Probe#(AxiLen)                 ar_len   <- mkProbe;
      Probe#(AxiSize)                ar_size  <- mkProbe;
      Probe#(AxiBurst)               ar_burst <- dbgProbe;
      Probe#(AxiLock)                ar_lock  <- dbgProbe;
      Probe#(AxiCache)               ar_cache <- mkProbe;
      Probe#(AxiProt)                ar_prot  <- mkProbe;
      Probe#(AxiAddr#(`TLM_TYPES))   ar_addr  <- mkProbe;
      Probe#(Bool)                   ar_valid <- mkProbe;

      Probe#(Bool)                   r_ready  <- mkProbe;

      rule every;
	 ar_id    <= ifc.arID;
	 ar_len   <= ifc.arLEN;
	 ar_size  <= ifc.arSIZE;
	 ar_burst <= ifc.arBURST;
	 ar_lock  <= ifc.arLOCK;
	 ar_cache <= ifc.arCACHE;
	 ar_prot  <= ifc.arPROT;
	 ar_addr  <= ifc.arADDR;
	 ar_valid <= ifc.arVALID;

	 r_ready  <= ifc.rREADY;
      endrule

   endmodule
endinstance


instance Monitorable#(AxiRdSlave#(`TLM_TYPES));
   module mkMonitor#(AxiRdSlave#(`TLM_TYPES) ifc) (Empty);

      Probe#(Bool)                   ar_ready <- mkProbe;

      Probe#(AxiId#(`TLM_TYPES))     r_id     <- mkProbe;
      Probe#(AxiData#(`TLM_TYPES))   r_data   <- mkProbe;
      Probe#(AxiResp)                r_resp   <- dbgProbe;
      Probe#(Bool)                   r_last   <- mkProbe;
      Probe#(Bool)                   r_valid  <- mkProbe;

      rule every;
	 ar_ready <= ifc.arREADY;

	 r_id     <= ifc.rID;
	 r_data   <= ifc.rDATA;
	 r_resp   <= ifc.rRESP;
	 r_last   <= ifc.rLAST;
	 r_valid  <= ifc.rVALID;
      endrule

   endmodule
endinstance


instance Monitorable#(AxiWrMaster#(`TLM_TYPES));
   module mkMonitor#(AxiWrMaster#(`TLM_TYPES) ifc) (Empty);

      Probe#(AxiId#(`TLM_TYPES))     aw_id    <- mkProbe;
      Probe#(AxiLen)                 aw_len   <- mkProbe;
      Probe#(AxiSize)                aw_size  <- mkProbe;
      Probe#(AxiBurst)               aw_burst <- dbgProbe;
      Probe#(AxiLock)                aw_lock  <- dbgProbe;
      Probe#(AxiCache)               aw_cache <- mkProbe;
      Probe#(AxiProt)                aw_prot  <- mkProbe;
      Probe#(AxiAddr#(`TLM_TYPES))   aw_addr  <- mkProbe;
      Probe#(Bool)                   aw_valid <- mkProbe;

      Probe#(AxiId#(`TLM_TYPES))     w_id     <- mkProbe;
      Probe#(AxiData#(`TLM_TYPES))   w_data   <- mkProbe;
      Probe#(AxiByteEn#(`TLM_TYPES)) w_strb   <- mkProbe;
      Probe#(Bool)                   w_last   <- mkProbe;
      Probe#(Bool)                   w_valid  <- mkProbe;

      Probe#(Bool)                   b_ready  <- mkProbe;

      rule every;
	 aw_id    <= ifc.awID;
	 aw_len   <= ifc.awLEN;
	 aw_size  <= ifc.awSIZE;
	 aw_burst <= ifc.awBURST;
	 aw_lock  <= ifc.awLOCK;
	 aw_cache <= ifc.awCACHE;
	 aw_prot  <= ifc.awPROT;
	 aw_addr  <= ifc.awADDR;
	 aw_valid <= ifc.awVALID;

	 w_id     <= ifc.wID;
	 w_data   <= ifc.wDATA;
	 w_strb   <= ifc.wSTRB;
	 w_last   <= ifc.wLAST;
	 w_valid  <= ifc.wVALID;

	 b_ready  <= ifc.bREADY;
      endrule

   endmodule
endinstance

instance Monitorable#(AxiWrSlave#(`TLM_TYPES));
   module mkMonitor#(AxiWrSlave#(`TLM_TYPES) ifc) (Empty);

      Probe#(Bool)                   aw_ready <- mkProbe;

      Probe#(Bool)                   w_ready  <- mkProbe;

      Probe#(AxiId#(`TLM_TYPES))     b_id     <- mkProbe;
      Probe#(AxiResp)                b_resp   <- dbgProbe;
      Probe#(Bool)                   b_valid  <- mkProbe;

      rule every;
	 aw_ready <= ifc.awREADY;

	 w_ready  <= ifc.wREADY;

	 b_id     <= ifc.bID;
	 b_resp   <= ifc.bRESP;
	 b_valid  <= ifc.bVALID;
      endrule

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance FShow#(AxiBurst);
   function Fmt fshow (AxiBurst label);
      case (label)
	 FIXED: return fshow("FIXED");
	 INCR:  return fshow("INCR");
	 WRAP:  return fshow("WRAP");
      endcase
   endfunction
endinstance

instance FShow#(AxiLock);
   function Fmt fshow (AxiLock label);
      case (label)
	 NORMAL:    return fshow("NORMAL");
	 EXCLUSIVE: return fshow("EXCLSV");
	 LOCKED:    return fshow("LOCKED");
      endcase
   endfunction
endinstance


instance FShow#(AxiResp);
   function Fmt fshow (AxiResp label);
      case (label)
	 OKAY:   return fshow("OKAY");
	 EXOKAY: return fshow("EXOKAY");
	 SLVERR: return fshow("SLVERR");
	 DECERR: return fshow("DECERR");
      endcase
   endfunction
endinstance


endpackage
