// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision: 17899 $
// $Date: 2009-09-21 09:39:55 -0400 (Mon, 21 Sep 2009) $

package AxiSlave;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import AxiDefines::*;
import Bus::*;
import FIFO::*;
import FShow::*;
import GetPut::*;
import SpecialFIFOs::*;
import TLM::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdSlaveIFC#(BusRecv#(AxiAddrCmd#(`TLM_TYPES)) request_addr,
			BusSend#(AxiRdResp#(`TLM_TYPES))  response) (AxiRdSlave#(`TLM_TYPES));
   
   Wire#(AxiId#(`TLM_TYPES))   arID_wire    <- mkBypassWire;
   Wire#(AxiLen)               arLEN_wire   <- mkBypassWire;
   Wire#(AxiSize)              arSIZE_wire  <- mkBypassWire;
   Wire#(AxiBurst)             arBURST_wire <- mkBypassWire;
   Wire#(AxiLock)              arLOCK_wire  <- mkBypassWire;
   Wire#(AxiCache)             arCACHE_wire <- mkBypassWire;
   Wire#(AxiProt)              arPROT_wire  <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_TYPES)) arADDR_wire  <- mkBypassWire;
   
   rule every;
      let addr_value = AxiAddrCmd {id:    arID_wire,
				   len:   arLEN_wire,
				   size:  arSIZE_wire,
				   burst: arBURST_wire,
				   lock:  arLOCK_wire,
				   cache: arCACHE_wire,
				   prot:  arPROT_wire,
				   addr:  arADDR_wire};
      request_addr.data(addr_value);
   endrule
  
   // Address Inputs
   method arID     = arID_wire._write;
   method arADDR   = arADDR_wire._write;
   method arLEN    = arLEN_wire._write;
   method arSIZE   = arSIZE_wire._write;
   method arBURST  = arBURST_wire._write;
   method arLOCK   = arLOCK_wire._write;
   method arCACHE  = arCACHE_wire._write;
   method arPROT   = arPROT_wire._write;
   method arVALID  = request_addr.valid;
      
   // Address Outputs
   method arREADY  = request_addr.ready;
      
   // Response Inputs
   method rREADY  = response.ready;
   
   // Response Outputs
   method rID     = response.data.id;
   method rDATA   = response.data.data;
   method rRESP   = response.data.resp;
   method rLAST   = response.data.last;
   method rVALID  = response.valid;
      
endmodule

module mkAxiWrSlaveIFC#(BusRecv#(AxiAddrCmd#(`TLM_TYPES)) request_addr,
			BusRecv#(AxiWrData#(`TLM_TYPES))  request_data,
			BusSend#(AxiWrResp#(`TLM_TYPES))  response) (AxiWrSlave#(`TLM_TYPES));
   
   Wire#(AxiId#(`TLM_TYPES))   awID_wire    <- mkBypassWire;
   Wire#(AxiLen)               awLEN_wire   <- mkBypassWire;
   Wire#(AxiSize)              awSIZE_wire  <- mkBypassWire;
   Wire#(AxiBurst)             awBURST_wire <- mkBypassWire;
   Wire#(AxiLock)              awLOCK_wire  <- mkBypassWire;
   Wire#(AxiCache)             awCACHE_wire <- mkBypassWire;
   Wire#(AxiProt)              awPROT_wire  <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_TYPES)) awADDR_wire  <- mkBypassWire;
   
   Wire#(AxiId#(`TLM_TYPES))     wID_wire     <- mkBypassWire;
   Wire#(AxiData#(`TLM_TYPES))   wDATA_wire   <- mkBypassWire;
   Wire#(AxiByteEn#(`TLM_TYPES)) wSTRB_wire   <- mkBypassWire;
   Wire#(Bool)                   wLAST_wire   <- mkBypassWire;

   
   rule every;
      let addr_value = AxiAddrCmd {id:    awID_wire,
				   len:   awLEN_wire,
				   size:  awSIZE_wire,
				   burst: awBURST_wire,
				   lock:  awLOCK_wire,
				   cache: awCACHE_wire,
				   prot:  awPROT_wire,
				   addr:  awADDR_wire};
      request_addr.data(addr_value);
      let data_value = AxiWrData {id:    wID_wire,
				  data:  wDATA_wire,
				  strb:  wSTRB_wire,
				  last:  wLAST_wire};
      request_data.data(data_value);
   endrule
  
   // Address Inputs
   method awID     = awID_wire._write;
   method awADDR   = awADDR_wire._write;
   method awLEN    = awLEN_wire._write;
   method awSIZE   = awSIZE_wire._write;
   method awBURST  = awBURST_wire._write;
   method awLOCK   = awLOCK_wire._write;
   method awCACHE  = awCACHE_wire._write;
   method awPROT   = awPROT_wire._write;
   method awVALID  = request_addr.valid;
      
   // Address Outputs
   method awREADY  = request_addr.ready;
      
   // Data Inputs
   method wID      = wID_wire._write;
   method wDATA    = wDATA_wire._write;
   method wSTRB    = wSTRB_wire._write;
   method wLAST    = wLAST_wire._write;
   method wVALID   = request_data.valid;
      
   // Data Outputs
   method wREADY   = request_data.ready;
	 
   // Response Inputs
   method bREADY  = response.ready;
   
   // Response Outputs
   method bID     = response.data.id;
   method bRESP   = response.data.resp;
   method bVALID  = response.valid;
      
endmodule

module mkAxiRdBusSlaveIFC#(AxiRdSlave#(`TLM_TYPES) ifc) (AxiRdBusSlave#(`TLM_TYPES));
   
   interface BusRecv addr;
      method Action data(AxiAddrCmd#(`TLM_TYPES) value);
	 ifc.arID(value.id);
	 ifc.arADDR(value.addr);
	 ifc.arLEN(value.len);
	 ifc.arSIZE(value.size);
	 ifc.arBURST(value.burst);
	 ifc.arLOCK(value.lock);
	 ifc.arCACHE(value.cache);
	 ifc.arPROT(value.prot);
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusSend resp;
      method AxiRdResp#(`TLM_TYPES) data;
	 let resp = AxiRdResp {id:    ifc.rID,
			       data: ifc.rDATA,
			       resp: ifc.rRESP,
			       last: ifc.rLAST};
	 return resp;
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxiWrBusSlaveIFC#(AxiWrSlave#(`TLM_TYPES) ifc) (AxiWrBusSlave#(`TLM_TYPES));
   
   interface BusRecv addr;
      method Action data(AxiAddrCmd#(`TLM_TYPES) value);
	 ifc.awID(value.id);
	 ifc.awADDR(value.addr);
	 ifc.awLEN(value.len);
	 ifc.awSIZE(value.size);
	 ifc.awBURST(value.burst);
	 ifc.awLOCK(value.lock);
	 ifc.awCACHE(value.cache);
	 ifc.awPROT(value.prot);
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusRecv data;
      method Action data(AxiWrData#(`TLM_TYPES) value);
	 ifc.wID(value.id);
	 ifc.wDATA(value.data);
	 ifc.wSTRB(value.strb);
	 ifc.wLAST(value.last);
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusSend resp;
      method AxiWrResp#(`TLM_TYPES) data;
	 let resp = AxiWrResp {id:   ifc.bID,
			       resp: ifc.bRESP};
	 return resp;
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdSlaveStd#(function Bool addr_match(AxiAddr#(`TLM_STD_TYPES) addr)) 
			   (AxiRdSlaveXActorIFC#(`TLM_STD_TYPES));
			
   let _ifc <- mkAxiRdSlaveSynthStd;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface AxiRdFabricSlave fabric;
      interface AxiRdSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxiRdSlave#(function Bool addr_match(AxiAddr#(`TLM_TYPES) addr)) 
			(AxiRdSlaveXActorIFC#(`TLM_TYPES))
   provisos(Bits#(TLMRequest#(`TLM_TYPES), s0),
	    Bits#(TLMResponse#(`TLM_TYPES), s1),
	    Bits#(RequestDescriptor#(`TLM_TYPES), s2));
			
   let _ifc <- mkAxiRdSlaveSynth;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface AxiRdFabricSlave fabric;
      interface AxiRdSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule


(* synthesize *)
module mkAxiRdSlaveSynthStd (AxiRdSlaveXActorIFC#(`TLM_STD_TYPES));
   let _ifc <- mkAxiRdSlaveSynth;
   return _ifc;
endmodule


module mkAxiRdSlaveSynth (AxiRdSlaveXActorIFC#(`TLM_TYPES))
   provisos(Bits#(TLMRequest#(`TLM_TYPES), s0),
	    Bits#(TLMResponse#(`TLM_TYPES), s1),
	    Bits#(RequestDescriptor#(`TLM_TYPES), s2),
	    Add#(SizeOf#(AxiLen), 1, n));
   
   BusReceiver#(AxiAddrCmd#(`TLM_TYPES)) rd_addr_fifo <- mkBypassBusReceiver;
   BusSender#(AxiRdResp#(`TLM_TYPES))    rd_resp_fifo <- mkBypassBusSender(unpack(0));
   
   FIFO#(TLMRequest#(`TLM_TYPES))        fifo_tx      <- mkBypassFIFO;
   FIFO#(TLMResponse#(`TLM_TYPES))       fifo_rx      <- mkBypassFIFO;
    
   FIFO#(Bool)                           fifo_buffer  <- mkBypassFIFO;
   
   Reg#(Bit#(n))                         count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_TYPES))  desc_prev    <- mkReg(?);
   
   let _ifc <- mkAxiRdSlaveIFC(rd_addr_fifo.in, rd_resp_fifo.out);
   
   rule grab_addr (count == 0);
      let value = rd_addr_fifo.out.first;
      TLMBurstSize#(`TLM_TYPES) zz = fromAxiSize(value.size);
      Bit#(n) remaining = {0, value.len} + 1;
      let desc = fromAxiAddrCmd(value);
      desc.command = READ;
      count <= remaining;
      desc_prev <= desc;
      rd_addr_fifo.out.deq;
   endrule
   
   rule do_read (count > 0);
      let remaining = count - 1;
      let last = (remaining == 0);
      count <= remaining;
      let desc = desc_prev;
      desc_prev <= incrTLMAddr(desc);
      desc.burst_length = 1;
      fifo_tx.enq(tagged Descriptor desc);
      fifo_buffer.enq(last);
   endrule
   
   rule grap_tlm_response;
      let response = fifo_rx.first;
      let id = response.transaction_id;
      let last = fifo_buffer.first;
      fifo_rx.deq;
      fifo_buffer.deq;
      AxiRdResp#(`TLM_TYPES) axi_response = unpack(0);
      axi_response.id = getAxiId(id);
      axi_response.resp = getAxiResp(response.status);
      axi_response.data = response.data;
      axi_response.last = last;
      rd_resp_fifo.in.enq(axi_response);
   endrule
   
   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface
   
   interface AxiRdFabricSlave fabric;
      interface AxiRdSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_TYPES) value) = False;
   endinterface
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiWrSlaveStd#(function Bool addr_match(AxiAddr#(`TLM_STD_TYPES) addr)) 
			   (AxiWrSlaveXActorIFC#(`TLM_STD_TYPES));
			
   let _ifc <- mkAxiWrSlaveSynthStd;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface AxiWrFabricSlave fabric;
      interface AxiWrSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxiWrSlave#(function Bool addr_match(AxiAddr#(`TLM_TYPES) addr)) 
			(AxiWrSlaveXActorIFC#(`TLM_TYPES))
   provisos(Bits#(TLMRequest#(`TLM_TYPES), s0),
	    Bits#(TLMResponse#(`TLM_TYPES), s1),
	    Bits#(RequestDescriptor#(`TLM_TYPES), s2));
			
   let _ifc <- mkAxiWrSlaveSynth;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface AxiWrFabricSlave fabric;
      interface AxiWrSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

(* synthesize *)
module mkAxiWrSlaveSynthStd (AxiWrSlaveXActorIFC#(`TLM_STD_TYPES));
   let _ifc <- mkAxiWrSlaveSynth;
   return _ifc;
endmodule

module mkAxiWrSlaveSynth (AxiWrSlaveXActorIFC#(`TLM_TYPES))
   provisos(Bits#(TLMRequest#(`TLM_TYPES), s0),
	    Bits#(TLMResponse#(`TLM_TYPES), s1),
	    Bits#(RequestDescriptor#(`TLM_TYPES), s2),
	    Add#(SizeOf#(AxiLen), 1, n));
   
   BusReceiver#(AxiAddrCmd#(`TLM_TYPES)) wr_addr_fifo <- mkBypassBusReceiver;
   BusReceiver#(AxiWrData#(`TLM_TYPES))  wr_data_fifo <- mkBypassBusReceiver;
   BusSender#(AxiWrResp#(`TLM_TYPES))    wr_resp_fifo <- mkBusSender(unpack(0));
   
   FIFO#(TLMRequest#(`TLM_TYPES))        fifo_tx      <- mkBypassFIFO;
   FIFO#(TLMResponse#(`TLM_TYPES))       fifo_rx      <- mkBypassFIFO;
   
   FIFO#(Maybe#(AxiId#(`TLM_TYPES)))     fifo_buffer  <- mkBypassFIFO;
   
   Reg#(Bit#(n))                         count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_TYPES))  desc_prev    <- mkReg(?);
   
   let _ifc <- mkAxiWrSlaveIFC(wr_addr_fifo.in, wr_data_fifo.in, wr_resp_fifo.out);
   
   rule grab_addr (count == 0);
      let value  = wr_addr_fifo.out.first;
      let dvalue = wr_data_fifo.out.first;
      Bit#(n) remaining = {0, value.len};
      let desc = fromAxiAddrCmd(value);
      desc.command = WRITE;
      desc.data = dvalue.data;
      desc.byte_enable = dvalue.strb;
      count <= remaining;
      desc_prev <= incrTLMAddr(desc);
      wr_addr_fifo.out.deq;
      wr_data_fifo.out.deq;
      let token = (dvalue.last) ? (tagged Valid dvalue.id) : tagged Invalid;
      desc.burst_length = 1;
      fifo_tx.enq(tagged Descriptor desc);
      fifo_buffer.enq(token);
   endrule
   
   rule grab_data (count > 0);
      let value = wr_data_fifo.out.first;
      let remaining =  count - 1;
      count <= remaining;
      let desc = desc_prev;
      desc.data = value.data;
      desc.byte_enable = value.strb;
      desc_prev <= incrTLMAddr(desc);
      wr_data_fifo.out.deq;
      let token = (value.last) ? (tagged Valid value.id) : tagged Invalid;
      desc.burst_length = 1;
      fifo_tx.enq(tagged Descriptor desc);
      fifo_buffer.enq(token);
   endrule
   
   rule grap_tlm_response (fifo_buffer.first matches tagged Invalid);
      fifo_buffer.deq;
      fifo_rx.deq;
   endrule
   
   rule send_axi_response (fifo_buffer.first matches tagged Valid .id);
      AxiWrResp#(`TLM_TYPES) resp = unpack(0);
      resp.id = id;
      resp.resp = OKAY;
      wr_resp_fifo.in.enq(resp);
      fifo_buffer.deq;
      fifo_rx.deq;
   endrule

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface
   
   interface AxiWrFabricSlave fabric;
      interface AxiWrSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_TYPES) value) = False;
   endinterface
      
endmodule

endpackage
