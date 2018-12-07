// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision: 17899 $
// $Date: 2009-09-21 09:39:55 -0400 (Mon, 21 Sep 2009) $

package AxiRdBus;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Arbiter::*;
import AxiDefines::*;
import AxiMaster::*;
import AxiSlave::*;
import Bus::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import TLM::*;
import BUtils::*;
import List::*;
import Vector::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdBus#(Vector#(master_count, AxiRdFabricMaster#(`TLM_TYPES)) masters, 
		   Vector#(slave_count,  AxiRdFabricSlave#(`TLM_TYPES))  slaves) (Empty)
   provisos(Log#(master_count, size_m), 
	    Log#(slave_count, size_s),
	    Add#(ignore0, size_m, id_size),
	    Add#(ignore1, size_s, id_size));
   
   Wire#(Bool) fixed <- mkDWire(True);
   
   function AxiRdMaster#(`TLM_TYPES) master_bus_ifc(AxiRdFabricMaster#(`TLM_TYPES) ifc) = ifc.bus;
   function AxiRdSlave#(`TLM_TYPES)  slave_bus_ifc(AxiRdFabricSlave#(`TLM_TYPES) ifc)   = ifc.bus;
   
   let master_vector <- mapM(mkAxiRdBusMasterIFC, map(master_bus_ifc, masters));
   let slave_vector  <- mapM(mkAxiRdBusSlaveIFC,  map(slave_bus_ifc, slaves));
      
   Arbiter_IFC#(master_count) arbiter <- mkArbiter(fixed);
   Vector#(master_count, ArbiterRequest_IFC) requests <- mapM(mkArbiterRequest, master_vector);
      
   zipWithM(mkConnection, arbiter.clients, requests);
      
   Reg#(TLMId#(`TLM_TYPES)) count <- mkReg(0);
   
   FIFOF#(BusSwitchPath#(`TLM_TYPES)) addr_path_fifo <- mkBypassFIFOF;
   FIFO#(BusSwitchPath#(`TLM_TYPES))  resp_path_fifo <- mkBypassFIFO;
      
   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the address phase.
   ////////////////////////////////////////////////////////////////////////////////
   
   let addr_sends = map(getAxiRdMasterAddr, master_vector);
   let addr_recvs = map(getAxiRdSlaveAddr,  slave_vector);
   
   BusSwitch#(`TLM_TYPES) addr_switch <- mkBusSwitch(addr_sends, addr_recvs, False);
      
   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the response phase.
   ////////////////////////////////////////////////////////////////////////////////
   
   let resp_recvs = map(getAxiRdMasterResp, master_vector);
   let resp_sends = map(getAxiRdSlaveResp,  slave_vector);
   
   BusSwitch#(`TLM_TYPES)  resp_switch <- mkBusSwitch(resp_sends, resp_recvs, False);
      
   let requests_pending = (pack(map(getRequest, requests)) != 0);
   
   rule pre_select_path (requests_pending && addr_path_fifo.notFull);
      fixed <= False;
   endrule
      
   rule select_path (requests_pending);
      let master_port = arbiter.grant_id;
      let master      = master_vector[master_port];
      let addr        = master.addr.data.addr;
      let zow         = map(addrMatch(addr), slaves);
      let slave_port  = getIndex(map(addrMatch(addr), slaves));
      let path = BusSwitchPath {send_port: zExtend(master_port),
				recv_port: {0, slave_port},
				send_id:   getId(master.addr.data),
				recv_id:   count};
      addr_path_fifo.enq(path);
      count <= count + 1;
   endrule
      
   rule set_addr_path;
      addr_switch.set_path(addr_path_fifo.first);
   endrule

   rule set_resp_path;
      resp_switch.set_path(resp_path_fifo.first);
   endrule
   
   rule finish_addr (addr_switch.done);
      addr_switch.ack;
      let resp_path = reverseBusSwitchPath(addr_path_fifo.first);
      resp_path_fifo.enq(resp_path);
      addr_path_fifo.deq;
   endrule
   
   rule finish_resp (resp_switch.done);
      resp_switch.ack;
      resp_path_fifo.deq;
   endrule
      
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function BusSend#(AxiAddrCmd#(`TLM_TYPES)) getAxiRdMasterAddr (AxiRdBusMaster#(`TLM_TYPES) master);
   return master.addr;
endfunction

function BusRecv#(AxiRdResp#(`TLM_TYPES)) getAxiRdMasterResp (AxiRdBusMaster#(`TLM_TYPES) master);
   return master.resp;
endfunction

function BusRecv#(AxiAddrCmd#(`TLM_TYPES)) getAxiRdSlaveAddr (AxiRdBusSlave#(`TLM_TYPES) slave);
   return slave.addr;
endfunction

function BusSend#(AxiRdResp#(`TLM_TYPES)) getAxiRdSlaveResp (AxiRdBusSlave#(`TLM_TYPES) slave);
   return slave.resp;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Bool getRequest(ArbiterRequest_IFC ifc);
   return ifc.request;
endfunction

function Bool getGrant(ArbiterClient_IFC ifc); 
   return ifc.grant;
endfunction

function Bool addrMatch(AxiAddr#(`TLM_TYPES) addr, AxiRdFabricSlave#(`TLM_TYPES) ifc);
   return ifc.addrMatch(addr);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage
