// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision: 17899 $
// $Date: 2009-09-21 09:39:55 -0400 (Mon, 21 Sep 2009) $

package AxiWrBus;

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

module mkAxiWrBus#(Vector#(master_count, AxiWrFabricMaster#(`TLM_TYPES)) masters, 
		   Vector#(slave_count,  AxiWrFabricSlave#(`TLM_TYPES))  slaves) (Empty)
   provisos(Log#(master_count, size_m), 
	    Log#(slave_count, size_s),
	    Add#(ignore0, size_m, id_size),
	    Add#(ignore1, size_s, id_size));
   
   Wire#(Bool) fixed <- mkDWire(True);
   
   function AxiWrMaster#(`TLM_TYPES) master_bus_ifc(AxiWrFabricMaster#(`TLM_TYPES) ifc) = ifc.bus;
   function AxiWrSlave#(`TLM_TYPES)  slave_bus_ifc(AxiWrFabricSlave#(`TLM_TYPES) ifc)   = ifc.bus;
   
   let master_vector <- mapM(mkAxiWrBusMasterIFC, map(master_bus_ifc, masters));
   let slave_vector  <- mapM(mkAxiWrBusSlaveIFC,  map(slave_bus_ifc, slaves));
      
   Arbiter_IFC#(master_count) arbiter <- mkArbiter(fixed);
   Vector#(master_count, ArbiterRequest_IFC) requests <- mapM(mkArbiterRequest, master_vector);
      
   zipWithM(mkConnection, arbiter.clients, requests);
      
   Reg#(TLMId#(`TLM_TYPES)) count <- mkReg(0);
   
   FIFOF#(BusSwitchPath#(`TLM_TYPES)) addr_path_fifo <- mkBypassFIFOF;
   FIFO#(BusSwitchPath#(`TLM_TYPES))  data_path_fifo <- mkBypassFIFO;
   FIFO#(BusSwitchPath#(`TLM_TYPES))  resp_path_fifo <- mkBypassFIFO;
      
   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the address phase.
   ////////////////////////////////////////////////////////////////////////////////
   
   let addr_sends = map(getAxiWrMasterAddr, master_vector);
   let addr_recvs = map(getAxiWrSlaveAddr,  slave_vector);
   
   BusSwitch#(`TLM_TYPES) addr_switch <- mkBusSwitch(addr_sends, addr_recvs, False);
      
   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the data phase.
   ////////////////////////////////////////////////////////////////////////////////
   
   let data_sends = map(getAxiWrMasterData, master_vector);
   let data_recvs = map(getAxiWrSlaveData,  slave_vector);
   
   BusSwitch#(`TLM_TYPES)  data_switch <- mkBusSwitch(data_sends, data_recvs, False);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the response phase.
   ////////////////////////////////////////////////////////////////////////////////
   
   let resp_recvs = map(getAxiWrMasterResp, master_vector);
   let resp_sends = map(getAxiWrSlaveResp,  slave_vector);
   
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
   
   rule set_data_path;
      data_switch.set_path(data_path_fifo.first);
   endrule
   
   rule set_resp_path;
      resp_switch.set_path(resp_path_fifo.first);
   endrule
   
   rule finish_addr (addr_switch.done);
      addr_switch.ack;
      data_path_fifo.enq(addr_path_fifo.first);
      addr_path_fifo.deq;
   endrule
   
   rule finish_data (data_switch.done);
      data_switch.ack;
      let resp_path = reverseBusSwitchPath(data_path_fifo.first);
      resp_path_fifo.enq(resp_path);
      data_path_fifo.deq;
   endrule
   
   rule finish_resp (resp_switch.done);
      resp_switch.ack;
      resp_path_fifo.deq;
   endrule
      
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function BusSend#(AxiAddrCmd#(`TLM_TYPES)) getAxiWrMasterAddr (AxiWrBusMaster#(`TLM_TYPES) master);
   return master.addr;
endfunction

function BusSend#(AxiWrData#(`TLM_TYPES)) getAxiWrMasterData (AxiWrBusMaster#(`TLM_TYPES) master);
   return master.data;
endfunction

function BusRecv#(AxiWrResp#(`TLM_TYPES)) getAxiWrMasterResp (AxiWrBusMaster#(`TLM_TYPES) master);
   return master.resp;
endfunction

function BusRecv#(AxiAddrCmd#(`TLM_TYPES)) getAxiWrSlaveAddr (AxiWrBusSlave#(`TLM_TYPES) slave);
   return slave.addr;
endfunction

function BusRecv#(AxiWrData#(`TLM_TYPES)) getAxiWrSlaveData (AxiWrBusSlave#(`TLM_TYPES) slave);
   return slave.data;
endfunction

function BusSend#(AxiWrResp#(`TLM_TYPES)) getAxiWrSlaveResp (AxiWrBusSlave#(`TLM_TYPES) slave);
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

function Bool addrMatch(AxiAddr#(`TLM_TYPES) addr, AxiWrFabricSlave#(`TLM_TYPES) ifc);
   return ifc.addrMatch(addr);
endfunction
  
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage
