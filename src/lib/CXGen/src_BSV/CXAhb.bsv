package CXAhb;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BUtils::*;
import DefaultValue::*;
import DReg::*;
import FIFO::*;
import FIFOF::*;
import FIFOLevel::*;
import FShow::*;
import GetPut::*;
import Connectable::*;
import SpecialFIFOs::*;
import TLM3::*;
import CBus::*;
import Clocks::*;
import SceMi::*;
import Ahb::*;
import Xactors::*;
import XactorsCommon::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface CXAhbXtorMaster#(`TLM_PRM_DCL);
   (* result = "HADDR"  *) method AhbAddr#(`TLM_PRM) haddr;
   (* result = "HWDATA" *) method AhbData#(`TLM_PRM) hwdata;
   (* result = "HWRITE" *) method Bit#(1)            hwrite;
   (* result = "HTRANS" *) method Bit#(2)            htrans;
   (* result = "HBURST" *) method Bit#(3)            hburst;
   (* result = "HSIZE"  *) method Bit#(3)            hsize;
   (* result = "HPROT"  *) method Bit#(4)            hprot;
   (* prefix = "", result = "unused0" *) method Action hrdata((* port = "HRDATA" *) AhbData#(`TLM_PRM) data);
   (* prefix = "", result = "unused1" *) method Action hready((* port = "HREADY" *) Bit#(1)            value);
   (* prefix = "", result = "unused2" *) method Action hresp ((* port = "HRESP"  *) Bit#(2)            response);

   (* result = "HBUSREQ" *) method Bit#(1)           hbusreq;
   (* result = "HLOCK"   *) method Bit#(1)           hlock;
   (* prefix = "", result = "unused3" *) method Action hgrant((* port = "HGRANT" *) Bit#(1)            value);
endinterface

(* always_ready, always_enabled *)
interface CXAhbXtorMasterDual#(`TLM_PRM_DCL);
   (* prefix = "", result = "unused0" *) method Action haddr ((* port = "HADDR"  *) AhbAddr#(`TLM_PRM) addr);
   (* prefix = "", result = "unused1" *) method Action hwdata((* port = "HWDATA" *) AhbData#(`TLM_PRM) data);
   (* prefix = "", result = "unused2" *) method Action hwrite((* port = "HWRITE" *) Bit#(1)    value);
   (* prefix = "", result = "unused3" *) method Action htrans((* port = "HTRANS" *) Bit#(2) value);
   (* prefix = "", result = "unused4" *) method Action hburst((* port = "HBURST" *) Bit#(3)    value);
   (* prefix = "", result = "unused5" *) method Action hsize ((* port = "HSIZE"  *) Bit#(3)     value);
   (* prefix = "", result = "unused6" *) method Action hprot ((* port = "HPROT"  *) Bit#(4)     value);
   (* result = "HRDATA" *) method AhbData#(`TLM_PRM) hrdata;
   (* result = "HREADY" *) method Bit#(1)            hready;
   (* result = "HRESP"  *) method Bit#(2)            hresp;

   (* prefix = "", result = "unused7" *) method Action hbusreq((* port = "HBUSREQ" *) Bit#(1) value);
   (* prefix = "", result = "unused8" *) method Action hlock  ((* port = "HLOCK"   *) Bit#(1) value);
   (* result = "HGRANT" *) method Bit#(1)            hgrant;
endinterface

// =======================================================

(* always_ready, always_enabled *)
interface CXAhbXtorSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "unused0" *) method Action haddr   ((* port = "HADDR"  *) AhbAddr#(`TLM_PRM) addr);
   (* prefix = "", result = "unused1" *) method Action hwdata  ((* port = "HWDATA" *) AhbData#(`TLM_PRM) data);
   (* prefix = "", result = "unused2" *) method Action hwrite  ((* port = "HWRITE" *) Bit#(1)           value);
   (* prefix = "", result = "unused3" *) method Action htrans  ((* port = "HTRANS" *) Bit#(2)           value);
   (* prefix = "", result = "unused4" *) method Action hburst  ((* port = "HBURST" *) Bit#(3)           value);
   (* prefix = "", result = "unused5" *) method Action hsize   ((* port = "HSIZE"  *) Bit#(3)           value);
   (* prefix = "", result = "unused6" *) method Action hprot   ((* port = "HPROT"  *) Bit#(4)           value);
   (* prefix = "", result = "unused7" *) method Action hready  ((* port = "HREADY" *) Bit#(1)           value);
   (* prefix = "", result = "unused8" *) method Action hmast   ((* port = "HMASTER"*) Bit#(4)           value);
   (* result = "HRDATA" *)    method AhbData#(`TLM_PRM) hrdata;
   (* result = "HREADYOUT" *) method Bit#(1)            hreadyout;
   (* result = "HRESP"  *)    method Bit#(2)            hresp;
   (* result = "HSPLIT" *)    method Bit#(4)            hsplit;

   (* prefix = "", result = "unused9" *) method Action hsel    ((* port = "HSEL" *) Bit#(1)             value);
endinterface

(* always_ready, always_enabled *)
interface CXAhbXtorSlaveDual#(`TLM_PRM_DCL);
   (* result = "HADDR"  *) method AhbAddr#(`TLM_PRM) haddr;
   (* result = "HWDATA" *) method AhbData#(`TLM_PRM) hwdata;
   (* result = "HWRITE" *) method Bit#(1)            hwrite;
   (* result = "HTRANS" *) method Bit#(2)            htrans;
   (* result = "HBURST" *) method Bit#(3)            hburst;
   (* result = "HSIZE"  *) method Bit#(3)            hsize;
   (* result = "HPROT"  *) method Bit#(4)            hprot;
   (* result = "HREADY" *) method Bit#(1)            hready;
   (* result = "HMAST"  *) method Bit#(4)            hmast;
   (* prefix = "", result = "unused0" *) method Action hrdata((* port = "HRDATA" *) AhbData#(`TLM_PRM) data);
   (* prefix = "", result = "unused1" *) method Action hreadyout((* port = "HREADYOUT" *) Bit#(1)            value);
   (* prefix = "", result = "unused2" *) method Action hresp ((* port = "HRESP"  *) Bit#(2)            response);
   (* prefix = "", result = "unused3" *) method Action hsplit((* port = "HSPLIT" *) Bit#(4)            split);

   (* result = "HSEL"   *) method Bit#(1)            hsel;
endinterface

// =======================================================

typedef CXAhbXtorMaster#(`TLM_PRM) CXAhbMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXAhbXtorSlave#(`TLM_PRM)  CXAhbSlaveSceMiXactor#(`TLM_PRM_DCL);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSenderX#(Bool xtor_enable, t x_in)(t) provisos(Bits#(t,st));
   Reg#(t) r <- mkRegU;
   rule setX (xtor_enable);
      r <= x_in;
   endrule
   return r;
endmodule

typeclass CXify#(type t, type cxt)
   dependencies (t determines cxt);

   module mkCXify#(t ifc_in, Bool xtor_enable)(cxt);
endtypeclass

instance CXify#(AhbXtorMaster#(`TLM_PRM), CXAhbXtorMaster#(`TLM_PRM));
   module mkCXify#(AhbXtorMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAhbXtorMaster#(`TLM_PRM));
      let sx = mkSenderX(xtor_enable);

      let m_hbusreq <- sx(ifc_in.arbiter.hbusreq);
      let m_hlock   <- sx(ifc_in.arbiter.hlock);

      method haddr   = ifc_in.bus.haddr;
      method hwdata  = ifc_in.bus.hwdata;
      method hwrite  = pack(ifc_in.bus.hwrite);
      method htrans  = pack(ifc_in.bus.htrans);
      method hburst  = pack(ifc_in.bus.hburst);
      method hsize   = pack(ifc_in.bus.hsize);
      method hprot   = pack(ifc_in.bus.hprot);
      method hrdata  = compose(ifc_in.bus.hrdata, unpack);
      method hready  = compose(ifc_in.bus.hready, unpack);
      method hresp   = compose(ifc_in.bus.hresp, unpack);

      method hbusreq = pack(m_hbusreq);
      method hlock   = pack(m_hlock);
      method hgrant  = compose(ifc_in.arbiter.hgrant, unpack);
   endmodule
endinstance

instance CXify#(AhbSlaveSceMiXactor#(`TLM_PRM), CXAhbMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(AhbSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAhbMasterSceMiXactor#(`TLM_PRM));
      let _ifc <- mkCXify(ifc_in.bus, xtor_enable);
      return _ifc;
   endmodule
endinstance

instance CXify#(AhbXtorSlave#(`TLM_PRM), CXAhbXtorSlave#(`TLM_PRM));
   module mkCXify#(AhbXtorSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAhbXtorSlave#(`TLM_PRM));
      method haddr    = ifc_in.bus.haddr;
      method hwdata   = ifc_in.bus.hwdata;
      method hwrite   = compose(ifc_in.bus.hwrite, unpack);
      method htrans   = compose(ifc_in.bus.htrans, unpack);
      method hburst   = compose(ifc_in.bus.hburst, unpack);
      method hsize    = compose(ifc_in.bus.hsize, unpack);
      method hprot    = compose(ifc_in.bus.hprot, unpack);
      method hready   = compose(ifc_in.bus.hreadyin, unpack);
      method hmast    = compose(ifc_in.bus.hmast, unpack);
      method hrdata   = pack(ifc_in.bus.hrdata);
      method hreadyout= pack(ifc_in.bus.hready);
      method hresp    = pack(ifc_in.bus.hresp);
      method hsplit   = pack(ifc_in.bus.hsplit);

      method hsel     = compose(ifc_in.selector.select, unpack);
   endmodule
endinstance

instance CXify#(AhbMasterSceMiXactor#(`TLM_PRM), CXAhbSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(AhbMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXAhbSlaveSceMiXactor#(`TLM_PRM));
      let _ifc <- mkCXify(ifc_in.bus, xtor_enable);
      return _ifc;
   endmodule
endinstance

//////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////


module mkCAhbMasterIFC#(Bool xtor_enable, Bool grant, EBTMode ebt_mode) (AhbMasterIFC#(`TLM_PRM));

   Reg#(AhbRequest#(`TLM_PRM))   request_reg  <- mkReg(unpack(0));
   Wire#(AhbResponse#(`TLM_PRM)) response     <- mkWire;

   let request = request_reg;

   Wire#(AhbResp)            response_wire <- mkBypassWire;
   Wire#(AhbData#(`TLM_PRM)) rdata_wire    <- mkBypassWire;
   Wire#(Bool)               ready_wire    <- mkBypassWire;

   Wire#(AhbWrite)         command_wire  <- mkWire;

   FIFOF#(Maybe#(AhbWrite)) fifo_op       <- mkDFIFOF(Invalid);


   rule every (xtor_enable && ready_wire);
      let command = fifo_op.first;
      fifo_op.deq;
      let value = AhbResponse {data:    rdata_wire,
			       status:  response_wire,
			       command: command};
      response <= value;
   endrule

   rule do_enq(xtor_enable);
      fifo_op.enq(tagged Valid command_wire);
   endrule

   rule pre_enq (xtor_enable &&
		 (request.ctrl.transfer != IDLE) &&
		 (request.ctrl.transfer != BUSY) &&
		 ready_wire);
      command_wire <= request.ctrl.command;
   endrule

   rule detect_split_retry (xtor_enable && !ready_wire && (response_wire == RETRY || response_wire == SPLIT));
      let r = request;
      let ctrl = r.ctrl;
      ctrl.transfer = IDLE;
      r.ctrl = ctrl;
      request_reg <= r;
   endrule

   interface ReadOnly resp_current;
      method _read() if (xtor_enable) = response_wire;
   endinterface

   interface GetPut obj;
      method ActionValue#(AhbResponse#(`TLM_PRM)) getput (AhbRequest#(`TLM_PRM) value) if (xtor_enable && ready_wire);
	 let ctrl = value.ctrl;
	 let value2 = value;
	 ctrl.transfer = (!grant) ? IDLE : ctrl.transfer;
	 value2.ctrl = ctrl;
	 request_reg <= value2;
	 return(response);
      endmethod
   endinterface

   interface AhbMaster bus;
      // Outputs
      method haddr  = request.ctrl.addr;
      method hwdata = request.data;
      method hwrite = request.ctrl.command;
      method htrans = (ebt_mode != RUN && request.ctrl.transfer == BUSY) ? IDLE : request.ctrl.transfer;
      method hburst = request.ctrl.burst;
      method hsize  = request.ctrl.size;
      method hprot  = request.ctrl.prot;

      // Inputs
      method hrdata = rdata_wire._write;
      method hready = ready_wire._write;
      method hresp  = response_wire._write;
   endinterface
endmodule

// =======================================================

module mkCAhbMaster#(parameter UInt#(32) max_flight, Bool xtor_enable) (AhbMasterXActor#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)));

//   let depth = max(3, max_flight);
   let depth = 3; // don't ever need more than this.

   Wire#(AhbResponse#(`TLM_PRM))      response_wire <- mkWire;

   FIFOF#(req_t)                      fifo_rx    <- mkBypassFIFOF;
   FIFOLevelIfc#(resp_t, 5)           fifo_tx    <- mkFIFOLevel;

   Reg#(Bool)                         req_wire   <- mkDWire(False);
   Reg#(Bool)                         req_reg    <- mkReg(False);

   Reg#(Maybe#(RequestDescriptor#(`TLM_PRM))) descriptor <- mkReg(Invalid);
   Reg#(TLMBLength#(`TLM_PRM))                count      <- mkReg(0);
   Reg#(Bool)                                 lock_reg   <- mkReg(False);


   Wire#(Bool)                                grant_wire <- mkBypassWire;
   Reg#(Bool)                                 grant_reg  <- mkReg(False);
   Wire#(Bool)                                stall_wire <- mkDWire(False);
   Reg#(Maybe#(AhbData#(`TLM_PRM)))           data_reg   <- mkReg(Invalid);
   Reg#(Maybe#(AhbData#(`TLM_PRM)))           data_prev  <- mkReg(Invalid);

   CFIFO#(UInt#(TAdd#(SizeOf#(TLMBLength#(`TLM_PRM)), 1))) length_fifo <- mkCFIFO(depth);
   FIFO#(TLMId#(`TLM_PRM))                           id_fifo           <- mkDepthParamFIFO(depth);
   Reg#(EBTMode)                                     ebt_mode          <- mkReg(RUN);

   let ifc <- mkCAhbMasterIFC(xtor_enable, grant_wire, ebt_mode);

   let rx_first = toTLMRequest(fifo_rx.first);

   rule update_grant(xtor_enable);
      grant_reg <= grant_wire;
   endrule

   rule send_request (xtor_enable &&& rx_first matches tagged Descriptor .d &&&
		      assertLock(d) == lock_reg &&&
		      !(count == 0 && stall_wire));
      req_wire <= True;
   endrule

   rule set_lock (xtor_enable &&& rx_first matches tagged Descriptor .d &&&
	     assertLock(d) != lock_reg &&&
             count == 0);
      lock_reg <= assertLock(d);
   endrule

   Reg#(Bit#(3)) in_flight <- mkReg(0);
   PulseWire     do_incr <- mkPulseWire;
   PulseWire     do_decr <- mkPulseWire;

   Bit#(3) in_flight_current =
   case (tuple2(do_incr, do_decr)) matches
	 {False, False} : in_flight;
	 {False, True } : (in_flight - 1);
	 {True,  False} : (in_flight + 1);
	 {True,  True } : in_flight;
   endcase;


   Reg#(Maybe#(AhbRequest#(`TLM_PRM))) req_prev <- mkReg(tagged Invalid);

   Bool early_end = /*!grant_wire &&*/ count != 0 && ebt_mode == RUN;
   Bool can_run = in_flight == 0 && ebt_mode != REDO_RETRY && ebt_mode != REDO_EBT;

   PulseWire split_retry <- mkPulseWire;
   PulseWire last_pw     <- mkPulseWire;

   rule detect_ebt (xtor_enable && early_end && !split_retry && !last_pw);
      ebt_mode <= (in_flight_current == 0) ? WIND_DOWN : REDO_EBT;
   endrule

   rule detect_split_retry (xtor_enable && split_retry);
      ebt_mode <= REDO_RETRY;
   endrule

   rule ebt_finish (xtor_enable && last_pw && !split_retry);
      ebt_mode <= RUN;
   endrule

   rule incr (xtor_enable && do_incr && ! do_decr);
      in_flight <= in_flight + 1;
   endrule

   rule decr (xtor_enable && !do_incr && do_decr);
      in_flight <= in_flight - 1;
   endrule

   (* preempts = "(redo_op, start_op, write_op, read_op), (idle_op, stall_op)" *)
   rule redo_op (xtor_enable &&&
		 req_prev matches tagged Valid .r &&&
		 !stall_wire &&&
		 grant_wire &&&
		 (ebt_mode == REDO_EBT || ebt_mode == REDO_RETRY));
      let ctrl = r.ctrl;
      let data = r.data;
      ctrl.burst = SINGLE;
      ctrl.transfer = NONSEQ;
      data_reg <= data_prev;
/* -----\/----- EXCLUDED -----\/-----
      let data = ?;
      if (data_reg matches tagged Valid .d) data = d;
      data_reg <= tagged Valid r.data;
 -----/\----- EXCLUDED -----/\----- */
      let ahb_request = AhbRequest { ctrl: ctrl, data: data};
//      req_prev <= tagged Valid ahb_request;
      let response <- ifc.obj.getput(ahb_request);
      ebt_mode <= WIND_DOWN;
      if (ebt_mode == REDO_RETRY) do_incr.send;
   endrule

//   (* preempts = "(start_op, write_op, read_op), (idle_op, stall_op)" *)
   rule start_op (xtor_enable &&&
		  rx_first matches tagged Descriptor .d &&&
		  (assertLock(d) == lock_reg) &&&
		  count == 0 &&&
		  !stall_wire &&&
		  can_run &&&
		  grant_wire);
      let next = incrTLMAddr(d);
      descriptor <= tagged Valid next;
      length_fifo.enq(extendNP(d.b_length) + 1);
      id_fifo.enq(d.transaction_id);
      let remaining = d.b_length;
      count <= remaining;
      let ctrl = getAhbCtrl(d);
      ctrl.transfer = NONSEQ;
      let data = ?;
      if (data_reg matches tagged Valid .d) data = d;
      data_reg <= tagged Valid getAhbData(d);
      let ahb_request = AhbRequest { ctrl: ctrl, data: data};
      req_prev <= tagged Valid ahb_request;
      data_prev <= data_reg;
      let response <- ifc.obj.getput(ahb_request);
      response_wire <= response;
      fifo_rx.deq;
      req_reg <= (ctrl.burst == INCR) && (remaining > 0);
      do_incr.send;
   endrule

   rule write_op (xtor_enable &&&
		  rx_first matches tagged Data .d &&&
		  descriptor matches tagged Valid .des &&&
		  des.command == WRITE &&&
		  count > 0 &&&
		  !stall_wire &&&
		  can_run &&&
		  grant_wire);
      let remaining = count - 1;
      count <= remaining;
      let next = incrTLMAddr(des);
      descriptor <= tagged Valid next;
      let ctrl = getAhbCtrl(des);
      ctrl.transfer = (getAhbBurst(des) == SINGLE) ? NONSEQ : SEQ;
      let data = ?;
      if (data_reg matches tagged Valid .d) data = d;
      data_reg <= tagged Valid d.data;
      let ahb_request = AhbRequest { ctrl: ctrl, data: data};
      req_prev <= tagged Valid ahb_request;
      do_incr.send;
      data_prev <= data_reg;
      if (ebt_mode == WIND_DOWN)
	 begin
	    ctrl.burst    = SINGLE;
	    ctrl.transfer = NONSEQ;
	    ahb_request = AhbRequest { ctrl: ctrl, data: data};
	 end
      let response <- ifc.obj.getput(ahb_request);
      response_wire <= response;
      fifo_rx.deq;
      req_reg <= (ctrl.burst == INCR) && (remaining > 0);
   endrule

   rule read_op (xtor_enable &&&
		 descriptor matches tagged Valid .des &&&
		 des.command == READ &&&
		 count > 0 &&&
		 !stall_wire &&&
		 can_run &&&
		 grant_wire);
      let remaining = count - 1;
      count <= remaining;
      let next = incrTLMAddr(des);
      descriptor <= tagged Valid next;
      let ctrl = getAhbCtrl(des);
      ctrl.transfer = (getAhbBurst(des) == SINGLE) ? NONSEQ : SEQ;
      let data = ?;
      if (data_reg matches tagged Valid .d) data = d;
      data_reg <= tagged Valid 0;
      let ahb_request = AhbRequest { ctrl: ctrl, data: data};
      req_prev <= tagged Valid ahb_request;
      do_incr.send;
      data_prev <= data_reg;
      if (ebt_mode == WIND_DOWN)
	 begin
	    ctrl.burst    = SINGLE;
	    ctrl.transfer = NONSEQ;
	    ahb_request = AhbRequest { ctrl: ctrl, data: data};
	 end
      let response <- ifc.obj.getput(ahb_request);
      response_wire <= response;
      req_reg <= (ctrl.burst == INCR) && (remaining > 0);
   endrule

   rule idle_op (xtor_enable &&& data_reg matches tagged Valid .x);
      let ctrl = unpack(0);
      if (descriptor matches tagged Valid .des) ctrl = getAhbCtrl(des);
      if (descriptor matches tagged Valid .des &&& (getAhbBurst(des) == SINGLE))
	 ctrl.transfer = IDLE;
      else
	 ctrl.transfer = (count == 0 || early_end) ? IDLE : BUSY;
      let data = ?;
      if (data_reg matches tagged Valid .d) data = d;
      data_reg <= tagged Invalid;
      data_prev <= data_reg;
      if (ebt_mode == WIND_DOWN)
	 begin
	    ctrl.burst    = SINGLE;
	    ctrl.transfer = IDLE;
	 end
      let response <- ifc.obj.getput(AhbRequest { ctrl: ctrl, data: data});
      response_wire <= response;
      req_reg <= (ctrl.transfer == IDLE) ? False : req_reg;
   endrule

   rule stall_op (xtor_enable &&& data_reg matches tagged Invalid);
      let ctrl = unpack(0);
      if (descriptor matches tagged Valid .des) ctrl = getAhbCtrl(des);
      if (descriptor matches tagged Valid .des &&& (getAhbBurst(des) == SINGLE))
	 ctrl.transfer = IDLE;
      else
	 ctrl.transfer = (count == 0 || early_end) ? IDLE : BUSY;
      if (ebt_mode == WIND_DOWN)
	 begin
	    ctrl.burst    = SINGLE;
	    ctrl.transfer = IDLE;
	 end
      let response <- ifc.obj.getput(AhbRequest { ctrl: ctrl, data: ?});
      response_wire <= response;
      req_reg <= (ctrl.transfer == IDLE) ? False : req_reg;
   endrule

   (* aggressive_implicit_conditions *)
   rule grab_valid_response (xtor_enable &&&
			     response_wire.command matches tagged Valid .c &&&
			     (length_fifo.first || fromAhbWrite(c) == READ));
      let is_last = length_fifo.first;
      let value = response_wire;
      TLMResponse#(`TLM_PRM) response = defaultValue ;
      response.data    = value.data;
      response.status  = fromAhbResp(value.status);
      response.command = fromAhbWrite(c);
      response.transaction_id = id_fifo.first;
      response.is_last = is_last;
      case (value.status)
	 SPLIT: begin
		   TLMErrorCode code = SPLIT;
		   response.data   = extendNP(pack(code));
		   split_retry.send;
		end
	 RETRY: begin
		   TLMErrorCode code = RETRY;
		   response.data   = extendNP(pack(code));
		   split_retry.send;
		end
	 ERROR: begin
		   TLMErrorCode code = SLVERR;
		   response.data   = extendNP(pack(code));
		end
	 default: begin // OKAY
		     response.data   = response.data;
		  end
      endcase
      if (value.status != SPLIT && value.status != RETRY)
	 begin
	    if(is_last) id_fifo.deq;
	    length_fifo.deq;
	    fifo_tx.enq(fromTLMResponse(response));
	 end
      do_decr.send;
      if (is_last) last_pw.send;
   endrule

   (* aggressive_implicit_conditions *)
   rule grab_valid_response_skip (xtor_enable &&& response_wire.command matches tagged Valid .c
				  &&& !(length_fifo.first || fromAhbWrite(c) == READ));

      let value = response_wire;
      TLMResponse#(`TLM_PRM) response = defaultValue ;
      response.data    = value.data;
      response.status  = fromAhbResp(value.status);
      response.command = fromAhbWrite(c);
      response.transaction_id = id_fifo.first;
      response.is_last = False;
      case (value.status)
	 SPLIT: begin
		   TLMErrorCode code = SPLIT;
		   response.data   = extendNP(pack(code));
		   split_retry.send;
		end
	 RETRY: begin
		   TLMErrorCode code = RETRY;
		   response.data   = extendNP(pack(code));
		   split_retry.send;
		end
	 ERROR: begin
		   TLMErrorCode code = SLVERR;
		   response.data   = extendNP(pack(code));
		end
	 default: begin // OKAY
		     response.data   = response.data;
		  end
      endcase
      do_decr.send;
      if (value.status != SPLIT && value.status != RETRY)
	 length_fifo.deq;
   endrule

   rule grab_invalid_response (xtor_enable &&& response_wire.command matches tagged Invalid);
      dummyAction;
   endrule


   rule stall (xtor_enable && fifo_tx.isGreaterThan(1) && (count != 1));
      stall_wire <= True;
   endrule

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface AhbXtorMaster fabric;
      interface AhbMaster bus = ifc.bus;
      interface AhbMasterArbiter arbiter;
	 method hbusreq = (req_wire || req_reg || ebt_mode != RUN);
	 method hlock   = lock_reg;
	 method hgrant  = grant_wire._write;
      endinterface
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkCAhbSlaveIFC#(Bool xtor_enable, Bool selected) (AhbSlaveIFC#(`TLM_PRM));

   Wire#(AhbData#(`TLM_PRM)) wdata_wire    <- mkDWire(?);
   Wire#(AhbWrite)           write_wire    <- mkDWire(?);
   Wire#(AhbSize)            size_wire     <- mkDWire(?);
   Wire#(AhbBurst)           burst_wire    <- mkDWire(?);
   Wire#(AhbTransfer)        transfer_wire <- mkDWire(?);
   Wire#(AhbProt)            prot_wire     <- mkDWire(?);
   Wire#(AhbAddr#(`TLM_PRM)) addr_wire     <- mkDWire(?);
   Wire#(AhbSplit)           mast_wire     <- mkDWire(?);
   Wire#(Bool)               readyin_wire  <- mkDWire(?);

   let dflt = AhbResponse {status:  OKAY,
                                    data:    truncateNP(1024'h123),
                                    command: tagged Invalid};

   Wire#(AhbResponse#(`TLM_PRM)) response_wire <- mkDWire(dflt);
   Wire#(Bool)                   ready         <- mkDWire(False);

   Reg#(Bool)                    select_reg    <- mkReg(False);

   rule update_select (xtor_enable && readyin_wire);
      select_reg <= selected;
   endrule

   interface Put response;
      method Action put (AhbResponse#(`TLM_PRM) value) if (xtor_enable && select_reg);
         response_wire <= value;
         ready         <= True;
      endmethod
   endinterface

   interface Put response_noready;
      method Action put (AhbResponse#(`TLM_PRM) value) if (xtor_enable && select_reg);
         response_wire <= value;
      endmethod
   endinterface

   interface ReadOnly request;
      method AhbRequest#(`TLM_PRM) _read;
         let ctrl = AhbCtrl {command:  write_wire,
                             size:     size_wire,
                             burst:    burst_wire,
                             transfer: transfer_wire,
                             prot:     prot_wire,
                             addr:     addr_wire};
         let value = AhbRequest {ctrl: ctrl, data: wdata_wire};
         return value;
      endmethod
   endinterface

   interface ReadOnly hmaster;
      method AhbSplit _read;
         return mast_wire;
      endmethod
   endinterface

   interface ReadOnly readyin;
      method _read = readyin_wire;
   endinterface

   interface AhbSlave bus;
      // Outputs
      method hrdata = response_wire.data;
      method hresp  = response_wire.status;
      method hsplit = 0;
//      method hready = ready;
      method hready = ready || !select_reg; // added by DB 8/26/2011 (to make assertions happy)
	                                    // I'm unsure why this is safe.

      // Inputs
      method haddr    = addr_wire._write;
      method hwdata   = wdata_wire._write;
      method hwrite   = write_wire._write;
      method hburst   = burst_wire._write;
      method htrans   = transfer_wire._write;
      method hsize    = size_wire._write;
      method hprot    = prot_wire._write;
      method hreadyin = readyin_wire._write;
      method hmast    = mast_wire._write;
   endinterface

endmodule

// =======================================================

module mkAhbSlaveP#(Bool xtor_enable,
		    Bool bypass_write_response,
		    Bool allow_early_reads,
		    Bool keep_bursts,
		    Bool big_endian,
                    FIFOF#(resp_t) fifo_rx,
                    FIFOF#(req_t)  fifo_tx) (AhbSlaveXActor#(`TLM_XTR))
   provisos(Bits#(resp_t, s0),
            DefaultValue#(TLMResponse#(`TLM_PRM)),
            TLMRequestTC#(req_t, `TLM_PRM),
            TLMResponseTC#(resp_t, `TLM_PRM));

   Reg#(Maybe#(AhbMastCtrl#(`TLM_PRM))) ctrl_reg    <- mkReg(Invalid);

   Wire#(Bool)             select_wire <- mkDWire(?);
   Reg#(Bool)              select_reg  <- mkReg(False);

//   FIFOF#(Bit#(0))         fifo_op     <- mkLFIFOF;
   MFIFO2#(TLMBLength#(`TLM_PRM), AhbSplit) fifo_op     <- mkMFIFO2;

   PulseWire                sampling    <- mkPulseWire;

   Reg#(TLMBLength#(`TLM_PRM))     count          <- mkReg(0);
   Reg#(Bool)               first_request  <- mkReg(True);
   Reg#(Bool)               first_response <- mkReg(True);

   let ifc <- mkAhbSlaveIFC(select_wire);

   let request = ifc.request;
   let ctrl_current = toAhbMastCtrl(request.ctrl);
   ctrl_current.mast = ifc.hmaster;


   let fifo_rx_write   = fifo_rx;
   let fifo_op_bypass  = ?;

   if (bypass_write_response)
      begin
         FIFOF#(resp_t)         fifo_write      <- mkFIFOF;
	 MFIFO#(TLMBLength#(`TLM_PRM)) fifo_op_bypass_ <- mkBypassMFIFO;

         fifo_rx_write   = fifo_write;
         fifo_op_bypass  = fifo_op_bypass_;
      end

   rule update_select (xtor_enable && ifc.readyin);
      select_reg <= select_wire;
   endrule

   rule grab_response (xtor_enable &&&
		       ctrl_reg matches tagged Valid .ctrl_prev &&&
                       ((ctrl_prev.transfer == SEQ) || (ctrl_prev.transfer == NONSEQ)));
      let tlm_response = toTLMResponse(fifo_rx.first);
      AhbResponse#(`TLM_PRM) ahb_response = fromTLMResponse(tlm_response);
      if (tlm_response.status == ERROR && first_response)
	 begin
	    first_response <= False;
	    ifc.response_noready.put(ahb_response);
	 end
      else
	 begin
	    first_response <= True;
	    ifc.response.put(ahb_response);
	    fifo_rx.deq;
	    let early = fifo_op.first;
	    ctrl_reg <= (select_wire) ? tagged Valid ctrl_current : tagged Invalid;
	    fifo_op.deq;
	    sampling.send;
	    TLMErrorCode code = unpack(truncateNP(tlm_response.data));
	    if (tlm_response.status == ERROR && code == SPLIT)
	       $display("(%0d) MASTER IS: ", $time, fifo_op.first);
	 end
   endrule

   if (bypass_write_response) begin

      (* preempts = "grab_response, grab_write_bypass_response" *)
      rule grab_write_bypass_response (xtor_enable &&&
				 ctrl_reg matches tagged Valid .ctrl_prev &&&
                                 ((ctrl_prev.transfer == SEQ) || (ctrl_prev.transfer == NONSEQ)));
         let ahb_response = AhbResponse {status:  OKAY,
                                         data:    0,
                                         command: tagged Invalid};
         ifc.response.put(ahb_response);
         ctrl_reg <= (select_wire) ? tagged Valid ctrl_current : tagged Invalid;
         fifo_op_bypass.deq;
      endrule

      rule grab_write_delayed_response(xtor_enable);
         TLMResponse#(`TLM_PRM) response = toTLMResponse(fifo_rx_write.first);
/*if (response.status != SUCCESS )
	    $display("ERROR: unexpected error in WRITE response."); */
         fifo_rx_write.deq;
      endrule

   end

   rule send_read_request (xtor_enable &&&
			   ctrl_reg matches tagged Valid .ctrl_prev &&&
                           ctrl_prev.command == READ &&&
                           select_reg &&&
                           ((ctrl_prev.transfer == SEQ) || (ctrl_prev.transfer == NONSEQ)));

      let desc = fromAhbCtrl(fromAhbMastCtrl(ctrl_prev));
      desc.data = request.data;
      let remaining = (count == 0) ? desc.b_length : (count - 1);
      count <= remaining;
      let keep = desc.b_length != 0 && keep_bursts;
      let is_last = remaining == 0;
      first_request <= is_last;
      desc.transaction_id = extendNP(ctrl_prev.mast);
      //	 do_incr.send;
      fifo_op.enq(1, ctrl_prev.mast);
      if (!keep)
	 begin
	    desc.b_length     = 0;
	    desc.byte_enable  = tagged Specify getTLMByteEn(big_endian, desc);
	    desc.mark = (is_last) ? LAST : NOT_LAST;
	    fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));

	    //	    fifo_op.enq(ctrl_prev.mast);
	 end
      else
	 begin
	    desc.transaction_id = extendNP(ctrl_prev.mast);
	    desc.mark = OPEN;
	    if (first_request)
	       fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
	 end
   endrule

   rule send_write_request (xtor_enable &&&
			    ctrl_reg matches tagged Valid .ctrl_prev &&&
                            ctrl_prev.command == WRITE &&&
			    select_reg &&&
                            ((ctrl_prev.transfer == SEQ) || (ctrl_prev.transfer == NONSEQ)));
      let desc = fromAhbCtrl(fromAhbMastCtrl(ctrl_prev));
      let keep = desc.b_length != 1 && keep_bursts;
      desc.data = request.data;
      desc.burst_mode   = INCR;
      desc.transaction_id = extendNP(ctrl_prev.mast);
      if (!keep)
	 begin
	    desc.b_length = 0;
	    desc.byte_enable  = tagged Specify getTLMByteEn(big_endian, desc);
	 end
      RequestData#(`TLM_PRM) data = ?;
      data.data = request.data;
      data.transaction_id = desc.transaction_id;
      if (count == 0)
	 begin

	    count <= desc.b_length;
	    fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
	 end
      else
	 begin
	    count <= count - 1;
	    if (keep)
	       fifo_tx.enq(fromTLMRequest(tagged Data data));
	    else
	       fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
	 end
      if (bypass_write_response)
	 fifo_op_bypass.enq(1);
      else
         fifo_op.enq(1, ctrl_prev.mast);
   endrule

   rule default_response (xtor_enable &&&
			  ctrl_reg matches tagged Valid .ctrl_prev &&&
			  ((ctrl_prev.transfer == IDLE) || (ctrl_prev.transfer == BUSY)));

      let ahb_response = AhbResponse {status:  OKAY,
                                      data:    truncateNP(1024'h123),
                                      command: tagged Invalid};
      ifc.response.put(ahb_response);
      ctrl_reg <= (select_wire) ? tagged Valid ctrl_current : tagged Invalid;
      sampling.send;
   endrule

   rule grab_ctrl (xtor_enable &&& ctrl_reg matches tagged Invalid);
      ctrl_reg <= (select_wire) ? tagged Valid ctrl_current : tagged Invalid;
   endrule

   if (allow_early_reads) begin

      (* preempts = "(send_write_request, send_read_request), first_early_read" *)
      rule first_early_read (xtor_enable &&&
			     ctrl_current.command == READ &&&
                             select_wire &&&
                             !select_reg &&&
                             ifc.readyin &&&
                             ((ctrl_current.transfer == SEQ) || (ctrl_current.transfer == NONSEQ)));

	 let desc = fromAhbCtrl(fromAhbMastCtrl(ctrl_current));
         desc.data = request.data;
	 let remaining = (count == 0) ? desc.b_length : (count - 1);
	 count <= remaining;
         desc.b_length     = 0;
	 desc.byte_enable  = tagged Specify getTLMByteEn(big_endian, desc);
	 desc.transaction_id = extendNP(ctrl_current.mast);
	 desc.mark = (remaining == 0) ? LAST : NOT_LAST;
         fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
         fifo_op.enq(1, ctrl_current.mast);
      endrule

      (* preempts = "(send_write_request, send_read_request), early_read_request" *)
      rule early_read_request (xtor_enable &&&
			       ctrl_current.command == READ &&&
			       select_wire &&&
			       select_reg &&&
			       sampling   &&&
			       ((ctrl_current.transfer == SEQ) || (ctrl_current.transfer == NONSEQ)));

	 let desc = fromAhbCtrl(fromAhbMastCtrl(ctrl_current));
         desc.data = request.data;
	 let remaining = (count == 0) ? desc.b_length : (count - 1);
	 count <= remaining;
         desc.b_length     = 0;
	 desc.byte_enable  = tagged Specify getTLMByteEn(big_endian, desc);
	 desc.transaction_id = extendNP(ctrl_current.mast);
	 desc.mark = (remaining == 0) ? LAST : NOT_LAST;
         fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
         fifo_op.enq(1, ctrl_current.mast);
      endrule
   end

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx;
         method Action put (value);
            let response = toTLMResponse(value);
            case (response.command)
               READ:    fifo_rx.enq(value);
               WRITE:   fifo_rx_write.enq(value);
               UNKNOWN: $display("(%0d) mkAhbSlave (%m): Unhandled case.", $time);
            endcase
         endmethod
      endinterface
   endinterface

   interface AhbXtorSlave fabric;
      interface AhbSlave bus = ifc.bus;
      interface AhbSlaveSelector selector;
         method select = select_wire._write;
      endinterface
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkCAhbMasterSceMiXactor (TLMXActorArgs args,
					      Bool xtor_enable,
					      AhbSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
///////////// (args, cclk, crst_n) = (args, _streamX.uclock, _streamX.ureset);
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   AhbMasterXActor#(`TLM_XTR) master_xactor <- mkCAhbMaster( args.max_flight, xtor_enable);
   let master_tlm    <- addFlowControl(args.flow_depth, master_xactor.tlm);
   ApiRecvIFC#(`TLM_RR) receiver <- mkApiReceiverXL (args.max_flight, -1
						     , clocked_by _streamX.uclock
						     , reset_by _streamX.ureset);
   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, _streamX.uclock, _streamX.ureset, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, _streamX.uclock);

   mkConnection(master_tlm.tx, toPut(response_fifo));
   mkConnection(master_tlm.rx, toGet(request_fifo));

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));
////////////
   //AhbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAhbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, receiver.scemi);

   interface bus = master_xactor.fabric;
endmodule

module [SceMiModule] mkCXAhbMasterSceMiXactor#(TLMXActorArgs args, Bool xtor_enable) (CXAhbMasterSceMiXactor#(`TLM_PRM))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)));
   let _ifc0 <- mkCAhbMasterSceMiXactor (args, xtor_enable);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

// =======================================================

module [SceMiModule] mkCAhbSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               AhbMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
/////// (args, cclk, crst_n) = (args, _streamX.uclock, _streamX.ureset)
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   FIFOF#(resp_t) fifo_rx <- mkSizedFIFOF(1);
   FIFOF#(req_t)  fifo_tx <- mkSizedFIFOF(1);
   let slave_xactor <- mkAhbSlaveP(xtor_enable,
				   args.write_bypass, True, args.keep_bursts, args.big_endian, fifo_rx, fifo_tx);
   // "True" above allows early reads

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by _streamX.uclock, reset_by _streamX.ureset);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, _streamX.uclock);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, _streamX.uclock, _streamX.ureset, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   mkConnection(toGet(response_fifo), slave_xactor.tlm.rx);
   mkConnection(slave_xactor.tlm.tx, toPut(request_fifo));
///////
   //AhbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkAhbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, sender.scemi);

   interface bus = slave_xactor.fabric;
endmodule

module [SceMiModule] mkCXAhbSlaveSceMiXactor#(TLMXActorArgs args, Bool xtor_enable) (CXAhbSlaveSceMiXactor#(`TLM_PRM))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)));
   let _ifc0 <- mkCAhbSlaveSceMiXactor (args, xtor_enable);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

endpackage
