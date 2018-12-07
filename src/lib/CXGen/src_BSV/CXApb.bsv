package CXApb;

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
import Apb::*;
import Xactors::*;
import XactorsCommon::*;
import Counter          ::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface CXApbXtorSlave#(`TLM_PRM_DCL);
   (* prefix = "", result = "PREADY"  *) method Bit#(1)                            pready;
   (* prefix = "", result = "PRDATA"  *) method ApbData#(`TLM_PRM)                 prdata;
   (* prefix = "", result = "PSLVERR" *) method Bit#(1)                           pslverr;
   (* prefix = "" *) method Action paddr  ((* port = "PADDR"   *) ApbAddr#(`TLM_PRM)   x);
   (* prefix = "" *) method Action pprot  ((* port = "PPROT"   *) Bit#(3)              x);
   (* prefix = "" *) method Action penable((* port = "PENABLE" *) Bit#(1)              x);
   (* prefix = "" *) method Action pwrite ((* port = "PWRITE"  *) Bit#(1)              x);
   (* prefix = "" *) method Action pwdata ((* port = "PWDATA"  *) ApbData#(`TLM_PRM)   x);
   (* prefix = "" *) method Action pstrb  ((* port = "PSTRB"   *) ApbByteEn#(`TLM_PRM) x);
   (* prefix = "" *) method Action psel   ((* port = "PSEL"    *) Bit#(1)              x);
endinterface

(* always_ready, always_enabled *)
interface CXApbXtorMaster#(`TLM_PRM_DCL);
   (* prefix = "", result = "PADDR"   *) method ApbAddr#(`TLM_PRM)                paddr;
   (* prefix = "", result = "PPROT"   *) method Bit#(3)                           pprot;
   (* prefix = "", result = "PENABLE" *) method Bit#(1)                         penable;
   (* prefix = "", result = "PWRITE"  *) method Bit#(1)                          pwrite;
   (* prefix = "", result = "PWDATA"  *) method ApbData#(`TLM_PRM)               pwdata;
   (* prefix = "", result = "PSTRB"   *) method ApbByteEn#(`TLM_PRM)              pstrb;
   (* prefix = "", result = "PSEL"    *) method Bit#(1)                            psel;
   (* prefix = "" *) method Action pready((* port = "PREADY"   *) Bit#(1)            x);
   (* prefix = "" *) method Action prdata((* port = "PRDATA"   *) ApbData#(`TLM_PRM) x);
   (* prefix = "" *) method Action pslverr((* port = "PSLVERR" *) Bit#(1)            x);
endinterface

// =======================================================

typedef CXApbXtorSlave#(`TLM_PRM)  CXApbMasterSceMiXactor#(`TLM_PRM_DCL);
typedef CXApbXtorMaster#(`TLM_PRM) CXApbSlaveSceMiXactor#(`TLM_PRM_DCL);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass CXify#(type t, type cxt)
   dependencies (t determines cxt);
   module mkCXify#(t ifc_in, Bool xtor_enable)(cxt);
endtypeclass

instance CXify#(ApbXtorSlave#(`TLM_PRM), CXApbXtorSlave#(`TLM_PRM));
   module mkCXify#(ApbXtorSlave#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXApbXtorSlave#(`TLM_PRM));
      method pready  = pack(ifc_in.bus.pready);
      method prdata  = pack(ifc_in.bus.prdata);
      method pslverr = pack(ifc_in.bus.pslverr);
      method paddr   = ifc_in.bus.paddr;
      method pprot   = compose(ifc_in.bus.pprot, unpack);
      method penable = compose(ifc_in.bus.penable, unpack);
      method pwrite  = compose(ifc_in.bus.pwrite, unpack);
      method pwdata  = ifc_in.bus.pwdata;
      method pstrb   = ifc_in.bus.pstrb;
      method psel    = compose(ifc_in.bus.psel, unpack);
   endmodule
endinstance

instance CXify#(ApbXtorMaster#(`TLM_PRM), CXApbXtorMaster#(`TLM_PRM));
   module mkCXify#(ApbXtorMaster#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXApbXtorMaster#(`TLM_PRM));
      method paddr   = ifc_in.bus.paddr;
      method pprot   = pack(ifc_in.bus.pprot);
      method penable = pack(ifc_in.bus.penable);
      method pwrite  = pack(ifc_in.bus.pwrite);
      method pwdata  = ifc_in.bus.pwdata;
      method pstrb   = ifc_in.bus.pstrb;
      method psel    = pack(ifc_in.bus.psel);
      method pready  = compose(ifc_in.bus.pready, unpack);
      method prdata  = ifc_in.bus.prdata;
      method pslverr = compose(ifc_in.bus.pslverr, unpack);
   endmodule
endinstance

instance CXify#(ApbMasterSceMiXactor#(`TLM_PRM), CXApbMasterSceMiXactor#(`TLM_PRM));
   module mkCXify#(ApbMasterSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXApbMasterSceMiXactor#(`TLM_PRM));
      let _ifc <- mkCXify(ifc_in.bus, xtor_enable);
      return _ifc;
   endmodule
endinstance

instance CXify#(ApbSlaveSceMiXactor#(`TLM_PRM), CXApbSlaveSceMiXactor#(`TLM_PRM));
   module mkCXify#(ApbSlaveSceMiXactor#(`TLM_PRM) ifc_in, Bool xtor_enable)(CXApbSlaveSceMiXactor#(`TLM_PRM));
      let _ifc <- mkCXify(ifc_in.bus, xtor_enable);
      return _ifc;
   endmodule
endinstance

//////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
module mkCApbMaster#(parameter Bool big_endian, Bool xtor_enable) (ApbMasterXActor#(`TLM_XTR))
   provisos (TLMRequestTC#(req_t, `TLM_PRM),
	     TLMResponseTC#(resp_t, `TLM_PRM),
	     Bits#(req_t, s0),
	     Bits#(resp_t, s1),
	     Bits#(RequestDescriptor#(`TLM_PRM), s2)
	     );

  ////////////////////////////////////////////////////////////////////////////////
  /// TLM Design Elements
  ////////////////////////////////////////////////////////////////////////////////
  FIFOF#(req_t)                            fTlmReqs        <- mkBypassFIFOF;
  FIFOF#(resp_t)                           fTlmRsps        <- mkLFIFOF;

  ////////////////////////////////////////////////////////////////////////////////
  /// Apb Design Elements
  ////////////////////////////////////////////////////////////////////////////////
  Reg#(ApbState)                           rState          <- mkReg(IDLE);

  Reg#(RequestDescriptor#(`TLM_PRM))       rDescriptor     <- mkRegU;
  Counter#(SizeOf#(TLMBLength#(`TLM_PRM))) rCount          <- mkCounter(0);

  Reg#(Bool)                               rEnable         <- mkReg(False);
  Reg#(ApbAddr#(`TLM_PRM))                 rAddr           <- mkReg(0);
  Reg#(ApbProt)                            rProt           <- mkRegU;
  Reg#(ApbWrite)                           rWrite          <- mkRegU;
  Reg#(ApbData#(`TLM_PRM))                 rWData          <- mkRegU;
  Reg#(ApbByteEn#(`TLM_PRM))               rWStrb          <- mkRegU;
  Reg#(Bool)                               rSel            <- mkReg(False);

  Wire#(ApbData#(`TLM_PRM))                wRData          <- mkBypassWire;
  Wire#(Bool)                              wSlvErr         <- mkBypassWire;
  Wire#(Bool)                              wReady          <- mkBypassWire;

  ////////////////////////////////////////////////////////////////////////////////
  /// Rules
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  /// Idle State
  ////////////////////////////////////////////////////////////////////////////////
  (* preempts = "idle_state_to_setup, idle_state_to_idle" *)
  rule idle_state_to_setup(xtor_enable &&& rState == IDLE &&&
			   toTLMRequest(fTlmReqs.first) matches tagged Descriptor .tlmreq);
    fTlmReqs.deq;

    ApbProt apbprot = ?;
    apbprot.access = tlmreq.access;
    apbprot.security = tlmreq.security;
    apbprot.privilege = tlmreq.privilege;

    rEnable     <= False;
    rAddr       <= zExtend(tlmreq.addr);
    rSel        <= True;
    rProt       <= apbprot;
    rWrite      <= getApbWrite(tlmreq.command);
    rWData      <= tlmreq.data;

     let be = getTLMByteEn(big_endian, tlmreq);
     if (tlmreq.byte_enable matches tagged Specify .b)
	be = b;
     rWStrb      <= be;

    rState      <= SETUP;
    rDescriptor <= incrTLMAddr(tlmreq);
    rCount.setF (pack(tlmreq.b_length));
  endrule

  rule idle_state_to_idle (xtor_enable && (rState == IDLE) && (!fTlmReqs.notEmpty));
    rEnable     <= False;
    rSel        <= False;
  endrule

  ////////////////////////////////////////////////////////////////////////////////
  /// Setup State
  ////////////////////////////////////////////////////////////////////////////////
  rule setup_state(xtor_enable && rState == SETUP);
    rEnable     <= True;
    rState      <= ACCESS;
  endrule

  ////////////////////////////////////////////////////////////////////////////////
  /// Access State
  ////////////////////////////////////////////////////////////////////////////////
  // We fire this rule on the occasions where we are about to finish this transaction and there
  // is no subsequent transaction to process.  We therefore progress to the IDLE state and
  // wait for the next request.
  rule access_state_to_idle(xtor_enable && rState == ACCESS && wReady &&
			    rCount.value == 0 && !fTlmReqs.notEmpty);
    TLMResponse#(`TLM_PRM) response = defaultValue;
    response.command        = rDescriptor.command;
    response.transaction_id = rDescriptor.transaction_id;
    response.status         = (wSlvErr) ? ERROR : SUCCESS;
    response.data           = duplicate(wRData);
    response.is_last        = True;

    fTlmRsps.enq (fromTLMResponse(response));

    rEnable     <= False;
    rState      <= IDLE;
    rSel        <= False;
  endrule

  // We fire this rule on the occasions where we are about to finish this transaction and there
  // is a subsequent transaction to process.  We progress to the SETUP state and start the address
  // phase without idle cycles.
  rule access_state_to_setup(xtor_enable &&& toTLMRequest(fTlmReqs.first) matches tagged Descriptor .d &&&
			     rState == ACCESS &&& wReady &&& rCount.value == 0);
    fTlmReqs.deq;

    TLMResponse#(`TLM_PRM) response = defaultValue;
    response.command        = rDescriptor.command;
    response.transaction_id = rDescriptor.transaction_id;
    response.status         = (wSlvErr) ? ERROR : SUCCESS;
    response.data           = duplicate(wRData);
    response.is_last        = True;

    fTlmRsps.enq (fromTLMResponse(response));

    ApbProt apbprot = ?;
    apbprot.access = d.access;
    apbprot.security = d.security;
    apbprot.privilege = d.privilege;

    rEnable     <= False;
    rAddr       <= d.addr;
    rSel        <= True;
    rProt       <= apbprot;
    rWrite      <= getApbWrite(d.command);
    rWData      <= d.data;

     let be = getTLMByteEn(big_endian, d);
     if (d.byte_enable matches tagged Specify .b)
	be = b;
     rWStrb      <= be;

    rState      <= SETUP;
    rDescriptor <= incrTLMAddr(d);
    rCount.setF(pack(d.b_length) );
  endrule

  // We fire this rule if we are in the middle of a read-burst transaction.  The read burst
  // is characterized by a single cycle TLM descriptor with a b_length field >1.  We
  // cycle through ACCESS and SETUP states until the slave has provided all the responses
  // of the read burst.
  rule access_state_to_setup_read_burst(xtor_enable && rState == ACCESS &&
					wReady && rCount.value > 0 && rDescriptor.command == READ);
    TLMResponse#(`TLM_PRM) response = defaultValue;
    response.command        = rDescriptor.command;
    response.transaction_id = rDescriptor.transaction_id;
    response.status         = (wSlvErr) ? ERROR : SUCCESS;
    response.data           = duplicate(wRData);
    response.is_last        = True;

    fTlmRsps.enq(fromTLMResponse(response));

    rEnable     <= False;
    rAddr       <= rDescriptor.addr;
    rSel        <= True;
    rState      <= SETUP;
    rDescriptor <= incrTLMAddr(rDescriptor);
    rCount.down;
  endrule

  // We fire this rule if we are in the middle of a write-burst transaction.  The write burst
  // is characterized by a single cycle TLM descriptor with a b_length field >1 and
  // subsequent TLM data beats.  We cycle through ACCESS and SETUP states until the slave has
  // provided all the responses of the write burst.
  rule access_state_to_setup_write_burst(xtor_enable &&& toTLMRequest(fTlmReqs.first) matches tagged Data .d &&&
					 rState == ACCESS &&& wReady &&& rCount.value > 0 &&& rDescriptor.command == WRITE);
    TLMResponse#(`TLM_PRM) response = defaultValue;
    response.command        = rDescriptor.command;
    response.transaction_id = rDescriptor.transaction_id;
    response.status         = (wSlvErr) ? ERROR : SUCCESS;
    response.data           = duplicate(wRData);
    response.is_last        = True;

    fTlmRsps.enq(fromTLMResponse(response));
    fTlmReqs.deq;

    ApbProt apbprot = ?;
    apbprot.access = rDescriptor.access;
    apbprot.security = rDescriptor.security;
    apbprot.privilege = rDescriptor.privilege;

    rEnable     <= False;
    rAddr       <= rDescriptor.addr;
    rSel        <= True;
    rProt       <= apbprot;
    rWrite      <= getApbWrite(rDescriptor.command);
//  rWData      <= (rDescriptor.addr[2] == 1) ? truncateLSB(d.data) : truncate(d.data);
    rWData      <= d.data;
    rWStrb      <= maxBound;
    rState      <= SETUP;
    rDescriptor <= incrTLMAddr(rDescriptor);
    rCount.down;
  endrule

  ////////////////////////////////////////////////////////////////////////////////
  /// Interface Connections / Methods
  ////////////////////////////////////////////////////////////////////////////////
  interface TLMRecvIFC tlm;
    interface Put rx = toPut (fTlmReqs);
    interface Get tx = toGet (fTlmRsps);
  endinterface

  interface ApbXtorMaster fabric;
    interface ApbMaster bus;
      method paddr   = rAddr;
      method pprot   = rProt;
      method penable = rEnable;
      method pwrite  = rWrite;
      method pwdata  = rWData;
      method pstrb   = rWStrb;
      method psel    = rSel;
      method pready  = wReady._write;
      method prdata  = wRData._write;
      method pslverr = wSlvErr._write;
    endinterface
  endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface ApbSlaveIFC#(`TLM_PRM_DCL);
   interface ApbSlave#(`TLM_PRM)              bus;
   interface Put#(ApbResponse#(`TLM_PRM))     response;
   interface ReadOnly#(ApbRequest#(`TLM_PRM)) request;
   interface ReadOnly#(Bool)                  penable;
   interface ReadOnly#(Bool)                  psel;
endinterface

module mkCApbSlaveIFC#(Bool xtor_enable) (ApbSlaveIFC#(`TLM_PRM))
   provisos(DefaultValue#(ApbResponse#(`TLM_PRM)));

   Wire#(ApbData#(`TLM_PRM))                 wPWDATA             <- mkBypassWire;
   Wire#(ApbWrite)                           wPWRITE             <- mkBypassWire;
   Wire#(ApbProt)                            wPPROT              <- mkBypassWire;
   Wire#(ApbAddr#(`TLM_PRM))                 wPADDR              <- mkBypassWire;
   Wire#(ApbByteEn#(`TLM_PRM))               wPSTRB              <- mkBypassWire;
   Wire#(Bool)                               wPENABLE            <- mkBypassWire;
   Wire#(Bool)                               wPSEL               <- mkBypassWire;

   RWire#(ApbResponse#(`TLM_PRM))            rwResponse          <- mkRWire;
   Reg#(Bool)                                rSelected           <- mkReg(False);

   rule update_select (xtor_enable);
      rSelected <= wPSEL;
   endrule

   interface Put response;
      method Action put(value) if (rSelected);
         rwResponse.wset(value);
      endmethod
   endinterface

   interface ReadOnly request;
      method ApbRequest#(`TLM_PRM) _read if (wPSEL && !wPENABLE);
         let ctrl  = ApbCtrl {
	    command: wPWRITE,
	    addr:    wPADDR,
	    prot:    wPPROT
	    };
         let value = ApbRequest {
	    ctrl: ctrl,
	    strb: wPSTRB,
	    data: wPWDATA
	    };
         return value;
      endmethod
   endinterface

   interface ReadOnly penable;
      method _read = wPENABLE;
   endinterface

   interface ReadOnly psel;
      method _read = wPSEL;
   endinterface

   interface ApbSlave bus;
      // Outputs
      method pready    = isValid(rwResponse.wget);
      method prdata    = fromMaybe(defaultValue, rwResponse.wget).data;
      method pslverr   = fromMaybe(defaultValue, rwResponse.wget).error;

      // Inputs
      method paddr     = wPADDR._write;
      method pprot     = wPPROT._write;
      method penable   = wPENABLE._write;
      method pwrite    = wPWRITE._write;
      method pwdata    = wPWDATA._write;
      method pstrb     = wPSTRB._write;
      method psel      = wPSEL._write;
   endinterface
endmodule

module mkCApbSlave#(Bool xtor_enable) (ApbSlaveXActor#(`TLM_XTR))
   provisos( Bits#(req_t, s0)
           , Bits#(resp_t, s1)
           , DefaultValue#(TLMResponse#(`TLM_PRM))
           , TLMRequestTC#(req_t, `TLM_PRM)
           , TLMResponseTC#(resp_t, `TLM_PRM)
           , FShow#(resp_t)
           , TLMRequestTC#(ApbRequest#(`TLM_PRM), `TLM_PRM));

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   FIFO#(resp_t)                             fifoRx              <- mkLFIFO;
   FIFO#(req_t)                              fifoTx              <- mkLFIFO;

   let                                       ifc                 <- mkApbSlaveIFC;
   let                                       request              = ifc.request;

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule process_setup(xtor_enable && ifc.psel && !ifc.penable);
      let desc = toTLMRequest(request);
      fifoTx.enq(fromTLMRequest(desc));
   endrule

   rule process_access(xtor_enable && ifc.psel && ifc.penable);
      let desc = toTLMResponse(fifoRx.first); fifoRx.deq;
      ifc.response.put(fromTLMResponse(desc));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifoTx);
      interface Put rx = toPut(fifoRx);
   endinterface

   interface ApbXtorSlave fabric;
      interface ApbSlave         bus = ifc.bus;
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkCApbMasterSceMiXactor (TLMXActorArgs args,
					      Bool xtor_enable,
					      ApbMasterSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiClientXactor#(Chunk, Chunk) _streamX <- mkClientStream(args) ;
///////////// (args, cclk, crst_n) = (args, _streamX.uclock, _streamX.ureset);
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   ApbSlaveXActor#(`TLM_XTR) slave_xactor <- mkCApbSlave(xtor_enable);

   ApiSendIFC#(`TLM_RR) sender <- mkApiSender(clocked_by _streamX.uclock, reset_by _streamX.ureset);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, clk, rst_n, _streamX.uclock);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, _streamX.uclock, _streamX.ureset, clk);

   mkConnection(sender.in.tx, toPut(response_fifo));
   mkConnection(sender.in.rx, toGet(request_fifo));

   mkConnection(toGet(response_fifo), slave_xactor.tlm.rx);
   mkConnection(slave_xactor.tlm.tx, toPut(request_fifo));
////////////
   //ApbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkApbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.server, sender.scemi);

   interface bus = slave_xactor.fabric;
endmodule

module [SceMiModule] mkCXApbMasterSceMiXactor#(TLMXActorArgs args, Bool xtor_enable) (CXApbMasterSceMiXactor#(`TLM_PRM))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)));
   let _ifc0 <- mkCApbMasterSceMiXactor (args, xtor_enable);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

// =======================================================

module [SceMiModule] mkCApbSlaveSceMiXactor (TLMXActorArgs args, Bool xtor_enable,
                               ApbSlaveSceMiXactor#(`TLM_PRM) ignore)
   provisos(
            Bits#(req_t, s0),
            Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM)
      );

   SceMiServerXactor#(Chunk, Chunk) _streamX <- mkServerStream(args) ;
/////// (args, cclk, crst_n) = (args, _streamX.uclock, _streamX.ureset)
   Clock clk   <- exposeCurrentClock;
   Reset rst_n <- exposeCurrentReset;

   ApbMasterXActor#(`TLM_XTR) master_xactor <- mkCApbMaster(args.big_endian, xtor_enable);

   ApiRecvIFC#(`TLM_RR) receiver <- mkApiReceiver(5, clocked_by _streamX.uclock, reset_by _streamX.ureset);

   // Clock crossing FIFOs
   SyncFIFOIfc#(req_t) request_fifo  <- mkSyncFIFO(2, _streamX.uclock, _streamX.ureset, clk);
   SyncFIFOIfc#(resp_t) response_fifo <- mkSyncFIFO(2, clk, rst_n, _streamX.uclock);

   mkConnection(receiver.out.tx, toPut(request_fifo));
   mkConnection(receiver.out.rx, toGet(response_fifo));

   mkConnection(toGet(request_fifo), master_xactor.tlm.rx);
   mkConnection(master_xactor.tlm.tx, toPut(response_fifo));
///////
   //ApbSlaveStreamXactor#(`TLM_PRM)  _busX <- mkApbSlaveStreamXactor(args, _streamX.uclock, _streamX.ureset);
   mkConnection (_streamX.client, receiver.scemi);

   interface bus = master_xactor.fabric;
endmodule

module [SceMiModule] mkCXApbSlaveSceMiXactor#(TLMXActorArgs args, Bool xtor_enable) (CXApbSlaveSceMiXactor#(`TLM_PRM))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
            Alias#( TLMRequest#(`TLM_PRM), req_t),
            Alias#( TLMResponse#(`TLM_PRM), resp_t),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)));
   let _ifc0 <- mkCApbSlaveSceMiXactor (args, xtor_enable);
   let _ifc1 <- mkCXify(_ifc0, xtor_enable);
   return _ifc1;
endmodule

endpackage
