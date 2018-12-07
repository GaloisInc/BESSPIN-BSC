package PowerManagement;

import Contexts::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import List::*;
import GetPut::*;
import Connectable::*;
import SceMi::*;
import BUtils::*;
import TLM::*;
import DefaultValue::*;

`include "TLM.defines"

typedef Bool     PMData;
typedef UInt#(8) PMAddr;

// ----------------------------------------------------------

function List#(Bool) lsb(List#(Bool) x);
   function Tuple2#(Bool, List#(Bool)) f (Bool b, Tuple2#(Bool, List#(Bool)) cbs);
      match {.c, .bs} = cbs;
      let rb = (c ? False : b);
      let rc = (b || c);
      return tuple2(rc, cons(rb,bs));
   endfunction
   match {.resb, .resbs} = foldr(f, tuple2(False, nil), x);
   return resbs;
endfunction

interface Choose;
   method ActionValue#(List#(Bool)) choose;
endinterface

function Bool read(Reg#(Bool) x) = x;
function Action write(Reg#(Bool) x, Bool v) = (action x <= v; endaction);

module mkChoose#(List#(PMIfc) x)(Choose);
   function f(x) = mkReg(True);
   List#(Reg#(Bool)) notRuledOut <- mapM(f, x);
   function t(x) = True;
   List#(Bool) noneRuledOut = map(t, x);

   method ActionValue#(List#(Bool)) choose;
      List#(Bool) nro = map(read, notRuledOut);
      function ready(i) = i.recvIfc.notEmpty();
      List#(Bool) rdy = map(ready, x);
      List#(Bool) eligible = zipWith(\&& , rdy, nro);
      if (!\or (eligible)) begin
	 eligible = rdy;
	 nro = noneRuledOut;
      end
      List#(Bool)  selected = lsb(eligible);
      if (\or (selected)) nro = zipWith(\/= , nro, selected);
      joinActions(zipWith(write, notRuledOut, nro));
      return selected;
   endmethod
endmodule

// ----------------------------------------------------------

interface Export#(type t);
   method t first();
   method Action deq();
   method Bool notEmpty();
endinterface

instance ToGet#(Export#(t), t);
   function Get#(t) toGet(Export#(t) e);
      return (interface Get;
		 method ActionValue#(t) get();
		    e.deq();
		    return e.first();
		 endmethod
	      endinterface
	      );
   endfunction
endinstance

function Export#(t) toExport(FIFOF#(t) f);
   return (interface Export;
	      method first    = f.first;
	      method deq      = f.deq;
	      method notEmpty = f.notEmpty;
	   endinterface
	   );
endfunction

// All addresses in the PM address space are either write-only or read-only.
// Transactions to a write-only address will always write, generating no
// response.  Transactions to a read-only address always generate a response,
// the address field indicating the source of the response.  The Bool (isFrom)
// indicates whether the address is the source or the destination.  All items
// are transmitted to all nodes (except the sender); it is the receiver's
// responsibility to ignore those which do not concern it.

typedef Tuple3#(PMAddr, PMData, Bool) PMItem;

interface PMIfc;
   interface    Put#(PMItem) sendIfc;
   interface Export#(PMItem) recvIfc;
endinterface

instance Connectable#(PMIfc, PMIfc);
   module mkConnection#(PMIfc i1, PMIfc i2)(Empty);
      mkConnection(i1.sendIfc, toGet(i2.recvIfc));
      mkConnection(i2.sendIfc, toGet(i1.recvIfc));
   endmodule
endinstance

Put#(PMItem) nullSendIfc = (
   interface Put;
      method Action put(x); endmethod
   endinterface
			     );

Export#(PMItem) nullRecvIfc = (
   interface Export;
      method first() if (False) = ?;
      method deq() if (False) = noAction;
      method notEmpty = False;
   endinterface
			       );

PMIfc nullPMIfc = (
   interface PMIfc;
      interface sendIfc = nullSendIfc;
      interface recvIfc = nullRecvIfc;
   endinterface
		   );

function List#(Integer) integers() = cons(0, map(\+ (1), integers()));

typedef List#(PMIfc) PMContext;

module mkInitialPMContext(PMContext);
   return nil;
endmodule

// The boilerplate for the Longfellow Bridge:

instance Expose#(PMContext, PMIfc, CLOCKCONTEXTSIZE);
   module unburyContext#(PMContext ifcs)(PMIfc);
      case (length(ifcs))
	 0: return nullPMIfc;
	 1: return head(ifcs);
	 default: begin
		     FIFOF#(PMItem) sendFF <- mkBypassFIFOF;
		     FIFOF#(PMItem) recvFF <- mkFIFOF;

		     PMIfc above = (interface PMIfc;
				       interface sendIfc = toPut(sendFF);
				       interface recvIfc = toExport(recvFF);
				    endinterface
				    );
		     let ifcs2 = cons(above, ifcs);
		     Choose c <- mkChoose(ifcs2);

		     function f(x) = mkWire;
		     List#(Wire#(Bool)) choice <- mapM(f, ifcs2);

		     (*no_implicit_conditions, fire_when_enabled*)
		     rule setChoice;
			let s <- c.choose();
			joinActions(zipWith(write, choice, s));
		     endrule

		     let cnis = zip3(choice, integers(), ifcs2);

		     function Rules doTheOne(Wire#(Bool) ws, Integer n, PMIfc i);
			Bool s = ws;
			return (rules
				   rule doit (s);
				      let x <- toGet(i.recvIfc).get();
				      function a(wni);
					 match {.w2, .n2, .i2} = wni;
					 return (
						 action
						    if (n!=n2) i2.sendIfc.put(x);
						 endaction
						 );
				      endfunction
				      joinActions(map(a,cnis));
				   endrule
				endrules
				);
		     endfunction

		     addRules(foldl(rJoinMutuallyExclusive, emptyRules,
				    zipWith3(doTheOne, choice, integers(), ifcs2)));

		     interface sendIfc = toPut(recvFF);
		     interface recvIfc = toExport(sendFF);
		  end
      endcase
   endmodule
endinstance

instance Hide#(mc, PMIfc)
   provisos (IsModule#(mc, a), Context#(mc, PMContext));
   module [mc] reburyContext#(PMIfc i)(Empty);
      PMContext c <- getContext();
      c = cons(i,c);
      putContext(c);
   endmodule
endinstance

interface PMSend;
   method Action pmSend(PMAddr a, PMData d);
endinterface

interface PMRecv;
   method ActionValue#(PMData) pmRecv();
endinterface

interface PMRead;
   method Action pmReq(PMAddr a);
   method ActionValue#(PMData) pmResp();
endinterface

module [mc] mkPMSend(PMSend)
   provisos (IsModule#(mc, a), Context#(mc, PMContext));
   PMContext c <- getContext();
   FIFOF#(PMItem) ff <- mkBypassFIFOF;
   PMIfc i = (
      interface PMIfc;
	 interface sendIfc = nullSendIfc;
	 interface recvIfc = toExport(ff);
      endinterface
	      );
   c = cons(i,c);
   putContext(c);

   method Action pmSend(a, d);
      ff.enq(tuple3(a,d,False));
   endmethod
endmodule

module [mc] mkPMRecvFor#(PMAddr a)(PMRecv)
   provisos (IsModule#(mc, aa), Context#(mc, PMContext));
   PMContext c <- getContext();
   FIFO#(PMItem) ff <- mkBypassFIFO;
   PMIfc i = (
      interface PMIfc;
	 interface sendIfc = toPut(ff);
	 interface recvIfc = nullRecvIfc;
      endinterface
	      );
   c = cons(i,c);
   putContext(c);

   rule discard1 if (ff.first() matches {.aa, .dd, .isf} &&& aa!=a);
      ff.deq();
   endrule
   (*preempts="discard2, discard1"*)
   rule discard2 if (ff.first() matches {.aa, .dd, .isf} &&& isf);
      ff.deq();
   endrule

   method ActionValue#(PMData) pmRecv() if (ff.first() matches {.aa, .dd, .isf} &&&
					    aa==a  &&& !isf);
      ff.deq();
      return dd;
   endmethod
endmodule

module [mc] mkPMRead#(PMAddr a)(PMRead)
   provisos (IsModule#(mc, aa), Context#(mc, PMContext));
   PMContext c <- getContext();
   FIFOF#(PMItem) ffReq <- mkBypassFIFOF;
   FIFO#(PMItem) ffRsp <- mkBypassFIFO;
   PMIfc i = (
      interface PMIfc;
	 interface sendIfc = toPut(ffRsp);
	 interface recvIfc = toExport(ffReq);
      endinterface
	      );
   c = cons(i,c);
   putContext(c);

   Reg#(Maybe#(PMAddr)) ra <- mkReg(Invalid);

   rule discard1 if (ffRsp.first() matches {.aa, .dd, .isf} &&& aa!=a);
      ffRsp.deq();
   endrule
   rule discard2if (ffRsp.first() matches {.aa, .dd, .isf} &&& aa!=a);
      ffRsp.deq();
   endrule
   (*descending_urgency="discard3,discard2,discard1"*)
   rule discard3 if (!isValid(ra));
      ffRsp.deq();
   endrule

   method Action pmReq(a) if (!isValid(ra));
      ffReq.enq(tuple3(a,?,True));
      ra <= tagged Valid a;
   endmethod

   method ActionValue#(PMData) pmResp() if (ra matches tagged Valid .a &&&
					    ffRsp.first() matches {.aa, .dd, .isf} &&&
					    aa==a  &&& isf);
      ffRsp.deq();
      ra <= tagged Invalid;
      return dd;
   endmethod
endmodule

module [mc] mkSendChanges#(PMAddr adr, PMData init, PMData x)(Empty)
   provisos (IsModule#(mc, aa), Context#(mc, PMContext));

   Reg#(PMData) prevx <- mkReg(init);
   PMSend pms <- mkPMSend;
   let sendIt = pms.pmSend(adr);

   rule doIt (x != prevx);
      prevx <= x;
      sendIt(x);
   endrule
endmodule

module [mc] mkReceiveChanges#(PMAddr adr, PMData init)(ReadOnly#(PMData))
   provisos (IsModule#(mc, aa), Context#(mc, PMContext));

   Reg#(PMData) x <- mkReg(init);
   PMRecv pmr <- mkPMRecvFor(adr);

   rule doIt;
      let y <- pmr.pmRecv();
      x <= y;
   endrule

   method _read = x;
endmodule

module [mc] mkPMXactor(TLMRecvIFC#(`TLM_PRM))
   provisos (IsModule#(mc, aa), Context#(mc, PMContext),
	     Bits#(TLMResponse#(`TLM_PRM), stlmr),
	     DefaultValue#(TLMResponse#(`TLM_PRM)));

   PMContext c <- getContext();
   FIFOF#(PMItem) ffReq <- mkBypassFIFOF;
   FIFO#(PMItem) ffRsp <- mkBypassFIFO;
   PMIfc i = (
      interface PMIfc;
	 interface sendIfc = toPut(ffRsp);
	 interface recvIfc = toExport(ffReq);
      endinterface
	      );
   c = cons(i,c);
   putContext(c);

   FIFO#(TLMResponse#(`TLM_PRM)) fRsp <- mkBypassFIFO;

   interface Put rx;
      method Action put(TLMRequest#(`TLM_PRM) req);
	 if (req matches tagged Descriptor .d) begin
	    if (d.burst_length == 1) begin
	       PMAddr addr = unpack(zExtend(d.addr));
	       PMData dta  = ?;
	       if (d.command == WRITE)
		  begin
		     dta  = unpack(zExtend(d.data));

		     TLMResponse#(`TLM_PRM) response = defaultValue;
		     response.status = SUCCESS;
		     response.transaction_id = d.transaction_id;
		     response.command = WRITE;
		     fRsp.enq(response);
		  end
	       ffReq.enq(tuple3(addr,dta,False));
	    end
	    else $display("PMXactor cannot handle burst lengths greater than 1");
	 end
      endmethod
   endinterface
   interface tx = toGet(fRsp);
endmodule


endpackage
