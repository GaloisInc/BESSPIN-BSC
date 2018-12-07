package CXCommon;

// ==========================

typedef Bool                         FromIfc;
typedef function Action f(Bool x)    ToIfc;

typedef Bit#(1)                      CXFromIfc;
typedef function Action f(Bit#(1) x) CXToIfc;

interface ReadyValid;
   method Bool toBridge;
   method Bool toDut;
endinterface

module mkReadyValid#(Bool fromBridge, Bool fromDut, Bool xtor_enable)(ReadyValid);
   // toDut must be asserted for complete xtor_cycle
   Reg#(Bool) fromBridgeR <- mkReg(False);
   Reg#(Bool) canDo       <- mkReg(False);
   Reg#(Bool) mustDo      <- mkReg(False);

   rule setRVR (xtor_enable);
      fromBridgeR  <= fromBridge;
      canDo        <= fromBridge;
   endrule

   (*no_implicit_conditions, fire_when_enabled*)
   rule setMustDo;
      mustDo <= xtor_enable;
   endrule

   Bool xfer = fromDut && canDo && mustDo;

   rule doXfer (xfer);
      canDo <= False;
   endrule

   method toDut    = fromBridgeR;
   method toBridge = xfer;
endmodule

// Bridge sends data, Dut receives data:
//
interface CXSenderIfc;
   method CXToIfc   ready;
   method CXFromIfc valid;
endinterface

module mkSenderRV#(Bool xtor_enable, ToIfc readyBridge, FromIfc validBridge)(CXSenderIfc);
   Wire#(FromIfc) readyDut <- mkBypassWire;
   let rv <- mkReadyValid(validBridge, readyDut, xtor_enable);

   (*no_implicit_conditions, fire_when_enabled*)
   rule toBridge;  readyBridge(rv.toBridge);  endrule

   method ready = compose(readyDut._write, unpack);
   method valid = pack(rv.toDut);
endmodule

interface SenderX#(type t);
   method t x;
endinterface

module mkSenderX#(Bool xtor_enable, t x_in)(t) provisos(Bits#(t,st));
   Reg#(t) r <- mkRegU;
   rule setX (xtor_enable);
      r <= x_in;
   endrule
   return r;
endmodule

// Bridge receives data, Dut sends data:
//
interface CXReceiverIfc;
   method CXFromIfc ready;
   method CXToIfc   valid;
endinterface

module mkReceiverRV#(Bool xtor_enable, FromIfc readyBridge, ToIfc validBridge)(CXReceiverIfc);
   Wire#(FromIfc) validDut <- mkBypassWire;
   let rv <- mkReadyValid(readyBridge, validDut, xtor_enable);

   (*no_implicit_conditions, fire_when_enabled*)
   rule toBridge;  validBridge(rv.toBridge);  endrule

   method ready = pack(rv.toDut);
   method valid = compose(validDut._write, unpack);
endmodule

// ==========================

endpackage
