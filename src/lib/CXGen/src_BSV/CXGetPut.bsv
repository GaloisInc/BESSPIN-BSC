package CXGetPut;

import SceMi::*;
import Clocks::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;

// ================

// Possibly these should go into the general library:

interface FifoIn#(type t);
  method Bool notFull();
  method Action enq(t x);
endinterface

interface FifoOut#(type t);
  method Bool notEmpty();
  method t first();
  method Action deq();
endinterface

module [SceMiModule] mkFifoInPortXactor (FifoOut#(ty))
   provisos (Bits#(ty, ty_sz));

   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   FIFOF#(ty) res_fifo <- mkSizedFIFOF(3);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inport));
   rule request;
      inport.request();
   endrule

   method notEmpty = res_fifo.notEmpty;
   method first    = res_fifo.first;
   method deq      = res_fifo.deq;
endmodule


module [SceMiModule] mkFifoOutPortXactor (FifoIn#(ty))
   provisos (Bits#(ty, ty_sz));

   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   FIFOF#(ty) res_fifo <- mkSizedFIFOF(3);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outport));

   method notFull = res_fifo.notFull;
   method enq(x)  = res_fifo.enq(x);
endmodule

module [SceMiModule] mkFifoInPipeXactor#(Integer depth, Visibility style) (FifoOut#(ty))
   provisos( Bits#(ty, ty_sz) );

   SceMiInputPipeIfc#(1, ty) inpipe <- mkSceMiInputPipe(depth, style);

   FIFOF#(ty) res_fifo <- mkSizedFIFOF(5);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inpipe));

   method notEmpty = res_fifo.notEmpty;
   method first    = res_fifo.first;
   method deq      = res_fifo.deq;
endmodule

module [SceMiModule] mkFifoOutPipeXactor#(Integer depth, Visibility style) (FifoIn#(ty))
   provisos (  Bits#(ty, ty_sz)
	     //, Add#(a__, ty_sz, TMul#(TDiv#(ty_sz, 8), 8))
	     );

   SceMiOutputPipeIfc#(1, ty) outpipe <- mkSceMiOutputPipe(depth, style);

   FIFOF#(ty) res_fifo <- mkSizedFIFOF(5);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outpipe));

   method notFull = res_fifo.notFull;
   method enq(x)  = res_fifo.enq(x);
endmodule

// ================

interface CXGet#(type t);  // Blasted dual Get
   method Action  get_RDY(Bit#(1) b);
   method Bit#(1) get_EN;
   method Action  get_DATA(t x);
endinterface

interface CXPut#(type t);  // Blasted dual Put
   method Action  put_RDY(Bit#(1) b);
   method Bit#(1) put_EN;
   method t       put_DATA;
endinterface

module [SceMiModule] mkCXGetOutPortXactor#(Bool xtor_enable) (CXGet#(ty))
   provisos (Bits#(ty, ty_sz));

   FifoIn#(ty) _ff <- mkFifoOutPortXactor;

   Reg#(ty)   w_value <- mkDWire(?);
   Reg#(Bool) r_ffRdy <- mkReg(False);
   Reg#(Bool) r_canDo <- mkReg(False);
   Reg#(Bool) w_rdy   <- mkDWire(False);

   Bool en = (r_ffRdy && w_rdy);

   (* fire_when_enabled *)
   rule connect_Rdy (xtor_enable && !r_canDo);
      r_ffRdy  <= _ff.notFull;
      r_canDo  <= _ff.notFull;
   endrule

   (* fire_when_enabled *)
   rule connect_en (en && r_canDo);
      _ff.enq(w_value);
      r_canDo <= False;
   endrule

   method get_RDY(x)  = w_rdy._write(unpack(x));
   method get_EN      = pack(en);
   method get_DATA(x) = w_value._write(x);
endmodule

module [SceMiModule] mkCXPutInPortXactor#(Bool xtor_enable) (CXPut#(ty))
   provisos (Bits#(ty, ty_sz));

   FifoOut#(ty) _ff <- mkFifoInPortXactor;

   Reg#(ty)   r_value <- mkRegU;
   Reg#(Bool) r_ffRdy <- mkReg(False);
   Reg#(Bool) r_canDo <- mkReg(False);
   Reg#(Bool) w_rdy   <- mkDWire(False);

   Bool en = (r_ffRdy && w_rdy);

   (* fire_when_enabled *)
   rule connect_Rdy (xtor_enable && !r_canDo);
      r_ffRdy  <= _ff.notEmpty;
      if (_ff.notEmpty) begin
	 r_canDo <= True;
	 r_value <= _ff.first;
      end
   endrule

   (* fire_when_enabled *)
   rule connect_en (en && r_canDo);
      _ff.deq;
      r_canDo <= False;
   endrule

   method put_RDY(x) = w_rdy._write(unpack(x));
   method put_EN     = pack(en);
   method put_DATA   = r_value;
endmodule

module [SceMiModule] mkCXGetOutPipeXactor#(Integer depth, Visibility style, Bool xtor_enable) (CXGet#(ty))
   provisos (Bits#(ty, ty_sz));

   FifoIn#(ty) _ff <- mkFifoOutPipeXactor(depth, style);

   Reg#(ty)   w_value <- mkDWire(?);
   Reg#(Bool) r_ffRdy <- mkReg(False);
   Reg#(Bool) r_canDo <- mkReg(False);
   Reg#(Bool) w_rdy   <- mkDWire(False);

   Bool en = (r_ffRdy && w_rdy);

   (* fire_when_enabled *)
   rule connect_Rdy (xtor_enable && !r_canDo);
      r_ffRdy  <= _ff.notFull;
      r_canDo  <= _ff.notFull;
   endrule

   (* fire_when_enabled *)
   rule connect_en (en && r_canDo);
      _ff.enq(w_value);
      r_canDo <= False;
   endrule

   method get_RDY(x)  = w_rdy._write(unpack(x));
   method get_EN      = pack(en);
   method get_DATA(x) = w_value._write(x);
endmodule

module [SceMiModule] mkCXPutInPipeXactor#(Integer depth, Visibility style, Bool xtor_enable) (CXPut#(ty))
   provisos (Bits#(ty, ty_sz));

   FifoOut#(ty) _ff <- mkFifoInPipeXactor(depth, style);

   Reg#(ty)   r_value <- mkRegU;
   Reg#(Bool) r_ffRdy <- mkReg(False);
   Reg#(Bool) r_canDo <- mkReg(False);
   Reg#(Bool) w_rdy   <- mkDWire(False);

   Bool en = (r_ffRdy && w_rdy);

   (* fire_when_enabled *)
   rule connect_Rdy (xtor_enable && !r_canDo);
      r_ffRdy  <= _ff.notEmpty;
      if (_ff.notEmpty) begin
	 r_canDo <= True;
	 r_value <= _ff.first;
      end
   endrule

   (* fire_when_enabled *)
   rule connect_en (en && r_canDo);
      _ff.deq;
      r_canDo <= False;
   endrule

   method put_RDY(x) = w_rdy._write(unpack(x));
   method put_EN     = pack(en);
   method put_DATA   = r_value;
endmodule

endpackage
