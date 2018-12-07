package Esl_defs;

import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Vector::*;
import Clocks::*;

function Bit#(n) range(Bit#(m) x, Integer i, Integer j);
   return x[i:j];
endfunction
   
typedef void Void;

function Array#(t) array() = ?;

typedef UInt#(32) Size_t;
typedef Size_t Uint;
typedef Size_t Unsignedint;
typedef Empty ESL_EMPTY;
typedef Empty ESL_EMPTY_T1#(type t);
typedef Empty ESL_EMPTY_T2#(type t, type t2);
typedef Empty ESL_EMPTY_T3#(type t, type t2, type t3);
typedef Empty ESL_EMPTY_T4#(type t, type t2, type t3, type t4);
typedef Empty ESL_EMPTY_T5#(type t, type t2, type t3, type t4, type t5);
typedef Bit Sc_bv;
typedef Bit#(1) Sc_bit;
function Sc_bit sc_bit() = ?;
typedef Int#(8) Char;   
typedef Int#(8) SignedChar;   
typedef UInt#(8) UnsignedChar;   
typedef UInt#(32) UnsignedLong;   
typedef UInt#(16) UnsignedShortint;   
typedef Int Sc_int;
typedef Int Sc_bigint;
typedef Int Sc_signed;
typedef UInt Sc_unsigned;   
typedef UInt Sc_uint;
typedef UInt Sc_biguint;

typedef struct {
   Array#(t) arr;
   s size_field;
   s max_size;
		} Ese_array#(type t, type s);

function Ese_array#(t,s) ese_array(s d)
   provisos (PrimIndex#(s,ss));
   Ese_array#(t,s) _a;
   _a.arr = primArrayNewU(toStaticIndex(d));
   _a.size_field = d;
   _a.max_size = d;
   return _a;
endfunction

instance PrimSelectable#(Ese_array#(t,s), t);
   function primSelectFn(p,a,i) = primSelectFn(p,a.arr,i);
endinstance

instance PrimUpdateable#(Ese_array#(t,s), t);      
   function primUpdateFn(p,a,i,x);
      let _aaa = a;
      Array#(t) _aa = primUpdateFn(p,a.arr,i,x);
      _aaa.arr = _aa;
      return _aaa;
   endfunction
endinstance
   
typedef struct {
   Vector#(n,t) arr;
		} Ese_arrayN#(type t, type s, numeric type n) deriving(Bits);

function Ese_arrayN#(t,s,n) ese_arrayN()
   provisos (PrimIndex#(s,ss));
   Ese_arrayN#(t,s,n) _a;
   _a.arr = newVector;
   return _a;
endfunction

instance PrimSelectable#(Ese_arrayN#(t,s,n), t);
   function primSelectFn(p,a,i) = primSelectFn(p,a.arr,i);
endinstance

instance PrimUpdateable#(Ese_arrayN#(t,s,n), t);       
   function primUpdateFn(p,a,i,x);
      let _aaa = a;
      Vector#(n,t) _aa = primUpdateFn(p,a.arr,i,x);
      _aaa.arr = _aa;
      return _aaa;
   endfunction
endinstance

/*instance Bits#(Ese_arrayN#(t,s,n), sa)
   provisos (Bits#(t,st), Bits#(s,ss), PrimIndex#(s,ss),
	     Mul#(st,n,sv), Add#(sv,ss,sa));
   function pack(Ese_arrayN#(t,s,n) a);
      Bit#(sv) vvv = pack((Ese_arrayN#(t,s,n)'(a)).arr);
      Bit#(ss) sss = pack(a.size);
      return {vvv,sss};
   endfunction

   function unpack(b);
      Ese_arrayN#(t,s,n) a = ?;
      match {.vvv, .sss} = split(b);
      a.arr = unpack(vvv);
      a.size = unpack(sss);
      a.max_size = fromInteger(valueof(n));
      return a;
   endfunction
endinstance
*/
   

// HasSize is back in Esl_defs.bsv now that bug 1235 is fixed
typeclass HasSize#(type a, type b) provisos (Literal#(b)) dependencies (a determines b);
   function b size(a x);
endtypeclass

   
instance HasSize#(Array#(t), int);
   function int size(Array#(t) x) = fromInteger(arrayLength(x));
endinstance
				       
instance HasSize#(Ese_array#(t,s), s) provisos (Literal#(s));
   function s size(Ese_array#(t,s) x) = x.size_field;
endinstance
				       
instance HasSize#(Ese_arrayN#(t,s,n), s) provisos (Literal#(s));
   function s size(Ese_arrayN#(t,s,n) x) = fromInteger(valueof(n));
endinstance

instance HasSize#(Bit#(n), s) provisos (Literal#(s));
   function s size(Bit#(n) x) = fromInteger(valueof(n));
endinstance

function UInt#(n) sc_uint() = ?;

function Bit#(n) sc_bv() = ?;
   
instance PrimSelectable#(UInt#(n), Bit#(1));
   function primSelectFn(p,v,i) = primSelectFn(p,pack(v),i);
endinstance
      
instance PrimUpdateable#(UInt#(n), Bit#(1));     
   function primUpdateFn(p,v,i,x) = unpack(primUpdateFn(p,pack(v),i,x));
endinstance
   
/*
instance PrimIndex#(t,st) provisos (Bits#(t,st), Literal#(t), Eq#(t));
   function isStaticIndex = compose(isStaticIndex, pack);
   function toStaticIndex = compose(toStaticIndex, pack);
   function toDynamicIndex = pack;
endinstance
*/
/*   
instance Bits#(Void,0);
   function pack(x) = ?;
   function unpack(x) = ?;
endinstance

instance Eq#(Void);
   function \== (x,y) = False;
   function \/= (x,y) = False;
endinstance
*/
interface Esl_reg#(type t);
   method t read();
   method t _read();
   method Action write(t x);
   method Action _write(t x);
endinterface

interface Esl_rwire#(type a) ;
   method Action wset(a datain) ;
   method Action send();
   method a wget() ;
   method Bool isValid();
endinterface: Esl_rwire

instance Connectable#(Esl_rwire#(t), Esl_rwire#(t));
   module mkConnection#(Esl_rwire#(t) tx, Esl_rwire#(t) rx)();
      rule connect_wire (tx.isValid());
	 rx.wset(tx.wget());
      endrule
   endmodule
endinstance      

function Action exit(Bit#(2) n) = $finish(n);

module esl_mkWireU(Esl_reg#(t)) provisos (Bits#(t,ts)); 
   Reg#(t) _x <- mkWire;
   method read   = _x._read;
   method _read  = _x._read;
   method write  = _x._write;
   method _write = _x._write;
endmodule

module esl_mkRegU(Esl_reg#(t)) provisos (Bits#(t,ts)); 
   Reg#(t) _x <- mkRegU;
   method read   = _x._read;
   method _read  = _x._read;
   method write  = _x._write;
   method _write = _x._write;
endmodule

module esl_mkReg#(t a)(Esl_reg#(t)) provisos (Bits#(t,ts)); 
   Reg#(t) _x <- mkReg(a);
   method read   = _x._read;
   method _read  = _x._read;
   method write  = _x._write;
   method _write = _x._write;
endmodule

module esl_mkRwire(Esl_rwire#(t)) provisos (Bits#(t,ts));
   let _x <- mkRWire;
   method wset = _x.wset;
   method isValid = isValid(_x.wget());
   method wget = validValue(_x.wget());
   method Action send;
      _x.wset(?);
   endmethod
endmodule

interface Ese_id_Vector#(type t);   
   method t read(ix i) provisos(PrimIndex#(ix,sx), Bits#(t,st));
   method Action write(ix i, t x) provisos(PrimIndex#(ix,sx), Bits#(t,st));
   method Integer size();
endinterface
   
module ese_id_vector#(ix n)(Ese_id_Vector#(t))
   provisos (PrimIndex#(ix,sx),Ord#(ix),Arith#(ix),Bits#(t,st));
   Integer sn = toStaticIndex(n);
   Reg#(t) _v[sn];
   for (ix i=0; i<n; i=i+1)
      _v[i] <- mkRegU;
   method read(j);
      let x = _v[j];
      return x;
   endmethod
   method Action write(j,x);
      _v[j] <= x;
   endmethod
   method size();
      return sn;
   endmethod
endmodule
   
instance PrimSelectable#(Ese_id_Vector#(t), t) provisos(Bits#(t,st), 
							PrimMakeUndefined#(t));
   function primSelectFn(p,v,i) = v.read(i);
endinstance
   
instance PrimWriteable#(Ese_id_Vector#(t), t)  provisos(Bits#(t, st),
							PrimMakeUndefined#(t));
   function primWriteFn(p,v,i,x) = v.write(i,x);
endinstance
   
typeclass C_convertible#(type a);
   function UInt#(32) to_ulong(a x);
   function Int#(32) to_long(a x);
   function UInt#(32) to_uint(a x);
   function Int#(32) to_int(a x);
endtypeclass

instance C_convertible#(UInt#(p)) provisos (Add#(p,m,32));
   function UInt#(32) to_ulong(UInt#(p) x) = extend(x);
   function Int#(32)  to_long (UInt#(p) x) = unpack(zeroExtend(pack(x)));
   function UInt#(32) to_uint(UInt#(p) x) = extend(x);
   function Int#(32)  to_int (UInt#(p) x) = unpack(zeroExtend(pack(x)));
endinstance

instance C_convertible#(Int#(p)) provisos (Add#(p,m,32));
   function UInt#(32) to_ulong(Int#(p) x) = unpack(zeroExtend(pack(x)));
   function Int#(32)  to_long (Int#(p) x) = extend(x);
   function UInt#(32) to_uint(Int#(p) x) = unpack(zeroExtend(pack(x)));
   function Int#(32)  to_int (Int#(p) x) = extend(x);
endinstance

instance C_convertible#(Bit#(p)) provisos (Add#(p,m,32));
   function UInt#(32) to_ulong(Bit#(p) x) = unpack(zeroExtend(x));
   function Int#(32)  to_long (Bit#(p) x) = unpack(signExtend(x));
   function UInt#(32) to_uint(Bit#(p) x) = unpack(zeroExtend(x));
   function Int#(32)  to_int (Bit#(p) x) = unpack(signExtend(x));
endinstance

typeclass C_convertible64#(type a);
   function UInt#(64) to_uint64(a x);
   function Int#(64) to_int64(a x);
endtypeclass

instance C_convertible64#(UInt#(p)) provisos (Add#(p,m,64));
   function UInt#(64) to_uint64(UInt#(p) x) = extend(x);
   function Int#(64)  to_int64 (UInt#(p) x) = unpack(zeroExtend(pack(x)));
endinstance

instance C_convertible64#(Int#(p)) provisos (Add#(p,m,64));
   function UInt#(64) to_uint64(Int#(p) x) = unpack(zeroExtend(pack(x)));
   function Int#(64)  to_int64 (Int#(p) x) = extend(x);
endinstance

instance C_convertible64#(Bit#(p)) provisos (Add#(p,m,64));
   function UInt#(64) to_uint64(Bit#(p) x) = unpack(zeroExtend(x));
   function Int#(64)  to_int64 (Bit#(p) x) = unpack(signExtend(x));
endinstance

function c bitsize(a x) provisos(Literal#(c), Bits#(a,sa));
   return fromInteger(valueof(sa)); 
endfunction
   
//=======
   
typedef Clock Esl_clock;
typedef Reset Esl_reset;
module esl_set_clock#(a ifc, Clock c)(Empty);
endmodule
module esl_set_reset#(a ifc, Reset c)(Empty);
endmodule
   
module esl_mkSyncRegToCC #( a_type initValue
                       )( Esl_clock sClkIn, Esl_reset sRstIn,
                          Esl_reg #(a_type) ifc)
   provisos (Bits#(a_type,sa)) ;
   
   Reg#(a_type) _x <- mkSyncRegToCC(initValue, sClkIn, sRstIn);
   method read   = _x._read;
   method _read  = _x._read;
   method write  = _x._write;
   method _write = _x._write;
endmodule
   
interface Esl_SyncFIFOIfc#(type t);
   method Bool ok_to_enq();
   method Action enq(t x);
   method Bool ok_to_deq();
   method Action deq();
   method t first();
endinterface      

module esl_mkSyncFIFOToCC #( Integer depthIn
                        )( Esl_clock sClkIn, Esl_reset sRstIn,
                           Esl_SyncFIFOIfc #(a_type) ifc)
   provisos (Bits#(a_type,sa));

   SyncFIFOIfc#(a_type) _x <- mkSyncFIFOToCC(depthIn, sClkIn, sRstIn);
   
   method first = _x.first;
   method enq = _x.enq;
   method deq = _x.deq;
   method ok_to_enq = _x.notFull;
   method ok_to_deq = _x.notEmpty;
endmodule

//=======   
endpackage
