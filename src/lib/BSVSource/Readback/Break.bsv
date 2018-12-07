// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Break;

import GetPut::*;
import Vector::*;

import ReadbackDefines::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {XX, Is0, Is1, Lst} BitConstraint deriving(Bounded, Bits, Eq, FShow);

typedef struct {
   Bool                      isSent;
   Bool                      isLast;
   Bool                      isDummy;
   Bit#(1)                   value;
   Bit#(4)                   num;
   Vector#(n, BitConstraint) constraints;
   } BitConstraints#(numeric type n) deriving (Bits, Eq, FShow);

function BitConstraints#(n) toBitConstraints (UInt#(m) value)
   provisos(Add#(TMul#(n, 2), TSub#(CompAddrSize, TMul#(n, 2)), m));
   let p = pack(value);
   
   Integer send_index = valueOf(TMul#(n, 2));
   
   BitConstraints#(n) b = ?;
   b.isSent = p[send_index] == 1;
   b.isLast = False;
   b.isDummy = False;
   b.value  = 0;
   b.num  = 0;
   b.constraints = unpack(truncate(p));
   return b;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface BreakTracker#(numeric type n);
   method Action       clear;
   method Bool         isValid;
   method Bool         value;
   interface Put#(BitConstraints#(n)) rx;
   interface Put#(Bit#(TExp#(n))) code;
endinterface

function Bool myValid (Maybe#(a) v);
   return isValid(v);
endfunction

module mkBreakTracker (BreakTracker#(n));


   Vector#(n, Tracker)      trackers   <- replicateM(mkTracker);
   Wire#(Vector#(n, Bool))  value_wire <- mkBypassWire;
   Reg#(Bit#(n))            value_reg  <- mkReg(?);
   Wire#(Bool)              valid_wire <- mkBypassWire;
   Reg#(Bool)               valid_reg  <- mkReg(?);
   Reg#(Bit#(TExp#(n)))     code_reg   <- mkReg(0);
   // Use mkPulseWireOR to allow the clear method to compose with itself
   PulseWire                clear_pw   <- mkPulseWireOR;
   
   // Wire#(Bool)               value_check <- mkBypassWire;
   
   // rule grab_value;
   //    value_check <= unpack(code_reg[value_reg]);
   // endrule
   
   (* aggressive_implicit_conditions *)
   rule every;
      Vector#(n, Bool) values = ?;
      Bit#(n)  xxx = ?;
      for (Integer i = 0; i < valueOf(n); i = i + 1)
	 begin
	    values[i] = trackers[i].isTrue;
	    xxx[i]   = pack(trackers[i].isTrue);
	 end
      
      value_wire <= values;
      value_reg  <= xxx;
      Bool is_valid = True;
      for (Integer i = 0; i < valueOf(n); i = i + 1)
	 is_valid = is_valid && trackers[i].isValid;
      is_valid = is_valid || code_reg == 0;
      valid_wire <= is_valid;
      valid_reg <= is_valid && !clear_pw;
   endrule
   
   method Action clear;
      for (Integer i = 0; i < valueOf(n); i = i + 1)
	 trackers[i].clear;
      clear_pw.send;
   endmethod
   
   method Bool isValid;
      return valid_reg;
   endmethod
   
   method value = unpack(code_reg[value_reg]);
   
   interface Put rx;
      method Action put (bc);
	 for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	    trackers[i].rx.put(tuple3(~bc.value, bc.constraints[i], bc.isLast));
	 end
      endmethod
   endinterface
   
   interface Put code;
      method Action put (p);
	 code_reg <= p;
      endmethod
   endinterface
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Tracker;
   method Action clear;
   (* always_ready *)
   method Bool isTrue;
   (* always_ready *)
   method Bool isFalse;
   (* always_ready *)
   method Bool isValid;
   interface Put#(Tuple3#(Bit#(1), BitConstraint, Bool)) rx;
endinterface

module mkTracker (Tracker);
   
   Reg#(Bool)                             is_true    <- mkReg(False);
   Reg#(Bool)                             is_false   <- mkReg(False);
   // Use mkPulseWireOR to allow the clear method to compose with itself
   PulseWire                              clear_pw   <- mkPulseWireOR;
   Wire#(Tuple3#(Bit#(1), BitConstraint, Bool)) value_wire <- mkWire;
   
   (* aggressive_implicit_conditions *)
   rule do_clear (clear_pw);
      is_true  <= False;
      is_false <= False;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule do_update (!clear_pw && !is_true && !is_false);
      case (value_wire) matches
	 { 1, Is0, .d}:   is_false <= True;
	 { 0, Is1, .d}:   is_false <= True;
	 {.v, Lst, .d}:   is_true  <= True;
	 { 0, Is0, True}: is_true  <= True;
	 { 1, Is1, True}: is_true  <= True;
	 {.v,  XX, True}: is_true  <= True;
      endcase
   endrule
   
   method Action clear;
      clear_pw.send;
   endmethod

   method isTrue  = is_true;
   method isFalse = is_false;
   method isValid = is_true || is_false;
   
   interface Put rx;
      method Action put (x);
	 value_wire <= x;
      endmethod
   endinterface
   
endmodule



endpackage
