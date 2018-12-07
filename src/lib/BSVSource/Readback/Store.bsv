// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Store;

import DReg::*;
import BRAMCore::*;
import BUtils::*;
import Vector::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready *)
interface RAMStore#(type a, type b);
   method Action write(b addr, a value);
   method Action prefetch (b addr);
   method Maybe#(Tuple2#(b, a)) read;
endinterface

module mkRAMStore#(Bool in_reg, Bool out_reg, Integer depth) (RAMStore#(data, addr))
   provisos(Bits#(addr, addr_sz), Bits#(data, data_sz), Eq#(addr));
   
   let _ifc <- mk1ClkBRAMCore1(depth, in_reg, out_reg);
   Reg#(Maybe#(addr)) addr_pipe   <- mkFixedDelay(getLatency(in_reg, out_reg), tagged Invalid);

   function readValue ();
      let value = tagged Invalid;
      if (addr_pipe matches tagged Valid .a) value = tagged Valid tuple2(a, _ifc.b.read);
      return value;
   endfunction
   
   method Action write(addr a, data d);
      _ifc.a.put(True, a, d);
   endmethod
      
   method Action prefetch (addr a);
      _ifc.b.put(False, a, ?);
      addr_pipe <= tagged Valid a;
   endmethod

   method read = readValue;

endmodule

(* no_default_reset *)
module mkFixedDelay#(Integer n, a init) (Reg#(a))
   provisos (Bits#(a, sa));
   
   Wire#(a)  write_wire <- mkDWire(init);
   
   Reg#(a) q[n];
   for (Integer i=0; i<n; i=i+1) q[i] <- mkRegU;
   
   (* aggressive_implicit_conditions *)
   rule do_shift;
      q[0] <= write_wire;
      for (Integer i=1; i<n; i=i+1) q[i] <= q[i-1];
   endrule
   
   method _write = write_wire._write;
   method _read  = q[n-1];
   
endmodule

module mk1ClkBRAMCore1#(Integer memSize, Bool in_reg, Bool out_reg) (BRAM_DUAL_PORT#(addr, data))
   provisos(Bits#(addr, addr_sz), Bits#(data, data_sz), Eq#(addr));
   
   Integer latency = getLatency(in_reg, out_reg);
   
   let _ifc <- mkBRAMCore1(memSize, out_reg);
   PulseWire write_pw <- mkPulseWire;
   
   Reg#(Maybe#(Tuple3#(Bool, addr, data))) in = ?;
   if (in_reg) begin
      in <- mkDRegU(tagged Invalid);
      
      (* aggressive_implicit_conditions *)
      rule send (in matches tagged Valid {.write, .adr, .d});
	 _ifc.put(write, adr, d);
      endrule
   end
   
   Reg#(Bool) is_valid  <- mkFixedDelay(latency, False);
   Reg#(data)        data_prev <- mkRegU;
   
   (* aggressive_implicit_conditions *)
   rule update_data_prev(is_valid);
      data_prev <= _ifc.read;
   endrule
   
   function Action do_put (Bool write, addr a, data d);
      action
	 if (write)  write_pw.send;
	 if (!write) is_valid <= True;
	 if (in_reg) in <= tagged Valid tuple3(write, a, d);
	 else _ifc.put(write, a, d);
      endaction
   endfunction
   
   interface BRAM_PORT a;
      method put = do_put;
      method data read = ?;
   endinterface
   interface BRAM_PORT b;
      method Action put(Bool write, addr a, data d) if (!write_pw) = do_put(write, a, d);
      method data read = (is_valid) ? _ifc.read : data_prev;
   endinterface

endmodule
   
function Integer getLatency (Bool ri, Bool ro) = 1 + ((ri)  ? 1 : 0) + ((ro) ? 1 : 0);
   
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Cache#(numeric type s, type k, type d);
   method Action add (k key, d data);
   method Maybe#(d) find (k key);
   method Action clear();
endinterface
      
module mkCache (Cache#(s, k, d))
   provisos(Bits#(k, sk), Bits#(d, sd), Eq#(k));
   
   Integer n  = valueOf(s);

   Reg#(LBit#(s)) index    <- mkReg(0);
//   PulseWire      clear_pw <- mkPulseWire;
   
   Vector#(s, Reg#(Maybe#(Tuple2#(k, d)))) sq <- replicateM(mkReg(tagged Invalid));
/* -----\/----- EXCLUDED -----\/-----
   
   (* aggressive_implicit_conditions *)
   rule do_clear (clear_pw);
      for (Integer i=0; i<n; i=i+1) 
	 sq[i] <= tagged Invalid;
   endrule
 -----/\----- EXCLUDED -----/\----- */

   method Action add (k key, d data);
      let values <- mapM(updatePair(key, data), sq);
      if (!any(id,values)) sq[index] <= tagged Valid tuple2(key, data);
      if (!any(id,values)) index     <= modIncr(fromInteger(n), index);
   endmethod
   
   method Maybe#(d) find (k key);
      Maybe#(d) result = tagged Invalid;
      for (Integer i=0; i<n; i=i+1) 
	 if (sq[i] matches tagged Valid {.kk, .dd} &&& key == kk) result = tagged Valid dd;
      return result;
   endmethod
   
   method Action clear;
      for (Integer i=0; i<n; i=i+1) 
	 sq[i] <= tagged Invalid;
   endmethod
   
endmodule

function ActionValue#(Bool) updatePair (k key, d data, Reg#(Maybe#(Tuple2#(k, d))) ifc)
   provisos(Eq#(k));
   actionvalue
      Bool changed = False;
      if (ifc matches tagged Valid {.kk, .dd} &&& kk == key) ifc <= tagged Valid tuple2(key, data);
      if (ifc matches tagged Valid {.kk, .dd} &&& kk == key) changed = True;
      return changed;
   endactionvalue
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
   
function a modIncr (a mod, a v) provisos (Bits#(a, sa)) = (pack(v) == pack(mod) - 1) ? unpack(0) : unpack(pack(v) + 1);
function a modDecr (a mod, a v) provisos (Bits#(a, sa)) = (pack(v) == 0) ? unpack(pack(mod) - 1) : unpack(pack(v) - 1);

endpackage