// Copyright 2009-2010 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiScan;

import CBus::*;
import Clocks::*;
import TieOff::*;

typedef enum {SET_INPUTS, GET_INPUTS,
	      SET_STATE, GET_STATE,
	      SET_BRAMS, GET_BRAMS,
	      SET_OUTPUTS, GET_OUTPUTS} ScanMode deriving(Bounded, Bits, Eq);

typedef UInt#(32) ST; // Used to represent scan chain length

// XXX These should be defined somewhere else instead:
typedef struct {
   UInt#(2) delay;
   UInt#(4) scan_num;
   UInt#(7) num;
		} CosimStruct deriving (Eq, Bits);

typedef enum { ISO, CYCLECOUNTS, REPORT, CHANGES } PwPrbKind deriving (Eq, Bits);

typedef struct {
   PwPrbKind kind;
   UInt#(10) num;
		} PowerStruct deriving (Eq, Bits);

typedef struct {
   UInt#(5) source;
   UInt#(7) block; // sizes must total 12, to ensure no undetermined bits in PrbNum
		} PowerAssoc deriving (Eq, Bits);

typedef union tagged {
   PowerStruct ProbeNumber;
   PowerAssoc  Assoc;
		      } PowerProbe deriving (Eq, Bits);

typedef union tagged {
   UInt#(13)   Explicit;
   UInt#(13)   HdlEdit;
   CosimStruct Cosim;
   PowerProbe  Power;
   UInt#(13)   Assertion;
   void        DUMMY5;
   void        DUMMY6;
   Bit#(13)    Control;
		      } PrbNum deriving (Eq, Bits);

instance Literal#(PrbNum);
   function fromInteger(i) = tagged Explicit (fromInteger(i));
   function inLiteralRange(x,i) = (i>=0 && i < 2**13);
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface Scan#(numeric type n);
   method Bit#(n) scan_out();
   method Action scan_in(Bit#(n) value);
   method Action scan_mode(Bool value);
endinterface

(* always_ready, always_enabled *)
interface ScanDual#(numeric type n);
   (* prefix = "SCAN" *)
   method Action scan_out((* port = "OUT" *) Bit#(n) value);
   (* result = "SCAN_IN" *)
   method Bit#(n) scan_in();
   (* result = "SCAN_MODE" *)
   method Bool scan_mode();
endinterface

(* always_ready, always_enabled *)
interface ScanIn#(numeric type n);
   method Action scan_in(Bit#(n) value);
   method Action scan_mode(Bool value);
endinterface

(* always_ready, always_enabled *)
interface ScanOut#(numeric type n);
   method Bit#(n) scan_out();
   method Action scan_mode(Bool value);
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface ScanConnector#(numeric type n);
   interface Scan#(n)     scan;
   interface ScanDual#(n) dual;
endinterface

interface ScanInConnector#(numeric type n);
   interface ScanIn#(n) scan;
   interface ScanDual#(n) dual;
endinterface

interface ScanOutConnector#(numeric type n);
   interface ScanOut#(n) scan;
   interface ScanDual#(n) dual;
endinterface

module mkScanConnector (ScanConnector#(n));

   Wire#(Bit#(n)) scan_out_wire    <- mkBypassWire;
   Wire#(Bit#(n)) scan_in_wire     <- mkBypassWire;
   Wire#(Bool)    scan_mode_wire   <- mkBypassWire;

   interface Scan scan;
      method scan_out    = scan_out_wire;
      method scan_in     = scan_in_wire._write;
      method scan_mode   = scan_mode_wire._write;
   endinterface
   interface ScanDual dual;
      method scan_out    = scan_out_wire._write;
      method scan_in     = scan_in_wire;
      method scan_mode   = scan_mode_wire;
   endinterface

endmodule
/* -----\/----- EXCLUDED -----\/-----

module toScanOut#(Scan#(n) scan) (ScanOut#(n))
   provisos (Add#(ignore, n, 32),
	     Log#(n, ln),
	     Mul#(k, n, 32));

   Wire#(ST)   length    <- mkBypassWire;
   Wire#(Bool) mode      <- mkBypassWire;
   Reg#(ST)    remaining <- mkReg(0);

   Bool no_extra = (valueOf(n) == 1);
   Bool idle     = remaining == 0;

   ST offset = 0;

   ST scan_count = ((length - 1) >> valueOf(ln)) + 1;

   if (!no_extra)
      begin
	 scan_count = scan_count + 1;
	 Reg#(Bit#(TAdd#(n, n))) extra <- mkReg(0);
	 UInt#(ln) offset0 = truncateNP(length);
	 let bn = fromInteger(valueOf(n));
	 offset = extendNP(offset0);
	 if (offset != 0)
	    offset = bn - offset;
	 let msb = bn + offset - 1;
	 let lsb = msb - (bn - 1);
	 Bit#(n) left = extra[msb:lsb];

	 rule shift (mode && !idle);
	    remaining <= remaining - 1;
	    extra <= truncate({extra, scan.scan_out});
	 endrule

	 rule every;
	    scan.scan_in(left);
	 endrule


	 rule initialize (!mode);
	    remaining <= scan_count;
	 endrule

      end
   else
      begin
	 rule shift (mode && !idle);
	    remaining <= remaining - 1;
	 endrule

	 rule every;
	    scan.scan_in(scan.scan_out);
	 endrule


	 rule initialize (!mode);
	    remaining <= scan_count;
	 endrule

      end

   rule grab_outputs;
      length <= scan.scan_length;
   endrule

   rule set_inputs;
      scan.scan_mode(mode && !idle);
   endrule

   method Bit#(n) scan_out() if (mode && !idle);
      return scan.scan_out;
   endmethod
   method Action scan_mode(Bool value);
      mode <= value;
   endmethod
   method ST scan_length();
      let result = (no_extra) ? scan.scan_length : (scan.scan_length +
						    fromInteger(valueOf(n)) +
						    offset);
      return result;
   endmethod
endmodule
 -----/\----- EXCLUDED -----/\----- */

module toScanInSimple#(Scan#(n) scan) (ScanIn#(n))
   provisos (Add#(ignore, n, 32));

   method scan_in     = scan.scan_in;
   method scan_mode   = scan.scan_mode;
endmodule

module toScanOutSimple#(Scan#(n) scan) (ScanOut#(n))
   provisos (Add#(ignore, n, 32));

   rule every;
      scan.scan_in(0);
   endrule

   method scan_out    = scan.scan_out;
   method scan_mode   = scan.scan_mode;
endmodule

module mkScanInConnector (ScanInConnector#(n))
   provisos (Add#(ignore, n, 32));

   ScanConnector#(n) connector <- mkScanConnector;
   let scan_in <- toScanInSimple(connector.scan);

   interface ScanIn   scan = scan_in;
   interface ScanDual dual = connector.dual;
endmodule

module mkScanOutConnector (ScanOutConnector#(n))
   provisos (Add#(ignore, n, 32));

   ScanConnector#(n) connector <- mkScanConnector;
   let scan_out <- toScanOutSimple(connector.scan);

   interface ScanOut  scan = scan_out;
   interface ScanDual dual = connector.dual;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
/* -----\/----- EXCLUDED -----\/-----

instance TieOff#(ScanDual#(n));
   module mkTieOff#(ScanDual#(n) dual) (Empty);
      rule every;
	 dual.scan_out(0);
	 dual.scan_length(0);
      endrule
   endmodule
endinstance
 -----/\----- EXCLUDED -----/\----- */

endpackage
