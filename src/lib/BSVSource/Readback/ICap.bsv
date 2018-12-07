// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package ICap;

import Connectable::*;
import Probe::*;

import ReadbackDefines::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface XlxICapSnoop;
   method Bit#(32) datain;
   method Bool    csb;
   method Bit#(32) dataout;
   method Bool    busy;
   method Bool    rdwr;
endinterface

(* always_ready, always_enabled *)
interface XlxICapSnoopRecv;
   method Action datain (Bit#(32) value);
   method Action csb (Bool value);
   method Action dataout (Bit#(32) value);
   method Action busy (Bool value);
   method Action rdwr (Bool value);
endinterface

instance Connectable#(XlxICapSnoop, XlxICapSnoopRecv);
   module mkConnection#(XlxICapSnoop snp, XlxICapSnoopRecv snp_recv) (Empty);
      (* aggressive_implicit_conditions *)
      rule snoop_connect;
	 snp_recv.datain(snp.datain);
	 snp_recv.csb(snp.csb);
	 snp_recv.dataout(snp.dataout);
	 snp_recv.busy(snp.busy);
	 snp_recv.rdwr(snp.rdwr);
      endrule
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef struct {Int#(32)     device_id;
		XilinxFamily family;
		Bool         is_dummy;
		} ICapArgs;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkICap#(ICapArgs args) (ICap#(32));
   
   ICap#(32) icap;

   if (args.is_dummy) begin
      let e2 <- icap_e2_dummy();
      icap   <- toICap(e2);
   end
   else if (args.family == VIRTEX6) begin
      icap  <- icap_virtex6(args.device_id, "NONE");
   end
   else begin // if (args.family == KINTEX7)
      let e2 <- icap_e2(args.device_id, "NONE");
      icap   <- toICap(e2);
   end
   
   method Action in (Bit#(32) value);
      icap.in(value);
   endmethod
   method Action csb (Bool value);
      icap.csb(value);
   endmethod
   method out  = icap.out;
   method busy = icap.busy;
   method Action rdwr (Bool value);
      icap.rdwr(value);
   endmethod
   
endmodule


module mkICapWithSnoop#(ICapArgs args) (ICap#(32));
   
   ICap#(32) icap;
   
   if (args.is_dummy) begin
      let e2 <- icap_e2_dummy();
      icap   <- toICap(e2);
   end
   else if (args.family == VIRTEX6) begin
      icap  <- icap_virtex6(args.device_id, "NONE");
   end
   else begin // if (args.family == KINTEX7)
      let e2 <- icap_e2(args.device_id, "NONE");
      icap   <- toICap(e2);
   end
   
   let icap_snoop <- mkXlxICapSnoopRecv;
   
   (* aggressive_implicit_conditions *)
   rule connect_outputs;
      icap_snoop.dataout(icap.out);
      icap_snoop.busy(icap.busy);
   endrule
   
   method Action in (Bit#(32) value);
      icap.in(value);
      icap_snoop.datain(value);
   endmethod
   method Action csb (Bool value);
      icap.csb(value);
      icap_snoop.csb(value);
   endmethod
   method out  = icap.out;
   method busy = icap.busy;
   method Action rdwr (Bool value);
      icap.rdwr(value);
      icap_snoop.rdwr(value);
   endmethod
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface ICap#(numeric type w);
   method Action  in (Bit#(w) value);
   method Action  csb (Bool value);
   method Bit#(w) out;
   method Bool    busy;
   method Action  rdwr   (Bool value);
endinterface

import "BVI" ICAP_VIRTEX6 =
module icap_virtex6#(parameter Int#(32)  device_id,
		     parameter String    cfg_file) (ICap#(w));
   
   String width = (case (valueOf(w))
		      8:  "X8";
		      16: "X16";
		      32: "X32";
		      default: error("XXX");
		   endcase);
   
   parameter DEVICE_ID         = device_id;
   parameter ICAP_WIDTH        = width;
   parameter SIM_CFG_FILE_NAME = cfg_file;
   
   default_clock clk(CLK);
   default_reset rst();
   
   method in   (I)     enable ((*inhigh*)ignore_0);
   method csb  (CSB)   enable ((*inhigh*)ignore_1);
   method O    out;
   method BUSY busy;
   method rdwr (RDWRB) enable ((*inhigh*)ignore_2);
      
   schedule out  CF (in, csb, out, busy, rdwr);
   schedule busy CF (in, csb, out, busy, rdwr);
   schedule in   C in;
   schedule in   CF (csb, rdwr);
   schedule csb  C csb;
   schedule csb  CF (in, rdwr);      
   schedule rdwr C rdwr;
   schedule rdwr CF (in, csb);      

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface Startup;
   method Bool cfgclk;
   method Bool cfgmclk;
   method Bool dinspi;
   method Bool eos;
   method Bool preq;
   method Bool tckspi;
   method Action gsr (Bool value);
   method Action gts (Bool value);
   method Action keyclearb (Bool value);
   method Action pack (Bool value);
   method Action usrcclko (Bool value);
   method Action usrcclkts (Bool value);
   method Action usrdoneo (Bool value);
   method Action usrdonets (Bool value);
endinterface

import "BVI" STARTUP_VIRTEX6 =
module startup_virtex6#(parameter String prog_usr) (Startup);
   
   parameter PROG_USR = prog_usr;
   
   default_clock clk();
   default_reset rst();
   
   port CLK = 1'b0;
   
   method CFGCLK cfgclk;
   method CFGMCLK cfgmclk;
   method DINSPI dinspi;
   method EOS eos;
   method PREQ preq;
   method TCKSPI tckspi;
      
   method gsr (GSR)             enable ((*inhigh*)ignore_0);
   method gts (GTS)             enable ((*inhigh*)ignore_1);
   method keyclearb (KEYCLEARB) enable ((*inhigh*)ignore_2);
   method pack (PACK)           enable ((*inhigh*)ignore_3);
   method usrcclko (USRCCLKO)   enable ((*inhigh*)ignore_4);
   method usrcclkts (USRCCLKTS) enable ((*inhigh*)ignore_5);
   method usrdoneo (USRDONEO)   enable ((*inhigh*)ignore_6);
   method usrdonets (USRDONETS) enable ((*inhigh*)ignore_7);
      
   schedule (cfgclk, cfgmclk, dinspi, eos, preq, tckspi, pack, gsr, gts, keyclearb, usrcclko, usrcclkts, usrdoneo, usrdonets) CF (cfgclk, cfgmclk, dinspi, eos, preq, tckspi, pack, gsr, gts, keyclearb, usrcclko, usrcclkts, usrdoneo, usrdonets);
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkXlxICapSnoopRecv (XlxICapSnoopRecv);
   
   let _inner <- mkXlxICapSnoopRecvSynth;
   
   Reg#(Bit#(32)) datain_reg  <- mkReg(0);
   Reg#(Bool)     csb_reg     <- mkReg(False);
   Reg#(Bool)     busy_reg    <- mkReg(False);
   Reg#(Bool)     rdwr_reg    <- mkReg(False);
   Reg#(Bit#(32)) dataout_reg <- mkReg(0);
   
   (* aggressive_implicit_conditions *)
   rule set_value;
      _inner.datain(datain_reg);
      _inner.csb(csb_reg);
      _inner.dataout(dataout_reg);
      _inner.busy(busy_reg);
      _inner.rdwr(rdwr_reg);
   endrule
   
   method datain  = datain_reg._write;
   method csb     = csb_reg._write;
   method dataout = dataout_reg._write;
   method busy    = busy_reg._write;
   method rdwr    = rdwr_reg._write;
endmodule

module mkXlxICapSnoopRecvSynth (XlxICapSnoopRecv);
   
   Reg#(Bool) csb_reg  <- mkReg(False);
   Reg#(Bool) busy_reg <- mkReg(False);
   Reg#(Bool) rdwr_reg <- mkReg(False);
   
   Wire#(Bit#(32))  datain_wire     <- mkBypassWire;
   Wire#(Bit#(32))  dataout_wire    <- mkBypassWire;
   Probe#(Bit#(32)) datain_swizzle  <- mkProbe;
   Probe#(Bit#(32)) dataout_swizzle <- mkProbe;
   Probe#(FPacket)  datain_packet   <- mkProbe;
   
   (* aggressive_implicit_conditions *)
   rule set_swizzels;
      datain_swizzle  <= swizzle_bits(datain_wire);
      dataout_swizzle <= swizzle_bits(dataout_wire);
      datain_packet   <= unpack(swizzle_bits(datain_wire));
   endrule
   
   method datain  = datain_wire._write;
   method csb     = csb_reg._write;
   method dataout = dataout_wire._write;
   method busy    = busy_reg._write;
   method rdwr    = rdwr_reg._write;
endmodule

function Bit#(32) swizzle_bits (Bit#(32) value);
   let vvv = {reverseBits({value[04], value[05], value[06], value[07]}),
	      reverseBits({value[00], value[01], value[02], value[03]}),
	      reverseBits({value[12], value[13], value[14], value[15]}),
	      reverseBits({value[08], value[09], value[10], value[11]}),
	      reverseBits({value[20], value[21], value[22], value[23]}),
	      reverseBits({value[16], value[17], value[18], value[19]}),
	      reverseBits({value[28], value[29], value[30], value[31]}),
	      reverseBits({value[24], value[25], value[26], value[27]}) };
   return reverseBits(vvv);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module toICap#(ICapE2#(w) icap_e2) (ICap#(w));
   
   Wire#(Bool)    csb_wire      <- mkBypassWire;
   Wire#(Bool)    rdwr_wire     <- mkBypassWire;
   
   Reg#(Bool)     csb_delay_0   <- mkReg(False);
   Reg#(Bool)     csb_delay_1   <- mkReg(False);
   Reg#(Bool)     csb_delay_2   <- mkReg(False);
   
   Reg#(Bool)     rdwr_int      <- mkReg(True);
   Reg#(Bool)     same_cycle    <- mkReg(False);
   
   Reg#(Bool)     abort_detect  <- mkReg(False);
   Reg#(Bool)     abort_delay_0 <- mkReg(False);
   Reg#(Bool)     abort_delay_1 <- mkReg(False);
   Reg#(Bool)     abort_delay_2 <- mkReg(False);
   
   Bool busy_abort = abort_delay_0 || abort_delay_1 || abort_delay_2 || abort_detect;
   Bool busy_int   = csb_delay_2 || csb_wire;

   (* aggressive_implicit_conditions *)
   rule set_csb;
      csb_delay_0 <= csb_wire;
      csb_delay_1 <= csb_delay_0;
      csb_delay_2 <= csb_delay_1;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule set_rdwr;
      rdwr_int   <= rdwr_wire || csb_wire;
      same_cycle <= !csb_wire;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule detect_abort (same_cycle && !csb_wire);
      abort_detect <= rdwr_int != rdwr_wire;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule set_abort;
      abort_delay_0 <= abort_detect;
      abort_delay_1 <= abort_delay_0;
      abort_delay_2 <= abort_delay_1;
   endrule
      
   method in   = icap_e2.in;
   method Action csb (Bool value);
      csb_wire <= value;
      icap_e2.csb(value);
   endmethod
      
   method out  = icap_e2.out;
   method Bool busy ();
      return (busy_int && rdwr_wire || busy_abort);
   endmethod
   method Action rdwr (Bool value);
      rdwr_wire <= value;
      icap_e2.rdwr(value);
   endmethod
endmodule


(* always_ready, always_enabled *)
interface ICapE2#(numeric type w);
   method Action  in (Bit#(w) value);
   method Action  csb (Bool value);
   method Bit#(w) out;
   method Action  rdwr   (Bool value);
endinterface

import "BVI" ICAPE2 =
module icap_e2#(parameter Int#(32)  device_id,
		parameter String    cfg_file) (ICapE2#(w));
   
   String width = (case (valueOf(w))
		      8:  "X8";
		      16: "X16";
		      32: "X32";
		      default: error("XXX");
		   endcase);
   
   parameter DEVICE_ID         = device_id;
   parameter ICAP_WIDTH        = width;
   parameter SIM_CFG_FILE_NAME = cfg_file;
   
   default_clock clk(CLK);
   default_reset rst();
   
   method in   (I)     enable ((*inhigh*)ignore_0);
   method csb  (CSIB)   enable ((*inhigh*)ignore_1);
   method O    out;
   method rdwr (RDWRB) enable ((*inhigh*)ignore_2);
      
   schedule out  CF (in, csb, out, rdwr);
   schedule in   C in;
   schedule in   CF (csb, rdwr);
   schedule csb  C csb;
   schedule csb  CF (in, rdwr);      
   schedule rdwr C rdwr;
   schedule rdwr CF (in, csb);      

endmodule

module icap_e2_dummy (ICapE2#(w));
   method Action  in (Bit#(w) value);
   endmethod
   method Action  csb (Bool value);
   endmethod
   method Bit#(w) out = 0;
   method Action  rdwr (Bool value);
   endmethod
endmodule


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {NOOP, Read, Write, Reserved} FOpCode deriving(Bounded, Bits, Eq);

typedef union tagged {void     CMD;
		      FHeader1 One;
		      FHeader2 Two;
		      void     Three;
		      void     Four;
		      void     SYNC;
		      void     Six;
		      void     DUMMY;
                      } FPacket deriving(Eq, Bits, Bounded);

typedef struct {FOpCode  op;
		Bit#(14) addr;
		Bit#(2)  reserved;
		Bit#(11) count;
		} FHeader1 deriving (Eq, Bits, Bounded);

typedef struct {FOpCode  op;
		Bit#(27) count;
		} FHeader2 deriving (Eq, Bits, Bounded);



endpackage
