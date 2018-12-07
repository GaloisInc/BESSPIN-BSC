// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package ICapFSM;

import BRAM::*;
import CBus::*;
import DefaultValue::*;
import FIFOF::*;
import GetPut::*;
import ICap::*;
import ICapFSMCreate::*;
import ICapFSMDefines::*;
import ReadbackDefines::*;

export ICapFSM(..);
export FrameCmd(..);
export TData(..);
export ICapArgs(..);
export mkICapFSM;
export frameSize;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Integer frameSize(XilinxFamily family);
   if (family == VIRTEX6)
      return 81;
   else // if (family == KINTEX7)
      return 101;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {Wait, DoInit, DoRun} FMode      deriving(Bounded, Bits, Eq);
typedef enum {Idle, CFG, LSB}      StampState deriving(Bounded, Bits, Eq);

typedef union tagged {a               FAddr;
		      a               FOffset;
		      RdBackCycleStamp Capture;
		      ConfigNum       Config;
		      void            Flush;
		      } FrameCmd#(type a) deriving(Eq, Bits, Bounded, FShow);

typedef union tagged {Bit#(30) Preamble;
		      Bit#(32) Data;
                      } TData deriving(Eq, Bits, Bounded, FShow);

interface ICapFSM#(type a);
   interface Put#(FrameCmd#(a))     cmd;
   interface Get#(TData)            data;
endinterface

module mkICapFSM#(ICapArgs args) (ICapFSM#(a))
   provisos(Bits#(a, sa), Bits#(FrameCmd#(a), sf));

   Integer limit = frameSize(args.family);
   Bit#(1) glutmask;
   if (args.family == VIRTEX6)
      glutmask = 0;
   else // if (args.family == KINTEX7)
      glutmask = 1;

   FIFOF#(FrameCmd#(a))         fifo_cmd  <- mkLFIFOF;
   FIFOF#(TData)         fifo_data <- mkSizedFIFOF(limit + 4); // limit + 1 + 3
   FIFOF#(TData)         fifo_last <- mkFIFOF;

   Reg#(StampState)      stamp_state <- mkReg(Idle);
   Reg#(RdBackCycleStamp) stamp      <- mkRegU;
   Reg#(ConfigNum)       cfg_prev    <- mkReg(maxBound);
   Reg#(Bit#(30))        msb_prev    <- mkReg(maxBound);
   Reg#(Bool)            msb_require <- mkReg(False);
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   
   let icap    <- mkICap(args);

   Wire#(Bool)     rdwr_wire         <- mkDWire(True);
   Wire#(Bool)     csb_wire          <- mkDWire(True);
   Wire#(Bit#(32)) datain_wire       <- mkDWire(0);
   
   (* aggressive_implicit_conditions *)
   rule connect_icap;
      icap.rdwr(rdwr_wire);
      icap.csb(csb_wire);
      icap.in(datain_wire);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
 
   Reg#(FMode)     mode       <- mkReg(Wait);  
   Reg#(Bit#(10))  wait_cnt   <- mkReg(0);
   
   Bool in_run_mode = mode == DoRun;
   
   (* aggressive_implicit_conditions *)
   rule do_wait (mode == Wait);
      wait_cnt <= wait_cnt + 1;
      if (wait_cnt == 1000) mode <= DoInit;
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   
   BRAM_Configure cfg = defaultValue ;
   cfg.latency = 1;
   if (args.family == VIRTEX6)
      cfg.loadFormat = tagged Hex "icap_fsm.txt";
   else // if (args.family == VIRTEX7)
      cfg.loadFormat = tagged Hex "icap_fsm7.txt";
   cfg.allowWriteResponseBypass = False ;
   BRAM1Port#(Bit#(7), ICapInputs) inst_mem <- mkBRAM1Server(cfg);
   Reg#(Bit#(7))                   pc       <- mkReg(0);
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   
   Reg#(Bit#(11))  count      <- mkReg(0);
   Reg#(Bool)      capture    <- mkReg(False);

   Reg#(Bool)      read_ctl   <- mkReg(True);

   Reg#(Bit#(32))  write_reg  <- mkReg(0);
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   
   Bit#(7) read_ctl_first  = 'h01;
   Bit#(7) read_ctl_last   = 'h08;
   Bit#(7) write_ctl_first = 'h09;
   Bit#(7) write_ctl_last  = 'h11;
   Bit#(7) capture_first   = 'h12;
   Bit#(7) capture_last    = 'h1a;
   Bit#(7) read_first      = 'h1b;
   Bit#(7) read_last       = 'h2a;
   Bit#(7) post_first      = 'h2b;
   Bit#(7) post_last       = 'h2e;
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   
   (* aggressive_implicit_conditions *)
   rule start_read_ctl (mode == DoInit && read_ctl && pc == 0 && count == 0);
      pc <= read_ctl_first;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule start_write_ctl (mode == DoInit && !read_ctl && pc == 0 && count == 0);
      pc <= write_ctl_first;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule start_read (fifo_cmd.first matches tagged FAddr .a &&& !fifo_data.notEmpty 
		    &&& in_run_mode &&& pc == 0 && count == 0);
      pc <= read_first;
      capture <= False;
      write_reg <= extendNP(pack(a));
      fifo_cmd.deq;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule send_config (fifo_cmd.first matches tagged Config .c &&& !fifo_data.notEmpty 
		     &&& in_run_mode &&& pc == 0 && count == 0 && stamp_state == Idle);
      fifo_cmd.deq;
      cfg_prev <= c;
      if (c != cfg_prev)
	 begin
	    Bit#(30) value = pack(c);
	    fifo_data.enq(tagged Preamble value);
	    // Require all the following fields
	    msb_require <= True;
	 end
      stamp_state <= CFG;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule start_capture (fifo_cmd.first matches tagged Capture .s &&& !fifo_data.notEmpty 
		       &&& in_run_mode &&& pc == 0 && count == 0 && stamp_state == CFG);
      pc <= capture_first;
      capture <= True;
      fifo_cmd.deq;
      stamp <= s;
      Bit#(30) msb = getCycleStampMSB(s);
      msb_require <= False;
      msb_prev <= msb;
      if (msb_require || (msb != msb_prev))
	 begin
	    fifo_data.enq(tagged Preamble msb);
	 end
      stamp_state <= LSB;
   endrule
   
   rule send_stamp_lsb (count == 0 && stamp_state == LSB);
      Bit#(30) value = getCycleStampLSB(stamp);
      fifo_data.enq(tagged Preamble value);
      stamp_state <= Idle;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule update_pc (pc != 0);
      Bool finish = False;
      Bool idle   = False;
      if (pc == post_last)
	 idle = True;
      if (pc == read_ctl_last)
	 finish = True;
      if (pc == write_ctl_last)
	 begin
	    finish = True;
	    mode <= DoRun;
	 end
      if (pc == capture_last || pc == read_last)
	 begin
	    finish  = True;
	    capture <= False;
	 end
      pc <= (idle) ? 0 :((finish) ? post_first : pc + 1);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule request_instruction (pc != 0);
      BRAMRequest#(Bit#(7), ICapInputs) request = ?;
      request.write           = False;
      request.responseOnWrite = False;
      request.address         = pc;
      request.datain          = ?;
      inst_mem.portA.request.put(request);
   endrule

   // This should only happen when (pc != 0) but BSC can't see that
   // so use an attribute to silence a scheduling warning
   (* descending_urgency = "start_read, process_instruction" *)
   (* aggressive_implicit_conditions *)
   rule process_instruction;
      ICapInputs inputs <- inst_mem.portA.response.get;
      rdwr_wire <= inputs.rdwr;
      csb_wire  <= inputs.csb;
      Bit#(32) data = 0;
      if (inputs.datain matches tagged Direct .d)
	 begin
	    data = d;
	    FPacket packet = unpack(d);
	    if (packet matches tagged One .h &&& h.op == Read)
	      count <= h.count; 
	 end
      if (inputs.datain matches tagged Reg)
	 data = write_reg;
      datain_wire <= swizzle_bits(data);
   endrule

   (* preempts = "process_instruction, do_read" *)
   (* aggressive_implicit_conditions *)
   rule do_read (pc == 0 && count != 0);
      csb_wire    <= False;
      rdwr_wire   <= True;
      datain_wire <= swizzle_bits(pack(noOp));
   endrule
   
   (* aggressive_implicit_conditions *)
   rule grab_data (!icap.busy && !csb_wire && rdwr_wire && count != 0);
      if (read_ctl && mode == DoInit) begin
	 let ctl = swizzle_bits(icap.out);
//	 $display("(%0d) Original ctl bits (%8h)\n", $time, ctl);
	 ctl[8] = glutmask;
//	 $display("(%0d) Setting GLUTMASK bit to %d (%8h)\n", $time, glutmask, ctl);
	 write_reg <= ctl;
	 read_ctl <= False;
      end
//      if (count == 82 && mode == DoRun) fifo_data.enq(write_reg); // frame address
      if (count < fromInteger(limit + 1)  && mode == DoRun) fifo_data.enq(tagged Data swizzle_bits(icap.out));
      count <= count - 1;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule connect_fifos;
      fifo_last.enq(fifo_data.first);
      fifo_data.deq;
   endrule
   
   interface cmd  = toPut(fifo_cmd);
   interface data = toGet(fifo_last);
   
endmodule

endpackage
