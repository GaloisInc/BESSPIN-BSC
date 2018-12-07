////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : JtagReadback.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package JtagReadback;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import ReadbackCore      ::*;
import ICap              ::*;
import Xilinx            ::*;
import GetPut            ::*;
import Clocks            ::*;
import DefaultValue      ::*;
import BUtils            ::*;
import FShow             ::*;
import FIFO              ::*;
import FIFOF             ::*;
import BRAMFIFO          ::*;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef enum {
   Unknown_0,
   Unknown_1,
   Unknown_2,
   Unknown_3,
   RdBackOn,
   RdBackOff,
   RdBackCmd,
   RdBackStore
} JtagRdBkCommand deriving (Eq, Bits, FShow);		

typedef struct {
   JtagRdBkCommand  command;
   Bit#(29)         data;
} JtagCommand deriving (Bits, Eq, FShow);

instance DefaultValue#(JtagCommand);
   defaultValue = JtagCommand {
      command:    Unknown_0,
      data:       0
      };
endinstance

function Bool isBrkCode(JtagCommand req);
   return (req.command == RdBackCmd && pack(req.data)[28] == 1);
endfunction

function Bool isFinish(JtagCommand req);
   return (req.command == RdBackCmd && pack(req.data)[28] != 1 && pack(req.data)[27] == 1);
endfunction

function Bool isClear(JtagCommand req);
   return (req.command == RdBackCmd && req.data == 0);
endfunction

function Bool isStore(JtagCommand req);
   return (req.command == RdBackStore);
endfunction

function Bool isQueueable(JtagCommand req);
   case(req.command)
      RdBackOn:    return True;
      RdBackOff:   return True;
      RdBackStore: return True;
      RdBackCmd:
      begin
	 if (isFinish(req) || isBrkCode(req) || isClear(req))
	    return True;
	 else
	    return False;
      end
      default: return False;
   endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
interface JtagReadback;
   (* always_ready, prefix = "", result = "BUSY" *)
   method    Bool     busy();
   (* always_ready, always_enabled, prefix = "" *)
   method    Action   cclock_pre_posedge((* port = "CCLOCK_PRE_POSEDGE" *)Bool i);
endinterface

(* always_ready, always_enabled *)
interface JTAG;
   method    Action   tck(Bit#(1) i);
   method    Action   tdi(Bit#(1) i);
   method    Action   tms(Bit#(1) i);
   method    Bit#(1)  tdo();
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkJtagReadback#(  parameter XilinxFamily family
		       , parameter Bool         fake_data
		       )(JtagReadback);
   
   Bool trace = False;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                           clock               <- exposeCurrentClock;
   Reset                           reset_n             <- exposeCurrentReset;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   Reg#(UInt#(64))                 rCycleStamp         <- mkReg(0);
   Reg#(ConfigNum)                 rConfigNum          <- mkReg(maxBound);
   Reg#(Bool)                      rRdBackOn           <- mkReg(False);
   Reg#(Bool)                      rPendingClear       <- mkReg(False);
   Reg#(Bool)                      rExtraRdBack        <- mkReg(False);
   Reg#(Bool)                      rStoreEmpty         <- mkReg(True);
   Reg#(Bool)                      rSampleOn           <- mkReg(False);
   Reg#(Bool)                      rExecuteSample      <- mkReg(False);
   ReadBackCore                    mCntrl              <- mkReadbackCore(fake_data, family);
   
   Bool                            doing_rdback         = rRdBackOn && mCntrl.storeNotEmpty;
   Bool                            break_condition      = mCntrl.brkSignal && doing_rdback;
   Bool                            active               = mCntrl.state == Done && ((!rExtraRdBack && mCntrl.storeIsReady && !rPendingClear) || !doing_rdback) && !break_condition;
   Wire#(Bool)                     wPrePosedge         <- mkDWire(False);
     
   InternalJtag                    mJtag               <- mkInternalJtag(1); // Use USER1 instruction
   FIFOF#(Bit#(32))                fJtagRdBack         <- mkGSizedFIFOF(False, True, 32);
   FIFOF#(Bit#(32))                fRdBack             <- mkSizedBRAMFIFOF(4096);
   FIFOF#(JtagCommand)             fCmdQueue           <- mkSizedBRAMFIFOF(8192);
   Reg#(Bool)                      rCmdQueueOvfl       <- mkReg(False);
								       
   JtagCommand                     request              = unpack(mJtag.command);
   JtagRdBkCommand                 command              = request.command;
   Reg#(Bool)                      rJtagCommandRecv    <- mkReg(False);
   PulseWire                       pwNewJtagCommand    <- mkPulseWire;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////

   // This rule takes responses from the readback core and places them into a queue
   // that is eventually transmitted back over JTAG.
   (* fire_when_enabled *)
   rule keep_rdback_fifo_full;
      let data = mCntrl.rdback.first; mCntrl.rdback.deq;
      fRdBack.enq(data);
   endrule
   
   (* fire_when_enabled *)
   rule transfer_to_jtag_rd_back;
      let data <- toGet(fRdBack).get;
      fJtagRdBack.enq(data);
   endrule
   
   // When a new command is received from the JTAG interface, the command_toggle method
   // will return a new value compared with the old one -- edge detected.  When that
   // occurrs, we assert the pulse wire stating we have a new command.
   (* fire_when_enabled, no_implicit_conditions *)
   rule track_jtag_command_toggle_flop;
      let toggle = mJtag.command_toggle();
      rJtagCommandRecv <= toggle;
      if (toggle != rJtagCommandRecv) begin
	 pwNewJtagCommand.send();
      end
   endrule
   
   // Update the cycle stamp and configuration number as necessary.
   (* fire_when_enabled, no_implicit_conditions *)
   rule update_cycle_stamp;
      mCntrl.stamp(rCycleStamp);
      mCntrl.cfg(rConfigNum);
   endrule

   // If requests are received that are directed to the readbackCore module, queue them up
   // and let the readbackCore module take them as it can.
   (* fire_when_enabled *)
   rule enqueue_requests if (pwNewJtagCommand && isQueueable(request));
      fCmdQueue.enq(request);
   endrule
   
   // Verify that we don't overflow the command queue (and possibly drop a request).  If we
   // receive a new request that should go in the queue and the queue is full, mark the overflow
   // status.
   (* fire_when_enabled, no_implicit_conditions *)
   rule check_cmd_queue_overflow if (!fCmdQueue.notFull() && pwNewJtagCommand && isQueueable(request));
      rCmdQueueOvfl <= True;
   endrule
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Process Commands from Command Queue
   ////////////////////////////////////////////////////////////////////////////////
   rule process_readback_on_request if (fCmdQueue.first().command == RdBackOn);
      fCmdQueue.deq;
      rRdBackOn <= True;
      if (trace) $display("[%t] READBACK ON", $time);
   endrule
   
   rule process_readback_off_request if (fCmdQueue.first().command == RdBackOff);
      fCmdQueue.deq;
      rRdBackOn <= False;
      if (trace) $display("[%t] READBACK OFF", $time);
   endrule
   
   rule process_clear_request if (fCmdQueue.first().command == RdBackCmd && fCmdQueue.first().data == 0);
      fCmdQueue.deq;
      rPendingClear <= True;
      if (trace) $display("[%t] CLEAR RECEIVED", $time);
   endrule
   
   rule process_finish_request if (isFinish(fCmdQueue.first()));
      fCmdQueue.deq;
      rExtraRdBack <= !rStoreEmpty;
      mCntrl.cmd(tagged Finish);
      rConfigNum <= rConfigNum + 1;
      if (trace) $display("[%t] FINISH (%d)", $time, rConfigNum);
   endrule
   
   rule process_break_code if (isBrkCode(fCmdQueue.first()));
      fCmdQueue.deq;
      mCntrl.brkCode(zExtend(request.data));
      if (trace) $display("[%t] CODE RECEIVED %04X", $time, request.data[15:0]);
   endrule
   
   rule process_readback_store if (fCmdQueue.first().command == RdBackStore);
      fCmdQueue.deq;
      rStoreEmpty <= False;
      RdBackStoreCmd scmd = cExtend(request.data);
      mCntrl.cmd(tagged Insert scmd);
      if (trace) $display("[%t] STORE ", $time, fshow(scmd));	    
   endrule      
   
   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_readback_data;
      mJtag.readback_data({ zExtend(pack(fJtagRdBack.notEmpty())), fJtagRdBack.first() }); // 2
      mJtag.readback_status({ fJtagRdBack.first(), 29'd0, pack(rCmdQueueOvfl), pack(!fCmdQueue.notFull), pack(fJtagRdBack.notEmpty) }); // 3
      mJtag.readback_cycle(pack(rCycleStamp)); // 4
      mJtag.readback_id(64'h426C75654A544147); // 7
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule process_sample_on if (pwNewJtagCommand && command == RdBackCmd && request.data == 5);
      rSampleOn <= True;
      if (trace) $display("[%t] READBACK SAMPLE ON", $time);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule process_sample_off if (pwNewJtagCommand && command == RdBackCmd && request.data == 6);
      rSampleOn <= False;
      if (trace) $display("[%t] READBACK SAMPLE OFF", $time);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule process_execute_sample if (pwNewJtagCommand && command == RdBackCmd && request.data == 8);
      rExecuteSample <= True;
      if (trace) $display("[%t] READBACK SAMPLE NOW!", $time);
   endrule
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule process_readback_dequeue if (pwNewJtagCommand && command == RdBackCmd && request.data == 10);
      fJtagRdBack.deq;
      if (trace) $display("[%t] READBACK DEQ", $time);
   endrule
  
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules to handle JTAG commands
   ////////////////////////////////////////////////////////////////////////////////
   rule process_clear if (rPendingClear);
      rPendingClear <= False;
      rStoreEmpty   <= True;
      mCntrl.clear;
      if (trace) $display("[%t] CLEAR PROCESSED", $time);
   endrule

   rule process_break if (mCntrl.state == Done && break_condition);
      mCntrl.brkClear;
      if (trace) $display("[%t] BREAK CLEAR", $time);
   endrule

   (* preempts = "process_clear, process_start_readback" *)
   rule process_start_readback if ((wPrePosedge || rExtraRdBack) && doing_rdback && (!rSampleOn || rExecuteSample));
      rExtraRdBack  <= False;
      mCntrl.startReadback;
      rExecuteSample <= False;
      if (trace) $display("[%t] START READBACK", $time);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   method    Bool   busy();
      return !active;
   endmethod
      
   method    Action cclock_pre_posedge(Bool i);
      wPrePosedge <= i;
      if (i) rCycleStamp <= rCycleStamp + 1;
   endmethod
endmodule: mkJtagReadback

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex7JtagReadback(JtagReadback);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(KINTEX7, False, reset_by rst);
   return _m;
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkKintex7JtagReadback(JtagReadback);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(KINTEX7, False, reset_by rst);
   return _m;
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex6JtagReadback(JtagReadback);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(VIRTEX6, False, reset_by rst);
   return _m;
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex7Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(KINTEX7, False, reset_by rst);

   (* fire_when_enabled, no_implicit_conditions *)
   rule every;
      _m.cclock_pre_posedge(True);
   endrule
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkKintex7Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(KINTEX7, False, reset_by rst);

   (* fire_when_enabled, no_implicit_conditions *)
   rule every;
      _m.cclock_pre_posedge(True);
   endrule
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex6Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagReadback(VIRTEX6, False, reset_by rst);

   (* fire_when_enabled, no_implicit_conditions *)
   rule every;
      _m.cclock_pre_posedge(True);
   endrule
endmodule

////////////////////////////////////////////////////////////////////////////////
/// Interface
////////////////////////////////////////////////////////////////////////////////
(* always_ready, always_enabled *)
interface InternalJtag;
   method    Bool     command_toggle();
   method    Bit#(32) command();
   method    Action   readback_data(Bit#(64) i);
   method    Action   readback_status(Bit#(64) i);
   method    Action   readback_cycle(Bit#(64) i);
   method    Action   readback_id(Bit#(64) i);
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" internal_jtag = 
module mkInternalJtag#(Integer chain)(InternalJtag);
   default_clock clk(CLK);
   default_reset rst(RST);
   
   parameter JTAG_CHAIN = chain;

   method REG_EN    command_toggle;
   method REG_CMD   command;
   method           readback_data(RDBK_DATA) enable((*inhigh*)en0);
   method           readback_status(RDBK_STATUS) enable((*inhigh*)en1);
   method           readback_cycle(RDBK_CYCLE) enable((*inhigh*)en2);
   method           readback_id(RDBK_ID) enable((*inhigh*)en3);
      
   schedule (command, command_toggle, readback_data, readback_status, readback_cycle, readback_id) CF 
            (command, command_toggle, readback_data, readback_status, readback_cycle, readback_id);   
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" JTAG_SIME2 =
module mkJTAG_SIME2#(parameter String partname)(JTAG);
   default_clock clk();
   default_reset rst();
   
   parameter PART_NAME = partname;
   
   method        tck(TCK) enable((*inhigh*)en0);
   method        tdi(TDI) enable((*inhigh*)en1);
   method        tms(TMS) enable((*inhigh*)en2);
   method TDO    tdo;
      
   schedule (tdo, tck, tdi, tms) CF (tdo, tck, tdi, tms);
endmodule

		 
endpackage: JtagReadback

