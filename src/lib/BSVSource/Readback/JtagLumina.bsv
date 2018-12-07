////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2016  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : JtagLumina.bsv
//  Description   : Wrapper around LuminaServer that adds JTAG transport
////////////////////////////////////////////////////////////////////////////////
package JtagLumina;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Xilinx            ::*;
import GetPut            ::*;
import Clocks            ::*;
import FShow             ::*;
import FIFO              ::*;
import FIFOF             ::*;
import BRAMFIFO          ::*;

import LuminaServer      ::*;
import ReadbackCore      ::*;
import ReadbackDefines   ::*;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////

typedef enum {
   Unknown_0,
   Unknown_1,
   Unknown_2,
   Unknown_3,
   Unknown_4,
   Unknown_5,
   RdBackCmd,
   RdBackStore
} JtagRdBkCommand deriving (Eq, Bits, FShow);		

typedef struct {
   JtagRdBkCommand  command;
   Bit#(29)         data;
} JtagCommand deriving (Bits, Eq, FShow);

function Bool isBrkCode(JtagCommand req);
   return (req.command == RdBackCmd && req.data[28] == 1);
endfunction

function Bool isFinish(JtagCommand req);
   return (req.command == RdBackCmd && req.data[28:27] == 2'b01);
endfunction

function Bool isClear(JtagCommand req);
   return (req.command == RdBackCmd && req.data == 0);
endfunction

function Bool isStore(JtagCommand req);
   return (req.command == RdBackStore);
endfunction

function Bool isReadState(JtagCommand req);
   return (req.command == RdBackCmd && req.data == 8);
endfunction

function Bool isDequeue(JtagCommand req);
   return (req.command == RdBackCmd && req.data == 10);
endfunction

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

module mkJtagLumina#( parameter XilinxFamily family
		    , parameter Bool         is_dummy
		    )();

   Bool trace = False;

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   LuminaServer                    mServer             <- mkLuminaServer(family, is_dummy);

   // A buffer to hold the Readback responses
   FIFOF#(Bit#(31))                fRdBack             <- mkSizedBRAMFIFOF(4096);
   // An unguarded FIFOF dequeue makes it easier to pass the data to JTAG
   // XXX (or at least the original author thought so)
   // so we move the elements from fRdBack to this FIFO first
   FIFOF#(Bit#(31))                fJtagRdBack         <- mkGSizedFIFOF(False, True, 32);

   // A buffer to hold incoming commands
   FIFOF#(JtagCommand)             fCmdQueue           <- mkSizedBRAMFIFOF(8192);
   // Rather an unguarded enqueue, we use a wire to separate the receive rule
   // from the implicit condition and report when there is a failure (overflow)
   PulseWire                       pwNewJtagCommand    <- mkPulseWire;
   Reg#(Bool)                      rCmdQueueOvfl       <- mkReg(False);

   InternalJtag                    mJtag               <- mkInternalJtag(1); // Use USER1 instruction

   // The arrivial of a JTAG command is signalled by toggling the enable signal
   // so we record the previous value, to detect changes
   Reg#(Bool)                      rJtagCommandRecv    <- mkReg(False);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////

   // -------------------------
   // Send response data to the host

   // This rule takes responses from the readback core and places them into a queue
   // that is eventually transmitted back over JTAG.
   (* fire_when_enabled *)
   rule keep_rdback_fifo_full;
      let data = mServer.rdback.first; mServer.rdback.deq;
      fRdBack.enq(data);
   endrule
   
   (* fire_when_enabled *)
   rule transfer_to_jtag_rd_back;
      let data <- toGet(fRdBack).get;
      fJtagRdBack.enq(data);
   endrule
   
   // -------------------------
   // Receive commands to the buffer

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

   // Move commands into the command queue
   (* fire_when_enabled *)
   rule enqueue_requests if (pwNewJtagCommand);
      JtagCommand request = unpack(mJtag.command);
      fCmdQueue.enq(request);
   endrule
   
   // Verify that we don't overflow the command queue (and possibly drop a request).
   // If we receive a new request and the queue is full, mark the overflow status.
   (* fire_when_enabled, no_implicit_conditions *)
   rule check_cmd_queue_overflow if ( pwNewJtagCommand && !fCmdQueue.notFull());
      rCmdQueueOvfl <= True;
   endrule

   // -------------------------
   // Decode commands
   // (pass server commands to the server, execute jtag commands)

   rule process_request;
      JtagCommand cmd <- toGet(fCmdQueue).get();
      if (isBrkCode(cmd))
         mServer.cmd.put(tagged LuminaBreakCode truncate(cmd.data));
      else if (isFinish(cmd))
         mServer.cmd.put(tagged LuminaFinish unpack(zeroExtend(cmd.data)));
      else if (isClear(cmd))
         mServer.cmd.put(tagged LuminaClear);
      else if (isStore(cmd))
         mServer.cmd.put(tagged LuminaStore unpack(truncate(cmd.data)));
      else if (isReadState(cmd))
         mServer.cmd.put(tagged LuminaReadState);
      else if (isDequeue(cmd))
         fJtagRdBack.deq;
      else
         noAction; // XXX set an error bit in the state?
   endrule

   // -------------------------
   // JTAG status values

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_readback_data;
      mJtag.readback_data({ zeroExtend(pack(fJtagRdBack.notEmpty())), fJtagRdBack.first() }); // 2
      mJtag.readback_status({ 1'b0, fJtagRdBack.first(), 29'd0, pack(rCmdQueueOvfl), pack(!fCmdQueue.notFull), pack(fJtagRdBack.notEmpty) }); // 3
      mJtag.readback_cycle(zeroExtend(pack(mServer.cycle()))); // 4
      mJtag.readback_id(64'h426C75654A544147); // 7
   endrule

endmodule: mkJtagLumina

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex7Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagLumina(KINTEX7, False, reset_by rst);
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkKintex7Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagLumina(KINTEX7, False, reset_by rst);
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex6Lumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagLumina(VIRTEX6, False, reset_by rst);
endmodule

(* synthesize *)
(* default_clock_osc = "UCLOCK", default_reset = "URESET_N" *)
module mkVirtex7DummyLumina(Empty);
   (* hide_all *)
   let clk <- exposeCurrentClock;
   let rst <- mkAsyncResetFromCR(100, clk);
   let _m <- mkJtagLumina(KINTEX7, True, reset_by rst);
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
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

endpackage: JtagLumina

