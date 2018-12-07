////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2016  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : SceMiLumina.bsv
//  Description   : Wrapper around LuminaServer that adds SceMi transport
////////////////////////////////////////////////////////////////////////////////
package SceMiLumina;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import DefaultValue      ::*;
import GetPut            ::*;
import SceMiCore         ::*;
import Vector            ::*;

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
} SceMiRdBkCommand deriving (Eq, Bits, FShow);		

typedef struct {
   SceMiRdBkCommand  command;
   Bit#(29)         data;
} SceMiCommand deriving (Bits, Eq, FShow);

function Bool isBrkCode(SceMiCommand req);
   return (req.command == RdBackCmd && req.data[28] == 1);
endfunction

function Bool isFinish(SceMiCommand req);
   return (req.command == RdBackCmd && req.data[28:27] == 2'b01);
endfunction

function Bool isClear(SceMiCommand req);
   return (req.command == RdBackCmd && req.data == 0);
endfunction

function Bool isStore(SceMiCommand req);
   return (req.command == RdBackStore);
endfunction

function Bool isReadState(SceMiCommand req);
   return (req.command == RdBackCmd && req.data == 8);
endfunction

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkSceMiLumina#( parameter XilinxFamily family
		                   , parameter Bool         is_dummy
		                   )();

   Bool trace = False;

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   LuminaServer                    mServer             <- mkLuminaServer(family, is_dummy);

   // SceMi port for incoming commands
   SceMiMessageInPortIfc#(SceMiCommand)  req_in     <- mkSceMiMessageInPort();

   // Always request data
   rule request_input;
      req_in.request();
   endrule

   // SceMi pipe for outgoing data
   SceMiOutputPipeParameters args = defaultValue;
   args.vis             = Fifo;
   args.accumulateTimer = 255;
   args.capacity        = 1023;
   SceMiOutputPipeIfc#(1,Bit#(31))       rdback_out <- mkSceMiOutputPipeP(args);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////

   // -------------------------
   // Send response data to the host

   (* aggressive_implicit_conditions *)
   rule send_output (rdback_out.can_send != 0);
      let cnt <- rdback_out.try_send (1, replicate(mServer.rdback.first), False);
      if (cnt == 1) begin
   	 if (trace) $display("(%0d) ITEM IN: %8x", $time, mServer.rdback.first);
         mServer.rdback.deq;
      end
   endrule

   // -------------------------
   // Decode commands
   // (pass server commands to the server, execute jtag commands)

   rule process_input;
      SceMiCommand cmd = req_in.read;
      if (isBrkCode(cmd)) begin
         mServer.cmd.put(tagged LuminaBreakCode truncate(cmd.data));
	 req_in.ack;
      end
      else if (isFinish(cmd)) begin
         mServer.cmd.put(tagged LuminaFinish unpack(zeroExtend(cmd.data)));
	 req_in.ack;
      end
      else if (isClear(cmd)) begin
         mServer.cmd.put(tagged LuminaClear);
	 req_in.ack;
      end
      else if (isStore(cmd)) begin
         mServer.cmd.put(tagged LuminaStore unpack(truncate(cmd.data)));
	 req_in.ack;
      end
      else if (isReadState(cmd)) begin
         mServer.cmd.put(tagged LuminaReadState);
	 req_in.ack;
      end
      else begin
         noAction; // XXX set an error bit in the state?
	 req_in.ack;
      end
   endrule

endmodule: mkSceMiLumina

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: SceMiLumina

