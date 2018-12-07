// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SimTbProxies;

import FIFO::*;
import GetPut::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

// Currently we only use the BVI version as we want the interpreter to
// start in the initial block (and we can't do that in BSV)

// module mkSimTbInterp#(UInt#(32) port) (Empty);

//    Reg#(Bool)      initialized <- mkReg(False);

//    rule start (!initialized);
//       let mbe <- bsvsimtb_interp_start(port);
//       case (mbe) matches
// 	 tagged Invalid : 
// 	    begin
// 	       $display("FATAL ERROR: Unable to start interpreter!");
// 	       $finish(0);
// 	    end
// 	 tagged Valid .index :
// 	    begin
// 	       $display("Interpreter successfully started using port %d.", port);
// 	       initialized <= True;
// 	    end
//       endcase
//    endrule

// endmodule

import "BVI" SimTbInterp =
module simTbInterp#(parameter UInt#(32) port) (Empty);
   
   parameter PORT = port;
   
   default_clock clk(CLK);
   default_reset rst(RST_N);

endmodule


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import "BDPI" function ActionValue#(Maybe#(void))      bsvsimtb_interp_start(UInt#(32) port);
import "BDPI" function ActionValue#(Maybe#(UInt#(32))) bsvsimtb_interp_message_get(UInt#(32) channel);
import "BDPI" function ActionValue#(Maybe#(void))      bsvsimtb_interp_message_send(UInt#(32) channel, UInt#(32) value);
		 
import "BDPI" function Action                          emu_stop();
import "BDPI" function Action                          emu_run(UInt#(32) number);
		 
import "BDPI" function Action                          timer_clear();		 
import "BDPI" function Action                          timer_show();
		 
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module sceMiLocalMessagePort#(parameter UInt#(32) channel) (Get#(UInt#(32)));
   
   // create an interpreter if one doesn't already exist
   let interp <- simTbInterp(0);
   
   FIFO#(UInt#(32)) fifo <- mkFIFO;
   
   rule every;
      let value <- bsvsimtb_interp_message_get(channel);
      if (value matches tagged Valid .v) 
	 begin
//	    $display("RECV MESSAGE: %d", v);
	    fifo.enq(v);
	 end
   endrule
   
   method ActionValue#(UInt#(32)) get;
      fifo.deq;
      return fifo.first;
   endmethod
   
endmodule

module mkSceMiLocalMessagePort#(Integer channel) (Get#(UInt#(32)));
   
   FIFO#(UInt#(32)) fifo <- mkFIFO;
   
   rule every;
      let value <- bsvsimtb_interp_message_get(fromInteger(channel));
      if (value matches tagged Valid .v) fifo.enq(v);
   endrule
   
   method ActionValue#(UInt#(32)) get;
      fifo.deq;
      return fifo.first;
   endmethod
   
endmodule		 
		 
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Cmd#(type a);
   (* always_ready *)
   method a      cmd;
   (* always_ready *)
   method Bool   isVerbose();
   (* always_ready *)
   method Action clear();
endinterface

(* synthesize *)
module messageSupport#(parameter UInt#(32) port) (Cmd#(UInt#(32)));
   
   Reg#(Bool)      verbose  <- mkReg(False);
   Reg#(UInt#(32)) value    <- mkReg(maxBound);
   PulseWire       do_clear <- mkPulseWire;

   let msg <- sceMiLocalMessagePort(port);
   
   rule handle_msg (!do_clear);
      let m <- msg.get;
      if (m == 5001) // verbose on
	 begin
	    verbose <= True;
	    $display("Turning verbose mode on.");
	 end
      if (m == 5000) // verbose off
	 begin
	    verbose <= False;
	    $display("Turning verbose mode off.");
	 end
      if (m != 5000 && m != 5001)
	 begin
	    value <= m;
	 end
   endrule
   
   interface cmd       = value;
   method    isVerbose = verbose;
   method Action clear();
      do_clear.send;
      value <= maxBound;
   endmethod
endmodule

endpackage