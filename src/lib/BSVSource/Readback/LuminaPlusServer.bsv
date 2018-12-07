////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2016 Bluespec, Inc.  All rights reserved.
// $Revision: 34866 $
// $Date: 2016-09-24 00:46:46 -0400 (Sat, 24 Sep 2016) $
////////////////////////////////////////////////////////////////////////////////
//  Filename      : LuminaPlusServer.bsv
//  Description   : Wrapper around the SemuServer that combines the separate
//                  data and status channels into one stream, which can be
//                  connected to a simple I/O link (rather than to a layer
//                  like SceMi which supports multiple channels).
////////////////////////////////////////////////////////////////////////////////
package LuminaPlusServer;

import Clocks::*;
import FIFO::*;
import GetPut::*;

import SemuServer::*;
import ReadbackDefines::*;

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

interface LuminaPlusServer;
   interface Put#(Bit#(32)) rx;
   interface Get#(Bit#(32)) tx;

   interface Clock cclk;
   interface Reset crst;
endinterface

interface LuminaPlusServerWithCClk;
   interface Put#(Bit#(32)) rx;
   interface Get#(Bit#(32)) tx;

   (* always_enabled *)
   method Action preedge(Bool val);
   (* always_ready *)
   method Bool allow_edge();
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkLuminaPlusServer
         #( parameter XilinxFamily family
	  , parameter Bool is_dummy
	  )
	  ( LuminaPlusServer );

   MakeClockIfc#(Bit#(1)) cclkgen <- mkUngatedClock(0);
   Clock cclock = cclkgen.new_clk;

   Reset creset <- mkAsyncResetFromCR(1, cclock);

   PulseWire rising_cclk_pw <- mkPulseWire;

   LuminaPlusServerWithCClk _server
       <- mkLuminaPlusServerWithCClk(family, is_dummy, cclock, creset);

   Reg#(Bool) initDone <- mkReg(False);

   rule toggle_cclk_reset (! initDone);
      let new_value = ~cclkgen.getClockValue();
      cclkgen.setClockValue(new_value);
      initDone <= True;
   endrule

   rule toggle_cclk_pos (initDone && (cclkgen.getClockValue == 0) && _server.allow_edge);
      let new_value = ~cclkgen.getClockValue();
      cclkgen.setClockValue(new_value);
      rising_cclk_pw.send();
   endrule

   rule toggle_cclk_neg (initDone && (cclkgen.getClockValue == 1));
      let new_value = ~cclkgen.getClockValue();
      cclkgen.setClockValue(new_value);
   endrule

   rule send_preedge;
      _server.preedge(rising_cclk_pw);
   endrule

   interface rx = _server.rx;
   interface tx = _server.tx;

   interface cclk = cclock;
   interface crst = creset;

endmodule: mkLuminaPlusServer

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkLuminaPlusServerWithCClk
         #( parameter XilinxFamily family
	  , parameter Bool is_dummy
	  )
	  ( Clock      cclock
	  , Reset      creset
	  , LuminaPlusServerWithCClk ifc
	  );
   
   SemuServer server <- mkSemuServer(family, is_dummy, cclock, creset);

   // Is a FIFO necessary?
   FIFO#(Bit#(32)) fTx <- mkFIFO;

   // Status messages are sent in 3 beats
   Reg#(Bit#(2)) status_beat_count <- mkReg(0);

   // Prepend a bit to distinguish between the channels (1 = status, 0 = rdback)
   // and give priority to the status channel, since it should be sparse

   (* descending_urgency = "tx_status, tx_rdback" *)
   rule tx_status;
      let s = server.status.first;
      Bit#(31) val;
      if (status_beat_count == 0) begin
         val = { pack(s.rdback_on), getCycleStampMSB(s.cycle) };
         status_beat_count <= status_beat_count + 1;
      end
      else if (status_beat_count == 1) begin
         val = { 1'b0, getCycleStampLSB(s.cycle) };
         status_beat_count <= status_beat_count + 1;
      end
      else begin
         val = { pack(s.running), pack(s.free_running), pack(s.edges) };
         status_beat_count <= 0;
	 server.status.deq;
      end
      fTx.enq({1'b1, val});
   endrule

   rule tx_rdback;
      Bit#(31) val = server.rdback.first;
      server.rdback.deq;
      fTx.enq({1'b0, val});
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface rx = server.cmd;
   interface tx = toGet(fTx);

   method preedge = server.preedge;
   method allow_edge = server.allow_edge;

endmodule: mkLuminaPlusServerWithCClk

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: LuminaPlusServer
