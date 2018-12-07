////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2016 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : SceMiSemu.bsv
//  Description   : Wrapper around SemuServer that adds SceMi transport and
//                  clock control
////////////////////////////////////////////////////////////////////////////////
package SceMiSemu;

import DefaultValue::*;
import FIFOF::*;
import GetPut::*;
import SceMiCore::*;
import SceMiXactors::*;
import Vector::*;

import ReadbackDefines::*;
import SemuServer::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkRdBackControl#(parameter XilinxFamily family,
				      parameter Bool is_dummy,
				      parameter SceMiClockConfiguration conf)
   (Empty ifc);
   
   Clock uclock <- exposeCurrentClock;
   
   SceMiClockPortIfc               clk_port            <- mkSceMiClockPort(conf);
   let cclock = clk_port.cclock;
   let creset = clk_port.creset;

   SceMiMessageInPortIfc#(RdBackControlReq)  req_in     <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(Bit#(32))         resp_out   <- mkSceMiMessageOutPort();
   
   SceMiOutputPipeParameters args = defaultValue;
   args.vis             = Fifo;
   args.accumulateTimer = 255;
   args.capacity        = 1023;
   SceMiOutputPipeIfc#(1,Bit#(31))           rdback_out <- mkSceMiOutputPipeP(args);

   SemuServer mServer <- mkSemuServer(family, is_dummy, cclock, creset);

   SceMiClockControlIfc clk_cntrl <- mkSceMiClockControl(conf.clockNum, mServer.allow_edge, mServer.allow_edge);

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   rule set_preedge;
     Bool val = clk_cntrl.pre_posedge || clk_cntrl.pre_negedge;
     mServer.preedge(val);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   (* aggressive_implicit_conditions *)
   rule putData (rdback_out.can_send != 0);
      let cnt <- rdback_out.try_send (1, replicate(mServer.rdback.first), False);
      if (cnt == 1) begin
   	 // $display("(%0d) ITEM IN: %8x", $time, mServer.rdback.first);
         mServer.rdback.deq;
      end
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// 
   ////////////////////////////////////////////////////////////////////////////////
   
   rule do_read;
      mServer.cmd.put(req_in.read);
      req_in.ack;
   endrule
   
   // Calling request every cycle for read above
   rule request;
      req_in.request();
   endrule: request

   ////////////////////////////////////////////////////////////////////////////////
   /// 
   ////////////////////////////////////////////////////////////////////////////////
   
   rule send_response;
      let s = mServer.status.first;
      mServer.status.deq;
      Bit#(32) resp = { pack(s.running), pack(s.free_running), pack(s.rdback_on), pack(s.edges) };
      resp_out.send(resp);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// 
   ////////////////////////////////////////////////////////////////////////////////
   
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: SceMiSemu
