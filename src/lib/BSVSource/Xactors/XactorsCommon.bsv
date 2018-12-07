// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

import XactorsDefines::*;

import SceMi::*;
import GetPut::*;
import ClientServer::*;


module [SceMiModule] mkClientStream #( TLMXActorArgs args
                                      ) ( SceMiClientXactor#(req_ty, resp_ty) )
   provisos (Bits#(resp_ty,x1)
             ,Bits#(req_ty, x2)
             );

  // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   Put#(req_ty) outstreamPut;
   Get#(resp_ty) instreamGet;

   if (args.usePipes) begin
      Integer    inPipeDepth  = args.inPipeDepth;
      Visibility inPipeVis    = args.inPipeVis;
      Integer    outPipeDepth = args.outPipeDepth;
      Visibility outPipeVis   = args.outPipeVis;

      SceMiInputPipeIfc#(1,resp_ty)   instream <- mkSceMiInputPipe(inPipeDepth, inPipeVis);
      SceMiOutputPipeIfc#(1,req_ty)  outstream <-  mkSceMiOutputPipe(outPipeDepth, outPipeVis);

      outstreamPut = toPut(outstream);
      instreamGet  = toGet(instream);

   end
   else begin
      SceMiMessageInPortIfc#(resp_ty)   instream <- mkSceMiMessageInPort();
      SceMiMessageOutPortIfc#(req_ty)  outstream <- mkSceMiMessageOutPort();
      outstreamPut = toPut(outstream);
      instreamGet  = toGet(instream);

      rule request;
         instream.request();
      endrule
   end

   interface Server server;
      interface request =  outstreamPut;
      interface response = instreamGet;
   endinterface
   interface uclock = uclk;
   interface ureset = urst;


endmodule

module [SceMiModule] mkServerStream #( TLMXActorArgs args
                                      ) ( SceMiServerXactor#(req_ty, resp_ty) )
   provisos (Bits#(resp_ty,x1)
             ,Bits#(req_ty, x2)
             );

  // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   Put#(resp_ty) outstreamPut;
   Get#(req_ty) instreamGet;

   if (args.usePipes) begin
      Integer    inPipeDepth  = args.inPipeDepth;
      Visibility inPipeVis    = args.inPipeVis;
      Integer    outPipeDepth = args.outPipeDepth;
      Visibility outPipeVis   = args.outPipeVis;

      SceMiInputPipeIfc#(1,req_ty)   instream <- mkSceMiInputPipe(inPipeDepth, inPipeVis);
      SceMiOutputPipeIfc#(1,resp_ty)  outstream <-  mkSceMiOutputPipe(outPipeDepth, outPipeVis);

      outstreamPut = toPut(outstream);
      instreamGet  = toGet(instream);

   end
   else begin
      SceMiMessageInPortIfc#(req_ty)   instream <- mkSceMiMessageInPort();
      SceMiMessageOutPortIfc#(resp_ty)  outstream <- mkSceMiMessageOutPort();
      outstreamPut = toPut(outstream);
      instreamGet  = toGet(instream);

      rule request;
         instream.request();
      endrule
   end

   interface Client client;
      interface request =  instreamGet;
      interface response = outstreamPut;
   endinterface
   interface uclock = uclk;
   interface ureset = urst;


endmodule
