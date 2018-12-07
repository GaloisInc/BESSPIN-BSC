// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiXactors;

import SceMiCore::*;
import SceMiDefines::*;

import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Clocks::*;
import DefaultValue::*;
import LBus::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import AlignedFIFOs ::*;
import NullCrossingFIFOF::*;

function SyncFIFOIfc#(t) stripClr(FIFOF#(t) ff) =
   ( interface SyncFIFOIfc;
	method enq = ff.enq;
	method deq = ff.deq;
	method first = ff.first;
	method notFull = ff.notFull;
	method notEmpty = ff.notEmpty;
     endinterface );

module mkAppropriateFIFO#( Integer depthIn
			  )(Clock sClkIn, Reset sRstIn,
			    Clock dClkIn,
			    SyncFIFOIfc #(a_type) ifc )
   provisos (Bits#(a_type,sa));

   SyncFIFOIfc #(a_type) _ifc;
   if (isAncestor(sClkIn, dClkIn)) begin
      FIFOF#(a_type) ff <- mkSizedFIFOF(depthIn, clocked_by sClkIn, reset_by sRstIn);
      _ifc = stripClr(ff);
   end
   else if (isAncestor(dClkIn, sClkIn)) begin
      FIFOF#(a_type) ff <- mkSizedFIFOF(depthIn, clocked_by dClkIn, reset_by sRstIn);
      _ifc = stripClr(ff);
   end

   else _ifc <- mkSyncFIFO(depthIn, sClkIn, sRstIn, dClkIn);
   return _ifc;
endmodule

module [SceMiModule] mkInPortXactor #(SceMiClockPortIfc clk_port ) (Get#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   // A SyncFIFO (of depth 2) which servers to cross the data into
   // the controlled domain and to store the data until it can be taken
   SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,uclock,ureset,cclock);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inport));
   rule request;
      inport.request();
   endrule

   return toGet(res_fifo);

endmodule

module [SceMiModule] mkInPortStallXactor #(parameter Integer clockNum,
                                           parameter Bool allowFirstClock,
                                           SceMiClockPortIfc clk_port ) (Get#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   // Cross into the controlled domain and to store the data
   SceMiCrossingReg#(ty) crossReg <- mkUtoCNullCrossingReg(uclock,ureset,cclock,creset);

   // Clock control
   Reg#(Bool)  allowC <- mkReg(allowFirstClock, clocked_by uclock, reset_by ureset);

   // Allow only when data is present in fifo.
   Bool allow_clock = allowC;
   SceMiClockControlIfc clkControl <- mkSceMiClockControl(clockNum, allow_clock, allow_clock);

   (* no_implicit_conditions, fire_when_enabled *)
   rule setAllow (!allowC && crossReg.writtenPulse);
      allowC <= True;
   endrule
   (* fire_when_enabled *)
   rule clrAllow (allowC && clkControl.pre_posedge);
      allowC <= False;
   endrule

   rule request (True);
      inport.request();
   endrule
   rule transfer (True);
      inport.ack;
      crossReg.regifc._write (inport.read);
   endrule


   return toGet(crossReg.regifc);
endmodule


module [SceMiModule] mkInPipeXactor#(Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Get#(ty))
   provisos(  Bits#(ty, ty_sz)
	    );
   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, ty) inpipe <- mkSceMiInputPipe(depth, style);

   // A SyncFIFO (of depth 4) which servers to cross the data into
   // the controlled domain and to store the data until it can be taken
   SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(4,uclock,ureset,cclock);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inpipe));

   return toGet(res_fifo);
endmodule

module [SceMiModule] mkOutPortXactor#(SceMiClockPortIfc clk_port ) (Put#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   // A SyncFIFO (of depth 2) which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be served
   SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,cclock,creset,uclock);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outport));

   return toPut(res_fifo);

endmodule

// Stalls clock when the outport is not ready
module [SceMiModule] mkOutPortStallXactor#(parameter Integer clockNum, SceMiClockPortIfc clk_port ) (Put#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   // A SyncFIFO which servers to cross the data into
   // the uncontrolled domain and to store the data until it can be served
   Wire#(ty)     datain   <- mkBypassWire;
   ReadOnly#(ty) crossing <- mkNullCrossing(uclock, datain);
   CrossingReg#(Bool)  ct <- mkNullCrossingRegA (uclock, False, clocked_by cclock, reset_by creset);

   Reg#(Bool)    ut <- mkRegA( False, clocked_by uclock, reset_by ureset);

   (* no_implicit_conditions, fire_when_enabled *)
   rule everyCclock (True);
      ct <= ! ct;
   endrule
   (* fire_when_enabled *)
   rule capture (ct.crossed != ut );
      ut <= ct.crossed;
      outport.send (crossing);
   endrule

   Bool allow_clock = (ct.crossed == ut) && (outport.accepting_data);
   SceMiClockControlIfc clkControl <- mkSceMiClockControl(clockNum, allow_clock, allow_clock);

   method Action put (ty x);
      datain <= x;
   endmethod

endmodule

module [SceMiModule] mkOutPipeXactor#(Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Put#(ty))
   provisos (  Bits#(ty, ty_sz)
	     , Add#(a__, ty_sz, TMul#(TDiv#(ty_sz, 8), 8))
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiOutputPipeIfc#(1, ty) outpipe <- mkSceMiOutputPipe(depth, style);

   // A SyncFIFO (of depth 4) which servers to cross the data into
   // the uncontrolled domain and to store the data until it can be served
   SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(4,cclock,creset,uclock);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outpipe));

   return toPut(res_fifo);
endmodule

// =========================
// Put Xactor

// Connects to a DUT with a Put interface
// This version does not stall the clock, so there are no guarantees
// about when data from the SW arrives.

module [SceMiModule] mkPutXactor #(Put#(ty) putifc, SceMiClockPortIfc clk_port ) (Empty)
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   if (cclock == uclock && creset == ureset)
      begin
         let connect_put <- mkConnection(toGet(inport), putifc);
      end
   else
      begin
          // A SyncFIFO (of depth 2) which serves to cross the data into
         // the controlled domain and to store the data until it can be taken
         SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,uclock,ureset,cclock);

         let connect_res <- mkConnection(toPut(res_fifo), toGet(inport));
         let connect_put <- mkConnection(toGet(res_fifo), putifc);
      end

   rule request;
      inport.request();
   endrule

endmodule

module [SceMiModule] mkPutPipeXactor #(Put#(ty) putifc, Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Empty)
   provisos (  Bits#(ty, ty_sz)
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, ty) inpipe <- mkSceMiInputPipe(depth, style);

   if (cclock == uclock && creset == ureset)
      begin
         let connect_put <- mkConnection(toGet(inpipe), putifc);
      end
   else
      begin
          // A SyncFIFO (of depth 2) which serves to cross the data into
         // the controlled domain and to store the data until it can be taken
         SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,uclock,ureset,cclock);

         let connect_res <- mkConnection(toPut(res_fifo), toGet(inpipe));
         let connect_put <- mkConnection(toGet(res_fifo), putifc);
      end
endmodule

// =========================
// Get Xactor

// Connects to a DUT with a Get interface
// This version does not stall the clock, so there are no guarantees
// about how many cycles in the HW will pass before the SW handles
// the data

module [SceMiModule] mkGetXactor#(Get#(ty) getifc, SceMiClockPortIfc clk_port ) (Empty)
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   if (cclock == uclock && creset == ureset)
      begin
         let connect_get <- mkConnection(toPut(outport), getifc);
      end
   else
      begin
         // A SyncFIFO (of depth 2) which servers to cross the data into
         // the uncontrolled domain and to store the data until it can be served
         SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,cclock,creset,uclock);

         let connect_res <- mkConnection(toGet(res_fifo), toPut(outport));
         let connect_get <- mkConnection(toPut(res_fifo), getifc);
      end
endmodule

module [SceMiModule] mkGetPipeXactor #(Get#(ty) getifc, Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Empty)
   provisos (  Bits#(ty, ty_sz)
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiOutputPipeIfc#(1, ty) outpipe <- mkSceMiOutputPipe(depth, style);

   if (cclock == uclock && creset == ureset)
      begin
         let connect_get <- mkConnection(toPut(outpipe), getifc);
      end
   else
      begin
         // A SyncFIFO (of depth 2) which servers to cross the data into
         // the uncontrolled domain and to store the data until it can be served
         SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(2,cclock,creset,uclock);

         let connect_res <- mkConnection(toGet(res_fifo), toPut(outpipe));
         let connect_get <- mkConnection(toPut(res_fifo), getifc);
      end
endmodule

////////////////////////////////////////////////////////////////////////////////
// Server Transactors
////////////////////////////////////////////////////////////////////////////////

interface SceMiServerXactor#(type req, type resp);
   interface Client#(req, resp) client;
   interface Clock  uclock;
   interface Reset ureset ;
endinterface

// Deals with a DUT with a Server interface
// This version just instantiates channels in both directions,
// with no stalling of the clock

module [SceMiModule]  mkServerXactor #(Server#(req_ty, resp_ty) server, SceMiClockPortIfc clk_port ) (Empty)
   provisos (Bits#(req_ty, req_ty_sz),
             Bits#(resp_ty, resp_ty_sz));

   let req <- mkPutXactor(server.request, clk_port);
   let resp <- mkGetXactor(server.response, clk_port);

endmodule

module [SceMiModule] mkServerStreamXactor (SceMiServerXactor#(req_ty, resp_ty))
   provisos(  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
            );

   // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   SceMiMessageInPortIfc#(req_ty)   instream <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(resp_ty) outstream <- mkSceMiMessageOutPort();

   rule request;
      instream.request();
   endrule

   interface Client client;
      interface request = toGet(instream);
      interface response = toPut(outstream);
   endinterface
   interface uclock = uclk;
   interface ureset = urst;
endmodule

module [SceMiModule] mkServerPipeXactor #(Server#(req_ty, resp_ty) server, Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Empty)
   provisos (  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
             , Add#(c__, req_ty_sz, TMul#(TDiv#(req_ty_sz, 8), 8))
	     );
   let req <- mkPutPipeXactor(server.request, depth, style, clk_port);
   let resp <- mkGetPipeXactor(server.response, depth, style, clk_port);
endmodule

// Another variation of the above with expected names
module [SceMiModule] mkPipeXactorMaster #(Server#(req_ty, resp_ty) server, Integer depth, Visibility style, SceMiClockPortIfc clk_port) (Empty)
   provisos (  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
             , Add#(c__, req_ty_sz, TMul#(TDiv#(req_ty_sz, 8), 8))
	     );
   let inpipe  <- mkPutPipeXactor(server.request, depth, style, clk_port);
   let outpipe <- mkGetPipeXactor(server.response, depth, style, clk_port);
endmodule

module [SceMiModule] mkServerStreamPipeXactor #(Integer inDepth,
                                                Integer outDepth
                                                ) (SceMiServerXactor#(req_ty, resp_ty) client )
   provisos(  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
            );

   // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   SceMiInputPipeIfc#(1,req_ty)   instream <- mkSceMiInputPipe(inDepth, Fifo);
   SceMiOutputPipeIfc#(1,resp_ty) outstream <- mkSceMiOutputPipe(outDepth, Fifo);

   interface Client client;
      interface request = toGet(instream);
      interface response = toPut(outstream);
   endinterface
   interface uclock = uclk;
   interface ureset = urst;
endmodule


////////////////////////////////////////////////////////////////////////////////
// Client Transactors
////////////////////////////////////////////////////////////////////////////////


interface SceMiClientXactor#(type req, type resp);
   interface Server#(req, resp) server;
   interface Clock  uclock;
   interface Reset ureset ;
endinterface

// Deals with a DUT with a Client interface
// This version just instantiates channels in both directions,
// with no stalling of the clock

module [SceMiModule] mkClientXactor #(Client#(req_ty, resp_ty) client, SceMiClockPortIfc clk_port ) (Empty)
   provisos (Bits#(req_ty, req_ty_sz),
             Bits#(resp_ty, resp_ty_sz));

   let req <- mkGetXactor(client.request, clk_port);
   let resp <- mkPutXactor(client.response, clk_port);

endmodule

module [SceMiModule] mkClientStreamXactor (SceMiClientXactor#(req_ty, resp_ty))
   provisos(  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
            );

   // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   SceMiMessageInPortIfc#(resp_ty)   instream <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(req_ty)  outstream <- mkSceMiMessageOutPort();

   rule request;
      instream.request();
   endrule

   interface Server server;
      interface request = toPut(outstream);
      interface response = toGet(instream);
   endinterface
   interface uclock = uclk;
   interface ureset = urst;
endmodule


module [SceMiModule] mkClientPipeXactor #(Client#(req_ty, resp_ty) client, Integer depth, Visibility style, SceMiClockPortIfc clk_port)(Empty)
   provisos (  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
             , Add#(a__, resp_ty_sz, TMul#(TDiv#(resp_ty_sz, 8), 8))
	     );
   let req <- mkGetPipeXactor(client.request, depth, style, clk_port);
   let resp <- mkPutPipeXactor(client.response, depth, style, clk_port);
endmodule

// Another variation of the above with expected names
module [SceMiModule] mkPipeXactorSlave #(Client#(req_ty, resp_ty) client, Integer depth, Visibility style, SceMiClockPortIfc clk_port)(Empty)
   provisos (  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
             , Add#(a__, resp_ty_sz, TMul#(TDiv#(resp_ty_sz, 8), 8))
	     );
   let outpipe <- mkGetPipeXactor(client.request, depth, style, clk_port);
   let inpipe <- mkPutPipeXactor(client.response, depth, style, clk_port);
endmodule

module [SceMiModule] mkClientStreamPipeXactor #(Integer inDepth,
                                                Integer outDepth
                                                )(SceMiClientXactor#(req_ty, resp_ty) client )
   provisos(  Bits#(req_ty, req_ty_sz)
             , Bits#(resp_ty, resp_ty_sz)
            );

   // Access the uncontrolled clock and reset
   Clock uclk <- sceMiGetUClock();
   Reset urst <- sceMiGetUReset();

   SceMiInputPipeIfc#(1,resp_ty)   instream <- mkSceMiInputPipe(inDepth, Fifo);
   SceMiOutputPipeIfc#(1,req_ty)  outstream <-  mkSceMiOutputPipe(outDepth, Fifo);

   interface Server server;
      interface request = toPut(outstream);
      interface response = toGet(instream);
   endinterface
   interface uclock = uclk;
   interface ureset = urst;
endmodule


// =========================
// Value Xactor

module [SceMiModule] mkValueXactor #(SceMiClockPortIfc clk_port, ty initVal) (ReadOnly#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock
   Clock cclock = clk_port.cclock;

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   // A register to hold incoming data and to provide the value in
   // the controlled domain
   CrossingReg#(ty) rg <- mkNullCrossingReg(cclock, initVal);

   rule connect;
      let val <- toGet(inport).get();
      rg <= val;
   endrule

   rule request;
      inport.request();
   endrule

   // Outgoing data comes the FIFO
   method _read = rg.crossed();

endmodule

// =========================
// Shutdown Transactor -- stop simulation from the TB side

module [SceMiModule] mkShutdownXactor ();

   SceMiMessageInPortIfc#(Bool)  ctrl_in  <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(Bool) ctrl_out <- mkSceMiMessageOutPort();

   Clock uClock <- sceMiGetUClock();
   Reset uReset <- sceMiGetUReset();
   Reg#(Bool) shutdown_pending <- mkReg(False, clocked_by uClock,
                                               reset_by uReset);
   rule request;
      ctrl_in.request();
   endrule: request

   rule ack;
      Bool shutdown <- toGet(ctrl_in).get();
      if (shutdown)
      begin
         ctrl_out.send(True);
         shutdown_pending <= True;
      end
   endrule: ack

   // When we are sure that the ack was received, shutdown
   rule shutdown (shutdown_pending && ctrl_out.accepting_data());
      $finish(0);
   endrule: shutdown

endmodule: mkShutdownXactor

// =========================
// Simulation-like clock control from the TB

typedef enum { Edges,Query,Stop,Resume } SimCommand deriving (Eq,Bits);
typedef Bit#(32) SimulationControlReq;
typedef Bit#(32) SimulationStatusResp;

function SimCommand command(SimulationControlReq req);
   return unpack(req[31:30]);
endfunction

function UInt#(30) edges(SimulationControlReq req);
   return unpack(req[29:0]);
endfunction

function SimulationStatusResp mkStatus(Bool running, Bool freerunning, UInt#(30) edge_count);
   return {pack(running), pack(freerunning) ,pack(edge_count)};
endfunction

module [SceMiModule] mkSimulationControl#( parameter SceMiClockConfiguration conf
                                         )
                                         ( Empty ifc );

   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   Reg#(Bool)      stopped        <- mkReg(True,  clocked_by uclock, reset_by ureset);
   Reg#(UInt#(30)) edges_to_allow <- mkReg(0,     clocked_by uclock, reset_by ureset);
   Reg#(Bool)      send_status    <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)      free_running   <- mkReg(False, clocked_by uclock, reset_by ureset);

   SceMiMessageInPortIfc#(SimulationControlReq) req_in <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(SimulationStatusResp) resp_out <- mkSceMiMessageOutPort();

   Bool active = !stopped && (edges_to_allow != 0);

   SceMiClockControlIfc clk_cntrl <- mkSceMiClockControl(conf.clockNum, active, active);

   Wire#(SimulationControlReq) req <- mkWire(clocked_by uclock, reset_by ureset);

   rule read;
      let x <- toGet(req_in).get;
      req <= x;
   endrule

   // Calling request every cycle for read above
   rule request;
      req_in.request();
   endrule: request

   // Edges command now has additional features.
   //  If the number of edges received:
   //   - 'h3FFFFFFF the simulation will be free_running mode (nonstop)
   rule handle_edge_request if (command(req) == Edges && !active);
      UInt#(30) numedges = edges(req);
      stopped <= False;
      free_running <=  (numedges == maxBound);
      edges_to_allow <= numedges;
   endrule

   rule handle_query_request if (command(req) == Query && !send_status);
      send_status <= True;
   endrule

   rule handle_stop_or_resume_request if (!send_status && (command(req) == Stop || command(req) == Resume));
      send_status <= True;
      stopped <= (command(req) == Stop);
   endrule

   Bool detected_posedge = (conf.dutyLo != 0) && clk_cntrl.pre_posedge();
   Bool detected_negedge = (conf.dutyHi != 0) && clk_cntrl.pre_negedge();
   rule count_edge if (active && (detected_posedge || detected_negedge));
      // If it is free running then count down to 1
      //  and then let edges allowed go back up to max
      if (edges_to_allow == 1) send_status <= True;
      if (free_running && edges_to_allow == 1)
         edges_to_allow <= maxBound;
      else begin
         edges_to_allow <= edges_to_allow - 1;
      end
   endrule

   rule send_response if (send_status);
      SimulationStatusResp resp = mkStatus(!stopped,free_running,edges_to_allow);
      // When the free running mode is stopped, clear the model and edge counter after sending the response
      (*split*)
      if ( stopped && free_running ) begin
         free_running <= False;
         edges_to_allow <= 0;
      end
      resp_out.send(resp);
      send_status <= False;
   endrule

endmodule: mkSimulationControl


// =========================

// SCEMI XACTOR:

// This transactor provides communication of a LBusContextIfc to the host side
// of a SceMi link.  This version just instantiates channels in both
// directions, with no stalling of the clock

module [SceMiModule]  mkLBusXactor #(LBusContextIfcP#(sa,sd) lbi,
                                     SceMiClockPortIfc clk_port ) (Empty);
   FIFO#(LbReq#(sa, sd)) reqFF <- mkBypassFIFO(clocked_by clockOf(lbi.req.put),
					       reset_by resetOf(lbi.req.put));
   Reg#(Bool) rdy <- mkReg(True,
                           clocked_by clockOf(lbi.req.put),
                           reset_by   resetOf(lbi.req.put));
   rule xmitReq (rdy);
      let x = reqFF.first();
      reqFF.deq();
      lbi.req.put(tagged Valid x);
      rdy <= False;
   endrule
   (*preempts = "xmitReq, xmitNonReq"*)
   rule xmitNonReq;
      lbi.req.put(tagged Invalid);
      rdy <= True;
   endrule

   FIFO#(Bit#(sd))              respFF <- mkBypassFIFO(clocked_by clockOf(lbi.ack.get),
                                                       reset_by resetOf(lbi.ack.get));
   rule xmitResp;
      let x <- lbi.ack.get();
      if (x matches tagged Valid .a) respFF.enq(a);
   endrule

   FIFO#(Bool) inrptFF <- mkBypassFIFO(clocked_by clockOf(lbi.inrpt.get),
                                       reset_by resetOf(lbi.inrpt.get));
   Reg#(Bool)  lastIV  <- mkReg(False, clocked_by clockOf(lbi.inrpt.get),
                                       reset_by resetOf(lbi.inrpt.get));
   rule xmitInrpt;
      let x0 <- lbi.inrpt.get();
      Bool x = unpack(x0);
      if (x!=lastIV) begin
         lastIV <= x;
         inrptFF.enq(x);
      end
   endrule

   let req   <- mkPutXactor(toPut(reqFF), clk_port);
   let ack   <- mkGetXactor(toGet(respFF), clk_port);
   let inrpt <- mkGetXactor(toGet(inrptFF), clk_port);
endmodule

// =========================
// Reset Xactor

// Connects to a DUT with a Get interface

interface SceMiResetXactorIfc;
   interface Reset new_rst;
endinterface

module [SceMiModule] mkSceMiResetXactor#(SceMiClockPortIfc clk_port) (SceMiResetXactorIfc ifc);

   SceMiMessageInPortIfc#(UInt#(16))  ctrl_in  <- mkSceMiMessageInPort();
   SceMiMessageOutPortIfc#(Bool) ctrl_out <- mkSceMiMessageOutPort();

   Clock uClock <- sceMiGetUClock();
   Reset uReset <- sceMiGetUReset();
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Reset cycles coming from inport
   CrossingFIFOF#(UInt#(16)) reset_cycles <- mkUtoCNullCrossingFIFOF(uClock,uReset,cclock,creset);

   // Pulse for ending reset (cclock domain to uclock domain)
   SyncPulseIfc end_reset <- mkSyncPulse(cclock, creset, uClock);

   // Counters for counting down number of reset cycles (cclock domain)
   Reg#(UInt#(16)) reset_count <- mkReg(0, clocked_by cclock, reset_by creset);

   // State of reset assertion
   Reg#(Bool) assert_reset <- mkReg(False, clocked_by uClock, reset_by uReset);

   // Reset generator (uclock domain)
   MakeResetIfc reset_sig <- mkReset(0, False, uClock);

   // Reset joiner for joining this reset and scemi reset
   Reset join_reset <- mkResetEither(reset_sig.new_rst, uReset);


   rule request;
      ctrl_in.request();
   endrule: request

   rule receive_reset;
      let rval <- toGet(ctrl_in).get();
      reset_cycles.fifo.enq(rval-1);
      if (rval > 0) begin
	 assert_reset <= True;
      end
      ctrl_out.send(True);
   endrule: receive_reset

   rule set_reset_count;
      reset_count <= reset_cycles.fifo.first();
      reset_cycles.fifo.deq();
   endrule: set_reset_count

   rule dec_reset_count (reset_count > 0);
      reset_count <= reset_count - 1;
      if (reset_count == 1) begin
	 end_reset.send();
      end
   endrule: dec_reset_count

   rule clear_reset (end_reset.pulse());
      assert_reset <= False;
   endrule: clear_reset

   rule scemi_assert_reset (assert_reset == True);
      reset_sig.assertReset();
   endrule: scemi_assert_reset

   interface Reset new_rst = join_reset;

endmodule: mkSceMiResetXactor

// =========================
// SemuInPort Xactor

// Connects to a DUT with a Put interface
// This version has 2 mode of operation:
//  TightlyCoupled - stalls the clock until data for all TightlyCoupled ports
//   on the dut of the same clock domain is received.
//  LooselyCoupled - never stalls the clock

typedef enum { LooselyCoupled,TightlyCoupled } EmuType deriving (Eq,Bits);

module [SceMiModule] mkSemuInPortXactor
   #(Put#(ty) putifc, EmuType xactortype, SceMiClockConfiguration cclk_conf) (Empty)
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   SceMiClockPortIfc clk_port <- mkSceMiClockPort(cclk_conf);
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   // Input port for setting emulation type (tightly/loosely coupled)
   SceMiMessageInPortIfc#(EmuType) ctrl_in <- mkSceMiMessageInPort();

   // The type of the xactor; loosely or tightly coupled
   Reg#(EmuType) xactor_type <- mkReg(xactortype, clocked_by uclock, reset_by ureset);

   if (cclock == uclock && creset == ureset)
      begin
         error("mkSemuInPortXactor: can't stall the uclock");
      end
   else
      begin

	 // A CrossingFIFOF which serves to cross the data into
	 // the uncontrolled domain and to store the data until it can be taken
	 CrossingFIFOF#(ty) res_fifo <- mkUtoCNullCrossingFIFOF(uclock,ureset,cclock,creset);

	 // Instantiate a controller for the controlled clock.
         // When there is data to send, stall the clock.
         // The ready condition of the outport doesn't need to be checked,
         // because it will be reflected in the data still being in the FIFO
         Bool allow_clock = res_fifo.allowCclock || (xactor_type == LooselyCoupled);
         SceMiClockControlIfc clk_cntrl <-
         mkSceMiClockControl(cclk_conf.clockNum, allow_clock, allow_clock);

	 let connect_res <- mkConnection(toPut(res_fifo), toGet(inport));
	 let connect_put <- mkConnection(toGet(res_fifo), putifc);
      end

   rule set_emulation_type;
      let etype <- toGet(ctrl_in).get();
      if (etype != xactor_type)
	 begin
	    xactor_type <= etype;
	 end
   endrule

   rule request;
      inport.request();
   endrule

   rule request_ctrl;
      ctrl_in.request();
   endrule

endmodule

// =========================
// SemuInPortPipe Xactor

// Connects to a DUT with a Put interface
// This version has 2 mode of operation:
//  TightlyCoupled - stalls the clock until data for all TightlyCoupled ports
//   on the dut of the same clock domain is received.
//  LooselyCoupled - never stalls the clock

module [SceMiModule] mkSemuInPortPipeXactor
   #(Put#(ty) putifc, EmuType xactortype, Integer depth, Visibility style,
     SceMiClockConfiguration cclk_conf) (Empty)
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   SceMiClockPortIfc clk_port <- mkSceMiClockPort(cclk_conf);
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // Input port for setting emulation type (tightly/loosely coupled)
   SceMiMessageInPortIfc#(EmuType) ctrl_in <- mkSceMiMessageInPort();

   // The type of the xactor; loosely or tightly coupled
   Reg#(EmuType) xactor_type <- mkReg(xactortype, clocked_by uclock, reset_by ureset);

   // The input pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, ty) inpipe <- mkSceMiInputPipe(depth, style);

   // A SyncFIFO (of depth 4) which servers to cross the data into
   // the controlled domain and to store the data until it can be taken
   //SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(4,uclock,ureset,cclock);
   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkUtoCNullCrossingFIFOF(uclock,ureset,cclock,creset);


   // Instantiate a controller for the controlled clock.
   // When there is data to send, stall the clock.
   // The ready condition of the outport doesn't need to be checked,
   // because it will be reflected in the data still being in the FIFO
   Bool allow_clock = res_fifo.allowCclock || (xactor_type == LooselyCoupled);
   SceMiClockControlIfc clk_cntrl <-
   mkSceMiClockControl(cclk_conf.clockNum, allow_clock, allow_clock);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inpipe));
   let connect_put <- mkConnection(toGet(res_fifo), putifc);

   rule set_emulation_type;
      let etype <- toGet(ctrl_in).get();
      if (etype != xactor_type)
	 begin
	    xactor_type <= etype;
	 end
   endrule

   rule request_ctrl;
      ctrl_in.request();
   endrule

endmodule

// =========================
// SemuOutPort Xactor

// Connects to a DUT with a Get interface
// This version has 2 mode of operation:
//  TightlyCoupled - data is captured and sent to SceMi every
//   controlled clock cycle.
//  LooselyCoupled - data is captured and sent to SceMi only
//   when there is a change in output value.

module [SceMiModule] mkSemuOutPortXactor
   #(Get#(ty) getifc, EmuType xactortype, SceMiClockConfiguration cclk_conf) (Empty)
   provisos (Bits#(ty, ty_sz), Eq#(ty), Literal#(ty));

   // Access the controlled clock and reset
   SceMiClockPortIfc clk_port <- mkSceMiClockPort(cclk_conf);
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   // Input port for setting emulation type (tightly/loosely coupled)
   SceMiMessageInPortIfc#(EmuType) ctrl_in <- mkSceMiMessageInPort();

   // The type of the xactor; loosely or tightly coupled
   Reg#(EmuType) xactor_type <- mkReg(xactortype, clocked_by uclock, reset_by ureset);
   ReadOnly#(EmuType) xactor_type_cdomain <- mkNullCrossingWire (cclock, xactor_type);

   // Reg for storing previous value and check for a change
   Reg#(Maybe#(ty)) prev_output <- mkReg(tagged Invalid, clocked_by cclock, reset_by creset);

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkCtoUNullCrossingFIFOF(cclock,creset,uclock,ureset);

   if (cclock == uclock && creset == ureset)
      begin
         error("mkSemuOutPortXactor: can't stall the uclock");
      end
   else
      begin
         let connect_res <- mkConnection(toGet(res_fifo), toPut(outport));
         //let connect_get <- mkConnection(toPut(res_fifo), getifc);

	 // Instantiate a controller for the controlled clock.
	 // When there is data to send, stall the clock.
	 // The ready condition of the outport doesn't need to be checked,
	 // because it will be reflected in the data still being in the FIFO
	 Bool allow_clock = res_fifo.allowCclock;
	 SceMiClockControlIfc clk_cntrl <-
	    mkSceMiClockControl(cclk_conf.clockNum, allow_clock, allow_clock);
      end

   rule set_emulation_type;
      let etype <- toGet(ctrl_in).get();
      if (etype != xactor_type)
	 begin
	    xactor_type <= etype;
	 end
   endrule

   rule capture_loosely_coupled_output (xactor_type_cdomain == LooselyCoupled);
      let new_value <- getifc.get();
      if (!isValid(prev_output) ||
	  (isValid(prev_output) && (new_value != fromMaybe(?, prev_output))))
	 begin
	    prev_output <= tagged Valid new_value;
	    res_fifo.fifo.enq(new_value);
	 end

   endrule

   rule capture_tightly_coupled_output (xactor_type_cdomain == TightlyCoupled);
      let new_value <- getifc.get();
      prev_output <= tagged Valid new_value;
      res_fifo.fifo.enq(new_value);
   endrule

   rule request_ctrl;
      ctrl_in.request();
   endrule

endmodule

// =========================
// SemuOutPortPipe Xactor

// Connects to a DUT with a Get interface
// This version has 2 mode of operation:
//  TightlyCoupled - data is captured and sent to SceMi every
//   controlled clock cycle.
//  LooselyCoupled - data is captured and sent to SceMi only
//   when there is a change in output value.

module [SceMiModule] mkSemuOutPortPipeXactor
   #(Get#(ty) getifc, EmuType xactortype, Integer depth, Visibility style,
     SceMiClockConfiguration cclk_conf) (Empty)
   provisos (Bits#(ty, ty_sz), Eq#(ty), Literal#(ty)
	     , Add#(a__, ty_sz, TMul#(TDiv#(ty_sz, 8), 8))
	     );

   // Access the controlled clock and reset
   SceMiClockPortIfc clk_port <- mkSceMiClockPort(cclk_conf);
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiOutputPipeIfc#(1, ty) outpipe <- mkSceMiOutputPipe(depth, style);

   // Input port for setting emulation type (tightly/loosely coupled)
   SceMiMessageInPortIfc#(EmuType) ctrl_in <- mkSceMiMessageInPort();

   // The type of the xactor; loosely or tightly coupled
   Reg#(EmuType) xactor_type <- mkReg(xactortype, clocked_by uclock, reset_by ureset);
   ReadOnly#(EmuType) xactor_type_cdomain <- mkNullCrossingWire (cclock, xactor_type);

   // Reg for storing previous value and check for a change
   Reg#(Maybe#(ty)) prev_output <- mkReg(tagged Invalid, clocked_by cclock, reset_by creset);

   // A SyncFIFO (of depth 4) which servers to cross the data into
   // the uncontrolled domain and to store the data until it can be served
   //SyncFIFOIfc#(ty) res_fifo <- mkAppropriateFIFO(4,cclock,creset,uclock);

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkCtoUNullCrossingFIFOF(cclock,creset,uclock,ureset);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outpipe));

   // Instantiate a controller for the controlled clock.
   // When there is data to send, stall the clock.
   // The ready condition of the outport doesn't need to be checked,
   // because it will be reflected in the data still being in the FIFO
   Bool allow_clock = res_fifo.allowCclock;
   SceMiClockControlIfc clk_cntrl <-
      mkSceMiClockControl(cclk_conf.clockNum, allow_clock, allow_clock);

   rule set_emulation_type;
      let etype <- toGet(ctrl_in).get();
      if (etype != xactor_type)
	 begin
	    xactor_type <= etype;
	 end
   endrule

   rule capture_loosely_coupled_output (xactor_type_cdomain == LooselyCoupled);
      let new_value <- getifc.get();
      if (!isValid(prev_output) ||
	  (isValid(prev_output) && (new_value != fromMaybe(?, prev_output))))
	 begin
	    prev_output <= tagged Valid new_value;
	    res_fifo.fifo.enq(new_value);
	 end

   endrule

   rule capture_tightly_coupled_output (xactor_type_cdomain == TightlyCoupled);
      let new_value <- getifc.get();
      prev_output <= tagged Valid new_value;
      res_fifo.fifo.enq(new_value);
   endrule

   rule request_ctrl;
      ctrl_in.request();
   endrule

endmodule

// =========================
// SemuRdyEnableInPort Xactor

// Connects to a DUT with a Put interface
// This version is the same as the generic mkInPortXactor but it uses
// mkUtoCNullCrossingFIFO instead of SyncFIFO.

module [SceMiModule] mkSemuRdyEnableInPortXactor #(SceMiClockPortIfc clk_port ) (Get#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageInPortIfc#(ty) inport <- mkSceMiMessageInPort();

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkUtoCNullCrossingFIFOF(uclock,ureset,cclock,creset);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inport));
   rule request;
      inport.request();
   endrule

   return toGet(res_fifo);

endmodule

// =========================
// SemuRdyEnableInPortPipe Xactor

// Connects to a DUT with a Put interface
// This version is the same as the generic mkInPortXactor but it uses
// mkUtoCNullCrossingFIFO instead of SyncFIFO.

module [SceMiModule] mkSemuRdyEnableInPortPipeXactor #(Integer depth, Visibility style,
						       SceMiClockPortIfc clk_port ) (Get#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The input pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an inport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, ty) inpipe <- mkSceMiInputPipe(depth, style);

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkUtoCNullCrossingFIFOF(uclock,ureset,cclock,creset);

   let connect_res <- mkConnection(toPut(res_fifo), toGet(inpipe));

   return toGet(res_fifo);

endmodule

// =========================
// SemuRdyEnableOutPort Xactor
// Connects to a DUT with a Get interface
// This version is the same as the generic mkOutPortXactor but it uses
// mkCtoUNullCrossingFIFO instead of SyncFIFO.

module [SceMiModule] mkSemuRdyEnableOutPortXactor#(SceMiClockPortIfc clk_port ) (Put#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output port
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiMessageOutPortIfc#(ty) outport <- mkSceMiMessageOutPort();

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkCtoUNullCrossingFIFOF(cclock,creset,uclock,ureset);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outport));

   return toPut(res_fifo);

endmodule

// =========================
// SemuRdyEnableOutPortPipe Xactor
// Connects to a DUT with a Get interface
// This version is the same as the generic mkOutPortXactor but it uses
// mkCtoUNullCrossingFIFO instead of SyncFIFO.

module [SceMiModule] mkSemuRdyEnableOutPortPipeXactor#(Integer depth, Visibility style,
						       SceMiClockPortIfc clk_port ) (Put#(ty))
   provisos (Bits#(ty, ty_sz));

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiOutputPipeIfc#(1, ty) outpipe <- mkSceMiOutputPipe(depth, style);

   // A CrossingFIFOF which serves to cross the data into
   // the uncontrolled domain and to store the data until it can be taken
   CrossingFIFOF#(ty) res_fifo <- mkCtoUNullCrossingFIFOF(cclock,creset,uclock,ureset);

   let connect_res <- mkConnection(toGet(res_fifo), toPut(outpipe));

   return toPut(res_fifo);

endmodule

endpackage: SceMiXactors
