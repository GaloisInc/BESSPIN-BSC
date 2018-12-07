////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : SceMiMemoryXactor.bsv
//  Description   : Collection of SceMi Memory based transactors
////////////////////////////////////////////////////////////////////////////////
package SceMiMemoryXactor;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import SceMiCore         ::*;
import Clocks            ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FIFOF             ::*;
import SpecialFIFOs      ::*;
import AlignedFIFOs      ::*;
import DReg              ::*;
import RegFile           ::*;
import Vector            ::*;
import Connectable       ::*;

import Memory            ::*;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef MemoryRequest#(32, 256)       MemReq;
typedef MemoryResponse#(256)          MemResp;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of a Synchronous Memory Transactor
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkSyncMemoryXactor#(MemoryClient#(a,d) usrifc)(MemoryClient#(a, d));
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                           	     uClock              <- sceMiGetUClock;
   Reset                           	     uReset              <- sceMiGetUReset;
   Clock                           	     cClock              <- exposeCurrentClock;
   Reset                           	     cReset              <- exposeCurrentReset;
   
   Reset                                     cRst_uclk           <- mkAsyncReset( 2, cReset, uClock );
   Reset                                     jointReset1         <- mkResetEither( uReset, cRst_uclk, clocked_by uClock );
   Reset                                     jointReset          <- mkAsyncReset( 2, jointReset1, uClock );
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   SceMiMessageInPortIfc#(MemoryRequest#(a,d)) mRequest            <- mkSceMiMessageInPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(MemoryResponse#(d)) mReadResponse       <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(Bool)             mWriteResponse      <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   
   FIFO#(MemoryRequest#(a,d))         	     fSceMiRequest       <- mkLFIFO( clocked_by uClock, reset_by uReset );
   FIFO#(MemoryResponse#(d))                 fSceMiRdResponse    <- mkLFIFO( clocked_by uClock, reset_by uReset );
   FIFO#(Bool)                               fSceMiWrResponse    <- mkLFIFO( clocked_by uClock, reset_by uReset );
   
   FIFOF#(Bool)                              fUserReadRequest    <- mkSizedBypassFIFOF( 2, clocked_by uClock, reset_by uReset );
   
   SyncFIFOIfc#(MemoryRequest#(a,d))         fUserRequest        <- mkSyncFIFO( 1, cClock, cReset, uClock );
   SyncFIFOIfc#(MemoryResponse#(d))          fUserResponse       <- mkSyncFIFO( 1, uClock, jointReset, cClock );
   
   FIFO#(MemoryRequest#(a,d))                fMemoryRequest      <- mkLFIFO( clocked_by uClock, reset_by uReset );
   FIFO#(MemoryResponse#(d))                 fMemoryResponse     <- mkLFIFO( clocked_by uClock, reset_by uReset );
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* descending_urgency = "process_user_read, process_user_write, process_scemi_read, process_scemi_write" *)
   rule process_user_write(fUserRequest.first.write);
      let request = fUserRequest.first; fUserRequest.deq;
      fMemoryRequest.enq(request);
      fUserResponse.enq(unpack(0));
   endrule
   
   rule process_user_read(!fUserRequest.first.write);
      let request = fUserRequest.first; fUserRequest.deq;
      fMemoryRequest.enq(request);
      fUserReadRequest.enq(True);
   endrule
   
   rule process_scemi_write(fSceMiRequest.first.write);
      let request = fSceMiRequest.first; fSceMiRequest.deq;
      fMemoryRequest.enq(request);
      fSceMiWrResponse.enq(True);
   endrule
   
   rule process_scemi_read(!fSceMiRequest.first.write);
      let request = fSceMiRequest.first; fSceMiRequest.deq;
      fMemoryRequest.enq(request);
      fUserReadRequest.enq(False);
   endrule
   
   
   rule process_user_response(fUserReadRequest.first);
      let response = fMemoryResponse.first; fMemoryResponse.deq;
      fUserReadRequest.deq;
      fUserResponse.enq(response);
   endrule
   
   rule process_scemi_response(!fUserReadRequest.first);
      let response = fMemoryResponse.first; fMemoryResponse.deq;
      fUserReadRequest.deq;
      fSceMiRdResponse.enq(response);
   endrule
   
   rule enqueue_scemi_request_request;
      mRequest.request;
   endrule
   
   rule enqueue_scemi_requests;
      let request <- toGet(mRequest).get;
      fSceMiRequest.enq(request);
   endrule
   
   rule dequeue_scemi_read_responses;
      let response = fSceMiRdResponse.first; fSceMiRdResponse.deq;
      mReadResponse.send(response);
   endrule
   
   rule dequeue_scemi_write_responses;
      let response = fSceMiWrResponse.first; fSceMiWrResponse.deq;
      mWriteResponse.send(response);
   endrule
   
   rule enqueue_user_requests;
      let request <- usrifc.request.get;
      fUserRequest.enq(request);
   endrule
   
   rule dequeue_user_responses;
      let response = fUserResponse.first; fUserResponse.deq;
      usrifc.response.put(response);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface request  = toGet(fMemoryRequest);
   interface response = toPut(fMemoryResponse);
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of an Asynchronous Memory Transactor
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkASyncMemoryXactor#(  MemoryClient#(a, d)    usrifc
					  , Clock                  sClock
					  , Reset                  sReset
					  )
                                          ( MemoryClient#(a, d) );
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     uClock              <- sceMiGetUClock;
   Reset                                     uReset              <- sceMiGetUReset;
   Clock                                     cClock              <- exposeCurrentClock;
   Reset                                     cReset              <- exposeCurrentReset;

   Reset                                     cRst_sclk           <- mkAsyncReset( 2, cReset, sClock );
   Reset                                     jointReset1         <- mkResetEither( sReset, cRst_sclk, clocked_by sClock );
   Reset                                     jointReset          <- mkAsyncReset( 2, jointReset1, sClock );

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   SceMiMessageInPortIfc#(MemoryRequest#(a, d)) mRequest            <- mkSceMiMessageInPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(MemoryResponse#(d))  mReadResponse       <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(Bool)             mWriteResponse      <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   
   SyncFIFOIfc#(MemoryRequest#(a, d))        fSceMiRequest       <- mkSyncFIFO( 1, uClock, uReset, sClock );
   SyncFIFOIfc#(MemoryResponse#(d))          fSceMiRdResponse    <- mkSyncFIFO( 1, sClock, sReset, uClock );
   SyncFIFOIfc#(Bool)                        fSceMiWrResponse    <- mkSyncFIFO( 1, sClock, sReset, uClock );
   
   FIFOF#(Bool)                              fUserReadRequest    <- mkSizedBypassFIFOF( 2, clocked_by sClock, reset_by sReset );
   
   SyncFIFOIfc#(MemoryRequest#(a, d))        fUserRequest        <- mkSyncFIFO( 1, cClock, cReset, sClock );
   SyncFIFOIfc#(MemoryResponse#(d))          fUserResponse       <- mkSyncFIFO( 1, sClock, jointReset, cClock );
   
   FIFO#(MemoryRequest#(a, d))               fMemoryRequest      <- mkLFIFO( clocked_by sClock, reset_by sReset );
   FIFO#(MemoryResponse#(d))                 fMemoryResponse     <- mkLFIFO( clocked_by sClock, reset_by sReset );

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* descending_urgency = "process_user_read, process_user_write, process_scemi_read, process_scemi_write" *)
   rule process_user_write(fUserRequest.first.write);
      let request = fUserRequest.first; fUserRequest.deq;
      fMemoryRequest.enq(request);
      fUserResponse.enq(unpack(0));
   endrule
   
   rule process_user_read(!fUserRequest.first.write);
      let request = fUserRequest.first; fUserRequest.deq;
      fMemoryRequest.enq(request);
      fUserReadRequest.enq(True);
   endrule
   
   rule process_scemi_write(fSceMiRequest.first.write);
      let request = fSceMiRequest.first; fSceMiRequest.deq;
      fMemoryRequest.enq(request);
      fSceMiWrResponse.enq(True);
   endrule
   
   rule process_scemi_read(!fSceMiRequest.first.write);
      let request = fSceMiRequest.first; fSceMiRequest.deq;
      fMemoryRequest.enq(request);
      fUserReadRequest.enq(False);
   endrule
   
   
   rule process_user_response(fUserReadRequest.first);
      let response = fMemoryResponse.first; fMemoryResponse.deq;
      fUserReadRequest.deq;
      fUserResponse.enq(response);
   endrule
   
   rule process_scemi_response(!fUserReadRequest.first);
      let response = fMemoryResponse.first; fMemoryResponse.deq;
      fUserReadRequest.deq;
      fSceMiRdResponse.enq(response);
   endrule
   
   rule enqueue_scemi_request_request;
      mRequest.request;
   endrule
   
   rule enqueue_scemi_requests;
      let request <- toGet(mRequest).get;
      fSceMiRequest.enq(request);
   endrule
   
   rule dequeue_scemi_read_responses;
      let response = fSceMiRdResponse.first; fSceMiRdResponse.deq;
      mReadResponse.send(response);
   endrule
   
   rule dequeue_scemi_write_responses;
      let response = fSceMiWrResponse.first; fSceMiWrResponse.deq;
      mWriteResponse.send(response);
   endrule
   
   rule enqueue_user_requests;
      let request <- usrifc.request.get;
      fUserRequest.enq(request);
   endrule
   
   rule dequeue_user_responses;
      let response = fUserResponse.first; fUserResponse.deq;
      usrifc.response.put(response);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface request  = toGet(fMemoryRequest);
   interface response = toPut(fMemoryResponse);

endmodule


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///  Fixed priority Asynchronous Memory Transactor
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module [SceMiModule] mkASyncMemoryXactorN#(  Vector#(n, MemoryClient#(a, d)) vIfc
					   , Vector#(n, Bit#(a))             vIfcOffsets
					   , Clock                           sClock
					   , Reset                           sReset
					   )
                                           ( MemoryClient#(a, d) )
   provisos(  Add#(1, _1, n)     // there must exist at least one non-SceMi request/response interface
	    , Add#(1, n, reqs)   // reqs = # of possible requestors of memory
	    , Log#(reqs, sreqs)  // sreqs = # bits to represent the # of possible requestors of memory
	    );
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     uClock              <- sceMiGetUClock;
   Reset                                     uReset              <- sceMiGetUReset;
   Clock                                     cClock              <- exposeCurrentClock;
   Reset                                     cReset              <- exposeCurrentReset;
   
   Reset                                     cRst_sclk           <- mkAsyncReset( 2, cReset, sClock );
   Reset                                     jointReset1         <- mkResetEither( sReset, cRst_sclk, clocked_by sClock );
   Reset                                     jointReset          <- mkAsyncReset( 2, jointReset1, sClock );

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   SceMiMessageInPortIfc#(MemoryRequest#(a, d)) mRequest            <- mkSceMiMessageInPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(MemoryResponse#(d))  mReadResponse       <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   SceMiMessageOutPortIfc#(Bool)             mWriteResponse      <- mkSceMiMessageOutPort( clocked_by uClock, reset_by uReset );
   
   SyncFIFOIfc#(MemoryRequest#(a, d))        fSceMiRequest       <- mkSyncFIFO( 1, uClock, uReset, sClock );
   SyncFIFOIfc#(MemoryResponse#(d))          fSceMiRdResponse    <- mkSyncFIFO( 1, sClock, sReset, uClock );
   SyncFIFOIfc#(Bool)                        fSceMiWrResponse    <- mkSyncFIFO( 1, sClock, sReset, uClock );
   
   // "0" represents the a request from SceMi
   FIFOF#(UInt#(sreqs))                      fReadRequestor      <- mkSizedBypassFIFOF( 2, clocked_by sClock, reset_by sReset );
   
   Vector#(n, SyncFIFOIfc#(MemoryRequest#(a, d))) vfUserRequest       <- replicateM(mkSyncFIFO( 1, cClock, cReset, sClock ));
   Vector#(n, SyncFIFOIfc#(MemoryResponse#(d)))   vfUserResponse      <- replicateM(mkSyncFIFO( 1, sClock, jointReset, cClock ));
   
   FIFO#(MemoryRequest#(a, d))               fMemoryRequest      <- mkLFIFO( clocked_by sClock, reset_by sReset );
   FIFO#(MemoryResponse#(d))                 fMemoryResponse     <- mkLFIFO( clocked_by sClock, reset_by sReset );

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   function Rules generate_user_rules(Integer index, SyncFIFOIfc#(MemoryRequest#(a, d)) fReq, SyncFIFOIfc#(MemoryResponse#(d)) fResp, MemoryClient#(a, d) ifc, Bit#(a) offset);
      return (rules
	         rule process_write(fReq.first.write);
		    let request = fReq.first; fReq.deq;
		    request.address = request.address + offset;
		    fMemoryRequest.enq(request);
		    fResp.enq(unpack(0));
		 endrule
		 rule process_read(!fReq.first.write);
		    let request = fReq.first; fReq.deq;
		    request.address = request.address + offset;
		    fMemoryRequest.enq(request);
		    fReadRequestor.enq(fromInteger(index+1));
		 endrule
		 rule process_response(fReadRequestor.first == fromInteger(index+1));
		    let response = fMemoryResponse.first; fMemoryResponse.deq;
		    fReadRequestor.deq;
		    fResp.enq(response);
		 endrule
		 rule enqueue_requests;
		    let request <- ifc.request.get;
		    fReq.enq(request);
		 endrule
		 rule dequeue_responses;
		    let response = fResp.first; fResp.deq;
		    ifc.response.put(response);
		 endrule
	      endrules);
   endfunction
   
   Rules scemi_rules = 
   (rules
       rule process_scemi_write(fSceMiRequest.first.write);
	  let request = fSceMiRequest.first; fSceMiRequest.deq;
	  fMemoryRequest.enq(request);
	  fSceMiWrResponse.enq(True);
       endrule
       rule process_scemi_read(!fSceMiRequest.first.write);
	  let request = fSceMiRequest.first; fSceMiRequest.deq;
	  fMemoryRequest.enq(request);
	  fReadRequestor.enq(0);
       endrule
       rule process_scemi_response(fReadRequestor.first == 0);
	  let response = fMemoryResponse.first; fMemoryResponse.deq;
	  fReadRequestor.deq;
	  fSceMiRdResponse.enq(response);
       endrule
       rule scemi_request;
	  mRequest.request;
       endrule
       rule enqueue_scemi_requests;
	  let request <- toGet(mRequest).get;
	  fSceMiRequest.enq(request);
       endrule
       rule dequeue_scemi_read_responses;
	  let response = fSceMiRdResponse.first; fSceMiRdResponse.deq;
	  mReadResponse.send(response);
       endrule
       rule dequeue_scemi_write_responses;
	  let response = fSceMiWrResponse.first; fSceMiWrResponse.deq;
	  mWriteResponse.send(response);
       endrule
    endrules);
   
   Rules rs = scemi_rules;
   
   // This loop sets the priority among the requestors.  Lower indexes on the input interface vector
   // have higher priority.  The SceMi request interface has the lowest priority.
   for(Integer i = 0; i < valueOf(n); i = i + 1) begin
      rs = rJoinDescendingUrgency(rs, generate_user_rules(i, vfUserRequest[i], vfUserResponse[i], vIfc[i], vIfcOffsets[i]));
   end

   addRules(rs);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface request  = toGet(fMemoryRequest);
   interface response = toPut(fMemoryResponse);
   
endmodule

endpackage: SceMiMemoryXactor



