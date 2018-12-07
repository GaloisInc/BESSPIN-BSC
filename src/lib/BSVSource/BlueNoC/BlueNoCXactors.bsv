////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2014  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
//  Filename      : BlueNoCXactors.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package BlueNoCXactors;

// Notes :
// XXX - nocIsActive for uclock storage signals and rule to perform it.

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Clocks            ::*;
import GetPut            ::*;
import FIFO              ::*;
import FIFOF             ::*;
import Connectable       ::*;
import DefaultValue      ::*;
import TieOff            ::*;
import NoCUtils          ::*;
import MsgFormat         ::*;
import MIMO              ::*;
import BRAMFIFO          ::*;
import BUtils            ::*;
import Cntrs             ::*;
import Vector            ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef 4 BPB;

typedef struct {
   Bit#(6)     opcode;
   Bit#(1)     unused;
   Bool        dont_wait;
   UInt#(8)    length;
   NodeID      src;
   NodeID      dst;
} NoCHeader deriving (Bits, Eq, FShow);

typedef 6'b101010   SceMi1Op;
typedef 6'b101011   SceMi2Op;
typedef 6'b010000   BNDataOp;
typedef 6'b011111   BNCreditOp;

typedef struct {
   Bool        underflow;
   Bit#(15)    unused;
   UInt#(16)   credits;
} CreditMsg deriving (Bits, Eq, FShow);		

typedef struct {
   NodeID      myID;                  // transactor node id
   Integer     capacity;              // depth of data store
   Integer     creditDepth;           // depth of credit store
   Integer     creditTimeout;         // cycles before pending credit updates are forced (0 = never)
   UInt#(16)   creditReturnThreshold; // credit update occurs if we can return this many (or more) credits
} NoCInPipeXactorParams;   
   
instance DefaultValue#(NoCInPipeXactorParams);
   defaultValue = NoCInPipeXactorParams {
      myID                  : ?,
      capacity              : 32,
      creditDepth           : 2,
      creditTimeout         : 31,
      creditReturnThreshold : 16
      };
endinstance

typedef struct {
   NodeID      myID;                  // transactor node id
   Integer     capacity;              // depth of data store
   Integer     creditDepth;           // depth of credit store
   Integer     creditTimeout;         // cycles before pending credit updates are forced (0 = never)
} NoCOutPipeXactorParams;   

instance DefaultValue#(NoCOutPipeXactorParams);
   defaultValue = NoCOutPipeXactorParams {
      myID          : ?,
      capacity      : 32,
      creditDepth   : 2,
      creditTimeout : 31
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
interface NoCOutPipeXactorPIfc#(type ty, numeric type bpb);
   interface Put#(ty)       in;  // input from logic
   interface MsgPort#(bpb)  noc; // output to NoC interface
endinterface

interface NoCInPipeXactorPIfc#(type ty, numeric type bpb);
   interface MsgPort#(bpb)  noc; // input from NoC interface
   interface Get#(ty)       out; // output to logic
endinterface

interface NoCOutPipeXactorIfc#(type ty);
   interface Put#(ty)       in;  // input from logic
   interface MsgPort#(BPB)  noc; // output to NoC interface
endinterface

interface NoCInPipeXactorIfc#(type ty);
   interface MsgPort#(BPB)  noc; // input from NoC interface
   interface Get#(ty)       out; // output to logic
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// This transactor transfers data from the hardware to the host.

module mkNoCOutPipeXactorP#(  Clock              nocClock
			    , Reset              nocReset
			    , Bool               nocIsActive
			    , NoCOutPipeXactorParams params
			    )(NoCOutPipeXactorPIfc#(ty,bpb))
   provisos(  Add#(0, BPB, bpb)          // construct the bpb numeric type requirement (needs to match the implementation)
	    , Max#(16, bpb, w)           // allow 16 or more bytes to be enqueued/dequeued in one shot with any MIMO
	    , Mul#(bpb, 8, bitspb)       // calculate the total number of bits in a beat
	    , Mul#(bitspb, 2, bitspb2)   // calculate the total number of bits in two beats
	    , Max#(4, TMul#(w,2), sz)    // adjust size to be twice as deep as enqueue/dequeue width or at least 8 entries
	    , Add#(2, _1, sz)            // mimo buffers must be at least 2 deep
	    , Add#(_2, w, sz)            // mimo buffers must be >= the width of the enqueue/dequeue interface
	    , Bits#(ty, szty)            // the end type must be represented with bits
	    , Add#(1, _4, szty)          // ensure the type is at least 1-bit wide (no zero-bit types)
	    , Div#(szty, 8, tybytes)     // calculate how many bytes make up the destination type
	    , Mul#(tybytes, 8, tybits)   // calculate a byte-aligned number of bits for the element
	    // BSC should be able to deduce the following (but doesn't)
	    , Add#(_3, TMul#(8,w), TMul#(8,sz))
	    );   
   
   Integer ibpb = valueOf(bpb);
   check_bytes_per_beat("mkNoCOutPipeXactor", ibpb);
   
   Integer depth = max(2, params.capacity);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                           uClock              <- exposeCurrentClock;
   Reset                           uReset              <- exposeCurrentReset;
   
   Reset                           nocReset_uClock     <- mkAsyncReset(0, nocReset, uClock);
   Reg#(Bool)                      inReset_uClock      <- mkReg(True);
   Reg#(Bool)                      prevReset_uClock    <- mkReg(True);
   Reg#(Bool)                      inReset_nocClock    <- mkReg(True, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc                    startingReset       <- mkSyncPulse(uClock, nocReset_uClock, nocClock);
   SyncPulseIfc                    endingReset         <- mkSyncPulse(uClock, nocReset_uClock, nocClock);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled *)
   rule deassert_after_reset if (inReset_uClock);
      inReset_uClock <= False;
   endrule

   (* fire_when_enabled *)
   rule detect_reset if (inReset_uClock);
      startingReset.send();
   endrule
   
   (* fire_when_enabled *)
   rule detect_end_of_reset;
      if (!inReset_uClock && prevReset_uClock)
	 endingReset.send();

      prevReset_uClock <= inReset_uClock;
   endrule
   
   (* fire_when_enabled *)
   rule initiate_reset_sequence if (!inReset_nocClock && startingReset.pulse());
      inReset_nocClock <= True;
   endrule
   
   (* fire_when_enabled *)
   rule complete_reset_sequence if (inReset_nocClock && endingReset.pulse());
      inReset_nocClock <= False;
   endrule
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   FifoMsgSource#(bpb)             fNoCBeatsOut        <- mkFifoMsgSource(clocked_by nocClock, reset_by nocReset);
   FifoMsgSink#(bpb)               fNoCBeatsIn         <- mkFifoMsgSink(clocked_by nocClock, reset_by nocReset);
   SyncFIFOIfc#(ty)                fDataBuffer         <- mkSyncBRAMFIFO(depth, uClock, uReset, nocClock, nocReset);
   FIFO#(ty)                       fElemsIn            <- mkFIFO(clocked_by uClock, reset_by uReset);
   MIMO#(w,w,sz,Bit#(8))           mByteBuffer         <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   MIMO#(w,w,sz,Bit#(8))           mIByteBuffer        <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   
   Reg#(Bool)                      rDataSending        <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rDataSendingHeader  <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(32))                 rDataTotalBytes     <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(8))                  rDataMsgBytes       <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(Bit#(tybits))              rDataEntry          <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(32))                 rDataEntryPtr       <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   
   Reg#(MsgBeat#(bpb))             rDataSavedMsg       <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(LUInt#(bpb))               rDataSavedSize      <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   
   Reg#(UInt#(8))                  rPacketBytes        <- mkReg(0, clocked_by nocClock, reset_by nocReset);

   UCount                          rCreditTimer        <- mkUCount(params.creditTimeout, params.creditTimeout, clocked_by nocClock, reset_by nocReset);
   Count#(UInt#(16))               rNoCCredits         <- mkCount(0, clocked_by nocClock, reset_by nocReset);
   Count#(UInt#(16))               rCredits            <- mkCount(fromInteger(params.capacity), clocked_by uClock, reset_by uReset);
   SyncFIFOIfc#(UInt#(16))         fCreditInfo         <- mkSyncFIFO(fromInteger(params.creditDepth), nocClock, nocReset, uClock);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule msg_source_noc_active if (nocIsActive);
      
      (* fire_when_enabled *)
      rule start_sending_message if (!rDataSending && !rDataSendingHeader && fDataBuffer.notEmpty);
	 rDataSendingHeader <= True;
	 rDataTotalBytes    <= fromInteger(valueOf(tybytes));
	 rDataEntryPtr      <= 0;
	 rDataEntry         <= cExtend(fDataBuffer.first());
	 fDataBuffer.deq;
      endrule
      
      (* fire_when_enabled *)
      rule sending_message_header if (!rDataSending && rDataSendingHeader && mByteBuffer.enqReadyN(4) && rDataTotalBytes > 0);
	 UInt#(8) length = cExtend(min(rDataTotalBytes, 251));
	 Vector#(4, Bit#(8)) v = newVector;
	 let nocHeader = NoCHeader { 
	    opcode    : fromInteger(valueOf(BNDataOp)),
	    unused    : 0,
	    dont_wait : False,
	    length    : length,
	    src       : params.myID,
	    dst       : 0
	    };
	 
	 v = cExtend(nocHeader);
	 mByteBuffer.enq(4, cExtend(v));
	 
	 rDataSending       <= True;
	 rDataSendingHeader <= False;
	 rDataMsgBytes      <= length;
	 rDataTotalBytes    <= rDataTotalBytes - cExtend(length);
      endrule

      (* fire_when_enabled *)
      rule sending_message_data if (rDataSending && !rDataSendingHeader && mByteBuffer.enqReadyN(fromInteger(ibpb)));
	 if (fromInteger(ibpb) <= fromInteger(valueOf(tybytes))) begin
	    MsgBeat#(bpb) x = cExtend(rDataEntry);
	    mByteBuffer.enq(fromInteger(ibpb), cExtend(x));
	    rDataMsgBytes <= rDataMsgBytes - fromInteger(ibpb);
	    rDataEntry    <= rDataEntry >> (8 * fromInteger(ibpb));
	    rDataEntryPtr <= rDataEntryPtr + fromInteger(ibpb);
	    if (rDataMsgBytes <= fromInteger(ibpb)) begin
	       rDataSending       <= False;
	       rDataSendingHeader <= (rDataTotalBytes != 0);
	    end
	 end
	 else begin
	    Bit#(bitspb2) bs = cExtend(rDataSavedMsg) | (cExtend(rDataEntry) << (8 * cExtend(rDataSavedSize)));
	    LUInt#(bpb) bytes_available = cExtend(fromInteger(valueOf(tybytes))) + cExtend(rDataSavedSize);
	    LUInt#(bpb) bytes_needed    = cExtend(min(rDataMsgBytes, fromInteger(ibpb)));
	    Bool        send_beat       = bytes_available >= bytes_needed;
	    if (send_beat) begin
	       rDataSavedMsg  <= cExtend(bs >> (8 * cExtend(bytes_needed)));
	       rDataSavedSize <= cExtend(bytes_available - bytes_needed);
	       rDataMsgBytes  <= rDataMsgBytes - cExtend(bytes_needed);
	       MsgBeat#(bpb) x = cExtend(bs & ~('1 << (8 * cExtend(bytes_needed))));
	       mByteBuffer.enq(fromInteger(ibpb), cExtend(x));
	       if (cExtend(bytes_needed) >= rDataMsgBytes) begin
		  if (rDataTotalBytes == 0) begin
		     rDataSending <= False;
		  end
	       end
	    end
	    else begin
	       rDataSavedMsg  <= cExtend(bs);
	       rDataSavedSize <= cExtend(bytes_available);
	    end
	 end
      endrule
      
      (* fire_when_enabled *)
      rule start_sending_packet if (rPacketBytes == 0 && mByteBuffer.deqReadyN(fromInteger(ibpb)));
	 NoCHeader header = cExtend(mByteBuffer.first);
	 mByteBuffer.deq(fromInteger(ibpb));
	 fNoCBeatsOut.enq(cExtend(mByteBuffer.first));
//	 $display("[%t] NoCOutPipeXactor(nodeid=%d) %02X%02X%02X%02X", $time, params.myID, mByteBuffer.first()[3], mByteBuffer.first()[2], mByteBuffer.first()[1], mByteBuffer.first()[0]);
	 UInt#(9) length = (cExtend(header.length) <= fromInteger(ibpb)) ? 0 : cExtend(header.length) - fromInteger(ibpb);
	 rPacketBytes <= cExtend(length);
      endrule
      
      (* fire_when_enabled *)
      rule continue_sending_packet if (rPacketBytes > 0 && mByteBuffer.deqReadyN(fromInteger(ibpb)));
	 mByteBuffer.deq(fromInteger(ibpb));
	 fNoCBeatsOut.enq(cExtend(mByteBuffer.first));
//	 $display("[%t] NoCOutPipeXactor(nodeid=%d) %02X%02X%02X%02X", $time, params.myID, mByteBuffer.first()[3], mByteBuffer.first()[2], mByteBuffer.first()[1], mByteBuffer.first()[0]);
	 rPacketBytes <= (rPacketBytes <= fromInteger(ibpb)) ? 0 : rPacketBytes - fromInteger(ibpb);
      endrule

      (* fire_when_enabled *)
      rule process_incoming_noc_beats if (mIByteBuffer.enqReadyN(fromInteger(ibpb)) && !fNoCBeatsIn.empty);
	 let data = fNoCBeatsIn.first; fNoCBeatsIn.deq;
	 mIByteBuffer.enq(fromInteger(ibpb), cExtend(data));
      endrule
      
      (* fire_when_enabled *)
      (* preempts = "msg_source_noc_active_process_incoming_credit_msg,\
                     msg_source_noc_active_enqueue_noc_credit_msg" *)
      rule process_incoming_credit_msg if (mIByteBuffer.deqReadyN(8));
	 Vector#(8, Bit#(8)) v = cExtend(mIByteBuffer.first());
	 Vector#(8, Bit#(8)) vv = rotateBy(v, 4);
	 NoCHeader header  = cExtend(mIByteBuffer.first());
	 CreditMsg message = cExtend(vv);
	 mIByteBuffer.deq(8);
	 rNoCCredits  <= rNoCCredits + message.credits;
	 rCreditTimer <= params.creditTimeout;
      endrule
      
      rule enqueue_noc_credit_msg if ((rCreditTimer.isEqual(0) && (params.creditTimeout != 0)) ||
				      (rNoCCredits != 0));
	 fCreditInfo.enq(rNoCCredits);
	 rNoCCredits <= 0;
      endrule
      
   endrule: msg_source_noc_active
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule msg_source_noc_inactive if (!nocIsActive);
      mByteBuffer.clear;
      rDataSending        <= False;
      rDataSendingHeader  <= False;
      rDataTotalBytes     <= 0;
      rDataMsgBytes       <= 0;
      rDataEntry          <= 0;
      rDataEntryPtr       <= 0;
      rDataSavedMsg       <= 0;
      rDataSavedSize      <= 0;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule tick_credit_timer if (!rCreditTimer.isEqual(0));
      rCreditTimer.decr(1);
   endrule

   (* fire_when_enabled *)
   rule handle_returned_credits;
      let credits <- toGet(fCreditInfo).get;
      rCredits.incr(credits);
//      $display("[%t] NoCOutPipeXactor(nodeid=%d): Receiving Credits %d", $time, params.myID, credits);
   endrule
      
   (* fire_when_enabled *)
   rule move_elements_update_credits;
      let data <- toGet(fElemsIn).get;
      fDataBuffer.enq(data);
      rCredits.decr(1);
//      $display("[%t] NoCOutPipeXactor(nodeid=%d): Enqueued: %x", $time, params.myID, data);
   endrule
         
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface in  = toPut(fElemsIn);
   interface noc = as_port(fNoCBeatsOut.source, fNoCBeatsIn.sink);
      
endmodule: mkNoCOutPipeXactorP

// This transactor transfers data from the host to the hardware.
module mkNoCInPipeXactorP#(  Clock              nocClock
			   , Reset              nocReset
			   , Bool               nocIsActive
			   , NoCInPipeXactorParams params
			   )(NoCInPipeXactorPIfc#(ty,bpb))
   provisos(  Add#(0, BPB, bpb)          // construct the bpb numeric type requirement (needs to match the implementation)
	    , Max#(16, bpb, w)           // allow 16 or more bytes to be enqueued/dequeued in one shot with any MIMO
	    , Mul#(bpb, 8, bitspb)       // calculate the total number of bits in a beat
	    , Mul#(bitspb, 2, bitspb2)   // calculate the total number of bits in two beats
	    , Max#(4, TMul#(w,2), sz)    // adjust size to be twice as deep as enqueue/dequeue width or at least 8 entries
	    , Add#(2, _1, sz)            // mimo buffers must be at least 2 deep
	    , Add#(_2, w, sz)            // mimo buffers must be >= the width of the enqueue/dequeue interface
	    , Bits#(ty, szty)            // the end type must be represented with bits
	    , Add#(1, _4, szty)          // ensure the type is at least 1-bit wide (no zero-bit types)
	    , Div#(szty, 8, tybytes)     // calculate how many bytes make up the destination type
	    , Max#(tybytes,bpb,ebytes)   // calculate the enqueue size for the entry buffer
	    , Mul#(2, ebytes, esz)       // calculate the size of the entry buffer (at least 2 entries or larger)
	    , Mul#(tybytes, 8, tybits)   // calculate a byte-aligned number of bits for the element
	    // BSC should be able to deduce the following (but doesn't)
	    , Add#(_3, TMul#(8,w), TMul#(8,sz))
	    , Add#(_5, tybits, TMul#(8,esz))
	    , Add#(_6, tybytes, esz)
	    , Add#(_7, ebytes, esz)
	    , Add#(_8, 2, esz)
	    , Add#(_9, TMul#(8,ebytes), TMul#(8,esz))
	    );   
   
   Integer ibpb = valueOf(bpb);
   check_bytes_per_beat("mkNoCInPipeXactor", ibpb);
   
   Integer depth = max(2, params.capacity);

   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                           uClock              <- exposeCurrentClock;
   Reset                           uReset              <- exposeCurrentReset;
   
   Reset                           nocReset_uClock     <- mkAsyncReset(0, nocReset, uClock);
   Reg#(Bool)                      inReset_uClock      <- mkReg(True);
   Reg#(Bool)                      prevReset_uClock    <- mkReg(True);
   Reg#(Bool)                      inReset_nocClock    <- mkReg(True, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc                    startingReset       <- mkSyncPulse(uClock, nocReset_uClock, nocClock);
   SyncPulseIfc                    endingReset         <- mkSyncPulse(uClock, nocReset_uClock, nocClock);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled *)
   rule deassert_after_reset if (inReset_uClock);
      inReset_uClock <= False;
   endrule

   (* fire_when_enabled *)
   rule detect_reset if (inReset_uClock);
      startingReset.send();
   endrule
   
   (* fire_when_enabled *)
   rule detect_end_of_reset;
      if (!inReset_uClock && prevReset_uClock)
	 endingReset.send();

      prevReset_uClock <= inReset_uClock;
   endrule
   
   (* fire_when_enabled *)
   rule initiate_reset_sequence if (!inReset_nocClock && startingReset.pulse());
      inReset_nocClock <= True;
   endrule
   
   (* fire_when_enabled *)
   rule complete_reset_sequence if (inReset_nocClock && endingReset.pulse());
      inReset_nocClock <= False;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   FifoMsgSource#(bpb)             fNoCBeatsOut        <- mkFifoMsgSource(clocked_by nocClock, reset_by nocReset);
   FifoMsgSink#(bpb)               fNoCBeatsIn         <- mkFifoMsgSink(clocked_by nocClock, reset_by nocReset);
   SyncFIFOIfc#(ty)                fDataBuffer         <- mkSyncBRAMFIFO(depth, nocClock, nocReset, uClock, uReset);
   FIFO#(ty)                       fElemsOut           <- mkFIFO(clocked_by uClock, reset_by uReset);
   MIMO#(w,w,sz,Bit#(8))           mByteBuffer         <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   MIMO#(ebytes,tybytes,esz,Bit#(8)) mEntryBuffer      <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   MIMO#(8,w,sz,Bit#(8))           mCreditByteBuffer   <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   
   Reg#(UInt#(8))                  rDataMsgBytes       <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rDataDecodeMsg      <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   
   UCount                          rCreditTimer        <- mkUCount(params.creditTimeout, params.creditTimeout, clocked_by uClock, reset_by uReset);
   Count#(UInt#(16))               rCredits            <- mkCount(0, clocked_by uClock, reset_by uReset);
   SyncFIFOIfc#(UInt#(16))         fCreditInfo         <- mkSyncFIFO(params.creditDepth, uClock, uReset, nocClock);
   
   Reg#(UInt#(8))                  rPacketBytes        <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule msg_sink_noc_active if (nocIsActive);

      (* fire_when_enabled *)
      rule process_incoming_noc_beats if (mByteBuffer.enqReadyN(fromInteger(ibpb)) && !fNoCBeatsIn.empty);
	 let data = fNoCBeatsIn.first; fNoCBeatsIn.deq;
	 mByteBuffer.enq(fromInteger(ibpb), cExtend(data));
      endrule

      (* fire_when_enabled *)
      rule start_receiving_message if (rDataMsgBytes == 0 && mByteBuffer.deqReadyN(4));
	 NoCHeader header = cExtend(mByteBuffer.first());
	 mByteBuffer.deq(4);
	 
	 rDataMsgBytes   <= header.length;
	 rDataDecodeMsg  <= (header.dst == params.myID) && (header.opcode == fromInteger(valueOf(BNDataOp)));
      endrule
      
      (* fire_when_enabled *)
      rule receive_message_continue if (rDataMsgBytes >= fromInteger(ibpb) && rDataDecodeMsg && mByteBuffer.deqReadyN(fromInteger(ibpb)) && mEntryBuffer.enqReadyN(fromInteger(ibpb)));
	 Vector#(ebytes, Bit#(8)) nextv = cExtend(mByteBuffer.first());
	 mByteBuffer.deq(fromInteger(ibpb));
	 mEntryBuffer.enq(fromInteger(ibpb), nextv);
	 rDataMsgBytes <= rDataMsgBytes - cExtend(fromInteger(ibpb));
      endrule
      
      (* fire_when_enabled *)
      rule receive_message_done if (rDataMsgBytes < fromInteger(ibpb) && rDataMsgBytes > 0 && rDataDecodeMsg && mByteBuffer.deqReadyN(fromInteger(ibpb)) && mEntryBuffer.enqReadyN(cExtend(rDataMsgBytes)));
	 Vector#(ebytes, Bit#(8)) nextv = cExtend(mByteBuffer.first());
	 mByteBuffer.deq(fromInteger(ibpb));
	 mEntryBuffer.enq(cExtend(rDataMsgBytes), nextv);
	 rDataMsgBytes  <= 0;
	 rDataDecodeMsg <= False;
      endrule

      (* fire_when_enabled *)
      rule process_bad_message if (rDataMsgBytes > 0 && !rDataDecodeMsg && mByteBuffer.deqReadyN(fromInteger(ibpb)));
	 mByteBuffer.deq(fromInteger(ibpb));
	 rDataMsgBytes   <= (rDataMsgBytes > fromInteger(ibpb)) ? rDataMsgBytes - cExtend(fromInteger(ibpb)) : 0;
      endrule
	 
      (* fire_when_enabled *)
      rule process_entry_buffer if (mEntryBuffer.deqReadyN(fromInteger(valueOf(tybytes))));
	 ty data = cExtend(mEntryBuffer.first());
	 fDataBuffer.enq(data);
	 mEntryBuffer.deq(fromInteger(valueOf(tybytes)));
      endrule
				    
      (* fire_when_enabled *)
      rule process_credit_message;
	 let credits <- toGet(fCreditInfo).get;

	 Vector#(8, Bit#(8)) v = newVector;
	 let nocHeader = NoCHeader {
	    opcode    : fromInteger(valueOf(BNCreditOp)),
	    unused    : 0,
	    dont_wait : False,
	    length    : 4,
	    src       : params.myID,
	    dst       : 0
	    };
	 let creditMsg = CreditMsg {
	    underflow : False,
	    unused    : 0,
	    credits   : credits
	    };
	 v = cExtend({ pack(creditMsg), pack(nocHeader) });
	 mCreditByteBuffer.enq(8, v);
      endrule
      
      (* fire_when_enabled *)
      rule start_sending_packet if (rPacketBytes == 0 && mCreditByteBuffer.deqReadyN(fromInteger(ibpb)));
	 NoCHeader header = cExtend(mCreditByteBuffer.first);
	 mCreditByteBuffer.deq(fromInteger(ibpb));
	 fNoCBeatsOut.enq(cExtend(mCreditByteBuffer.first));
	 UInt#(9) length = (cExtend(header.length) <= fromInteger(ibpb)) ? 0 : cExtend(header.length) - fromInteger(ibpb);
	 rPacketBytes <= cExtend(length);
      endrule
      
      (* fire_when_enabled *)
      rule continue_sending_packet if (rPacketBytes > 0 && mCreditByteBuffer.deqReadyN(fromInteger(ibpb)));
	 mCreditByteBuffer.deq(fromInteger(ibpb));
	 fNoCBeatsOut.enq(cExtend(mCreditByteBuffer.first));
	 rPacketBytes <= (rPacketBytes <= fromInteger(ibpb)) ? 0 : rPacketBytes - fromInteger(ibpb);
      endrule
      
   endrule: msg_sink_noc_active
   
   (* fire_when_enabled *)
   rule msg_sink_noc_inactive if (!nocIsActive);
      mByteBuffer.clear;
      mEntryBuffer.clear;
      mCreditByteBuffer.clear;
      rDataMsgBytes   <= 0;
      rDataDecodeMsg  <= False;
   endrule

   rule move_elements_update_credits if (rCredits < fromInteger(params.capacity));
      let data <- toGet(fDataBuffer).get;
      fElemsOut.enq(data);
      rCredits.incr(1);
//      $display("[%t] NoCInPipeXactor(nodeid=%d): Enqueued: %x", $time, params.myID, data);
   endrule
      
   (* fire_when_enabled, no_implicit_conditions *)
   rule tick_credit_timer if (!rCreditTimer.isEqual(0));
      rCreditTimer.decr(1);
   endrule
   
   Bool sendCreditsNow = ((rCreditTimer.isEqual(0) && (params.creditTimeout != 0) && (rCredits > 0)) || 
			  (rCredits >= params.creditReturnThreshold));
   
   (* descending_urgency = "move_elements_update_credits, send_credit_message" *)
   rule send_credit_message if (sendCreditsNow);
      fCreditInfo.enq(rCredits);
      rCredits.decr(rCredits);
//      $display("[%t] NoCInPipeXactor(nodeid=%d): Sending Credits %d", $time, params.myID, rCredits);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface noc = as_port(fNoCBeatsOut.source, fNoCBeatsIn.sink);
   interface Get out = toGet(fElemsOut);

endmodule: mkNoCInPipeXactorP

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkNoCOutPipeXactor#(  Clock              nocClock
			   , Reset              nocReset
			   , Bool               nocIsActive
			   , NoCOutPipeXactorParams params
			   )(NoCOutPipeXactorIfc#(ty))
   provisos(  Add#(0, BPB, bpb)          // construct the bpb numeric type requirement (needs to match the implementation)
	    , Max#(16, bpb, w)           // allow 16 or more bytes to be enqueued/dequeued in one shot with any MIMO
	    , Mul#(bpb, 8, bitspb)       // calculate the total number of bits in a beat
	    , Mul#(bitspb, 2, bitspb2)   // calculate the total number of bits in two beats
	    , Max#(4, TMul#(w,2), sz)    // adjust size to be twice as deep as enqueue/dequeue width or at least 8 entries
	    , Add#(2, _1, sz)            // mimo buffers must be at least 2 deep
	    , Add#(_2, w, sz)            // mimo buffers must be >= the width of the enqueue/dequeue interface
	    , Bits#(ty, szty)            // the end type must be represented with bits
	    , Add#(1, _4, szty)          // ensure the type is at least 1-bit wide (no zero-bit types)
	    , Div#(szty, 8, tybytes)     // calculate how many bytes make up the destination type
	    , Mul#(tybytes, 8, tybits)   // calculate a byte-aligned number of bits for the element
	    // BSC should be able to deduce the following (but doesn't)
	    , Add#(_3, TMul#(8,w), TMul#(8,sz))
	    );   
   (* hide_all *)
   let _m <- mkNoCOutPipeXactorP(nocClock, nocReset, nocIsActive, params);
   interface in  = _m.in;
   interface noc = _m.noc;
endmodule

module mkNoCInPipeXactor#(  Clock              nocClock
			  , Reset              nocReset
			  , Bool               nocIsActive
			  , NoCInPipeXactorParams params
			  )(NoCInPipeXactorIfc#(ty))
   provisos(  Add#(0, BPB, bpb)          // construct the bpb numeric type requirement (needs to match the implementation)
	    , Max#(16, bpb, w)           // allow 16 or more bytes to be enqueued/dequeued in one shot with any MIMO
	    , Mul#(bpb, 8, bitspb)       // calculate the total number of bits in a beat
	    , Mul#(bitspb, 2, bitspb2)   // calculate the total number of bits in two beats
	    , Max#(4, TMul#(w,2), sz)    // adjust size to be twice as deep as enqueue/dequeue width or at least 8 entries
	    , Add#(2, _1, sz)            // mimo buffers must be at least 2 deep
	    , Add#(_2, w, sz)            // mimo buffers must be >= the width of the enqueue/dequeue interface
	    , Bits#(ty, szty)            // the end type must be represented with bits
	    , Add#(1, _4, szty)          // ensure the type is at least 1-bit wide (no zero-bit types)
	    , Div#(szty, 8, tybytes)     // calculate how many bytes make up the destination type
	    , Max#(tybytes,bpb,ebytes)   // calculate the enqueue size for the entry buffer
	    , Mul#(2, ebytes, esz)       // calculate the size of the entry buffer (at least 2 entries or larger)
	    , Mul#(tybytes, 8, tybits)   // calculate a byte-aligned number of bits for the element
	    // BSC should be able to deduce the following (but doesn't)
	    , Add#(_3, TMul#(8,w), TMul#(8,sz))
	    , Add#(_5, tybits, TMul#(8,esz))
	    , Add#(_6, tybytes, esz)
	    , Add#(_7, ebytes, esz)
	    , Add#(_8, 2, esz)
	    , Add#(_9, TMul#(8,ebytes), TMul#(8,esz))
	    );   
   (* hide_all *)
   let _m <- mkNoCInPipeXactorP(nocClock, nocReset, nocIsActive, params);
   interface out = _m.out;
   interface noc = _m.noc;
endmodule

endpackage: BlueNoCXactors

