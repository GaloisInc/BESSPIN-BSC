// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiPCIE;

// This is an implementation of SceMi over PCI-Express.

import Clocks::*;
import Vector::*;
import List::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;
import TieOff::*;
import ModuleContext::*;
import DefaultValue::*;

import SceMiDefines::*;
import SceMiInternals::*;
import SceMiClocks::*;

import SceMiVirtex5PCIE::*;
import SceMiVirtex6PCIE::*;
import SceMiKintex7PCIE::*;
import SceMiDiniPCIE::*;

//import CheckProbe::*;
import BUtils::*;

export SceMiV5PCIEArgs(..), SceMiV5PCIEIfc(..), buildSceMiPCIEV5;
export SceMiV6PCIEArgs(..), SceMiV6PCIEIfc(..), buildSceMiPCIEV6;
export SceMiK7PCIEArgs(..), SceMiK7PCIEIfc(..), buildSceMiPCIEK7;
export SceMiDiniPCIEArgs(..), SceMiDiniPCIEIfc(..), buildSceMiPCIEDini;
export mkSceMiPCIEMessageInPort, mkSceMiPCIEMessageOutPort;
export mkSceMiPCIEClockPort, mkSceMiPCIEClockControl;

// Message input port for PCIE
// This gets shifted in one-word-at-a-time across the PCIE bus
// and then read broadside into the transactor.

typedef enum { IDLE, WAITING, READY, ACK } PortStatus deriving (Bits,Eq);

module [SceMiModule] mkSceMiPCIEMessageInPort(SceMiMessageInPortRawIfc#(a))
   provisos ( Bits#(a,sz), Div#(sz,32,nwords)
            , Add#(1,nwords,nwords_plus_1), Log#(nwords_plus_1,cnt_bits) );

   // Extract state from module context
   let state <- getContext();
   let uClock     = getUClock(state);
   let uReset     = getUReset(state);
   let scemiClock = getSceMiClock(state);
   let scemiReset = getSceMiReset(state);

   // For PCIE the data will get split into 32-bit chunks.  The data
   // is shifted in 32-bits at a time from the PCIE side (on the
   // scemiClock).  The data is read in its entirety from the
   // transactor side (on the uClock).  Due to latency involved in the
   // path to move the port to the READY status, there is no
   // additional synchronization needed for this domain crossing.
   Integer num_words = valueOf(nwords);
   Vector#(nwords,CrossingReg#(Bit#(32))) scemiInportWords <-
        replicateM(mkNullCrossingReg(uClock, unpack('0), clocked_by scemiClock, reset_by scemiReset));
   Vector#(nwords,CrossingReg#(Bit#(32))) words = scemiInportWords;

   // Count of the number of words remaining to be written before the
   // entire value is ready to be read by the transactor.  This is
   // read and written from the PCIE side (on the scemiClock).
   Reg#(UInt#(cnt_bits)) remaining <- mkReg( fromInteger(num_words)
					   , clocked_by scemiClock
					   , reset_by scemiReset
					   );
   SyncPulseIfc buffer_full_sp  <- mkSyncHandshake(scemiClock, scemiReset, uClock);
   SyncPulseIfc buffer_empty_sp <- mkSyncHandshake(scemiClock, scemiReset, uClock);

   // Register which maintains the current port status.  It is
   // read and written in the uClock domain, and read in the
   // scemiClock domain.
   CrossingReg#(PortStatus) status <- mkNullCrossingReg(scemiClock, IDLE, clocked_by uClock, reset_by uReset);

   SyncPulseIfc next_sp <- mkSyncHandshake(uClock, uReset, scemiClock);
   PulseWire request_pw  <- mkPulseWire(clocked_by uClock, reset_by uReset);
   PulseWire got_word_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);

   // On the PCIE-side, the port is written one word at a time
   // using a shift register.
   InPortInfo info;
   info.portNum = state.inPortCount;
   info.bitWidth = valueOf(sz);
   info.wordIfc = (interface WordLevelInPortIfc;
		      method Bool hasRequest();
			 return (status.crossed() == WAITING);
		      endmethod
		      method Action putWord(Bit#(32) x);
			 if (remaining != 0) begin
			    if (num_words > 1) begin
			       for (Integer n = num_words - 1;
                                    n > 0;
                                    n = n - 1) begin
		                  words[n-1] <= words[n];
		               end
			    end
			    words[num_words-1] <= x;
			    got_word_pw.send();
			 end
		      endmethod
		   endinterface);
   state.inPortCount = state.inPortCount + 1;
   state.inports = List::cons(info,state.inports);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_channelId  <- mkSceMiUInt32Parameter(fromInteger(info.portNum));

   // Implement the port status state machine
   rule first_request if (status == IDLE && request_pw);
      status <= WAITING;
      next_sp.send();
   endrule

   rule data_ready if (status == WAITING && buffer_full_sp.pulse());
      status <= READY;
   endrule

   rule read_complete if (status == READY && request_pw);
      status <= ACK;
      next_sp.send();
   endrule

   rule port_reset if (status == ACK && buffer_empty_sp.pulse());
      status <= WAITING;
   endrule

   // in scemiClock domain
   rule update_remaining if (got_word_pw || next_sp.pulse());
      if (got_word_pw) begin
	 if (remaining == 1)
	    buffer_full_sp.send();
	 remaining <= remaining - 1;
      end
      else begin
	 remaining <= fromInteger(num_words);
	 buffer_empty_sp.send();
      end
   endrule

   method Bool has_data();
      return (status == READY);
   endmethod: has_data

   method a read() if (status == READY);
      let val = pack(Vector::map(readCrossingRegDst,words));
      return unpack(val[valueOf(sz)-1:0]);
   endmethod: read

   method Action request;
      request_pw.send();
   endmethod: request

endmodule: mkSceMiPCIEMessageInPort


// Message output port for PCIE
// This gets written broadside from the transactor and then muxed
// out one-word-at-a-time across the PCIE bus.
module [SceMiModule] mkSceMiPCIEMessageOutPort(SceMiMessageOutPortIfc#(a))
   provisos ( Bits#(a,sz), Div#(sz,32,nwords)
	    , Add#(1,nwords,nwords_plus_1), Log#(nwords_plus_1,cnt_bits));

   // Extract state from module context
   let state <- getContext();
   let uClock     = getUClock(state);
   let uReset     = getUReset(state);
   let scemiClock = getSceMiClock(state);
   let scemiReset = getSceMiReset(state);

   // For PCIE the data will get split into 32-bit chunks.  The data is written
   // in its entirety from the transactor side (on the uClock) and then read out
   // 32-bits at a time across the PCIE bus (on the scemiClock).
   Integer num_words = valueOf(nwords);
   Vector#(nwords,CrossingReg#(Bit#(32))) words <-
       replicateM(mkNullCrossingRegU(scemiClock, clocked_by uClock, reset_by uReset));

   Reg#(UInt#(cnt_bits)) count <- mkReg(0, clocked_by scemiClock
                                         , reset_by scemiReset);

   PulseWire decr <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   SyncPulseIfc next <- mkSyncHandshake(uClock, uReset, scemiClock);
   SyncPulseIfc finished <- mkSyncHandshake(scemiClock, scemiReset, uClock);

   // On the PCIE-side, the port is read one word at a time using a
   // mux.  We don't use a shift register because of the clock domain
   // crossing -- it would have to be written on uClock and shifted on
   // scemiClock.
   Bit#(32) current_word = (count > 0) ? Vector::reverse(words)[count-1].crossed() : ?;
   OutPortInfo info;
   info.portNum = state.outPortCount;
   info.bitWidth = valueOf(sz);
   info.wordIfc = (interface WordLevelOutPortIfc;
		      method ActionValue#(Bit#(32)) takeWord();
			 if (count != 0) decr.send();
			 return current_word;
		      endmethod
		   endinterface);
   state.outPortCount = state.outPortCount + 1;
   state.outports = List::cons(info,state.outports);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_channelId  <- mkSceMiUInt32Parameter(fromInteger(info.portNum));

   rule update_count if (decr || next.pulse());
      if (decr)
	 begin
	    let newCount = count - 1;
	    count <= newCount;
	    if (newCount==0) finished.send();
	 end
      else
	 count <= fromInteger(num_words);
   endrule

   // On the user-side, the port is accessed using the whole vector.

   UInt#(10) port = fromInteger(info.portNum);
   Reg#(Bool) ok <- mkReg(True, clocked_by uClock, reset_by uReset);
   Bool ok_to_send = ok && !isInReset(state) && (state.currentOutPort == port);

   rule setOK if (finished.pulse());
      ok <= True;
   endrule

   method Bool accepting_data();
      return ok_to_send;
   endmethod: accepting_data

   method Action send(a x) if (ok_to_send);
      ok <= False;
      Vector::joinActions(Vector::zipWith(writeCrossingReg,words,toChunks(x)));
      next.send();
      addOutputMsgChannel(state,port);
   endmethod: send

endmodule: mkSceMiPCIEMessageOutPort


// SCE-MI clock port for PCIE
// This uses the BSV clock generation logic.
module [SceMiModule] mkSceMiPCIEClockPort#( parameter Integer clockNum
				          , parameter Integer ratioNumerator
				          , parameter Integer ratioDenominator
				          , parameter Integer dutyHi
				          , parameter Integer dutyLo
				          , parameter Integer phase
					  , parameter Integer resetCycles
					  , parameter Integer clockGroup
          				  , parameter SceMiLinkType link_type
                                          )
                                          (SceMiClockPortIfc);
   (*hide*)
   let _m <- mkSceMiBSVClockPort( clockNum
				, ratioNumerator, ratioDenominator
				, dutyHi, dutyLo, phase
				, resetCycles
				, clockGroup
                                , link_type
				);
   return _m;
endmodule: mkSceMiPCIEClockPort


// SCE-MI clock controller for PCIE
// This uses the BSV clock generation logic.
module [SceMiModule] mkSceMiPCIEClockControl#( parameter Integer clockNum
                                             , Bool allow_pos_edge
                                             , Bool allow_neg_edge
                                             )
				 	     (SceMiClockControlIfc);
   (*hide*)
   let _m <- mkSceMiBSVClockControl(clockNum, allow_pos_edge, allow_neg_edge);
   return _m;
endmodule: mkSceMiPCIEClockControl


endpackage: SceMiPCIE
