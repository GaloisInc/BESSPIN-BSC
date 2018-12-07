/////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiNoC;

// This is an implementation of SceMi interfacing to the NoC.

import Clocks        :: *;
import ModuleContext :: *;
import Vector        :: *;
import List          :: *;
import GetPut        :: *;
import Connectable   :: *;
import TieOff        :: *;
import BlueNoC       :: *;
import FIFO          :: *;
import FIFOF         :: *;
import BUtils        :: *;
import MIMO          :: *;
import DefaultValue  :: *;
import ConfigReg     :: *;
import Cntrs         :: *;
import CBus          :: *;      // truncateNP
import Probe         :: *;

import SceMiDefines   :: *;
import SceMiInternals :: *;
import SceMiClocks    :: *;

export SceMiNoCArgs(..), SceMiNoCArgsNoClock(..), SceMiNoCIfc(..), buildSceMiNoC, buildSceMiNoCNoClock;
export mkSceMiNoCMessageInPort, mkSceMiNoCMessageOutPort;
export mkSceMiNoCClockPort, mkSceMiNoCClockControl;
export mkSceMiNoCInputPipe, mkSceMiNoCOutputPipe;

// Fully parameterized version of pipes
export mkSceMiNoCInputPipeP, SceMiInputPipeParameters(..), InpipeCreditReturnMode (..);
export mkSceMiNoCOutputPipeP, SceMiOutputPipeParameters(..);

// The NoC message format used for SCE-MI is:
//
// For SCE-MI 1.1 traffic opcode = 2a (101010)
//
// There are 4 message payload types:
//
// A request message sent from a MessageInPort to its proxy has a
// 4-byte payload:
//
//   Bits 31-29: MessageType = 3'b010  (request on input channel)
//   Bits 9-0:   PortNum     = 10-bit port number
//
// A data message sent from a MessageInPort proxy to the port:
//
// 1st 4 bytes of payload, MSB to LSB:
//   Bits 31-29: MessageType = 3'b00x (data in input channel)
//     the low bit is an EndOfFrame indicator, so:
//       MessageType = 3'b000 if the data element continues into the next message
//       MessageType = 3'b001 if this message completes the data element
//   Bits 28-10: BitCount    = 19-bit count of bits remaining in the data element
//   Bits 9-0:   PortNum     = 10-bit port number
//
// The first 4 bytes are followed by 1 or more bytes of element data
// in little-endian format.
//
// A data message sent from a MessageOutPort to its proxy:
//
// 1st 4 bytes of payload, MSB to LSB:
//   Bits 31-29: MessageType = 3'b10x (data in output channel)
//     the low bit is an EndOfFrame indicator, so:
//       MessageType = 3'b100 if the data element continues into the next message
//       MessageType = 3'b101 if this message completes the data element
//   Bits 28-10: BitCount    = 19-bit count of bits remaining in the data element
//   Bits 9-0:   PortNum     = 10-bit port number
//
// The first message in the sequence will have 8 bytes which contain
// the cycle stamp for the message.
//
// The rest of the message consists of 1 or more bytes of element data
// in little-endian format.
//
// An acknowledgement message sent from a MessageOutPort proxy to the
// port consists of a sequence of 4-byte groupings in this format:
//
//   Bits 31-29: MessageType = 3'b110 (ack in output channel, first group only)
//   Bit 26:     Port2Valid  = 1 if ack'ing Port2Num
//   Bits 25-16: Port2Num    = 10-bit port number
//   Bit 10:     Port1Valid  = 1 if ack'ing Port1Num
//   Bits 9-0:   Port1Num    = 10-bit port number
//
// Each group can provide up to 2 numbers of ports that want to acknowledge
// data received. All groups but the last should have both port numbers valid.
// The last group may have 1 or 2 port numbers valid.
//
//
// For SCE-MI 2.1 traffic opcode = 2b (101011)
//
// 1st 4 payload bytes, MSB to LSB:
//   Bit 31:     Data/Credit Flag = 1 for data, 0 for credit
//   Bit 30:     More Indicator   = 1 when more data to send / more data requested
//   Bit 29:     Flush            = Data msg:   1 when data should be flushed
//                                  Credit msg: current autoflush setting
//   Bit 28:     EOM              = 1 EOM message indicator
//   Bits 27-16: Pipe             = 12-bit pipe number
//   Bits 15-0:  N                = Data msg: 16-bit element count (input pipe only)
//                                  Credit msg: 16-bit credit amount
//
// When Bit 31 is set (i.e., this is a data message) there will be
// additional payload bytes containing pipe data. These bytes will
// use the little-endian format:
//   <data_element_bits> + <padding to byte boundary (if needed)>
//   ...
//   <data_element_bits> + <padding to byte boundary (if needed)>
//
// There is no requirement that data message boundaries must align
// with element boundaries. It is OK for a data message to begin or
// end part-way through an element, but when the data payloads are
// joined together in sequence they must form a valid sequence of
// <data,padding> tuples. The element count field for data
// messages should correspond to the number of elements values in
// the message payload.

// Interface wrapper for NoC
interface SceMiNoCIfc#(type i);
   interface i           orig_ifc;
   interface MsgPort#(BPB) noc_src;   // connected to the lower layer (the source)
   interface MsgPort#(BPB) noc_cont;  // unparsed messages continue on/are received and injected

   (* always_ready *)
   method    Bool        isOutOfReset();
   (* always_ready *)
   method    Bool        isClockAdvancing();
endinterface

// Interface wrapper for NoC connections module
interface MsgPortFilter#(numeric type bpb);
   interface MsgPort#(bpb) noc_src;   // connected to the lower layer (the source)
   interface MsgPort#(bpb) noc_cont;  // unparsed messages continue on/are received and injected
endinterface

// Argument structure used for passing in NoC clock
typedef struct {
   Clock         clk;
   Reset         rst;
   Bool          noc_is_active;
   SceMiLinkType link_type;
} SceMiNoCArgs;

typedef struct {
   Clock         clk;
   Reset         rst;
   Bool          noc_is_active;
   SceMiLinkType link_type;
} SceMiNoCArgsNoClock;

//
typedef struct {
   Bit#(6)     opcode;
   Bit#(1)     unused;
   Bool        dont_wait;
   UInt#(8)    length;
   NodeID      src;
   NodeID      dst;
} NoCHeader deriving (Bits, Eq);

typedef enum {
   INPORT_DATA,
   INPORT_REQ,
   OUTPORT_DATA,
   OUTPORT_ACK
} SceMi1MsgType deriving (Bits, Eq);

typedef struct {
   SceMi1MsgType     msg_type;
   Bit#(30)          payload;
} SceMi1Header deriving (Bits, Eq);

typedef struct {
   SceMi1MsgType     msg_type;
   Bool              last;
   Bit#(19)          unused;
   UInt#(10)         port;
} SceMi1InPortReq deriving (Bits, Eq);

typedef struct {
   SceMi1MsgType     msg_type;
   Bool              last;
   UInt#(19)         bitsrem;
   UInt#(10)         port;
} SceMi1InPortData deriving (Bits, Eq);

typedef struct {
   SceMi1MsgType     msg_type;
   Bool              last;
   UInt#(19)         bitsrem;
   UInt#(10)         port;
} SceMi1OutPortData deriving (Bits, Eq);

typedef struct {
   SceMi1MsgType     msg_type;
   Bool              last;
   Bit#(2)           unused;
   Maybe#(UInt#(10)) port2;
   Bit#(5)           unused2;
   Maybe#(UInt#(10)) port1;
} SceMi1OutPortAck deriving (Bits, Eq);

typedef struct {
   Bool              is_data;
   Bool              more;
   Bool              flush;
   Bool              eom;
   UInt#(12)         pipe;
   UInt#(16)         count;
} SceMi2Msg deriving (Bits, Eq);


// This module builds the transactor hierarchy, the clock
// generation logic and the NoC-to-port logic.
module [Module] buildSceMiNoC#( SceMiModule#(i) mod
                              , Clock noc_clk
                              , Reset noc_rst
                              , Bool noc_is_active
                              , SceMiLinkType link_type
                              )
                              (SceMiNoCIfc#(i));

   // Expose clock and reset
   Clock sys_clk  <- exposeCurrentClock();
   Reset sys_rstn <- exposeCurrentReset();

   // The sys_clk is the base SCE-MI clock
   Clock scemiClock = sys_clk;
   Reset scemiReset = sys_rstn;

   // We create a clock for use as the uncontrolled clock
   MakeClockIfc#(Bit#(1)) uclkgen <- mkUngatedClock(0, clocked_by scemiClock, reset_by scemiReset);
   Clock uClock = uclkgen.new_clk;

   PulseWire rising_uclock_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   ReadOnly#(Bool) rising_uclock = pulseWireToReadOnly(rising_uclock_pw);

   rule toggle_uclock;
      let new_value = ~uclkgen.getClockValue();
      uclkgen.setClockValue(new_value);
      if (new_value == 1)
         rising_uclock_pw.send();
   endrule

   // We create a reset for use as the uncontrolled reset
   MakeResetIfc rstgen <- genSceMiReset(uClock, clocked_by scemiClock, reset_by scemiReset);
   Reset uReset = rstgen.new_rst;

   // Setup initial state for the SceMiModule monad
   SceMiModuleState init_state <- get_initial_state(uClock, uReset,
                                                    scemiClock, scemiReset,
                                                    noc_clk, noc_rst,
                                                    rising_uclock, link_type);

   // Execute the SceMi module with the initial state
   let {state, _m} <- runWithContext(init_state, mod, clocked_by uClock, reset_by uReset);

   // Create the clock generation logic
   let clockGenerators <- build_clock_generator(state, rstgen);

   // Create the NoC-to-pipe logic
   MsgPortFilter#(BPB) _noc <- build_NoC_pipe_connections( state
							 , noc_clk
							 , noc_rst
							 , noc_is_active
							 , clocked_by scemiClock
							 , reset_by scemiReset
							 );
   // Pass along the required interface elements
   interface orig_ifc = _m;
   interface noc_src  = _noc.noc_src;
   interface noc_cont = _noc.noc_cont;
   method Bool isOutOfReset     = clockGenerators.outOfReset;
   method Bool isClockAdvancing = clockGenerators.isClockAdvancing;

endmodule: buildSceMiNoC

module [Module] buildSceMiNoCNoClock#( SceMiModule#(i) mod
				      , Clock noc_clk
				      , Reset noc_rst
				      , Bool noc_is_active
				      , SceMiLinkType link_type
				      )
                                      (SceMiNoCIfc#(i));

   // Expose clock and reset
   Clock sys_clk  <- exposeCurrentClock();
   Reset sys_rstn <- exposeCurrentReset();

   // The sys_clk is the base SCE-MI clock
   Clock scemiClock = sys_clk;
   Reset scemiReset = sys_rstn;
   Clock uClock     = scemiClock;
   Reset uReset     = scemiReset;

   ReadOnly#(Bool) rising_uclock =
      (interface ReadOnly;
	  method _read = True;
       endinterface );

   // Setup initial state for the SceMiModule monad
   SceMiModuleState init_state <- get_initial_state(uClock, uReset,
                                                    scemiClock, scemiReset,
                                                    noc_clk, noc_rst,
                                                    rising_uclock, link_type);

   // Execute the SceMi module with the initial state
   let {state, _m} <- runWithContext(init_state, mod, clocked_by uClock, reset_by uReset);

   // Create the NoC-to-pipe logic
   MsgPortFilter#(BPB) _noc <- build_NoC_pipe_connections( state
							 , noc_clk
							 , noc_rst
							 , noc_is_active
							 , clocked_by scemiClock
							 , reset_by scemiReset
							 );
   let isInReset <- isResetAsserted(clocked_by scemiClock, reset_by scemiReset);

   // Pass along the required interface elements
   interface orig_ifc = _m;
   interface noc_src  = _noc.noc_src;
   interface noc_cont = _noc.noc_cont;
   method Bool isOutOfReset     = !isInReset;
   method Bool isClockAdvancing = True;
endmodule: buildSceMiNoCNoClock

// Message input port for NoC
// This gets shifted in one-beat-at-a-time across the NoC
// and then read broadside into the transactor.

typedef enum { IDLE, WAITING, READY, ACK } PortStatus deriving (Bits,Eq);

module [SceMiModule] mkSceMiNoCMessageInPort(SceMiMessageInPortRawIfc#(a))
   provisos (  Bits#(a, sz)
	     , Add#(0, BPB, bpb)
	     , Mul#(bpb, 8, bitspb)
	     , Div#(sz, bitspb, nbeats)
	     , Add#(1,nbeats,nbeats_plus_1)
	     , Log#(nbeats_plus_1,cnt_bits)
	     );

   // Extract state from module context
   let state <- getContext();
   let uClock   = getUClock(state);
   let uReset   = getUReset(state);
   let nocClock = getNoCClock(state);
   let nocReset = getNoCReset(state);

   // When SCE-MI is reset, the input port must reset too
   Reset nocResetUClock <- mkAsyncReset(0, nocReset, uClock);

   Reg#(Bool) in_reset_uclk    <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool) prev_reset_uclk  <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool) in_reset_noc     <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc starting_reset <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset
   SyncPulseIfc ending_reset   <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset

   (* fire_when_enabled *) // in uClock domain
   rule deassert_after_reset if (in_reset_uclk);
      in_reset_uclk <= False;
   endrule

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_scemi_reset if (in_reset_uclk);
      starting_reset.send();
   endrule

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_end_of_scemi_reset;
      if (!in_reset_uclk && prev_reset_uclk)
         ending_reset.send();
      prev_reset_uclk <= in_reset_uclk;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule initiate_reset_sequence if (!in_reset_noc && starting_reset.pulse());
      in_reset_noc <= True;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule complete_reset_sequence if (in_reset_noc && ending_reset.pulse());
      in_reset_noc <= False;
   endrule

   // For 4-byte NoC the data will get split into 32-bit chunks.  The data
   // is shifted in 32-bits at a time from the NoC side (on the
   // nocClock).  The data is read in its entirety from the
   // transactor side (on the uClock).  Due to latency involved in the
   // path to move the port to the READY status, there is no
   // additional synchronization needed for this domain crossing.
   Integer num_beats = valueOf(nbeats);
   Vector#(nbeats,CrossingReg#(Bit#(bitspb))) scemiInportBeats <-
        replicateM(mkNullCrossingReg(uClock, unpack('0), clocked_by nocClock, reset_by nocReset));
   Vector#(nbeats,CrossingReg#(Bit#(bitspb))) beats = scemiInportBeats;

   // Count of the number of beats remaining to be written before the
   // entire value is ready to be read by the transactor.  This is
   // read and written from the NoC side (on the nocClock).
   Reg#(UInt#(cnt_bits)) remaining <- mkReg( 0
                                           , clocked_by nocClock
                                           , reset_by nocReset
                                           );
   SyncPulseIfc buffer_full_sp  <- mkSyncHandshake(nocClock, nocReset, uClock);
   SyncPulseIfc buffer_empty_sp <- mkSyncHandshake(nocClock, nocReset, uClock);

   // Register which maintains the current port status.  It is
   // read and written in the uClock domain, and read in the
   // nocClock domain.
   CrossingReg#(PortStatus) status <- mkNullCrossingReg(nocClock, IDLE, clocked_by uClock, reset_by uReset);

   SyncPulseIfc next_sp <- mkSyncHandshake(uClock, uReset, nocClock);
   SyncPulseIfc wait_sp <- mkSyncHandshake(uClock, uReset, nocClock);
   PulseWire request_pw  <- mkPulseWire(clocked_by uClock, reset_by uReset);
   PulseWire got_beat_pw <- mkPulseWire(clocked_by nocClock, reset_by nocReset);

   // On the NoC-side, the port is written one beat at a time
   // using a shift register.
   InPortInfo info;
   info.portNum = state.inPortCount;
   info.bitWidth = valueOf(sz);
   info.beatIfc = (interface BeatLevelInPortIfc;
                      method Bool sendRequest();
                         return next_sp.pulse();
                      endmethod
                      method Bool awaitingData();
                         return (remaining != 0);
                      endmethod
                      method Action putBeat(Bit#(bitspb) x);
                         if (remaining != 0) begin
                            if (num_beats > 1) begin
                               for (Integer n = num_beats - 1;
                                    n > 0;
                                    n = n - 1) begin
                                  beats[n-1] <= beats[n];
                               end
                            end
                            beats[num_beats-1] <= x;
                            got_beat_pw.send();
                         end
                      endmethod
                   endinterface);
   state.inPortCount = state.inPortCount + 1;
   state.inports = List::cons(info,state.inports);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_channelId <- mkSceMiUInt32Parameter(fromInteger(info.portNum));

   // Implement the port status state machine
   rule first_request if (status == IDLE && request_pw);
      status <= WAITING;
      next_sp.send();
      wait_sp.send();
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
      wait_sp.send();
   endrule

   // reset NoC part of port on SCE-MI reset (executes in nocClock domain)
   rule handle_scemi_reset if (in_reset_noc);
      remaining <= 0;
   endrule

   // in nocClock domain
   rule update_remaining if (!in_reset_noc && (got_beat_pw || next_sp.pulse() || wait_sp.pulse()));
      if (got_beat_pw) begin
         if (remaining == 1)
            buffer_full_sp.send();
         remaining <= remaining - 1;
      end
      else begin
	 if (next_sp.pulse()) begin
            buffer_empty_sp.send();
	 end
	 if (wait_sp.pulse()) begin
            remaining <= fromInteger(num_beats);
	 end
      end
   endrule

   method Bool has_data();
      return (status == READY);
   endmethod: has_data

   method a read() if (status == READY);
      let val = pack(Vector::map(readCrossingRegDst,beats));
      return unpack(val[valueOf(sz)-1:0]);
   endmethod: read

   method Action request;
      request_pw.send();
   endmethod: request

endmodule: mkSceMiNoCMessageInPort


// Message output port for NoC
// This gets written broadside from the transactor and then muxed
// out one-beat-at-a-time across the NoC bus.
module [SceMiModule] mkSceMiNoCMessageOutPort(SceMiMessageOutPortIfc#(a))
   provisos (  Bits#(a, sz)
	     , Add#(0, BPB, bpb)
	     , Mul#(bpb, 8, bitspb)
	     , Div#(sz, bitspb, nbeats)
	     , Add#(1,nbeats,nbeats_plus_1)
	     , Log#(nbeats_plus_1,cnt_bits)
	     );

   // Extract state from module context
   let state <- getContext();
   let uClock   = getUClock(state);
   let uReset   = getUReset(state);
   let nocClock = getNoCClock(state);
   let nocReset = getNoCReset(state);

   // When SCE-MI is reset, the output port must reset too
   Reset nocResetUClock <- mkAsyncReset(0, nocReset, uClock);

   Reg#(Bool) in_reset_uclk    <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool) prev_reset_uclk  <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool) in_reset_noc     <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc starting_reset <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset
   SyncPulseIfc ending_reset   <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset

   (* fire_when_enabled *) // in uClock domain
   rule deassert_after_reset if (in_reset_uclk);
      in_reset_uclk <= False;
   endrule

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_scemi_reset if (in_reset_uclk);
      starting_reset.send();
   endrule

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_end_of_scemi_reset;
      if (!in_reset_uclk && prev_reset_uclk)
         ending_reset.send();
      prev_reset_uclk <= in_reset_uclk;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule initiate_reset_sequence if (!in_reset_noc && starting_reset.pulse());
      in_reset_noc <= True;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule complete_reset_sequence if (in_reset_noc && ending_reset.pulse());
      in_reset_noc <= False;
   endrule

   // For 4-byte NoC the data will get split into 32-bit chunks.  The
   // data is written in its entirety from the transactor side (on the
   // uClock) and then read out 32-bits at a time across the NoC
   // (on the nocClock).
   Integer num_beats = valueOf(nbeats);
   Vector#(nbeats,CrossingReg#(Bit#(bitspb))) beats <-
       replicateM(mkNullCrossingRegU(nocClock, clocked_by uClock, reset_by uReset));

   Reg#(UInt#(cnt_bits)) count <- mkReg(0, clocked_by nocClock, reset_by nocReset);

   PulseWire decr <- mkPulseWire(clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc next <- mkSyncHandshake(uClock, uReset, nocClock);
   SyncPulseIfc finished <- mkSyncHandshake(nocClock, nocReset, uClock);

   // On the NoC-side, the port is read one beat at a time using a
   // mux.  We don't use a shift register because of the clock domain
   // crossing -- it would have to be written on uClock and shifted on
   // nocClock.
   Bit#(bitspb) current_beat = (count > 0) ? Vector::reverse(beats)[count-1].crossed() : ?;
   OutPortInfo info;
   info.portNum = state.outPortCount;
   info.bitWidth = valueOf(sz);
   info.beatIfc = (interface BeatLevelOutPortIfc;
                      method ActionValue#(Bit#(bitspb)) takeBeat();
                         if (count != 0) decr.send();
                         return current_beat;
                      endmethod
                      method Action ack();
                         finished.send();
                      endmethod
                   endinterface);
   state.outPortCount = state.outPortCount + 1;
   state.outports = List::cons(info,state.outports);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_channelId <- mkSceMiUInt32Parameter(fromInteger(info.portNum));

   rule update_count if (!in_reset_noc && (decr || next.pulse()));
      if (decr)
         count <= count - 1;
      else
         count <= fromInteger(num_beats);
   endrule

   // On the user-side, the port is accessed using the whole vector.

   UInt#(10) port = fromInteger(info.portNum);
   Reg#(Bool) ok <- mkReg(True, clocked_by uClock, reset_by uReset);

   // XXXX This is wrong w.r.t. scemi spec.   The round-robin implemention of the send causes the
   // the accepting_data to toggle, whereas the spec says once ready, it will stay ready.
   Bool ok_to_send = ok && !isInReset(state) && (state.currentOutPort == port) && canAddOutputMsgChannel(state);
   Probe#(Bool) okToSend <- mkProbe;
   (*hide_all*)  let pok <- mkConnection (okToSend._write, ok_to_send);

   rule setOK if (finished.pulse());
      ok <= True;
   endrule

   // reset NoC part of port on SCE-MI reset (executes in nocClock domain)
   rule handle_scemi_reset if (in_reset_noc);
      count <= 0;
   endrule

   method Bool accepting_data();
      return ok_to_send;
   endmethod: accepting_data

   method Action send(a x) if (ok_to_send);
      ok <= False;
      Vector::joinActions(Vector::zipWith(writeCrossingReg,beats,toChunks(x)));
      next.send();
      addOutputMsgChannel(state,port);
   endmethod: send

endmodule: mkSceMiNoCMessageOutPort

// SCE-MI clock port for NoC
// This uses the BSV clock generation logic.
module [SceMiModule] mkSceMiNoCClockPort#( parameter Integer clockNum
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
endmodule: mkSceMiNoCClockPort

// SCE-MI clock controller for NoC
// This uses the BSV clock generation logic.
module [SceMiModule] mkSceMiNoCClockControl#( parameter Integer clockNum
                                            , Bool allow_pos_edge
                                            , Bool allow_neg_edge
                                            )
                                            (SceMiClockControlIfc);
   (*hide*)
   let _m <- mkSceMiBSVClockControl(clockNum, allow_pos_edge, allow_neg_edge);
   return _m;
endmodule: mkSceMiNoCClockControl

// --------------------

typedef enum {NoDelay, Timer, Value, Both} InpipeCreditReturnMode deriving (Eq);

typedef struct {
   Integer     capacity;
   Visibility  vis;
   Integer     consumeTimer;
   Integer     creditFifoDepth;
   Integer     infoFifoDepth;
   UInt#(16)   creditReturnThreshold;
   InpipeCreditReturnMode creditReturn;

   // Ideas for other parameters
   // Drop eom bit
} SceMiInputPipeParameters;

instance DefaultValue #( SceMiInputPipeParameters );
   function SceMiInputPipeParameters defaultValue();
      return SceMiInputPipeParameters {
         capacity : 32
         , vis : Fifo
         , consumeTimer : 15
         , creditFifoDepth : 1
         , infoFifoDepth : 2
         , creditReturnThreshold : 1
         , creditReturn : Timer
         };
   endfunction
endinstance

module [SceMiModule] mkSceMiNoCInputPipe#(Integer depth
                                          , Visibility vis
   ) (SceMiInputPipeIfc#(m,e))
   provisos ( Bits#(e, esz)
             , Div#(esz, 8, e_bytes)
             , Add#(0, BPB, bpb)
             // The max payload size "m" must be at least 1
             , Add#(1, _xx0, m)
             // We must be able to index "m" by a 16-bit pointer
             , Add#(_xx1, TLog#(m), 16)
             );

   SceMiInputPipeParameters params = defaultValue;
   params.capacity = depth;
   params.vis   = vis;

   (*hide*)
   let _ip <- mkSceMiNoCInputPipeP (params);
   return _ip;
endmodule

module [SceMiModule] mkSceMiNoCInputPipeP#(SceMiInputPipeParameters params)
                                         (SceMiInputPipeIfc#(m,e))
   provisos ( Bits#(e, esz)
             , Div#(esz, 8, e_bytes)
             , NumAlias#( BPB, bpb)
             // The max payload size "m" must be at least 1
             , Add#(1, _xx0, m)
             // We must be able to index "m" by a 16-bit pointer
             , Add#(_xx1, TLog#(m), 16)
             ,Mul#(e_bytes, 8, esz2)
           );


   // Extract state from module context
   let state <- getContext();

   let uClock   = getUClock(state);
   let uReset   = getUReset(state);

   let nocClock = getNoCClock(state);
   let nocReset = getNoCReset(state);

   Reset nocResetUClock <- mkAsyncReset(0, nocReset, uClock);


   // Check that the depth is at least as large as the maximum payload
   if (params.capacity < valueof(m))
      error("SCE-MI input pipe depth cannot be smaller than maximum payload size.");

   // ---------------

   // When SCE-MI is reset, the input pipe runs through a reset sequence
   Reg#(Bool) reset_uclk_done1    <- mkRegA(False, clocked_by uClock, reset_by uReset);
   Reg#(Bool) reset_uclk_done2    <- mkRegA(False, clocked_by uClock, reset_by uReset);

   rule passReset (!reset_uclk_done1);
      // indicate after reset is dasserted
      reset_uclk_done1 <= True;
   endrule
   Bool in_reset_uclk = ! reset_uclk_done2;

   Reg#(Bool) in_reset_noc     <- mkRegA(False, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc starting_reset <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset
   SyncPulseIfc ending_reset   <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_scemi_reset (!reset_uclk_done1);
      starting_reset.send();
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule initiate_reset_sequence if (!in_reset_noc && starting_reset.pulse());
      in_reset_noc <= True;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule complete_reset_sequence if (in_reset_noc && ending_reset.pulse);
      in_reset_noc <= False;
   endrule

   // ---------------

   // Pipe state (consumer) and FSM actions

   Vector#(m,Reg#(Tuple2#(Bool,e))) elems         <- replicateM(mkRegU, clocked_by uClock, reset_by uReset);
   Reg#(UInt#(16))                  elem_count    <- mkRegU(clocked_by uClock, reset_by uReset);
   Count#(UInt#(16))                credits       <- mkCount(0, clocked_by uClock, reset_by uReset);
   Reg#(UInt#(16))                  elems_recvd   <- mkRegU(clocked_by uClock, reset_by uReset);
   Reg#(Bool)                       underflow     <- mkRegU(clocked_by uClock, reset_by uReset);
   Reg#(Bool)                       active        <- mkRegU(clocked_by uClock, reset_by uReset);
   Reg#(Bool)                       pending_send  <- mkRegU(clocked_by uClock, reset_by uReset);

   Reg#(Bool)                       pending_flush <- mkReg(False, clocked_by uClock, reset_by uReset);

   UCount                           consume_timer <- mkUCount(params.consumeTimer, params.consumeTimer, clocked_by uClock, reset_by uReset);
   Reg#(Bool)                       send_credit_request <- mkRegU(clocked_by uClock, reset_by uReset);
   Reg#(Bool)                       send_underflow <- mkRegU(clocked_by uClock, reset_by uReset);


   // Clock-crossing FIFO for data message info (count, more, flush)
   SyncFIFOIfc#(Tuple3#(UInt#(16),Bool,Bool)) data_info_fifo <- mkSyncFIFO(params.infoFifoDepth, nocClock, nocReset, uClock);

   // Clock-crossing FIFO for credits returned
   // (when elems are taken out of the register array) (count, underflow)
   SyncFIFOIfc#(Tuple2#(UInt#(16),Bool)) credit_fifo <- mkSyncFIFO(params.creditFifoDepth, uClock, uReset, nocClock);

   Wire#(Tuple4#(UInt#(16),Bool,Bool,Bool))      updates_from_msg <- mkDWire( tuple4(0,False,False,False)
                                                                            , clocked_by uClock, reset_by uReset
                                                                            );
   Wire#(Tuple4#(UInt#(16),UInt#(16),Bool,Bool)) updates_from_ifc <- mkDWire( tuple4(0,0,False,False)
                                                                            , clocked_by uClock, reset_by uReset
                                                                            );
   rule consumer_handle_msg if (!in_reset_uclk);
      match {.n, .overflow, .flush} = data_info_fifo.first();
      data_info_fifo.deq();

      Bool      return_credits  = False;

      if (params.vis == Deferred) begin
         return_credits =  ((credits == fromInteger(params.capacity)) && overflow)
                        || ((elems_recvd == 0) && (n == 0) && flush)
                        ;
      end

      updates_from_msg <= tuple4(n, return_credits, overflow, flush);

   endrule

   function ActionValue#(UInt#(16)) consumer_try_receive(UInt#(16) num_elems, UInt#(16) elems_available);
      actionvalue

         UInt#(16) yielded        = 0;
         Bool      return_credits = False;
         Bool      send_fail      = False;

         if (params.vis == Deferred) begin
            yielded        = !active ? 0 : min(num_elems,elems_available);
            return_credits =  (  ((credits + yielded) == fromInteger(params.capacity))
                              && ((yielded != num_elems) || pending_send)
                              )
                           || ((yielded == elems_recvd) && pending_flush)
                           ;
            send_fail      = ((yielded == elems_recvd) && (num_elems > yielded)) && !underflow;
         end
         else begin
            yielded        = min(num_elems,elems_available);
            return_credits = (yielded != 0);
         end

         updates_from_ifc <= tuple4(yielded, num_elems, return_credits, send_fail);

         return (yielded);
      endactionvalue
   endfunction

   // Count down on the timer to know when we are idle
   rule tick_timer (! consume_timer.isEqual (0));
      consume_timer.decr(1);
   endrule

   // Delaying of deferring when to send credits changes performance
   Bool sendCreditsNow = case (params.creditReturn)
                            NoDelay : return True;
                            Timer   : return consume_timer.isEqual(0);
                            Value   : return credits >= params.creditReturnThreshold;
                            Both    : return (( consume_timer.isEqual(0)) ||
                                          (credits >= params.creditReturnThreshold));
                         endcase;

   rule send_credit_message (reset_uclk_done2 &&
                             (send_underflow ||
                              (credits == fromInteger(params.capacity)) ||
                              (sendCreditsNow && (credits != 0) && send_credit_request)
                              ) );
      credit_fifo.enq (tuple2( credits,  send_underflow) );
      credits.decr(credits);

      send_underflow      <= False;
      send_credit_request <= False;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule update_inpipe_consumer_state if (!in_reset_uclk);
      match {.elems_added, .return_credits1, .overflow, .flush}            = updates_from_msg;
      match {.elems_taken, .elems_requested, .return_credits2, .send_fail} = updates_from_ifc;

      UInt#(16) elems_recvd0   = elems_recvd;
      Bool      underflow0     = underflow;
      Bool      active0        = active;
      Bool      pending_send0  = pending_send;
      Bool      pending_flush0 = pending_flush;

      // order as if updates_from_msg happen before updates_from_ifc

      elems_recvd0  = elems_recvd + elems_added;
      underflow0    = underflow && (elems_added == 0);

      if (params.vis == Deferred) begin
         active0        = (active || (elems_added != 0)) && !return_credits1;
         pending_send0  = (pending_send || overflow) && !return_credits1;
         pending_flush0 = (pending_flush || flush) && !return_credits1;
      end

      elems_recvd <= elems_recvd0 - elems_taken;
      underflow   <= (underflow0 && (elems_requested == 0))
                  || (  (elems_taken != elems_requested)
                     && (elems_taken == elems_recvd0)
                     )
                  ;

      if (params.vis == Deferred) begin
         active        <= active0 && !send_fail;
         pending_send  <= pending_send0 && !return_credits2;
         pending_flush <= pending_flush0 && !return_credits2;
      end

      credits.incr (elems_taken);
      if (send_fail)   send_underflow <= True;
      if (return_credits1 || return_credits2)  send_credit_request <= True;
      if (elems_taken != 0)  consume_timer <= params.consumeTimer;

   endrule

   // ---------------

   // Incoming messages are decoded and the data is assembled into a buffer
   // the size of one element.  When an element is assembled, it is enqueued
   // into a FIFO which synchronizes the data into the uClock domain and
   // serves as additional buffer beyond the "max" value.

   function  divRound(x,y) = (x + (y-1)) / y;

   Integer bytes_per_elem = fromInteger(valueOf(e_bytes));
   Integer ibpb = valueOf(bpb);
   Integer beats_per_elem = divRound(bytes_per_elem, ibpb);

   // The fifo for synchronizing and buffering
   // It contains elements, paired with whether they are the "eom".
   Integer in_fifo_depth = max(2,params.capacity - valueOf(m));
   SyncFIFOIfc#(Tuple2#(Bool,e))
       in_fifo <- mkSyncFIFO(in_fifo_depth, nocClock, nocReset, uClock);

   // mimo to pack bytes-in to elements out.
   MIMO#(bpb,e_bytes,TAdd#(bpb, e_bytes), Bit#(8))  mimo <- mkMIMOV (clocked_by nocClock, reset_by nocReset);
   let ie_bytes = fromInteger (valueOf (e_bytes));

   // Count of bytes in the buffer and the number of bytes beyond the
   // current record

   Integer bytes_in_record = bytes_per_elem + (((valueOf(esz) % 8) == 0) ? 1 : 0);

   // Use config reg here, since read and write are disjoint, but bsc does not see it.
   Reg#(UInt#(16)) ecount <- mkConfigReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)      eom_in <- mkConfigRegU( clocked_by nocClock, reset_by nocReset);

   (* fire_when_enabled *)
   rule take_completed_element if (!in_reset_noc && ecount != 0 && mimo.deqReadyN(ie_bytes));
      Vector#(e_bytes, Bit#(8))  dout = mimo.first;
      Bit#(esz2) x1 = pack(dout);
      e v = unpack (truncateNP(x1));
      Bool eom = (ecount == 1) && (eom_in);

      mimo.deq(ie_bytes);

      in_fifo.enq(tuple2(eom,v));
      ecount <= ecount - 1;
   endrule

   // ---------------

   // Elements are taken from the FIFO and put into a register array,
   // containing up to the maximum number of elements that can be taken.
   // TODO  another job for a mimo
   PulseWire        elem_count_incr <- mkPulseWire(clocked_by uClock, reset_by uReset);
   Wire#(UInt#(16)) elem_count_decr <- mkDWire(0, clocked_by uClock, reset_by uReset);

   // Drain from the SyncFIFO to the elem array
   (* fire_when_enabled *)
   rule move_elem ((elem_count != fromInteger(valueOf(m))) && !in_reset_uclk);
      in_fifo.deq();
      elems[elem_count] <= in_fifo.first();
      elem_count_incr.send();
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule update_elem_count if (!in_reset_uclk);
      elem_count <= elem_count - elem_count_decr + (elem_count_incr ? 1 : 0);
   endrule

   // ---------------

   // The reset sequence for the inpipe restores the pipe to its
   // initial state.

   (* fire_when_enabled *) // in nocClock domain
   rule drain_credit_fifo if (in_reset_noc);
      credit_fifo.deq();
   endrule

   (* fire_when_enabled *) // in uClock domain
   rule drain_data_info_fifo if (!reset_uclk_done2);
      data_info_fifo.deq();
   endrule

   (* fire_when_enabled *) // in uClock domain
   rule drain_in_fifo if (!reset_uclk_done2);
      in_fifo.deq();
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // in uClock domain
   rule reset_uclock_side ( reset_uclk_done1 && !reset_uclk_done2 &&
                           !data_info_fifo.notEmpty() && !in_fifo.notEmpty());
      writeVReg(elems,replicate(tuple2(False,?)));
      elem_count    <= 0;
      elems_recvd   <= 0;
      credits       <= 0;
      underflow     <= False;
      active        <= False;
      pending_send  <= False;
      reset_uclk_done1 <= True;
      send_credit_request  <= False;
      send_underflow <= False;
      ending_reset.send();
      reset_uclk_done2 <= True;
   endrule

   // ---------------

   // Store one side in the state
   PipeInfo#(InPipeIfc) info = ?;
   info.pipeNum = state.inPipeCount;
   info.pipeIfc = (interface InPipeIfc;
                      method Action data_msg(UInt#(16) n, Bool more, Bool flush, Bool eom) if (!in_reset_noc && ecount == 0);
                         data_info_fifo.enq(tuple3(n,more,flush));
                         ecount <= n;
                         eom_in <= eom;
                      endmethod
                      method Action add_data(LUInt#(bpb) count, MsgBeat#(bpb) beat) if (!in_reset_noc && mimo.enqReadyN(fromInteger(ibpb)));
                         mimo.enq(count, unpack(beat));
                      endmethod
                      method ActionValue#(Tuple2#(UInt#(16),Bool)) take_credit_msg() if (!in_reset_noc);
                         credit_fifo.deq();
                         return credit_fifo.first();
                      endmethod
                   endinterface);
   state.inPipeCount = state.inPipeCount + 1;
   state.input_pipes = List::cons(info,state.input_pipes);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_pipeNum    <- mkSceMiUInt32Parameter(fromInteger(info.pipeNum));
   Ignored param_depth      <- mkSceMiUInt32Parameter(fromInteger(params.capacity));
   Ignored param_visibility <- mkSceMiStringParameter( (params.vis == Deferred)  ? "Deferred"
                                                     : (params.vis == Immediate) ? "Immediate"
                                                     :                      "Fifo"
                                                     );

   // ---------------

   // Return the SceMi input pipe interface
   //

   // Pointer to the location after the first "eom", if there is one
   // (add one to get the location after)
   Maybe#(UInt#(16)) m_eom_ptr =
      (findIndex(tpl_1, readVReg(elems)) matches tagged Valid .res
          ? tagged Valid (extend(res) + 1) : tagged Invalid);

   // Only return up to the next "eom"
   // (by keeping "eom" False for unused entries, we know that "eom_ptr"
   // will not be greater than "elem_count")
   UInt#(16) elems_available = (m_eom_ptr matches tagged Valid .eom_ptr
                                   ? eom_ptr : elem_count);

   // Only allow data to be received if enough of the available data
   // has made it into the elem array
   Bool has_enough_data =  (elem_count == fromInteger(valueOf(m)))
                        || (elem_count >= elems_recvd)
                        ;

   method UInt#(32) can_receive() = (in_reset_uclk || !has_enough_data)
                                  ? 0
                                  : extend(elem_count)
                                  ;

   method ActionValue#(Tuple3#(UInt#(32), Vector#(m,e), Bool))
      try_receive(UInt#(32) num_elems);
      if (in_reset_uclk || !has_enough_data )
         return tuple3(0,replicate(?),False);
      else begin
         UInt#(16) elems_taken <- consumer_try_receive(truncate(num_elems),elems_available);

         let old_vec = readVReg(elems);
         // Use '?' to optimize the logic
         let new_vec = shiftOutFrom0(tuple2(False,?), old_vec, elems_taken);
         writeVReg(elems, new_vec);

         elem_count_decr <= elems_taken;

         Bool eom = (m_eom_ptr matches tagged Valid .eom_ptr
                  ? (eom_ptr == elems_taken)
                  : False);

         Vector#(m,e) es = map(tpl_2, readVReg(elems));

         return tuple3(extend(elems_taken), es, eom);
      end
   endmethod

endmodule: mkSceMiNoCInputPipeP

/////////////////////////////////////////////////////////////////////////////

typedef struct {
   Integer     capacity;
   Visibility  vis;
   Integer     creditFifoDepth;
   Integer     infoFifoDepth;
   Integer     accumulateTimer;
   Integer     creditTimer;

   // Ideas for other parameters
   // send info message limit
   // Host Credit limit
} SceMiOutputPipeParameters;

instance DefaultValue #( SceMiOutputPipeParameters );
   function SceMiOutputPipeParameters defaultValue();
      return SceMiOutputPipeParameters {
         capacity          : 32
         , vis             : Fifo
         , creditFifoDepth : 1
         , infoFifoDepth   : 2
         , accumulateTimer : 31
         , creditTimer     : 7
         };
   endfunction
endinstance

module [SceMiModule] mkSceMiNoCOutputPipe#(Integer depth, Visibility vis)
                                          (SceMiOutputPipeIfc#(m,e))
   provisos (  Bits#(e, esz)
             , Div#(esz, 8, e_bytes)
             , NumAlias#(BPB, bpb)
             // The max payload size "m" must be at least 1
            , Add#(1, _0, m)
             // We must be able to index "m" by a 16-bit pointer
             , Add#(_xx1, TLog#(m), 16)
            );

   SceMiOutputPipeParameters params = defaultValue;
   params.vis = vis;
   params.capacity = depth;

   (*hide*)
   let _op <- mkSceMiNoCOutputPipeP (params);
   return _op;

endmodule

module [SceMiModule] mkSceMiNoCOutputPipeP#(SceMiOutputPipeParameters prms
                                            ) (SceMiOutputPipeIfc#(m,e))
   provisos ( Bits#(e, esz)
             , Div#(esz, 8, e_bytes)
             , NumAlias#(BPB, bpb)
             // The max payload size "m" must be at least 1
             , Add#(1, _0, m)
             // We must be able to index "m" by a 16-bit pointer
             , Add#(_xx1, TLog#(m), 16)
            );

   let params = prms;


   // Extract state from module context
   let state <- getContext();

   let uClock   = getUClock(state);
   let uReset   = getUReset(state);

   let nocClock = getNoCClock(state);
   let nocReset = getNoCReset(state);

   // Check that the depth is at least as large as the maximum payload
   if (params.capacity < valueof(m))
      error("SCE-MI output pipe depth cannot be smaller than maximum payload size.");

   Integer ibpb = valueOf(bpb);
   let inVectorWidth = fromInteger (valueOf(m));

   // ---------------
   // Function to compute the number of payload bytes in a data message

   function UInt#(32) get_byte_count(UInt#(16) n);
      // n is the number of elements
      // n can never be greater than  params.capacity, so we can 0 the uppper bits.
      Integer maskI = (2**(log2(params.capacity + 1)) - 1 );
      UInt#(16)  maskU = fromInteger(maskI) ;
      UInt#(32) res = zeroExtend(maskU & n) * fromInteger(valueOf (e_bytes));
      return res;
   endfunction

   // Pipe state (producer)

   Vector#(m, Reg#(e)) elems        <- replicateM(mkRegU, clocked_by uClock, reset_by uReset);
   Reg#(UInt#(16))     elem_count   <- mkReg(0, clocked_by uClock, reset_by uReset);
   Reg#(UInt#(16))     credits      <- mkReg(fromInteger(params.capacity), clocked_by uClock, reset_by uReset);
   Reg#(Bool)          overflow     <- mkReg(False, clocked_by uClock, reset_by uReset);
   Reg#(Bool)          flushing     <- mkReg(False, clocked_by uClock, reset_by uReset);
   Reg#(Bool)          active       <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool)          pending_recv <- mkReg(False, clocked_by uClock, reset_by uReset);
   Reg#(Bool)          autoflush    <- mkReg(False, clocked_by uClock, reset_by uReset);

   // ---------------

   // When SCE-MI is reset, the output pipe runs through a reset sequence
   Reset nocResetUClock <- mkAsyncReset(0, nocReset, uClock);

   Reg#(Bool)   in_reset_uclk   <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool)   prev_reset_uclk <- mkReg(True, clocked_by uClock, reset_by uReset);
   Reg#(Bool)   in_reset_noc    <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   SyncPulseIfc starting_reset  <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset
   SyncPulseIfc ending_reset    <- mkSyncPulse(uClock, nocResetUClock, nocClock); // don't use uReset

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_scemi_reset if (in_reset_uclk);
      starting_reset.send();
   endrule

   (* fire_when_enabled *) // executes in uClock domain, during reset
   rule detect_end_of_scemi_reset;
      if (!in_reset_uclk && prev_reset_uclk)
         ending_reset.send();
      prev_reset_uclk <= in_reset_uclk;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule initiate_reset_sequence if (!in_reset_noc && starting_reset.pulse());
      in_reset_noc <= True;
   endrule

   (* fire_when_enabled *) // executes in nocClock domain
   rule complete_reset_sequence if (in_reset_noc && ending_reset.pulse);
      in_reset_noc <= False;
   endrule

   // ---------------

   // Outgoing elements are first put into a shift register and then
   // shifted one-at-a-time into a SyncFIFO which moves the data to
   // the NoC clock domain and provides enough buffering to meet the
   // depth requirement of the pipe.
   MIMO#(m, 1, TAdd#(m,1), Bit#(esz))  indata_mimo <- mkMIMOV(clocked_by uClock, reset_by uReset);
   Bool indata_mimo_not_ready = ! indata_mimo.enqReadyN(inVectorWidth);

   // These wires are needed so that we can structure the code in a
   // way that avoids false conflicts (Data and Count )
   Wire#(Tuple2#(Vector#(m,e),UInt#(16))) add_to_output_buffer
       <- mkDWire(tuple2(?,0), clocked_by uClock, reset_by uReset);

   // The fifo for synchronizing and buffering
   // It contains elements, paired with whether they are the "eom".
   Integer out_fifo_depth = max(2,params.capacity - valueOf(m));
   SyncFIFOIfc#(e)
       out_fifo <- mkSyncFIFO(out_fifo_depth, uClock, uReset, nocClock);

   (* fire_when_enabled *)
   rule update_output_buffer if (!in_reset_uclk);
      match {.data, .n} = add_to_output_buffer;
      if (n != 0) begin
         indata_mimo.enq( truncateNP(n), map (pack, data));
      end
   endrule

   (* fire_when_enabled *)
   rule shift_elements_out_of_buffer if (!in_reset_uclk && indata_mimo.deqReadyN(1));
      Bit#(esz) data = head (indata_mimo.first());
      out_fifo.enq(unpack(data));
      indata_mimo.deq(1);
   endrule


   // ---------------

   // When a request to send buffered data out on the NoC is received
   // in the NoC clock domain, data is drained from the out_fifo and
   // converted into beats for transmission across the NoC to the
   // consumer.

   // Clock-crossing FIFO for data message info
   SyncFIFOIfc#(Tuple4#(UInt#(32),Bool,Bool, Bool)) data_info_fifo
     <- mkSyncFIFO(params.infoFifoDepth, uClock, uReset, nocClock);

   // Staging area for NoC beats
   Reg#(Vector#(e_bytes,Bit#(8)))            noc_buf       <- mkRegU(clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(16))                           noc_buf_bytes <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   PulseWire data_beat_taken <- mkPulseWire(clocked_by nocClock, reset_by nocReset);

   (* fire_when_enabled *)
   rule transfer_element_data if (!in_reset_noc && (noc_buf_bytes == 0));
      e val = out_fifo.first();
      out_fifo.deq();
      noc_buf       <= unpack(extendNP(pack(val)));
      noc_buf_bytes <= fromInteger(valueOf(e_bytes));
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule remove_taken_beat if (!in_reset_noc && (noc_buf_bytes != 0) && (data_beat_taken));
      LUInt#(bpb) n = truncate(min(fromInteger(ibpb),noc_buf_bytes));
      noc_buf       <= shiftOutFrom0(0,noc_buf,ibpb);
      noc_buf_bytes <= noc_buf_bytes - zeroExtend(n);
   endrule

   // ---------------

   // Clock-crossing FIFO for credits returned
   SyncFIFOIfc#(Tuple3#(UInt#(16),Bool,Bool)) credit_fifo <- mkSyncFIFO(params.creditFifoDepth, nocClock, nocReset, uClock);

   Wire#(Tuple4#(UInt#(16),Bool,Bool,Bool))           updates_from_msg
       <- mkDWire(tuple4(0,False,False,False), clocked_by uClock, reset_by uReset);
   // elements tryed, elements accepted, send_now, send_failed, flush, eom
   Wire#(Tuple6#(UInt#(16),UInt#(16),Bool,Bool,Bool, Bool)) updates_from_ifc
       <- mkDWire(tuple6(0,0,False,False,False,False), clocked_by uClock, reset_by uReset);
   PulseWire flush_request <- mkPulseWire(clocked_by uClock, reset_by uReset);
   PulseWire flush_done    <- mkPulseWire(clocked_by uClock, reset_by uReset);
   Reg#(Bool) flush_requested <- mkReg(False, clocked_by uClock, reset_by uReset);

   UCount          accumulateTimer <- mkUCount(params.accumulateTimer, params.accumulateTimer, clocked_by uClock, reset_by uReset);
   Reg#(Bool)      sendDataOK      <- mkReg(False, clocked_by uClock, reset_by uReset);

   (* fire_when_enabled *)
   rule handle_returned_credits if (!in_reset_uclk && data_info_fifo.notFull());
      match {.n, .underflow, .current_autoflush} = credit_fifo.first();
      credit_fifo.deq();

      UInt#(16) new_credits = credits + n;
      Bool send_data    = False;
      Bool end_overflow = False;

      if (params.vis == Deferred) begin
         send_data    = (elem_count == fromInteger(params.capacity)) && underflow;
         end_overflow = overflow && (n != 0);
      end
      else begin
         UInt#(16) threshold = (params.vis == Fifo) ? 1 : fromInteger(params.capacity);
         end_overflow = overflow && (new_credits >= threshold);
      end

      autoflush <= current_autoflush;
      updates_from_msg <= tuple4(n,underflow,send_data,end_overflow);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule register_flush_request if (!in_reset_uclk && (flush_request || flush_done));
      flush_requested <= flush_request;
   endrule

   (* fire_when_enabled *)
   rule update_producer_state if (!in_reset_uclk);
      match {.credits_added, .underflow, .send_data0, .end_overflow}            = updates_from_msg;
      match {.num_elems, .credits_consumed, .send_data1, .send_fail, .do_flush, .eom} = updates_from_ifc;

      // order as if updates_from_msg happen before updates_from_ifc

      UInt#(16) credits0      = credits + credits_added;
      Bool      became_empty  = (credits0 == fromInteger(params.capacity)) && (credits_added != 0);
      Bool      overflow0     = overflow && !end_overflow;
      Bool      flushing0     = flushing && !became_empty;
      Bool      active0       = active || (credits_added != 0);
      UInt#(16) elems_to_send = elem_count;
      Bool      do_flush2     = False;
      Bool      nextSendDataOK = sendDataOK || send_data1;

      if (flush_requested && (credits0 != fromInteger(params.capacity)) && !flushing0)
         do_flush2 = True;

      // if we want to send the data to the consumer at this point,
      // put the relevant info into the data_info_fifo
      if (send_data0) begin           // Send now -- deferred mode underflow
         data_info_fifo.enq(tuple4(get_byte_count(elem_count),False,False, eom));
         elem_count <= credits_consumed ;
         nextSendDataOK = False;
      end
      else if (send_fail || do_flush || do_flush2 || eom) begin // Send now, flush or overflow
         data_info_fifo.enq(tuple4( get_byte_count(elem_count + credits_consumed)
                                  , send_fail
                                  , do_flush || do_flush2
                                  , eom
                                  )
                           );
         elem_count <= 0;
         nextSendDataOK = False;
      end
      else if (credits_consumed != 0) begin // new data by producer
         accumulateTimer <=   params.accumulateTimer;
         elem_count <= elem_count + credits_consumed;
      end
      else if ((elem_count != 0) && sendDataOK &&
               ( accumulateTimer.isEqual(0) || (credits == 0) ))
         begin // time to send
            data_info_fifo.enq(tuple4(get_byte_count(elem_count),False,False, False));
            elem_count <= 0;
            nextSendDataOK = False;
         end
      else if ( ! accumulateTimer.isEqual (0) ) begin // tick-tock
         accumulateTimer.decr (1);
      end

      sendDataOK <= nextSendDataOK;

      credits      <= credits0 - credits_consumed;
      overflow     <= overflow0 || ((num_elems != 0) && (credits_consumed != num_elems));
      active       <= active0 && !send_data1;
      flushing     <= flushing0 || do_flush || do_flush2;
      pending_recv <= (pending_recv || underflow) && !send_data0;

      if (do_flush || do_flush2)
         flush_done.send();

   endrule

   // ---------------
   // manage credit messages from the NOC
   RWire#(Tuple3#(UInt#(16),Bool,Bool)) creditMsg <- mkRWire (clocked_by nocClock, reset_by nocReset);
   UCount         creditTimer <- mkUCount(params.creditTimer, params.creditTimer, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(16)) nocCredits <- mkReg(0, clocked_by nocClock, reset_by nocReset);

   Reg#(Bool)      nocUnderFlow <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)      nocAutoFlush <- mkReg(False, clocked_by nocClock, reset_by nocReset);

   rule tickCreditTimer ( !creditTimer.isEqual (0) );
      creditTimer.decr (1);
   endrule

   (* preempts = "nocHandleCreditMsg, nocEnqCreditMsg" *)
   (* fire_when_enabled, no_implicit_conditions *)
   rule nocHandleCreditMsg (creditMsg.wget matches tagged Valid {.cnt, .under, .autof} ) ;
      nocCredits <= nocCredits + cnt;
      if (under) nocUnderFlow <= True;
      if (autof) nocAutoFlush <= True;

      creditTimer <= params.creditTimer;
   endrule
   rule nocEnqCreditMsg (nocUnderFlow  ||
                         ( creditTimer.isEqual(0) && (nocCredits != 0)));
      credit_fifo.enq( tuple3( nocCredits, nocUnderFlow, nocAutoFlush) );
      nocCredits    <= 0;
      nocUnderFlow  <= False;
      nocAutoFlush  <= False;
   endrule
   // ---------------

   // The reset sequence for the outpipe restores the pipe to
   // its initial state

   (* fire_when_enabled *) // in nocClock domain
   rule drain_data_info_fifo if (in_reset_noc);
      data_info_fifo.deq();
   endrule

   (* fire_when_enabled *) // in nocClock domain
   rule drain_out_fifo if (in_reset_noc);
      out_fifo.deq();
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // in nocClock domain
   rule reset_noc_side if (in_reset_noc);
      noc_buf_bytes <= 0;
   endrule

   (* fire_when_enabled *) // in uClock domain
   rule drain_credit_fifo if (in_reset_uclk);
      credit_fifo.deq();
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // in uClock domain
   rule reset_uclock_side if (in_reset_uclk && !credit_fifo.notEmpty());
      elem_count        <= 0;
      credits           <= fromInteger (params.capacity);
      overflow          <= False;
      flushing          <= False;
      active            <= True;
      pending_recv      <= False;
      autoflush         <= False;
      flush_requested   <= False;
      in_reset_uclk     <= False;
      indata_mimo.clear;
   endrule

   // ---------------

   // Store one side in the state
   PipeInfo#(OutPipeIfc) info;
   info.pipeNum = state.outPipeCount;
   info.pipeIfc = (interface OutPipeIfc;
                      method Action credit_msg(UInt#(16) credits, Bool underflow, Bool autoflush);
                         creditMsg.wset(tuple3(credits,underflow,autoflush));
                      endmethod
                      method ActionValue#(Tuple4#(UInt#(32),Bool,Bool,Bool)) start_data_msg() if (!in_reset_noc);
                         data_info_fifo.deq();
                         return data_info_fifo.first();
                      endmethod
                      method ActionValue#(Tuple2#(LUInt#(bpb),MsgBeat#(bpb))) take_data_beat() if (!in_reset_noc);
                         if (noc_buf_bytes == 0)
                            return tuple2(0,?);
                         else begin
                            LUInt#(bpb) n = truncate(min(fromInteger(ibpb),noc_buf_bytes));
                            MsgBeat#(bpb) beat = zExtend(pack(noc_buf));
                            data_beat_taken.send();
                            return tuple2(n,beat);
                         end
                      endmethod
                   endinterface);
   state.outPipeCount = state.outPipeCount + 1;
   state.output_pipes = List::cons(info,state.output_pipes);

   // Update the context with the augmented state
   putContext(state);

   // record parameters for infrastructure linkage tool
   Ignored param_pipeNum    <- mkSceMiUInt32Parameter(fromInteger(info.pipeNum));
   Ignored param_depth      <- mkSceMiUInt32Parameter(fromInteger(params.capacity));
   Ignored param_visibility <- mkSceMiStringParameter(  (params.vis == Deferred)  ? "Deferred"
                                                      : (params.vis == Immediate) ? "Immediate"
                                                      :                      "Fifo"
                                                     );

   // ---------------
   // Return the SceMi output pipe interface

   method UInt#(32) can_send() = (  in_reset_uclk
                                 || (flushing || flush_requested)
                                 || ((params.vis == Deferred) && !active)
                                 || indata_mimo_not_ready
                                 || !data_info_fifo.notFull()
                                 )
                                 ? 0
                                 : zeroExtend(credits)
                                 ;

   method ActionValue#(UInt#(32))
             try_send(UInt#(32)      num_elems,
                      Vector#(m,e)   data,
                      Bool           eom);
      if (  in_reset_uclk
         || (num_elems == 0)
         || (flushing || flush_requested)
         || indata_mimo_not_ready
         || !data_info_fifo.notFull()
         ) begin
         return 0;
      end
      else begin
         // calculate accepted elements and whether to send to consumer
         UInt#(16) num_accepted = 0;
         Bool      send_data    = False;
         Bool      send_fail    = False;
         Bool      do_flush     = False;

         if (params.vis == Deferred) begin
            num_accepted = (!active) ? 0 : min(credits,truncate(num_elems));
            send_data    =  ((elem_count + num_accepted) == fromInteger(params.capacity))
                         && (  (num_accepted != truncate(num_elems))
                            || pending_recv
                            )
                         ;
            send_fail    = (num_accepted != truncate(num_elems)) && !overflow;
         end
         else begin
            num_accepted = min(credits,truncate(num_elems));
            send_data    = num_accepted != 0;
         end

         // add the accepted data to the output buffer
         add_to_output_buffer <= tuple2(data, num_accepted);
         Bool real_eom = eom && (num_accepted == truncate(num_elems));
         if (real_eom && autoflush)
            do_flush = True;

         updates_from_ifc <= tuple6(truncate(num_elems),num_accepted,send_data,send_fail,do_flush, real_eom );

         return zeroExtend(num_accepted);
      end
   endmethod

   method Action flush();
      flush_request.send();
   endmethod

endmodule: mkSceMiNoCOutputPipeP


// The core used by all of the native SCE-MI implementations
module build_NoC_pipe_connections#(  SceMiModuleState state
				   , Clock            nocClock
				   , Reset            nocReset
				   , Bool             noc_is_active
				   )(MsgPortFilter#(bpb))
    provisos( Add#(0, BPB, bpb)
	    , Max#(16, bpb, w)        // allow 16 or more bytes to be enqueued/dequeued in one shot with any MIMO
	    , Mul#(bpb, 8, bitspb)    // calculate the total number of bits in a beat
	    , Mul#(bitspb, 2, bitspb2) // calculate the total number of bits in two beats
    	    , Max#(4, TMul#(w,2), sz) // adjust size to be twice as deep as enqueue/dequeue width or at least 8 entries
    	    , Add#(2, _1, sz)         // mimo buffers must be at least 2 deep
    	    , Add#(_2, w, sz)         // mimo buffers must be >= the width of the enqueue/dequeue interface
    	    // BSC should be able to deduce this
    	    , Add#(_3, TMul#(8, w), TMul#(8, sz))
    	    );

   Integer bytes_per_beat = valueOf(bpb);

   check_bytes_per_beat("build_NoC_pipe_connections", bytes_per_beat);

   ////////////////////////////////////////////////////////////////////////////////
   /// Pipes / Ports
   ////////////////////////////////////////////////////////////////////////////////
   List#(InPortInfo)               input_ports          = List::sortBy(cmp_inport_num,  state.inports);
   List#(OutPortInfo)              output_ports         = List::sortBy(cmp_outport_num, state.outports);

   List#(InPipeIfc)                input_pipes          = List::map(get_pipe_ifc, List::sortBy(cmp_pipe_num, state.input_pipes));
   List#(OutPipeIfc)               output_pipes         = List::map(get_pipe_ifc, List::sortBy(cmp_pipe_num, state.output_pipes));

   ////////////////////////////////////////////////////////////////////////////////
   /// Functions
   ////////////////////////////////////////////////////////////////////////////////
   // Function to help extract Outport Acks from the SceMi1 packets.  There are a potential
   // for two acks per 32-bits of scemi data.
   function Tuple2#(Integer, Vector#(n, UInt#(10))) getOutportAcks(Vector#(m, SceMi1OutPortAck) x)
      provisos( Add#(m, m, n) );
      Integer count = 0;
      Vector#(n, UInt#(10)) v = newVector;

      for(Integer i = 0; i < valueOf(m); i = i + 1) begin
	 if (isValid(x[i].port2)) begin
	    v[count] = validValue(x[i].port2);
	    count = count + 1;
	 end
	 if (isValid(x[i].port1)) begin
	    v[count] = validValue(x[i].port1);
	    count = count + 1;
	 end
      end

      return tuple2(count, v);
   endfunction

   ////////////////////////////////////////////////////////////////////////////////
   /// MsgSink
   ////////////////////////////////////////////////////////////////////////////////
   // Signals and storage connected directly to the MsgSink interface
   PulseWire                       pwFromBridgeReady   <- mkPulseWire(clocked_by nocClock, reset_by nocReset);
   Wire#(MsgBeat#(bpb))            wFromBridgeBeat     <- mkBypassWire(clocked_by nocClock, reset_by nocReset);
   MIMO#(w,w,sz,Bit#(8))           fFromBridgeBeat     <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);

   // If the data is not consumed by this module and passed along to ports/pipes, then it continues
   // on to the other noc bus.  These signals manage that always ready interface/storage.
   MIMO#(w,w,sz,Bit#(8))           fToContinueBeat     <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   PulseWire                       pwToContinueReady   <- mkPulseWire(clocked_by nocClock, reset_by nocReset);
   Wire#(MsgBeat#(bpb))            wToContinueBeat     <- mkDWire(?, clocked_by nocClock, reset_by nocReset);

   // State to manage the parsing/decoding of incoming noc packets.
   Reg#(Bool)                      rSceMi1MsgIn        <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rSceMi2MsgIn        <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rOtherMsgIn         <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(8))                  rInMsgBytes         <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rDecodeSceMi        <- mkReg(False, clocked_by nocClock, reset_by nocReset);

   // State used while delivering messages to SceMi Ports/Pipes.
   Reg#(Bool)                      rS1MsgInIsData      <- mkReg (False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rS1MsgInIsAck       <- mkReg (False, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(10))                 rS1InPortNum        <- mkRegU(clocked_by nocClock, reset_by nocReset);
   MIMO#(14, 1, 16, UInt#(10))     fS1OutPortAcks      <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);

   Reg#(UInt#(12))                 rS2InPipeNum        <- mkRegU(clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rS2MsgInIsData      <- mkReg (False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rS2MsgInIsCred      <- mkReg (False, clocked_by nocClock, reset_by nocReset);

   // Rules to execute while noc is active
   rule msg_sink_noc_active if (noc_is_active);
      // Since the MsgSink interface is always ready, we have the following rule to enqueue the incoming
      // beats into a fifo where we can examine them at will and provide proper back-pressure without
      // implicit conditions on the MsgSink interface
      (* fire_when_enabled *)
      rule receive_beat_from_bridge if (fFromBridgeBeat.enqReadyN(fromInteger(bytes_per_beat)) && pwFromBridgeReady);
	 fFromBridgeBeat.enq(fromInteger(bytes_per_beat), cExtend(wFromBridgeBeat));
      endrule

      // wait for the length/opcode fields to arrive.  Once the BlueNoC header is present, analyze the
      // header information and determine if this is a scemi packet (and what type of scemi packet it is).
      // otherwise, pass the beats along until the packet is retired.

      // This implementation is optimized for bpb sizes up-to and including 4-bpb.  The entire noc
      // header is collected and analyzed.  Once that is complete, then it is dequeued from the MIMO
      // and deeper inspection is allowed.
      (* fire_when_enabled *)
      rule decode_noc_header_4bpb if (fFromBridgeBeat.deqReadyN(4) && (rInMsgBytes == 0) && (bytes_per_beat <= 4));
	 NoCHeader header = cExtend(fFromBridgeBeat.first());
	 fFromBridgeBeat.deq(4);
	 case(header.opcode) matches
	    'b101010: // SceMi 1.1
	    begin
	       rSceMi1MsgIn <= True;
	       rSceMi2MsgIn <= False;
	       rOtherMsgIn  <= False;
	       rInMsgBytes  <= header.length;
	       rDecodeSceMi <= True;
	    end
	    'b101011: // SceMi 2.1
	    begin
	       rSceMi1MsgIn <= False;
	       rSceMi2MsgIn <= True;
	       rOtherMsgIn  <= False;
	       rInMsgBytes  <= header.length;
	       rDecodeSceMi <= True;
	    end
	    default: // Not SceMi 1.1 nor SceMi 2.1
	    begin
	       rSceMi1MsgIn <= False;
	       rSceMi2MsgIn <= False;
	       rOtherMsgIn  <= True;
	       rInMsgBytes  <= header.length;
	       fToContinueBeat.enq(4, cExtend(header));
	    end
	 endcase
      endrule

      // This implementation is optimized for 8-bpb or larger.  The entire noc header and scemi
      // header is completely analyzed.  If the data is SceMi1 based, either inport data is inbound
      // or outport acks.  If it is SceMi2 data, then it is either inpipe data or outpipe credits.
      (* fire_when_enabled *)
      rule decode_noc_header_8bpb if (fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat)) && (rInMsgBytes == 0) && (bytes_per_beat > 4));
	 NoCHeader nocheader = cExtend(fFromBridgeBeat.first());
	 SceMi1Header scemi1header = cExtend(shiftOutFrom0(0, fFromBridgeBeat.first(), 4));
	 SceMi2Msg scemi2header = cExtend(shiftOutFrom0(0, fFromBridgeBeat.first(), 4));

	 fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	 UInt#(8) length = (nocheader.length < (fromInteger(bytes_per_beat - 4))) ? 0 : nocheader.length - fromInteger(bytes_per_beat - 4);

	 case(nocheader.opcode) matches
	    'b101010: // SceMi 1.1
	    begin
	       rSceMi1MsgIn <= (length > 0);
	       rSceMi2MsgIn <= False;
	       rOtherMsgIn  <= False;
	       rInMsgBytes  <= length;
	       case(scemi1header.msg_type) matches
		  INPORT_DATA: // SceMi 1.1 MessageInPort Data
		  begin
		     SceMi1InPortData x = cExtend(scemi1header);
		     rS1MsgInIsData    <= (length > 0);
		     rS1MsgInIsAck     <= False;
		     rS1InPortNum      <= x.port;
		  end
		  OUTPORT_ACK: // SceMi 1.1 MessageOutPort Ack
		  begin
		     Vector#(1, SceMi1OutPortAck) x = cExtend(scemi1header);
		     rS1MsgInIsAck     <= (length > 0);
		     rS1MsgInIsData    <= False;
		     match { .count, .v } = getOutportAcks(x);
		     fS1OutPortAcks.enq(fromInteger(count), cExtend(v));
		  end
	       endcase
	    end
	    'b101011: // SceMi 2.1
	    begin
	       rSceMi1MsgIn <= False;
	       rSceMi2MsgIn <= (length > 0);
	       rOtherMsgIn  <= False;
	       rInMsgBytes  <= length;

	       if (scemi2header.is_data) begin // SceMi 2.1 InPipe Data
		  rS2InPipeNum   <= scemi2header.pipe;
		  rS2MsgInIsData <= (length > 0);
		  rS2MsgInIsCred <= False;
		  input_pipes[scemi2header.pipe].data_msg(scemi2header.count, scemi2header.more, scemi2header.flush, scemi2header.eom);
	       end
	       else begin // SceMi 2.1 OutPipe Credits
		  rS2MsgInIsData <= False;
		  rS2MsgInIsCred <= False; // credit messages are always 4 bytes long
		  output_pipes[scemi2header.pipe].credit_msg(scemi2header.count, scemi2header.more, scemi2header.flush);
	       end
	    end
	    default: // Not SceMi 1.1 nor SceMi 2.1
	    begin
	       rSceMi1MsgIn <= False;
	       rSceMi2MsgIn <= False;
	       rOtherMsgIn  <= (length > 0);
	       rInMsgBytes  <= length;
	       fToContinueBeat.enq(fromInteger(bytes_per_beat), cExtend(fFromBridgeBeat.first()));
	    end
	 endcase
      endrule

      // These rules involve packets decoded as SceMi 1.1 packets and are longer than the BPB allows to be
      // analyzed in a single cycle.
      rule scemi1 if (rSceMi1MsgIn && !rSceMi2MsgIn && !rOtherMsgIn && (rInMsgBytes > 0));
	 // If we were not able to analyze the SceMi 1.1 packet header in the previous cycle, we analyze it now
	 // requiring the entire header is present.  This rule will only fire when we have 4-bpb or less for the
	 // noc interface.
	 (* fire_when_enabled *)
	 rule decode_scemi_header if (fFromBridgeBeat.deqReadyN(4) && rDecodeSceMi && !rS1MsgInIsAck && !rS1MsgInIsData && fS1OutPortAcks.enqReadyN(2) && (bytes_per_beat <= 4));
	    SceMi1Header header = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(4);

	    case(header.msg_type) matches
	       INPORT_DATA: // SceMi 1.1 MessageInPort Data
	       begin
		  SceMi1InPortData x = cExtend(header);
		  rS1MsgInIsData    <= (rInMsgBytes > 4);
		  rS1MsgInIsAck     <= False;
		  rS1InPortNum      <= x.port;
	       end
	       OUTPORT_ACK: // SceMi 1.1 MessageOutPort Ack
	       begin
		  Vector#(1, SceMi1OutPortAck) x = cExtend(header);
		  rS1MsgInIsAck     <= (rInMsgBytes > 4);
		  rS1MsgInIsData    <= False;
		  match { .count, .v } = getOutportAcks(x);
		  fS1OutPortAcks.enq(fromInteger(count), cExtend(v));
	       end
	    endcase
	    // Done decoding.  Continue processing if there are more bytes in this message/packet
	    rDecodeSceMi <= False;
	    rSceMi1MsgIn <= (rInMsgBytes > 4);
	    rInMsgBytes  <= (rInMsgBytes > 4) ? (rInMsgBytes - 4) : 0;
	 endrule

	 // Continue processing input port data forwarding it along to the proper input port.
	 (* fire_when_enabled *)
	 rule process_inport_data if (  fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat))
				      && rS1MsgInIsData
				      && !rS1MsgInIsAck
				      && input_ports[rS1InPortNum].beatIfc.awaitingData()
				      && (state.inPortCount != 0)
				      );
	    Vector#(bpb, Bit#(8)) data = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    input_ports[rS1InPortNum].beatIfc.putBeat(cExtend(data));
	    rSceMi1MsgIn   <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rS1MsgInIsData <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes    <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	 endrule

	 // Continue processing output port acks placing them in a FIFO for subsequent processing.
	 // This rule is optimized for less than 4-bpb.  We wait until we have an entire outport ack
	 // message in the buffer, then process the two potential acks contained within.
	 (* fire_when_enabled *)
	 rule process_outport_ack_lt4bpb if (fFromBridgeBeat.deqReadyN(4)
					    && !rS1MsgInIsData
					    && rS1MsgInIsAck
					    && fS1OutPortAcks.enqReadyN(2)
					    && (bytes_per_beat < 4)
					    );
	    Vector#(1, SceMi1OutPortAck) x = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(4);
	    match { .count, .v } = getOutportAcks(x);
	    rSceMi1MsgIn   <= (rInMsgBytes > 4);
	    rS1MsgInIsAck  <= (rInMsgBytes > 4);
	    rInMsgBytes    <= (rInMsgBytes > 4) ? (rInMsgBytes - 4) : 0;
	    fS1OutPortAcks.enq(fromInteger(count), cExtend(v));
	 endrule

	 // Continue processing output port acks placing them in a FIFO for subsequent processing.
	 // This rule is optimized for 4-bpb or larger.  The entire beat is processed for the acks
	 // contained within.  Since 4-bpb or larger means there are exact multiples of the ack
	 // message on the bus, we can extract a vector of the acks to enqueue.
	 (* fire_when_enabled *)
	 rule process_outport_ack_4bpb if (fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat))
					  && !rS1MsgInIsData
					  && rS1MsgInIsAck
					  && fS1OutPortAcks.enqReadyN(fromInteger(bytes_per_beat/2))
					  && (bytes_per_beat >= 4)
					  );
	    Vector#(TDiv#(bpb,4), SceMi1OutPortAck) x = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    match { .count, .v } = getOutportAcks(x);
	    rSceMi1MsgIn   <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rS1MsgInIsAck  <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes    <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	    fS1OutPortAcks.enq(fromInteger(count), cExtend(v));
	 endrule

	 // If for some reason we decoded a SceMi 1.1 packet, but the format was corrupted or the
	 // packet itself was malformed, we simply disregard the packet and continue on.  This rule
	 // will flush the packet from the system and ready it to receive the next packet.
	 (* fire_when_enabled *)
	 rule disregard_packet if (fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat)) && !rS1MsgInIsAck && !rS1MsgInIsData && !rDecodeSceMi);
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    rSceMi1MsgIn   <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes    <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	 endrule
      endrule: scemi1

      // Process the enqueued outport ack messages.  The buffer simply contains the outport number to
      // ack.  We process these one at a time in case there are multiple for the same outport queued up
      // as the method to perform the ack conflicts with itself.  This rule fires in the background and
      // if not too many ack messages flood in at once, the system on the whole won't trigger flow control
      // on the input side to process these.  Ideally, we would handle as many as can be contained in a beat,
      // but we only handle one at a time for now.
      (* fire_when_enabled *)
      rule execute_outport_acks if (fS1OutPortAcks.deqReady());
	 UInt#(10) port = cExtend(fS1OutPortAcks.first());
	 fS1OutPortAcks.deq(1);
	 output_ports[port].beatIfc.ack();
      endrule

      // These rules involve packets decoded as SceMi 2.1 packets and are larger than the BPB allows to be
      // analyzed in a single clock cycle.
      rule scemi2 if (rSceMi2MsgIn && !rSceMi1MsgIn && !rOtherMsgIn && (rInMsgBytes > 0));
	 // If we were not able to analyze the SceMi 2.1 packet header in the previous cycle, we analyze it now
	 // requiring the entire packet header is present.  This rule will only fire when we have 4-bpb or less
	 // for the noc interface.
	 (* fire_when_enabled *)
	 rule decode_scemi_header_4bpb if (fFromBridgeBeat.deqReadyN(4) && rDecodeSceMi && !rS2MsgInIsData && !rS2MsgInIsCred && (bytes_per_beat <= 4));
	    SceMi2Msg header = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(4);

	    if (header.is_data) begin // SceMi 2.1 InPipe Data
	       rS2InPipeNum   <= header.pipe;
	       rS2MsgInIsData <= (rInMsgBytes > 4);
	       rS2MsgInIsCred <= False;
	       input_pipes[header.pipe].data_msg(header.count, header.more, header.flush, header.eom);
	    end
	    else begin // SceMi 2.1 OutPipe Credits
	       rS2MsgInIsData <= False;
	       rS2MsgInIsCred <= False; // credit messages are always 4 bytes long (this may change?)
	       output_pipes[header.pipe].credit_msg(header.count, header.more, header.flush);
	    end
	    // Done decoding.  Continue processing the rest of the packet if there are more bytes.
	    rDecodeSceMi <= False;
	    rSceMi2MsgIn   <= (rInMsgBytes > 4);
	    rInMsgBytes    <= (rInMsgBytes > 4) ? (rInMsgBytes - 4) : 0;
	 endrule

	 // If for some reason we decoded a SceMi 2.1 packet, but the format was corrupted or the
	 // packet itself was malformed, we simply disregard the packet and continue on.  This rule
	 // will flush the packet from the system and ready it to receive the next packet.
	 (* fire_when_enabled *)
	 rule disregard_packet if (fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat)) && !rS2MsgInIsData && !rS2MsgInIsCred && !rDecodeSceMi);
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    rSceMi2MsgIn   <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes    <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	 endrule
      endrule: scemi2

      // This rule allows non-SceMi traffic from the Bridge to be forwarded along on the out NoC
      // interface for downstream consumption.  This enables SceMi to play "nice" with other NoC
      // targets in the system.  If the downstream NoC interface is not used, it should be tied off
      // as to not cause back-pressure on the upstream NoC interface.
      rule other if (rOtherMsgIn && !rSceMi1MsgIn && !rSceMi2MsgIn && (rInMsgBytes > 0));
	 (* fire_when_enabled *)
	 rule process_other_data if (fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat)) && fToContinueBeat.enqReadyN(fromInteger(bytes_per_beat)));
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    fToContinueBeat.enq(fromInteger(bytes_per_beat), cExtend(fFromBridgeBeat.first()));
	    rOtherMsgIn <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	 endrule
      endrule: other
   endrule: msg_sink_noc_active

   // Continue processing inpipe data forwarding it along to the proper input pipe.
   // This belongs above inside the scemi2 set of rules, but is not allowed by the
   // compiler.
   if (state.inPipeCount != 0) begin
      for(Integer i = 0; i < state.inPipeCount; i = i + 1) begin
	 (* fire_when_enabled *)
	 rule msg_sink_noc_active_scemi2_process_inpipe_data if (  noc_is_active
								&& rSceMi2MsgIn
								&& !rSceMi1MsgIn
								&& !rOtherMsgIn
								&& (rInMsgBytes > 0)
								&& rS2MsgInIsData
								&& fFromBridgeBeat.deqReadyN(fromInteger(bytes_per_beat))
								&& rS2InPipeNum == fromInteger(i)
								);
	    MsgBeat#(bpb) b = cExtend(fFromBridgeBeat.first());
	    fFromBridgeBeat.deq(fromInteger(bytes_per_beat));
	    LUInt#(bpb) byte_count = (rInMsgBytes > fromInteger(bytes_per_beat)) ? fromInteger(bytes_per_beat) : cExtend(rInMsgBytes);
	    input_pipes[i].add_data(byte_count, b);
	    rSceMi2MsgIn   <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rS2MsgInIsData <= (rInMsgBytes > fromInteger(bytes_per_beat));
	    rInMsgBytes    <= (rInMsgBytes > fromInteger(bytes_per_beat)) ? (rInMsgBytes - fromInteger(bytes_per_beat)) : 0;
	 endrule
      end
   end // if (state.inPipeCount != 0) begin

   // We need to reset inbound state if the NoC interface goes inactive.  Only need
   // to reset state that is normally reset during a hard-reset event.
   (* fire_when_enabled, no_implicit_conditions *)
   rule msg_sink_noc_inactive if (!noc_is_active);
      fFromBridgeBeat.clear();
      fToContinueBeat.clear();
      rSceMi1MsgIn     <= False;
      rSceMi2MsgIn     <= False;
      rOtherMsgIn      <= False;
      rInMsgBytes      <= 0;
      rDecodeSceMi     <= False;
      fS1OutPortAcks.clear();
   endrule


   ////////////////////////////////////////////////////////////////////////////////
   /// MsgSource
   ////////////////////////////////////////////////////////////////////////////////
   // Signals and storage connected directly to the MsgSource interface
   PulseWire                       pwToBridgeReady     <- mkPulseWire(clocked_by nocClock, reset_by nocReset);
   Wire#(MsgBeat#(bpb))            wToBridgeBeat       <- mkDWire(?, clocked_by nocClock, reset_by nocReset);
   FIFOF#(MsgBeat#(bpb))           fToBridgeBeat       <- mkLFIFOF(clocked_by nocClock, reset_by nocReset);

   // If data is incoming from other NoC targets thorugh this module, arbitrate
   // and forward that data along to the bridge.
   PulseWire                       pwFromContinueReady <- mkPulseWire(clocked_by nocClock, reset_by nocReset);
   Wire#(MsgBeat#(bpb))            wFromContinueBeat   <- mkBypassWire(clocked_by nocClock, reset_by nocReset);
   MIMO#(w,w,sz,Bit#(8))           fFromContinueBeat   <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);

   // State to manage the forming/transmission of noc packets.
   Reg#(Bool)                      rSceMi1MsgOut       <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rSceMi2MsgOut       <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rOtherMsgOut        <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(UInt#(8))                  rOutMsgBytes        <- mkReg(0, clocked_by nocClock, reset_by nocReset);
   RWire#(UInt#(2))                rwNextOutMsgGrant   <- mkRWire(clocked_by nocClock, reset_by nocReset); // 0 = other, 1 = scemi1, 2 = scemi2, 3 = reserved.
   Reg#(Vector#(3, Bool))          rvPrevMsgGrant      <- mkReg(replicate(False), clocked_by nocClock, reset_by nocReset);
   Reg#(Vector#(3, Bool))          rvPrevPrevMsgGrant  <- mkReg(replicate(False), clocked_by nocClock, reset_by nocReset);

   // SceMi 1.1 Message Out choices
   MIMO#(w,w,sz,Bit#(8))           fS1MsgOut           <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   // InPort Request Request
   Reg#(Bool)                      rS1MsgOutReqReq     <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   // OutPort Send Data Request
   Reg#(Bool)                      rS1MsgOutDataReq    <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   // InPort Request Granted = 1, OutPort Send Data Request Granted = 0
   Reg#(Bool)                      rS1MsgOutReqGrant   <- mkReg((state.inPortCount != 0), clocked_by nocClock, reset_by nocReset);

   // SceMi 2.1 Message Out choices
   MIMO#(w,w,sz,Bit#(8))           fS2MsgOut           <- mkMIMO(defaultValue, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rS2MsgOutDataReq    <- mkReg(False, clocked_by nocClock, reset_by nocReset);
   Reg#(Bool)                      rS2MsgOutCredGrant  <- mkReg((state.inPipeCount != 0), clocked_by nocClock, reset_by nocReset);
   PulseWire                       pwS2OutDataGranted  <- mkPulseWireOR(clocked_by nocClock, reset_by nocReset);

   function UInt#(2) arbitrate_next_out_msg_grant(Vector#(3, Bool) requests);
      Vector#(3, Bool) grant = replicate(False);
      Vector#(3, Bool) prev  = rvPrevMsgGrant;
      Vector#(3, Bool) prev2 = rvPrevPrevMsgGrant;
      case(countElem(True, requests))
	 0: begin
	       grant = replicate(False);
	    end
	 1: begin
	       grant = requests;
	    end
	 2: begin
	       case({ pack(requests), pack(prev) })
		  { 3'b011, 3'b000 }: grant = unpack(3'b001); //
		  { 3'b011, 3'b001 }: grant = unpack(3'b010);
		  { 3'b011, 3'b010 }: grant = unpack(3'b001);
		  { 3'b011, 3'b100 }: grant = unpack(3'b001); //
		  { 3'b110, 3'b000 }: grant = unpack(3'b010); //
		  { 3'b110, 3'b001 }: grant = unpack(3'b010); //
		  { 3'b110, 3'b010 }: grant = unpack(3'b100);
		  { 3'b110, 3'b100 }: grant = unpack(3'b010);
		  { 3'b101, 3'b000 }: grant = unpack(3'b100); //
		  { 3'b101, 3'b001 }: grant = unpack(3'b100);
		  { 3'b101, 3'b010 }: grant = unpack(3'b100); //
		  { 3'b101, 3'b100 }: grant = unpack(3'b001);
	       endcase
	    end
	 3: begin
	       case({ pack(prev), pack(prev2) })
		  { 3'b001, 3'b000 }: grant = unpack(3'b010);
		  { 3'b001, 3'b001 }: grant = unpack(3'b010);
		  { 3'b001, 3'b010 }: grant = unpack(3'b100);
		  { 3'b001, 3'b100 }: grant = unpack(3'b010);
		  { 3'b010, 3'b000 }: grant = unpack(3'b100);
		  { 3'b010, 3'b001 }: grant = unpack(3'b100);
		  { 3'b010, 3'b010 }: grant = unpack(3'b100);
		  { 3'b010, 3'b100 }: grant = unpack(3'b001);
		  { 3'b100, 3'b000 }: grant = unpack(3'b001);
		  { 3'b100, 3'b001 }: grant = unpack(3'b010);
		  { 3'b100, 3'b010 }: grant = unpack(3'b001);
		  { 3'b100, 3'b100 }: grant = unpack(3'b001);
		  default:            grant = unpack(3'b001);
	       endcase
	    end
      endcase

      case(True)
	 grant[0]: return 0;
	 grant[1]: return 1;
	 grant[2]: return 2;
	 default:  return 3;
      endcase
   endfunction


   // Rules to execute while noc is active
   rule msg_source_noc_active if (noc_is_active);
      // Since the MsgSource interface is always ready, we have the following rules to manage the driving of
      // the data and subsequent dequeue when that data has been accepted by the bridge.  This allows backpressure
      // without implicit conditions.
      (* fire_when_enabled *)
      rule transmit_beat_to_bridge;
	 wToBridgeBeat <= fToBridgeBeat.first();
      endrule

      // When both sides are ready to accept the data, the beat was taken.  Move to the next beat.
      (* fire_when_enabled *)
      rule move_to_next_output_beat if (pwToBridgeReady);
	 fToBridgeBeat.deq();
      endrule

      // round robin arbitrate with which queue is next to transmit.  If there are queues ready to transmit,
      // there are no bubbles to help decide -- arbitration is handled in a single cycle.  Since there are only
      // three sources, this isn't too difficult.
      (* fire_when_enabled *)
      rule arbitrate_transmit_messages if (rOutMsgBytes == 0);
	 Vector#(3, Bool) req = newVector;
	 UInt#(8) bytes_ready = min(4, fromInteger(bytes_per_beat));
	 req[0] = fFromContinueBeat.deqReadyN(cExtend(bytes_ready));
	 req[1] = fS1MsgOut.deqReadyN(cExtend(bytes_ready));
	 req[2] = fS2MsgOut.deqReadyN(cExtend(bytes_ready));
	 UInt#(2) grant = arbitrate_next_out_msg_grant(req);
	 rwNextOutMsgGrant.wset(grant);
	 rvPrevPrevMsgGrant <= rvPrevMsgGrant;
	 case(grant)
	    0: rvPrevMsgGrant <= unpack(3'b001);
	    1: rvPrevMsgGrant <= unpack(3'b010);
	    2: rvPrevMsgGrant <= unpack(3'b100);
	    3: rvPrevMsgGrant <= unpack(3'b000);
	 endcase
      endrule

      // An external packet source was granted the next packet transmission.  Compute the packet length
      // and start forwarding the packet onto the NoC bus.
      (* fire_when_enabled *)
      rule dispatch_next_granted_other if (rwNextOutMsgGrant.wget matches tagged Valid .granted
					  &&& (rOutMsgBytes == 0)
					  &&& (granted == 0)
					  &&& fFromContinueBeat.deqReadyN(fromInteger(bytes_per_beat))
					  );
	 NoCHeader header = cExtend(fFromContinueBeat.first());
	 fFromContinueBeat.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fFromContinueBeat.first()));
	 UInt#(9) length = ((cExtend(header.length)+4) <= fromInteger(bytes_per_beat)) ? 0 : cExtend(header.length) - fromInteger(bytes_per_beat) + 4;
	 rOutMsgBytes  <= cExtend(length);
	 rOtherMsgOut  <= (length > 0);
	 rSceMi1MsgOut <= False;
	 rSceMi2MsgOut <= False;
      endrule

      // The SceMi 1.1 path was granted the next packet transmission.  Compute the packet length
      // and start forwarding the packet onto the NoC bus.
      (* fire_when_enabled *)
      rule dispatch_next_granted_scemi1 if (rwNextOutMsgGrant.wget matches tagged Valid .granted
					   &&& (rOutMsgBytes == 0)
					   &&& (granted == 1)
					   &&& fS1MsgOut.deqReadyN(fromInteger(bytes_per_beat))
					   );
	 NoCHeader header = cExtend(fS1MsgOut.first());
	 fS1MsgOut.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fS1MsgOut.first()));
	 UInt#(9) length = ((cExtend(header.length)+4) <= fromInteger(bytes_per_beat)) ? 0 : cExtend(header.length) - fromInteger(bytes_per_beat) + 4;
	 rOutMsgBytes  <= cExtend(length);
	 rOtherMsgOut  <= False;
	 rSceMi1MsgOut <= (length > 0);
	 rSceMi2MsgOut <= False;
      endrule

      // The SceMi 2.1 path was granted the next packet transmission.  Compute the packet length
      // and start forwarding the packet onto the NoC bus.
      (* fire_when_enabled *)
      rule dispatch_next_granted_scemi2 if (rwNextOutMsgGrant.wget matches tagged Valid .granted
					   &&& (rOutMsgBytes == 0)
					   &&& (granted == 2)
					   &&& fS2MsgOut.deqReadyN(fromInteger(bytes_per_beat))
					   );
	 NoCHeader header = cExtend(fS2MsgOut.first());
	 fS2MsgOut.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fS2MsgOut.first()));
	 UInt#(9) length = ((cExtend(header.length)+4) <= fromInteger(bytes_per_beat)) ? 0 : cExtend(header.length) - fromInteger(bytes_per_beat) + 4;
	 rOutMsgBytes  <= cExtend(length);
	 rOtherMsgOut  <= False;
	 rSceMi1MsgOut <= False;
	 rSceMi2MsgOut <= (length > 0);
      endrule

      // Continue transmitting the external packet until the packet is completed.
      (* fire_when_enabled *)
      rule continue_other if (rOtherMsgOut && !rSceMi1MsgOut && !rSceMi2MsgOut && (rOutMsgBytes > 0) && fFromContinueBeat.deqReadyN(fromInteger(bytes_per_beat)));
	 fFromContinueBeat.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fFromContinueBeat.first()));
	 rOutMsgBytes  <= (rOutMsgBytes <= fromInteger(bytes_per_beat)) ? 0 : rOutMsgBytes - fromInteger(bytes_per_beat);
	 rOtherMsgOut  <= (rOutMsgBytes > fromInteger(bytes_per_beat));
      endrule

      // Continue transmitting the SceMi 1.1 packet until the packet is completed.
      (* fire_when_enabled *)
      rule continue_scemi1 if (rSceMi1MsgOut && !rSceMi2MsgOut && !rOtherMsgOut && (rOutMsgBytes > 0) && fS1MsgOut.deqReadyN(fromInteger(bytes_per_beat)));
	 fS1MsgOut.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fS1MsgOut.first()));
	 rOutMsgBytes  <= (rOutMsgBytes <= fromInteger(bytes_per_beat)) ? 0 : rOutMsgBytes - fromInteger(bytes_per_beat);
	 rSceMi1MsgOut <= (rOutMsgBytes > fromInteger(bytes_per_beat));
      endrule

      // Continue transmitting the SceMi 2.1 packet until the packet is completed.
      (* fire_when_enabled *)
      rule continue_scemi2 if (rSceMi2MsgOut && !rSceMi1MsgOut && !rOtherMsgOut && (rOutMsgBytes > 0) && fS2MsgOut.deqReadyN(fromInteger(bytes_per_beat)));
	 fS2MsgOut.deq(fromInteger(bytes_per_beat));
	 fToBridgeBeat.enq(cExtend(fS2MsgOut.first()));
	 rOutMsgBytes  <= (rOutMsgBytes <= fromInteger(bytes_per_beat)) ? 0 : rOutMsgBytes - fromInteger(bytes_per_beat);
	 rSceMi2MsgOut <= (rOutMsgBytes > fromInteger(bytes_per_beat));
      endrule
   endrule: msg_source_noc_active

   // We need to reset the outbound state if the NoC interface goes inactive.  Only need
   // to reset state that is normally reset during a hard-reset event.
   (* fire_when_enabled, no_implicit_conditions *)
   rule msg_source_noc_inactive if (!noc_is_active);
      fToBridgeBeat.clear();
      fFromContinueBeat.clear();
      rSceMi1MsgOut      <= False;
      rSceMi2MsgOut      <= False;
      rOtherMsgOut       <= False;
      rOutMsgBytes       <= 0;
      rvPrevMsgGrant     <= replicate(False);
      rvPrevPrevMsgGrant <= replicate(False);
      fS1MsgOut.clear();
      rS1MsgOutReqReq    <= False;
      rS1MsgOutDataReq   <= False;
      rS1MsgOutReqGrant  <= (state.inPortCount != 0);
      fS2MsgOut.clear();
      rS2MsgOutDataReq   <= False;
      rS2MsgOutCredGrant <= (state.inPipeCount != 0);
   endrule

   // Rules to execute while noc is active and we have at least 1 input port.
   if (state.inPortCount != 0) begin
      List#(Reg#(Bool))            lrS1PendingRequests <- List::replicateM(state.inPortCount, mkReg(False, clocked_by nocClock, reset_by nocReset));
      List#(Reg#(Bool))            lrS1ActiveRequests  <- List::replicateM(state.inPortCount, mkReg(False, clocked_by nocClock, reset_by nocReset));
      Reg#(UInt#(10))              rS1RequestedPort    <- mkReg(0, clocked_by nocClock, reset_by nocReset);
      Bool                         bS1LoadNewGroup      = List::map(readReg, lrS1ActiveRequests) == List::replicate(state.inPortCount, False);

      rule msg_source_noc_active_inports if (noc_is_active);
	 // If we have no active requests to issue and we aren't due to send any soon, then update
	 // the set of requests thereby allowing more to arbitrate for transmission.
	 (* fire_when_enabled, no_implicit_conditions *)
	 rule load_new_request_group if (bS1LoadNewGroup && !rS1MsgOutReqReq);
	    for(Integer n = 0; n < state.inPortCount; n = n + 1) begin
	       lrS1PendingRequests[n] <= input_ports[n].beatIfc.sendRequest();
	       lrS1ActiveRequests[n]  <= lrS1PendingRequests[n];
	    end
	    rS1RequestedPort <= 0;
	 endrule

	 // If we aren't currently sending a request message and we have active requests to send, yet
	 // the next one on the list isn't requesting, shift to the next input port (skipping this one)
	 (* fire_when_enabled, no_implicit_conditions *)
	 rule shift_new_request_vector if (!bS1LoadNewGroup && !rS1MsgOutReqReq && !lrS1ActiveRequests[0]);
	    if (state.inPortCount > 1) begin
	       for(Integer n = 1; n < state.inPortCount; n = n + 1) begin
		  lrS1ActiveRequests[n-1] <= lrS1ActiveRequests[n];
	       end
	    end
	    lrS1ActiveRequests[state.inPortCount-1] <= False;
	    rS1RequestedPort <= rS1RequestedPort + 1;
	 endrule

	 // While we are not loading new groups to the active request set or we are transmitting the
	 // requests over the NoC interface, we should refresh the pending request list in the background
	 (* fire_when_enabled, no_implicit_conditions *)
	 rule accumulate_pending_requests if (!bS1LoadNewGroup || rS1MsgOutReqReq);
	    for(Integer n = 0; n < state.inPortCount; n = n + 1) begin
	       lrS1PendingRequests[n] <= lrS1PendingRequests[n] || input_ports[n].beatIfc.sendRequest();
	    end
	 endrule

	 // If we have a request to send out, indicate the desire to send the request by issuing a
	 // request "request".  When granted, the request message will transmit on the NoC bus.
	 (* fire_when_enabled, no_implicit_conditions *)
	 rule start_next_request if (!bS1LoadNewGroup && !rS1MsgOutReqReq && lrS1ActiveRequests[0]);
	    rS1MsgOutReqReq       <= True;
	    lrS1ActiveRequests[0] <= False;
	 endrule

	 // Once our request request has been granted, transmit the inport request message on the NoC
	 // interface.  We can assemble the 8-byte message entirely in one rule and push it into the
	 // MIMO module for transmission at the NoC rate.
	 (* fire_when_enabled *)
	 rule send_request_message if (rS1MsgOutReqReq && rS1MsgOutReqGrant && fS1MsgOut.enqReadyN(8));
	    Vector#(8, Bit#(8)) v = newVector;
	    let noc_header = NoCHeader {
		  opcode:     6'b101010, // SceMi 1.1
		  unused:     0,
		  dont_wait:  False,
		  length:     4,
		  src:        1,
		  dst:        0
	       };
	    let scemi_header = SceMi1InPortReq {
	       msg_type:    INPORT_REQ,
	       last:        False,
	       unused:      0,
	       port:        rS1RequestedPort
	       };
	    v = unpack({ pack(scemi_header), pack(noc_header) });
	    fS1MsgOut.enq(8, cExtend(v));
	    rS1MsgOutReqReq <= False;
	    if (state.outPortCount != 0)
	       rS1MsgOutReqGrant <= False;
	 endrule
      endrule: msg_source_noc_active_inports

      // We need to reset the outbound state if the NoC interface goes inactive.  Only need
      // to reset state that is normally reset during a hard-reset event.
      (* fire_when_enabled, no_implicit_conditions *)
      rule msg_source_noc_inactive_inports if (!noc_is_active);
	 for(Integer n = 0; n < state.inPortCount; n = n + 1) begin
	    lrS1PendingRequests[n] <= False;
	    lrS1ActiveRequests[n]  <= False;
	 end
	 rS1RequestedPort <= 0;
      endrule
   end // if (state.inPortCount != 0)

   // Rules to execute when there are output ports in the design.
   if (state.outPortCount != 0) begin
      Reg#(UInt#(10))              rS1OutPort          <- mkRegU(clocked_by nocClock, reset_by nocReset);
      Reg#(SceMiCycleStamp)        rS1CycleStamp       <- mkRegU(clocked_by nocClock, reset_by nocReset);
      Reg#(UInt#(19))              rS1BitsRem          <- mkRegU(clocked_by nocClock, reset_by nocReset);
      Reg#(UInt#(8))               rS1OutMsgSize       <- mkRegU(clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS1OutMsgIsCont     <- mkRegU(clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS1OutDataHeader    <- mkReg (False, clocked_by nocClock, reset_by nocReset);

      // continually scan the output ports to determine if any have information to transmit
      // to the host.  The state variable here is used in the hasOutputMsg function.  The scan
      // can continuously operate even while an output port message is transmitted since it
      // has no side effects.
      (* fire_when_enabled, no_implicit_conditions *)
      rule scan_output_ports;
	 state.currentOutPort <= (state.currentOutPort == fromInteger(state.outPortCount-1)) ? 0 : state.currentOutPort + 1;
      endrule

      // Rules to execute while the noc is active.
      rule msg_source_noc_active_outports if (noc_is_active);
	 // the scan output port has detected a port that needs to transmit a data message.
	 // We query the port for information and make it the next outport data message sent.
	 // Subsequently, we need to be granted access to the transmit bus.  We do not attempt
	 // to start data messages if there is another SceMi 1.1 output port transmitting.
	 (* fire_when_enabled *)
	 rule start_data_message if (hasOutputMsg(state) && !rS1MsgOutDataReq && !rS1OutDataHeader);
	    UInt#(19) bitwidth = fromInteger(output_ports[nextMsgChannel(state)].bitWidth);
	    rS1OutPort         <= nextMsgChannel(state);
	    rS1CycleStamp      <= nextMsgCycle(state);
	    rS1MsgOutDataReq   <= True;
	    rS1BitsRem         <= bitwidth;
	    rS1OutMsgSize      <= (bitwidth >= (240*8)) ? 252 : (12 + truncate((bitwidth + 7) / 8));
	    rS1OutMsgIsCont    <= False;
	    rS1OutDataHeader   <= True;
	    advanceOutputMsg(state);
	 endrule

	 // Queue the output port data message header when the SceMi 1.1 manager decides
	 // that an output port data message is next to transmit.
	 (* fire_when_enabled *)
	 rule send_data_message_header if (rS1MsgOutDataReq && !rS1MsgOutReqGrant && rS1OutDataHeader && fS1MsgOut.enqReadyN(16));
	    Vector#(16, Bit#(8)) v = newVector;
	    rS1OutDataHeader <= False;
	    let noc_header = NoCHeader {
	       opcode:     6'b101010,
	       unused:     0,
	       dont_wait:  False,
	       length:     rS1OutMsgSize,
	       src:        1,
	       dst:        0
	       };
	    let scemi_header = SceMi1OutPortData {
	       msg_type:   OUTPORT_DATA,
	       last:       (rS1BitsRem + (rS1OutMsgIsCont ? 32 : 96)) <= (252*8),
	       bitsrem:    rS1BitsRem,
	       port:       rS1OutPort
	       };
	    if (!rS1OutMsgIsCont) begin
	       v = unpack({ pack(rS1CycleStamp), pack(scemi_header), pack(noc_header) });
	       rS1OutMsgSize <= rS1OutMsgSize - 12;
	       fS1MsgOut.enq(16, cExtend(v));
	    end
	    else begin
	       v = cExtend({ pack(scemi_header), pack(noc_header) });
	       rS1OutMsgSize <= rS1OutMsgSize - 4;
	       fS1MsgOut.enq(8, cExtend(v));
	    end
	 endrule

	 // Queue up the output port message data once the header has been queued.
	 // Subsequent arbitration will empty the SceMi 1.1 queue to transmit this
	 // packet.
	 (* fire_when_enabled *)
	 rule send_data_message_data if (rS1MsgOutDataReq && !rS1MsgOutReqGrant && !rS1OutDataHeader && fS1MsgOut.enqReadyN(fromInteger(bytes_per_beat)));
	    let x <- output_ports[rS1OutPort].beatIfc.takeBeat();
	    fS1MsgOut.enq(fromInteger(bytes_per_beat), cExtend(x));
	    if (rS1OutMsgSize <= fromInteger(bytes_per_beat)) begin
	       if (rS1BitsRem <= fromInteger(bytes_per_beat*8)) begin
		  rS1MsgOutDataReq <= False;
		  if (state.inPortCount != 0)
		     rS1MsgOutReqGrant <= True;
	       end
	       else begin
		  rS1OutMsgSize    <= (rS1BitsRem >= (252*8)) ? 252 : (4 + truncate((rS1BitsRem - 25) / 8));
		  rS1BitsRem       <= rS1BitsRem - fromInteger(bytes_per_beat*8);
		  rS1OutMsgIsCont  <= True;
		  rS1OutDataHeader <= True;
	       end
	    end
	    else begin
	       rS1BitsRem    <= rS1BitsRem - fromInteger(bytes_per_beat*8);
	       rS1OutMsgSize <= rS1OutMsgSize - fromInteger(bytes_per_beat);
	    end
	 endrule
      endrule: msg_source_noc_active_outports
   end // if (state.outPortCount != 0)

   if ((state.inPortCount != 0) && (state.outPortCount != 0)) begin
      // This rule manages the SceMi 1.1 grant signal arbitrating between sending inport requests and
      // outport data.  If one of them is requesting and the other is not requesting and the grant is
      // currently not choosing the requester, the grant switches to the requesting path so it can
      // transmit.
      (* no_implicit_conditions *)
      (* descending_urgency = "msg_source_noc_active_outports_start_data_message, swap_scemi1_outport_grant" *)
      (* descending_urgency = "msg_source_noc_active_inports_start_next_request, swap_scemi1_outport_grant" *)
      rule swap_scemi1_outport_grant if ((rS1MsgOutReqReq && !rS1MsgOutReqGrant && !rS1MsgOutDataReq) ||
					 (rS1MsgOutDataReq && rS1MsgOutReqGrant && !rS1MsgOutReqReq));
	 rS1MsgOutReqGrant <= !rS1MsgOutReqGrant;
      endrule
   end

   // Portion of the design that exists if we have any input pipes.
   if (state.inPipeCount != 0) begin
      Reg#(UInt#(12))              rS2InCreditIndex    <- mkReg(0, clocked_by nocClock, reset_by nocReset);

      // Rule to arbitrate among all the input pipes and decide which is next
      // to transmit.  This can be improved with a round robin style interface that
      // chooses in a single cycle instead of every n cycles if only one port needs
      // to transmit a lot and the others don't.
      Rules scan =
      rules
      	 rule scan_inpipe_credit_index if (fS2MsgOut.enqReadyN(8));
      	    if (rS2InCreditIndex == fromInteger(state.inPipeCount - 1))
      	       rS2InCreditIndex <= 0;
      	    else
      	       rS2InCreditIndex <= rS2InCreditIndex + 1;
      	 endrule
      endrules;

      // Rules to manage the credit messages outbound for input pipes.
      // The credit message are small enough they can be enqueued into
      // the SceMi 2.1 outbound MIMO in one rule.
      Rules take_credit = emptyRules;
      for(Integer i = 0; i < state.inPipeCount; i = i + 1) begin
      	 Rules r =
	 rules
      	    rule start_inpipe_credit_request if (   (rS2InCreditIndex == fromInteger(i))
      						 && rS2MsgOutCredGrant
						 && !pwS2OutDataGranted
						 && fS2MsgOut.enqReadyN(8)
						 && noc_is_active
						 );
	       match { .amount, .underflow } <- input_pipes[i].take_credit_msg();
	       Vector#(8, Bit#(8)) v = newVector;
	       let noc_header = NoCHeader {
		  opcode:     6'b101011,
		  unused:     0,
		  dont_wait:  False,
		  length:     4,
		  src:        1,
		  dst:        0
		  };
	       let scemi_header = SceMi2Msg {
		  is_data:    False,
		  more:       underflow,
		  flush:      False,
		  eom:        False,
		  pipe:       rS2InCreditIndex,
		  count:      amount
		  };
	       v = unpack({ pack(scemi_header), pack(noc_header) });
	       fS2MsgOut.enq(8, cExtend(v));
	    endrule
      	 endrules;
	 take_credit = rJoinMutuallyExclusive(take_credit, r);
      end
      addRules(rJoinDescendingUrgency(take_credit, scan));

      // We need to reset the outbound state if the NoC interface goes inactive.  Only need
      // to reset state that is normally reset during a hard-reset event.
      (* fire_when_enabled, no_implicit_conditions *)
      rule reset_inpipe_when_noc_is_inactive if (!noc_is_active);
	 rS2InCreditIndex   <= 0;
      endrule
   end // if (state.inPipeCount != 0)

   // Portion of the design that exists if we have any output pipes.
   if (state.outPipeCount != 0) begin
      Reg#(UInt#(12))              rS2OutDataIndex     <- mkReg(0, clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS2SendOutDataMsg   <- mkReg(False, clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS2SendOutDataHdr   <- mkReg(False, clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS2OutDataOverflow  <- mkReg(False, clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS2OutDataFlush     <- mkReg(False, clocked_by nocClock, reset_by nocReset);
      Reg#(Bool)                   rS2OutDataEOM       <- mkReg(False, clocked_by nocClock, reset_by nocReset);

      Reg#(UInt#(32))              rS2OutBytes         <- mkReg(0, clocked_by nocClock, reset_by nocReset);
      Reg#(UInt#(8))               rS2OutMsgBytes      <- mkReg(0, clocked_by nocClock, reset_by nocReset);

      Reg#(MsgBeat#(bpb))          rS2SavedBytes       <- mkReg(0, clocked_by nocClock, reset_by nocReset);
      Reg#(LUInt#(bpb))            rS2NumSaved         <- mkReg(0, clocked_by nocClock, reset_by nocReset);

      // Rule to arbitrate among all the output pipes and decide which is next
      // to transmit.  This can be improved with a round robin style interface that
      // chooses in a single cycle instead of every n cycles if only one port needs
      // to transmit a lot and the others don't.
      Rules scan =
      rules
	 rule scan_outpipe_data_index if (!rS2SendOutDataMsg && fS2MsgOut.enqReadyN(8));
	    if (rS2OutDataIndex == fromInteger(state.outPipeCount - 1))
	       rS2OutDataIndex <= 0;
	    else
	       rS2OutDataIndex <= rS2OutDataIndex + 1;
	 endrule
      endrules;

      // These rules will enqueue the NoC and SceMi 2.1 header for the next arbitrated
      // output pipe data message.  If the data is larger than zero bytes, the send
      // rule is executed the cycle after this one.
      Rules start_msg = emptyRules;
      for(Integer i = 0; i < state.outPipeCount; i = i + 1) begin
	 Rules r =
	 rules
	    (* fire_when_enabled *)
	    rule start_outpipe_data_message if (  !rS2SendOutDataMsg
					       && (rS2OutDataIndex == fromInteger(i))
					       );
	       match { .bytes, .overflow, .flush, .eom } <- output_pipes[i].start_data_msg();
	       rS2SendOutDataMsg   <= True;
	       rS2SendOutDataHdr   <= True;
	       rS2MsgOutCredGrant  <= False;
	       rS2OutBytes         <= bytes;
	       rS2OutDataOverflow  <= overflow;
	       rS2OutDataFlush     <= flush;
               rS2OutDataEOM       <= eom;
	       pwS2OutDataGranted.send();
	    endrule
	 endrules;
	 start_msg = rJoinMutuallyExclusive(start_msg, r);
      end
      addRules(rJoinDescendingUrgency(start_msg, scan));

      // This rule sends the actual header bytes and prepares to send another sequence
      // of output data.  If the output data is larger than 248 bytes, this rule will
      // fire again and begin another packet.
      (* fire_when_enabled *)
      rule send_outpipe_data_header if (  rS2SendOutDataMsg
				       && rS2SendOutDataHdr
				       && !rS2MsgOutCredGrant
				       && fS2MsgOut.enqReadyN(8)
				       );
	 UInt#(8) len = cExtend(min(rS2OutBytes, 248));
         Bool lastNoCPacket = extend(len) == rS2OutBytes;
	 Vector#(8, Bit#(8)) v = newVector;
	 let noc_header = NoCHeader {
	    opcode:     6'b101011,
	    unused:     0,
	    dont_wait:  False,
	    length:     (len + 4),
	    src:        1,
	    dst:        0
	    };
	 let scemi_header = SceMi2Msg {
	    is_data:    True,
	    more:       lastNoCPacket ? rS2OutDataOverflow : False,
	    flush:      lastNoCPacket ? rS2OutDataFlush : False,
	    eom:        lastNoCPacket ? rS2OutDataEOM : False,
	    pipe:       rS2OutDataIndex,
	    count:      0
	    };
	 v = unpack({ pack(scemi_header), pack(noc_header) });
	 fS2MsgOut.enq(8, cExtend(v));

         // Messages can be 0 length
         if (len == 0) begin
	    // this was the last data in the final message
	    rS2SendOutDataMsg  <= False;
	    rS2MsgOutCredGrant <= True;
	    if (rS2OutDataIndex == fromInteger(state.outPipeCount - 1))
	       rS2OutDataIndex <= 0;
	    else
	       rS2OutDataIndex <= rS2OutDataIndex + 1;
         end
         else begin
	    rS2OutMsgBytes     <= len;
	    rS2OutBytes        <= rS2OutBytes - cExtend(len);
	    rS2SendOutDataHdr  <= False;
         end

      endrule

      // We have started an outpipe data message by sending the NoC header and the
      // SceMi 2.1 header.  Now, we transmit the data.  Get the data from the outpipe
      // and send it along until we are done.  Be sure to pad any extra bytes to
      // the width of the NoC bus if necessary.
      Rules send_data = emptyRules;
      for(Integer i = 0; i < state.outPipeCount; i = i + 1) begin
	 Rules r =
	 rules
	    (* fire_when_enabled *)
	    rule send_outpipe_data_message if (  rS2SendOutDataMsg
					      && !rS2SendOutDataHdr
					      && (rS2OutDataIndex == fromInteger(i))
					      && !rS2MsgOutCredGrant
					      && fS2MsgOut.enqReadyN(fromInteger(bytes_per_beat))
					      );

	       LUInt#(bpb)   n = 0;
	       MsgBeat#(bpb) v = ?;
	       LUInt#(bpb) bytes_needed    = cExtend(min(rS2OutMsgBytes, fromInteger(bytes_per_beat)));
	       if ((rS2NumSaved < bytes_needed) || (cExtend(bytes_needed) != rS2OutMsgBytes)) begin
		  match { .n0, .v0 } <- output_pipes[i].take_data_beat();
		  n = n0;  v = v0;
	       end
	       Bit#(bitspb2) bs = cExtend(rS2SavedBytes) | (cExtend(v) << (8 * cExtend(rS2NumSaved)));
	       LUInt#(bpb) bytes_available = cExtend(n) + cExtend(rS2NumSaved);
	       Bool send_beat = bytes_available >= bytes_needed;
	       if (send_beat) begin
		  rS2SavedBytes  <= cExtend(bs >> (8 * cExtend(bytes_needed)));
		  rS2NumSaved    <= cExtend(bytes_available - bytes_needed);
		  rS2OutMsgBytes <= rS2OutMsgBytes - cExtend(bytes_needed);
		  MsgBeat#(bpb) x = cExtend((bs & ~('1 << (8 * cExtend(bytes_needed)))));
		  fS2MsgOut.enq(fromInteger(bytes_per_beat), cExtend(x));
		  if (cExtend(bytes_needed) == rS2OutMsgBytes) begin
		     // this message is complete
		     if (rS2OutBytes != 0) begin
			// more data to send, so start a new header
			rS2SendOutDataHdr <= True;
		     end
		     else begin
			// this was the last data in the final message
			rS2SendOutDataMsg  <= False;
			rS2MsgOutCredGrant <= True;
			if (rS2OutDataIndex == fromInteger(state.outPipeCount - 1))
			   rS2OutDataIndex <= 0;
			else
			   rS2OutDataIndex <= rS2OutDataIndex + 1;
		     end
		  end
	       end
	       else if (n > 0) begin
		  // we are not sending the bytes, just accumulating them.
		  rS2SavedBytes <= cExtend(bs);
		  rS2NumSaved   <= cExtend(bytes_available);
	       end
	    endrule
	 endrules;
	 send_data = rJoinMutuallyExclusive(send_data, r);
      end
      addRules(send_data);

      // We need to reset the outbound state if the NoC interface goes inactive.  Only need
      // to reset state that is normally reset during a hard-reset event.
      (* fire_when_enabled, no_implicit_conditions *)
      rule reset_outpipe_when_noc_is_inactive if (!noc_is_active);
	 rS2OutDataIndex   <= 0;
	 rS2SendOutDataMsg <= False;
	 rS2SendOutDataHdr <= False;
	 rS2OutBytes       <= 0;
	 rS2OutMsgBytes    <= 0;
	 rS2SavedBytes     <= 0;
	 rS2NumSaved       <= 0;
      endrule
   end // if (state.outPipeCount != 0)


   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface MsgPort noc_src;
      interface MsgSink in;
   	 method Bool dst_rdy();
   	    return fFromBridgeBeat.enqReadyN(fromInteger(bytes_per_beat));
   	 endmethod

   	 method Action src_rdy(Bool b);
   	    if (b) pwFromBridgeReady.send();
   	 endmethod

   	 method Action beat(MsgBeat#(bpb) v);
   	    wFromBridgeBeat <= v;
   	 endmethod
      endinterface

      interface MsgSource out;
   	 method Bool src_rdy();
            return fToBridgeBeat.notEmpty();
   	 endmethod

   	 method Action dst_rdy(Bool b);
   	    if (b) pwToBridgeReady.send();
   	 endmethod

   	 method MsgBeat#(bpb) beat = wToBridgeBeat;
      endinterface
   endinterface

   interface MsgPort noc_cont;
      interface MsgSink in;
	 method Bool dst_rdy();
	    return fFromContinueBeat.enqReadyN(fromInteger(bytes_per_beat));
	 endmethod

	 method Action src_rdy(Bool b);
	    if (b) pwFromContinueReady.send();
	 endmethod

	 method Action beat(MsgBeat#(bpb) v);
	    wFromContinueBeat <= v;
	 endmethod
      endinterface

      interface MsgSource out;
	 method Bool src_rdy();
	    return fToContinueBeat.deqReadyN(fromInteger(bytes_per_beat));
	 endmethod

	 method Action dst_rdy(Bool b);
	    if (b) pwToContinueReady.send();
	 endmethod

	 method MsgBeat#(bpb) beat = wToContinueBeat;
      endinterface
   endinterface

endmodule: build_NoC_pipe_connections

// arbitration function
//
function Maybe#(Integer)
arbitrate_round_robin (NodeID        priority_ptr,
                       List#(Bool)   request_vector,
                       Integer       vec_len);

   Maybe#(Integer)   maybe_grant_idx  = tagged Invalid;
   Bool              found            = True;

   // Walk the list twice, to make sure that
   for (Integer x = 0; x < (2 * vec_len); x = x + 1) begin
      Integer   y = (x % vec_len);

      // Ignore everything before the priority index
      if (fromInteger(y) == priority_ptr)
         found = False;

      // After the index point,
      if (!found && request_vector[y]) begin
         maybe_grant_idx = tagged Valid y;
         found           = True;
      end

   end
   return maybe_grant_idx;
endfunction

endpackage: SceMiNoC
