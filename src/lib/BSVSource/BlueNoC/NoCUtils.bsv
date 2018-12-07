// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package NoCUtils;

// This package contains utility modules for working with the NoC
// fabric.

import Clocks      :: *;
import FIFO        :: *;
import FIFOF       :: *;
import Connectable :: *;
import Counter     :: *;
import Gray        :: *;
import DReg        :: *;

import MsgFormat   :: *;

// ==================================================================
//
// Detecting and synchronizing with other FPGAs
//
// This is a little utility for exchanging training sequences with a
// neighboring FPGA so that the two can begin communicating reliably.
//
// ==================================================================

interface TrainingSignals;
   (* always_ready *)
   method Bit#(1) sequence_out();
   (* always_ready, always_enabled *)
   method Action sequence_in(Bit#(1) b);
endinterface

interface TrainingStatus;
   (* always_ready *)
   method Bool partner_detected();
   (* always_ready *)
   method Bool partner_is_listening();
   (* always_ready *)
   method Action retrain();
endinterface

interface Training;
   interface TrainingSignals pins;
   interface TrainingStatus  status;
endinterface

instance Connectable#(TrainingSignals,TrainingSignals);
   module mkConnection#(TrainingSignals t1, TrainingSignals t2)();
      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_training_1_to_2;
         t2.sequence_in(t1.sequence_out());
      endrule
      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_training_2_to_1;
         t1.sequence_in(t2.sequence_out());
      endrule
   endmodule
endinstance

(* synthesize *)
module mkTrainer#(Clock external_clk, Reset external_rst)(Training);

   Clock local_clk <- exposeCurrentClock();
   Reset local_rst <- exposeCurrentReset();

   // The training sequence consists of seven fixed bits, followed by
   // one status bit

   Bit#(7) training_header = 7'b0111010;

   Reg#(Bit#(8))  pattern_in     <- mkReg('0, clocked_by external_clk, reset_by external_rst);
   Reg#(Bit#(8))  pattern_out    <- mkReg({training_header,1'b0}, clocked_by local_clk, reset_by local_rst);
   Reg#(UInt#(3)) status_bit_pos <- mkReg(0, clocked_by local_clk, reset_by local_rst);

   SyncPulseIfc  pattern_match <- mkSyncHandshake(external_clk, external_rst, local_clk);
   SyncPulseIfc  payload_true  <- mkSyncHandshake(external_clk, external_rst, local_clk);

   Reg#(Bool) matched_header   <- mkReg(False, clocked_by local_clk, reset_by local_rst);
   Reg#(Bool) link_established <- mkReg(False, clocked_by local_clk, reset_by local_rst);

   Reg#(UInt#(8)) cycles_without_a_match <- mkReg(0, clocked_by local_clk, reset_by local_rst);

   Reg#(Bool) force_retrain <- mkDReg(False, clocked_by local_clk, reset_by local_rst);

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule generate_training_sequence if (!force_retrain);
      if (status_bit_pos == 0)
         pattern_out <= {pack(matched_header),pattern_out[7:1]};
      else
         pattern_out <= {pattern_out[0],pattern_out[7:1]};
      status_bit_pos <= status_bit_pos + 1;
   endrule

   (* fire_when_enabled *) // on external clock
   rule check_training_sequence;
      if (pattern_in[7:1] == training_header) begin
         pattern_match.send();
         if (pattern_in[0] == 1)
            payload_true.send();
      end
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule update_partner_status if (!force_retrain);
      if (pattern_match.pulse()) begin
         matched_header <= True;
         cycles_without_a_match <= 0;
         if (payload_true.pulse())
            link_established <= True;
      end
      else if (link_established && (cycles_without_a_match > 7)) begin
         force_retrain <= True;
      end
      else if (cycles_without_a_match == 255) begin
         force_retrain <= True;
      end
      else
         cycles_without_a_match <= cycles_without_a_match + 1;
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule restart_training if (force_retrain);
      pattern_out            <= {training_header,1'b0};
      status_bit_pos         <= 0;
      matched_header         <= False;
      link_established       <= False;
      cycles_without_a_match <= 0;
   endrule

   // We use wires with noReset to eliminate warnings about the reset
   // associated with these values not being available at the FPGA
   // boundary.

   Wire#(Bit#(1)) bit_in  <- mkBypassWire(clocked_by external_clk, reset_by noReset);
   Wire#(Bit#(1)) bit_out <- mkBypassWire(clocked_by local_clk, reset_by noReset);

   (* fire_when_enabled, no_implicit_conditions *) // on external clock
   rule xfer_bit_in;
      pattern_in <= {bit_in,pattern_in[7:1]};
   endrule

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule xfer_bit_out;
      bit_out <= pattern_out[0];
   endrule

   // signals to exchange training sequences with partner
   interface TrainingSignals pins;

      method Bit#(1) sequence_out = bit_out;

      method Action sequence_in(Bit#(1) b);
         bit_in <= b;
      endmethod

   endinterface: pins

   interface TrainingStatus status;

      // methods to expose current training status

      method Bool partner_detected     = matched_header;

      method Bool partner_is_listening = link_established;

      // method to initiate re-training

      method Action retrain();
         force_retrain <= True;
      endmethod

   endinterface: status

endmodule

// ==================================================================
//
// Moving NoC beats between FPGAs
//
// The FPGAMsgPort is a credit-based FPGA-to-FPGA interconnect
// that supports the bidirectional flow of NoC messages between
// FPGAs.
//
// ==================================================================

// This controls how deep the FIFOs are when crossing between FPGAs
// and how many credits are available to compensate for the clock
// crossing latency.
typedef 4 CreditSz;

typedef UInt#(CreditSz) CreditCount; // type used to exchange credits
Integer fifo_depth = 2**valueOf(CreditSz);

// This is a source of MsgBeats at an FPGA boundary
interface FPGASource#(numeric type bpb);
   (* always_ready, always_enabled *)
   method Action credit_return(Gray#(2) count);
   (* always_ready *)
   method Maybe#(MsgBeat#(bpb)) beat();
endinterface

// This is a sink of MsgBeats at an FPGA boundary
interface FPGASink#(numeric type bpb);
   (* always_ready *)
   method Gray#(2) credit_count();
   (* always_ready, always_enabled *)
   method Action beat(Maybe#(MsgBeat#(bpb)) b);
endinterface

// The sources and sinks at FPGA boundaries are connectable. In hardware
// the connection would be done with traces on the board, but in simulation
// with multiple FPGAs these Connectable instances are useful.

instance Connectable#(FPGASource#(bpb), FPGASink#(bpb));
   module mkConnection#(FPGASource#(bpb) src, FPGASink#(bpb) snk)();
      (* fire_when_enabled, no_implicit_conditions *)
      rule xmit_credits;
         src.credit_return(snk.credit_count());
      endrule
      (* fire_when_enabled, no_implicit_conditions *)
      rule xmit_beats;
         snk.beat(src.beat);
      endrule
   endmodule
endinstance

instance Connectable#(FPGASink#(bpb),FPGASource#(bpb));
   module mkConnection#(FPGASink#(bpb) snk, FPGASource#(bpb) src)();
      (*hide*)
      let _m <- mkConnection(src,snk);
      return _m;
   endmodule
endinstance

// A source and sink along with the clock (for source-synchronous
// signaling) and a training sequence together make up a NoC crossing
// at the FPGA boundary.

interface FPGACrossing#(numeric type bpb);
   interface Clock            clk;
   interface TrainingSignals  training;
   interface FPGASource#(bpb) source;
   interface FPGASink#(bpb)   sink;
endinterface

// An FPGAMsgPort has an FPGACrossing interface to route to the FPGA
// boundary and a regular MsgPort interface for connecting to the NoC
// inside the FPGA.

interface FPGAMsgPort#(numeric type bpb);
   interface MsgPort#(bpb)      noc;
   interface FPGACrossing#(bpb) pins;
   interface TrainingStatus     status;
endinterface

// This is the top-level module for creating a NoC message port at the
// FPGA boundary. It takes an input clock which is the clock from the
// partner FPGA associated with the incoming signals. Its default
// clock is the local clock used with the NoC on this FPGA, which will
// be exported to the partner FPGA as part of the source-synchronous
// signaling methodology.

module mkFPGAMsgPort#(Clock external_clk)(FPGAMsgPort#(bpb));

   // give unambiguous names to all the clocks and resets

   Clock local_clk <- exposeCurrentClock();
   Reset local_rst <- exposeCurrentReset();

   Reset external_rst <- mkAsyncReset(2,local_rst,external_clk);

   // FIFOs on the NoC side
   FifoMsgSource#(bpb) to_noc   <- mkFifoMsgSource(clocked_by local_clk, reset_by local_rst);
   FifoMsgSink#(bpb)   from_noc <- mkFifoMsgSink(clocked_by local_clk, reset_by local_rst);

   // Training sequence
   Training trainer <- mkTrainer(external_clk,external_rst, clocked_by local_clk, reset_by local_rst);

   Bool link_is_up =  trainer.status.partner_detected()
                   && trainer.status.partner_is_listening()
                   ;

   // The FPGA interface signals are registered immediately on both
   // input and output.  These registers have no resets because we
   // pass the clock between FPGAs but not the associated reset.

   CrossingReg#(Gray#(2))      incoming_credits <- mkNullCrossingReg( local_clk, 0
                                                                    , clocked_by external_clk, reset_by external_rst
                                                                     );
   Reg#(Maybe#(MsgBeat#(bpb))) incoming_beat    <- mkRegU(clocked_by external_clk);
   Reg#(Gray#(2))              outgoing_credits <- mkReg(0, clocked_by local_clk, reset_by local_rst);
   Reg#(Maybe#(MsgBeat#(bpb))) outgoing_beat    <- mkDReg(tagged Invalid, clocked_by local_clk, reset_by local_rst);

   // Credits are returned from the sink each time a beat is taken by
   // the downstream NoC. To keep the credit return latency as low as
   // possible the partner FPGA increments a Gray-encoded 2-bit value
   // on each credit. That Gray-encoded value is passed to the local
   // FPGA and registered in the local clock domain. Because the
   // external and local clock frequencies are closely matched, the
   // local FPGA should never see the incoming credit counter increment
   // by more than 2 from one cycle to the next. The local FPGA tracks
   // the increase in the incoming credit counter and adds the increment
   // to its local credit accumulator.

   Reg#(Gray#(2))     local_credits       <- mkReg(0, clocked_by local_clk, reset_by local_rst);
   Reg#(UInt#(2))     prev_credit_counter <- mkReg(0, clocked_by local_clk, reset_by local_rst);
   Counter#(CreditSz) can_send_up_to      <- mkCounter( '1
                                                      , clocked_by local_clk, reset_by local_rst
                                                      );

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule accumulate_incoming_credits if (link_is_up);
      UInt#(2) new_credit_counter = unpack(grayDecode(local_credits));
      if (new_credit_counter != prev_credit_counter) begin
         if (new_credit_counter == prev_credit_counter + 1)
            can_send_up_to.inc(1);
         else
            can_send_up_to.inc(2);
      end
      local_credits       <= incoming_credits.crossed();
      prev_credit_counter <= new_credit_counter;
   endrule

   // We forward beats from the NoC to our partner FPGA
   // if we have the credits available to do so.

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule prepare_next_beat if (  link_is_up
                             && (can_send_up_to.value() != 0)
                             && !from_noc.empty()
                             );
      MsgBeat#(bpb) beat = from_noc.first(); // unguarded first and deq!
      from_noc.deq();
      outgoing_beat <= tagged Valid beat;
      can_send_up_to.down();
   endrule

   // Message beats arrive from our partner FPGA on the external clock
   // and pass through a sync FIFO so that they can be injected onto
   // the NoC using the local clock.
   SyncFIFOIfc#(MsgBeat#(bpb)) msg_sync <- mkSyncFIFO(fifo_depth, external_clk, external_rst, local_clk);

   // Each time a beat is injected onto the NoC, it increments the credit
   // Gray code counter by one.
   PulseWire      credit_granted      <- mkPulseWire(clocked_by local_clk, reset_by local_rst);
   Reg#(Gray#(2)) credit_gray_counter <- mkReg(0, clocked_by local_clk, reset_by local_rst);

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule send_next_credits if (link_is_up);
      Gray#(2) new_counter_value = credit_granted ? grayIncr(credit_gray_counter) : credit_gray_counter;
      outgoing_credits    <= new_counter_value;
      credit_gray_counter <= new_counter_value;
   endrule

   // Forward data from the partner FPGA out to the NoC

   (* fire_when_enabled *) // on external clock
   rule take_incoming_beat if (incoming_beat matches tagged Valid .x);
      msg_sync.enq(x);
   endrule

   (* fire_when_enabled *) // on internal clock
   rule send_out_to_noc;
      MsgBeat#(bpb) beat = msg_sync.first();
      msg_sync.deq();
      to_noc.enq(beat);
      credit_granted.send();
   endrule

   // When the link is retraining, force everything back to the
   // initial state

   (* fire_when_enabled, no_implicit_conditions *) // on internal clock
   rule reset_until_trained if (!link_is_up);
      credit_gray_counter <= 0;
      prev_credit_counter <= 0;
      local_credits       <= 0;
      outgoing_credits    <= 0;
      can_send_up_to.setF('1);
      to_noc.clear();
      from_noc.clear();
   endrule

   //
   // interface definitions
   //

   interface MsgPort noc = as_port(get_source_ifc(to_noc),get_sink_ifc(from_noc));

   // value methods are on the local_clk and
   // action methods are on the external_clk

   interface FPGACrossing pins;

      interface Clock clk = local_clk;

      interface TrainingSignals training = trainer.pins;

      interface FPGASource source;

         method Action credit_return(Gray#(2) count);
            incoming_credits <= count;
         endmethod

         method Maybe#(MsgBeat#(bpb)) beat();
            return outgoing_beat;
         endmethod

      endinterface: source

      interface FPGASink sink;

         method Gray#(2) credit_count();
            return outgoing_credits;
         endmethod

         method Action beat(Maybe#(MsgBeat#(bpb)) b);
            incoming_beat <= b;
         endmethod

      endinterface: sink

   endinterface: pins

   interface TrainingStatus status = trainer.status;

endmodule

endpackage: NoCUtils
