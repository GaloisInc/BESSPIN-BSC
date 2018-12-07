// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiTCP;

// This is an implementation of SceMi in software using a TCP connection
// as the communication link.

import Clocks::*;
import Vector::*;
import ModuleContext::*;
import EdgeDetect::*;

import SceMiDefines::*;
import SceMiInternals::*;
import SceMiClocks::*;

export mkSceMiTCPMessageInPort, mkSceMiTCPMessageOutPort;
export mkSceMiTCPClockPort, mkSceMiTCPClockControl;
export buildSceMiTCP;
export mkSceMiTCPShutdownPoll;

module [SceMiModule] mkSceMiTCPMessageInPort(SceMiMessageInPortRawIfc#(a))
   provisos (Bits#(a,sz));

   // The constant value is determined by the SceMi infrastructure
   // linkage tool.
   ReadOnly#(UInt#(32)) index <- mkSceMiConstant();

   ReadOnly#(Bool) inReset <- isResetAsserted();

   PulseWire data_available <- mkPulseWire();
   Wire#(a)  the_data       <- mkWire();

   RequestDetect req_detect <- mkRequestDetect();

   (* no_implicit_conditions, fire_when_enabled *)
   rule poll (!inReset && req_detect.pending);
      let ok <- bscemi_is_data_available(index);
      if (ok)
      begin
	 data_available.send();
	 let val <- bscemi_recv(index, fromInteger(valueOf(sz)));
         the_data <= val;
	 req_detect.ack;
      end
   endrule: poll

   (* no_implicit_conditions, fire_when_enabled *)
   rule do_request (!inReset && req_detect.pulse);
      bscemi_request(index);
   endrule: do_request

   method Bool has_data();
      return data_available;
   endmethod: has_data

   method a read() if (data_available);
      return the_data;
   endmethod: read

   method Action request;
      req_detect.send();
   endmethod: request

endmodule: mkSceMiTCPMessageInPort


module [SceMiModule] mkSceMiTCPMessageOutPort(SceMiMessageOutPortIfc#(a))
   provisos (Bits#(a,sz));

   // The constant value is determined by the SceMi infrastructure
   // linkage tool.
   ReadOnly#(UInt#(32)) index <- mkSceMiConstant();

   Reg#(Bool) space_available <- mkReg(False);

   let state <- getContext();
   ReadOnly#(SceMiCycleStamp) cycle_stamp = state.stamp;

   (* fire_when_enabled *)
   rule check_buffer;
      let ok <- bscemi_is_buffer_available(index);
      if (ok) space_available <= True;
   endrule: check_buffer

   method Bool accepting_data();
      return space_available;
   endmethod: accepting_data

   method Action send(a x) if (space_available);
      bscemi_send(index, fromInteger(valueOf(sz)), pack(x), cycle_stamp);
   endmethod: send

endmodule: mkSceMiTCPMessageOutPort


module [SceMiModule] mkSceMiTCPClockPort#( parameter Integer clockNum
				         , parameter Integer ratioNumerator
				         , parameter Integer ratioDenominator
				         , parameter Integer dutyHi
				         , parameter Integer dutyLo
				         , parameter Integer phase
					 , parameter Integer resetCycles
					 , parameter Integer clockGroup
					 )
                                         (SceMiClockPortIfc);
   (*hide*)
   let _m <- mkSceMiBSVClockPort( clockNum
				, ratioNumerator, ratioDenominator
				, dutyHi, dutyLo, phase
				, resetCycles
				, clockGroup
				, TCP
				);
   return _m;
endmodule: mkSceMiTCPClockPort


module [SceMiModule] mkSceMiTCPClockControl#( parameter Integer clockNum
                                            , Bool allow_pos_edge
					    , Bool allow_neg_edge
                                            )
					    (SceMiClockControlIfc);
   (*hide*)
   let _m <- mkSceMiBSVClockControl(clockNum, allow_pos_edge, allow_neg_edge);
   return _m;
endmodule: mkSceMiTCPClockControl

module [Module] buildSceMiTCP#(SceMiModule#(i) mod
                              , Vector#(n,Clock) clks_in
                              , Vector#(n,Reset) rsts_in
			      )
                              (i);

   // This module's clock is the base SCE-MI clock
   Clock scemiClock <- exposeCurrentClock();
   Reset scemiReset <- exposeCurrentReset();

   // We create a clock for use as the uncontrolled clock
   (*hide_all*) MakeClockIfc#(Bit#(1)) uclkgen <- mkUngatedClock(0);
   Clock uClock = uclkgen.new_clk;

   (*hide_all*) PulseWire rising_uclock_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   ReadOnly#(Bool) rising_uclock = pulseWireToReadOnly(rising_uclock_pw);

   (*hide*) rule toggle_uclock;
      let new_value = ~uclkgen.getClockValue();
      uclkgen.setClockValue(new_value);
      if (new_value == 1)
	 rising_uclock_pw.send();
   endrule

   // We create a reset for use as the uncontrolled reset
   (*hide_all*) MakeResetIfc rstgen <- genSceMiReset(uClock, clocked_by scemiClock, reset_by scemiReset);
   Reset uReset = rstgen.new_rst;

   // Setup initial state for the SceMiModule monad
   (*hide_all*) SceMiModuleState init_state <- get_initial_state(uClock, uReset, scemiClock, scemiReset, rising_uclock, TCP);

   // Possibly add external clocks and resets to the state
   init_state = add_external_clocks_and_resets(init_state, clks_in, rsts_in);

   // Execute the SceMi module with the initial state
   let {state, _m} <- runWithContext(init_state, mod, clocked_by uClock, reset_by uReset);

   // Create the clock generation logic
   (*hide_all*) let clockGenerators <- build_clock_generator(state, rstgen);

   // Return module contents in Module monad
   return _m;
endmodule: buildSceMiTCP


// This module allows an external shutdown trigger to
// be detected and the simulation stopped.
module mkSceMiTCPShutdownPoll();
   (*hide*) rule detect_shutdown;
      Bool die <- bscemi_shutdown_triggered();
      if (die) $finish(0);
   endrule
endmodule: mkSceMiTCPShutdownPoll

// ================================================================
// Access to C communication library used internally

import "BDPI" function ActionValue#(Bool) bscemi_is_data_available(UInt#(32) index);
import "BDPI" function ActionValue#(a) bscemi_recv(UInt#(32) index, UInt#(32) len)
		 provisos (Bits#(a,sz));
import "BDPI" function Action bscemi_request(UInt#(32) index);
import "BDPI" function ActionValue#(Bool) bscemi_is_buffer_available(UInt#(32) index);
import "BDPI" function Action bscemi_send(UInt#(32) index, UInt#(32) len, Bit#(sz) data, SceMiCycleStamp stamp);
import "BDPI" function ActionValue#(Bool) bscemi_shutdown_triggered();

// Imports of C library routines which are used from Verilog
// but not in BSV.
import "BDPI" function Action bscemi_open_socket(UInt#(32) tcp_port);
import "BDPI" function Action bscemi_close_socket();

endpackage: SceMiTCP
