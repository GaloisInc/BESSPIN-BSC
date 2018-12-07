// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiMacros;

// This is a set of BSV wrappers around Verilog SCE-MI standard
// macro stubs.  The SCE-MI standard defines these Verilog
// stub interfaces, and they should generate Verilog module
// instantiations which work with any standard SCE-MI flow.

import List::*;
import Vector::*;
import Clocks::*;
import ModuleContext::*;

import SceMiDefines::*;
import SceMiInternals::*;

export mkSceMiMacroMessageInPort, mkSceMiMacroMessageOutPort;
export mkSceMiMacroClockPort, mkSceMiMacroClockControl;
export buildSceMiMacro;

// The SceMi standard requires MessageInPorts to use a request/grant
// protocol rather than a ready/enable protocol, because the ready
// on the SW side is triggered by the ReceiveReady signal on the HW
// side.  Therefore, we import the macro with a request/grant
// interface style and then wrap it with a protocol translator to
// produce the ready/enable protocol that fits nicely with BSV.

// Instantiate an in-port macro which conforms to the SCE-MI standard
import "BVI" SceMiMessageInPort =
   module mkSceMiMacroMessageInPort(SceMiMessageInPortRawIfc#(a))
      provisos (Bits#(a,sz));

      default_clock clk(); // no CLK port
      no_reset;

      parameter PortWidth = valueOf(sz);

      method TransmitReady has_data();
      method Message read() ready(TransmitReady);
      method request() enable(ReceiveReady);

      schedule (read, has_data) CF (read, has_data, request);
      schedule request C request;
   endmodule: mkSceMiMacroMessageInPort

// The SceMi standard requires MessageOutPorts to use a try/accept
// protocol rather than a ready/enable protocol, because the attempt
// to read on the HW side triggers the sending from the SW side.
// Therefore, we import the macro with a try/accept interface style
// and then wrap it with a protocol translator to produce the
// ready/enable protocol that fits nicely with BSV.

// Instantiate an out-port macro which conforms to the SCE-MI standard
import "BVI" SceMiMessageOutPort =
   module mkSceMiMacroMessageOutPortRaw(SceMiMessageOutPortRawIfc#(a))
      provisos (Bits#(a,sz));

      default_clock clk(); // no CLK port
      no_reset;

      parameter PortWidth = valueOf(sz);
//      parameter PortPriority = 0;  // no longer used

      method ReceiveReady try_to_send(Message) enable(TransmitReady);

      schedule try_to_send C try_to_send;
   endmodule: mkSceMiMacroMessageOutPortRaw

// This wrapper translates from the raw try/accept protocol to the
// desired ready/enable protocol.
module mkSceMiMacroMessageOutPort(SceMiMessageOutPortIfc#(a))
   provisos(Bits#(a,sz));
   (*hide*)
   let _m <- rawOutPortToBSV(mkSceMiMacroMessageOutPortRaw);
   return _m;
endmodule

// Instantiate a clk-port macro which conforms to the SCE-MI standard
module [SceMiModule] mkSceMiMacroClockPort#( parameter Integer clockNum
				           , parameter Integer ratioNumerator
				           , parameter Integer ratioDenominator
				           , parameter Integer dutyHi
				           , parameter Integer dutyLo
				           , parameter Integer phase
				           , parameter Integer resetCycles
					   )
                                           (SceMiClockPortIfc);

   // Extract the current state
   let state <- getContext();

   // See if there is existing controlled domain info for this
   // clockNum.  If so, take the cclock and creset from there.
   // Otherwise, add the domain info based on the local clock port.
   Clock cClock;
   Reset cReset;
   if (lookup(clockNum, state.cdomains) matches tagged Valid .dom)
   begin
      // Use existing cclock and creset info
      cClock = dom.clk;
      cReset = dom.rst;

      warningM("Multiple clock ports found for clock number " + integerToString(clockNum) + ".");

      // Check that the parameters match the defining clock port
      // instance
      Maybe#(ClockPortInfo) orig_port = List::find(isForClockPort(clockNum), state.clkports);
      if (orig_port matches tagged Valid .p)
      begin
	 if ((p.ratioNumerator * ratioDenominator) != (ratioNumerator * p.ratioDenominator))
	 begin
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has different frequency.");
	 end

	 if (((p.dutyHi * (dutyHi + dutyLo)) != (dutyHi * (p.dutyHi + p.dutyLo))) ||
	     ((p.dutyLo * (dutyHi + dutyLo)) != (dutyLo * (p.dutyHi + p.dutyLo))))
	 begin
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has different duty cycle.");
	 end

	 if ((p.phase * (dutyHi + dutyLo)) != (phase * (p.dutyHi + p.dutyLo)))
	 begin
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has different phase shift.");
	 end

	 if (p.resetCycles != resetCycles)
	 begin
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has reset duration.");
	 end
      end
      else
      begin
	 error("Unable to find parameters for original SceMiClockPort with ClockNum " + integerToString(clockNum) + ".");
      end
   end
   else
   begin
      // This is the first clock port for this clockNum, so take
      // the cClock and cReset from it.

      (* hide *)
      (* doc = "synthesis syn_noprune=1" *)
      let _m <- vSceMiClockPort( clockNum
			       , ratioNumerator, ratioDenominator
			       , dutyHi, dutyLo, phase
			       , resetCycles
			       );
      cClock = _m.cclock;

      // SceMi resets are positively asserted, so we need to invert them
      Reset inverted <- mkResetInverter(_m.creset, clocked_by _m.cclock);
      cReset = inverted;

      // Report information on this domain
      ClockPortInfo port;
      port.clockNum         = clockNum;
      port.clockGroup       = noClockGroup;
      port.ratioNumerator   = ratioNumerator;
      port.ratioDenominator = ratioDenominator;
      port.dutyHi           = dutyHi;
      port.dutyLo           = dutyLo;
      port.phase            = phase;
      port.resetCycles      = resetCycles;
      state.clkports = List::cons(port, state.clkports);

      // Record the cClock and cReset
      DomainInfo dinfo;
      dinfo.clk = cClock;
      dinfo.rst = cReset;
      state.cdomains = List::cons(tuple2(clockNum,dinfo),state.cdomains);

      // Update the context with the augmented state
      putContext(state);
   end

   interface Clock cclock = cClock;
   interface Reset creset = cReset;

endmodule: mkSceMiMacroClockPort


import "BVI" SceMiClockPort =
   module vSceMiClockPort#( parameter Integer clockNum
			  , parameter Integer ratioNumerator
			  , parameter Integer ratioDenominator
			  , parameter Integer dutyHi
			  , parameter Integer dutyLo
			  , parameter Integer phase
			  , parameter Integer resetCycles
			  )
                          (SceMiClockPortIfc);
      default_clock no_clock;
      no_reset;

      parameter ClockNum         = clockNum;
      parameter RatioNumerator   = ratioNumerator;
      parameter RatioDenominator = ratioDenominator;
      parameter DutyHi           = dutyHi;
      parameter DutyLo           = dutyLo;
      parameter Phase            = phase;
      parameter ResetCycles      = resetCycles;

      output_clock cclock(Cclock);
      output_reset creset(Creset) clocked_by(cclock);
   endmodule: vSceMiClockPort

// Instantiate a clk-control macro which conforms to the SCE-MI standard
module [SceMiModule] mkSceMiMacroClockControl#( parameter Integer clockNum
                                              , Bool allow_pos_edge
                                              , Bool allow_neg_edge
                                              )
                                              (SceMiClockControlIfc);

   // Extract the state from the module context
   let state <- getContext();

   // See if there is existing uncontrolled domain info.
   // If so, take the uclock and ureset from there.
   // Otherwise, add the udomain info based on the
   // local clock control.
   Clock uClock;
   Reset uReset;
   RawSceMiClockControlIfc _m;
   if (state.udomain matches tagged Valid .dom)
   begin
      // Use existing uclock and ureset info
      uClock = dom.clk;
      uReset = dom.rst;

      // Create the low-level clock controller instance.
      (* doc = "synthesis syn_noprune=1" *)
      _m <- vSceMiClockControlClone(clockNum,uClock,uReset);
   end
   else
   begin
      // This is the first clock controller, so take the uClock
      // and uReset from it.

      // Create the low-level clock controller instance.
      (* doc = "synthesis syn_noprune=1" *)
      _m <- vSceMiClockControl(clockNum);
      uClock = _m.uclock;

      // SceMi resets are positively asserted, so we need to invert them
      Reset inverted <- mkResetInverter(_m.ureset, clocked_by _m.uclock);
      uReset = inverted;

      // Record uClock and uReset for future clock controls
      DomainInfo udinfo;
      udinfo.clk = uClock;
      udinfo.rst = uReset;
      state.udomain = tagged Valid udinfo;

      putContext(state);
   end

   // This is a little kludge to set the reset associated with
   // the exported methods to use the new uReset even though the
   // methods on the import are associated with a different reset.
   Wire#(Bool) _force_my_reset <- mkDWire(True, clocked_by uClock, reset_by uReset);

   (* fire_when_enabled, no_implicit_conditions *)
   rule control_pos_edge if (allow_pos_edge);
      _m.allow_posedge();
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule control_neg_edge if (allow_neg_edge);
      _m.allow_negedge();
   endrule

   // This is the user-visible interface
   interface uclock = uClock;
   interface ureset = uReset;

   method Bool pre_posedge() if (_force_my_reset);
      return _m.pre_posedge;
   endmethod

   method Bool pre_negedge() if (_force_my_reset);
      return _m.pre_negedge;
   endmethod

endmodule: mkSceMiMacroClockControl

import "BVI" SceMiClockControl =
   module vSceMiClockControl#(parameter Integer clockNum)
                             (RawSceMiClockControlIfc);
      default_clock no_clock;
      no_reset;

      parameter ClockNum = clockNum;

      output_clock uclock(Uclock);
      output_reset ureset(Ureset) clocked_by(uclock);

      // Use no reset with these methods, since the ureset
      // supplied to the user will be inverted.  The correct
      // reset is attached to the methods in the layer above
      // this import.
      method allow_posedge() enable(ReadyForCclock) clocked_by(uclock);
      method CclockEnabled pre_posedge() clocked_by(uclock);
      method allow_negedge() enable(ReadyForCclockNegEdge) clocked_by(uclock);
      method CclockNegEdgeEnabled pre_negedge() clocked_by(uclock);

      schedule allow_posedge C allow_posedge;
      schedule allow_negedge C allow_negedge;
      schedule allow_posedge CF allow_negedge;
      schedule (pre_posedge, pre_negedge) CF (pre_posedge, pre_negedge);
      schedule allow_posedge SB pre_posedge;
      schedule allow_negedge SB pre_negedge;
      schedule allow_posedge CF pre_negedge;
      schedule allow_negedge CF pre_posedge;

      path (ReadyForCclock, CclockEnabled);
      path (ReadyForCclockNegEdge, CclockNegEdgeEnabled);

   endmodule: vSceMiClockControl

import "BVI" SceMiClockControl =
   module vSceMiClockControlClone#(parameter Integer clockNum, Clock uClock, Reset uReset)
                                  (RawSceMiClockControlIfc);
      default_clock no_clock;
      no_reset;

      parameter ClockNum = clockNum;

      input_clock real_uclock() = uClock;
      input_reset real_ureset() = uReset;
      output_clock uclock(Uclock);
      output_reset ureset(Ureset) clocked_by(uclock);

      method allow_posedge() enable(ReadyForCclock) clocked_by(real_uclock) reset_by(real_ureset);
      method CclockEnabled pre_posedge() clocked_by(real_uclock) reset_by(real_ureset);
      method allow_negedge() enable(ReadyForCclockNegEdge) clocked_by(real_uclock) reset_by(real_ureset);
      method CclockNegEdgeEnabled pre_negedge() clocked_by(real_uclock) reset_by(real_ureset);

      schedule allow_posedge C allow_posedge;
      schedule allow_negedge C allow_negedge;
      schedule allow_posedge CF allow_negedge;
      schedule (pre_posedge, pre_negedge) CF (pre_posedge, pre_negedge);
      schedule allow_posedge SB pre_posedge;
      schedule allow_negedge SB pre_negedge;
      schedule allow_posedge CF pre_negedge;
      schedule allow_negedge CF pre_posedge;

      path (ReadyForCclock, CclockEnabled);
      path (ReadyForCclockNegEdge, CclockNegEdgeEnabled);

   endmodule: vSceMiClockControlClone

module [Module] buildSceMiMacro#( SceMiModule#(i) mod
                                , Vector#(n,Clock) clks_in
                                , Vector#(n,Reset) rsts_in
                                , SceMiLinkType linktype
                                )
                                (i);
   // Run the SceMi builder with empty initial state
   SceMiModuleState init_state = get_initial_state_unclocked(linktype);

   // Possibly add external clocks and resets to the state
   init_state = add_external_clocks_and_resets(init_state, clks_in, rsts_in);

   let {state, _m} <- runWithContext(init_state,mod);

   // return module contents in Module monad
   return _m;
endmodule: buildSceMiMacro

endpackage: SceMiMacros
