// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiEveMacros;

// This is a set of BSV wrappers around EVE's zcei*
// macro stubs.

import List::*;
import Vector::*;
import Clocks::*;
import ModuleContext::*;

import SceMiDefines::*;
import SceMiInternals::*;

export mkSceMiEveMessageInPort, mkSceMiEveMessageOutPort;
export mkSceMiEveClockPort, mkSceMiEveClockControl;
export buildSceMiEve;
export mkSceMiEveExternalClockPort;

// Instantiate a zceiMessageInPort macro
import "BVI" zceiMessageInPort =
   module mkSceMiEveMessageInPort(SceMiMessageInPortRawIfc#(a))
      provisos (Bits#(a,sz));

      default_clock clk(); // no CLK port
      no_reset;

      parameter Pwidth = valueOf(sz);

      method transmitReady has_data();
      method message read() ready(transmitReady);
      method request() enable(receiveReady);

      schedule (read, has_data) CF (read, has_data, request);
      schedule request C request;
   endmodule: mkSceMiEveMessageInPort

// Instantiate a zceiMessageOutPort macro
import "BVI" zceiMessageOutPort =
   module mkSceMiEveMessageOutPortRaw(SceMiMessageOutPortRawIfc#(a))
      provisos (Bits#(a,sz));

      default_clock clk(); // no CLK port
      no_reset;

      parameter Pwidth = valueOf(sz);

      method receiveReady try_to_send(message) enable(transmitReady);

      schedule try_to_send C try_to_send;
   endmodule: mkSceMiEveMessageOutPortRaw

// This wrapper translates from the raw try/accept protocol to the
// desired ready/enable protocol.
module mkSceMiEveMessageOutPort(SceMiMessageOutPortIfc#(a))
   provisos(Bits#(a,sz));
   (*hide*)
   let _m <- rawOutPortToBSV(mkSceMiEveMessageOutPortRaw);
   return _m;
endmodule

// Instantiate a zceiClockPort macro
module [SceMiModule] mkSceMiEveClockPort#( parameter Integer clockNum
				         , parameter Integer ratioNumerator
				         , parameter Integer ratioDenominator
				         , parameter Integer dutyHi
				         , parameter Integer dutyLo
				         , parameter Integer phase
				         , parameter Integer resetCycles
					 )
                                         (SceMiClockPortIfc);

   // Extract the name of this module
   Reg#(Bit#(1)) _dummy <- mkRegU;
   String clockName = primGetNameString(primGetModuleName(_dummy));

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
      let _m <- mkEveClockPort(clockNum, clockName);
      cClock = _m.cclock;
      cReset = _m.creset;

      // Add the EVE reset info to the state
      state.rawResets = List::cons(_m.creset,state.rawResets);

      // Report information on this domain
      ClockPortInfo port;
      port.clockNum         = clockNum;
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

endmodule: mkSceMiEveClockPort

import "BVI" zceiClockPort =
   module mkEveClockPort#(parameter Integer clockNum
                         ,parameter String  clockName
                         )
                         (SceMiClockPortIfc);
      default_clock no_clock;
      no_reset;

      parameter clockNum   = clockNum;
      parameter cclockName = clockName;

      output_clock cclock(cclock);
      output_reset creset(cresetn) clocked_by(cclock);
   endmodule: mkEveClockPort

// DISCLAIMER:
// The following kludge was implemented reluctantly and with
// the proper measure of shame and self-loathing.  Its
// purpose is to manipulate, with 0 delay, the global
// uncontrolled reset in response to deassertion of the
// controlled resets from EVE's clock ports.

interface CoerceResetIfc;
   method Action assertReset();
   interface Reset new_rst;
endinterface: CoerceResetIfc

module mkResetFromWF(MakeResetIfc);
   (* hide *)
   CoerceResetIfc _cr <- vCoerceReset();

   method assertReset = _cr.assertReset;
   method Bool isAsserted(); // NOT TO BE USED!!!!
      return error("isAsserted() was called on a mkResetFromWF!");
   endmethod
   interface Reset new_rst = _cr.new_rst;
endmodule: mkResetFromWF

import "BVI" ResetInverter =
   module vCoerceReset(CoerceResetIfc);
      default_clock clk();
      no_reset;

      output_reset new_rst(RESET_OUT) clocked_by(clk);

      method assertReset() enable(RESET_IN) clocked_by(clk);

      schedule assertReset C assertReset;

      path (RESET_IN, RESET_OUT);
   endmodule: vCoerceReset

// End of kludge

// Note: EVE's ZeBu software does not handle multiple clock
// controllers for the same clock within a module.  Therefore, the BSV
// SceMi library works around this by gathering all of the
// allow_posedge and allow_negedge signals from all instances on the
// same clockNum together and using the AND of all of the values to
// control a single clock controller instance.

// Instantiate a zceiClockControl macro
module [SceMiModule] mkSceMiEveClockControl#( parameter Integer clockNum
                                            , Bool allow_pos_edge
                                            , Bool allow_neg_edge
                                            )
                                            (SceMiClockControlIfc);

   // Extract the state from the module context
   let state <- getContext();

   // If this is the first clock control for this domain,
   // instantiate the macro and use its clock.
   // Subsequent instantiations of clock controls for this
   // domain do not instantiate another macro but instead
   // just reuse the outputs from the first instantiation.
   Clock uClock;
   Reset uReset;
   RawSceMiClockControlIfc _cc;
   if (lookup(clockNum, state.ccifcs) matches tagged Valid .ctrl)
   begin
      // Reuse the existing controller outputs for this domain
      uClock = ctrl.uclock;
      uReset = ctrl.ureset;
      _cc = ctrl;
   end
   else
   begin
      // instantiate the first clock controller for this domain

      // See if there is existing uncontrolled domain info.
      // If so, take the uclock and ureset from there.
      // Otherwise, add the udomain info based on the
      // local clock control and a newly created reset generator.
      if (state.udomain matches tagged Valid .dom)
      begin
	 // Use existing uclock and ureset info
	 uClock = dom.clk;
	 uReset = dom.rst;

	 // Create the low-level clock controller instance.
	 (* doc = "synthesis syn_noprune=1" *)
	 RawSceMiClockControlIfc _m <- mkEveClockControlClone(clockNum,uClock,uReset);
	 _cc = (interface RawSceMiClockControlIfc;
		   interface Clock uclock = uClock;
		   interface Reset ureset = uReset;
		   method Action allow_posedge();
		      _m.allow_posedge();
		   endmethod
		   method Action allow_negedge();
		      _m.allow_negedge();
		   endmethod
		   method Bool pre_posedge();
		      return _m.pre_posedge();
		   endmethod
		   method Bool pre_negedge();
		      return _m.pre_negedge();
		   endmethod
	        endinterface);
      end
      else
      begin
	 // This is the first clock controller, so take the uClock
	 // from it.

	 // Create the low-level clock controller instance.
	 (* doc = "synthesis syn_noprune=1" *)
	 RawSceMiClockControlIfc _m <- mkEveClockControl(clockNum);
	 uClock = _m.uclock;

	 // Make uReset using imported Verilog trickery
	 MakeResetIfc rstgen <- mkResetFromWF(clocked_by uClock, reset_by noReset);
	 uReset = rstgen.new_rst;

	 // Record uClock and uReset for future clock controls
	 DomainInfo udinfo;
	 udinfo.clk = uClock;
	 udinfo.rst = uReset;
	 state.udomain = tagged Valid udinfo;

	 // Record uReset generator for buildSceMi module
	 state.rstgens = List::cons(rstgen, state.rstgens);

	 // Record raw EVE reset for buildSceMi module
	 // We have to invert the output reset to be active low.
	 Reset eve_uresetn <- mkResetInverter(_m.ureset, clocked_by _m.uclock);
	 state.rawResets = List::cons(eve_uresetn, state.rawResets);

	 // This is a little kludge to set the reset associated with
	 // the exported methods to use the new uReset even though the
	 // methods on the import are associated with a different reset.
	 Wire#(Bool) _force_my_reset <- mkDWire(True, clocked_by uClock, reset_by uReset);

	 // The recorded version of the interface should use
	 // uReset for the methods.
	 _cc = (interface RawSceMiClockControlIfc;
		   interface Clock uclock = uClock;
		   interface Reset ureset = uReset;
		   method Action allow_posedge() if (_force_my_reset);
		      _m.allow_posedge();
		   endmethod
		   method Action allow_negedge() if (_force_my_reset);
		      _m.allow_negedge();
		   endmethod
		   method Bool pre_posedge() if (_force_my_reset);
		      return _m.pre_posedge();
		   endmethod
		   method Bool pre_negedge() if (_force_my_reset);
		      return _m.pre_negedge();
		   endmethod
	        endinterface);
      end
      state.ccifcs = List::cons(tuple2(clockNum,_cc),state.ccifcs);
   end

   // Instantiate some wires to communicate the allow_posedge
   // and allow_negedge values to the SceMi builder module.
   PulseWire allow_pos_pw <- mkPulseWire(clocked_by uClock, reset_by uReset);
   PulseWire allow_neg_pw <- mkPulseWire(clocked_by uClock, reset_by uReset);

   ClockControlInfo ccinfo;
   ccinfo.clockNum = clockNum;
   ccinfo.ens = (interface ClkEnableIfc;
		    method allow_pos = allow_pos_pw;
		    method allow_neg = allow_neg_pw;
		    method Action tick(Bool value);
		       noAction;  // not used by EVE
		    endmethod
		 endinterface);
   state.clkcntrls = List::cons(ccinfo, state.clkcntrls);

   putContext(state);

   (* fire_when_enabled, no_implicit_conditions *)
   rule control_pos_edge if (allow_pos_edge);
      allow_pos_pw.send();
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule control_neg_edge if (allow_neg_edge);
      allow_neg_pw.send();
   endrule

   // This is the user-visible interface

   interface Clock uclock = uClock;
   interface Reset ureset = uReset;

   method pre_posedge = _cc.pre_posedge;
   method pre_negedge = _cc.pre_negedge;

endmodule: mkSceMiEveClockControl

import "BVI" zceiClockControl =
   module mkEveClockControl#(parameter Integer clockNum)
                            (RawSceMiClockControlIfc);
      default_clock no_clock;
      no_reset;

      parameter clockNum = clockNum;

      output_clock uclock(uclock);
      output_reset ureset(ureset) clocked_by(uclock);

      // Use no reset with these methods, since the ureset
      // supplied to the user will be inverted.  The correct
      // reset is attached to the methods in the layer above
      // this import.
      method allow_posedge() enable(readyForCclock) clocked_by(uclock);
      method cclockEnabled pre_posedge() clocked_by(uclock);
      method allow_negedge() enable(readyForCclockNegEdge) clocked_by(uclock);
      method cclockNegEdgeEnabled pre_negedge() clocked_by(uclock);

      schedule allow_posedge C allow_posedge;
      schedule allow_negedge C allow_negedge;
      schedule allow_posedge CF allow_negedge;
      schedule (pre_posedge, pre_negedge) CF (pre_posedge, pre_negedge);
      schedule allow_posedge SB pre_posedge;
      schedule allow_negedge SB pre_negedge;
      schedule allow_posedge CF pre_negedge;
      schedule allow_negedge CF pre_posedge;

      path (readyForCclock, cclockEnabled);
      path (readyForCclockNegEdge, cclockNegEdgeEnabled);

   endmodule: mkEveClockControl

import "BVI" zceiClockControl =
   module mkEveClockControlClone#(parameter Integer clockNum, Clock uClock, Reset uReset)
                                 (RawSceMiClockControlIfc);
      default_clock no_clock;
      no_reset;

      parameter clockNum = clockNum;

      input_clock real_uclock() = uClock;
      input_reset real_ureset() = uReset;
      output_clock uclock(uclock);
      output_reset ureset(ureset) clocked_by(uclock);

      method allow_posedge() enable(readyForCclock) clocked_by(real_uclock) reset_by(real_ureset);
      method cclockEnabled pre_posedge() clocked_by(real_uclock) reset_by(real_ureset);
      method allow_negedge() enable(readyForCclockNegEdge) clocked_by(real_uclock) reset_by(real_ureset);
      method cclockNegEdgeEnabled pre_negedge() clocked_by(real_uclock) reset_by(real_ureset);

      schedule allow_posedge C allow_posedge;
      schedule allow_negedge C allow_negedge;
      schedule allow_posedge CF allow_negedge;
      schedule (pre_posedge, pre_negedge) CF (pre_posedge, pre_negedge);
      schedule allow_posedge SB pre_posedge;
      schedule allow_negedge SB pre_negedge;
      schedule allow_posedge CF pre_negedge;
      schedule allow_negedge CF pre_posedge;

      path (readyForCclock, cclockEnabled);
      path (readyForCclockNegEdge, cclockNegEdgeEnabled);

      same_family(uclock,real_uclock);

   endmodule: mkEveClockControlClone

module [Module] buildSceMiEve#( SceMiModule#(i) mod
                              , Vector#(n,Clock) clks_in
                              , Vector#(n,Reset) rsts_in
                              , SceMiLinkType linktype
                              )
                              (i);

   // Initial SceMi builder state
   SceMiModuleState init_state = get_initial_state_unclocked(linktype);

   // Possibly add external clocks and resets to the state
   init_state = add_external_clocks_and_resets(init_state, clks_in, rsts_in);

   // Run the SceMi builder module using the initial state
   let {state, _m} <- runWithContext(init_state,mod);

   if (!isNull(state.rstgens))
   begin

      // There should be only one reset generator -- the one for uReset
      if (length(state.rstgens) != 1)
      begin
	 error("Multiple reset generators for uReset");
      end
      MakeResetIfc rstgen = state.rstgens[0];

      // Get the uncontrolled clock
      Clock uClock = noClock;
      Reset uReset = noReset;
      if (state.udomain matches tagged Valid .dom)
      begin
	 uClock = dom.clk;
	 uReset = dom.rst;
      end
      else
      begin
	 error("No uncontrolled domain information in design!");
      end

      // Determine if any reset is asserted
      List#(Bool) reset_tests <- List::mapM(testReset(uClock), state.rawResets);
      Bool any_in_reset = List::foldl(\|| , False, reset_tests);

      // handle assertion of uReset whenever any uncontrolled
      // or controlled reset is asserted
      rule handle_reset if (any_in_reset);
	 rstgen.assertReset();
      endrule
   end

   // AND together allow_posedge and allow_negedge for all instances
   // of clock controllers in each domain and use the result to
   // control the single instance for the domain.
   List#(Integer) clock_nums = List::map(tpl_1,state.ccifcs);
   List#(Action) acts = List::map(control_edges(state), clock_nums);

   rule update_clock_controller;
      List::joinActions(acts);
   endrule

   // return module contents in Module monad
   return _m;
endmodule: buildSceMiEve

module testReset(Clock uClock, Reset rst, Bool ifc);
   Reset rstu <- mkAsyncReset(0, rst, uClock);
   ReadOnly#(Bool) inReset <- isResetAsserted(clocked_by uClock, reset_by rstu);
   return inReset;
endmodule

function Action control_edges(SceMiModuleState state, Integer clockNum);
   action
      case (List::lookup(clockNum,state.ccifcs)) matches
	 tagged Valid .ccifc:
	    begin
	       List#(ClockControlInfo) ccs = List::filter(isForClock(clockNum), state.clkcntrls);
	       Bool all_allow_pos = List::foldl(\&& , True, List::map(call_allow_pos, ccs));
	       Bool all_allow_neg = List::foldl(\&& , True, List::map(call_allow_neg, ccs));
	       if (all_allow_pos)
		  ccifc.allow_posedge();
	       if (all_allow_neg)
		  ccifc.allow_negedge();
	    end
	 tagged Invalid:
	    error("control_edges: invalid clockNum");
      endcase
   endaction
endfunction

function Bool call_allow_pos(ClockControlInfo ccinfo);
   return ccinfo.ens.allow_pos();
endfunction

function Bool call_allow_neg(ClockControlInfo ccinfo);
   return ccinfo.ens.allow_neg();
endfunction

// Instantiate a clock port outside of the SceMiModule monad
module mkSceMiEveExternalClockPort#(Integer clockNum)
                                   (SceMiClockPortIfc);

   // Extract the name of this module
   Reg#(Bit#(1)) _dummy <- mkRegU;
   String clockName = primGetNameString(primGetModuleName(_dummy));

   (* hide *)
   (* doc = "synthesis syn_noprune=1" *)
   let _m <- mkEveClockPort(clockNum, clockName);

   interface Clock cclock = _m.cclock;
   interface Reset creset = _m.creset;

endmodule: mkSceMiEveExternalClockPort


endpackage: SceMiEveMacros
