// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiAldecMacros;

// This is a set of BSV wrappers around Verilog SCE-MI macro
// stubs supporting the Aldec extensions.

import List::*;
import Clocks::*;
import ModuleContext::*;

import SceMiDefines::*;
import SceMiInternals::*;

export mkSceMiAldecClockPort;

// Instantiate a clk-port macro which supports Aldec's clockGroup extension
module [SceMiModule] mkSceMiAldecClockPort#( parameter Integer clockNum
					   , parameter Integer ratioNumerator
					   , parameter Integer ratioDenominator
					   , parameter Integer dutyHi
					   , parameter Integer dutyLo
					   , parameter Integer phase
					   , parameter Integer resetCycles
					   , parameter Integer clockGroup
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
      Maybe#(ClockPortInfo) orig_port = find(isForClockPort(clockNum), state.clkports);
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
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has different reset duration.");
	 end

	 if (p.clockGroup != clockGroup)
	 begin
	    error("Duplicate SceMiClockPort for ClockNum " + integerToString(p.clockNum) + " has different clock group.");
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
      let _m <- vAldecClockPort( clockNum
			       , ratioNumerator, ratioDenominator
			       , dutyHi, dutyLo, phase
			       , resetCycles
			       , clockGroup
			       );
      cClock = _m.cclock;

      // SceMi resets are positively asserted, so we need to invert them
      Reset inverted <- mkResetInverter(_m.creset, clocked_by _m.cclock);
      cReset = inverted;

      // Report information on this domain
      ClockPortInfo port;
      port.clockNum         = clockNum;
      port.clockGroup       = clockGroup;
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

endmodule: mkSceMiAldecClockPort


import "BVI" SceMiClockPort =
   module vAldecClockPort#( parameter Integer clockNum
			  , parameter Integer ratioNumerator
			  , parameter Integer ratioDenominator
			  , parameter Integer dutyHi
			  , parameter Integer dutyLo
			  , parameter Integer phase
			  , parameter Integer resetCycles
			  , parameter Integer clockGroup
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
      parameter ClockGroup       = clockGroup;

      output_clock cclock(Cclock);
      output_reset creset(Creset) clocked_by(cclock);
   endmodule: vAldecClockPort

endpackage: SceMiAldecMacros