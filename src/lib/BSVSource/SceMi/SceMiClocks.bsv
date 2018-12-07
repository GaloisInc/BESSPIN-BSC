// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiClocks;

// This is an implementation of SceMi clock control modules

import List::*;
import Vector::*;
import Clocks::*;
import Probe::*;
import Connectable::*;
import ModuleContext::*;
import XilinxCells::*;

import SceMiDefines::*;
import SceMiInternals::*;

//import MkR::*;

interface SceMiClockGenIfc;
   (* always_ready *)
   method Bool outOfReset();
   (* always_ready *)
   method Bool isClockAdvancing();
endinterface

function Bool checkIsInReset(SceMiClockGenIfc cgi);
   return !cgi.outOfReset();
endfunction: checkIsInReset

function Bool checkIsClockAdvancing(SceMiClockGenIfc cgi);
   return cgi.isClockAdvancing();
endfunction

// Print an Integer padded to 8 characters wide
function String pad(Integer x);
   String s = integerToString(x);
   if (x < 10)
      return "       " + s;
   else if (x < 100)
      return "      " + s;
   else if (x < 1000)
      return "     " + s;
   else if (x < 10000)
      return "    " + s;
   else if (x < 100000)
      return "   " + s;
   else if (x < 1000000)
      return "  " + s;
   else if (x < 10000000)
      return " " + s;
   else
      return s;
endfunction

function String prettyPrintReal(Real x, Integer digits);
   let {i,f} = primSplitReal(x);
   Integer limit = 10 ** digits;
   String s = "";
   if (i >= limit) begin
      s = integerToString(i);
   end
   else begin
      if (i > 1000)
         limit = limit / 1000;
      else if (i > 100)
         limit = limit / 100;
      else if (i > 10)
         limit = limit / 10;
      Integer n = primRealTrunc(f*fromInteger(limit));
      String zeros = "";
      if (limit == 100) begin
         if (n < 10)
            zeros = "0";
      end
      else if (limit == 1000) begin
         if (n < 10)
            zeros = "00";
         else if (n < 100)
            zeros = "0";
      end
      else if (limit == 10000) begin
         if (n < 10)
            zeros = "000";
         else if (n < 100)
            zeros = "00";
         else if (n < 1000)
            zeros = "0";
      end
      s = (integerToString(i) + "." + zeros + integerToString(n));
   end
   return s;
endfunction

// This module builds clock generation logic to implement
// the SCE-MI clock control and reset protocols in BSV.
module build_clock_generator( SceMiModuleState state
                            , MakeResetIfc rstgen
                            , SceMiClockGenIfc ifc);

   messageM("\n(SCE-MI) Begin clock generation");

   Clock scemiClock = getSceMiClock(state);
   Reset scemiReset = getSceMiReset(state);
   Clock uClock     = getUClock(state);

   if (List::isNull(state.clkports))
      messageM("\n(SCE-MI) Warning: no controlled clocks found.");
   List#(ClockControlInfo) clock_controllers = state.clkcntrls;

   // Separate clock ports according to clockGroup number
   List#(ClockPortInfo) all_clock_ports = List::sortBy(cmpClockGroup, state.clkports);
   List#(List#(ClockPortInfo)) clock_groups_unsorted = List::groupBy(sameClockGroup, all_clock_ports);
   List#(List#(ClockPortInfo)) clock_groups = List::map(List::sortBy(cmpClockNum), clock_groups_unsorted);

   // Print messages describing the clocks in each group
   String msg = "";
   for (Integer g = 0; g < List::length(clock_groups); g = g + 1) begin
      List#(ClockPortInfo) clock_ports = clock_groups[g];
      Integer clock_group = List::head(clock_ports).clockGroup;
      if (clock_group == noClockGroup)
         msg = msg + "\n(SCE-MI) Default clock group";
      else
         msg = msg + "\n(SCE-MI) Clock group " + integerToString(clock_group);

      for (Integer i = 0; i < List::length(clock_ports); i = i + 1) begin
         ClockPortInfo p = clock_ports[i];
         msg = msg + "\n(SCE-MI) Clock #" + integerToString(p.clockNum) +
                   + "\n  Numerator: "    + integerToString(p.ratioNumerator)
                   + "\n  Denominator: "  + integerToString(p.ratioDenominator)
                   + "\n  DutyHi: "       + integerToString(p.dutyHi)
                   + "\n  DutyLo: "       + integerToString(p.dutyLo)
                   + "\n  Phase: "        + integerToString(p.phase)
                   + "\n  Reset Cycles: " + integerToString(p.resetCycles);
         List#(ClockControlInfo) ctrls = filter(isForClock(p.clockNum), clock_controllers);
         Integer num_controllers = List::length(ctrls);
         msg = msg + "\n(SCE-MI) Found " + integerToString(num_controllers)
                   + " controllers for clock #" + integerToString(p.clockNum);
      end
   end
   messageM(msg);

   // Check clock ports and clock controllers for errors
   for (Integer g = 0; g < List::length(clock_groups); g = g + 1) begin
      List#(ClockPortInfo) clock_ports = clock_groups[g];
      Integer found_error = List::foldl(\+ , 0, List::map(checkClockPortError(state.clkports), clock_ports));
      found_error = (found_error + List::foldl(\+ , 0, List::map(checkClockControlError(state.clkports),
                                                                 clock_controllers)));
      if (found_error > 0)
         error("Error(s) found in SceMiClockPort or SceMiClockControl specification, exiting...");
   end

   // Adjust all clock info to a common time base
   List#(ClockPortInfo) simplified = List::map(simplifyRatios, all_clock_ports);
   Integer denom_lcm = List::foldl(findDenomLCM, 1, simplified);
   List#(ClockPortInfo) pre_scaled = List::map(scaleClockRatio(denom_lcm),simplified);
   List#(Integer) edge_gcds = List::map(findEdgeGCD, pre_scaled);
   Integer time_base = denom_lcm / List::foldl(gcd,denom_lcm,edge_gcds);
   List#(ClockPortInfo) scaled_clocks = List::map(scaleClockRatio(time_base),simplified);

   List#(List#(ClockPortInfo)) scaled_clock_groups_unsorted = List::groupBy(sameClockGroup, scaled_clocks);
   List#(List#(ClockPortInfo)) scaled_clock_groups = List::map(List::sortBy(cmpClockNum), scaled_clock_groups_unsorted);
   Bool can_div2 = List::\and (List::map(List::all(canDivideBy2),scaled_clock_groups));
   if ((time_base % 2 == 0) && can_div2) begin
      time_base = time_base / 2;
      scaled_clocks = List::map(scaleClockRatio(time_base),simplified);
      scaled_clock_groups_unsorted = List::groupBy(sameClockGroup, scaled_clocks);
      scaled_clock_groups = List::map(List::sortBy(cmpClockNum), scaled_clock_groups_unsorted);
   end

   // Report scaled clock parameters
   msg = "";
   msg = msg + "\n(SCE-MI) Common clock time scale = " + integerToString(time_base);
   msg = msg + "\n(SCE-MI)    Group    Clock   Period     Rise     Fall     Freq";
   for (Integer g = 0; g < List::length(scaled_clock_groups); g = g + 1) begin
      List#(ClockPortInfo) scaled = scaled_clock_groups[g];

      Integer clock_group = List::head(scaled).clockGroup;
      String cgrp = (clock_group == noClockGroup) ? " default" : pad(clock_group);

      for (Integer i = 0; i < List::length(scaled); i = i + 1) begin
         ClockPortInfo p = scaled[i];
         String cnum = pad(p.clockNum);
         String period = pad(p.ratioNumerator);
         Real f = fromInteger(100) / fromInteger(p.ratioNumerator * 2);
         String freq = prettyPrintReal(f,3) + " MHz";
         if (p.ratioNumerator == 1) begin
            String style = p.inverted ? "  inverted uclock" : "        uclock   ";
            msg = msg + "\n(SCE-MI) " + cgrp + " " + cnum + " " + period + " " + style + "   " + freq;
         end
         else begin
            Integer hi = p.dutyHi;
            Integer lo = p.dutyLo;
            if (p.dutyHi == 0) begin
               hi = p.dutyLo / 2;
               lo = p.dutyLo - hi;
            end
            else if (p.dutyLo == 0) begin
               lo = p.dutyHi / 2;
               hi = p.dutyHi - lo;
            end
            String rise = pad(p.phase);
            String fall = pad((p.phase + hi) % p.ratioNumerator);
            msg = msg + "\n(SCE-MI) " + cgrp + " " + cnum + " " + period + " " + rise + " " + fall + "   " + freq;
         end
      end
   end
   msg = msg + "\n(SCE-MI) Note: clock frequencies assume 100 MHz reference clock";
   messageM(msg);

   // Build clock logic for each clock group
   List#(SceMiClockGenIfc) clock_gens <- List::mapM(build_one_clock_group(state),scaled_clock_groups);

   // Build shared reset logic

   Bool any_in_reset = List::any(checkIsInReset, clock_gens);
   if (! (isNull(state.clkports) && isNull(state.clkcntrls)))
   begin
      rule handle_reset if (any_in_reset);
         List#(Action) cresets = List::map(assertCReset, state.rstgens);
         List::joinActions(cresets);
         rstgen.assertReset();
         state.is_in_reset <= True;
      endrule
   end

   // Build shared cycle stamp logic

   Bool any_clock_advancing = List::any(checkIsClockAdvancing,clock_gens);
   Reg#(SceMiCycleStamp) cycle_stamp = state.proto_stamp;

   if (! (isNull(state.clkports) && isNull(state.clkcntrls)))
   begin
      ClockCounter one_to_one_cclock <- mkClockCounter( uClock
                                                      , 0, 1, fromInteger(2*time_base-1)
                                                      , clocked_by scemiClock
                                                      , reset_by   scemiReset);
      Reg#(Bool) free_stamp <- mkReg(False, clocked_by scemiClock, reset_by scemiReset);

      (* no_implicit_conditions, fire_when_enabled *)
      rule track_reset if (!state.rising_uclock);
         free_stamp <= !any_in_reset;
      endrule

      (* no_implicit_conditions, fire_when_enabled *)
      rule stall_one_to_one_cclock if (!free_stamp || !any_clock_advancing);
         one_to_one_cclock.stall();
      endrule

      (* no_implicit_conditions, fire_when_enabled *)
      rule incr_cycle_stamp if (one_to_one_cclock.triggerPosedge());
         cycle_stamp <= cycle_stamp + 1;
      endrule
   end

   // Combine clock gen interfaces

   method Bool outOfReset();
      return !any_in_reset;
   endmethod

   method Bool isClockAdvancing();
      return any_clock_advancing;
   endmethod

endmodule: build_clock_generator

interface ClockCounter;
   method Bool preAlignment();
   method Bool triggerPosedge();
   method Bool triggerNegedge();
   method Bool prePosedge();
   method Bool preNegedge();
   method Bool prePosedgeUser();
   method Bool preNegedgeUser();
   method Action stall();
endinterface

module mkClockCounter#(Clock uclock, UInt#(32) riseAt, UInt#(32) fallAt, UInt#(32) wrapAt)(ClockCounter);

   // expand pre-edge zone to fill a full uclock cycle
   UInt#(32) riseAt_alt = (riseAt == 0) ? wrapAt : (riseAt - 1);
   UInt#(32) fallAt_alt = (fallAt == 0) ? wrapAt : (fallAt - 1);

   // detect special cases
   Bool is_uclock = False;
   Bool is_inverted_uclock = False;

   if (wrapAt == 1) begin
      if (riseAt == 0) begin
         is_uclock = True;
         riseAt_alt = riseAt;
         fallAt_alt = fallAt;
      end
      else begin
         is_inverted_uclock = True;
         riseAt_alt = riseAt;
         fallAt_alt = fallAt;
      end
   end

   // count SCE-MI clock cycles
   Reg#(UInt#(32))   count    <- mkReg(0);
   PulseWire         stall_pw <- mkPulseWire();
   SyncBitIfc#(Bool) pre_posedge_uclk <- mkSyncBit05FromCC(uclock);
   SyncBitIfc#(Bool) pre_negedge_uclk <- mkSyncBit05FromCC(uclock);

   rule incr if (!stall_pw);
      UInt#(32) new_count = (count == wrapAt) ? 0 : (count + 1);
      count <= new_count;
      pre_posedge_uclk.send(new_count == riseAt || new_count == riseAt_alt);
      pre_negedge_uclk.send(new_count == fallAt || new_count == fallAt_alt);
   endrule

   // indicate that the point of alignment will occur on the
   // next SCE-MI clock cycle
   method Bool preAlignment();
      return (count == (wrapAt - 1));
   endmethod

   // indicate that the clock should rise on the next SCE-MI clock
   // cycle
   method Bool triggerPosedge();
      return (count == riseAt) && !stall_pw;
   endmethod

   // indicate that the clock should fall on the next SCE-MI clock
   // cycle
   method Bool triggerNegedge();
      return (count == fallAt) && !stall_pw;
   endmethod

   // indicate that the clock will rise on the next uclock posedge
   // (usually asserted for 2 SCE-MI clock cycles -- 1 uclock cycle)
   method Bool prePosedge();
      return (count == riseAt || count == riseAt_alt);
   endmethod

   // indicate that the clock will fall on the next uclock posedge
   // (usually asserted for 2 SCE-MI clock cycles -- 1 uclock cycle)
   method Bool preNegedge();
      return (count == fallAt || count == fallAt_alt);
   endmethod

   // indicate that the clock will rise on the next uclock posedge
   // (usually asserted for 2 SCE-MI clock cycles -- 1 uclock cycle)
   method Bool prePosedgeUser();
      return (is_uclock || pre_posedge_uclk.read());
   endmethod

   // indicate that the clock will fall on the next uclock posedge
   // (usually asserted for 2 SCE-MI clock cycles -- 1 uclock cycle)
   method Bool preNegedgeUser();
      return (is_inverted_uclock || pre_negedge_uclk.read());
   endmethod

   // this method is used to stop the counters from advancing while
   // the controlled clocks are stalled
   method Action stall();
      stall_pw.send();
   endmethod
endmodule

module setupClockCounter#(PulseWire stall_pw,
                          Clock scemiClock, Reset scemiReset,
                          Clock uClock,
                          ClockPortInfo p)
                         (ClockCounter);
   UInt#(32) wrap = fromInteger(2 * p.ratioNumerator - 1);
   UInt#(32) rise;
   UInt#(32) fall;

   if (p.ratioNumerator == 1) begin
      rise = p.inverted ? 1 : 0;
      fall = p.inverted ? 0 : 1;
   end
   else begin
      Integer hi = p.dutyHi;
      Integer lo = p.dutyLo;
      if (p.dutyHi == 0) begin
         hi = p.dutyLo / 2;
         lo = p.dutyLo - hi;
      end
      else if (p.dutyLo == 0) begin
         lo = p.dutyHi / 2;
         hi = p.dutyHi - lo;
      end
      rise = fromInteger(2 * p.phase);
      fall = fromInteger(2 * ((p.phase + hi) % p.ratioNumerator));
   end

   (* hide *)
   let _clkcntr <- mkClockCounter(uClock, rise, fall, wrap, clocked_by scemiClock, reset_by scemiReset);

   (* fire_when_enabled, no_implicit_conditions *)
   rule do_stall if (stall_pw);
      _clkcntr.stall();
   endrule

   return _clkcntr;
endmodule

function Bool checkAlignment(ClockCounter cc);
   return cc.preAlignment();
endfunction

module build_one_clock_group#( SceMiModuleState state
                             , List#(ClockPortInfo) clock_ports)
                             (SceMiClockGenIfc ifc);
   Clock scemiClock = getSceMiClock(state);
   Reset scemiReset = getSceMiReset(state);
   Clock uClock     = getUClock(state);

   Bool rising_uclock = state.rising_uclock;

   Integer clock_group_num = clock_ports[0].clockGroup;
   String clock_group_name = (clock_group_num == noClockGroup)
                           ? "default clock group"
                           : ("clock group " + integerToString(clock_group_num));

   List#(ClockControlInfo) clock_controllers = state.clkcntrls;
   List#(Tuple2#(Integer,MakeClockIfc#(Bit#(1)))) clkgens = state.clkgens;

   // Clock generation runs off a fast clock.  The uncontrolled clock
   // is a 2x divided version, created using the same flop mechanism
   // as the controlled clocks, to preserve alignment during FPGA
   // synthesis.

   // Each clock gets a counter, which counts from 0 to 2 *
   // ratioNumerator - 1 incrementing on scemiClock posedges in
   // which clocks are not stalled.  The clocks are aligned when each
   // of their counters is at 0.  The point of alignment is used to
   // time the deassertion of reset.  Each clock has a fixed counter
   // value at which it rises and a fixed counter value at which it
   // falls.  The counts are scaled by 2x to accomodate 1:1 clocks
   // with don't care edges.
   PulseWire stall_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   List#(ClockCounter) counters <- List::mapM(setupClockCounter(stall_pw,scemiClock,scemiReset,uClock),clock_ports);
   List#(Tuple2#(Integer,ClockCounter)) clock_counters = List::zip(List::map(getClockNum,clock_ports),counters);
   for (Integer i = 0; i < List::length(clock_counters); i = i + 1) begin
      let {n,counter} = clock_counters[i];
      case (List::lookup(n,clkgens)) matches
         tagged Invalid:   error("No clock generator found");
         tagged Valid .cg: begin
            rule tick_clock;
               if (counter.triggerPosedge())
                  cg.setClockValue(1);
               else if (counter.triggerNegedge())
                  cg.setClockValue(0);
            endrule
         end
      endcase
   end

   // Each clock has a reset cycles value which is the minimum number
   // of fast clock cycles the reset must be asserted.  Once reset has
   // been asserted for the maximum required by any clock, it is held
   // and deasserted one cycle before the next point of alignment.
   Bool pre_aligned = List::all(checkAlignment,counters);
   Integer max_reset_count = 2 * List::foldl(max,8,List::map(getResetCycles,clock_ports));
   messageM("\n(SCE-MI) Max reset count = " + integerToString(max_reset_count));
   Reg#(UInt#(32)) reset_counter <- mkReg(0,clocked_by scemiClock, reset_by scemiReset);
   PulseWire in_reset_pw <- mkPulseWire(clocked_by scemiClock, reset_by scemiReset);
   Reg#(Bool) out_of_reset_reg <- mkReg(True,clocked_by scemiClock, reset_by scemiReset);
   rule manage_reset;
      if (reset_counter < fromInteger(max_reset_count)) begin
         reset_counter <= reset_counter + 1;
         in_reset_pw.send();
         out_of_reset_reg <= False;
      end
      else if (!out_of_reset_reg) begin
         if (pre_aligned)
            out_of_reset_reg <= True;
         else
            in_reset_pw.send();
      end
   endrule

   // Stalling is controlled by checking the allow_posedge and
   // allow_negedge signals of the clock controllers against the
   // pre-posedge and pre-negedge signals from the clock counters,
   // ignoring don't care edges.
   //
   // The clock generating counters are in the scemiClock domain, but
   // the allow_posedge and allow_negedge signals as well as the
   // pre_posedge and pre_negedge signals need to be in the uclock
   // domain.  To work with these two clock domains, we need to
   // selectively cross some signals, track which domain each
   // computation is in, and compute stall signals independently for
   // the clock generator and the user.

   // constants -- unclocked
   List#(Tuple2#(Bool,Bool)) controllable = List::map(isControllableEdge,clock_ports);

   // pre-edge indicators from the counters, in both domains
   List#(Tuple2#(Bool,Bool)) at_edge_user  = List::map(isPreEdgeUser,counters);
   List#(Tuple2#(Bool,Bool)) at_edge_scemi = List::map(isPreEdgeSceMi,counters);

   ////let inspect_at_edge_user <- mkRL(at_edge_user);
   //let inspect_at_edge_scemi <- mkRL(at_edge_scemi);

   // allow-edge signals from the user, in both domains
   List#(Tuple2#(Bool,Bool)) is_allowed_user = List::map(isEdgeAllowed(clock_controllers),clock_ports);
   List#(ReadOnly#(Tuple2#(Bool,Bool))) is_allowed_sync <- List::mapM(mkDomainCross(scemiClock),is_allowed_user);
   List#(Tuple2#(Bool,Bool)) is_allowed_scemi = List::map(readReadOnly, is_allowed_sync);

   //let inspect_is_allowed_user <- mkRL(is_allowed_user);
   //let inspect_is_allowed_scemi <- mkRL(is_allowed_scemi);

   // list of controlled edges, in both domains
   List#(Tuple2#(Bool,Bool)) edges_user  = List::zipWith(andPair,controllable,at_edge_user);
   List#(Tuple2#(Bool,Bool)) edges_scemi = List::zipWith(andPair,controllable,at_edge_scemi);

   //let inspect_edges_user <- mkRLcc(edges_user, clocked_by uClock, reset_by noReset);
   //let inspect_edges_scemi <- mkRLcc(edges_scemi, clocked_by scemiClock, reset_by scemiReset);

   // stall indicator for the SCE-MI clock generator logic
   List#(Tuple2#(Bool,Bool)) stalls_scemi = List::zipWith(andNotPair,edges_scemi,is_allowed_scemi);
   Bool stall_scemi = List::\or (untuple(stalls_scemi));
   Reg#(Bool) stalled <- mkReg(False, clocked_by scemiClock, reset_by scemiReset);
   Bool start_stall    = stall_scemi && !stalled && rising_uclock;
   Bool continue_stall = stalled && (stall_scemi || !rising_uclock);
   Bool out_of_reset_pre = (reset_counter == fromInteger(max_reset_count)) && !out_of_reset_reg && pre_aligned;
   Bool after_reset = reset_counter != 0 && (out_of_reset_reg || out_of_reset_pre);

   //let inspect_stalls_scemi <- mkRL(stalls_scemi);
   //let inspect_stall_scemi <- mkR(stall_scemi);
   //let inspect_start_stall <- mkR(start_stall);
   //let inspect_continue_stall <- mkR(continue_stall);

   rule do_stall_scemi if (after_reset && (start_stall || continue_stall));
      stall_pw.send();
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule update_stalled if (after_reset && rising_uclock);
      stalled <= stall_scemi;
   endrule

   // stall indicator for the user pre-edge signals
   List#(Tuple2#(Bool,Bool)) stalls_user = List::zipWith(andNotPair,edges_user,is_allowed_user);
   Bool stall_user = List::\or (untuple(stalls_user));

   // We need to set the pre-posedge and pre-negedge signals for each
   // clock control.  We only assert the signal if the edge is
   // controlled, the pre-edge signal from the counter is asserted,
   // and we are not stalling.
   rule do_ticks if (!stall_user);
      List#(Action) ticks = List::zipWith(tick(clock_controllers),edges_user,clock_ports);
      List::joinActions(ticks);
   endrule

   // interface methods for communicating clockgen status

   method Bool outOfReset();
      return !in_reset_pw;
   endmethod

   method Bool isClockAdvancing();
      return !stall_pw;
   endmethod

endmodule: build_one_clock_group


function Tuple2#(Bool,Bool) isControllableEdge(ClockPortInfo p);
   Bool dont_care_posedge = p.dutyLo == 0;
   Bool dont_care_negedge = p.dutyHi == 0;
   return tuple2(!dont_care_posedge,!dont_care_negedge);
endfunction

function Tuple2#(Bool,Bool) isPreEdgeUser(ClockCounter c);
   return tuple2(c.prePosedgeUser(),c.preNegedgeUser());
endfunction

function Tuple2#(Bool,Bool) isPreEdgeSceMi(ClockCounter c);
   return tuple2(c.prePosedge(),c.preNegedge());
endfunction

function Bool testPosEn(ClockControlInfo ctrl);
   return ctrl.ens.allow_pos();
endfunction

function Bool testNegEn(ClockControlInfo ctrl);
   return ctrl.ens.allow_neg();
endfunction

function Tuple2#(Bool,Bool) isEdgeAllowed( List#(ClockControlInfo) clock_controllers
                                         , ClockPortInfo p
                                         );
   List#(ClockControlInfo) cntrls = List::filter(isForClock(p.clockNum),clock_controllers);
   List#(Bool) allow_pos = List::map(testPosEn, cntrls);
   List#(Bool) allow_neg = List::map(testNegEn, cntrls);
   return tuple2(List::\and (allow_pos), List::\and (allow_neg));
endfunction

module mkDomainCross(Clock dClk, a x, ReadOnly#(a) ifc)
   provisos(Bits#(a,sa));

   ReadOnly#(a) _ifc;
   if (isStaticIndex(pack(x))) begin
      // no clock -- can't use null crossing wire
      _ifc = (interface ReadOnly;
                 method _read = x;
              endinterface);
   end
   else begin
      // for non-static values we must use a null crossing wire
      (*hide*)
      _ifc <- mkNullCrossingWire(dClk, x);
   end
   return _ifc;
endmodule

function Tuple2#(Bool,Bool) andPair(Tuple2#(Bool,Bool) x, Tuple2#(Bool,Bool) y);
   let {x1,x2} = x;
   let {y1,y2} = y;
   return tuple2(x1 && y1, x2 && y2);
endfunction

function Tuple2#(Bool,Bool) andNotPair(Tuple2#(Bool,Bool) x, Tuple2#(Bool,Bool) y);
   let {x1,x2} = x;
   let {y1,y2} = y;
   return tuple2(x1 && !y1, x2 && !y2);
endfunction

function List#(a) interleave(List#(a) xs, List#(a) ys);
   if (List::isNull(xs))
      return ys;
   else if (List::isNull(ys))
      return xs;
   else return List::cons(List::head(xs),interleave(ys,List::tail(xs)));
endfunction

function List#(Bool) untuple(List#(Tuple2#(Bool,Bool)) l);
   let {xs,ys} = List::unzip(l);
   return interleave(xs,ys);
endfunction

function Action tick(List#(ClockControlInfo) all_ctrls, Tuple2#(Bool,Bool) edges, ClockPortInfo p);
   Action act = noAction;
   if (tpl_1(edges) || tpl_2(edges)) begin
      Integer n = p.clockNum;
      List#(ClockControlInfo) ctrls = List::filter(isForClock(p.clockNum),all_ctrls);
      Bool value = tpl_1(edges);
      List#(Action) acts = List::map(callTick(value),ctrls);
      act = List::joinActions(acts);
   end
   return act;
endfunction

function Action callTick(Bool v, ClockControlInfo ctrl);
   action
      ctrl.ens.tick(v);
   endaction
endfunction

// reduce ratios to lowest terms
function ClockPortInfo simplifyRatios(ClockPortInfo p);
   ClockPortInfo new_port = p;

   // handle clock ratio
   Integer x = gcd(p.ratioNumerator, p.ratioDenominator);
   new_port.ratioNumerator   = quot(p.ratioNumerator,x);
   new_port.ratioDenominator = quot(p.ratioDenominator,x);

   // handle duty cycle and phase
   Integer x0 = gcd(p.dutyHi, p.dutyLo);
   Integer x1 = gcd(x0, p.phase);
   Integer x2 = gcd(x1, p.dutyHi + p.dutyLo);
   new_port.dutyHi = quot(p.dutyHi,x2);
   new_port.dutyLo = quot(p.dutyLo,x2);
   new_port.phase = quot(p.phase,x2);

   // At this point, we don't want to allow dutyHi + dutyLo == 1, because
   // it may preclude finding an integral value for one of the edges.
   // We will ensure that dutyHi + dutyLo is always at least 2 for now,
   // and then deal with don't care edges at a 1:1 ratio once we
   // have adjusted all of the clocks.
   if ((new_port.dutyHi + new_port.dutyLo) == 1) begin
      new_port.dutyHi = new_port.dutyHi * 2;
      new_port.dutyLo = new_port.dutyLo * 2;
      new_port.phase  = new_port.phase * 2;
   end

   return new_port;
endfunction: simplifyRatios

function Integer checkClockPortError(List#(ClockPortInfo)port_infos, ClockPortInfo p);
   Integer found_error = 0;

   if (p.ratioNumerator < p.ratioDenominator)
   begin
      String s = "RatioNumerator cannot be less than RatioDenominator for a SceMiClockPort with ClockNum " + integerToString(p.clockNum) + ".";
      found_error = found_error + 1;
      found_error = message(s, found_error);
   end

   if (p.phase >= (p.dutyHi + p.dutyLo))
   begin
      String s = "Phase cannot be greater than or equal to the sum of DutyHi and DutyLo for a SceMiClockPort with ClockNum " + integerToString(p.clockNum) + ".";
      found_error = found_error + 1;
      found_error = message(s, found_error);
   end

   if (p.dutyHi == 0 && p.dutyLo == 0)
   begin
      String s = "Both DutyHi and DutyLo cannot be equal to zero for SceMiClockPort with ClockNum " + integerToString(p.clockNum) + ".";
      found_error = found_error + 1;
      found_error = message(s, found_error);
   end

   List#(ClockPortInfo) ports = filter(isForClockPort(p.clockNum), port_infos);
   if (length(ports) > 1)
   begin
      String s = "More than one SceMiClockPort with ClockNum " + integerToString(p.clockNum) + ".";
      found_error = found_error + 1;
      found_error = message(s, found_error);
   end

   return found_error;

endfunction: checkClockPortError

function Integer checkClockControlError(List#(ClockPortInfo)port_infos, ClockControlInfo ctrl);
   Integer found_error = 0;
   List#(ClockPortInfo) ports = filter(isForClockPort(ctrl.clockNum), port_infos);
   if (isNull(ports))
   begin
      String s = "No SceMiClockPort found for a SceMiClockControl with ClockNum " + integerToString(ctrl.clockNum) + ".";
      found_error = found_error + 1;
      found_error = message(s, found_error);
   end
   return found_error;
endfunction: checkClockControlError

function Integer findDenomLCM(Integer x, ClockPortInfo p);
   Integer x0 = p.ratioDenominator * (p.dutyHi + p.dutyLo);
   return lcm(x,x0);
endfunction: findDenomLCM

function Integer findEdgeGCD(ClockPortInfo p);
   Integer rise = p.phase + p.ratioNumerator;
   Integer hi = (p.dutyHi == 0 || p.dutyLo == 0) ? (p.ratioNumerator / 2) : p.dutyHi;
   Integer fall = p.phase + hi;
   if (p.dutyHi == 0)      // don't care negedge
      return rise;
   else if (p.dutyLo == 0) // don't care posedge
      return fall;
   else                    // both edges controlled
      return gcd(rise,fall);
endfunction: findEdgeGCD

function Integer findPeriodLCM(Integer x, ClockPortInfo p);
   return lcm(x, p.dutyHi + p.dutyLo);
endfunction: findPeriodLCM

function Bool canDivideBy2(ClockPortInfo p);
   Bool ok = False;
   if (p.ratioNumerator == 2 &&
       p.ratioDenominator == 2 &&
       (p.dutyHi == 0 || p.dutyLo == 0))
      ok = True;
   else if (p.ratioNumerator % 2 == 0 &&
            p.ratioDenominator % 2 == 0 &&
            p.dutyHi % 2  == 0 &&
            p.dutyLo % 2  == 0 &&
            p.phase % 2 == 0)
      ok = True;
   return ok;
endfunction

// Scale up clock ratios to guarantee that all clock events
// occur at integral times, and convert values to the
// same controlled time scale.
function ClockPortInfo scaleClockRatio(Integer base, ClockPortInfo p);
   ClockPortInfo new_port = p;

   Integer period = base * p.ratioNumerator / p.ratioDenominator;

   // scale clock ratio
   new_port.ratioNumerator   = period;
   new_port.ratioDenominator = base;

   // scale duty cycle and phase
   new_port.dutyHi = period * p.dutyHi / (p.dutyHi + p.dutyLo);
   new_port.dutyLo = period * p.dutyLo / (p.dutyHi + p.dutyLo);
   new_port.phase  = period * p.phase  / (p.dutyHi + p.dutyLo);

   // scale reset cycles
   new_port.resetCycles = period * p.resetCycles;

   // detect when scaling has lost information about a phase shift
   if (new_port.phase == 0 && p.phase != 0)
      new_port.inverted = True;

   return new_port;
endfunction: scaleClockRatio

function Action assertCReset(MakeResetIfc rstgen);
   action
      rstgen.assertReset();
   endaction
endfunction: assertCReset

// Implementation of mkSceMiClockPort using the SceMiModule monad

module [SceMiModule] mkSceMiBSVClockPort#( parameter Integer clockNum
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

   // Extract state from module context
   let state <- getContext();

   // Check if there is already domain info for this clockNum.
   // If there is, use it.  Otherwise create clock and reset generators
   // for this domain and add them to the module context.
   Clock cClock = noClock;
   Reset cReset = noReset;
   Maybe#(DomainInfo) cdomain = lookup(clockNum, state.cdomains);
   if (cdomain matches tagged Valid .dom)
   begin
      // This domain has already been defined.  We just want to clone
      // it here.
      cClock = dom.clk;
      cReset = dom.rst;

      if (dom.external)
      begin
         warningM("Clock number "+ integerToString(clockNum) +
                  " is defined both internally and externally.");
      end
      else
      begin
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
   end
   else
   begin
      // This is the first definition of this domain.

      Clock scemiClock = getSceMiClock(state);
      Reset scemiReset = getSceMiReset(state);

      // Internally instantiate a clock generation module
      Bool reset_val = !((dutyHi == 0 || phase < dutyLo) && (dutyLo != 0));
      MakeClockIfc#(Bit#(1)) clkgen <- mkUngatedClock( pack(reset_val)
                                                     , clocked_by scemiClock
                                                     , reset_by   scemiReset );
      cClock = clkgen.new_clk;

      // Create a reset signal synchronized to the new clock
      MakeResetIfc rstgen <- genSceMiReset( cClock
                                          , clocked_by scemiClock
                                          , reset_by scemiReset );
      if (isXilinxSceMiLinkType(link_type))
         cReset <- mkResetBUFG(clocked_by cClock, reset_by rstgen.new_rst());
      else
         cReset = rstgen.new_rst;

      // Create new domain info to be used by any later calls
      DomainInfo dinfo;
      dinfo.clk = cClock;
      dinfo.rst = cReset;
      dinfo.external = False;
      state.cdomains = List::cons(tuple2(clockNum,dinfo), state.cdomains);

      // Add clock generator to state
      state.clkgens = List::cons(tuple2(clockNum,clkgen), state.clkgens);

      // Add reset generator to state
      state.rstgens = List::cons(rstgen, state.rstgens);

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
      port.inverted         = False;

      state.clkports = List::cons(port, state.clkports);

      // Update the context with the augmented state
      putContext(state);
   end

   // This is the user-level interface
   interface Clock cclock = cClock;
   interface Reset creset = cReset;

endmodule: mkSceMiBSVClockPort


// Implementation of mkSceMiClockControl using the SceMiModule monad

module [SceMiModule] mkSceMiBSVClockControl#(parameter Integer clockNum,
                                             Bool allow_pos_edge,
                                             Bool allow_neg_edge)
                                            (SceMiClockControlIfc);

   // Extract state from module context
   let state <- getContext();

   // Get the uClock and uReset from the module context.
   Clock uClock = noClock;
   Reset uReset = noReset;
   if (state.udomain matches tagged Valid .dom)
   begin
      uClock = dom.clk;
      uReset = dom.rst;
   end
   else
   begin
      // Something is wrong.  This should have been provided by buildSceMiBSV
      error("Uncontrolled domain info is missing!");
   end

   Probe#(Bool) allowPos <- mkProbe(clocked_by uClock, reset_by noReset);
   Probe#(Bool) allowNeg <- mkProbe (clocked_by uClock, reset_by noReset);
   (*hide_all*)
   let pconnect <- mkConnection (allowPos._write, allow_pos_edge);
   (*hide_all*)
   let nconnect <- mkConnection (allowNeg._write, allow_neg_edge);

   
   // Instantiate wires for back-channel communication.
   // These have noReset because they are used by the clock- and
   // reset-generating logic.
   PulseWire pre_pos_pw <- mkPulseWire(clocked_by uClock, reset_by noReset);
   PulseWire pre_neg_pw <- mkPulseWire(clocked_by uClock, reset_by noReset);

   // build internal data for back-channel communication
   ClockControlInfo cntrl;
   cntrl.clockNum = clockNum;
   cntrl.ens = interface ClkEnableIfc;
                  method Bool allow_pos = allow_pos_edge;
                  method Bool allow_neg = allow_neg_edge;
                  method Action tick(Bool value);
                     if (value)
                        pre_pos_pw.send();
                     else
                        pre_neg_pw.send();
                  endmethod
               endinterface;
   state.clkcntrls = List::cons(cntrl, state.clkcntrls);

   // Update the context with the augmented state
   putContext(state);

   // This is a little kludge to set the reset associated with
   // the exported methods to use the uReset even though the
   // PulseWires themselves are on noReset.
   Wire#(Bool) _force_my_reset <- mkDWire(True, clocked_by uClock, reset_by uReset);

   // This is the user-level interface

   interface Clock uclock = uClock;
   interface Reset ureset = uReset;

   method Bool pre_posedge() if (_force_my_reset);
      return pre_pos_pw;
   endmethod

   method Bool pre_negedge() if (_force_my_reset);
      return pre_neg_pw;
   endmethod

endmodule: mkSceMiBSVClockControl

endpackage: SceMiClocks
