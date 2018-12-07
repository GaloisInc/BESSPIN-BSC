package PowerContext;

import Contexts::*;
import FIFO::*;
import Vector::*;
import List::*;
import GetPut::*;
import HList::*;
import SceMiSerialProbe::*;
import PowerManagement::*;

// The ClockContext contains a slot for each distinct clock in the design.  By
// convention, element 0 is the uclock, and element 1 is the fastest cclock
// (which has a clocking edge for each cycleStamp).  Next come any other
// cclocks (i.e. clocks supplied from outside the design).  The remaining
// slots are for gated versions of any of the cclocks.

// TODO: think about whether to reallocate "gated" slots in separate areas of
// the design.

// The PowerContext contains information about each power island and functional block.
// In general there are more blocks than islands.  Each island draws its power either from
// an external power supply, or from the output of a switch.  Each block is
// placed into an island; this association is configurable at the start of
// an emulation run (i.e. without resynthesis), but the dut itself is not
// supposed to alter this association.  (If this sort of flexibility is
// required, extra switches -- which the dut can control -- must be used, to
// provide extra islands.)

// The PowerContext has two components for information supplied from outside
// the source of the design, for static (unchanging) and dynamic information
// respectively.  There is also a third component, the PMContext (power
// management context) for communication of power management information
// within the design and explicitly manipulated by the design.

typedef 12 BLOCKVECTORSIZE;  // number of distinct functional blocks
typedef  4 ISLANDVECTORSIZE; // number of distinct power islands


typedef struct {UInt#(TLog#( BLOCKVECTORSIZE)) blockIx;} BlockIx
   deriving (Bits, Eq, Literal);
typedef struct {UInt#(TLog#(ISLANDVECTORSIZE)) powerIx;} PowerIx
   deriving (Bits, Eq, Literal);
typedef struct {UInt#(TLog#(CLOCKCONTEXTSIZE)) clockIx;} ClockIx
   deriving (Bits, Eq, Literal);

typedef struct {
   Clock   clk;
   Reset   rst;
   ClockIx ix;
		} ClockResetAndIndex;

typedef struct {
   UInt#(20) freq; // units KHz
		} ClockFreq deriving (Bits, Eq, Ord, Literal);

typedef struct {
   UInt#(16) voltage; // units mV
   ClockFreq maxFreq;
		} PowerVoltage deriving (Bits, Eq);

typedef struct {
   Bool state; // for the time being (might need "partial on" later)
		} PowerState deriving (Bits, Eq);

typedef struct {
   Bool running;
		} ClockState deriving (Bits, Eq);

// The static part of the PowerContext:

typedef struct {
   // static items:
   Vector#(CLOCKCONTEXTSIZE, ClockFreq)      clockFreqV; // frequency of each clock
   Vector#(ISLANDVECTORSIZE, PowerVoltage)    voltageV;
		} StaticPowerContext deriving (Eq);

// The dynamic part of the PowerContext:

typedef struct {
   // dynamic items, all in the uclock domain:
   Wire#(ClockIx) defaultClockIndex;
   Wire#(PowerIx) defaultPower;
   Vector#(BLOCKVECTORSIZE,  Wire#(PowerIx))    islandSourceV;
   Vector#(ISLANDVECTORSIZE, Wire#(PowerState)) powerStateInitialV;
   Vector#(ISLANDVECTORSIZE, Wire#(PowerState)) powerStateV;
   Vector#(CLOCKCONTEXTSIZE, Wire#(ClockState)) clockStateInitialV;
   Vector#(CLOCKCONTEXTSIZE, Wire#(ClockState)) clockStateV;
 	} DynamicPowerContext;

module mkInitialDynamicPowerContextWithClocks#(Vector#(CLOCKCONTEXTSIZE, Clock) cks,
					       Vector#(CLOCKCONTEXTSIZE, Reset) rs)
   (DynamicPowerContext);

   let uclock = cks[0];
   let ureset =  rs[0];

   let v1 <- replicateM(mkBypassWire, clocked_by uclock, reset_by ureset);
   let v2 <- replicateM(mkBypassWire, clocked_by uclock, reset_by ureset);

   DynamicPowerContext res = ?;
   res.islandSourceV <- replicateM(mkBypassWire, clocked_by uclock, reset_by ureset);
   res.defaultClockIndex <- mkBypassWire(clocked_by uclock, reset_by ureset);
   res.defaultPower <- mkBypassWire(clocked_by uclock, reset_by ureset);
   res.powerStateInitialV = v1;
   res.powerStateV   = v1;
   res.clockStateInitialV = v2;
   res.clockStateV   = v2;

   return res;
endmodule

// The complete PowerContext:

typedef struct {
   StaticPowerContext        staticC;
   DynamicPowerContext       dynamicC;
		} PowerContext;

module mkInitialPowerContextWithClocks#(StaticPowerContext initStaticPowerContext,
					Vector#(CLOCKCONTEXTSIZE, Clock) cks,
					Vector#(CLOCKCONTEXTSIZE, Reset) rs)
   (PowerContext);

   PowerContext res;
   res.staticC   = initStaticPowerContext;
   res.dynamicC <- mkInitialDynamicPowerContextWithClocks(cks, rs);
   return res;
endmodule

// the CompletePowerContexts includes the PMContext:
typedef HCons#(PMContext, HCons#(PowerContext, CompleteProbeContexts)) CompletePowerContexts;
typedef ModuleContext#(CompletePowerContexts) PowerModule;

module setSourceIslandAssociation#(PowerContext pc,
				   Vector#(BLOCKVECTORSIZE, PowerIx) assocV)
                                  (Empty);
   (*no_implicit_conditions, fire_when_enabled*)
   rule writeAssocWires;
      writeVReg(pc.dynamicC.islandSourceV, assocV);
   endrule
endmodule

// The boilerplate for the Longfellow Bridge:

interface PowerContextIfc;
   // all in the uclock domain:
   (*always_enabled*)
   interface Wire#(ClockIx) defaultClockIndexW;
   (*always_enabled*)
   interface Wire#(PowerIx) defaultPowerW;
   (*always_enabled*)
   interface Vector#(BLOCKVECTORSIZE, Wire#(PowerIx)) islandSourceWV;
   (*always_enabled*)
   interface Vector#(ISLANDVECTORSIZE, Wire#(PowerState)) powerStateInitialWV;
   (*always_ready*)
   interface Vector#(ISLANDVECTORSIZE, PowerState) powerStateFinalV;
   (*always_enabled*)
   interface Vector#(CLOCKCONTEXTSIZE, Wire#(ClockState)) clockStateInitialWV;
   (*always_ready*)
   interface Vector#(CLOCKCONTEXTSIZE, ClockState) clockStateFinalV;
endinterface

instance Expose#(PowerContext, PowerContextIfc, CLOCKCONTEXTSIZE);
   module unburyContextWithClocks#(PowerContext pc, ClockContext cc)(PowerContextIfc);
      let dc = pc.dynamicC;
      let uclock = cc.clks[0];
      let ureset = cc.rsts[0];

      function PowerState fp(Wire#(PowerState) w) = w._read;
      function ClockState fc(Wire#(ClockState) w) = w._read;

      interface defaultClockIndexW = dc.defaultClockIndex;
      interface defaultPowerW = dc.defaultPower;
      interface islandSourceWV = dc.islandSourceV;
      interface powerStateInitialWV = dc.powerStateInitialV;
      interface powerStateFinalV = map(fp, dc.powerStateV);
      interface clockStateInitialWV = dc.clockStateInitialV;
      interface clockStateFinalV = map(fc, dc.clockStateV);
   endmodule
endinstance

instance Hide#(m, PowerContextIfc)
   provisos (IsModule#(m,a), Context#(m, PowerContext), Context#(m, ClockContext));

   module [m] reburyContext#(PowerContextIfc i)(Empty);
      PowerContext pc <- getContext();
      ClockContext cc <- getContext();
      let c = pc.dynamicC;
      let uclock = cc.clks[0];
      let ureset = cc.rsts[0];

      (*no_implicit_conditions, fire_when_enabled*)
      rule xmit_defaultClockIndex;
	 i.defaultClockIndexW <= c.defaultClockIndex;
      endrule

      (*no_implicit_conditions, fire_when_enabled*)
      rule xmit_defaultPower;
	 i.defaultPowerW <= c.defaultPower;
      endrule

      Rules rs = emptyRules;
      for (Integer j=0; j<valueof(ISLANDVECTORSIZE); j=j+1) begin
	 Wire#(PowerState) w <- mkBypassWire(clocked_by uclock, reset_by ureset);
	 rs = rJoin(rs, (
	    rules
	       (*no_implicit_conditions, fire_when_enabled*)
	       rule xmit_islandSource;
		  i.islandSourceWV[j] <= c.islandSourceV[j];
	       endrule
	       rule xmit_initialPowerState;
		  i.powerStateInitialWV[j] <= c.powerStateV[j];
	       endrule
	       rule xmit_finalPowerState;
		  w <= i.powerStateFinalV[j];
	       endrule
	    endrules
			 ));
	 pc.dynamicC.powerStateV[j] = w;
      end
      addRules(rs);

      Rules rs2 = emptyRules;
      for (Integer j=0; j<valueof(CLOCKCONTEXTSIZE); j=j+1) begin
	 Wire#(ClockState) w <- mkBypassWire(clocked_by uclock, reset_by ureset);
	 rs2 = rJoin(rs2, (
	    rules
	       (*no_implicit_conditions, fire_when_enabled*)
	       rule xmit_initialClockState;
		  i.clockStateInitialWV[j] <= c.clockStateV[j];
	       endrule
	       rule xmit_finalClockState;
		  w <= i.clockStateFinalV[j];
	       endrule
	    endrules
			 ));
	 pc.dynamicC.clockStateV[j] = w;
      end
      addRules(rs2);
      putContext(pc);
   endmodule
endinstance

endpackage
