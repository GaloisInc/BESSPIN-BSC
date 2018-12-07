package PowerPrimitives;

import Contexts::*;
import FIFOF::*;
import FIFO::*;
import Vector::*;
import List::*;
import GetPut::*;
import Connectable::*;
import TLM2::*;
import ClientServer::*;
import Clocks::*;
import SceMiSerialProbe::*;
import SceMiScan::*;
import CommitIfc::*;
import UnitAppendList::*;
import HList::*;

import PowerContext::*;
import PowerManagement::*;

// Identifier of an isolator:
typedef UInt#(8) IsolatorId;
// Identifier of a cycle counter:
typedef UInt#(8) CounterId;

// power messages etc.

typedef 16 CTRSIZE;

typedef struct {
   Bool      valid;
   ClockIx   clock;
   PowerIx   pd;
   UInt#(TSub#(31, TAdd#(SizeOf#(ClockIx), SizeOf#(PowerIx)))) dta;
		} CycleCountStruct deriving (Bits);

typedef struct {
   PowerIx   pd;
   Bool switchState;
   Bool valid;
		} PowerSwitchStruct deriving (Bits);

typedef struct {
   PowerIx extnl;
   PowerIx intnl;
   Bool    tog;
   Bool valid;
		} IsoFailureStruct deriving (Bits);

// A module interface at a power-island boundary comes with a PowerInfo
// method, for use in verification:

typedef struct {
   PowerIx extPower;
   PowerIx intPower;
   ClockIx extClock;
   ClockIx intClock;
		} PowerInfo;

interface PoweredMod#(type ifc_t);
   interface ifc_t ifc;
   method PowerInfo pow;
endinterface

typeclass IsOn#(type m, type t);
   module [m] isOn#(t x)(Bool);
endtypeclass

instance IsOn#(m, PowerIx)
   provisos (IsModule#(m,a),
	     Context#(m, PowerContext), Context#(m, ClockContext));
   module [m] isOn#(PowerIx x)(Bool);
      PowerContext pc <- getContext();
      ClockContext cc <- getContext();
      let c = pc.dynamicC;
      let clk <- exposeCurrentClock();
      let ioc <- mkNullCrossingReg(clk, False,
				   clocked_by cc.clks[0], reset_by cc.rsts[0]);
      rule writeIoc;
	 ioc <= c.powerStateV[x.powerIx].state;
      endrule
      return ioc.crossed;
   endmodule
endinstance

instance IsOn#(m, PoweredMod#(ifc))
   provisos (IsModule#(m,a),
	     Context#(m, PowerContext), Context#(m, ClockContext));
   module [m] isOn#(PoweredMod#(ifc) x)(Bool);
      PowerContext pc <- getContext();
      let c = pc.dynamicC;
      let io <- isOn(x.pow.intPower);
      return io;
   endmodule
endinstance

// Instantiate a powered block, with specified clock and reset:

module [m] mkWithPowerAndClock#(BlockIx blockIx, ClockResetAndIndex cri, m#(ifc_t) mkM)(PoweredMod#(ifc_t))
   provisos (IsModule#(m,a), Context#(m, PowerContext), Context#(m, ClockContext));

   PowerContext pc <- getContext();
   ClockContext cc <- getContext();
   let c = pc.dynamicC;
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   PowerIx powerIx = c.islandSourceV[blockIx.blockIx];

   Wire#(PowerIx) wp <- mkBypassWire(clocked_by uclock, reset_by ureset);
   Wire#(ClockIx) wc <- mkBypassWire(clocked_by uclock, reset_by ureset);
   Wire#(PowerIx) wp0 = c.defaultPower;
   Wire#(ClockIx) wc0 = c.defaultClockIndex;
   pc.dynamicC.defaultPower = wp;
   pc.dynamicC.defaultClockIndex = wc;
   let pi = PowerInfo {extPower: c.defaultPower, intPower: powerIx,
		       extClock: c.defaultClockIndex, intClock: cri.ix };

   // Pass new defaults on to subsequent (interior) instantiations:

   (*no_implicit_conditions, fire_when_enabled*)
   rule xmit_powerIx;
      wp <= powerIx;
   endrule

   (*no_implicit_conditions, fire_when_enabled*)
   rule xmit_clockIx;
      wc <= cri.ix;
   endrule

   putContext(pc);

   // Instantiate the parameter module:

   ifc_t _ifc <- mkM(clocked_by cri.clk, reset_by cri.rst);

   // Restore the wires with the original defaults:

   pc <- getContext();
   pc.dynamicC.defaultPower = wp0;
   pc.dynamicC.defaultClockIndex = wc0;
   putContext(pc);

   // The parameter module's interface:
   interface ifc = _ifc;
   // The PowerInfo information:
   method pow = pi;
endmodule

// Run a module (mkM) with power management facilities:

module [ProbeModule] runWithPower#(PowerModule#(i) mkM,
				   StaticPowerContext initSPC)(i);
   ProbeContext pr <- getContext();
   ClockContext cc <- getContext();
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   Vector#(BLOCKVECTORSIZE, Reg#(PowerIx)) assocVR;
   for (Integer i=0; i<valueof(BLOCKVECTORSIZE); i=i+1)
      assocVR[i] <- mkReg(0, clocked_by uclock, reset_by ureset);

   let initPM <- mkInitialPMContext();
   let initP <- mkInitialPowerContextWithClocks(initSPC, cc.clks, cc.rsts);
   setSourceIslandAssociation(initP, readVReg(assocVR));
   let c_ifc <- runWithContexts(hList2(initPM, initP), mkM);
   match {.pcs, .ifc} = c_ifc;
   let pmc = hHead(pcs);
   let c   = hHead(hTail(pcs));

   // Tie off pmc:
   let pm <- unburyContext(pmc);
   mkConnection(pm, nullPMIfc);

   // Handle c (in uclock domain):

   // Record and report power switching changes:
   let initRegBuff = PowerSwitchStruct {pd: ?, switchState: ?, valid: False };
   Reg#(PowerSwitchStruct) regBuff <- mkReg(initRegBuff,
					    clocked_by uclock, reset_by ureset);
   FIFOF#(PowerSwitchStruct) fifoBuff <- mkFIFOF(clocked_by uclock, reset_by ureset);
   let staller <- mkStaller(fifoBuff.notEmpty);

   let prb <- mkSerialProbe1(tagged Power (tagged ProbeNumber
				PowerStruct {kind: CHANGES,
					     num:  0}),
			     "power_changes", True, regBuff,
			     clocked_by uclock, reset_by ureset);

   Reg#(Bool) delayReg <- mkReg(True, clocked_by uclock, reset_by ureset);

   rule updateDelayReg (delayReg);
      delayReg <= False;
   endrule

   rule feedRegBuff (!(prb.delay || delayReg));
      let x = fifoBuff.first;
      fifoBuff.deq;
      regBuff <= x;
      delayReg <= True;
   endrule

   let c_i   <- unburyContextWithClocks(c, cc);

   // Initialize dynamic values (all in uclock domain):

   (*fire_when_enabled*)
   rule setAssoc(pr.enabler matches {.n, .*} &&&
		      n matches tagged Power .ps &&&
		 ps matches tagged Assoc .as);
      let j = as.block;
      if (j<fromInteger(valueof(BLOCKVECTORSIZE)))
	 assocVR[as.block] <= PowerIx{powerIx: truncate(as.source)};
   endrule

   (*no_implicit_conditions, fire_when_enabled*)
   rule init_defaultClockIx;
      c_i.defaultClockIndexW <= 1;
   endrule
   (*no_implicit_conditions, fire_when_enabled*)
   rule init_defaultPowerIx;
      c_i.defaultPowerW <= 0;
   endrule
   Vector#(ISLANDVECTORSIZE, PowerState) initPSV = replicate(PowerState {state: False});
   initPSV[0].state = True;
   for (Integer j=0; j<valueof(ISLANDVECTORSIZE); j=j+1)
      (*no_implicit_conditions, fire_when_enabled*)
      rule init_InitialPState;
	 c_i.powerStateInitialWV[j] <= initPSV[j];
      endrule
   Vector#(CLOCKCONTEXTSIZE, ClockState) initCSV = replicate(ClockState {running: True});
   for (Integer j=0; j<valueof(CLOCKCONTEXTSIZE); j=j+1)
      (*no_implicit_conditions, fire_when_enabled*)
      rule init_InitialCState;
	 c_i.clockStateInitialWV[j] <= initCSV[j];
      endrule

   // Track and report dynamic power switching changes:
   Vector#(ISLANDVECTORSIZE, Reg#(PowerState)) psRV <-
                       replicateM(mkReg(PowerState {state: False},
					clocked_by uclock, reset_by ureset));
   for (Integer j=0; j<valueof(ISLANDVECTORSIZE); j=j+1)
      rule track_it (c_i.powerStateFinalV[j] != psRV[j]);
         psRV[j] <= c_i.powerStateFinalV[j];
	 fifoBuff.enq(PowerSwitchStruct
		     {pd: fromInteger(j),
		      switchState: c_i.powerStateFinalV[j].state,
		      valid: True });
      endrule

   return ifc;
endmodule

// ISOLATORS

// Each type of interface requires its own type of isolator.  So we group them
// into a typeclass:

typeclass Isolable#(type i, type c, type m)
   dependencies ((i,m) determines c);
   module [m] mkIso#(PMAddr adr, PoweredMod#(i) arg, c clamp_v, Bool init)(i)
      provisos (IsModule#(m,a),
		Context#(m, PowerContext), Context#(m, PMContext),
		Context#(m, ProbeContext), Context#(m, ClockContext));
endtypeclass

// The clamp_v value is the value to which an output is clamped when isolation
// is on; "on" is a dynamic argument to switch isolation on and off.

// TODO: clamp_v should be a tagged union type, also allowing the signal to be
// clamped to its present value, instead of specifying a specific value.

// Subinterfaces are in the IsolableSub typeclass; they are also in the
// Isolable typeclass (which does more verification):

instance Isolable#(i, cl, m)
   provisos (IsolableSub#(i, cl),
	     IsModule#(m,a),
	     Context#(m, PowerContext),
	     Context#(m, PMContext),
	     Context#(m, ProbeContext),
	     Context#(m, ClockContext));

   module [m] mkIso#(PMAddr adr, PoweredMod#(i) arg, cl clamp_v, Bool init)(i);
      // Receive power management signal:
      Reg#(Bool) on <- mkReg(init);
      PMIfc pmi = (
	 interface PMIfc;
	    interface Put sendIfc;
	       method Action put(x);
		  match {.addr, .data, .isFrom} = x;
		  if (addr==adr && !isFrom) on <= data;
	       endmethod
	    endinterface
	    interface recvIfc = nullRecvIfc;
	 endinterface
		   );
      PMContext pmc <- getContext;
      pmc = List::cons(pmi, pmc);
      putContext(pmc);

      let p = arg.pow;

      PowerContext pc <- getContext();
      let dc = pc.dynamicC;
      ClockContext cc <- getContext();
      let uclock = cc.clks[0];
      let ureset = cc.rsts[0];
      let cclock <- exposeCurrentClock();

      // Report if the internal power is switched off while the external power
      // remains on, unless isolation has been properly applied:

      // The dynamic values must be registered, to enable crossing into the
      // cclock domain:
      CrossingReg#(PowerIx) extPowerR <- mkNullCrossingRegU(cclock,
	 clocked_by uclock, reset_by ureset);
      CrossingReg#(PowerIx) intPowerR <- mkNullCrossingRegU(cclock,
	 clocked_by uclock, reset_by ureset);
      CrossingReg#(Bool) bad <- mkNullCrossingReg(cclock, False,
	 clocked_by uclock, reset_by ureset);

      (*fire_when_enabled*)
      rule setExtPowerR;
	 extPowerR <= p.extPower;
      endrule
      (*fire_when_enabled*)
      rule setIntPowerR;
	 intPowerR <= p.intPower;
      endrule
      (*fire_when_enabled*)
      rule setBad;
	 bad <= (!dc.powerStateV[p.intPower.powerIx].state &&
		 dc.powerStateV[p.extPower.powerIx].state);
      endrule
      // A flag to ensure each error is reported just once:
      Reg#(Bool) reported <- mkReg(False);
      Reg#(Bool) toggle   <- mkReg(False);
      Reg#(Bool) validr   <- mkReg(False);
      // report power-off without iso:
      let s = IsoFailureStruct {
				extnl:extPowerR.crossed,
				intnl: intPowerR.crossed,
				tog:   toggle,
				valid: validr };

      let id  <- newIsoId();
      let prb <- mkSerialProbe1(tagged Power (tagged ProbeNumber
				   PowerStruct {kind: ISO,
						num: id}),
				"isolator_failure", True, s);

      rule report_iso_failure ( bad.crossed && !on && !reported);
	 validr   <= True;
	 reported <= True;
	 toggle   <= !toggle;
      endrule

      // Reset the "reported" flag when the error condition goes away:
      rule resetReport (reported && !(bad.crossed && !on));
	 reported <= False;
      endrule

      i _ifc <- mkIsoS(arg.ifc, clamp_v, on);
      return _ifc;
   endmodule
endinstance

// The IsolableSub typeclass:

typeclass IsolableSub#(type i, type c)
   dependencies (i determines c);
   module mkIsoS#(i arg, c clamp_v, Bool on)(i);
endtypeclass

// A tuple of subinterfaces is isolable if its components are (this property
// automatically extends to tuples of all sizes):

instance IsolableSub#(Tuple2#(i1, i2), Tuple2#(c1, c2))
   provisos (IsolableSub#(i1, c1), IsolableSub#(i2, c2));
   module mkIsoS#(Tuple2#(i1, i2) i1i2, Tuple2#(c1, c2) c1c2, Bool on)(Tuple2#(i1, i2));
      match {.i1, .i2} = i1i2;
      match {.c1, .c2} = c1c2;
      let o1 <- mkIsoS(i1, c1, on);
      let o2 <- mkIsoS(i2, c2, on);
      return tuple2(o1, o2);
   endmodule
endinstance

instance IsolableSub#(Tuple3#(i1, i2, i3), Tuple3#(c1, c2, c3))
   provisos (IsolableSub#(i1, c1), IsolableSub#(i2, c2), IsolableSub#(i3, c3));
   module mkIsoS#(Tuple3#(i1, i2, i3) i1i2i3, Tuple3#(c1, c2, c3) c1c2c3, Bool on)(Tuple3#(i1, i2, i3));
      match {.i1, .i2, .i3} = i1i2i3;
      match {.c1, .c2, .c3} = c1c2c3;
      let o1 <- mkIsoS(i1, c1, on);
      let o2 <- mkIsoS(i2, c2, on);
      let o3 <- mkIsoS(i3, c3, on);
      return tuple3(o1, o2, o3);
   endmodule
endinstance

// Particular subinterfaces must be dealt with explicitly.  The present design
// needs Gets and Puts:

instance IsolableSub#(Get#(i), i);
   module mkIsoS#(Get#(i) ifc, i clamp, Bool on)(Get#(i));
      method ActionValue#(i) get() if (!on);
	 let x <- ifc.get();
	 return (on ? clamp : x);
      endmethod
   endmodule
endinstance

instance IsolableSub#(Put#(i), i);
   module mkIsoS#(Put#(i) ifc, i clamp, Bool on)(Put#(i));
      method Action put(x) if (!on);
	 ifc.put(x);
      endmethod
   endmodule
endinstance

instance IsolableSub#(TLMRecvIFC#(req,resp), Tuple2#(req,resp));
   module mkIsoS#(TLMRecvIFC#(req,resp) arg, Tuple2#(req,resp) clamp_v, Bool on)(TLMRecvIFC#(req,resp));
      match {.rx_clamp, .tx_clamp} = clamp_v;
      let o1 <- mkIsoS(arg.rx, rx_clamp, on);
      let o2 <- mkIsoS(arg.tx, tx_clamp, on);
      interface rx = o1;
      interface tx = o2;
   endmodule
endinstance

instance IsolableSub#(TLMSendIFC#(req,resp), Tuple2#(req,resp));
   module mkIsoS#(TLMSendIFC#(req,resp) arg, Tuple2#(req,resp) clamp_v, Bool on)(TLMSendIFC#(req,resp));
      match {.tx_clamp, .rx_clamp} = clamp_v;
      let o1 <- mkIsoS(arg.rx, rx_clamp, on);
      let o2 <- mkIsoS(arg.tx, tx_clamp, on);
      interface rx = o1;
      interface tx = o2;
   endmodule
endinstance

// The present design provides a Server, which must therefore be an instance
// of the Isolable typeclass.  It is convenient to make a tuple of all the
// subinterfaces, and exploit the previous instance definitions:

instance IsolableSub#(Server#(req,resp), Tuple2#(req,resp));

   module mkIsoS#(Server#(req,resp) arg,
		  Tuple2#(req,resp) clamp_v, Bool on)(Server#(req,resp));
      let arg2 = tuple2(arg.request, arg.response);

      match {.o1, .o2} <- mkIsoS(arg2, clamp_v, on);
      interface request = o1;
      interface response = o2;
   endmodule
endinstance

// VARIOUS UTILITIES

// Retrieve the index of the current power island from the context:

module [m] currentPower(PowerIx)
   provisos (IsModule#(m,a), Context#(m, PowerContext));
   PowerContext pc <- getContext();
   return pc.dynamicC.defaultPower;
endmodule

// Retrieve the current clock and reset, and their index:

module [m] currentClockResetAndIndex(ClockResetAndIndex)
   provisos (IsModule#(m,a), Context#(m, PowerContext));
   PowerContext pc <- getContext();
   let cclk <- exposeCurrentClock();
   let crst <- exposeCurrentReset();
   return ClockResetAndIndex {clk: cclk,
			 rst: crst,
			 ix: pc.dynamicC.defaultClockIndex};
endmodule

// Construct the ClockResetAndIndex structure from its components (also
// setting them into the context):

module [m] mkClockResetAndIndex#(Clock cl, Reset re, ClockIx n)(ClockResetAndIndex)
   provisos (IsModule#(m,a), Context#(m, ClockContext));
   ClockContext cc <- getContext();
   cc.clks[n.clockIx] = cl;
   cc.rsts[n.clockIx] = re;
   putContext(cc);

   return ClockResetAndIndex {clk: cl, rst: re, ix: n };
endmodule

// A POWER SWITCH

module [m] mkPowerSwitch#(PMAddr adr, PowerIx extnl, PowerIx intnl, Bool init)(Empty)
   provisos (IsModule#(m,a), Context#(m, PowerContext),
	     Context#(m, PMContext), Context#(m, ClockContext));

   PowerContext pc <- getContext();
   ClockContext cc <- getContext();
   let c = pc.dynamicC;
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   CrossingReg#(Bool) stateR <- mkNullCrossingReg(uclock, init);
   Wire#(PowerState) w <- mkBypassWire(clocked_by uclock, reset_by ureset);
   pc.dynamicC.powerStateV[intnl.powerIx] = w;
   putContext(pc);

   // Receive power management signal:
   PMIfc pmi = (
      interface PMIfc;
	 interface Put sendIfc;
		method Action put(x);
		   match {.addr, .data, .isFrom} = x;
		   if (addr==adr && !isFrom) stateR <= data;
		endmethod
	 endinterface
	 interface recvIfc = nullRecvIfc;
      endinterface
		);
   PMContext pmc <- getContext;
   pmc = List::cons(pmi, pmc);
   putContext(pmc);

   (*no_implicit_conditions, fire_when_enabled*)
   rule xmitState;
      let extSt = c.powerStateV[extnl.powerIx].state;
      w <= PowerState { state: extSt && stateR.crossed };
   endrule
endmodule

// RETENTION REGISTER

module [m]  mkRetentionReg#(PMAddr adr, PowerIx ri, a init, Bool initial_state)(Reg#(a))
   provisos (IsModule#(m,_a), Context#(m, PowerContext), Context#(m, PMContext),
	     Context#(m, ClockContext), Bits#(a, sa),
	     Add#(_b, sa, TMul#(sa, 2)));
   // Receive power management signal:
   Reg#(Bool) ret_on <- mkReg(initial_state);
   PMIfc pmi = (
      interface PMIfc;
	 interface Put sendIfc;
		method Action put(x);
		   match {.addr, .data, .isFrom} = x;
		   if (addr==adr && !isFrom) ret_on <= data;
		endmethod
	 endinterface
	 interface recvIfc = nullRecvIfc;
      endinterface
		);
   PMContext pmc <- getContext;
   pmc = List::cons(pmi, pmc);
   putContext(pmc);

   PowerContext pc <- getContext();
   let dc = pc.dynamicC;
   let ri_state = dc.powerStateV[ri.powerIx].state;
   let di_state = dc.powerStateV[dc.defaultPower.powerIx].state;

   Vector#(sa, Bit#(2)) vuni = replicate('b10);
   a uninit = unpack(truncate(pack(vuni)));

   Reg#(a) r <- mkReg(init);
   PulseWire good_write <- mkPulseWire;
   let good_read = (di_state || (ri_state && ret_on));

   rule corrupt(!good_read && !good_write);
      r <= uninit;
   endrule

   method _read = (good_read ? r : uninit);
   method Action _write(x);
      if (di_state) begin
	 r <= x; // if fully operational, a normal write
	 good_write.send();
      end
   endmethod
endmodule

// CLOCK GATING

module [m] mkGatedClockResetAndIndex#(PMAddr adr, ClockIx extnl, ClockIx intnl,
				      Bool init_g, Bool init_r)     (ClockResetAndIndex)
   provisos (IsModule#(m,a), Context#(m, PowerContext),
	     Context#(m, PMContext), Context#(m, ClockContext));

   PowerContext pc <- getContext();
   ClockContext cc <- getContext();
   let c = pc.dynamicC;
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   CrossingReg#(Bool) gStateR <- mkNullCrossingReg(uclock, init_g); // True means clock running
   Reg#(Bool) rStateR <- mkReg(init_r);                             // True means in reset
   Wire#(ClockState) w <- mkBypassWire(clocked_by uclock, reset_by ureset);
   pc.dynamicC.clockStateV[intnl.clockIx] = w;
   putContext(pc);

   // Receive power management signal:
   PMIfc pmi = (
      interface PMIfc;
	 interface Put sendIfc;
		method Action put(x);
		   match {.addr, .data, .isFrom} = x;
		   if (!isFrom) begin
		           if (addr==adr)   gStateR <= data;
		      else if (addr==adr+1) rStateR <= data;
		   end
		endmethod
	 endinterface
	 interface recvIfc = nullRecvIfc;
      endinterface
		);
   PMContext pmc <- getContext;
   pmc = List::cons(pmi, pmc);
   putContext(pmc);

   // Transmit the clock state in the context:

   (*no_implicit_conditions, fire_when_enabled*)
   rule xmitState;
      let extSt = c.clockStateV[extnl.clockIx].running;
      w <= ClockState { running: extSt && gStateR.crossed };
   endrule

   // Construct and control the gated clock:

   let cclk <- exposeCurrentClock();
   let gc_ifc <- mkGatedClock(False, cclk);
   cc.clks[intnl.clockIx] = gc_ifc.new_clk;

   (*no_implicit_conditions, fire_when_enabled*)
   rule setGateR;
      gc_ifc.setGateCond(gStateR);
   endrule

   // Construct and control the new reset:

   let crst <- exposeCurrentReset();
   let rst_ifc <- mkReset(1, True, cclk, clocked_by cclk, reset_by crst);
   cc.rsts[intnl.clockIx] = rst_ifc.new_rst;

   (*no_implicit_conditions, fire_when_enabled*)
   rule setRst;
      if (rStateR)rst_ifc.assertReset();
   endrule

   putContext(cc);

   return ClockResetAndIndex { clk: gc_ifc.new_clk, rst: rst_ifc.new_rst, ix: intnl };
endmodule

// A CYCLE COUNTER

module [m] mkCycleCounter#(String str, Bool pred)(Empty)
   provisos (IsModule#(m,a),
	     Context#(m, ProbeContext), Context#(m, PowerContext), Context#(m, ClockContext));
   let id <- newCounterId();
   // The cycles are counted only when pred is True.
   PowerContext pc <- getContext();
   ProbeContext pr <- getContext();
   ClockContext cc <- getContext();
   let c = pc.dynamicC;
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];
   let cclck <- exposeCurrentClock();

   CrossingReg#(Bool) ctrFlag <- mkNullCrossingRegA(uclock, False,
						    reset_by cc.rsts[1]); // temporary hack
   // In the uclock domain:
   Reg#(Bool)           ucFlag  <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(UInt#(CTRSIZE)) ctr     <- mkReg(0,     clocked_by uclock, reset_by ureset);
   Reg#(Bool)           mustRpt <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)           busy    <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)           sending <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)           enabled <- mkReg(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)           validr  <- mkReg(False, clocked_by uclock, reset_by ureset);


   // In the cclock domain:

   //(*no_implicit_conditions, fire_when_enabled*) -- inhibited, because of gated clocks
   rule toggleFlag (pred);
      ctrFlag <= !ctrFlag;
   endrule

   // In the uclock domain:

   rule incCtr(ctrFlag.crossed != ucFlag);
      ctr <= ctr + 1;
      ucFlag <= ctrFlag.crossed;
   endrule


   rule setReport(pr.enabler matches {.n, .*} &&&
		      n matches tagged Power .ps &&&
		      ps matches tagged ProbeNumber .p &&&
		      p.kind == REPORT &&& enabled);
      mustRpt <= True;
   endrule

   let s = CycleCountStruct {clock: c.defaultClockIndex,
			     pd:    c.defaultPower,
			     dta:   extend(ctr),
			     valid: validr};

   PrbNum prbNum = tagged Power (tagged ProbeNumber
				    PowerStruct {kind: CYCLECOUNTS,
						 num:extend(id)});
   UInt#(8) dummyNum = 0;
   SceMiSerialInfo#(CycleCountStruct) params
      <- mkSceMiSerialInfo(prbNum, dummyNum, POWERMETER, str,
			   clocked_by uclock, reset_by ureset);

   rule setEnabled (pr.enabler matches {.n, .m} &&& n==prbNum);
      if (m==TRIGGER)
         begin
            //$display("Probe %0d: invalid TRIGGER signal", num);
         end
      else
	 begin
	    if (m==ENABLE || m==DISABLE) enabled <= (m==ENABLE);
	    ctr <= 0;
	 end
   endrule

   Reg#(Bit#(32)) resR <- mkRegU(clocked_by uclock, reset_by ureset);

   rule startRpt (mustRpt && !sending);
      resR    <= pack(FirstWord {num: prbNum, nWords: 1});
      mustRpt <= False;
      busy    <= True;
      sending <= True;
      validr  <= True;
   endrule

   PulseWire acked <- mkPulseWire(clocked_by uclock, reset_by ureset);

   (* descending_urgency = "endRpt, incCtr" *)
   rule endRpt if (acked && busy);
      resR <= pack(s);
      busy <= False;
      ctr  <= 0;
   endrule

   rule stopSending if (acked && sending && !busy);
      sending <= False;
   endrule

   let prbdata = (
      interface PrbStr;
	 method Bit#(32) dataout() if (sending);
	    return resR;
	 endmethod

	 method Action ack();
	    acked.send();
	 endmethod
      endinterface
   );

   Bool probeDelayClocksv = mustRpt || sending; // xxx is (mustRpt || busy) sufficient?
   List#(Bool) probeDelayClockss = List::cons(probeDelayClocksv,pr.delays);
   pr.delays = probeDelayClockss;
   pr.ds = tagged Append tuple2(pr.ds, tagged One prbdata);
   putContext(pr);
endmodule

endpackage
