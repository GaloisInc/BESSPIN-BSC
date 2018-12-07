// Copyright (c) 2008-2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
package SceMiSerialProbe;

import GetPut::*;
import BRAMCore::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Connectable::*;
import ConfigReg::*;
import SceMiInternals::*;
import SceMiDefines::*;
import SceMiCore::*;
import Vector::*;
import List::*;
import Clocks::*;
import DReg::*;
import RegFile ::*;
import SceMiScan::*;
import FIFOLevel::*;
import CommitIfc::*;
import Array::*;

import HList::*;
import ModuleContext::*;
import UnitAppendList::*;

export mkSceMiSerialXactor;
export SceMiProbeConfiguration(..);
export EnableMode(..), PrbStr, PrbNum, PrbCnt, ProbeContextIfc(..);
export ProbeContext(..), RunLengthSize, Run(..), nullWord, FirstWord(..);
export setIdBaseProbes, setIdBaseAssertions, setIdBaseCounters, setIdBaseIso;
export newProbeId, newAssertionId, newCounterId, newIsoId;
export setIdBases, setEachIdBase;

export SerialTrigger(..), mkSerialProbe, mkSerialTrigger, mkSerialTriggers, mkSerialCapture;
export mkSerialProbeNoId, AssertionReporter(..), mkAssertionReporter;
export CProbe(..), mkSerialProbe1, mkStaller;
export CaptureIfc(..);
export CompleteProbeContexts, ProbeModule, CompleteProbeContextsIfc(..), CLOCKCONTEXTSIZE;
export mkInitialProbeContextWithClocks, WithOrWithoutProbes(..), WithOrWithoutProbesReset(..);
export mkReg_p, mkReg_c, mkRegU_p, mkRegU_c; // mkDReg_p, mkDReg_c;
export Probifiable(..), CProbifiable(..);
export runWithProbes2CC;
export ProbeKind(..), SceMiSerialInfo, mkSceMiSerialInfo;
export /*cosimObserve, CosimObserve(..),*/ VProbeIfc(..), PrbSVtr(..), PrbSV(..);

typedef enum { NULL, DISABLE, ENABLE, TRIGGER, // for normal probes
	       CYCLECOUNTS,                    // for power "probes"
	       SENDONCE                        // mainly for assertions
	      } EnableMode deriving (Eq, Bits);

typedef struct {
   Wire#(Tuple2#(PrbNum, EnableMode)) enabler;
   List#(Bool) delays;
   PulseWire  advanceCclock;
   UAList#(PrbStr) ds;

   UInt#(13) idBaseProbes;
   UInt#(13) idBaseAssertions;
   UInt#(10) idBaseCounters;
   UInt#(10) idBaseIso;
} ProbeContext;

module [mc] setIdBaseProbes#(UInt#(13) n)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   pc.idBaseProbes = n;
   putContext(pc);
endmodule

module [mc] setIdBaseAssertions#(UInt#(13) n)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   pc.idBaseAssertions = n;
   putContext(pc);
endmodule

module [mc] setIdBaseCounters#(UInt#(10) n)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   pc.idBaseCounters = n;
   putContext(pc);
endmodule

module [mc] setIdBaseIso#(UInt#(10) n)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   pc.idBaseIso = n;
   putContext(pc);
endmodule

module [mc] setIdBases#(UInt#(13) p, UInt#(13) a,
			UInt#(10) c, UInt#(10) i)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   setIdBaseProbes    (p);
   setIdBaseAssertions(a);
   setIdBaseCounters  (c);
   setIdBaseIso       (i);
endmodule

module [mc] setEachIdBase#(UInt#(10) n)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   setIdBases(extend(n), extend(n), n, n);
endmodule

module [mc] newCounterId(UInt#(10))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   let n = pc.idBaseCounters;
   pc.idBaseCounters = n+1;
   putContext(pc);
   return n;
endmodule

module [mc] newIsoId(UInt#(10))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   let n = pc.idBaseIso;
   pc.idBaseIso = n+1;
   putContext(pc);
   return n;
endmodule

module [mc] newProbeId(UInt#(13))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   let n = pc.idBaseProbes;
   pc.idBaseProbes = n+1;
   putContext(pc);
   return n;
endmodule

module [mc] newAssertionId(UInt#(13))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));
   ProbeContext pc <- getContext();
   let n = pc.idBaseAssertions;
   pc.idBaseAssertions = n+1;
   putContext(pc);
   return n;
endmodule

/* This interface is used for cosim, where the pin names
 * are required to match externally generated Verilog files.
 */
interface PrbSV#(type a);
   (* prefix = "", result = "DATAUP", always_ready, always_enabled *)
   method a first;
   (* prefix = "", result = "DATAVALID", always_ready, always_enabled *)
   method Bool first_rdy;
   (* prefix = "", enable = "ACK", always_ready *)
   method Action deq;
endinterface

module toPrbSV#(SendCommit#(a) fo) (PrbSV#(a))
   provisos(Bits#(a, sa));

   PulseWire first_rdy_pw <- mkPulseWire;
   PulseWire deq_pw       <- mkPulseWire;
   Wire#(a) first_wire    <- mkDWire(?);

   rule grab_first;
      first_rdy_pw.send;
      first_wire <= fo.dataout;
   endrule

   rule do_deq (deq_pw);
      fo.ack;
   endrule

   method a first;
      return first_wire;
   endmethod

   method Bool first_rdy;
      return first_rdy_pw;
   endmethod

   method Action deq;
      if (first_rdy_pw) deq_pw.send;
   endmethod

endmodule

typedef SendCommit#(Bit#(32)) PrbStr;
typedef PrbSV#(Bit#(32))      PrbSVtr;

interface ProbeContextIfc;
   interface PrbStr words;
   (*always_ready*)
   method Action enblr(Tuple2#(PrbNum, EnableMode) x);
   (* always_ready *)
   method Bool probeDelayClocks;
   (* always_ready *)
   method Action advanceCclock();
endinterface

module mkInitialProbeContext(ProbeContext);
   let w <- mkWire;
   let advanceCC <- mkPulseWire;
   let initP = ProbeContext { enabler: w, ds: NoItems, delays: List::nil, advanceCclock:advanceCC,
			      idBaseProbes:0, idBaseAssertions:0, idBaseCounters:0, idBaseIso:0 };
   return initP;
endmodule

typedef struct {
   PrbNum    num;
   UInt#(16) nWords;
} FirstWord deriving (Eq, Bits);

Bit#(32) nullWord = pack(FirstWord { num: unpack('1), nWords: 0 });

typedef 32 RunLengthSize;

typedef struct {
   t probe;
   UInt#(RunLengthSize) runlength;
} Run#(type t)
deriving (Bits);

typedef struct {
   Integer clockNum;    // The clockNum of the SceMiClockPort that controls the DUT
} SceMiProbeConfiguration;

typedef struct {
    probes probe;
    SceMiCycleStamp cycles;
} Probes#(type probes)
deriving (Bits);

typedef struct {
   Bool enable;
   PrbNum  num;
} ProbeControl deriving (Bits);

typedef UInt#(16) PrbCnt;

module [SceMiModule] mkSceMiSerialXactor#(parameter SceMiProbeConfiguration conf,
					  ProbeContextIfc pi) (Empty);

   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // SceMi In and Out ports for talking to the software host:
   SceMiMessageOutPortIfc#(Bit#(32))       data_out  <- mkSceMiMessageOutPort();
   SceMiMessageInPortIfc# (ProbeControl) control_in  <- mkSceMiMessageInPort ();

   Reg#(PrbCnt) count <- mkReg(0);
   Reg#(Bool) pinged <- mkReg(False);
   FIFO#(Tuple2#(PrbNum, EnableMode)) enff <- mkFIFO;
   FIFO#(FirstWord)  ackFifo <- mkFIFO;

   Vector#(4, Reg#(Bit#(8))) sampleIntervalV <- replicateM(mkReg(0));
   UInt#(64) sampleInterval = extend(unpack(pack(readVReg(sampleIntervalV))));
   Reg#(UInt#(64)) nextSample <- mkReg(0);

   // For Power PrbNums of type PowerStruct with kind REPORT, if num != '1
   // then the num is used to set an element of sampleIntervalV, as in the
   // function setSampleInterval below.  If num=='1, a REPORT request is sent
   // to all probes (via enff, by the receiveControl rule), and if
   // sampleInterval!=0 the nextSample reg is initialized.  When the SceMi
   // timestamp passes nextSample a REPORT request is sent, and nextSample
   // re-initialized.

   let scTime <- sceMiCycleStamp();

   function Action setSampleInterval(UInt#(10) xx);
      action
	 if (xx==0)
	    for (Integer i=0; i<4; i=i+1) sampleIntervalV[i]<=0;
	 else begin
	    let x = pack(xx);
	    let i = x[9:8];
	    let v = x[7:0];
	    sampleIntervalV[i] <= v;
	 end
      endaction
   endfunction

   PrbNum request = tagged Power (tagged ProbeNumber PowerStruct
				       { kind: REPORT, num: unpack('1)});

   Action startSampling =
      action
	 enff.enq(tuple2(request, ENABLE));
	 if (sampleInterval != 0) begin
	    nextSample <= scTime + sampleInterval;
	 end
      endaction ;

   rule setSample (sampleInterval != 0 && scTime == nextSample);
	 startSampling;
   endrule

   Reg#(Bool) flag <- mkReg(False);

   (* preempts="setSample, flagSample"*)
   rule flagSample (!flag && sampleInterval != 0 && scTime == nextSample);
      flag <= True;
   endrule

   rule unsetFlag (flag);
      flag <= False;
      startSampling;
   endrule

   (* fire_when_enabled *)
   rule send_en;
      let x = enff.first;
      enff.deq;
      pi.enblr(x);
   endrule

   // Continually invite control input:
   rule requestInput;
      control_in.request();
   endrule

   rule receiveControl;
      ProbeControl c <- toGet(control_in).get;
      if (c.num==tagged Control 'h1FFF) pinged <= True;
      else if (c.num==request) startSampling;
      else if (c.num matches tagged Power (tagged ProbeNumber .ps ) &&&
	       ps.kind==REPORT)
	 setSampleInterval(ps.num);
      else begin
         enff.enq(tuple2(c.num, (c.enable ? ENABLE : DISABLE)));
         if (!c.enable) begin
            ackFifo.enq(FirstWord {num:c.num, nWords: 'hFFF9} );
         end
      end
   endrule

   // Use a FIFOF to convert SendCommit pi.words interface to a FIFOF
   // interface that we can use inside the module
   FIFOF#(Bit#(32)) prb_str <- mkFIFOF();
   RecvCommit#(Bit#(32)) prb_str_recv <- mkRecvCommit(prb_str);
   Empty contextToPrbStr <- mkConnection(pi.words,prb_str_recv);

   Bit#(32) x = prb_str.first();
   FirstWord fw = unpack(x);

   Bool canStart = count==0;

   (*preempts = "receiveTrigger, receiveControl" *)
   rule receiveTrigger (canStart && pack(fw.nWords)[15:3]=='1);
      // Special values for fw.nWords:
      // FFFC: Cycle counts
      // FFFB: Trigger
      // FFFA: Enable
      // FFF9: Disable

      //
      // REPLICATED CODE WARNING
      // If these encodings change, the following files must be kept in sync:
      //
      // SceMiSerialProbe.bsv
      //
      // Verilog Probes (ProbeTrigger.v, ProbeCapture.v etc.)
      //
      // C++ Transactor code (src/lib/SceMi/bsvxactors/SerialProbeXactor.cxx)
      //

      EnableMode y = unpack(pack(fw.nWords)[2:0]);
      prb_str.deq();
      enff.enq(tuple2(fw.num, y));
      toPut(data_out).put(x);
   endrule

   (*preempts = "receiveTrigger,  receiveFirstData"*)
   rule receiveFirstData (canStart);
      count <= fw.nWords;
      prb_str.deq();
      toPut(data_out).put(x);
      pinged <= False;
      //$display("sending first data (%0d): nWords %0d", fw.num, fw.nWords);
   endrule

   rule receiveMoreData (count!=0);
      prb_str.deq();
      toPut(data_out).put(x);
      pinged <= False;
      //$display("sending more data %0d: %h", count, x);
      count <= count - 1;
   endrule

   (*preempts = "respondToPing, receiveFirstData"*)
   rule respondToPing (canStart && pinged);
      pinged <= False;
      toPut(data_out).put(pack(FirstWord {num: tagged Control 'h1FFF, nWords: ?}));
   endrule

   // Sending back ack should preeempt first data otherwise a data stream can starve this rule.
   (*preempts = "sendAck, receiveFirstData"*)
   rule sendAck (canStart && ! pinged);
      toPut(data_out).put(pack(ackFifo.first));
      ackFifo.deq;
   endrule

   // Clock controllers for dut and probes
   Bool allow_edges = !pi.probeDelayClocks && ! prb_str.notEmpty;
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl( conf.clockNum
                                                        , allow_edges
                                                        , allow_edges
                                                        );

   rule announceCclock (dutclkctrl.pre_posedge);
      pi.advanceCclock();
   endrule
endmodule: mkSceMiSerialXactor

// =====

interface ReadOnlyWithClear#(type a);
   method Maybe#(a) _read();
   method Action clear();
endinterface


// Not synthesized; clocked by cclock. sample is clocked by cclock; the
// provided interface is clocked by uclock.
module mkCtoUclock#(a sample, Clock uclk, Reset urst)(ReadOnlyWithClear#(a))
   provisos (Bits#(a,sa));

   CrossingReg#(a) valR <- mkNullCrossingReg( uclk, unpack(0));
   // This register toggles whenever a sample is caught in the cclock domain
   CrossingReg#(Bool) toggleR <- mkNullCrossingReg(uclk, False);

   (*fire_when_enabled*)
   rule readSampleC (True);
      toggleR <= !toggleR;
      valR    <= sample;
   endrule

   Reg#(a) valRU <- mkReg(unpack(0), clocked_by uclk, reset_by urst);
   Reg#(Bool) toggleRU <- mkReg(False, clocked_by uclk, reset_by urst);
   Reg#(Bool) checkRU  <- mkReg(False, clocked_by uclk, reset_by urst);

   (*no_implicit_conditions, fire_when_enabled*)
   rule readSampleU;
      toggleRU <= toggleR.crossed;
      valRU    <= valR.crossed;
   endrule

   method _read = (toggleRU != checkRU ? tagged Valid valRU : Invalid);
   method Action clear;
      checkRU  <= toggleRU;
   endmethod
endmodule


interface CaptureChange#(type a);
   (*always_ready*)
   method a _read();
   (*always_ready*)
   method Bool changed;
   (*always_ready*)
   method Action ack;
endinterface

module mkCaptureChange#(a sample, Clock uclk, Reset urst)(CaptureChange#(a))
   provisos (Bits#(a,sa), Eq#(a) );

   CrossingReg#(a) valR <- mkNullCrossingRegU( uclk);
   // This register toggles whenever a sample is caught in the cclock domain
   CrossingReg#(Bool) toggleR <- mkNullCrossingRegU(uclk);

   (*fire_when_enabled*)
   rule readSampleC (sample != valR);
      toggleR <= !toggleR;
      valR    <= sample;
   endrule

   Reg#(Bool) toggleRU <- mkRegU(clocked_by uclk, reset_by urst);

   Bool newData = toggleR.crossed != toggleRU;
   method _read = valR.crossed;
   method Bool changed = newData;
   method Action ack ;          //  ack when not ready are allowed
      toggleRU <= toggleR.crossed ;
   endmethod
endmodule

/*
   __   B__    __    __    __
__|  |__|  |__|  |__|  |__|  |__ uclock
     ___         ___         ___
\\__|   \\\\\___|   \\\\\___|   \cclock
    A

I'm assuming that the skew caused by the SceMi clock control logic's PLAs is
as shown above (the \\\\ denote "don't care" negedges).  Thus a transition
happening in the cclock domain at A should (if there's no combinational logic)
be safely registered in the uclock domain at B.  It is of course even better
if A is aligned with the preceding uclock edge.

*/

// Special values for fw.nWords:
// FFFC: Cycle counts
// FFFB: Trigger
// FFFA: Enable
// FFF9: Disable

//
// REPLICATED CODE WARNING
// If these encodings change, the following files must be kept in sync:
//
// SceMiSerialProbe.bsv
//
// Verilog Probes (ProbeTrigger.v, ProbeCapture.v etc.)
//
// C++ Transactor code (src/lib/SceMi/bsvxactors/SerialProbeXactor.cxx)
//


function Bool isSpecial(UInt#(16) n);
   return (pack(n)[15:3]=='1);
endfunction

// Should be kept compatible with SerialProbeType in
// lib/SceMi/bsvxactors/SerialProbeXactor.h:
typedef enum { NORMALPROBE, CAPTUREPROBE, TRIGGERPROBE, CAPTUREPROBEV2,
               SCANPROBE, POWERMETER, POWERSOURCE, POWERPROBE } ProbeKind deriving (Eq, Bits);

typedef HList2#(ProbeContext, ClockContext) CompleteProbeContexts;
typedef ModuleContext#(CompleteProbeContexts) ProbeModule;
typedef Tuple2#(ProbeContextIfc, Empty) CompleteProbeContextsIfc;

module mkSceMiSerialInfo#(parameter PrbNum num,
			  parameter UInt#(addr_sz) trigger_point,
			  ProbeKind knd,
			  String lbl)(SceMiSerialInfo#(a))
   provisos(Add#(a__, addr_sz, 32)
            ,Bits#(a,sa)
            );

   Ignored param_prbnum     <- mkSceMiUInt32Parameter(extend(unpack(pack(num))));
   Ignored param_numsamples <- mkSceMiUInt32Parameter(1 << valueof(addr_sz));
   Ignored param_offset     <- mkSceMiUInt32Parameter(extend(trigger_point));
   Ignored param_prbknd     <- mkSceMiUInt32Parameter(extend(unpack(pack(knd))));
   Ignored param_label      <- mkSceMiStringParameter(lbl);
   Ignored param_width      <- mkSceMiUInt32Parameter(fromInteger(valueOf(sa)));

   return ?;
endmodule

module mkSceMiSerialInfo2#(parameter PrbNum num,
                           ProbeKind knd,
                           String lbl,
                           parameter Integer runlengthwidth,
                           parameter Integer memsize
                           ) (SceMiSerialInfo#(a))
   provisos (Bits#(a,sa));

   Ignored param_prbnum     <- mkSceMiUInt32Parameter(extend(unpack(pack(num))));
   Ignored param_numsamples <- mkSceMiUInt32Parameter(fromInteger(memsize));
   Ignored param_offset     <- mkSceMiUInt32Parameter(fromInteger(runlengthwidth));
   Ignored param_prbknd     <- mkSceMiUInt32Parameter(extend(unpack(pack(knd))));
   Ignored param_label      <- mkSceMiStringParameter(lbl);
   Ignored param_width      <- mkSceMiUInt32Parameter(fromInteger(valueOf(sa)));

   return ?;
endmodule

interface InternalIfc;
   (* always_ready *)
   method Bool probeDelayClocks();
   interface PrbStr data;
endinterface

UInt#(8) dummyNum = 0; // for irrelevant params of mkSceMiSerialInfo etc.

////////////////////////////////////////////////////////////////////////
// Verilog interface and wrappere for ProbeValue
interface VProbeValue#(type a);
   (*always_ready*)
   method Action enabler(Tuple2#(PrbNum, EnableMode) cmd);
   (*always_ready*)
   method Bool delayClocks;

   // data up interface
   interface PrbStr data;

   // Probe enable interface on Controlled Clock
   (*always_ready*)
   method Action probein (a pdata);
endinterface

/////////////////////////////////////////////////////////////////////////////
// Interface for mkSerialProbeC (to be used only if instantiated with uclock)

interface CProbe;
   method Bool delay;
endinterface

module [mc] mkSerialProbeC#(PrbNum num, String lbl, Bool initiallyEnabled, a x)(CProbe)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext),
	     Bits#(a,sa),
	     Div#(sa, 32, nwords));

   ProbeContext pc <- getContext();
   ClockContext cc <- getContext();
   let cclk <- exposeCurrentClock;
   let crst <- exposeCurrentReset;
   let uclk = cc.clks[0];
   let urst = cc.rsts[0];

   CrossingReg#(a)         csample <- mkNullCrossingReg(uclk, unpack(0));
   CrossingReg#(a)     cPrevSample <- mkNullCrossingReg(uclk, unpack(0));
   CrossingReg#(Bool)      cToggle <- mkNullCrossingReg(uclk, False);
   a    usample     = readCrossingRegDst(csample);
   a    uPrevSample = readCrossingRegDst(cPrevSample);
   Bool uToggle     = readCrossingRegDst(cToggle);

   (*no_implicit_conditions, fire_when_enabled *)
   rule sample;
      csample <= x;
      cPrevSample <= csample;
      cToggle <= !cToggle;
   endrule

   // clocked and reset by uclk and urst:
   module mkInternal(InternalIfc);
      // Logic for enabling and disabling:
      Reg#(Bool) enabled <- mkReg(initiallyEnabled);
      Reg#(Bool) forceChange <- mkReg(False);
      Wire#(Bool) setForceChange <- mkWire;
      Reg#(Bool) sendOnce <- mkReg(False);
      Reg#(Bool) sentOne  <- mkReg(False);

      rule setEnabled (pc.enabler matches {.n, .m} &&& n==num);
	 if (m==TRIGGER)
            begin
               //$display("Probe %0d: invalid TRIGGER signal", num);
            end
	 else
	    begin
	       enabled <= (m==ENABLE || m==SENDONCE);
	       if (m==ENABLE) setForceChange <= True;
	       sendOnce <= (m==SENDONCE);
	       sentOne  <= False;
	    end
      endrule

      // This should happen at the end of the cycle (hence the wire):
      rule setForceChangeReg (setForceChange);
	 forceChange <= True;
      endrule

      Integer num_words = valueOf(nwords);
      Vector#(nwords, Reg#(Bit#(32))) words <- replicateM(mkRegU);
      Reg#(UInt#(16)) remaining <- mkReg(0);

      Reg#(Bool) prevToggle <- mkReg(False);

      function fresh(x) = pack(x)!=pack(uPrevSample);

      Bool idle = (remaining == 0);
      Bool mustSend = (enabled != sentOne) &&
	 uToggle!=prevToggle && (forceChange || fresh(usample));
      Bool dontSend = (enabled != sentOne) &&
         uToggle!=prevToggle && !(forceChange || fresh(usample));
      Bool trigger_delay = mustSend || !idle;

      Reg#(Bit#(32)) resR <- mkRegU;

      rule startSend(mustSend && idle);
	 let x = usample;
	 resR <= pack(FirstWord {num: num, nWords: fromInteger(num_words)});
	 writeVReg(words,toChunks(x));
	 remaining <= fromInteger(num_words+1);
	 forceChange <= False;
	 prevToggle <= uToggle;
      endrule

      rule noSend(dontSend);
	 prevToggle <= uToggle;
      endrule

      PulseWire acked <- mkPulseWire();
      rule sendNext if (acked && !idle);
	 resR <= words[0];
	 remaining <= remaining - 1;
	 for (Integer n = 1; n < num_words; n = n+1)
	    words[n-1] <= words[n];
      endrule

      method probeDelayClocks = trigger_delay;

      interface PrbStr data;

	 method Bit#(32) dataout() if (!idle);
	    return resR;
	 endmethod

	 method Action ack();
	    acked.send();
	 endmethod

      endinterface
   endmodule

   let serPrbInt <- mkInternal(clocked_by cc.clks[0], reset_by cc.rsts[0]);
   Bool probeDelayClocksv = serPrbInt.probeDelayClocks();
   List#(Bool) probeDelayClockss = List::cons(probeDelayClocksv,pc.delays);
   pc.delays = probeDelayClockss;

   pc.ds = tagged Append tuple2(pc.ds, tagged One serPrbInt.data);
   putContext(pc);

   method delay = probeDelayClocksv;
endmodule

import "BVI" ProbeValue =
module vProbeValue (PrbNum probeId, Clock cclk, // default clock is uclock
		    Bool initiallyEnabled,
		    VProbeValue#(a) ifc)
   provisos (Bits#(a,sa));
   default_clock uclock(UCLK, (*unused*)CLK_GATEu) ;
   default_reset ureset(URST);
   input_clock cclock(CLK, (*unused*)CLK_GATEc) = cclk;
   parameter ProbeId = pack(probeId);
   parameter ProbeWidth = fromInteger(valueOf(sa));
   parameter InitiallyEnabled = pack(initiallyEnabled);
   method        enabler    (CMD) enable(CMDEN)   clocked_by(uclock) reset_by(ureset);
   method DELAY  delayClocks()                    clocked_by(uclock) reset_by(ureset);
   interface PrbStr data;
      method DATAUP dataout    ()   ready(DATAVALID) clocked_by(uclock) reset_by(ureset);
      method        ack        ()    enable(ACK)     clocked_by(uclock) reset_by(ureset);
   endinterface
   method        probein    (PROBEIN) enable(PROBEEN)  clocked_by(cclock) reset_by(no_reset);

      schedule (enabler, delayClocks, data_dataout, data_ack, probein) CF (delayClocks, data_dataout, data_ack);
      schedule enabler C enabler;
      schedule probein C probein;
      schedule enabler CF probein;
endmodule

module [mc] mkSerialProbe#(UInt#(13) num, String lbl, a x)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext),
	     Bits#(a,sa)
	     );
   let _ifc <- mkSerialProbe1(tagged Explicit num, lbl, False, x);
endmodule

module [mc] mkSerialProbeNoId#(String lbl, a x)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext),
	     Bits#(a,sa)
	     );
   let num <- newProbeId;
   let _ifc <- mkSerialProbe1(tagged Explicit num, lbl, False, x);
endmodule

interface AssertionReporter;
   method Action reportFailedAssertion();
endinterface

typedef struct {
   Bool valid;
   Bool toggle;
		} AssStruct deriving (Bits, Eq);

module [mc] mkAssertionReporter#(String lbl)(AssertionReporter)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext));
   ProbeContext pc <- getContext();
   let num <- newAssertionId();
   Reg#(AssStruct) r <- mkReg(AssStruct{valid: False, toggle: False});
   let _ifc <- mkSerialProbe1(tagged Assertion num, lbl, True, r);

   method Action reportFailedAssertion();
      r <= AssStruct{valid: True, toggle: !r.toggle};
   endmethod
endmodule

// The provided interface is valid only if currentClock is uclock:
module [mc] mkSerialProbe1#(PrbNum num, String lbl, Bool initiallyEnabled, a x)(CProbe)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext),
	     Bits#(a,sa)
             );

   Clock cclock <- exposeCurrentClock();
   // Get the clock context and extract the Uclock/Ureset:
   ClockContext cc <- getContext();
   let uclk = cc.clks[0];
   let urst = cc.rsts[0];

   case (num) matches
      tagged Power .p:
	 if (p matches tagged ProbeNumber .pn &&& pn.kind == CYCLECOUNTS)
	    begin
	       SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, dummyNum, POWERMETER, lbl);
	    end
	 else if (p matches tagged ProbeNumber .pn &&& pn.kind == CHANGES)
	    begin
	       SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, dummyNum, POWERSOURCE, lbl);
	    end
	 else
	    begin
	       SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, dummyNum, POWERPROBE, lbl);
	    end
      tagged Explicit .*:
	 SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, dummyNum, NORMALPROBE, lbl);
      tagged HdlEdit .*:
	 SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, dummyNum, NORMALPROBE, lbl);
   endcase
   if (genC || uclk==cclock) begin
      let _ifc <- mkSerialProbeC(num,lbl,initiallyEnabled, x);
      return _ifc;
   end
   else begin
      ProbeContext pc <- getContext();

      VProbeValue#(a) _prb <- vProbeValue(num, cclock,
					  initiallyEnabled,
					  clocked_by(uclk), reset_by(urst));

      (* fire_when_enabled *)      // implicit conditions are OK
      rule probeValue (True);
         _prb.probein(x);
      endrule
      (* fire_when_enabled *)      // implicit conditions are OK
      rule probeEnabler (True);
         _prb.enabler(pc.enabler);
      endrule

      List#(Bool) probeDelayClockss = List::cons(_prb.delayClocks,pc.delays);
      pc.delays = probeDelayClockss;
      pc.ds = tagged Append tuple2(pc.ds, tagged One _prb.data);
      putContext(pc);
      return ?;
   end
endmodule

// A "system" module (in the uclock domain) for stalling the clocks, used (for
// example) by some of the power primitives:
module [mc] mkStaller#(Bool x)(Empty)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext));

   ProbeContext pc <- getContext();
   List#(Bool) probeDelayClockss = List::cons(x,pc.delays);
   pc.delays = probeDelayClockss;
   putContext(pc);
endmodule

////////////////////////////////////////////////////////////////////////

interface SerialTrigger;
   method Action trigger;
   method Action startCapture;
   method Action stopCapture;
endinterface

module [mc] mkSerialTrigger#(UInt#(13) trgNum, String lbl, UInt#(13) num)(SerialTrigger)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext));
   (*hide*)let _ifc <-
      mkSerialTrigger1(tagged Explicit trgNum, lbl, tagged Explicit num);
   return _ifc;
endmodule

module [mc] mkSerialTrigger1#(PrbNum trgNum, String lbl, PrbNum num)(SerialTrigger)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext));

   SceMiSerialInfo#(void) params <- mkSceMiSerialInfo(trgNum, dummyNum, TRIGGERPROBE, lbl);

   ProbeContext pc <- getContext();
   ClockContext cc <- getContext();
   let cclk <- exposeCurrentClock;
   let crst <- exposeCurrentReset;
   let uclk = cc.clks[0];
   let urst = cc.rsts[0];

   Wire#(EnableMode) trgW <- mkDWire(NULL);

   ReadOnlyWithClear#(EnableMode) usample <- mkCtoUclock(trgW, uclk, urst);

   // clocked and reset by cc.clks[0] and cc.rsts[0] (uclock and ureset):
   module mkInternal(InternalIfc);
      // See comment on similar definition above:

      Reg#(UInt#(8)) enableCount <- mkConfigReg(0);
      Bool enabled = (enableCount != 0);
      Reg#(Bool) sendDisable <- mkReg(False);
      Reg#(Bool) sendTrgRpt  <- mkReg(False);

      rule setEnabled (pc.enabler matches {.n, .m} &&& n==trgNum);
	 if (m!=TRIGGER) enableCount <= (m==ENABLE ? enableCount+1 : 0);
      endrule

      let mm = usample;

      Bool x = (mm matches tagged Valid .m &&& m != NULL ? True : False);
      Bool mustSend = (enabled && x);
      /*
      Bool cancel = (mm matches tagged Valid .m &&& !mustSend ? True : False);

      rule cancelR (cancel);
	 usample.clear();
      endrule
      */

      Reg#(Bit#(32)) resR <- mkReg(nullWord);
      Reg#(Bool)   sendIt <- mkReg(False);

      rule startSend(mustSend && (resR == nullWord));
	 let m = validValue(mm);
	 resR <= pack(FirstWord {num: num, nWords: unpack({'1, pack(m) })});
	 sendIt <= True;
      endrule

      PulseWire acked <- mkPulseWire();
      rule sendNext if (acked && (resR != nullWord));
	 Bit#(32) res = nullWord;
	 Bool sendDis = sendDisable;
	 let new_enableCount = enableCount - 1;
	 if (sendDis) begin
	    // Send a disable signal if the specified number of events have
	    // now been triggered:
	    sendDis = False;
	    res = pack(FirstWord {num: trgNum, nWords: unpack({'1, pack(DISABLE) })});
	 end
	 else if (sendIt) begin
	    sendIt <= False;
	    res = pack(FirstWord {num: trgNum, nWords: unpack({'1, pack(TRIGGER) })});
	    enableCount <= new_enableCount;
	    if (new_enableCount==0) sendDis = True;
	 end
	 resR <= res;
	 sendDisable <= sendDis;
	 usample.clear();
      endrule

      Bool trigger_delay = mustSend || resR!=nullWord;

      method probeDelayClocks = trigger_delay;

      interface PrbStr data;

	 method Bit#(32) dataout() if (resR != nullWord);
	    return resR;
	 endmethod

	 method Action ack();
	    acked.send();
	 endmethod

      endinterface
   endmodule

   let serTrgsInt <- mkInternal(clocked_by cc.clks[0], reset_by cc.rsts[0]);
   pc.delays = List::cons(serTrgsInt.probeDelayClocks(),pc.delays);
   pc.ds = tagged Append tuple2(pc.ds, tagged One serTrgsInt.data);
   putContext(pc);

   method Action trigger;
      trgW <= TRIGGER;
   endmethod

   method Action startCapture;
      trgW <= ENABLE;
   endmethod

   method Action stopCapture;
      trgW <= DISABLE;
   endmethod
endmodule

module [mc] mkSerialTriggers#(UInt#(13) trgNum, String lbl, UInt#(13) nums[])(SerialTrigger)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext));
   function f(x) = tagged Explicit x;
   let newNums = Array::map(f, nums);
   (*hide*)let _ifc <-
      mkSerialTriggers1(tagged Explicit trgNum, lbl, newNums);
   return _ifc;
endmodule

module [mc] mkSerialTriggers1#(PrbNum trgNum, String lbl, PrbNum nums[])(SerialTrigger)
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext));

   SceMiSerialInfo#(void) params <- mkSceMiSerialInfo(trgNum, dummyNum, TRIGGERPROBE, lbl);

   ProbeContext pc <- getContext();
   ClockContext cc <- getContext();
   let cclk <- exposeCurrentClock;
   let crst <- exposeCurrentReset;
   let uclk = cc.clks[0];
   let urst = cc.rsts[0];

   Wire#(EnableMode) trgW <- mkDWire(NULL);

   ReadOnlyWithClear#(EnableMode) usample <- mkCtoUclock(trgW, uclk, urst);

   // clocked and reset by cc.clks[0] and cc.rsts[0] (uclock and ureset):
   module mkInternal(InternalIfc);
      // See comment on similar definition above:

      Reg#(UInt#(8)) enableCount <- mkConfigReg(0);
      Bool enabled = (enableCount != 0);
      Reg#(Bool) sendDisable <- mkReg(False);
      Reg#(EnableMode) modeToSend <- mkRegU;

      rule setEnabled (pc.enabler matches {.n, .m} &&& n==trgNum);
	 if (m!=TRIGGER) enableCount <= (m==ENABLE ? enableCount+1 : 0);
      endrule

      let mm = usample;

      Reg#(UInt#(16)) remaining <- mkReg(0);

      Bool x = (mm matches tagged Valid .m &&& m != NULL ? True : False);
      Bool idle = (remaining == 0);
      Bool mustSend = (enabled && x);

      rule clearNull (mm matches tagged Valid .m &&& m == NULL);
	 usample.clear();
      endrule

      Reg#(Bit#(32)) resR <- mkReg(nullWord);

      rule startSend(mustSend && idle);
	 let m = validValue(mm);
	 let rem = fromInteger(arrayLength(nums));
	 resR <= pack(FirstWord {num: nums[rem-1], nWords: unpack({'1, pack(m) })});

	 modeToSend <= m;
	 remaining <= rem;
      endrule

      PulseWire acked <- mkPulseWire();
      rule sendNext if (acked && (resR != nullWord));
	 Bit#(32) res = nullWord;
	 UInt#(16) rem = remaining;
	 Bool sendDis = sendDisable;
	 EnableMode mode = modeToSend;
	 let new_enableCount = enableCount - 1;
	 if (sendDis) begin
	    // Send a disable signal if the specified number of events have
	    // now been triggered:
	    sendDis = False;
	    res = pack(FirstWord {num: trgNum, nWords: unpack({'1, pack(DISABLE) })});
	 end
	 else if (!idle) begin
	    if (rem==1)
	       begin
		  enableCount <= new_enableCount;
		  res = pack(FirstWord {num: trgNum, nWords: unpack({'1, pack(TRIGGER) })});
		  if (new_enableCount==0) sendDis = True;
	       end
	    else
	       res = pack(FirstWord {num: nums[rem-2], nWords: unpack({'1, pack(mode) })});
	    rem = rem-1;
	 end
	 resR <= res;
	 remaining <= rem;
	 modeToSend <= mode;
	 sendDisable <= sendDis;
	 usample.clear();
      endrule

      Bool trigger_delay = mustSend || resR!=nullWord;

      method probeDelayClocks = trigger_delay;

      interface PrbStr data;

	 method Bit#(32) dataout() if (resR != nullWord);
	    return resR;
	 endmethod

	 method Action ack();
	    acked.send();
	 endmethod

      endinterface
   endmodule

   let serTrgsInt <- mkInternal(clocked_by cc.clks[0], reset_by cc.rsts[0]);
   pc.delays = List::cons(serTrgsInt.probeDelayClocks(),pc.delays);
   pc.ds = tagged Append tuple2(pc.ds, tagged One serTrgsInt.data);
   putContext(pc);

   method Action trigger;
      trgW <= TRIGGER;
   endmethod

   method Action startCapture;
      trgW <= ENABLE;
   endmethod

   method Action stopCapture;
      trgW <= DISABLE;
   endmethod
endmodule


interface CaptureIfc ;
   (*always_ready*)
   method Action trigger();
endinterface


////////////////////////////////////////////////////////////////////////
// Verilog interface and wrappere for ProbeCapture
interface VProbeCapture#(type a);
   (*always_ready*)
   method Action enabler(Tuple2#(PrbNum, EnableMode) cmd);
   (*always_ready*)
   method Bool delayClocks;

   // data up interface
   interface PrbStr data;

   // Probe enable interface on Controlled Clock
   (*always_ready*)
   method Action probein (a pdata);
   (*always_ready*)
   method Action trigger;
endinterface

import "BVI" ProbeCapture =
module vProbeCapture (PrbNum probeId
                      ,Integer memsize
                      ,Clock cclk
                      ,UInt#(addr_sz) triggerToDump
                      ,Bool announceClocks
                      ,VProbeCapture#(a) ifc) // default clock is uclock
   provisos (Bits#(a,sa));
   default_clock uclock(UCLK, (*unused*)CLK_GATEu) ;
   default_reset ureset(URST);
   input_clock cclock(CLK, (*unused*)CLK_GATEc) = cclk;

   parameter ProbeId = pack(probeId);
   parameter ProbeWidth = fromInteger(valueOf(sa));
   parameter MemSize =  memsize;
   parameter RunLenWidth = fromInteger(valueOf(addr_sz));
   parameter TriggerToDump = (triggerToDump < (fromInteger(2 ** 16))) ? pack(triggerToDump) :
      error ("mkSerialCapture triggerToDump argument must be less than 2**16.");
   parameter MemAddrWidth = fromInteger(valueOf(addr_sz));

   port CTIMER = announceClocks;

   method        enabler    (CMD) enable(CMDEN)   clocked_by(uclock) reset_by(ureset);
   method DELAY  delayClocks()                    clocked_by(uclock) reset_by(ureset);
   interface PrbStr data;
      method DATAUP dataout    ()   ready(DATAVALID) clocked_by(uclock) reset_by(ureset);
      method        ack        ()    enable(ACK)     clocked_by(uclock) reset_by(ureset);
   endinterface
   method        trigger       () enable(TRIGGER)      clocked_by(cclock) reset_by(no_reset);
   method        probein    (PROBEIN) enable(PROBEEN)  clocked_by(cclock) reset_by(no_reset);

      schedule (enabler, delayClocks, data_dataout, data_ack, probein, trigger) CF (delayClocks, data_dataout, data_ack);

      schedule enabler C enabler;
      schedule probein C probein;
      schedule trigger C trigger;
      schedule trigger CF probein;
      schedule (enabler) CF (probein, trigger);
endmodule


module [mc] mkSerialCapture#(parameter UInt#(13) probeNumber,
			                 parameter String label,
                             parameter UInt#(addr_sz) triggerToDump,
			                 a sample
                             ) (CaptureIfc)
   provisos (IsModule#(mc, _a)
             ,Context#(mc, ProbeContext)
             ,Context#(mc, ClockContext)
             ,Bits#(a,sa)
	     ,Add#(_b, addr_sz, 32)
             );
   (*hide*)let _ifc <- mkSerialCapture1(tagged Explicit probeNumber,
					label, triggerToDump, sample);
   return _ifc;
endmodule

typedef enum { DISABLED, WAITING, FINISHING, OFFLOAD1, OFFLOAD2 } CaptureState deriving (Eq,Bits);

module [mc] mkSerialCaptureC#(parameter PrbNum num,
			      parameter String lbl,
			      parameter UInt#(addr_sz) trigger_point,
			      a x)(CaptureIfc)
   provisos (IsModule#(mc, _a),
	     Context#(mc, ProbeContext), Context#(mc, ClockContext),
	     Add#(_1, addr_sz, 32),
	     Bits#(a,sa),
	     Div#(TAdd#(sa, RunLengthSize), 32, nwords));
   UInt#(addr_sz) largest_window_addr = unpack('1);

   SceMiSerialInfo#(a) params <- mkSceMiSerialInfo(num, trigger_point, CAPTUREPROBE, lbl);

   ProbeContext pc <- getContext();
   ClockContext cc <- getContext();
   let cclk <- exposeCurrentClock;
   let crst <- exposeCurrentReset;
   let uclk = cc.clks[0];
   let urst = cc.rsts[0];

   Reg#(Bool) local_triggerR     <- mkDReg(False);
   ReadOnly#(Bool) local_trigger <- mkNullCrossingWire(uclk, local_triggerR);

   // This delivers a Valid result whenever a new sample is caught in the
   // cclock domain.  It is cleared either immediately (by cancelR) or when
   // transmission starts (by the deq method):
   ReadOnlyWithClear#(a) usample <- mkCtoUclock(x, uclk, urst);

   // clocked and reset by uclk and urst:
   module mkInternal(InternalIfc);
      let mx = usample;

      // capture state register
      Reg#(CaptureState) state <- mkReg(DISABLED);

      // BRAM to store data samples
      Integer mem_size = 2 ** valueOf(addr_sz);
      BRAM_PORT#(UInt#(addr_sz),a) bram <- mkBRAMCore1(mem_size,False);
      Reg#(a)    last_captured   <- mkRegU;

      // location of next empty slot
      Reg#(UInt#(addr_sz)) wr_ptr <- mkRegU;

      // location of first full slot
      Reg#(UInt#(addr_sz)) rd_ptr <- mkRegU;

      // track window extent before and after trigger point
      // the invariants are:
      //    start_offset <= trigger_point
      //    start_offset + end_offset <= largest_window_addr
      //    wr_ptr - end_offset == trigger address
      //    wr_ptr - end_offset - start_offset == first sample address
      Reg#(UInt#(addr_sz)) start_offset <- mkRegU;
      Reg#(UInt#(addr_sz)) end_offset   <- mkRegU;
      Reg#(UInt#(RunLengthSize)) trigger_offset    <- mkRegU;
      Reg#(UInt#(RunLengthSize)) run_length        <- mkRegU;

      // wires for communicating status from methods etc.
      PulseWire remote_trigger <- mkPulseWire;
      PulseWire start_capture  <- mkPulseWire;
      PulseWire stop_capture   <- mkPulseWire;
      PulseWire offloading     <- mkPulseWire;

      let got_trigger = local_trigger || remote_trigger;

      rule ignore_data if (state != WAITING && state != FINISHING);
	 usample.clear();
      endrule

      rule setEnabled (pc.enabler matches {.n, .m} &&& n==num);
	 case (m)
	    TRIGGER: if (state == WAITING) remote_trigger.send();
	    ENABLE:  start_capture.send();
	    default: stop_capture.send();
	 endcase
      endrule

      FIFOF#(Run#(a)) outFF <- mkFIFOF;

      Reg#(UInt#(16)) remaining <- mkReg(0); // remaining words in current
				             // part of transfer
      Bool idle = (remaining == 0);
      Reg#(Bool) starting    <- mkReg(False);
      Reg#(Bool) finishing   <- mkReg(False);

      // all state updates in one rule to eliminate spurious conflicts
      rule update_state;
	 (*split*)
	 // user disable overrides everything else
	 if (stop_capture) begin
	    state <= DISABLED;
	 end
	 // in the DISABLED state, only the user can initiate activity
	 else if (state == DISABLED && start_capture) begin
	    state <= WAITING;
	    start_offset <= 0;
	    end_offset <= 0;
	    rd_ptr <= 0;
	    wr_ptr <= 0;
	 end
	 // in the WAITING state, we collect samples and handle triggers
	 else if (state == WAITING) (*nosplit*)begin
	    if (mx matches tagged Valid .x) begin
	       usample.clear();
	       bram.put(True,wr_ptr,x);
	       wr_ptr <= wr_ptr + 1;
	       if (!got_trigger) begin
		  if (start_offset != trigger_point)
		     start_offset <= start_offset + 1;
		  else
		     rd_ptr <= rd_ptr + 1;
	       end
	       else begin
		  end_offset <= end_offset + 1;
	       end
	    end
	    if (got_trigger) begin
	       trigger_offset <= (start_offset < trigger_point) ? extend(wr_ptr) :
	       extend(trigger_point);
	       if (trigger_point == largest_window_addr)
		  state <= OFFLOAD1;
	       else
		  state <= FINISHING;
	    end
	 end
	 // in the FINISHING state, we collect samples
	 else if (state == FINISHING &&& mx matches tagged Valid .x) (*nosplit*)begin
	    usample.clear();
	    bram.put(True,wr_ptr,x);
	    wr_ptr <= wr_ptr + 1;
	    if (end_offset == largest_window_addr - trigger_point)
	       state <= OFFLOAD1;
	    end_offset <= end_offset + 1;
	 end
	 // in the OFFLOAD1 state, we initiate reading for offloading
	 else if (state == OFFLOAD1) begin
	    bram.put(False,rd_ptr,?);
	    state <= OFFLOAD2;
	    run_length <= 0;
	    starting <= True;
	 end
	 // in the OFFLOAD2 state we either send the data or refetch
	 else if (state == OFFLOAD2) (*nosplit*)begin
	    let next = rd_ptr + 1;
	    // data *may* be sent from offload_data rule
	    if (offloading) begin
	       if (next == wr_ptr)
		  begin
		     state <= WAITING;
		     start_offset <= 0;
		     end_offset <= 0;
		     next = 0;
		     wr_ptr <= 0;
		     finishing <= True;
		  end
	       else
		  bram.put(False,next,?);
	       rd_ptr <= next;
	    end
	    else
	       bram.put(False,rd_ptr,?); // refetch
	 end
      endrule

      Bool mustSend = (state == OFFLOAD1 || state == OFFLOAD2);
      Bool trigger_delay = mustSend || !idle;

      Integer num_words = valueOf(nwords);
      Vector#(nwords, Reg#(Bit#(32))) words <- replicateM(mkRegU);

      Reg#(Bit#(32)) resR <- mkReg(nullWord);

      // handle sending data to host:
      rule offload_data if (state == OFFLOAD2);
	 let data = bram.read();
	 offloading.send();
	 //$display("Capture read (%0d): %h %0d", num, data, run_length);
	 if ((pack(last_captured) != pack(data)) ||
	     (run_length == 0) || // Make sure first data is sent
	     (wr_ptr == rd_ptr+1)) // Make sure last data is sent
	    begin
               //$display("* Capture send (%0d): %h %0d", num, data, run_length);
	       outFF.enq(Run {probe: data, runlength:run_length});
	       last_captured <= data;
	       run_length <= 1;
	    end
	 else
	    run_length <= run_length + 1;
      endrule

      rule start_offload (outFF.notEmpty && idle && starting);
	 resR <= pack(FirstWord {num: num, nWords: fromInteger(num_words)});
	 writeVReg(words,toChunks(outFF.first()));
	 outFF.deq;
	 remaining <= fromInteger(num_words);
	 starting  <= False;
      endrule

      PulseWire acked <- mkPulseWire();
      rule offload_next if (acked && !starting);
	 // Other end will only accept nullWord once per cclock:
	 Bit#(32) res = nullWord;
	 UInt#(16) rem = remaining;

	 if (!idle) begin
	    res = words[0]; // send less significant end first
	    for (Integer n = 1; n < num_words; n = n+1)
	       words[n-1] <= words[n];
	    rem = rem - 1;
	 end
	 else if (outFF.notEmpty) begin
	    res = pack(FirstWord {num: num, nWords: fromInteger(num_words)});
	    writeVReg(words,toChunks(outFF.first()));
	    outFF.deq;
	    rem = fromInteger(num_words);
	 end
	 else if (finishing) begin
	    finishing <= False;
	 end

	 remaining <= rem;
	 resR <= res;
      endrule

      method probeDelayClocks = trigger_delay;

      interface PrbStr data;

	 method Bit#(32) dataout() if (!starting && (outFF.notEmpty || !idle || finishing));
	    return resR;
	 endmethod

	 method Action ack();
	    acked.send();
	 endmethod

      endinterface
   endmodule

   let serCptInt <- mkInternal(clocked_by cc.clks[0], reset_by cc.rsts[0]);
   pc.delays = List::cons(serCptInt.probeDelayClocks(),pc.delays);
   pc.ds = tagged Append tuple2(pc.ds, tagged One serCptInt.data);
   putContext(pc);

   method Action trigger();
      local_triggerR <= True;
   endmethod
endmodule

module [mc] mkSerialCapture1#(parameter PrbNum probeNumber,
			      parameter String label,
			      parameter UInt#(addr_sz) triggerToDump,
                              a sample
			      ) (CaptureIfc)
   provisos (IsModule#(mc, _a)
             ,Context#(mc, ProbeContext)
             ,Context#(mc, ClockContext)
             ,Bits#(a,sa)
	     ,Add#(_b, addr_sz, 32)
             );

      if (genC) begin
	 (*hide*)let _ifc <- mkSerialCaptureC(probeNumber, label, triggerToDump, sample);
	 return _ifc;
      end
      else begin
         Integer memsize = 2 ** valueOf(addr_sz);
         SceMiSerialInfo#(a) params <- mkSceMiSerialInfo2(probeNumber, CAPTUREPROBEV2, label,
                                                          valueOf(addr_sz),
                                                          memsize );

         ProbeContext pc <- getContext();
         Clock cclock <- exposeCurrentClock();
         // Get the clock context and extact the U clock/reset
         ClockContext cc <- getContext();
         let uclk = cc.clks[0];
         let urst = cc.rsts[0];

         VProbeCapture#(a) _prb <- vProbeCapture(probeNumber
                                                 ,memsize
                                                 ,cclock
                                                 ,triggerToDump
                                                 ,pc.advanceCclock
                                                 ,clocked_by(uclk), reset_by(urst));

         (* fire_when_enabled *)      // implicit conditions are OK
         rule probeValue (True);
            _prb.probein(sample);
         endrule
         (* fire_when_enabled *)      // implicit conditions are OK
         rule probeEnabler (True);
            _prb.enabler(pc.enabler);
         endrule

         List#(Bool) probeDelayClockss = List::cons(_prb.delayClocks,pc.delays);
         pc.delays = probeDelayClockss;
         pc.ds = tagged Append tuple2(pc.ds, tagged One _prb.data);
         putContext(pc);

         method Action trigger = _prb.trigger;
      end
endmodule
////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////
instance Expose#(ProbeContext, ProbeContextIfc, CLOCKCONTEXTSIZE);
   module unburyContextWithClocks#(ProbeContext ps, ClockContext cc)(ProbeContextIfc);
      let uclock = cc.clks[0];
      let ureset = cc.rsts[0];

      List#(PrbStr) pds = flatten(ps.ds);
      Reg#(UInt#(16)) current <- mkRegU(clocked_by uclock, reset_by ureset);
      Reg#(UInt#(16)) remaining <- mkReg(0, clocked_by uclock, reset_by ureset);

      FIFOF#(Bit#(32))  resFF <- mkFIFOF(clocked_by uclock, reset_by ureset);

      // Use FIFOFs to convert SendCommit pds interfaces to FIFOF
      // interfaces that we can use inside the module
      module convert_to_FIFO#(PrbStr prb_str_in)(FIFOF#(Bit#(32)));
	 FIFOF#(Bit#(32))      prb_fifo     <- mkFIFOF(clocked_by uclock, reset_by ureset);
	 RecvCommit#(Bit#(32)) prb_str_recv <- mkRecvCommit(prb_fifo, clocked_by uclock, reset_by ureset);
	 mkConnection(prb_str_in, prb_str_recv, clocked_by uclock, reset_by ureset);
	 return prb_fifo;
      endmodule

      function Bool getNotEmpty (FIFOF#(a) f) = f.notEmpty;

      List#(FIFOF#(Bit#(32))) prb_strs <- List::mapM(convert_to_FIFO,pds);
      List#(Bool)   localNotEmptys = List::map(getNotEmpty, prb_strs);

      Rules rs = emptyRules;

      for (Integer ii=0; ii<List::length(pds); ii=ii+1)
	 begin
	    let i = fromInteger(ii);
	    FIFOF#(Bit#(32)) pd = prb_strs[i];
	    let v = pd.first();

	    rule takeMore (remaining!=0 && current==i);
	       resFF.enq(v);
	       pd.deq();
	       remaining <= remaining-1;
	    endrule

	    Rules r = (rules
	       rule takeOne (remaining==0);
		  resFF.enq(v);
		  pd.deq();
		  let r = unpack(v).nWords;
		  remaining <= (isSpecial(r) ? /*(unpack(pack(r)[1:0])==NULL ? 2 : 0)*/0 : r);
		  current <= i;
	       endrule
		       endrules);
	    rs = rJoinDescendingUrgency(rs, r);
	 end
      addRules(rs);

//      Bool probeDelayClockss = List::\or (ps.delays) || (remaining != 0) || resFF.notEmpty;
      Bool probeDelayClockss = List::\or (ps.delays) || List::\or (localNotEmptys) || resFF.notEmpty;

      PrbStr sendC <- mkSendCommit(resFF, clocked_by uclock, reset_by ureset);
      interface PrbStr words = sendC;

      method Action enblr(Tuple2#(PrbNum, EnableMode) x);
	 ps.enabler <= x;
      endmethod

      method probeDelayClocks = probeDelayClockss;

      method Action advanceCclock ();
         ps.advanceCclock.send;
      endmethod

   endmodule
endinstance

instance Hide#(mc, ProbeContextIfc)
   provisos (IsModule#(mc, a), Context#(mc, ProbeContext));
   module [mc] reburyContext#(ProbeContextIfc i)(Empty);
      ProbeContext c <- getContext();

      rule xmit_enblr;
	 i.enblr(c.enabler);
      endrule

      rule xmit_advanceCclock (c.advanceCclock) ;
         i.advanceCclock();
      endrule
      List#(Bool) probeDelayClockss = List::cons(i.probeDelayClocks,c.delays);

      c.delays = probeDelayClockss;
      c.ds = tagged Append tuple2(c.ds, tagged One i.words);
      putContext(c);
   endmodule
endinstance

module mkInitialProbeContextWithClocks#(Vector#(CLOCKCONTEXTSIZE, Clock) cks,
					Vector#(CLOCKCONTEXTSIZE, Reset) rs)
   (CompleteProbeContexts)
   provisos (Gettable#(CompleteProbeContexts, ClockContext));

   let initC <- mkInitialClockContextWithClocks(cks, rs);
   (*hide_all*) let initP <- mkInitialProbeContext(clocked_by cks[0], reset_by rs[0]);
   return hList2(initP, initC);
endmodule

module [SceMiModule] runWithProbes#(ProbeModule#(top) mkTop,
				    SceMiClockPortIfc clk_port,
				    Maybe#(Reset) mDutReset)(top);

   // The controlled clock, and associated reset (note that by default the
   // modules instantiated here are clocked by the uncontrolled clock):
   Clock c_clock = clk_port.cclock;
   Reset c_reset = clk_port.creset;

   // Construct a description of the probe configuration:
   SceMiProbeConfiguration probe_conf;
   probe_conf.clockNum  = 0; // hard-wired for the time being

   // The dut stuff:

   let uclock <- sceMiGetUClock();
   let ureset <- sceMiGetUReset();

   Vector#(CLOCKCONTEXTSIZE, Clock) clocks = replicate(noClock);
   Vector#(CLOCKCONTEXTSIZE, Reset) resets = replicate(noReset);
   clocks[0] = uclock;  resets[0] = ureset;
   clocks[1] = c_clock; resets[1] = c_reset;

   (*hide_all*) let initProbe <- mkInitialProbeContextWithClocks(clocks, resets
                                                                 ,clocked_by uclock, reset_by ureset);
   (*hide_all*) CompleteProbeContexts probeHook <- addProbeHookToContext(initProbe
                                                                         ,clocked_by uclock, reset_by ureset);
   let initP = probeHook ;

   Reset dutReset = c_reset;
   if (mDutReset matches tagged Valid .r) dutReset = r;

   // The dut is in the controlled domain and reset by the new reset:
   match {.ctxtout, .dutIfc} <- liftModule(runWithCompleteContext(initP, mkTop,
								  clocked_by c_clock,
								  reset_by dutReset));
      ClockContext cc = getIt(ctxtout);

   match {.p_ifc, .*} <- unburyContextWithClocks(ctxtout, cc);

   (*hide_all*) let prb_control <- mkSceMiSerialXactor(probe_conf, p_ifc
                                                       ,clocked_by uclock, reset_by ureset);

   return  dutIfc;
endmodule

module [SceMiModule] runWithProbes2CC#(ProbeModule#(top) mkTop,
				       SceMiClockPortIfc clk_port,
				       SceMiClockPortIfc clk2_port,
				       Maybe#(Reset) mDutReset)(top);

   // The controlled clock, and associated reset (note that by default the
   // modules instantiated here are clocked by the uncontrolled clock):
   Clock c_clock = clk_port.cclock;
   Reset c_reset = clk_port.creset;

   // An additional controlled clock, and associated reset:
   Clock c2_clock = clk2_port.cclock;
   Reset c2_reset = clk2_port.creset;

   // Construct a description of the probe configuration:
   SceMiProbeConfiguration probe_conf;
   probe_conf.clockNum  = 0; // hard-wired for the time being

   // The dut stuff:

   let uclock <- sceMiGetUClock();
   let ureset <- sceMiGetUReset();

   Vector#(CLOCKCONTEXTSIZE, Clock) clocks = replicate(noClock);
   Vector#(CLOCKCONTEXTSIZE, Reset) resets = replicate(noReset);
   clocks[0] = uclock;
   clocks[1] = c_clock;
   clocks[2] = c2_clock;

   resets[0] = ureset;
   resets[1] = c_reset;
   resets[2] = c2_reset;

   let initProbe <- mkInitialProbeContextWithClocks(clocks, resets, clocked_by uclock, reset_by ureset);
   CompleteProbeContexts probeHook <- addProbeHookToContext(initProbe, clocked_by uclock, reset_by ureset);
   let initP = probeHook;

   Reset dutReset = c_reset;
   if (mDutReset matches tagged Valid .r) dutReset = r;

   // The dut is in the controlled domain and reset by the new reset:
   match {.ctxtout, .dutIfc} <- liftModule(runWithCompleteContext(initP, mkTop,
								  clocked_by c_clock,
								  reset_by dutReset));
      ClockContext cc = getIt(ctxtout);

   match {.p_ifc, .*} <- unburyContextWithClocks(ctxtout, cc);

   let prb_control <- mkSceMiSerialXactor(probe_conf, p_ifc, clocked_by uclock, reset_by ureset);

   return  dutIfc;
endmodule

module [SceMiModule] runWithClocks#(ClockModule#(top) mkTop,
				    SceMiClockPortIfc clk_port,
				    Maybe#(Reset) mDutReset)(top);

   SceMiModuleState state <- getContext();
   // The controlled clock, and associated reset (note that by default the
   // modules instantiated here are clocked by the uncontrolled clock):
   Clock c_clock = clk_port.cclock;
   Reset c_reset = clk_port.creset;

   // Add a hook and xactor for probes -- all modules have probes
   SceMiProbeConfiguration probe_conf;
   probe_conf.clockNum  = 0; // hard-wired for the time being
   let probeHook   <- mkProbeHook; // clocked by uclock, ureset
   let prb_control <- mkSceMiSerialXactor(probe_conf, probeHook);

   // The dut stuff:

   let uclock = getUClock(state);
   let ureset = getUReset(state);

   Vector#(CLOCKCONTEXTSIZE, Clock) clocks = replicate(noClock);
   Vector#(CLOCKCONTEXTSIZE, Reset) resets = replicate(noReset);
   clocks[0] = uclock;  resets[0] = ureset;
   clocks[1] = c_clock; resets[1] = c_reset;
   (*hide_all*) let initClocks <- mkInitialClockContextWithClocks(clocks, resets);

   Reset dutReset = c_reset;
   if (mDutReset matches tagged Valid .r) dutReset = r;

   // The dut is in the controlled domain and reset by the new reset:
   match {.ctxtout, .dutIfc} <- liftModule(runWithCompleteContext(hList1(initClocks), mkTop,
								  clocked_by c_clock,
								  reset_by dutReset));
   return  dutIfc;
endmodule

module [SceMiModule] runWithoutProbes#(Module#(top) mkTop,
				       SceMiClockPortIfc clk_port,
				       Maybe#(Reset) mDutReset)(top);

   // The controlled clock, and associated reset (note that by default the
   // modules instantiated here are clocked by the uncontrolled clock):
   Clock c_clock = clk_port.cclock;
   Reset c_reset = clk_port.creset;
   let uclock <- sceMiGetUClock();
   let ureset <- sceMiGetUReset();

   // The dut stuff:

   Reset dutReset = c_reset;
   if (mDutReset matches tagged Valid .r) dutReset = r;

   // Add a hook and xactor for probes -- all modules have probes
   SceMiProbeConfiguration probe_conf;
   probe_conf.clockNum  = 0; // hard-wired for the time being
   let probeHook   <- mkProbeHook(clocked_by uclock, reset_by ureset); // clocked by uclock, ureset
   let prb_control <- mkSceMiSerialXactor(probe_conf, probeHook, clocked_by uclock, reset_by ureset);

   // The dut is in the controlled domain and reset by the new reset:
   let dutIfc <- liftModule(mkTop, clocked_by c_clock, reset_by dutReset);

   return  dutIfc;
endmodule

typeclass WithOrWithoutProbes#(type m);
   module [SceMiModule] buildDut#(m#(top) mkTop, SceMiClockPortIfc clk_port)(top);
endtypeclass

instance WithOrWithoutProbes#(Module);
   module [SceMiModule] buildDut#(Module#(top) mkTop, SceMiClockPortIfc clk_port)(top);
      (*hide*)
      let _ifc <- runWithoutProbes(mkTop, clk_port, Invalid);
      return _ifc;
   endmodule
endinstance

instance WithOrWithoutProbes#(ProbeModule);
   module [SceMiModule] buildDut#(ProbeModule#(top) mkTop, SceMiClockPortIfc clk_port)(top);
      (*hide*)
      let _ifc <- runWithProbes(mkTop, clk_port, Invalid);
      return _ifc;
   endmodule
endinstance

instance WithOrWithoutProbes#(ClockModule);
   module [SceMiModule] buildDut#(ClockModule#(top) mkTop, SceMiClockPortIfc clk_port)(top);
      (*hide*)
      let _ifc <- runWithClocks(mkTop, clk_port, Invalid);
      return _ifc;
   endmodule
endinstance

typeclass WithOrWithoutProbesReset#(type m);
   module [SceMiModule] buildDutWithReset#(m#(top) mkTop, SceMiClockPortIfc clk_port, Reset dutReset)(top);
endtypeclass

instance WithOrWithoutProbesReset#(Module);
   module [SceMiModule] buildDutWithReset#(Module#(top) mkTop, SceMiClockPortIfc clk_port, Reset dutReset)(top);
      (*hide*)
      let _ifc <- runWithoutProbes(mkTop, clk_port, tagged Valid dutReset);
      return _ifc;
   endmodule
endinstance

instance WithOrWithoutProbesReset#(ProbeModule);
   module [SceMiModule] buildDutWithReset#(ProbeModule#(top) mkTop, SceMiClockPortIfc clk_port, Reset dutReset)(top);
      (*hide*)
      let _ifc <- runWithProbes(mkTop, clk_port, tagged Valid dutReset);
      return _ifc;
   endmodule
endinstance

instance WithOrWithoutProbesReset#(ClockModule);
   module [SceMiModule] buildDutWithReset#(ClockModule#(top) mkTop, SceMiClockPortIfc clk_port, Reset dutReset)(top);
      (*hide*)
      let _ifc <- runWithClocks(mkTop, clk_port, tagged Valid dutReset);
      return _ifc;
   endmodule
endinstance

// =====

typeclass Probifiable#(type i, type m);
   module [m] mk_p#(m#(i) mkM, UInt#(13) n, String s)(i);
endtypeclass

typeclass CProbifiable#(type i, type m, numeric type sz);
   module [m] mk_c#(m#(i) mkM, UInt#(13) n, String s, UInt#(sz) offst)(i);
endtypeclass

instance Probifiable#(Reg#(t), m)
   provisos (Bits#(t,st),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_p#(m#(Reg#(t)) mkM, UInt#(13) n, String s)(Reg#(t));
      (*hide*)
      Reg#(t) _ifc <- mkM;
      mkSerialProbe(n,s, _ifc);
      return _ifc;
   endmodule
endinstance
instance CProbifiable#(Reg#(t), m, sz)
   provisos (Bits#(t,st), Add#(_b, sz, 32),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_c#(m#(Reg#(t)) mkM, UInt#(13) n, String s, UInt#(sz) offst)(Reg#(t));
      (*hide*)
      Reg#(t) _ifc <- mkM;
      mkSerialCapture(n,s, offst, _ifc);
      return _ifc;
   endmodule
endinstance

instance Probifiable#(RWire#(t), m)
   provisos (Bits#(t,st), Eq#(t),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_p#(m#(RWire#(t)) mkM, UInt#(13) n, String s)(RWire#(t));
      (*hide*)
      RWire#(t) _ifc <- mkM;
      mkSerialProbe(n,s, _ifc.wget);
      return _ifc;
   endmodule
endinstance
instance CProbifiable#(RWire#(t), m, sz)
   provisos (Bits#(t,st), Add#(_b, sz, 32),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_c#(m#(RWire#(t)) mkM, UInt#(13) n, String s, UInt#(sz) offst)(RWire#(t));
      (*hide*)
      RWire#(t) _ifc <- mkM;
      mkSerialCapture(n,s, offst, _ifc.wget);
      return _ifc;
   endmodule
endinstance

instance Probifiable#(FIFO#(t), m)
   provisos (Bits#(t,st), Eq#(t),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_p#(m#(FIFO#(t)) mkM, UInt#(13) n, String s)(FIFO#(t));
      (*hide*)
      FIFO#(t) _ifc <- mkM;
      RWire#(t) rw <- mkRWire;
      rule test_ff;
	 rw.wset(_ifc.first);
      endrule
      mkSerialProbe(n,s, rw.wget);
      return _ifc;
   endmodule
endinstance
instance CProbifiable#(FIFO#(t), m, sz)
   provisos (Bits#(t,st), Add#(_b, sz, 32),
	     IsModule#(m,_a), Context#(m, ProbeContext), Context#(m, ClockContext));
   module [m] mk_c#(m#(FIFO#(t)) mkM, UInt#(13) n, String s, UInt#(sz) offst)(FIFO#(t));
      (*hide*)
      FIFO#(t) _ifc <- mkM;
      RWire#(t) rw <- mkRWire;
      rule test_ff;
	 rw.wset(_ifc.first);
      endrule
      mkSerialCapture(n,s, offst, rw.wget);
      return _ifc;
   endmodule
endinstance

// =====
// Shorthands for some common probed registers:

module [mc] mkReg_p#(t x, UInt#(13) num, String lbl)(Reg#(t))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext), Bits#(t, st));

   (*hide*)
   Reg#(t) _ifc <- mkReg(x);
   mkSerialProbe(num, lbl, _ifc);
   return _ifc;
endmodule

module [mc] mkReg_c#(t x, UInt#(13) num, String lbl, UInt#(16) tpt)(Reg#(t))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext), Bits#(t, st), Eq#(t));

   (*hide*)
   Reg#(t) _ifc <- mkReg(x);
   mkSerialCapture(num, lbl, tpt, _ifc);
   return _ifc;
endmodule

module [mc] mkRegU_p#(UInt#(13) num, String lbl)(Reg#(t))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext), Bits#(t, st));

   (*hide*)
   Reg#(t) _ifc <- mkRegU;
   mkSerialProbe(num, lbl, _ifc);
   return _ifc;
endmodule

module [mc] mkRegU_c#(UInt#(13) num, String lbl, UInt#(16) tpt)(Reg#(t))
   provisos (IsModule#(mc, _a), Context#(mc, ProbeContext),
	     Context#(mc, ClockContext), Bits#(t, st));

   (*hide*)
   Reg#(t) _ifc <- mkRegU;
   mkSerialCapture(num, lbl, tpt, _ifc);
   return _ifc;
endmodule

// Isomorphic to ProbeContextIfc with always_ready attributes
interface VProbeHookIfc;
   interface PrbStr words;
   (*always_ready*)
   method Action enblr (Tuple2#(PrbNum, EnableMode) cmd);
   (*always_ready*)
   method Action advanceCclock();
  (*always_ready*)
   method Bool probeDelayClocks;
endinterface


import "BVI" ProbeHook =
module vProbeHook(VProbeHookIfc);
   default_clock clk(UCLK);
   default_reset rst(URST);

   interface PrbStr words;
      method DATAUP dataout ()     ready(DATAVALID);
      method ack () enable(ACK);
   endinterface
   method DELAY probeDelayClocks();
   method enblr (CMD) enable(CMDEN);
   method advanceCclock () enable(CTIMER);


      schedule (probeDelayClocks, words_dataout) CF (probeDelayClocks, words_dataout);
      schedule (probeDelayClocks, words_dataout) SB (words_ack) ;
      schedule (words_ack) C (words_ack);
      schedule enblr C enblr;
      schedule enblr CF (words_dataout, words_ack, probeDelayClocks);
      schedule advanceCclock C advanceCclock;
      schedule advanceCclock CF (words_dataout, words_ack, probeDelayClocks, enblr);

endmodule

// this is a hook to find the probe context in a synthesized verilog context.
module  addProbeHookToContext #(CompleteProbeContexts cin) (CompleteProbeContexts ifc)
   provisos ( Gettable#(CompleteProbeContexts,ProbeContext),
              Gettable#(CompleteProbeContexts,ClockContext)
             ) ;

   CompleteProbeContexts newcxt = cin;
   if (genVerilog) begin
      ProbeContext pc = getIt (cin);
      ClockContext cc = getIt (cin);

      // grab the uncontrolled clock and reset
      VProbeHookIfc _hook <- vProbeHook(clocked_by cc.clks[0], reset_by cc.rsts[0]);
      rule connectCommand ;
         _hook.enblr ( pc.enabler );
      endrule
      rule advanceCC (pc.advanceCclock);
         _hook.advanceCclock;
      endrule

      // Update context
      pc.delays = List::cons(_hook.probeDelayClocks,pc.delays);
      pc.ds = tagged Append tuple2 (pc.ds, tagged One _hook.words);
      newcxt = putIt (cin, pc);
   end

   return newcxt;
endmodule

module mkProbeHook (ProbeContextIfc);
   ProbeContextIfc ifc =
   (interface ProbeContextIfc
       interface PrbStr words ;
          method dataout () if (False);
             return 0;
          endmethod
          method Action ack();
             noAction;
          endmethod
       endinterface
       method probeDelayClocks = False;
       method Action enblr (Tuple2#(PrbNum, EnableMode) cmd);
          noAction;
       endmethod
       method Action advanceCclock ();
          noAction;
       endmethod
    endinterface);

   if (genVerilog) begin
      (*hide*)
      let _hook <- vProbeHook;

      ifc =
      (interface ProbeContextIfc
          interface PrbStr words = _hook.words;
          method Action enblr (Tuple2#(PrbNum, EnableMode) x);
             _hook.enblr(x);
          endmethod
          method Bool probeDelayClocks = _hook.probeDelayClocks;
       method Action advanceCclock = _hook.advanceCclock;
       endinterface);
   end
   return ifc;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////

typedef enum { IDLE, START, IN_HEADER, SCAN_IN, ST_HEADER, SCAN_ST, WAIT } ScanState deriving (Eq, Bits);

interface CosimObserve#(numeric type in_size, numeric type st_size);
   (* prefix = "" *)
   interface VProbeIfc          probe;
   (* prefix = "ST" *)
   interface ScanDual#(st_size) dual_st;
   (* prefix = "IN" *)
   interface ScanDual#(in_size) dual_in;
   (* always_ready, always_enabled, result = "SCAN_ANY" *)
   method Bool scan_any;
endinterface

interface VProbeIfc;
   (* prefix = "" *)
   interface PrbSVtr words;
   (* always_ready,  prefix = "", enable = "CMDEN" *)
   method Action enblr ((* port = "CMD" *) Tuple2#(PrbNum, EnableMode) cmd);
   (* always_ready, result = "DELAY" *)
   method Bool probeDelayClocks;
   (*always_ready,  prefix = "", enable = "TRIGGER" *)
   method Action trigger;
endinterface

(* synthesize, no_default_reset *)
module cosimObserve#(parameter UInt#(13) id, parameter ST length_0, parameter ST length_1) ((* osc="UCLK" *) Clock uclk, (* reset="URST" *) Reset urst, CosimObserve#(32, 32) ifc);
   let _ifc <- cosimObserveP(uclk, urst, id, length_0, length_1);
   return _ifc;
endmodule

module cosimObserveP#(Clock uclk, Reset urst, parameter UInt#(13) id, ST length_0, ST length_1) (CosimObserve#(m, n))
   provisos (Add#(ignore0, m, 32),
	     Add#(ignore1, n, 32));
   Wire#(Bool) scan_any_wire      <- mkBypassWire(clocked_by uclk, reset_by urst);
   ScanOutConnector#(m) sc_in     <- mkScanOutConnector(clocked_by uclk, reset_by urst);
   ScanOutConnector#(n) sc_st     <- mkScanOutConnector(clocked_by uclk, reset_by urst);
   let _probe <- mkCosimObserve(uclk, urst, sc_st.scan, sc_in.scan, id, scan_any_wire, length_0, length_1);
   interface probe    = _probe;
   interface dual_st  = sc_st.dual;
   interface dual_in  = sc_in.dual;
   method    scan_any = scan_any_wire;
endmodule

module mkCosimObserve#(Clock uclk, Reset urst, ScanOut#(n) scan_st, ScanOut#(m) scan_in, UInt#(13) num, Wire#(Bool) scan_any_wire, ST length_st, ST length_in) (VProbeIfc)
   provisos (Add#(ignore0, m, 32),
	     Add#(ignore1, n, 32),
	     Log#(m, lm),
	     Log#(n, ln));

   let cclk <- exposeCurrentClock;

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   PulseWire       stall_pw     <- mkPulseWire(clocked_by uclk, reset_by urst);
   Reg#(UInt#(16)) remaining    <- mkReg(0, clocked_by uclk, reset_by urst);
   Reg#(Bool)      first        <- mkReg(False, clocked_by uclk, reset_by urst);
   Reg#(Bool)      modeu_prev   <- mkRegU(clocked_by uclk, reset_by urst);
   Reg#(Bool)      shutdown     <- mkReg(False, clocked_by uclk, reset_by urst);
   Reg#(ScanState) scan_state   <- mkReg(IDLE, clocked_by uclk, reset_by urst);

   Reg#(Bool)      enabled      <- mkReg(False, clocked_by uclk, reset_by urst);

   CrossingReg#(Bool) mode      <- mkNullCrossingRegU(uclk);

   PulseWire          trig_pw   <- mkPulseWire(reset_by noReset);
   CrossingReg#(Bool) doTrigger <- mkNullCrossingRegU(uclk);
   PulseWire          start_pw  <- mkPulseWireOR(clocked_by uclk, reset_by urst);

   rule flip_on_cclk;
      mode <= !mode;
   endrule

   rule set_trigger;
      doTrigger <= trig_pw;
   endrule

   // The lengths are all really static values.
   UInt#(16) cnt_in = truncate(((length_in - 1) >> valueOf(lm)) + 1);
   UInt#(16) cnt_st = truncate(((length_st - 1) >> valueOf(ln)) + 1);

   // scan chain 0 is state, scan chain 1 is inputs;

   let scan_num_st = 0;
   let delay_st    = 0;

   let scan_num_in = 1;
   let delay_in    = 0;

   // encode the scan number in the probe number;
   PrbNum num_mod_st = tagged Cosim
			  CosimStruct {
			     delay: delay_st,
			     scan_num: scan_num_st,
			     num: truncate(num) };

   PrbNum num_mod_in = tagged Cosim
			  CosimStruct {
			     delay: delay_in,
			     scan_num: scan_num_in,
			     num: truncate(num) };

   Bool allow_next = scan_state == WAIT && mode.crossed != modeu_prev;

   FIFOLevelIfc#(Bit#(32), 3) fifo <- mkFIFOLevel(clocked_by uclk, reset_by urst);

   Wire#(Tuple2#(PrbNum, EnableMode)) pc_enabler <- mkWire(clocked_by uclk, reset_by urst);

   rule stall (fifo.isGreaterThan(1));
      stall_pw.send;
   endrule

   // clocked and reset by uclk and urst:
   module mkInternal(InternalIfc);

      rule every;
	 modeu_prev <= mode.crossed;
      endrule

      rule drive_scan_inputs;
	 scan_st.scan_mode(scan_state == SCAN_ST && !stall_pw);
	 scan_in.scan_mode(scan_state == SCAN_IN && !stall_pw);
      endrule

      rule start (scan_state == START);
	 scan_state <= IN_HEADER;
      endrule

      rule in_header (scan_state == IN_HEADER && !stall_pw);
	 fifo.enq(pack(FirstWord {num: num_mod_in, nWords: cnt_in}));
	 scan_state <= SCAN_IN;
	 remaining <= cnt_in;
      endrule

      rule grab_in_output(scan_state == SCAN_IN && !stall_pw);
	 remaining <= remaining - 1;
	 fifo.enq({0, scan_in.scan_out});
	 if ((remaining - 1) == 0) scan_state <= (first) ? ST_HEADER : WAIT;
      endrule

      rule state_header (scan_state == ST_HEADER && !stall_pw);
	 fifo.enq(pack(FirstWord {num: num_mod_st, nWords: cnt_st}));
	 scan_state <= SCAN_ST;
	 remaining <= cnt_st;
	 first <= False;
      endrule

      rule grab_st_output(scan_state == SCAN_ST && !stall_pw);
	 remaining <= remaining - 1;
	 fifo.enq({0, scan_st.scan_out});
	 if ((remaining - 1) == 0) scan_state <= WAIT;
      endrule

      rule restart (allow_next);
	 scan_state <= START;
      endrule

      rule startfromLocalTrigger (doTrigger.crossed &&
				  enabled &&
				  scan_state == IDLE);
	 start_pw.send;
      endrule

      rule startfromRemoteTrigger (pc_enabler matches {tagged Explicit .n, .m} &&&
				   n == num &&&
				   m == TRIGGER &&&
				   enabled &&&
				   scan_state == IDLE);
	 start_pw.send;
      endrule

      rule setEnabled (pc_enabler matches {tagged Explicit .n, .m} &&&
		       n == num &&&
		       m == ENABLE &&&
		       scan_state == IDLE);
	 enabled <= True;
	 if (doTrigger.crossed) start_pw.send;
      endrule

      rule do_start (start_pw && scan_state == IDLE);
	 first <= True;
	 scan_state <= WAIT;
      endrule

      rule setDisabled (pc_enabler matches {tagged Explicit .n, .m} &&&
			n == num &&&
			m == DISABLE &&&
			!(scan_state == IDLE)
			&&& !shutdown);
	 enabled  <= False;
	 shutdown <= True;
      endrule

      Bool trigger_delay = (scan_state != IDLE && scan_state != WAIT) || (allow_next && scan_state == WAIT) || shutdown;

      (* preempts="do_shutdown, restart" *)
      rule do_shutdown (shutdown && scan_state == WAIT);
	 scan_state <= IDLE;
	 shutdown   <= False;
      endrule
      PrbStr sendC <- mkSendCommit(fifo);

      method probeDelayClocks = trigger_delay;
      interface PrbStr data = sendC;
   endmodule

   let serPrbInt <- mkInternal(clocked_by uclk, reset_by urst);

   let prbsvtr   <- toPrbSV(serPrbInt.data, clocked_by uclk, reset_by urst);

   rule send_any;
//      scan_any_wire <= (scan_state != IDLE && scan_state != WAIT && scan_state != START);
      scan_any_wire <= scan_state != IDLE && (scan_state != WAIT || mode.crossed != modeu_prev);
   endrule

   interface PrbSVtr words = prbsvtr;
   method Action enblr (Tuple2#(PrbNum, EnableMode) cmd);
      pc_enabler <= cmd;
   endmethod
   method probeDelayClocks             = serPrbInt.probeDelayClocks;
   method Action trigger;
      trig_pw.send;
   endmethod

endmodule

interface ErrorProbe;
   method Action reportError;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: SceMiSerialProbe
