// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision: 20418 $
// $Date: 2010-05-03 09:50:10 -0400 (Mon, 03 May 2010) $

package SceMiProbe;

import GetPut::*;
import Connectable::*;
import SceMi::*;
import AlignedFIFOs::*;

export SceMiProbeXactorIfc(..), mkSceMiProbeXactor;
export SceMiProbeConfiguration(..);

typedef struct {
   Integer clockNum;    // The clockNum of the SceMiClockPort that controls the DUT
} SceMiProbeConfiguration;

typedef struct {
    probes probe;
    SceMiCycleStamp cycles;
} Probes#(type probes)
deriving (Bits);

interface SceMiProbeXactorIfc#(type probes);
   method Action in_probes(probes p);
endinterface: SceMiProbeXactorIfc

module [SceMiModule] mkSceMiProbeXactor#(Clock cclock, Reset creset
					,parameter SceMiProbeConfiguration conf
					)
                                        (SceMiProbeXactorIfc#(probes) ifc)
   provisos(Bits#(probes, probes_sz), Eq#(probes));

   Integer found_error = 0;

   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // SceMi In and Out port and probes for talking to the software host
   SceMiMessageOutPortIfc#(Probes#(probes))   probe_out <- mkSceMiMessageOutPort();
   SceMiMessageOutPortIfc#(Bool)             status_out <- mkSceMiMessageOutPort();
   SceMiMessageInPortIfc#(Bool)              control_in <- mkSceMiMessageInPort ();

   // Synchronizers for going to and coming from the DUT
   // Aligned FIFO with 2 registers for storage
   Store#(UInt#(1), Probes#(probes), 0 ) store <- mkRegVectorStore(cclock, uclock, reset_by creset);
   AlignedFIFO#(Probes#(probes)) probeFifo <- mkAlignedFIFO( cclock, creset, uclock, ureset, store, True, True);

   // Clock controllers for dut and probes
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl(conf.clockNum, probeFifo.dNotFull, probeFifo.dNotFull);

   // keep last probe value to only send changes.   Always send value when enabled.
   Reg#(Bool)      forceChange <- mkReg(True, clocked_by uclock, reset_by ureset);
   Reg#(probes)    lastProbe   <- mkRegU(clocked_by uclock, reset_by ureset);

   // Register for keeping track of cycle_stamp
   Reg#(SceMiCycleStamp) cycleStamp <- mkReg(0, clocked_by cclock, reset_by creset);

   // State of the simulation -- set back acknowledge after it is set
   Reg#(Bool)   enabled <- mkReg(False, clocked_by uclock, reset_by ureset);

   rule request;
      control_in.request();
   endrule: request

   rule setEnabled_0 (!enabled);
      let datain <- toGet(control_in).get;
      enabled <= datain;
      status_out.send (datain);
      forceChange <= datain;
   endrule

   rule setEnabled_1  (enabled);
      let datain <- toGet(control_in).get;
      enabled <= datain;
      status_out.send (datain);
   endrule

   (*fire_when_enabled, no_implicit_conditions*)
   rule count_cclock;
      cycleStamp <= cycleStamp + 1;
   endrule

   Bool sendData =  forceChange || (probeFifo.first.probe != lastProbe);

   rule driveprobe (enabled);
      if (sendData) begin
         toPut(probe_out).put(probeFifo.first);
         lastProbe <= probeFifo.first.probe;
         forceChange <= False;
      end
      probeFifo.deq;
   endrule

   // When the probe is turned off avoid overflow
   rule drop_probes (!enabled);
      probeFifo.deq();
   endrule

   method Action in_probes(probes p);
      probeFifo.enq(Probes {probe:p,
                            cycles:cycleStamp }
                     );
   endmethod

endmodule: mkSceMiProbeXactor

// Allow SceMiProbeXactor to be treated as Put interfaces
instance ToPut#(SceMiProbeXactorIfc#(a),a);
   function toPut(p) = (interface Put;
			   method put = p.in_probes;
		        endinterface);
endinstance


instance Connectable #(SceMiProbeXactorIfc#(p), ReadOnly#(p)) ;
   module mkConnection (SceMiProbeXactorIfc#(p) x, ReadOnly#(p) v, Empty ifc);
      rule probeConnection (True);
         x.in_probes(v._read);
      endrule
   endmodule
endinstance

instance Connectable #( ReadOnly#(p), SceMiProbeXactorIfc#(p));
   module mkConnection ( ReadOnly#(p) v, SceMiProbeXactorIfc#(p) x, Empty ifc);
      (*hide*)
      Empty _i <- mkConnection(x,asIfc(v));
   endmodule
endinstance

instance Connectable #(SceMiProbeXactorIfc#(p), p);
   module mkConnection (SceMiProbeXactorIfc#(p) x, p v, Empty ifc);
      rule probeConnection (True);
         x.in_probes(v);
      endrule
   endmodule
endinstance

instance Connectable #(p, SceMiProbeXactorIfc#(p)) ;
   module mkConnection (p v, SceMiProbeXactorIfc#(p) x, Empty ifc) ;
      (*hide*)
      let _c <- mkConnection(x,v);
   endmodule
endinstance

instance Connectable #( SceMiProbeXactorIfc#(p), Get#(p) );
   module mkConnection (SceMiProbeXactorIfc#(p) pr, Get#(p) g, Empty ifc);
      rule probeConnectGet (True);
         p v <- g.get;
         pr.in_probes(v);
      endrule
   endmodule
endinstance
instance Connectable #(Get#(p), SceMiProbeXactorIfc#(p)) ;
   module mkConnection (Get#(p) g, SceMiProbeXactorIfc#(p) pr, Empty ifc) ;
      (*hide*)
      let _c <- mkConnection(pr,g);
   endmodule
endinstance

endpackage: SceMiProbe
