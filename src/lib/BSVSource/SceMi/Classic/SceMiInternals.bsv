// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiInternals;

import List::*;
import Vector::*;
import Clocks::*;
import ModuleContext::*;
import FIFOF::*;
import SpecialFIFOs::*;
import FIFOLevel::*;

import SceMiDefines::*;

// This is the interface used for back-channel communication
// with the SceMiClockControl modules
interface ClkEnableIfc;
   (* always_ready *)
   method Bool allow_pos();
   (* always_ready *)
   method Bool allow_neg();
   (* always_ready *)
   method Action tick(Bool value);
endinterface: ClkEnableIfc

// This is the full raw SceMiClockControl interface used with
// imported macros for the SCE-MI standard.  It includes the
// allow_posedge() and allow_negedge() as methods and they are
// converted from interface arguments in the BSV wrapper.
interface RawSceMiClockControlIfc;
   interface Clock uclock;
   interface Reset ureset;
   (* always_ready *)
   method Action allow_posedge();
   (* always_ready *)
   method Bool pre_posedge();
   (* always_ready *)
   method Action allow_negedge();
   (* always_ready *)
   method Bool pre_negedge();
endinterface: RawSceMiClockControlIfc


Integer noClockGroup = -1;

// Complete clock port info, used by TCP link type
typedef struct {
   Integer clockNum;
   Integer clockGroup;
   Integer ratioNumerator;
   Integer ratioDenominator;
   Integer dutyHi;
   Integer dutyLo;
   Integer phase;
   Integer resetCycles;
   Bool    inverted;
} ClockPortInfo;

function Integer getClockNum(ClockPortInfo port);
   return port.clockNum;
endfunction: getClockNum

function Integer getResetCycles(ClockPortInfo port);
   return port.resetCycles;
endfunction: getResetCycles

function Bool isForClockPort(Integer n, ClockPortInfo port);
   return (port.clockNum == n);
endfunction: isForClockPort

function Ordering cmpClockNum(ClockPortInfo cpi1, ClockPortInfo cpi2);
   return compare(cpi1.clockNum,cpi2.clockNum);
endfunction

function Ordering cmpClockGroup(ClockPortInfo cpi1, ClockPortInfo cpi2);
   return compare(cpi1.clockGroup,cpi2.clockGroup);
endfunction

function Bool sameClockGroup(ClockPortInfo cpi1, ClockPortInfo cpi2);
   return (cpi1.clockGroup == cpi2.clockGroup);
endfunction

// Complete clock controller info, used by TCP link type
typedef struct {
   Integer clockNum;
   ClkEnableIfc ens;
} ClockControlInfo;

function Bool isForClock(Integer n, ClockControlInfo ctrl);
   return (ctrl.clockNum == n);
endfunction: isForClock

// Clock domain and reset info
typedef struct {
   Clock clk;
   Reset rst;
   Bool  external;
} DomainInfo;

// Word-level input port interface
(* always_ready *)
interface WordLevelInPortIfc;
   method Bool hasRequest();
   method Action putWord(Bit#(32) x);
endinterface: WordLevelInPortIfc

// Word-level output port interface
(* always_ready *)
interface WordLevelOutPortIfc;
   method ActionValue#(Bit#(32)) takeWord();
endinterface: WordLevelOutPortIfc

// Input Port Info
typedef struct {
   Integer            portNum;
   Integer            bitWidth;
   WordLevelInPortIfc wordIfc;
} InPortInfo;

function Ordering cmp_inport_num(InPortInfo a, InPortInfo b);
   return compare(a.portNum,b.portNum);
endfunction

// Output Port Info
typedef struct {
   Integer             portNum;
   Integer             bitWidth;
   WordLevelOutPortIfc wordIfc;
} OutPortInfo;

function Ordering cmp_outport_num(OutPortInfo a, OutPortInfo b);
   return compare(a.portNum,b.portNum);
endfunction

// This is the context state used for building SceMi infrastructure
typedef struct {
   SceMiLinkType                                           link_type;
   List#(ClockPortInfo)                                    clkports;
   List#(ClockControlInfo)                                 clkcntrls;
   Integer                                                 inPortCount;
   List#(InPortInfo)                                       inports;
   Integer                                                 outPortCount;
   List#(OutPortInfo)                                      outports;
   Maybe#(DomainInfo)                                      udomain;
   List#(Tuple2#(Integer,DomainInfo))                      cdomains;
   Clock                                                   scemi_clock;
   Reset                                                   scemi_reset;
   ReadOnly#(Bool)                                         rising_uclock;
   Reg#(SceMiCycleStamp)                                   proto_stamp;
   ReadOnly#(SceMiCycleStamp)                              stamp;
   Wire#(Bool)                                             is_in_reset;
   ReadOnly#(Bool)                                         is_in_reset_u;
   List#(Reset)                                            rawResets;
   List#(Tuple2#(Integer,MakeClockIfc#(Bit#(1))))          clkgens;
   List#(MakeResetIfc)                                     rstgens;
   List#(Tuple2#(Integer,RawSceMiClockControlIfc))         ccifcs;
   SyncFIFOCountIfc#(Tuple2#(SceMiCycleStamp,UInt#(10)),4) outMsgInfo;
   Reg#(UInt#(10))                                         currentOutPort;
} SceMiModuleState;

// These modules/functions are used to define the initial state
// for the SceMiModule monad

module get_initial_state#(Clock uClock, Reset uReset,
			  Clock scemiClock, Reset scemiReset,
			  ReadOnly#(Bool) rising_uclock,
                          SceMiLinkType linktype)
                         (SceMiModuleState);

   // Create a cycle stamp register which will be updated in the
   // scemiClock domain and read in the uClock domain
   CrossingReg#(SceMiCycleStamp) cycle_stamp <- mkNullCrossingReg( uClock
                                                                 , 0
                                                                 , clocked_by scemiClock
                                                                 , reset_by scemiReset
                                                                 );

   // Track reset in both scemiClock and uClock
   Wire#(Bool) any_in_reset <- mkDWire(False, clocked_by scemiClock, reset_by scemiReset);
   CrossingReg#(Bool) any_in_reset_uclk <- mkNullCrossingReg( uClock
                                                            , False
                                                            , clocked_by scemiClock
                                                            , reset_by scemiReset
                                                            );

   rule track_reset;
      any_in_reset_uclk <= any_in_reset;
   endrule

   // FIFO for output channel ordering.  This has na unguarded deq
   // method so that is does not interfere with the operation of the
   // PCIE read pipeline.  The FIFO is explicitly cleared during
   // SCE-MI reset.
   SyncFIFOCountIfc#(Tuple2#(SceMiCycleStamp,UInt#(10)),4) msgFIFO <-
               mkGSyncFIFOCount(False, True, uClock, uReset, scemiClock);

   // Register to scan over output ports
   Reg#(UInt#(10)) out_port <- mkReg(0, clocked_by uClock, reset_by uReset);

   SceMiModuleState init_state;
   init_state.link_type      = linktype;
   init_state.clkports       = List::nil;
   init_state.clkcntrls      = List::nil;
   init_state.inPortCount    = 0;
   init_state.inports        = List::nil;
   init_state.outPortCount   = 0;
   init_state.outports       = List::nil;
   DomainInfo dinfo;
   dinfo.clk      = uClock;
   dinfo.rst      = uReset;
   dinfo.external = True;
   init_state.udomain        = tagged Valid dinfo;
   init_state.cdomains       = List::nil;
   init_state.scemi_clock    = scemiClock;
   init_state.scemi_reset    = scemiReset;
   init_state.rising_uclock  = rising_uclock;
   init_state.proto_stamp    = crossingRegToReg(cycle_stamp);
   init_state.stamp          = crossingRegDstToReadOnly(cycle_stamp);
   init_state.is_in_reset    = any_in_reset;
   init_state.is_in_reset_u  = crossingRegDstToReadOnly(any_in_reset_uclk);
   init_state.rawResets      = List::nil;
   init_state.clkgens        = List::nil;
   init_state.rstgens        = List::nil;
   init_state.ccifcs         = List::nil;
   init_state.outMsgInfo     = msgFIFO;
   init_state.currentOutPort = out_port;
   return init_state;

endmodule

function SceMiModuleState get_initial_state_unclocked(SceMiLinkType linktype);

   SceMiModuleState init_state;
   init_state.link_type      = linktype;
   init_state.clkports       = List::nil;
   init_state.clkcntrls      = List::nil;
   init_state.inPortCount    = 0;
   init_state.inports        = List::nil;
   init_state.outPortCount   = 0;
   init_state.outports       = List::nil;
   init_state.udomain        = tagged Invalid;
   init_state.cdomains       = List::nil;
   init_state.scemi_clock    = noClock;
   init_state.scemi_reset    = noReset;
   init_state.rising_uclock  = ?;
   init_state.proto_stamp    = ?;
   init_state.stamp          = ?;
   init_state.is_in_reset    = ?;
   init_state.is_in_reset_u  = ?;
   init_state.rawResets      = List::nil;
   init_state.clkgens        = List::nil;
   init_state.rstgens        = List::nil;
   init_state.ccifcs         = List::nil;
   init_state.outMsgInfo     = ?;
   init_state.currentOutPort = ?;

   return init_state;

endfunction

// This function is used to add external clocks and resets
// to the SceMiModuleState
function SceMiModuleState add_external_clocks_and_resets( SceMiModuleState init_state
                                                        , Vector#(n,Clock) cv
                                                        , Vector#(n,Reset) rv
							);
   SceMiModuleState augmented_state = init_state;
   if (valueOf(n) > 0)
   begin
      for (Integer clockNum = 0; clockNum < valueOf(n); clockNum = clockNum + 1)
      begin
	 DomainInfo dom;
	 dom.clk      = cv[clockNum];
	 dom.rst      = rv[clockNum];
	 dom.external = True;
	 augmented_state.cdomains = List::cons( tuple2(clockNum,dom)
					      , augmented_state.cdomains
					      );
      end
   end

   return augmented_state;

endfunction

function Clock getUClock(SceMiModuleState state);
   if (state.udomain matches tagged Valid .dinfo)
      return dinfo.clk;
   else
      return noClock;
endfunction

function Reset getUReset(SceMiModuleState state);
   if (state.udomain matches tagged Valid .dinfo)
      return dinfo.rst;
   else
      return noReset;
endfunction

function Clock getSceMiClock(SceMiModuleState state);
   return state.scemi_clock;
endfunction

function Reset getSceMiReset(SceMiModuleState state);
   return state.scemi_reset;
endfunction

function Bool isInReset(SceMiModuleState state);
   return state.is_in_reset_u;
endfunction

// Functions for working with the outMsgInfo FIFO

function Bool hasOutputMsg(SceMiModuleState state);
   return state.outMsgInfo.dNotEmpty();
endfunction

function SceMiCycleStamp nextMsgCycle(SceMiModuleState state);
   return tpl_1(state.outMsgInfo.first());
endfunction

function UInt#(10) nextMsgChannel(SceMiModuleState state);
   return tpl_2(state.outMsgInfo.first());
endfunction

function Action advanceOutputMsg(SceMiModuleState state);
   action
      state.outMsgInfo.deq();
   endaction
endfunction

function Action clearOutputMsgs(SceMiModuleState state);
   action
      state.outMsgInfo.dClear();
   endaction
endfunction

function Action addOutputMsgChannel(SceMiModuleState state, UInt#(10) portNum);
   action
      state.outMsgInfo.enq(tuple2(state.stamp,portNum));
   endaction
endfunction

// Convenient alias for SceMi module type
typedef ModuleContext#(SceMiModuleState,i) SceMiModule#(type i);

// Placeholder for a constant supplied during infrastructure linkage
module mkSceMiConstant(ReadOnly#(a)) provisos(Bits#(a,_));
   (*hide*)
   Reg#(a) _x <- mkRegU();
   return regToReadOnly(_x);
endmodule: mkSceMiConstant

// Record parameters for use during infrastructure linkage

// The Ignored interface is used to work around XST synthesis issues
interface Ignored;
   (* always_ready *)
   method Bit#(1) not_used();
endinterface

(* synthesize *)
(* no_default_clock, no_default_reset *)
module mkSceMiUInt32Parameter#(parameter UInt#(32) n)(Ignored);
   // will be pruned during synthesis
   method Bit#(1) not_used();
      return 0;
   endmethod
endmodule: mkSceMiUInt32Parameter

(* synthesize *)
(* no_default_clock, no_default_reset *)
module mkSceMiUInt64Parameter#(parameter UInt#(64) n)(Ignored);
   // will be pruned during synthesis
   method Bit#(1) not_used();
      return 0;
   endmethod
endmodule: mkSceMiUInt64Parameter

(* synthesize *)
(* no_default_clock, no_default_reset *)
module mkSceMiLinkTypeParameter#(parameter SceMiLinkType link_type)(Ignored);
   // will be pruned during synthesis
   method Bit#(1) not_used();
      return 0;
   endmethod
endmodule: mkSceMiLinkTypeParameter

(* synthesize *)
(* no_default_clock, no_default_reset *)
module mkSceMiStringParameter#(parameter String n)(Ignored);
   // will be pruned during synthesis
   method Bit#(1) not_used();
      return 0;
   endmethod
endmodule: mkSceMiStringParameter

// BSV adaptation of the SceMiMessageInPort macro
// interface described in section 5.2.2 of the SCE-MI 2.0
// specification.
// This is a try/accept version of the input port interface.
// It has strict compliance semantic to SceMi specification.
interface SceMiMessageInPortRawIfc#(type msg_type);
   (* always_ready *)
   method Action request();
   (* always_ready *)
   method Bool has_data();
   method msg_type read();
endinterface: SceMiMessageInPortRawIfc

module [m] rawInPortToBSV( m#(SceMiMessageInPortRawIfc#(a)) raw
                         , SceMiMessageInPortIfc#(a) ifc
                         )
   provisos(Bits#(a,sz), IsModule#(m,c));

   (*hide*)
   let          _corePort <- raw();
   FIFOF#(void) requestF  <- mkPipelineFIFOF();
   FIFOF#(a)    dataF     <- mkBypassFIFOF();

   // assert request on the port
   // Don't assert request if the data FIFO has no room, because once the
   // request is asserted it is held through the cycle in which data arrives.
   (* fire_when_enabled *)
   rule receive_ready if ( requestF.notEmpty() &&
                           dataF.notFull() );
      _corePort.request();
   endrule

   // when data is available, take it and remove the request
   (* fire_when_enabled *)
   rule receive if (_corePort.has_data());
      dataF.enq(_corePort.read());
      requestF.deq();
   endrule

   method Action request();
      requestF.enq(?);
   endmethod

   method Bool request_pending();
      return requestF.notEmpty();
   endmethod

   method read = dataF.first;

   method Action ack();
      dataF.deq();
   endmethod

endmodule: rawInPortToBSV

// This is a try/accept version of the output port interface.
// It is used internally with the SceMi and EVE macros.

interface SceMiMessageOutPortRawIfc#(type msg_type);
   (* always_ready *)
   method ActionValue#(Bool) try_to_send(msg_type m);
endinterface: SceMiMessageOutPortRawIfc

module [m] rawOutPortToBSV( m#(SceMiMessageOutPortRawIfc#(a)) raw
		          , SceMiMessageOutPortIfc#(a) ifc
		          )
   provisos(Bits#(a,sz), IsModule#(m,c));

   (*hide*)
   let       _corePort <- raw();
   FIFOF#(a) dataF    <- mkBypassFIFOF();

   rule xfer;
      let ok <- _corePort.try_to_send(dataF.first());
      if (ok)
	 dataF.deq();
   endrule

   method accepting_data = dataF.notFull;
   method send           = dataF.enq;

endmodule: rawOutPortToBSV

// This is a reset generation module that will assert reset early, but
// not too early, to satisfy the requirements of the SCE-MI spec.

module genSceMiReset#(Clock dstClk)(MakeResetIfc);

   Clock clk <- exposeCurrentClock();
   Reset rstn <- exposeCurrentReset();

   Clock inv_clk <- invertCurrentClock();
   Reset inv_rstn <- mkAsyncResetFromCR(0,inv_clk);

   // Create a reset pulse on the inverted clock, so that
   // reset gets asserted on the first negedge of the clock
   MakeResetIfc inv_rstgen <- mkReset(1, False, dstClk, clocked_by inv_clk, reset_by inv_rstn);
   Reset reset0 = inv_rstgen.new_rst;

   Reg#(Bool) init <- mkReg(False, clocked_by inv_clk, reset_by inv_rstn);

   rule trigger if (!init);
      init <= True;
      inv_rstgen.assertReset();
   endrule

   // This is the main reset, based on the non-inverted clock
   MakeResetIfc rstgen <- mkReset(0, False, dstClk);
   Reset reset1 = rstgen.new_rst;

   // Combine both resets to get the desired reset waveform
   Reset final_reset <- mkResetEither(reset0, reset1, clocked_by dstClk);

   method Action assertReset = rstgen.assertReset;
   method Bool isAsserted    = rstgen.isAsserted;
   interface Reset new_rst   = final_reset;
endmodule


endpackage: SceMiInternals
