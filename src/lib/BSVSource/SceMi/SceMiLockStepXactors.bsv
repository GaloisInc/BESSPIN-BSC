// Copyright (c) 2008-2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiLockStepXactors;

import Clocks::*;
import GetPut::*;
import SceMiCore::*;
import SceMiDefines::*;
import Vector::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
module [SceMiModule] mkLockStepPipeXactorSR#(Integer clockNum,
					     Integer depth, 
					     SceMiClockPortIfc clk_port, Clock iclock, Reset ireset) (SceMiPipe#(a, b))
   provisos (Bits#(a, sa), Bits#(b, sb)
	     , Add#(a__, sa, TMul#(TDiv#(sa, 8), 8))
	     , Add#(b__, sb, TMul#(TDiv#(sb, 8), 8))
	     );
   
   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();
      
   SceMiPipe#(a, b) lock <- mkLockStepPipeXactorDirectSR(clockNum, depth, clk_port);
   
   CrossingReg#(a) in_signals_u_i  <- mkNullCrossingRegA(iclock, unpack(0), clocked_by uclock, reset_by ureset);
   Wire#(b)        out_signals_i_i <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(b)    out_signals_i_u <- mkNullCrossing(uclock, out_signals_i_i, clocked_by iclock, reset_by ireset);
   
   // uclock domain
   rule get_inputs;
      let value <- lock.inputs.get;
      in_signals_u_i <= value;
   endrule
   
   // uclock domain
   rule put_outputs;
      lock.outputs.put(out_signals_i_u);
   endrule
   
   Wire#(b)     outputs_c_c <- mkBypassWire(clocked_by cclock, reset_by creset);
   ReadOnly#(b) outputs_c_i <- mkNullCrossing(iclock, outputs_c_c, clocked_by cclock, reset_by creset);
   Wire#(a)     inputs_i_i  <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(a) inputs_i_c  <- mkNullCrossing(cclock, inputs_i_i, clocked_by iclock, reset_by ireset);
   
   // iclock domain
   rule every0;
      inputs_i_i <= in_signals_u_i.crossed;
   endrule
   
   // iclock domain
   rule every1;
      out_signals_i_i <= outputs_c_i;
   endrule
   
   interface Put outputs;
      method Action put(b value);
	 outputs_c_c <= value;
      endmethod
   endinterface

   interface Get inputs;
      method ActionValue#(a) get;
	 return inputs_i_c ;
      endmethod
   endinterface

endmodule

(* always_ready, always_enabled *)
module [SceMiModule] mkLockStepPipeXactorRS#(Integer clockNum,
					     Integer depth, 
					     SceMiClockPortIfc clk_port, Clock iclock, Reset ireset) (SceMiPipe#(a, b))
   provisos (Bits#(a, sa), Bits#(b, sb)
	     , Add#(a__, sa, TMul#(TDiv#(sa, 8), 8))
	     , Add#(b__, sb, TMul#(TDiv#(sb, 8), 8))
	     );
   
   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();
      
   SceMiPipe#(a, b) lock <- mkLockStepPipeXactorDirectRS(clockNum, depth, clk_port);
   
   CrossingReg#(a) in_signals_u_i  <- mkNullCrossingRegA(iclock, unpack(0), clocked_by uclock, reset_by ureset);
   Wire#(b)        out_signals_i_i <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(b)    out_signals_i_u <- mkNullCrossing(uclock, out_signals_i_i, clocked_by iclock, reset_by ireset);
   
   // uclock domain
   rule get_inputs;
      let value <- lock.inputs.get;
      in_signals_u_i <= value;
   endrule
   
   // uclock domain
   rule put_outputs;
      lock.outputs.put(out_signals_i_u);
   endrule
   
   Wire#(b)     outputs_c_c <- mkBypassWire(clocked_by cclock, reset_by creset);
   ReadOnly#(b) outputs_c_i <- mkNullCrossing(iclock, outputs_c_c, clocked_by cclock, reset_by creset);
   Wire#(a)     inputs_i_i  <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(a) inputs_i_c  <- mkNullCrossing(cclock, inputs_i_i, clocked_by iclock, reset_by ireset);
   
   // iclock domain
   rule every0;
      inputs_i_i <= in_signals_u_i.crossed;
   endrule
   
   // iclock domain
   rule every1;
      out_signals_i_i <= outputs_c_i;
   endrule
   
   interface Put outputs;
      method Action put(b value);
	 outputs_c_c <= value;
      endmethod
   endinterface

   interface Get inputs;
      method ActionValue#(a) get;
	 return inputs_i_c ;
      endmethod
   endinterface

endmodule


(* always_ready, always_enabled *)
module [SceMiModule] mkLockStepPipeXactor#(Integer clockNum,
					   Integer depth, 
					   SceMiClockPortIfc clk_port, Clock iclock, Reset ireset) (SceMiPipe#(a, b))
   provisos (Bits#(a, sa), Bits#(b, sb)
	     , Add#(a__, sa, TMul#(TDiv#(sa, 8), 8))
	     , Add#(b__, sb, TMul#(TDiv#(sb, 8), 8))
	     );
   
   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;

   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();
      
   SceMiPipe#(a, b) lock <- mkLockStepPipeXactorDirect(clockNum, depth, clk_port);
   
   CrossingReg#(a) in_signals_u_i  <- mkNullCrossingRegA(iclock, unpack(0), clocked_by uclock, reset_by ureset);
   Wire#(b)        out_signals_i_i <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(b)    out_signals_i_u <- mkNullCrossing(uclock, out_signals_i_i, clocked_by iclock, reset_by ireset);
   
   // uclock domain
   rule get_inputs;
      let value <- lock.inputs.get;
      in_signals_u_i <= value;
   endrule
   
   // uclock domain
   rule put_outputs;
      lock.outputs.put(out_signals_i_u);
   endrule
   
   Wire#(b)     outputs_c_c <- mkBypassWire(clocked_by cclock, reset_by creset);
   ReadOnly#(b) outputs_c_i <- mkNullCrossing(iclock, outputs_c_c, clocked_by cclock, reset_by creset);
   Wire#(a)     inputs_i_i  <- mkBypassWire(clocked_by iclock, reset_by ireset);
   ReadOnly#(a) inputs_i_c  <- mkNullCrossing(cclock, inputs_i_i, clocked_by iclock, reset_by ireset);
   
   // iclock domain
   rule every0;
      inputs_i_i <= in_signals_u_i.crossed;
   endrule
   
   // iclock domain
   rule every1;
      out_signals_i_i <= outputs_c_i;
   endrule
   
   interface Put outputs;
      method Action put(b value);
	 outputs_c_c <= value;
      endmethod
   endinterface

   interface Get inputs;
      method ActionValue#(a) get;
	 return inputs_i_c ;
      endmethod
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkLockStepPipeXactorDirectSR#(Integer clockNum,
						   Integer depth, 
						   SceMiClockPortIfc clk_port) (SceMiPipe#(a, b))
   provisos (Bits#(a, as), Bits#(b, bs)
	     , Add#(a__, as, TMul#(TDiv#(as, 8), 8))
      	     , Add#(b__, bs, TMul#(TDiv#(bs, 8), 8))
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;
   
   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, a)   inpipe  <- mkSceMiInputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);
   SceMiOutputPipeIfc#(1, b)  outpipe <- mkSceMiOutputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);

   CrossingReg#(Bool) flip        <- mkNullCrossingRegA(uclock, False, clocked_by cclock, reset_by creset);   
   Reg#(Bool)         uflip       <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         uflip_prev  <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         allow_clock <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bit#(2))      clk_count   <- mkRegA(0, clocked_by uclock, reset_by ureset);
      
   SceMiClockControlIfc clkControl <- mkSceMiClockControl(clockNum, allow_clock, allow_clock);
   
   rule every;
      uflip_prev <= flip.crossed;
   endrule
   
   rule do_flip;
      flip <= !flip;
   endrule
   
   (* preempts="stop_allow, update" *)
   rule stop_allow (flip.crossed != uflip_prev && allow_clock);
      allow_clock <= False;
      clk_count <= 0;
   endrule
   
   rule update;
      clk_count <= (clk_count == '1) ? '1 : clk_count + 1;
   endrule
   
   Wire#(UInt#(32)) can_recv <- mkBypassWire;
   Wire#(UInt#(32)) can_send <- mkBypassWire;
   
   rule can_0;
      can_recv <= inpipe.can_receive();
   endrule
   
   rule can_1;
      can_send <= outpipe.can_send();
   endrule
   
   Reg#(Bool) polarity <- mkRegA(False, clocked_by uclock, reset_by ureset);
   
   interface Put outputs;
      method Action put(b value) if (outpipe.can_send() != 0 && !allow_clock && clk_count == '1 && !polarity);
         let n <- outpipe.try_send(1, replicate(value), True);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe send: attempted %0d, got %0d", $time, 1, n);
//	 $display("[%0d] HW SEND: %h", $time, value);
	 polarity <= !polarity;
      endmethod
   endinterface

   interface Get inputs;
      method ActionValue#(a) get if (inpipe.can_receive() != 0 && !allow_clock && clk_count == '1 && polarity);
         match { .n, .v, .eom } <- inpipe.try_receive(1);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe receive: expected %0d, got %0d", $time, 1, n);
//   	 $display("[%0d] HW RECV: %h", $time, v[0]);
	 polarity <= !polarity;
	 allow_clock <= True;
         return v[0];
      endmethod
   endinterface

endmodule


module [SceMiModule] mkLockStepPipeXactorDirectRS#(Integer clockNum,
						   Integer depth, 
						   SceMiClockPortIfc clk_port) (SceMiPipe#(a, b))
   provisos (Bits#(a, as), Bits#(b, bs)
	     , Add#(a__, as, TMul#(TDiv#(as, 8), 8))
      	     , Add#(b__, bs, TMul#(TDiv#(bs, 8), 8))
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;
   
   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, a)   inpipe  <- mkSceMiInputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);
   SceMiOutputPipeIfc#(1, b)  outpipe <- mkSceMiOutputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);

   CrossingReg#(Bool) flip        <- mkNullCrossingRegA(uclock, False, clocked_by cclock, reset_by creset);   
   Reg#(Bool)         uflip       <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         uflip_prev  <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         allow_clock <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bit#(2))      clk_count   <- mkRegA(0, clocked_by uclock, reset_by ureset);
      
   SceMiClockControlIfc clkControl <- mkSceMiClockControl(clockNum, allow_clock, allow_clock);
   
   rule every;
      uflip_prev <= flip.crossed;
   endrule
   
   rule do_flip;
      flip <= !flip;
   endrule
   
   (* preempts="stop_allow, update" *)
   rule stop_allow (flip.crossed != uflip_prev && allow_clock);
      allow_clock <= False;
      clk_count <= 0;
   endrule
   
   rule update;
      clk_count <= (clk_count == '1) ? '1 : clk_count + 1;
   endrule
   
   Wire#(UInt#(32)) can_recv <- mkBypassWire;
   Wire#(UInt#(32)) can_send <- mkBypassWire;
   
   rule can_0;
      can_recv <= inpipe.can_receive();
   endrule
   
   rule can_1;
      can_send <= outpipe.can_send();
   endrule
   
   Reg#(Bool) polarity <- mkRegA(False, clocked_by uclock, reset_by ureset);
   
   interface Get inputs;
      method ActionValue#(a) get if (inpipe.can_receive() != 0 && !allow_clock && clk_count == '1 && !polarity);
         match { .n, .v, .eom } <- inpipe.try_receive(1);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe receive: expected %0d, got %0d", $time, 1, n);
//   	 $display("[%0d] HW RECV: %h", $time, v[0]);
	 polarity <= !polarity;
         return v[0];
      endmethod
   endinterface

   interface Put outputs;
      method Action put(b value) if (outpipe.can_send() != 0 && !allow_clock && clk_count == '1 && polarity);
         let n <- outpipe.try_send(1, replicate(value), True);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe send: attempted %0d, got %0d", $time, 1, n);
//	 $display("[%0d] HW SEND: %h", $time, value);
	 polarity <= !polarity;
   	 allow_clock <= True;
      endmethod
   endinterface



endmodule

module [SceMiModule] mkLockStepPipeXactorDirect#(Integer clockNum,
						 Integer depth, 
						 SceMiClockPortIfc clk_port) (SceMiPipe#(a, b))
   provisos (Bits#(a, as), Bits#(b, bs)
	     , Add#(a__, as, TMul#(TDiv#(as, 8), 8))
      	     , Add#(b__, bs, TMul#(TDiv#(bs, 8), 8))
	     );

   // Access the controlled clock and reset
   Clock cclock = clk_port.cclock;
   Reset creset = clk_port.creset;
   
   // Access the uncontrolled clock and reset
   Clock uclock <- sceMiGetUClock();
   Reset ureset <- sceMiGetUReset();

   // The output pipe
   // (Since this is not hidden, via naming or attribute, the C++ side
   // has to instantiate an outport proxy with this in its name, or use
   // a software side C++ class for this transactor which hides the name.)
   SceMiInputPipeIfc#(1, a)   inpipe  <- mkSceMiInputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);
   SceMiOutputPipeIfc#(1, b)  outpipe <- mkSceMiOutputPipe(depth, Fifo, clocked_by uclock, reset_by ureset);

   CrossingReg#(Bool) flip        <- mkNullCrossingRegA(uclock, False, clocked_by cclock, reset_by creset);   
   Reg#(Bool)         uflip       <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         uflip_prev  <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         allow_clock <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bit#(2))      clk_count   <- mkRegA(0, clocked_by uclock, reset_by ureset);
   
   Reg#(Bool)         have_sent     <- mkRegA(False, clocked_by uclock, reset_by ureset);
   Reg#(Bool)         have_received <- mkRegA(False, clocked_by uclock, reset_by ureset);
      
   SceMiClockControlIfc clkControl <- mkSceMiClockControl(clockNum, allow_clock, allow_clock);
   
   rule every;
      uflip_prev <= flip.crossed;
   endrule
   
   rule do_flip;
      flip <= !flip;
   endrule
   
   (* preempts="stop_allow, update" *)
   rule stop_allow (flip.crossed != uflip_prev && allow_clock);
      allow_clock <= False;
      clk_count <= 0;
   endrule
   
   rule update;
      clk_count <= (clk_count == '1) ? '1 : clk_count + 1;
   endrule
   
   Wire#(UInt#(32)) can_recv <- mkBypassWire;
   Wire#(UInt#(32)) can_send <- mkBypassWire;
   
   rule can_0;
      can_recv <= inpipe.can_receive();
   endrule
   
   rule can_1;
      can_send <= outpipe.can_send();
   endrule
   
   Reg#(Bool) polarity <- mkRegA(False, clocked_by uclock, reset_by ureset);
   
   rule start_allow (have_received && have_sent);
      have_received <= False;
      have_sent     <= False;
      allow_clock   <= True;
   endrule
   
   interface Get inputs;
      method ActionValue#(a) get if (inpipe.can_receive() != 0 && !allow_clock && clk_count == '1 && !have_received);
         match { .n, .v, .eom } <- inpipe.try_receive(1);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe receive: expected %0d, got %0d", $time, 1, n);
//   	 $display("[%0d] HW RECV: %h", $time, v[0]);
	 have_received <= True;
         return v[0];
      endmethod
   endinterface

   interface Put outputs;
      method Action put(b value) if (outpipe.can_send() != 0 && !allow_clock && clk_count == '1 && !have_sent);
         let n <- outpipe.try_send(1, replicate(value), True);
	 if (n != 1)
	    $display("Time %0d: ERROR: incomplete pipe send: attempted %0d, got %0d", $time, 1, n);
//	 $display("[%0d] HW SEND: %h", $time, value);
   	 have_sent <= True;
      endmethod
   endinterface



endmodule

endpackage