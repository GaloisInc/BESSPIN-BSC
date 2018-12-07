// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiProxies;

// This is the SceMiProxies package.

import SceMiDefines::*;
import FIFOF::*;
import GetPut::*;

export SceMiMessageInPortProxyIfc(..), mkSceMiMessageInPortProxy;
export SceMiMessageOutPortProxyIfc(..), mkSceMiMessageOutPortProxy;
export SceMiInPipeProxyIfc(..), SceMiOutPipeProxyIfc(..);
export SceMiPipeProxy(..);
export mkSceMiInPipeProxy,  mkSceMiInPipeProxyF,  sceMiInPipeProxy;
export mkSceMiOutPipeProxy, mkSceMiOutPipeProxyF, sceMiOutPipeProxy;
export mkSceMiOutPipeProxyGet, mkSceMiInPipeProxyPut, sceMiOutPipeProxyGet, sceMiInPipeProxyPut;
export sceMiPipeProxySR, sceMiPipeProxyRS;
export bsvscemi_bind_inpipe;
export bsvscemi_bind_outpipe;
export bsvscemi_inpipe_proxy_send_immediate;
export bsvscemi_outpipe_proxy_data_get_immediate;
export bsvscemi_pipe_proxy_send_get_immediate;

// Instantiate the message input port proxy
module mkSceMiMessageInPortProxy#(String paramFile, String transactorName, String portName)
   (SceMiMessageInPortProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)      initialized <- mkReg(False);
   Reg#(UInt#(32)) proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj

   PulseWire   space_available <- mkPulseWire();

   rule start (!initialized);
      let mindex <- bsvscemi_bind_message_inport(paramFile, transactorName, portName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
            proxy_index <= index;
            initialized <= True;
	  end
      endcase
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule check_buffer (initialized);
      let ok <- bsvscemi_message_inport_proxy_ready(proxy_index);
      if (ok) space_available.send();
   endrule: check_buffer

   method Bool accepting_data();
      return space_available;
   endmethod: accepting_data

   method Action send(a x) if (space_available);
      bsvscemi_message_inport_proxy_send(pack(x), fromInteger(valueOf(sz)), proxy_index);
   endmethod: send

endmodule: mkSceMiMessageInPortProxy

// Instantiate the message output port proxy
module mkSceMiMessageOutPortProxy#(String paramFile, String transactorName, String portName)
   (SceMiMessageOutPortProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)           initialized <- mkReg(False);
   Reg#(UInt#(32))      proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj

   Reg#(Bool)        data_available <- mkReg(False);
   Reg#(a)                 the_data <- mkReg(?);
   Reg#(SceMiCycleStamp)  the_cycle  <- mkReg(0);

   rule start (!initialized);
      let mindex <- bsvscemi_bind_message_outport(paramFile, transactorName, portName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
            proxy_index <= index;
            initialized <= True;
          end
      endcase
   endrule

   rule poll (initialized && data_available == False);
      let ok <- bsvscemi_message_outport_proxy_ready(proxy_index);
      if (ok)
	begin
	 let val <- bsvscemi_message_outport_proxy_data_get(fromInteger(valueOf(sz)), proxy_index);
	 match {.a,.b} = val;
	 the_data <= a;
	 the_cycle <= b;
	 data_available <= True;
	end
   endrule: poll

   method Bool has_data();
      return data_available;
   endmethod: has_data

   method Action ack();
      data_available <= False;
   endmethod: ack

   method a read() if (data_available);
      return the_data;
   endmethod: read

   method SceMiCycleStamp cycle_stamp() if (data_available);
      return the_cycle;
   endmethod: cycle_stamp

   method Action shutdown = bsvscemi_shutdown();

endmodule: mkSceMiMessageOutPortProxy


////////////////////////////////////////////////////////////////////////////////
/// SceMi Pipe Proxies
////////////////////////////////////////////////////////////////////////////////

module mkSceMiInPipeProxy#(String paramFile, String transactorName, String portName)
   (SceMiInPipeProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)      initialized <- mkReg(False);
   Reg#(UInt#(32)) proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj

   PulseWire   space_available <- mkPulseWire();

   rule start (!initialized);
      let mindex <- bsvscemi_bind_inpipe(paramFile, transactorName, portName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
            proxy_index <= index;
            initialized <= True;
	  end
      endcase
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule check_buffer (initialized);
      let ok <- bsvscemi_inpipe_proxy_ready(proxy_index);
      if (ok) space_available.send();
   endrule: check_buffer

   method Bool accepting_data();
      return space_available;
   endmethod: accepting_data

   method Action send(a x) if (space_available);
      bsvscemi_inpipe_proxy_send(pack(x), fromInteger(valueOf(sz)), proxy_index);
   endmethod: send

endmodule

module mkSceMiOutPipeProxy#(String paramFile, String transactorName, String pipeName)
   (SceMiOutPipeProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)           initialized <- mkReg(False);
   Reg#(UInt#(32))      proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj

   Reg#(Bool)        data_available <- mkReg(False);
   Reg#(a)                 the_data <- mkReg(?);

   rule start (!initialized);
      let mindex <- bsvscemi_bind_outpipe(paramFile, transactorName, pipeName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
            proxy_index <= index;
            initialized <= True;
          end
      endcase
   endrule

   rule poll (initialized && data_available == False);
      let ok <- bsvscemi_outpipe_proxy_ready(proxy_index);
      if (ok)
	begin
	 let val <- bsvscemi_outpipe_proxy_data_get(fromInteger(valueOf(sz)), proxy_index);
	 the_data <= val;
	 data_available <= True;
	end
   endrule: poll

   method Bool has_data();
      return data_available;
   endmethod: has_data

   method Action ack();
      data_available <= False;
   endmethod: ack

   method a read() if (data_available);
      return the_data;
   endmethod: read

   method Action shutdown = bsvscemi_shutdown();

endmodule


module mkSceMiInPipeProxyF#(String paramFile, String transactorName, String portName)
   (SceMiInPipeProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)      initialized <- mkReg(False);
   Reg#(UInt#(32)) proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj

   PulseWire   space_available <- mkPulseWire();
   
   FIFOF#(a)  fifo <- mkFIFOF;

   rule start (!initialized);
      let mindex <- bsvscemi_bind_inpipe(paramFile, transactorName, portName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
	    $display("(%0d) VALID (%0d)", $time, index);
            proxy_index <= index;
            initialized <= True;
	  end
      endcase
   endrule
   
   (* no_implicit_conditions, fire_when_enabled *)
   rule check_buffer (initialized);
      let ok <- bsvscemi_inpipe_proxy_ready(proxy_index);
      if (ok) space_available.send();
   endrule: check_buffer
   
   rule send_data (space_available);
      let x = fifo.first;
      bsvscemi_inpipe_proxy_send(pack(x), fromInteger(valueOf(sz)), proxy_index);
      fifo.deq;
   endrule

   method accepting_data = fifo.notFull;
   method send           = fifo.enq;

endmodule


import "BVI" SceMiInPipeProxyF =
module sceMiInPipeProxy#(parameter String paramFileB, 
			 parameter String transactorNameB, 
			 parameter String portNameB) (SceMiInPipeProxyIfc#(a))
   provisos(Bits#(a, sa));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portName       = portNameB;
   
   method ACCEPT accepting_data;
   method send (DATA) enable (DATA_EN) ready (DATA_RDY);
   
   schedule accepting_data  CF ( accepting_data );
   schedule accepting_data  SB ( send );
   schedule send  C ( send );

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSceMiOutPipeProxyF#(String paramFile, String transactorName, String pipeName)
   (SceMiOutPipeProxyIfc#(a) ifc)
   provisos(Bits#(a,sz));

   Reg#(Bool)           initialized <- mkReg(False);
   Reg#(UInt#(32))      proxy_index <- mkReg(0); // The index of coresponding C++ proxy obj
   PulseWire            deq_pw      <- mkPulseWire;
   
   FIFOF#(a)  fifo <- mkFIFOF;

   rule start (!initialized);
      let mindex <- bsvscemi_bind_outpipe(paramFile, transactorName, pipeName);
      case (mindex) matches
         tagged Invalid : $finish(0);
         tagged Valid .index :
          begin
            proxy_index <= index;
            initialized <= True;
          end
      endcase
   endrule

   rule poll (initialized);
      let ok <- bsvscemi_outpipe_proxy_ready(proxy_index);
      if (ok)
	begin
	 let val <- bsvscemi_outpipe_proxy_data_get(fromInteger(valueOf(sz)), proxy_index);
	 fifo.enq(val);
	end
   endrule
   
   rule do_deq (deq_pw);
      fifo.deq;
   endrule
   

   method has_data = fifo.notEmpty;

   method ack = deq_pw.send;

   method a read();
      return fifo.first;
   endmethod

   method Action shutdown = bsvscemi_shutdown();

endmodule

import "BVI" SceMiOutPipeProxyF =
module sceMiOutPipeProxy#(parameter String paramFileB, 
			  parameter String transactorNameB, 
			  parameter String portNameB) (SceMiOutPipeProxyIfc#(a))
   provisos(Bits#(a, sa));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portName       = portNameB;
   
   method HAS_DATA has_data();
   method DATA read() ready (DATA_RDY);
   method shutdown () enable (SHUTDOWN) ready (SHUTDOWN_RDY);
   method ack () enable (ACK);
   
   schedule has_data  CF ( has_data, read, shutdown, ack );
   schedule read  CF ( has_data, read, shutdown, ack );
   schedule shutdown  CF ( has_data, read, ack );
   schedule shutdown  C ( shutdown );
   schedule ack  CF ( has_data, read, shutdown );
   schedule ack  C ( ack );

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSceMiInPipeProxyPut#(String paramFile, String transactorName, String pipeName) (Put#(a))
   provisos(Bits#(a,sa));
   
   SceMiInPipeProxyIfc#(a) proxy <- sceMiInPipeProxy(paramFile, transactorName, pipeName);
   
   return toPut(proxy);
endmodule


module mkSceMiOutPipeProxyGet#(String paramFile, String transactorName, String pipeName) (Get#(a))
   provisos(Bits#(a,sa));
   
   SceMiOutPipeProxyIfc#(a) proxy <- sceMiOutPipeProxy(paramFile, transactorName, pipeName);
   
   return toGet(proxy);
endmodule

import "BVI" SceMiInPipeProxyPut =
module sceMiInPipeProxyPut#(parameter String paramFileB, 
			    parameter String transactorNameB, 
			    parameter String portNameB) (Put#(a))
   provisos(Bits#(a, sa));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portName       = portNameB;
   
   method put (DATA) enable (DATA_EN) ready (DATA_RDY);
   
   schedule put C ( put );

endmodule

import "BVI" SceMiOutPipeProxyGet =
module sceMiOutPipeProxyGet#(parameter String paramFileB, 
			     parameter String transactorNameB, 
			     parameter String portNameB) (Get#(a))
   provisos(Bits#(a, sa));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portName       = portNameB;
   
   method DATA get() enable (DATA_EN) ready (DATA_RDY);
   
   schedule get C ( get );

endmodule

////////////////////////////////////////////////////////////////////////////////
/// SceMi Pipe Proxies that stall until they can return (thus no
/// ready/enables needed)
////////////////////////////////////////////////////////////////////////////////

// This code in non-functional as-is
// it is used to create the template code that is edited to make the BVI version
//
// (* always_ready, always_enabled *)
// module mkSceMiPipeProxyRS#(String paramFile, String transactorName, String portNameIn, String portNameOut) (SceMiPipeProxy#(a, b))
//    provisos(Bits#(a,sa), Bits#(b,sb));

//    Reg#(Bool)           initialized <- mkReg(False);
//    Reg#(UInt#(32))      index_in    <- mkReg(0);
//    Reg#(UInt#(32))      index_out   <- mkReg(0);
   
//    rule start (!initialized);
//       let m_out <- bsvscemi_bind_outpipe(paramFile, transactorName, portNameOut);
//       let m_in  <- bsvscemi_bind_inpipe(paramFile, transactorName, portNameIn);
//       case (m_out) matches
//          tagged Invalid : $finish(0);
//          tagged Valid .index :
//             index_out <= index;
//       endcase
//       case (m_in) matches
//          tagged Invalid : $finish(0);
//          tagged Valid .index :
//             index_in <= index;
//       endcase
//       initialized <= True;
//    endrule
   
//    interface Get outputs;
//       method ActionValue#(a) get;
// 	 a val = ?;
// 	 if (initialized) 
// 	    val <- bsvscemi_outpipe_proxy_data_get_immediate(0, index_out);
//       	 return val;
//       endmethod
//    endinterface
   
//    interface Put inputs;
//       method Action put(b value);
//    	 if (initialized) 
//    	    bsvscemi_inpipe_proxy_send_immediate(pack(value), 0, index_in);
//       endmethod
//    endinterface

// endmodule

// (* synthesize, always_ready, always_enabled *)
// module zow (SceMiPipeProxy#(Bit#(113), Bit#(107)));
//    let _ifc <- mkSceMiPipeProxyRS("aa", "bb", "cc", "dd");
//    return _ifc;
// endmodule

import "BVI" SceMiPipeProxyRS =
module sceMiPipeProxyRS#(parameter String paramFileB, 
			parameter String transactorNameB, 
			parameter String portNameInB,
			parameter String portNameOutB) (SceMiPipeProxy#(a, b))
   provisos(Bits#(a, sa), Bits#(b, sb));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH_IN = valueOf(sb);
   parameter WIDTH_OUT = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portNameIn     = portNameInB;
   parameter portNameOut    = portNameOutB;
   
   interface Get outputs;
      method OUTPUTS get () enable ((*inhigh*) ignore0);
   endinterface
   interface Put inputs;
      method put (INPUTS)   enable ((*inhigh*) ignore1);
   endinterface
   
   schedule outputs_get C  outputs_get;
   schedule inputs_put  C  inputs_put;
   schedule outputs_get CF inputs_put;

endmodule

import "BVI" SceMiPipeProxySR =
module sceMiPipeProxySR#(parameter String paramFileB, 
			parameter String transactorNameB, 
			parameter String portNameInB,
			parameter String portNameOutB) (SceMiPipeProxy#(a, b))
   provisos(Bits#(a, sa), Bits#(b, sb));
   
   default_clock clk(CLK);
   default_reset rst(RST_N);
   
   parameter WIDTH_IN = valueOf(sb);
   parameter WIDTH_OUT = valueOf(sa);
   parameter paramFile      = paramFileB;
   parameter transactorName = transactorNameB;
   parameter portNameIn     = portNameInB;
   parameter portNameOut    = portNameOutB;
   
   interface Get outputs;
      method OUTPUTS get () enable ((*inhigh*) ignore0);
   endinterface
   interface Put inputs;
      method put (INPUTS)   enable ((*inhigh*) ignore1);
   endinterface
   
   schedule outputs_get C  outputs_get;
   schedule inputs_put  C  inputs_put;
   schedule outputs_get CF inputs_put;

endmodule


////////////////////////////////////////////////////////////////////////////////
///  Access to SceMi C API from BSV SceMiMessagePort Proxies
////////////////////////////////////////////////////////////////////////////////

import "BDPI" function ActionValue#(Maybe#(UInt#(32)))
		 bsvscemi_bind_message_inport(String paramFile, String transactorName,
					    String portName);
import "BDPI" function ActionValue#(Maybe#(UInt#(32)))
		 bsvscemi_bind_message_outport(String paramFile, String transactorName,
					     String portName);
import "BDPI" function ActionValue#(Bool) bsvscemi_message_inport_proxy_ready(UInt#(32) index);
import "BDPI" function Action bsvscemi_message_inport_proxy_send(Bit#(sz) data, UInt#(32) len,
							       UInt#(32) index);
import "BDPI" function ActionValue#(Bool) bsvscemi_message_outport_proxy_ready(UInt#(32) index);
import "BDPI" function ActionValue#(Tuple2#(a,SceMiCycleStamp))
		 bsvscemi_message_outport_proxy_data_get(UInt#(32) len, UInt#(32) index)
		 provisos(Bits#(a,sz), Bits#(Tuple2#(a,SceMiCycleStamp),rsz));
import "BDPI" function Action bsvscemi_shutdown();

////////////////////////////////////////////////////////////////////////////////
/// Access to SceMi C API from BSV SceMiPipe Proxies
////////////////////////////////////////////////////////////////////////////////

import "BDPI" function ActionValue#(Maybe#(UInt#(32)))
		 bsvscemi_bind_outpipe(String paramFile, String transactorName,
				       String pipeName);
import "BDPI" function ActionValue#(Maybe#(UInt#(32)))
		 bsvscemi_bind_inpipe(String paramFile, String transactorName,
				       String pipeName);
		 
import "BDPI" function ActionValue#(Bool) bsvscemi_inpipe_proxy_ready(UInt#(32) index);
import "BDPI" function Action bsvscemi_inpipe_proxy_send(Bit#(sz) data, UInt#(32) len,
							 UInt#(32) index);
import "BDPI" function Action bsvscemi_inpipe_proxy_send_immediate(Bit#(sz) data, UInt#(32) len,
							 UInt#(32) index);
import "BDPI" function ActionValue#(Bool) bsvscemi_outpipe_proxy_ready(UInt#(32) index);
import "BDPI" function ActionValue#(a)
		 bsvscemi_outpipe_proxy_data_get(UInt#(32) len, UInt#(32) index)
		 provisos(Bits#(a,sz));
import "BDPI" function ActionValue#(a)
		 bsvscemi_outpipe_proxy_data_get_immediate(UInt#(32) len, UInt#(32) index)
		 provisos(Bits#(a,sz));
		 
import "BDPI" function ActionValue#(a)
		 bsvscemi_pipe_proxy_send_get_immediate(Bit#(sb) data, UInt#(32) len, UInt#(32) index_in, UInt#(32) index_out)
		 provisos(Bits#(a, sa));	 

endpackage: SceMiProxies
