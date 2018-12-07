// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiDefines;

// This is a set of universal SceMi definitions based on the standard.
// It is intended to be used with multiple different implementations,
// which vary the communication mechanism and target platform.

// The user should never need to import this package directly -- instead,
// they should import only the SceMi package.

import GetPut :: *;
import Vector :: *;

// -------------------
typedef 4             BPB;
typedef TMul#(8, BPB) BITSPB;
// -------------------

// BSV adaptation of the SceMiMessageInPort macro.
// This is the BSV safe version of the try/accept protocol.
// This differs from the SceMi spec in that the read method is not always ready
interface SceMiMessageInPortIfc#(type msg_type);
   method Action request();
   method Bool request_pending();
   method msg_type read();
   method Action ack();
endinterface: SceMiMessageInPortIfc

// BSV adaptation of the SceMiMessageOutPort macro
// interface described in section 5.2.3 of the SCE-MI 2.0
// specification.
interface SceMiMessageOutPortIfc#(type msg_type);
   (* always_ready *)
   method Bool accepting_data();
   method Action send(msg_type msg);
endinterface: SceMiMessageOutPortIfc

// Allow input ports to be treated as Get interfaces
instance ToGet#(SceMiMessageInPortIfc#(a),a);
   function toGet(p) = (interface Get;
                           method ActionValue#(a) get();
                              p.ack();
                              return p.read();
                           endmethod
                        endinterface);
endinstance

// Allow output ports to be treated as Put interfaces
instance ToPut#(SceMiMessageOutPortIfc#(a),a);
   function toPut(p) = (interface Put;
                           method put = p.send;
                        endinterface);
endinstance

// -------------------

typedef UInt#(16) NoCCredits;

typedef enum { Deferred
             , Immediate
             , Fifo
             } Visibility deriving (Eq);

// BSV adaptation of the scemi_input_pipe HDL-side interface,
// described in section 5.7.1.2 of the SCE-MI 2.0 specification.
//
interface SceMiInputPipeIfc#(numeric type max_elems, type elem_type);

/* // XXX This should go in a blocking form of the pipe
   // First perform the request,
   // then receive (possibly blocked until ready)
   method Action                         request(UInt#(32) num_elems);
   method ActionValue#(Tuple3#(UInt#(32),
                               Vector#(max_elems, elem_type),
                               Bool))    receive();
*/

   (* always_ready *)
   method ActionValue#(Tuple3#(UInt#(32),
                               Vector#(max_elems, elem_type),
                               Bool))    try_receive(UInt#(32) num_elems);

   (* always_ready *)
   method UInt#(32) can_receive();

endinterface: SceMiInputPipeIfc

// Allow input pipes to be treated as Get interfaces
// The trade-off with using this style interface is that you
// will only get one object at a time vs. what is allowed by
// the SceMi pipes standard which is multiple objects in a
// single request.
instance ToGet#(SceMiInputPipeIfc#(1,a), a);
   function toGet(p) = (interface Get;
			   method ActionValue#(a) get() if (p.can_receive() > 0);
			      match { .n, .v, .eom } <- p.try_receive(1);
			      if (n != 1)
				 $display("Time %0d: ERROR: incomplete pipe receive: expected %0d, got %0d", $time, 1, n);
			      return v[0];
			   endmethod
			endinterface);
endinstance


// BSV adaptation of the scemi_output_pipe HDL-side interface,
// described in section 5.7.1.2 of the SCE-MI 2.0 specification.
//
interface SceMiOutputPipeIfc#(numeric type max_elems, type elem_type);

/* // XXX This should go in a blocking form of the pipe
   method Action send(UInt#(32)                      num_elems,
                      Vector#(max_elems, elem_type)  data,
                      Bool                           eom);
*/

   (* always_ready *)
   method ActionValue#(UInt#(32))
                 try_send(UInt#(32)                      num_elems,
                          Vector#(max_elems, elem_type)  data,
                          Bool                           eom);

   (* always_ready *)
   method UInt#(32) can_send();

   (* always_ready *)
   method Action flush();

endinterface: SceMiOutputPipeIfc

// Allow output pipes to be treated as Put interfaces
// The trade-off with using this style interface is that you
// can only put one object at a time vs. what is allowed by the
// SceMi pipes standard which is multiple objects in a single
// request.
instance ToPut#(SceMiOutputPipeIfc#(1,a),a);
   function toPut(p) = (interface Put;
			   method Action put(a x) if (p.can_send() > 0);
			      let n <- p.try_send(1, replicate(x), False);
			      if (n != 1)
				 $display("Time %0d: ERROR: incomplete pipe send: attempted %0d, got %0d", $time, 1, n);
			   endmethod
			endinterface);
endinstance

// -------------------

// BSV adaptation of the SceMiClockPort macro
// interface described in section 5.2.4 of the SCE-MI 2.0
// specification.
interface SceMiClockPortIfc;
   interface Clock cclock;
   interface Reset creset;
endinterface: SceMiClockPortIfc

// BSV adaptation of the SceMiClockControl macro
// interface described in section 5.2.5 of the SCE-MI 2.0
// specification.  Control inputs are given as interface
// arguments to the mkSceMiClockControl module because
// of clock-domain crossing restrictions on rules.
interface SceMiClockControlIfc;
   interface Clock uclock;
   interface Reset ureset;
   (* always_ready *)
   method Bool pre_posedge();
   (* always_ready *)
   method Bool pre_negedge();
endinterface: SceMiClockControlIfc

// Type used to represent controlled time
typedef UInt#(64) SceMiTime;

// Type used to represent cycle stamps
typedef UInt#(64) SceMiCycleStamp;

// Mechanism for handling different linkage types.
// There is a definition in src/comp/SceMiElement
// .hs that
// needs to match this one.
typedef enum { TCP
             , SCEMI
             , EVE
             , ALDEC
             , PCIE_VIRTEX5
             , PCIE_VIRTEX6
             , PCIE_KINTEX7
	     , PCIE_VIRTEX7
	     , PCIE_VIRTEXU
             , PCIE_DINI
	     , PCIE_ARRIA10
	     , TCP_NOCLOCK
             , UNDEFINED
             } SceMiLinkType deriving (Eq,Bits);

function Bool isXilinxSceMiLinkType(SceMiLinkType link_type);
   return ((link_type == PCIE_VIRTEX5) ||
	   (link_type == PCIE_VIRTEX6) ||
	   (link_type == PCIE_KINTEX7) ||
	   (link_type == PCIE_VIRTEX7) ||
	   (link_type == PCIE_VIRTEXU));
endfunction

// Mechanism for handling different clock generation types.
typedef enum { FAST
             , FAST_INVERTING
             , REGULAR
             } SceMiClockGenType deriving (Eq,Bits);

// BSV adaptation of the SceMiMessageInPortProxy macro
// interface described in section 5.2.2 of the SCE-MI 2.0
// specification.
interface SceMiMessageInPortProxyIfc#(type msg_type);
   (* always_ready *)
   method Bool accepting_data();
   method Action send(msg_type msg);
endinterface: SceMiMessageInPortProxyIfc

// BSV adaptation of the SceMiMessageOutPort macro
// interface described in section 5.2.3 of the SCE-MI 2.0
// specification.
interface SceMiMessageOutPortProxyIfc#(type msg_type);
   (* always_ready *)
   method Bool has_data();
   method msg_type read();
   method SceMiCycleStamp cycle_stamp();
   method Action shutdown();
   (* always_ready *)
   method Action ack();
endinterface: SceMiMessageOutPortProxyIfc

// Allow input port proxy to be treated as Put interfaces
instance ToPut#(SceMiMessageInPortProxyIfc#(a),a);
   function toPut(p) = (interface Put;
                           method put = p.send;
                        endinterface);
endinstance

// Allow output port proxy to be treated as Get interfaces
instance ToGet#(SceMiMessageOutPortProxyIfc#(a),a);
   function toGet(p) = (interface Get;
                           method ActionValue#(a) get();
                              p.ack();
                              return p.read();
                           endmethod
                        endinterface);
endinstance

// Interface for "Breadcrumb" module to be included in probe and capture
// modules, for conveying information to the link params file:
interface SceMiSerialInfo#(type a);
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface SceMiInPipeProxyIfc#(type msg_type);
   (* always_ready *)
   method Bool accepting_data();
   method Action send(msg_type msg);
endinterface: SceMiInPipeProxyIfc

interface SceMiOutPipeProxyIfc#(type msg_type);
   (* always_ready *)
   method Bool has_data();
   method msg_type read();
   method Action shutdown();
   (* always_ready *)
   method Action ack();
endinterface: SceMiOutPipeProxyIfc

// Allow input pipe proxy to be treated as a Put interface
instance ToPut#(SceMiInPipeProxyIfc#(a),a);
   function toPut(p) = (interface Put;
                           method put = p.send;
                        endinterface);
endinstance

// Allow output pipe proxy to be treated as Get interface
instance ToGet#(SceMiOutPipeProxyIfc#(a),a);
   function toGet(p) = (interface Get;
                           method ActionValue#(a) get();
                              p.ack();
                              return p.read();
                           endmethod
                        endinterface);
endinstance

interface SceMiPipeProxy#(type a, type b);
   interface Get#(a) outputs;
   interface Put#(b) inputs;
endinterface

interface SceMiPipe#(type a, type b);
   interface Get#(a) inputs;
   interface Put#(b) outputs;
endinterface

endpackage: SceMiDefines
