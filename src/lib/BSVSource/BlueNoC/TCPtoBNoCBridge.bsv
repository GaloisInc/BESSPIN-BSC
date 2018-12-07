////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : TCPtoBNoCBridge.bsv
//  Description   : A bridge between a TCP socket stream interface on one side 
//                  and a NoC interface on the other
////////////////////////////////////////////////////////////////////////////////
package TCPtoBNoCBridge;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import GetPut            ::*;
import Connectable       ::*;
import Vector            ::*;
import FIFO              ::*;
import FIFOF             ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import Clocks            ::*;
import MsgFormat         ::*;

import "BDPI" function ActionValue#(Int#(32)) bluenoc_open_tcp_socket(UInt#(32) tcp_port);
import "BDPI" function Action                 bluenoc_close_tcp_socket(Int#(32) socket_descriptor);
import "BDPI" function ActionValue#(UInt#(8)) bluenoc_send_tcp_beat(  Int#(32)     socket_descriptor
                                                                    , UInt#(8)     beat_size
                                                                    , UInt#(8)     bytes_already_sent
                                                                    , MsgBeat#(16) beat_value
                                                                    );
import "BDPI" function ActionValue#(Tuple2#(MsgBeat#(16),UInt#(8))) bluenoc_recv_tcp_beat(  Int#(32)     socket_descriptor
                                                                                          , UInt#(8)     beat_size
                                                                                          , UInt#(8)     bytes_already_received
                                                                                          , MsgBeat#(16) beat_value_so_far
                                                                                          );
                 

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
interface TCPtoBNoC#(numeric type bpb);
   method ActionValue#(Bool) listen(UInt#(32) port_number);

   method Action close();

   (* always_ready *)
   method Bool active();
      
   interface MsgPort#(bpb) noc;
endinterface: TCPtoBNoC
                 
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkTCPtoBNoC(TCPtoBNoC#(bpb))
   provisos (  Add#(_1, TMul#(8, bpb), 128)
             );
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   Reg#(Int#(32))                  rFileHandle         <- mkReg(-1);
   PulseWire                       pwShutdown          <- mkPulseWire();
   ReadOnly#(Bool)                 inReset             <- isResetAsserted;

   Reg#(MsgBeat#(bpb))             rBeatFromTCP        <- mkRegU;
   Reg#(UInt#(8))                  rBytesFromTCP       <- mkReg(0);
   PulseWire                       pwFromTCPReady      <- mkPulseWire;
   FIFOF#(MsgBeat#(bpb))           fOutgoingData       <- mkLFIFOF;
   PulseWire                       pwOutgoingValid     <- mkPulseWire;
   Wire#(MsgBeat#(bpb))            wOutgoingData       <- mkDWire(?);

   PulseWire                       pwIncomingBeat      <- mkPulseWire;
   Wire#(MsgBeat#(bpb))            wIncomingData       <- mkBypassWire;
   FIFOF#(MsgBeat#(bpb))           fIncomingData       <- mkLFIFOF;
   Reg#(UInt#(8))                  rBytesToTCP         <- mkReg(0);
   Reg#(MsgBeat#(bpb))             rBeatToTCP          <- mkRegU;
   
   Bool                            bReadyToRecvTCP_0    = rBytesFromTCP == 0;
   Bool                            bReadyToRecvTCP_bpb  = rBytesFromTCP == fromInteger(valueOf(bpb));
   Bool                            bReadyToRecvTCP      = bReadyToRecvTCP_0 || bReadyToRecvTCP_bpb;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Outgoing Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, aggressive_implicit_conditions *)
   rule recv_beat_from_tcp if ((rFileHandle >= 0) && bReadyToRecvTCP);
      match { .data, .count } <- bluenoc_recv_tcp_beat(rFileHandle, fromInteger(valueOf(bpb)), rBytesFromTCP, cExtend(rBeatFromTCP));
      if ((rBytesFromTCP + count) == fromInteger(valueOf(bpb))) begin
	 fOutgoingData.enq(cExtend(data));
	 rBytesFromTCP <= 0;
//	 $display("[%t] HW_RECV %d %x", $time, count, data);
      end
      else begin
	 rBytesFromTCP <= rBytesFromTCP + count;
	 rBeatFromTCP  <= cExtend(data);
      end
   endrule
   
   (* fire_when_enabled *)
   rule process_outgoing_data_beat if (pwOutgoingValid);
      fOutgoingData.deq();
   endrule
   
   (* fire_when_enabled *)
   rule drive_outgoing_data;
      wOutgoingData <= pack(fOutgoingData.first());
   endrule
      
   (* fire_when_enabled *)
   rule process_incoming_data_beat if (pwIncomingBeat);
      fIncomingData.enq(wIncomingData);
   endrule

   (* fire_when_enabled, aggressive_implicit_conditions *)
   rule send_beat_to_tcp;
      UInt#(8) x = (rBytesToTCP == 0) ? fromInteger(valueOf(bpb)) : rBytesToTCP;
      UInt#(8) n <- bluenoc_send_tcp_beat(rFileHandle, fromInteger(valueOf(bpb)), x, cExtend(fIncomingData.first()));

      if (n != fromInteger(valueOf(bpb))) begin
	 rBytesToTCP <= fromInteger(valueOf(bpb)) - n;
      end
      else begin
	 rBytesToTCP <= 0;
//	 $display("[%t] HW_SEND %d %x", $time, x, fIncomingData.first());
	 fIncomingData.deq;
      end
   endrule

   
   (* fire_when_enabled, no_implicit_conditions *)
   rule close_socket if ((rFileHandle >= 0) && pwShutdown);
      bluenoc_close_tcp_socket(rFileHandle);
      rFileHandle <= -1;
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   method ActionValue#(Bool) listen(UInt#(32) port_number) if (rFileHandle == -1 && !inReset);
      Int#(32) res <- bluenoc_open_tcp_socket(port_number);
      rFileHandle <= (res >= 0) ? res : -1;
      return (res >= 0);
   endmethod
   
   method Action close() if (rFileHandle >= 0);
      pwShutdown.send;
   endmethod
   
   method Bool active();
      return (rFileHandle >= 0);
   endmethod
   
   interface MsgPort noc;
      interface MsgSource out;
         method Action dst_rdy(Bool b);
	    if (b && fOutgoingData.notEmpty())
               pwOutgoingValid.send();
         endmethod
         method Bool src_rdy();
	    return fOutgoingData.notEmpty();
         endmethod
         method MsgBeat#(bpb) beat();
            return wOutgoingData;
         endmethod
      endinterface
      interface MsgSink in;
         method Bool dst_rdy();
	    return fIncomingData.notFull() && (rFileHandle >= 0);
         endmethod
         method Action src_rdy(Bool b);
            if (b) pwIncomingBeat.send();
         endmethod
         method Action beat(MsgBeat#(bpb) v);
            wIncomingData <= v;
         endmethod
      endinterface
   endinterface

endmodule: mkTCPtoBNoC

endpackage: TCPtoBNoCBridge
