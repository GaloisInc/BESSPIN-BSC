// Copyright 2010-2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
//  Filename      : XilinxTEMAC.bsv
////////////////////////////////////////////////////////////////////////////////
package XilinxTEMAC;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Clocks            ::*;
import Vector            ::*;
import GetPut            ::*;
import ClientServer      ::*;
import Connectable       ::*;
import BRAM              ::*;
import FIFO              ::*;
import SpecialFIFOs      ::*;
import GrayCounter       ::*;
import DefaultValue      ::*;

import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export IPAddress(..);
export MACAddress(..);
export EthernetData(..);

export getData;
export matchesFirst;
export matchesData;
export matchesLast;

export GMII_Pins(..);
export EthernetMAC(..);

export mkEthernetMAC;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef Bit#(32)         IPAddress;
typedef Bit#(48)         MACAddress;

typedef struct {
   Bool      sof;
   Bool      eof;
   Bit#(8)   data;
} EthernetData deriving (Bits, Eq);   

typedef struct {
   MACAddress       destmac;
   MACAddress       srcmac;
   UInt#(16)        ethertype;
} EthernetIIHeader deriving (Bits, Eq);

////////////////////////////////////////////////////////////////////////////////
/// Functions
////////////////////////////////////////////////////////////////////////////////
function Bit#(8) getData(EthernetData x);
   return x.data;
endfunction

function Bool matchesFirst(EthernetData x);
   return x.sof;
endfunction
   
function Bool matchesData(EthernetData x);
   return (!x.sof && !x.eof);
endfunction
   
function Bool matchesLast(EthernetData x);
   return x.eof;
endfunction
   
////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface GMII;
   interface Clock       tx_clk;
   method    Bit#(8)     txd;
   method    Bit#(1)     tx_en;
   method    Bit#(1)     tx_er;
      
   method    Action      rxd(Bit#(8) i);
   method    Action      rx_dv(Bit#(1) i);
   method    Action      rx_er(Bit#(1) i);
      
   method    Action      col(Bit#(1) i);
   method    Action      crs(Bit#(1) i);
endinterface: GMII

(* always_enabled, always_ready *)
interface GMII_Pins;
   (* prefix = "" *)
   interface Clock       tx_clk;
   (* prefix = "", result = "TXD" *)
   method    Bit#(8)     txd;
   (* prefix = "", result = "TX_EN" *)
   method    Bit#(1)     tx_en;
   (* prefix = "", result = "TX_ER" *)
   method    Bit#(1)     tx_er;
      
   //(* prefix = "" *)
   //interface Clock       rx_clk;
   (* prefix = "" *)
   method    Action      rxd((* port = "RXD" *) Bit#(8) i);
   (* prefix = "" *)
   method    Action      rx_dv((* port = "RX_DV" *) Bit#(1) i);
   (* prefix = "" *)
   method    Action      rx_er((* port = "RX_ER" *) Bit#(1) i);
      
   (* prefix = "" *)
   method    Action      col((* port = "COL" *) Bit#(1) i);
   (* prefix = "" *)
   method    Action      crs((* port = "CRS" *) Bit#(1) i);

   (* prefix = "" *)
   interface Reset       mac_rst_n;
endinterface: GMII_Pins

(* always_enabled, always_ready *)
interface MAC_RX;
   interface Clock       clk;
   interface Reset       rst;
   method    Bit#(8)     data;
   method    Bool        data_valid;
   method    Bool        good_frame;
   method    Bool        bad_frame;
   method    Bool        frame_drop;
endinterface: MAC_RX

(* always_enabled, always_ready *)
interface RX_MAC;
   method    Action      data(Bit#(8) i);
   method    Action      data_valid(Bool i);
   method    Action      good_frame(Bool i);
   method    Action      bad_frame(Bool i);
   method    Action      frame_drop(Bool i);
endinterface: RX_MAC

(* always_enabled, always_ready *)
interface MAC_RX_STATS;
   method    Bit#(7)   	 stats;
   method    Bool      	 stats_valid;
   method    Bool      	 stats_byte_valid;
endinterface: MAC_RX_STATS

(* always_enabled, always_ready *)
interface RX_STATS;
   method    Action   	 stats(Bit#(7) i);
   method    Action    	 stats_valid(Bool i);
   method    Action    	 stats_byte_valid(Bool i);
endinterface: RX_STATS

(* always_enabled, always_ready *)
interface MAC_TX;
   interface Clock       clk;
   interface Reset       rst;
   method    Action    	 data(Bit#(8) i);
   method    Action    	 data_valid(Bool i);
   method    Bool      	 ack;
   method    Action    	 first_byte(Bool i);
   method    Action    	 underrun(Bool i);
   method    Bool      	 collision;
   method    Bool      	 retransmit;
   method    Action    	 ifg_delay(Bit#(8) i);
endinterface: MAC_TX   

(* always_enabled, always_ready *)
interface TX_MAC;
   method    Bit#(8)     data;
   method    Bool        data_valid;
   method    Action      ack(Bool i);
   method    Bool        first_byte;
   method    Bool        underrun;
   method    Action      collision(Bool i);
   method    Action      retransmit(Bool i);
   method    Bit#(8)     ifg_delay;
endinterface: TX_MAC

(* always_enabled, always_ready *)
interface MAC_TX_STATS;
   method    Bit#(1)   	 stats;
   method    Bool      	 stats_valid;
   method    Bool      	 stats_byte_valid;
endinterface: MAC_TX_STATS

(* always_enabled, always_ready *)
interface TX_STATS;
   method    Action   	 stats(Bit#(1) i);
   method    Action    	 stats_valid(Bool i);
   method    Action    	 stats_byte_valid(Bool i);
endinterface: TX_STATS
  
interface TEMAC;
   interface GMII         gmii;
   interface MAC_RX       rx;
   interface MAC_RX_STATS rx_stats;
   interface MAC_TX       tx;
   interface MAC_TX_STATS tx_stats;
endinterface: TEMAC

interface RX_MAC_BUFFER;
   interface RX_MAC             rx;
   interface Get#(EthernetData) toMac;
endinterface: RX_MAC_BUFFER
      
interface TX_MAC_BUFFER;
   interface TX_MAC             tx;
   interface Put#(EthernetData) fromMac;
endinterface: TX_MAC_BUFFER

interface MAC_TO_BUFFER;
   interface RX_MAC_BUFFER      rx;
   interface TX_MAC_BUFFER      tx;
endinterface: MAC_TO_BUFFER

interface EthernetMAC;
   interface GMII_Pins          gmii;
   interface Get#(EthernetData) tx;
   interface Put#(EthernetData) rx;
endinterface: EthernetMAC

////////////////////////////////////////////////////////////////////////////////
/// Definitions
////////////////////////////////////////////////////////////////////////////////
import "BVI" xilinx_ethernet_wrapper = 
module vMkVirtex5EthernetMAC#(MACAddress macaddr, Clock gmii_rx_clk, Clock mii_tx_clk, Clock ref_clk)(TEMAC);
   Vector#(6, Bit#(8)) vaddr = unpack(macaddr);
   parameter MAC_ADDRESS = pack(reverse(vaddr));

   default_clock clk(GTX_CLK_0);
   default_reset rst(RESET_N);

   input_clock    gmii_rx_clk(GMII_RX_CLK_0) = gmii_rx_clk;
   input_clock    mii_tx_clk(MII_TX_CLK_0)   = mii_tx_clk;
   input_clock    ref_clk() = ref_clk;
   
   port CLIENTEMAC0PAUSEREQ   = 0;
   port CLIENTEMAC0PAUSEVAL   = 0;  
   
   interface GMII gmii;
      output_clock                 	tx_clk(GMII_TX_CLK_0);
      method GMII_TXD_0            	txd                                           clocked_by(ref_clk) reset_by(no_reset);
      method GMII_TX_EN_0          	tx_en                                         clocked_by(ref_clk) reset_by(no_reset);
      method GMII_TX_ER_0          	tx_er                                         clocked_by(ref_clk) reset_by(no_reset);
	 
      method                       	rxd(GMII_RXD_0)     enable((*inhigh*)rxd_en)  clocked_by(ref_clk) reset_by(no_reset);
      method                       	rx_dv(GMII_RX_DV_0) enable((*inhigh*)rdv_en)  clocked_by(ref_clk) reset_by(no_reset);
      method                       	rx_er(GMII_RX_ER_0) enable((*inhigh*)rer_en)  clocked_by(ref_clk) reset_by(no_reset);
	 
      method                            col(GMII_COL_0)     enable((*inhigh*)col_en)  clocked_by(ref_clk) reset_by(no_reset);
      method                            crs(GMII_CRS_0)     enable((*inhigh*)crs_en)  clocked_by(ref_clk) reset_by(no_reset);
   endinterface: gmii
   
   schedule (gmii_txd, gmii_tx_en, gmii_tx_er, gmii_rxd, gmii_rx_dv, gmii_rx_er, gmii_col, gmii_crs) CF
            (gmii_txd, gmii_tx_en, gmii_tx_er, gmii_rxd, gmii_rx_dv, gmii_rx_er, gmii_col, gmii_crs);
   
   interface MAC_RX rx;
      output_clock                 	clk(RX_CLIENT_CLK);
      output_reset                      rst(RX_CLIENT_RST_N) clocked_by(rx_clk);
      method EMAC0CLIENTRXD             data                 clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXDVLD          data_valid           clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXGOODFRAME     good_frame           clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXBADFRAME      bad_frame            clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXFRAMEDROP     frame_drop           clocked_by(rx_clk) reset_by(rx_rst);
   endinterface: rx
   
   schedule (rx_data_valid, rx_good_frame, rx_bad_frame, rx_frame_drop) SB (rx_data);
   schedule (rx_data_valid, rx_good_frame, rx_bad_frame, rx_frame_drop) CF (rx_data_valid, rx_good_frame, rx_bad_frame, rx_frame_drop);
   schedule (rx_data) CF (rx_data);

   
   interface MAC_RX_STATS rx_stats;
      method EMAC0CLIENTRXSTATS         stats                clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXSTATSVLD      stats_valid          clocked_by(rx_clk) reset_by(rx_rst);
      method EMAC0CLIENTRXSTATSBYTEVLD  stats_byte_valid     clocked_by(rx_clk) reset_by(rx_rst);
   endinterface: rx_stats
   
   schedule (rx_stats_stats, rx_stats_stats_valid, rx_stats_stats_byte_valid) CF (rx_stats_stats, rx_stats_stats_valid, rx_stats_stats_byte_valid);
   

   interface MAC_TX tx;
      output_clock                   	clk(TX_CLIENT_CLK);
      output_reset                      rst(TX_CLIENT_RST_N) clocked_by (tx_clk);
      method                  	     	data(CLIENTEMAC0TXD)                enable((*inhigh*)entx_0) clocked_by(tx_clk) reset_by(tx_rst);
      method                  	     	data_valid(CLIENTEMAC0TXDVLD)       enable((*inhigh*)entx_1) clocked_by(tx_clk) reset_by(tx_rst);
      method EMAC0CLIENTTXACK 	     	ack                                                          clocked_by(tx_clk) reset_by(tx_rst);
      method                  	     	first_byte(CLIENTEMAC0TXFIRSTBYTE)  enable((*inhigh*)entx_2) clocked_by(tx_clk) reset_by(tx_rst);
      method                  	     	underrun(CLIENTEMAC0TXUNDERRUN)	 enable((*inhigh*)entx_3)    clocked_by(tx_clk) reset_by(tx_rst);
      method EMAC0CLIENTTXCOLLISION  	collision                                                    clocked_by(tx_clk) reset_by(tx_rst);
      method EMAC0CLIENTTXRETRANSMIT 	retransmit          	                                     clocked_by(tx_clk) reset_by(tx_rst);
      method                         	ifg_delay(CLIENTEMAC0TXIFGDELAY)    enable((*inhigh*)entx_4) clocked_by(tx_clk) reset_by(tx_rst);
   endinterface: tx
   
   schedule (tx_data_valid, tx_ack, tx_first_byte, tx_underrun, tx_collision, tx_retransmit) SB (tx_data);
   schedule (tx_ack, tx_collision, tx_retransmit, tx_ifg_delay) CF (tx_ack, tx_collision, tx_retransmit, tx_ifg_delay);
   schedule (tx_data) C (tx_data);
   schedule (tx_ack, tx_collision) CF (tx_data_valid, tx_first_byte, tx_underrun);
   schedule (tx_data, tx_data_valid, tx_first_byte, tx_underrun) CF (tx_ifg_delay);
   schedule (tx_data_valid) C (tx_data_valid);
   schedule (tx_data_valid) CF (tx_first_byte, tx_underrun, tx_retransmit);
   schedule (tx_first_byte) C (tx_first_byte);
   schedule (tx_first_byte) CF (tx_data_valid, tx_underrun, tx_retransmit);
   schedule (tx_underrun) C (tx_underrun);
   schedule (tx_underrun) CF (tx_data_valid, tx_first_byte, tx_retransmit);
      
   interface MAC_TX_STATS tx_stats;
      method EMAC0CLIENTTXSTATS         stats               clocked_by(tx_clk) reset_by(tx_rst);
      method EMAC0CLIENTTXSTATSVLD      stats_valid         clocked_by(tx_clk) reset_by(tx_rst);
      method EMAC0CLIENTTXSTATSBYTEVLD  stats_byte_valid    clocked_by(tx_clk) reset_by(tx_rst);
   endinterface: tx_stats
   
   schedule (tx_stats_stats, tx_stats_stats_valid, tx_stats_stats_byte_valid) CF (tx_stats_stats, tx_stats_stats_valid, tx_stats_stats_byte_valid);
   
   schedule (rx_data, rx_data_valid, rx_good_frame, rx_bad_frame, rx_frame_drop, rx_stats_stats, rx_stats_stats_valid, rx_stats_stats_byte_valid) CF
	    (tx_data, tx_data_valid, tx_ack, tx_first_byte, tx_underrun, tx_collision, tx_retransmit, tx_ifg_delay, tx_stats_stats, tx_stats_stats_valid, tx_stats_stats_byte_valid);

   schedule (tx_stats_stats, tx_stats_stats_valid, tx_stats_stats_byte_valid) CF (tx_data, tx_data_valid, tx_ack, tx_first_byte, tx_underrun, tx_collision, tx_retransmit, tx_ifg_delay);
   schedule (rx_stats_stats, rx_stats_stats_valid, rx_stats_stats_byte_valid) CF (rx_data, rx_data_valid, rx_good_frame, rx_bad_frame, rx_frame_drop);
   
endmodule: vMkVirtex5EthernetMAC


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Rx Packet Buffer
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkRxPacketBuffer#(Clock wrClk, Reset wrRstN)(RX_MAC_BUFFER);
   
   Clock                                     coreclk             <- exposeCurrentClock;
   Reset                                     corerst_n           <- exposeCurrentReset;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   // EMAC
   Wire#(Bit#(8))                            wData               <- mkWire(clocked_by wrClk, reset_by wrRstN);
   Wire#(Bool)                               wDataValid          <- mkWire(clocked_by wrClk, reset_by wrRstN);
   Wire#(Bool)                               wGoodFrame          <- mkWire(clocked_by wrClk, reset_by wrRstN);
   Wire#(Bool)                               wBadFrame           <- mkWire(clocked_by wrClk, reset_by wrRstN);
   Wire#(Bool)                               wFrameDrop          <- mkWire(clocked_by wrClk, reset_by wrRstN);
   
   PulseWire                                 pwEnqueue           <- mkPulseWire(clocked_by wrClk, reset_by wrRstN);
   Reg#(Bit#(12))                            rWrCurrPtr          <- mkReg(0, clocked_by wrClk, reset_by wrRstN);
   Reg#(Bit#(12))                            rWrStartPtr         <- mkReg(0, clocked_by wrClk, reset_by wrRstN); 
   Reg#(Bool)                                rInPacket           <- mkReg(False, clocked_by wrClk, reset_by wrRstN);
   GrayCounter#(8)                           rWrPackets          <- mkGrayCounter(0, coreclk, corerst_n, clocked_by wrClk, reset_by wrRstN);
   Reg#(Bit#(8))                             rData_D1            <- mkRegU(clocked_by wrClk, reset_by wrRstN);

   // MEMORY
   BRAM_Configure                            cfg                  = defaultValue;
   cfg.latency                  = 1;
   cfg.allowWriteResponseBypass = False;
   BRAM2Port#(Bit#(12), EthernetData)        memBuffer           <- mkSyncBRAM2Server(cfg, wrClk, wrRstN, coreclk, corerst_n);

   // CLIENT
   FIFO#(EthernetData)                       fifoDeq             <- mkBypassFIFO;

   Reg#(Bit#(12))                            rRdCurrPtr          <- mkReg(0);
   Reg#(Bit#(12))                            rRdStartPtr         <- mkReg(0);
   GrayCounter#(8)                           rRdPackets          <- mkGrayCounter(0, wrClk, wrRstN);
   Reg#(Bool)                                rOutPacket          <- mkReg(False);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule enqueue_first_beat(wDataValid && !rInPacket);
      let data = EthernetData { sof: True, eof: False, data: wData };
      rData_D1   <= wData;
      rInPacket  <= True;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrStartPtr, datain: data });
      rWrCurrPtr <= rWrStartPtr + 1;
   endrule
   
   rule enqueue_next_data_beat(wDataValid && !wGoodFrame && !wBadFrame && !wFrameDrop && rInPacket);
      let data = EthernetData { sof: False, eof: False, data: wData };
      rData_D1   <= wData;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrCurrPtr, datain: data });
      rWrCurrPtr <= rWrCurrPtr + 1;
   endrule

   rule commit_packet(wGoodFrame && !wFrameDrop && rInPacket);
      let data = EthernetData { sof: False, eof: True, data: rData_D1 };
      rInPacket  <= False;
      rWrPackets.incr;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrCurrPtr - 1, datain: data });
      rWrStartPtr <= rWrCurrPtr;
   endrule

   rule punt_packet((wBadFrame || wFrameDrop) && !wGoodFrame && rInPacket);
      rInPacket  <= False;
   endrule
   
   rule dequeue_first_beat((rWrPackets.dReadGray != rRdPackets.sReadGray) && !rOutPacket);
      memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdStartPtr, datain: ? });
      rRdCurrPtr <= rRdStartPtr + 1;
      rOutPacket <= True;
   endrule
   
   rule dequeue_next_beat(rOutPacket);
      let data <- memBuffer.portB.response.get;
      fifoDeq.enq(data);
      
      if (data.eof) begin
	 rOutPacket  <= False;
	 rRdStartPtr <= rRdCurrPtr;
	 rRdPackets.incr;
      end
      else begin
	 memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdCurrPtr, datain: ? });
	 rRdCurrPtr <= rRdCurrPtr + 1;
      end
   endrule
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connectiosn / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface RX_MAC rx;
      method Action data(x)       = wData._write(x);
      method Action data_valid(x) = wDataValid._write(x);
      method Action good_frame(x) = wGoodFrame._write(x);
      method Action bad_frame(x)  = wBadFrame._write(x);
      method Action frame_drop(x) = wFrameDrop._write(x);
   endinterface: rx
   
   interface Get toMac = toGet(fifoDeq);
      
endmodule: mkRxPacketBuffer

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Tx Packet Buffer
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkTxPacketBuffer#(Clock rdClk, Reset rdRstN)(TX_MAC_BUFFER);
   
   Clock                                     coreclk             <- exposeCurrentClock;
   Reset                                     corerst_n           <- exposeCurrentReset;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   // CLIENT
   FIFO#(EthernetData)                       fifoEnq             <- mkBypassFIFO;
   Reg#(Bit#(12))                            rWrCurrPtr          <- mkReg(0);
   Reg#(Bit#(12))                            rWrStartPtr         <- mkReg(0);
   GrayCounter#(8)                           rWrPackets          <- mkGrayCounter(0, rdClk, rdRstN);
   
   // MEMORY
   BRAM_Configure                            cfg                  = defaultValue;
   cfg.latency                  = 1;
   cfg.allowWriteResponseBypass = False;
   BRAM2Port#(Bit#(12), EthernetData)        memBuffer           <- mkSyncBRAM2Server(cfg, coreclk, corerst_n, rdClk, rdRstN);
   
   // EMAC
   Wire#(Bit#(8))                            wDataOut            <- mkDWire(0, clocked_by rdClk, reset_by rdRstN);
   Wire#(Bool)                               wDataValid          <- mkDWire(False, clocked_by rdClk, reset_by rdRstN);
   Wire#(Bool)                               wAck                <- mkWire(clocked_by rdClk, reset_by rdRstN);
   Reg#(Bool)                                rUnderrun           <- mkReg(False, clocked_by rdClk, reset_by rdRstN);
   Wire#(Bool)                               wCollision          <- mkWire(clocked_by rdClk, reset_by rdRstN);
   Wire#(Bool)                               wRetransmit         <- mkWire(clocked_by rdClk, reset_by rdRstN);
   Reg#(Bit#(8))                             rIfgDelay           <- mkReg(5, clocked_by rdClk, reset_by rdRstN);

   Reg#(Bit#(12))                            rRdCurrPtr          <- mkReg(0, clocked_by rdClk, reset_by rdRstN);
   Reg#(Bit#(12))                            rRdStartPtr         <- mkReg(0, clocked_by rdClk, reset_by rdRstN);
   GrayCounter#(8)                           rRdPackets          <- mkGrayCounter(0, coreclk, corerst_n, clocked_by rdClk, reset_by rdRstN);
   Reg#(Bool)                                rAcked              <- mkReg(False, clocked_by rdClk, reset_by rdRstN);
   Reg#(Bool)                                rOutPacket          <- mkReg(False, clocked_by rdClk, reset_by rdRstN);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule enqueue_first_data_beat(fifoEnq.first.sof && !fifoEnq.first.eof);
      fifoEnq.deq;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrStartPtr, datain: fifoEnq.first });
      rWrCurrPtr <= rWrStartPtr + 1;
   endrule
   
   rule enqueue_next_data_beat(!fifoEnq.first.sof && !fifoEnq.first.eof);
      fifoEnq.deq;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrCurrPtr, datain: fifoEnq.first });
      rWrCurrPtr <= rWrCurrPtr + 1;
   endrule
   
   rule enqueue_last_data_beat(!fifoEnq.first.sof && fifoEnq.first.eof);
      fifoEnq.deq;
      memBuffer.portA.request.put(BRAMRequest{ responseOnWrite: False, write: True, address: rWrCurrPtr, datain: fifoEnq.first });
      rWrCurrPtr  <= rWrCurrPtr + 1;
      rWrStartPtr <= rWrCurrPtr + 1;
      rWrPackets.incr;
   endrule
   
   rule dequeue_first_data_beat((rWrPackets.dReadGray != rRdPackets.sReadGray) && !rOutPacket);
      rOutPacket     <= True;
      memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdStartPtr, datain: ? });
      rRdCurrPtr <= rRdStartPtr + 1;
   endrule
   
   (* preempts = "(dequeue_wait_for_ack, dequeue_got_ack, dequeue_next_data_beat), transmit_underrun" *)
   rule dequeue_wait_for_ack(rOutPacket && !rAcked && !wAck);
      let data <- memBuffer.portB.response.get;
      memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdStartPtr, datain: ? });
      wDataOut      <= getData(data);
      wDataValid    <= True;
   endrule
   
   rule dequeue_got_ack(rOutPacket && !rAcked && wAck);
      let data <- memBuffer.portB.response.get;
      memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdCurrPtr, datain: ? });
      rRdCurrPtr    <= rRdCurrPtr + 1; 
      wDataOut      <= getData(data);
      wDataValid    <= True;
      rAcked        <= True;
   endrule
   
   rule dequeue_next_data_beat(rOutPacket && rAcked);
      let data <- memBuffer.portB.response.get;
      wDataOut      <= getData(data);
      wDataValid    <= True;

      if (data.eof) begin
	 rAcked      <= False;
	 rOutPacket  <= False;
	 rRdStartPtr <= rRdCurrPtr;
	 rRdPackets.incr;
      end
      else if (!data.sof && !data.eof) begin
	 memBuffer.portB.request.put(BRAMRequest{ responseOnWrite: False, write: False, address: rRdCurrPtr, datain: ? });
	 rRdCurrPtr  <= rRdCurrPtr + 1;
      end
   endrule
   
   rule transmit_underrun(rOutPacket && !rUnderrun);
      rUnderrun  <= True;
      rOutPacket <= False;
      rAcked     <= False;
   endrule
   
   rule clear_underrun(rUnderrun);
      rUnderrun  <= False;
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connectiosn / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface TX_MAC tx;
      method Bit#(8) data          = wDataOut;
      method Bool    data_valid    = wDataValid;
      method Action  ack(i)        = wAck._write(i);
      method Bool    first_byte    = False;
      method Bool    underrun      = rUnderrun;
      method Action  collision(i)  = wCollision._write(i);
      method Action  retransmit(i) = wRetransmit._write(i);
      method Bit#(8) ifg_delay     = rIfgDelay;
   endinterface: tx
   
   interface Put fromMac = toPut(fifoEnq);
            
endmodule: mkTxPacketBuffer

////////////////////////////////////////////////////////////////////////////////
/// Ethernet Module
////////////////////////////////////////////////////////////////////////////////
module mkEthernetMAC#(MACAddress macaddr, Clock gmii_rx_clk, Clock mii_tx_clk, Clock ref_clk)(EthernetMAC);
   
   Clock                                     clk                 <- exposeCurrentClock;
   Reset                                     rst_n               <- exposeCurrentReset;
   
   Reset                                     macreset_n          <- mkAsyncReset(100, rst_n, clk);
   Reset                                     gmii_rx_rst_n       <- mkAsyncReset(0, rst_n, gmii_rx_clk);
   Reset                                     macreset_ref_n      <- mkAsyncReset(0, macreset_n, ref_clk);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   IDELAYCTRL                                dlyctrl0            <- mkIDELAYCTRL(12, clocked_by ref_clk);
   Clock                                     gmii_rxc0_delay     <- mkClockIODELAY(defaultValue, clocked_by gmii_rx_clk, reset_by gmii_rx_rst_n);
   
   Clock                                     rx_clk               = gmii_rxc0_delay;
  
   TEMAC                                     mac                 <- vMkVirtex5EthernetMAC(macaddr, rx_clk, mii_tx_clk, ref_clk);
   
   Clock                                     rx_client_clk        = mac.rx.clk;
   Reset                                     rx_client_rst_n      = mac.rx.rst;
   Clock                                     tx_client_clk        = mac.tx.clk;
   Reset                                     tx_client_rst_n      = mac.tx.rst;
   
   RX_MAC_BUFFER                             rx_buffer           <- mkRxPacketBuffer(rx_client_clk, rx_client_rst_n);
   TX_MAC_BUFFER                             tx_buffer           <- mkTxPacketBuffer(tx_client_clk, tx_client_rst_n);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   mkConnection(rx_buffer.rx, mac.rx);
   mkConnection(tx_buffer.tx, mac.tx);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface GMII_Pins gmii;
      interface tx_clk    = mac.gmii.tx_clk;
      method    txd       = mac.gmii.txd;
      method    tx_en     = mac.gmii.tx_en;
      method    tx_er     = mac.gmii.tx_er;
      method    rxd(x)    = mac.gmii.rxd(x);
      method    rx_dv(x)  = mac.gmii.rx_dv(x);
      method    rx_er(x)  = mac.gmii.rx_er(x);
      method    col(x)    = mac.gmii.col(x);
      method    crs(x)    = mac.gmii.crs(x);
      interface mac_rst_n = macreset_ref_n;
   endinterface

   interface tx        = rx_buffer.toMac;
   interface rx        = tx_buffer.fromMac;

endmodule: mkEthernetMAC

////////////////////////////////////////////////////////////////////////////////
/// Connection Templates
////////////////////////////////////////////////////////////////////////////////
instance Connectable#(MAC_RX, RX_MAC);
   module mkConnection#(MAC_RX m, RX_MAC r)(Empty);
      rule connect;
         r.data(m.data);
	 r.data_valid(m.data_valid);
	 r.good_frame(m.good_frame);
	 r.bad_frame(m.bad_frame);
	 r.frame_drop(m.frame_drop);
      endrule
   endmodule
endinstance
instance Connectable#(RX_MAC, MAC_RX);
   module mkConnection#(RX_MAC r, MAC_RX m)(Empty);
      mkConnection(m, r);
   endmodule
endinstance

instance Connectable#(MAC_TX, TX_MAC);
   module mkConnection#(MAC_TX m, TX_MAC t)(Empty);
      rule connect_a;
         m.data(t.data);
      endrule
      rule connect_b;
	 t.ack(m.ack);
      endrule
      rule connect_c;
	 t.collision(m.collision);
      endrule
      rule connect_d;
	 t.retransmit(m.retransmit);
      endrule
      rule connect_e;
	 m.data_valid(t.data_valid);
      endrule
      rule connect_f;
	 m.first_byte(t.first_byte);
      endrule
      rule connect_g;
	 m.underrun(t.underrun);
      endrule
      rule connect_h;
	 m.ifg_delay(t.ifg_delay);
      endrule
   endmodule
endinstance
instance Connectable#(TX_MAC, MAC_TX);
   module mkConnection#(TX_MAC t, MAC_TX m)(Empty);
      mkConnection(m, t);
   endmodule
endinstance

instance Connectable#(MAC_RX_STATS, RX_STATS);
   module mkConnection#(MAC_RX_STATS m, RX_STATS r)(Empty);
      rule connect;
	 r.stats(m.stats);
	 r.stats_valid(m.stats_valid);
	 r.stats_byte_valid(m.stats_byte_valid);
      endrule
   endmodule
endinstance
instance Connectable#(RX_STATS, MAC_RX_STATS);
   module mkConnection#(RX_STATS r, MAC_RX_STATS m)(Empty);
      mkConnection(m, r);
   endmodule
endinstance

instance Connectable#(MAC_TX_STATS, TX_STATS);
   module mkConnection#(MAC_TX_STATS m, TX_STATS t)(Empty);
      rule connect;
	 t.stats(m.stats);
	 t.stats_valid(m.stats_valid);
	 t.stats_byte_valid(m.stats_byte_valid);
      endrule
   endmodule
endinstance
instance Connectable#(TX_STATS, MAC_TX_STATS);
   module mkConnection#(TX_STATS t, MAC_TX_STATS m)(Empty);
      mkConnection(m, t);
   endmodule
endinstance

endpackage: XilinxTEMAC
