////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
package Arria10PCIE;

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Clocks            ::*;
import Vector            ::*;
import BuildVector       ::*;
import Connectable       ::*;
import GetPut            ::*;
import Reserved          ::*;
import TieOff            ::*;
import DefaultValue      ::*;
import DReg              ::*;
import Gearbox           ::*;
import FIFO              ::*;
import FIFOF             ::*;
import SpecialFIFOs      ::*;
import BRAMFIFO          ::*;
import ClientServer      ::*;
import BUtils            ::*;
import FIFOLevel         ::*;
import Cntrs             ::*;

import PCIE              ::*;

import ArriaBVI          ::*;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////

// Constants
typedef                              512 MaxPayload;   // bytes, from endpoint configuration
typedef TDiv#(TAdd#(MaxPayload, 20), 16) TotalPktSize; // 128-bit beats

typedef struct {
   Bool          sof;
   Bool          last;
   Bit#(7)       bar;
   Bit#(128)     data;
} AxiRx deriving (Bits, Eq);

typedef struct {
   Bool          sof;
   Bool          last;
   Bool          empty;
   Bit#(128)     data;
} AxiTx deriving (Bits, Eq);

////////////////////////////////////////////////////////////////////////////////
/// Interfaces original PCIE TRN for bridge
////////////////////////////////////////////////////////////////////////////////

interface PCIE_TRN_COMMON_A;
   interface Clock       clk;
   interface Reset       reset_n;
   method    Bool        link_up;
   method    PciId       pci_id;
   method    Bit#(3)     max_rd_req_bytes;
   method    Bit#(3)     max_payload_bytes;
   method    Bool        read_completion_boundary;
   method    Bool        msix_enable;
   method    Bool        msix_mask;
endinterface

interface PCIE_TRN_XMIT_A;
   method    Action      xmit(TLPData#(16) data);
endinterface

typedef Get#(TLPData#(16)) PCIE_TRN_RECV_A;

interface PCIExpressA#(numeric type lanes);
   interface Pcie              pcie;
   interface PCIE_TRN_COMMON_A trn;
   interface PCIE_TRN_XMIT_A   trn_tx;
   interface PCIE_TRN_RECV_A   trn_rx;
endinterface

////////////////////////////////////////////////////////////////
// Special clock-handling primitives
////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface OutputBit;
   method Bit#(1) out;
endinterface

import "BVI" ASSIGN1 =
module packClock#(Clock clk)(OutputBit);

   default_clock no_clock;
   default_reset no_reset;

   input_clock clk(IN) = clk;

   method OUT out;

   schedule (out) CF (out);

endmodule

interface ClockHolder;
   interface Clock heldClk;
   (*always_ready, always_enabled*)
   method Action inBit(Bit#(1) x);
endinterface

import "BVI" ASSIGN1 =
module mkClockHolder(ClockHolder);

   default_clock no_clock;
   default_reset no_reset;

   output_clock heldClk(OUT);
   method inBit(IN) enable((*inhigh*)en0) clocked_by (heldClk);
endmodule

module mkHoldClock#(ClockHolder h)(Empty);
   let c <- exposeCurrentClock();
   let p <- packClock(c);
   (*no_implicit_conditions, fire_when_enabled*)
   rule every;
      h.inBit(p.out);
   endrule
endmodule

////////////////////////////////////////////////////////////////
// The BSV PCIE endpoint (A for Altera, Arria)
////////////////////////////////////////////////////////////////

(* no_default_clock, no_default_reset *)
module mkPCIExpressEndpointA#(Clock ref_clk, Reset pin_perst)(PCIExpressA#(lanes));

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   ClockHolder pld_clk_holder         <- mkClockHolder;
   Arria10GX128PCIExpress ep          <- vMkArria10GX128PCIExpress(pld_clk_holder.heldClk,
								   pin_perst,
								   pin_perst,
								   clocked_by ref_clk,
								   reset_by noReset);
   Clock               clk125        = ep.coreclkout_hip;

   mkHoldClock(pld_clk_holder, clocked_by clk125);

   Reg#(UInt#(6))      rst_ctr <- mkRegU(clocked_by clk125);
   let rst_ifc <- mkResetSync(4,True,clk125, clocked_by clk125, reset_by noReset);
   rule dec_rst_ctr (rst_ctr != 0);
      rst_ctr <= rst_ctr - 1;
      rst_ifc.assertReset();
   endrule

   rule set_rst_ctr (ep.reset_status || !(ep.pld_clk_inuse && ep.dlup_exit && ep.hotrst_exit && ep.l2_exit));
      rst_ctr <= 36;   // must be at least 32
   endrule

   Reset               user_reset_n    = rst_ifc.new_rst;

   Reg#(Bool)          outOfReset     <- mkReg(False, clocked_by clk125, reset_by user_reset_n);
   rule setOutOfReset;
      // This is effective only when outOfReset is out of reset:
      outOfReset <= True;
   endrule


   Wire#(Bool)                               wAxiTxValid         <- mkDWire(False, clocked_by clk125, reset_by noReset);
   Wire#(Bool)                               wAxiTxSof           <- mkDWire(False, clocked_by clk125, reset_by noReset);
   Wire#(Bool)                               wAxiTxLast          <- mkDWire(False, clocked_by clk125, reset_by noReset);
   Wire#(Bool)                               wAxiTxEmpty         <- mkDWire(False, clocked_by clk125, reset_by noReset);
   Wire#(Bit#(128))                          wAxiTxData          <- mkDWire(0, clocked_by clk125, reset_by noReset);
   Wire#(Bool)                               wAxiRxReady         <- mkDWire(False, clocked_by clk125, reset_by noReset);
   Wire#(Bool)                               wSending            <- mkDWire(False, clocked_by clk125, reset_by noReset);

   // This FIFO must be large enough to hold a full-size TLP, plus some extra:
   FIFOF#(AxiTx)                             fAxiTx              <- mkSizedFIFOF(valueof(TotalPktSize) + 8,
										 clocked_by clk125, reset_by user_reset_n);
   // Counter for number of TLPs it contains:
   Count#(UInt#(5))                          rxCntr              <- mkCount(0, clocked_by clk125, reset_by user_reset_n);

   // The siae of thie FIFO is not critical, provided it can contain the watermarks.  We choose to let it contain at least
   // one full-sized paxket:
   FIFOLevelIfc#(AxiRx, TAdd#(TotalPktSize, 16)) fAxiRx          <- mkFIFOLevel(clocked_by clk125, reset_by user_reset_n);
   Reg#(Bool)                                rg_okToReceive      <- mkReg(True, clocked_by clk125, reset_by user_reset_n);

   // Configuration values, obtained from endpoint:
   Reg#(PciId)    rg_id                 <- mkRegU(clocked_by clk125);
   Reg#(Bit#(3))  rg_max_read_req_bytes <- mkRegU(clocked_by clk125);
   Reg#(Bit#(3))  rg_max_payload_bytes  <- mkRegU(clocked_by clk125);
   Reg#(Bool)     rg_completion_bdry    <- mkRegU(clocked_by clk125);
   Reg#(Bool)     rg_msix_enable        <- mkRegU(clocked_by clk125);
   Reg#(Bool)     rg_msix_mask          <- mkRegU(clocked_by clk125);

   // working registers:
   Reg#(UInt#(4)) rg_ctr <- mkRegU(clocked_by clk125);
   Reg#(UInt#(4)) rg_max <- mkRegU(clocked_by clk125);
   Reg#(Bit#(4))  rg_adr <- mkRegU(clocked_by clk125);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////

   (* fire_when_enabled, no_implicit_conditions *)
   rule tieoff;
      ep.test_in('h188);
      ep.simu_mode_pipe(0);
      ep.hpg_ctrler(0);
      ep.cpl_err(0);
      ep.cpl_pending(0);

      ep.pm_auxpwr(0);
      ep.pm_data(0);
      ep.pme_to_cr(0);
      ep.pm_event(0);

      ep.tx_cred_fc_sel(0);

      ep.app_int_sts(0);
      ep.app_msi_req(0);
      ep.app_msi_num(0);
      ep.app_msi_tc(0);
      ep.tx_st_err(False);
      ep.rx_st_mask(0);

      ep.pld_core_ready(ep.serdes_pll_locked);
   endrule

   // obtain configuration values:
   rule r1;
      let adr = ep.tl_cfg_add;
      let v   = ep.tl_cfg_ctl;
      if (rg_ctr == (rg_max/2)) begin // strobe the value
	 case (adr)
	    0:  begin
		   rg_max_read_req_bytes <= v[26:24];
		   rg_max_payload_bytes  <= v[18:16];
		end
	    2:  begin
		   rg_completion_bdry <= unpack(v[19]);
		end
	    13: begin
		   rg_msix_enable <= unpack(v[31]);
		   rg_msix_mask   <= unpack(v[30]);
		end
	    15: begin
		   rg_id <= PciId{bus: v[12:5],
				   dev: v[4:0],
				   func: 0};
		end
	    default: noAction;
	 endcase
      end
      if (rg_adr != adr) begin
	 rg_max <= rg_ctr;
	 rg_ctr <= 0;
      end
      else rg_ctr <= rg_ctr+1;
      rg_adr <= adr;
   endrule

   // sending:

   Reg#(Maybe#(AxiTx)) rg_maybeTop   <- mkReg(Invalid, clocked_by clk125, reset_by user_reset_n);
   Reg#(Bool)          rg_sending    <- mkReg(False, clocked_by clk125, reset_by user_reset_n);
   Reg#(Bool)          rg_inPkt      <- mkReg(False, clocked_by clk125, reset_by user_reset_n);
   PulseWire           pw_BackToBack <- mkPulseWire(clocked_by clk125, reset_by user_reset_n);
   PulseWire           pw_MoveOn     <- mkPulseWire(clocked_by clk125, reset_by user_reset_n);
   Reg#(Bool)          rg_inPktEnq   <- mkReg(False, clocked_by clk125, reset_by user_reset_n);

   rule reset_sending (!isValid(rg_maybeTop));
      rg_sending <= False;
      rg_inPkt   <= False;
   endrule

   rule fillTop (!isValid(rg_maybeTop));
      let x <- toGet(fAxiTx).get();
      rg_maybeTop <= tagged Valid x;
   endrule

   rule startSending(rg_maybeTop matches tagged Valid .t &&&
		     !rg_sending &&&
		     ep.tx_st_ready &&&
		     rxCntr != 0 &&&
		     !rg_inPkt &&&
		     t.sof &&&
		     outOfReset);
      rg_sending <= True;
      rg_inPkt   <= True;
   endrule

   rule resumeSending(!rg_sending &&
		      ep.tx_st_ready &&
		      rg_inPkt);
      rg_sending <= True;
   endrule

   rule canBackToBack (fAxiTx.first.sof);
      pw_BackToBack.send();
   endrule

   rule send (rg_maybeTop matches tagged Valid .t &&&
	      rg_sending);
      let sending = True;
      wSending    <= sending;
      wAxiTxSof   <= t.sof;
      wAxiTxLast  <= t.last;
      wAxiTxEmpty <= t.empty;
      wAxiTxData  <= t.data;

      if (t.last) begin
	 rxCntr.decr(1);
	 if (rxCntr._read == 1 ||
	     !pw_BackToBack) begin // can't back to back
				rg_inPkt <= False;
			        sending   = False;
			     end
      end

      pw_MoveOn.send();
      /*
      if (fAxiTx.notEmpty()) begin
	 let x <- toGet(fAxiTx).get();
	 rg_maybeTop <= tagged Valid x;
      end
      else rg_maybeTop <= tagged Invalid;
      */
      rg_maybeTop <= tagged Invalid;

      if (!ep.tx_st_ready) sending = False;

      rg_sending <= sending;
   endrule

   (* descending_urgency = "moveOn, fillTop", fire_when_enabled *)
   rule moveOn (pw_MoveOn);
      let x <- toGet(fAxiTx).get();
      rg_maybeTop <= tagged Valid x;
   endrule

   /*
   (*preempts = "moveOn1, moveOn2"*)
   rule moveOn2 (pw_MoveOn);
      rg_maybeTop <= tagged Invalid;
   endrule
   */

   (* fire_when_enabled, no_implicit_conditions *)
   rule driveTx;
      ep.tx_st_valid(wSending);
      ep.tx_st_sop(wAxiTxSof);
      ep.tx_st_eop(wAxiTxLast);
      ep.tx_st_empty(wAxiTxEmpty);
      ep.tx_st_data(wAxiTxData);
   endrule

   // receiving:

   Integer highWaterMark = valueof(TotalPktSize) + 8;
   Integer lowWaterMark  = 8;

   rule suspendReception (rg_okToReceive && fAxiRx.isGreaterThan(highWaterMark));
      rg_okToReceive <= False;
   endrule

   rule resumeReception (!rg_okToReceive && fAxiRx.isLessThan(lowWaterMark));
      rg_okToReceive <= True;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_axi_rx_ready;
      ep.rx_st_ready(rg_okToReceive && outOfReset);
   endrule

   (* fire_when_enabled *)
   rule sink_axi_rx if (ep.rx_st_valid && outOfReset);
      let info = AxiRx {
	 sof:     ep.rx_st_sop,
	 last:    ep.rx_st_eop,
	 bar:     truncate(ep.rx_st_bar),
	 data:    ep.rx_st_data
	 };
      fAxiRx.enq(info);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface pcie = ep.pcie;

   interface PCIE_TRN_COMMON_A trn;
      interface clk                      = clk125;
      interface reset_n                  = user_reset_n;
      method    link_up                  = ep.pld_clk_inuse;
      method    pci_id                   = rg_id;
      method    max_rd_req_bytes         = rg_max_read_req_bytes;
      method    max_payload_bytes        = rg_max_payload_bytes;
      method    read_completion_boundary = rg_completion_bdry;
      method    msix_enable              = rg_msix_enable;
      method    msix_mask                = rg_msix_mask;
   endinterface

   interface PCIE_TRN_XMIT_A trn_tx;
      method Action xmit(data);
	 if (rg_inPktEnq || data.sof) begin
	    fAxiTx.enq(AxiTx {sof: data.sof,
			      last: data.eof,
			      empty: ((data.be & 'hFF) == 0),
			      data: dwordSwap128(data.data) });
	    if (data.eof) begin
	       rxCntr.incr(1);
	       rg_inPktEnq <= False;
	    end
	    else if (data.sof) rg_inPktEnq <= True;
	 end
      endmethod
   endinterface

   interface PCIE_TRN_RECV_A trn_rx;
      method ActionValue#(TLPData#(16)) get();
	 let info <- toGet(fAxiRx).get;
	 TLPData#(16) retval = defaultValue;
	 retval.sof  = info.sof;
	 retval.eof  = info.last;
	 retval.hit  = info.bar;
	 retval.be   = (?);
	 retval.data = dwordSwap128(info.data);
	 return retval;
      endmethod
   endinterface
endmodule: mkPCIExpressEndpointA

////////////////////////////////////////////////////////////////////////////////
/// Shims for Altera conventions
////////////////////////////////////////////////////////////////////////////////

// PCIeToBNocBridge supplies the .be field in TLPData, but does not use it.
// So we use it in mkBNoCToEP (to compute the "empty" signal), but do not
// generate it in the reverse direction.

/*
 TLPs from EP always have 3DW headers
 To EP: MEM_READs always 4DW, MEM_WRITES always 4DW except maybe 1-word interrupt writes.
 */

typedef Bit#(8) Byte;
typedef Vector#(4, Byte)  DWord;
typedef Vector#(4, DWord) Volley;

function Bool hasData(TLPPacketFormat f) = unpack(pack(f)[1]);
function Bool isAligned(Bit#(b) bs)      = (bs[0] == 0);
function Bool   is4Wd(Volley v );
   TLPMemoryIO3DWHeader hdr_3dw = unpack(pack(v));
   return unpack(pack(hdr_3dw.format)[0]);
endfunction

function UInt#(2) lengthMod4(Volley v );
   TLPMemoryIO3DWHeader hdr_3dw = unpack(pack(v));
   UInt#(10) ln = unpack(hdr_3dw.length);
   return truncate(ln);
endfunction

function Bool padded(Volley v);
   TLPMemoryIO3DWHeader hdr_3dw = unpack(pack(v));
   TLPMemory4DWHeader   hdr_4dw = unpack(pack(v));
   return ((!hasData(hdr_3dw.format)) ? False :
	   is4Wd(v) ? !isAligned(hdr_4dw.addr) :
	   isAligned(hdr_3dw.addr));
endfunction

// BlueNoC bridge to endpoint:

module mkBNoCToEP#(Get#(TLPData#(16)) in_ifc)(Get#(TLPData#(16)));
   // Adds an "ignored" DWord if necessary.

   FIFO#(TLPData#(16)) outFF <- mkFIFO;
   Reg#(Bool) inPkt   <- mkReg(False);
   Reg#(Maybe#(DWord)) leftOver <- mkRegU; // when inPkt, valid implies padding
   Reg#(Bool) lastVolley <- mkReg(False);

   rule processLastVolley (lastVolley &&& leftOver matches tagged Valid .dw);
      lastVolley <= False;
      //leftOver <= tagged Invalid; // unnecessary
      Volley v = replicate(reverse(dw)); // always a data DWord, so reverse bytes
      outFF.enq(TLPData{sof: False,
			eof: True,
			hit: 0,
			be:  'hF000,
			data: pack(v)});
   endrule

   rule processVolley (!lastVolley);
      let tlp <- in_ifc.get();
      Volley v = unpack(tlp.data);
      if (tlp.sof) begin
	 let padding =  padded(v);
	 let ending  = tlp.eof;
	 let bes     = tlp.be;
	 inPkt    <= ! ending;
	 leftOver <= padding ? tagged Valid v[0] : tagged Invalid;
	 if (!is4Wd(v)) v[0] = reverse(v[0]); // a data DWord
	 if (padding) begin
	    if (ending) begin
	       if (bes == 'hFFFF) begin
		  lastVolley <= True;
		  ending = False;
	       end
	       else bes = ('hF000 | (bes >> 4));
	    end
	    outFF.enq(TLPData{sof: True,
			      eof: ending,
			      hit: 0,
			      be:  bes,
			      data: pack(v)});
	 end
	 else begin // sof, not padding
	    tlp.data = pack(v);
	    outFF.enq(tlp);
	 end
      end
      else if (inPkt) begin // not sof, so all DWords data
	 if (leftOver matches tagged Valid .dw) begin //i.e. if padding
	    leftOver <= tagged Valid v[0];
	    let bes = tlp.be;
	    v = vec(v[1],v[2],v[3], dw);
	    let ending = tlp.eof;
	    inPkt <= ! ending;
	    if (ending) begin
	       if (bes == 'hFFFF) begin
		  lastVolley <= True;
		  ending = False;
	       end
	       else bes = ('hF000 | (bes >> 4));
	    end
	    outFF.enq(TLPData{sof: False,
			      eof: ending,
			      hit: 0,
			      be:  bes,
			      data: pack(map(reverse, v))});
	 end
	 else begin // not sof, not padding
	    tlp.data = pack(map(reverse, v)); // all DWords data
	    outFF.enq(tlp);
	    inPkt <= !tlp.eof;
	 end
      end
      // else not sof, not inPkt, so ignore
   endrule

   return toGet(outFF);
endmodule

// Endpoint to BlueNoC bridge:

module mkEPTOBNoC#(Put#(TLPData#(16)) out_ifc)(Put#(TLPData#(16)));
   // remove padding if necessary
   FIFO#(TLPData#(16)) inFF <- mkFIFO;
   FIFO#(Bool)  oneInLastFF <- mkFIFO;
   Reg#(Bool) inPkt   <- mkReg(False);
   Reg#(Maybe#(Vector#(3, DWord))) leftOver <- mkRegU; // when inPkt, valid implies padding
   Reg#(Bool) starting <- mkReg(False);
   Reg#(Bool) pending  <- mkReg(False);

   rule processPendingVolley (!inPkt &&& pending &&& leftOver matches tagged Valid .v);
      pending <= False;
      Volley vd = vec(unpack(0),v[0],v[1],v[2]);
      vd = map(reverse, vd); // all data DWords
      out_ifc.put(TLPData{sof: False,
			  eof: True,
			  hit: 0,
			  be:  (?),
			  data: pack(vd)});
   endrule

   rule processFirstVolley (!inPkt && !pending);
      let tlp <- toGet(inFF).get();
      Volley v = unpack(tlp.data);
      if (!is4Wd(v)) v[0] = reverse(v[0]); // a data DWord
      let padding = False;
      if (tlp.sof) begin // otherwise ignore
	 if (!tlp.eof) begin //if eof no padding contraction possible
	    inPkt <= True;
	    padding = padded(v);
	    if (padding) begin
	       UInt#(2) lnMod4 = lengthMod4(v);
	       starting <= True;
	       if (is4Wd(v)) begin
		  lnMod4 = lnMod4 + 1; // 4DW + 1 for padding == 1 (mod 4)
		  padding = False; // to get header out on this cycle
		  leftOver <= Invalid;
	       end
	       else begin
		  // 3DW + 1 for padding == 0 (mod 4)
		  leftOver <= tagged Valid (drop(v)); // ignore v[0] (padding)
	       end
	       oneInLastFF.enq(lnMod4 == 1);
	    end
	    else leftOver <= Invalid; // not padding
	 end
	 else inPkt <= False; // eof
	 if (!padding) begin
	    tlp.data = pack(v); // in case data word has been reversed
	    out_ifc.put(tlp);
	 end
      end
      // else not sof, not inPkt, so ignore
   endrule

   rule processVolley (inPkt); // all volleys except the first
      let tlp <- toGet(inFF).get();
      Volley v = unpack(tlp.data);
      let remnant = leftOver;
      if (starting) begin
	 starting <= False;
	 if (isValid(remnant)) tlp.sof = True;
	 // else this volley began with padding
      end
      if (remnant matches tagged Valid .r) begin // there was padding in previous volley
	 leftOver <= tagged Valid take(v);
	 v = vec(v[3],r[0],r[1],r[2]); // if starting, r's are header
	 if (starting) v[0] = reverse(v[0]); // was v[3] in previous line
	 else v = map(reverse, v); // all data if not starting
	 tlp.data = pack(v);
	 if (tlp.eof) begin
	    inPkt <= False;
	    let oneInLast <- toGet(oneInLastFF).get();
	    if (!oneInLast) begin
	       tlp.eof = False;
	       pending <= True;
	    end
	 end
	 out_ifc.put(tlp);
      end
      else begin // invalid remnant: no padding (unless starting)
	 if (starting) begin
	    leftOver <= tagged Valid take(v); // allow for padding
 	    if (tlp.eof) begin
	       inPkt <= False;
	       // "oneInLast" impossible in this situation
	       // (4DW header followed by padding and data),
	       // but must dequeue the FIFO
	       oneInLastFF.deq();
	    end
	    // note: no output on this cycle
	 end
	 else begin // not padding, and not sof so all data
	    inPkt <= !tlp.eof;
	    tlp.data = pack(map(reverse, v));
	    out_ifc.put(tlp);
	 end
      end
   endrule

   return toPut(inFF);
endmodule

////////////////////////////////////////////////////////////////////////////////
/// Connection Instances
////////////////////////////////////////////////////////////////////////////////

// Basic TLPData#(8) connections to PCIE endpoint
instance Connectable#(Get#(TLPData#(16)), PCIE_TRN_XMIT_A);
   module mkConnection#(Get#(TLPData#(16)) g, PCIE_TRN_XMIT_A p)(Empty);
      rule connect;
         let data <- g.get;
         p.xmit(data);
      endrule
   endmodule
endinstance

instance Connectable#(PCIE_TRN_XMIT_A, Get#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_XMIT_A p, Get#(TLPData#(16)) g)(Empty);
      mkConnection(g, p);
   endmodule
endinstance

/*       --  covered by the Get/Put polymorphic instances:

instance Connectable#(Put#(TLPData#(16)), PCIE_TRN_RECV_A);
   module mkConnection#(Put#(TLPData#(16)) p, PCIE_TRN_RECV_A r)(Empty);
      rule connect;
         let data <- r.get;
         p.put(data);
      endrule
   endmodule
endinstance

instance Connectable#(PCIE_TRN_RECV_A, Put#(TLPData#(16)));
   module mkConnection#(PCIE_TRN_RECV_A r, Put#(TLPData#(16)) p)(Empty);
      mkConnection(p, r);
   endmodule
endinstance
*/

/*
(*synthesize*)
(* no_default_clock, no_default_reset *)
module mkPTest#(Clock c, Reset r)(PCIExpressA#(8));
   let ifc <- mkPCIExpressEndpointA(c, r);
   return ifc;
endmodule
*/

endpackage
