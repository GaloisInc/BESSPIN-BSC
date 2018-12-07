package ReadbackCoreSlice;

import Break::*;
import Collector::*;
import DefaultValue::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import ICapFSM::*;
import OInt::*;
import ReadbackDefines::*;
import SFIFO::*;
import Store::*;
import Vector::*;
import Shuffle::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {Config, Capture, Run, Done, BreakWait} ReadBackState deriving(Bounded, Bits, Eq);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef FrameCmd#(CompAddrType) RdBackFrameCmd;

function Bool isConstraint (CompAddrType value);
   Bit#(1) msb = truncateLSB(pack(value));
   return (msb == 1);
//   Bit#(3) left = truncateLSB(pack(value));
//   return (left[2] == 1 && left[1] == 1);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface RxTx#(type a);
   interface Put#(a) rx;
   interface Get#(a) tx;
endinterface

interface ReadBackCoreSlice;
   interface GetS#(Bit#(31)) rdback;
   interface RxTx#(BitConstraints#(4)) bc;
   method Action startReadback;
   (* always_ready, always_enabled *)
   method Action stamp (RdBackCycleStamp value);
   (* always_ready *)
   method Bool isDone ();
   method Action clear;
   method Action cmd (FrameStoreCmd#(RdBackStoreCmd) value);
   method Bool   storeIsReady;
   method Bool   storeNotEmpty;
endinterface

module mkReadbackCoreSlice#(Integer num, Bool is_lcl, Bool self_last, XilinxFamily family, Bool is_dummy) (ReadBackCoreSlice);
   ReadBackCoreSlice _ifc;
   if (family == VIRTEX6) begin
      if (is_dummy)
         _ifc <- readbackCoreSlice6Dummy(fromInteger(num), is_lcl, self_last);
      else
         _ifc <- readbackCoreSlice6(fromInteger(num), is_lcl, self_last);
   end
   else begin // if (family == KINTEX7)
      if (is_dummy)
         _ifc <- readbackCoreSlice7Dummy(fromInteger(num), is_lcl, self_last);
      else
         _ifc <- readbackCoreSlice7(fromInteger(num), is_lcl, self_last);
   end
   return _ifc;
endmodule

(* synthesize *)
module readbackCoreSlice6#(parameter UInt#(8) num, parameter Bool is_lcl, parameter Bool self_last)
   (ReadBackCoreSlice);
   let _ifc <- mkReadbackCoreInnerSlice(num, is_lcl, self_last, VIRTEX6, False);
   return _ifc;
endmodule

(* synthesize *)
module readbackCoreSlice7#(parameter UInt#(8) num, parameter Bool is_lcl, parameter Bool self_last) 
   (ReadBackCoreSlice);
   let _ifc <- mkReadbackCoreInnerSlice(num, is_lcl, self_last, KINTEX7, False);
   return _ifc;
endmodule

(* synthesize *)
module readbackCoreSlice6Dummy#(parameter UInt#(8) num, parameter Bool is_lcl, parameter Bool self_last)
   (ReadBackCoreSlice);
   let _ifc <- mkReadbackCoreInnerSlice(num, is_lcl, self_last, VIRTEX6, True);
   return _ifc;
endmodule

(* synthesize *)
module readbackCoreSlice7Dummy#(parameter UInt#(8) num, parameter Bool is_lcl, parameter Bool self_last) 
   (ReadBackCoreSlice);
   let _ifc <- mkReadbackCoreInnerSlice(num, is_lcl, self_last, KINTEX7, True);
   return _ifc;
endmodule

module mkReadbackCoreInnerSlice#(UInt#(8) num, Bool is_lcl, Bool self_last, XilinxFamily family, Bool is_dummy) (ReadBackCoreSlice);

   Reg#(ConfigNum)                 cfg            <- mkReg(maxBound);   
   Wire#(RdBackCycleStamp)         stamp_wire     <- mkBypassWire;
   Reg#(ReadBackState)             rdback_state   <- mkReg(Done);
   FrameAddrStore#(RdBackStoreCmd) store          <- mkFrameAddrStore(2048);

   Integer frame_size = frameSize(family);

   ICapArgs args;
   args.family = family;
   args.is_dummy = is_dummy;
   // XXX Should this be buried inside ICapFSM?
   if (family == VIRTEX6)
      args.device_id = 32'h04250093;
   else // if (family == KINTEX7)
      args.device_id = 32'h03651093;

   ICapFSM#(CompAddrType)          icap_fsm       <- mkICapFSM(args);
   FIFOF#(TData)                   fifo_icap      <- mkLFIFOF;
   
   MaskGenerator#(CompAddrType)    mask_gen       <- mkMaskGenerator(frame_size);
//   SFIFO#(Bit#(16), Bit#(5))       fifo_mask      <- mkSafeDepthParamSFIFO(8);
   FIFO#(Bit#(16))                 fifo_mask      <- mkSizedFIFO(100);

   Collector#(16, Bit#(1))         collector      <- mkCollector(5);
   
   FIFO#(Bit#(31))                     fifo_rdback <- mkFIFO;
   Reg#(Maybe#(BitConstraints#(4)))    bc_current  <- mkReg(tagged Invalid);
   SFIFO#(BitConstraints#(4), Bit#(4)) fifo_bc     <- mkSafeDepthParamSFIFO(6);
//   FIFO#(BitConstraints#(4))           fifo_bc     <- mkSizedFIFO(100);
   Shuffle#(BitConstraints#(4))        reorder      <- mkShuffleSwitch(self_last);
   
   Reg#(Bool)                          send_dummy_bc   <- mkReg(False);
   
   Reg#(UInt#(3))                      path_count      <- mkReg(0);
   PulseWire                           incr_path_count <- mkPulseWire;
   PulseWire                           decr_path_count <- mkPulseWire;
   
   rule incr_paths (incr_path_count && !decr_path_count);
      path_count <= path_count + 1;
   endrule
   
   rule decr_paths (!incr_path_count && decr_path_count);
      path_count <= path_count - 1;
   endrule
   
   rule do_config (rdback_state == Config);
      icap_fsm.cmd.put(tagged Config cfg);
      rdback_state <= Capture;
   endrule

   (* aggressive_implicit_conditions *)
   (* preempts="do_capture, send_mask_flush" *)
   rule do_capture (rdback_state == Capture);
      RdBackCycleStamp ss = stamp_wire;
      icap_fsm.cmd.put(tagged Capture ss);
      mask_gen.rx.put(tagged Capture ss);
      rdback_state <= (store.notEmpty) ? Run : Done;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule do_readback (rdback_state == Run);
      let cmd <- store.next;
      RdBackFrameCmd r = tagged FAddr 0;
      if (cmd matches tagged Addr .a) 
	 begin
	    r = tagged FAddr a;
	    //	    $display("(%0d) SEND ADDR: %h", $time, r);
	    icap_fsm.cmd.put(r);
	    mask_gen.rx.put(r);
	 end
      if (cmd matches tagged Offset .o)
	 begin
	    r = tagged FOffset extend(o);
	    if (isConstraint(o))
	       begin
		  //		  $display("(%0d) XCONSTRAINT %b", $time, o);
		  BitConstraints#(4) bc = toBitConstraints(o);
		  bc_current <= tagged Valid bc;
	       end
	    else
	       begin
		  //		  $display("(%0d) XNO CONSTRAINT %b", $time, o);
		  if (bc_current matches tagged Valid .bc)
		     begin
			let x = bc;
			x.isLast = store.isLast;
			x.num = unpack(truncate(pack(num)));
			fifo_bc.enq(x);
		     end
		  else 
		     begin
			BitConstraints#(4) bc = unpack(0);
			bc.isSent = True;
			bc.isLast = store.isLast;
			bc.num = unpack(truncate(pack(num)));
			fifo_bc.enq(bc);			
		     end
		  mask_gen.rx.put(r);
		  bc_current <= tagged Invalid;
	       end
	 end
      if (store.isLast) rdback_state <= Done;
//      if (store.isLast && break_tracker.isValid) rdback_state <= Done;
//      if (store.isLast && !break_tracker.isValid) rdback_state <= BreakWait;
   endrule
   
   // (* aggressive_implicit_conditions *)
   // rule finish_wait (rdback_state == BreakWait &&  break_tracker.isValid);
   //    rdback_state <= Done;
   // endrule
   
   (* aggressive_implicit_conditions *)
   rule send_mask_flush (rdback_state != Run);
      mask_gen.rx.put(tagged Flush);
   endrule
   
   // For debug only
   Reg#(Bit#(32)) data_count <- mkReg(0);
   
   (* aggressive_implicit_conditions *)
   rule grab_data;
      let value <- icap_fsm.data.get;
      fifo_icap.enq(value);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule send_preamble (fifo_icap.first matches tagged Preamble .p 
		       &&& collector.isEmpty);
      fifo_icap.deq;
      data_count <= 0;
      if (is_lcl)
	 // Tag header words with 1'b1
	 fifo_rdback.enq({1'b1, p});
   endrule
   
   (* aggressive_implicit_conditions *)
   rule mask_transfer;
      let mask  <- mask_gen.tx.get;
//      $display("(%0d) MASK: %16b", $time, mask);
      fifo_mask.enq(mask);
   endrule

   (* aggressive_implicit_conditions *)
   rule send_rdback( fifo_icap.first matches tagged Data .d);
      fifo_icap.deq;
      let dd = compressRdBackData(d, family);
      let mask  = fifo_mask.first;
      fifo_mask.deq;
      Vector#(16, Maybe#(Bit#(1))) zz;
      for (Integer i = 0; i < 16; i = i + 1)
	 zz[i] = (mask[i] == 1) ? (tagged Valid dd[i]) : tagged Invalid;
      collector.rx.put(zz);
   endrule
 
   (* aggressive_implicit_conditions *)
   rule drain_collector (!send_dummy_bc);
      let value <- collector.tx.get;
      data_count <= data_count + 1;
      let bc = fifo_bc.first;
      bc.value = value;
      fifo_bc.deq;
      reorder.rx_1.put(bc);
   endrule
   
   rule do_send_dummy_bc (send_dummy_bc);
      send_dummy_bc <= False;
      BitConstraints#(4) bc = unpack(0);
      bc.isDummy = True;
      bc.isLast = True;
      reorder.rx_1.put(bc);
      incr_path_count.send;    
   endrule
   
   interface rdback = fifoToGetS(fifo_rdback);

   interface RxTx bc;
      interface Put rx = reorder.rx_0;
      interface Get tx;
	 method ActionValue#(BitConstraints#(4)) get;
	    let bc <- reorder.tx.get;
	    if (bc.isLast) decr_path_count.send;
	    return bc;
	 endmethod
      endinterface
   endinterface
   
   method Action startReadback if (rdback_state == Done && store.isReady && !send_dummy_bc);
      if (store.notEmpty) begin
	 rdback_state <= Config;
	 incr_path_count.send;
      end
      if (!store.notEmpty) begin
	 rdback_state <= Config;
	 send_dummy_bc <= True;
      end
   endmethod
   
   method Action stamp (RdBackCycleStamp value);
      stamp_wire <= value;
   endmethod

   method Bool isDone ();
      return rdback_state == Done;
   endmethod
   
   // clear
   method Action clear if (rdback_state == Done);
      store.clear;
   endmethod
   
   method Action cmd (value) if (rdback_state == Done);
      if (value matches tagged Finish) begin
//	 $display("(%0d) FINISH CMD AT SLICE %d", $time, num);
	 cfg <= cfg + 1;
      end
      if (value matches tagged Insert .i) begin
//	 $display("(%0d) CMD AT SLICE %d ", $time, num, fshow(i));
      end
      store.cmd(value);
   endmethod
   
   method storeIsReady  = store.isReady;
   method storeNotEmpty = store.notEmpty;

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {void         Finish;
		      a            Insert;
                      } FrameStoreCmd#(type a) deriving(Eq, Bits, Bounded);


interface IFrameAddrStore#(numeric type log2depth, type a);
   interface FrameAddrStore#(a) store;
endinterface

interface FrameAddrStore#(type a);
   method Action clear;
   method Action cmd (FrameStoreCmd#(a) value);
   method ActionValue#(a) next;
   method Bool isLast;
   method Bool isReady;
   method Bool notFull;
   method Bool notEmpty;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkFrameAddrStore#(Integer m) (FrameAddrStore#(t))
   provisos(Bits#(t, st));

   let _i = ?;

   if      (m<2)      	  begin IFrameAddrStore#(1, t)  _store1  <- mkIFrameAddrStore(m); _i = _store1.store;  end
   else if (m<4)      	  begin IFrameAddrStore#(2, t)  _store2  <- mkIFrameAddrStore(m); _i = _store2.store;  end
   else if (m<8)      	  begin IFrameAddrStore#(3, t)  _store3  <- mkIFrameAddrStore(m); _i = _store3.store;  end
   else if (m<16)     	  begin IFrameAddrStore#(4, t)  _store4  <- mkIFrameAddrStore(m); _i = _store4.store;  end
   else if (m<32)     	  begin IFrameAddrStore#(5, t)  _store5  <- mkIFrameAddrStore(m); _i = _store5.store;  end
   else if (m<64)     	  begin IFrameAddrStore#(6, t)  _store6  <- mkIFrameAddrStore(m); _i = _store6.store;  end
   else if (m<128)    	  begin IFrameAddrStore#(7, t)  _store7  <- mkIFrameAddrStore(m); _i = _store7.store;  end
   else if (m<256)    	  begin IFrameAddrStore#(8, t)  _store8  <- mkIFrameAddrStore(m); _i = _store8.store;  end
   else if (m<512)    	  begin IFrameAddrStore#(9, t)  _store9  <- mkIFrameAddrStore(m); _i = _store9.store;  end
   else if (m<1024)   	  begin IFrameAddrStore#(10, t) _store10 <- mkIFrameAddrStore(m); _i = _store10.store; end
   else if (m<2048)   	  begin IFrameAddrStore#(11, t) _store11 <- mkIFrameAddrStore(m); _i = _store11.store; end
   else if (m<4096)   	  begin IFrameAddrStore#(12, t) _store12 <- mkIFrameAddrStore(m); _i = _store12.store; end
   else if (m<8192)   	  begin IFrameAddrStore#(13, t) _store13 <- mkIFrameAddrStore(m); _i = _store13.store; end
   else if (m<16384)  	  begin IFrameAddrStore#(14, t) _store14 <- mkIFrameAddrStore(m); _i = _store14.store; end
   else if (m<32768)  	  begin IFrameAddrStore#(15, t) _store15 <- mkIFrameAddrStore(m); _i = _store15.store; end
   else if (m<65536)  	  begin IFrameAddrStore#(16, t) _store16 <- mkIFrameAddrStore(m); _i = _store16.store; end
   else if (m<101072) 	  begin IFrameAddrStore#(17, t) _store17 <- mkIFrameAddrStore(m); _i = _store17.store; end
   else if (m<262144) 	  begin IFrameAddrStore#(18, t) _store18 <- mkIFrameAddrStore(m); _i = _store18.store; end
   else if (m<524288) 	  begin IFrameAddrStore#(19, t) _store19 <- mkIFrameAddrStore(m); _i = _store19.store; end
   else if (m<1048576) 	  begin IFrameAddrStore#(20, t) _store20 <- mkIFrameAddrStore(m); _i = _store20.store; end
   else if (m<2097152) 	  begin IFrameAddrStore#(21, t) _store21 <- mkIFrameAddrStore(m); _i = _store21.store; end
   else if (m<4194304) 	  begin IFrameAddrStore#(22, t) _store22 <- mkIFrameAddrStore(m); _i = _store22.store; end
   else if (m<8388608) 	  begin IFrameAddrStore#(23, t) _store23 <- mkIFrameAddrStore(m); _i = _store23.store; end
   else if (m<16777216)   begin IFrameAddrStore#(24, t) _store24 <- mkIFrameAddrStore(m); _i = _store24.store; end
   else if (m<33554432)   begin IFrameAddrStore#(25, t) _store25 <- mkIFrameAddrStore(m); _i = _store25.store; end
   else if (m<67108864)   begin IFrameAddrStore#(26, t) _store26 <- mkIFrameAddrStore(m); _i = _store26.store; end
   else if (m<134217728)  begin IFrameAddrStore#(27, t) _store27 <- mkIFrameAddrStore(m); _i = _store27.store; end
   else if (m<268435456)  begin IFrameAddrStore#(28, t) _store28 <- mkIFrameAddrStore(m); _i = _store28.store; end
   else if (m<536870912)  begin IFrameAddrStore#(29, t) _store29 <- mkIFrameAddrStore(m); _i = _store29.store; end
   else if (m<1073741824) begin IFrameAddrStore#(30, t) _store30 <- mkIFrameAddrStore(m); _i = _store30.store; end
   else if (m<2147483648) begin IFrameAddrStore#(31, t) _store31 <- mkIFrameAddrStore(m); _i = _store31.store; end
   else if (m<4294967296) begin IFrameAddrStore#(32, t) _store32 <- mkIFrameAddrStore(m); _i = _store32.store; end

   return _i;
endmodule

module mkIFrameAddrStore#(Integer m)(IFrameAddrStore#(l, t))
   provisos(Bits#(t, st), Add#(1, l, d));
   
   RAMStore#(t, Bit#(l))  rstore <- mkRAMStore(False, False, m);
   Cache#(2, Bit#(d), t)  cache  <- mkCache;

   Reg#(Bit#(d))                             rWrPtr              <- mkReg(0);
   Reg#(Bit#(d))                             rRdPtr              <- mkReg(0);
   Reg#(Bit#(d))                             count               <- mkReg(0);
   Reg#(Bool)                                is_ready            <- mkReg(True);
   
   Bool is_last = rRdPtr == (count - 1);
   
   Bool empty = rWrPtr == 0;
   Bool full  = rWrPtr == fromInteger(m);
   
   Bit#(d) rRdNext = modIncr(count, rRdPtr);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   
   (* preempts = "do_prefetch_current, do_prefetch_next" *)
   (* aggressive_implicit_conditions *)
   rule do_prefetch_current (cache.find(rRdPtr) matches tagged Invalid);
      rstore.prefetch(truncate(rRdPtr));
   endrule

   (* aggressive_implicit_conditions *)
   rule do_prefetch_next;
      rstore.prefetch(truncate(rRdNext));
   endrule
   
   (* aggressive_implicit_conditions *)
   rule update_cache (rstore.read() matches tagged Valid {.a , .v});
      cache.add(extend(a), v);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface FrameAddrStore store;
   
      method Action clear if (is_ready);
	 is_ready <= False;
	 rWrPtr <= 0;
	 count  <= 0;
	 cache.clear;
      endmethod

      method Action cmd (FrameStoreCmd#(t) value) if (rRdPtr == 0 && !is_ready);
	 if (value matches tagged Insert .s &&& !full) // just drop if full for now
	    begin
	       is_ready <= False;
	       rstore.write(truncate(rWrPtr), s);
	       rWrPtr <= rWrPtr + 1;
	       count   <= count + 1;
	    end
	 if (value matches tagged Finish)
	    begin
	       is_ready <= True;
	    end
      endmethod
   
      method ActionValue#(t) next if (cache.find(rRdPtr) matches tagged Valid .v);
	 rRdPtr <= rRdNext;
	 return v;
      endmethod

      method Bool isLast   = is_last;
      method Bool isReady  = is_ready;
      method Bool notFull  = !full;
      method Bool notEmpty = !empty;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface MaskGenerator#(type a);
   interface Put#(FrameCmd#(a)) rx;
   interface Get#(Bit#(16))    tx;
endinterface

module mkMaskGenerator#(Integer n)  (MaskGenerator#(a))
   provisos(Bits#(a, sa), Bits#(FrameCmd#(a), sf));
   
   FIFO#(FrameCmd#(a)) fifo_cmd  <- mkFIFO;
   FIFO#(Bit#(16))     fifo_mask <- mkFIFO;
   
   Reg#(Maybe#(Bit#(sa))) word_num     <- mkReg(tagged Invalid);
   Reg#(Bit#(16))         word_current <- mkReg(0);
   Reg#(Bit#(16))         word_temp    <- mkReg(0);
   
   rule handle_flush (fifo_cmd.first matches tagged Flush);
      if (word_num matches tagged Valid .v)
	 begin
	    fifo_mask.enq(word_current);
	    word_current <= 0;
	    if (v == fromInteger(n - 1))
	       word_num <= tagged Invalid;
	    else
	       word_num <= tagged Valid (v + 1);
	 end
      else
	 fifo_cmd.deq;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_capture_early (fifo_cmd.first matches tagged Capture .t &&& 
			      word_num matches tagged Valid .v);
//      $display("(%0d) Sending word: %2d (%16b)", $time, word_num, word_current);
      fifo_mask.enq(word_current);
      word_current <= 0;
      if (v == fromInteger(n - 1))
	 word_num <= tagged Invalid;
      else
	 word_num <= tagged Valid (v + 1);
   endrule

   (* aggressive_implicit_conditions *)
   rule handle_capture (fifo_cmd.first matches tagged Capture .t &&& 
			word_num matches tagged Invalid);
      fifo_cmd.deq;
   endrule
   
   (* aggressive_implicit_conditions *)
   rule handle_addr (fifo_cmd.first matches tagged FAddr .a &&&
		     word_num matches tagged Valid .v);
//      $display("(%0d) Sending word: %2d (%16b)", $time, word_num, word_current);
      fifo_mask.enq(word_current);
      word_current <= 0;
      if (v == fromInteger(n - 1))
	 word_num <= tagged Invalid;
      else
	 word_num <= tagged Valid (v + 1);
   endrule
   
   (* aggressive_implicit_conditions *)
   rule ignore_addr (fifo_cmd.first matches tagged FAddr .a &&& 
		     word_num matches tagged Invalid);
      word_num <= tagged Valid 0;
      fifo_cmd.deq;
   endrule
   
   
   (* aggressive_implicit_conditions *)
   rule handle_offset (fifo_cmd.first matches tagged FOffset .o &&&
		       word_num matches tagged Valid .v);
      Bit#(sa) offset = pack(o);
      let wd = offset >> 4;
      Bit#(4) pos = offset[3:0];
      if (wd > v)
	 begin
//	    $display("(%0d) Sending word: %2d (%16b)", $time, word_num, word_current);
	    fifo_mask.enq(word_current);
	    word_current <= 0;
	    word_num <= (v == fromInteger(n - 1)) ? tagged Invalid : tagged Valid (v + 1);
	 end
      if (wd == v)
	 begin
	    let x = word_current | pack(toOInt(pos));
	    word_current <= x;
	    fifo_cmd.deq;
	 end
   endrule

   interface rx = toPut(fifo_cmd);
   interface tx = toGet(fifo_mask);
   
endmodule

function Bit#(16) compressRdBackData (Bit#(32) data, XilinxFamily family);
   Bit#(16) value = 0;
   if (family == VIRTEX6) 
      value = compressRdBackData_6(data);
   if (family == KINTEX7) 
      value = compressRdBackData_7(data);
   return value;
endfunction

function Bit#(32) uncompressRdBackData (Bit#(16) data, XilinxFamily family);
   Bit#(32) value = 0;
   if (family == VIRTEX6) 
      value = uncompressRdBackData_6(data);
   if (family == KINTEX7) 
      value = uncompressRdBackData_7(data);
   return value;
endfunction

function Bit#(16) compressRdBackData_6 (Bit#(32) data);
   Bit#(16) value = 0;
   value[0]  = data[0];
   value[1]  = data[2];
   value[2]  = data[4];
   value[3]  = data[6];
   value[4]  = data[8];
   value[5]  = data[12];
   value[6]  = data[19];
   value[7]  = data[20];
   value[8]  = data[22];
   value[9]  = data[23];
   value[10] = data[25];
   value[11] = data[26];
   value[12] = data[28];
   value[13] = data[29];
   value[14] = data[31];
   
   return value;
endfunction

function Bit#(32) uncompressRdBackData_6 (Bit#(16) data);
   Bit#(32) value = 0;
   
   value[0]  = data[0];
   value[2]  = data[1];
   value[4]  = data[2];
   value[6]  = data[3];
   value[8]  = data[4];
   value[12] = data[5];
   value[19] = data[6];
   value[20] = data[7];
   value[22] = data[8];
   value[23] = data[9];
   value[25] = data[10];
   value[26] = data[11];
   value[28] = data[12];
   value[29] = data[13];
   value[31] = data[14];
   
   return value;
endfunction


function Bit#(16) compressRdBackData_7 (Bit#(32) data);
   
   Bit#(16) value = 0;
   value[0]  = data[1];
   value[1]  = data[2];
   value[2]  = data[3];
   value[3]  = data[4];
   value[4]  = data[5];
   value[5]  = data[6];
   value[6]  = data[9];
   value[7]  = data[10];
   value[8]  = data[19];
   value[9]  = data[20];
   value[10] = data[22];
   value[11] = data[23];
   value[12] = data[26];
   value[13] = data[27];
   value[14] = data[28];
   value[15] = data[29];
   
   return value;
endfunction

function Bit#(32) uncompressRdBackData_7 (Bit#(16) data);

   Bit#(32) value = 0;
   value[1]  = data[0];
   value[2]  = data[1];
   value[3]  = data[2];
   value[4]  = data[3];
   value[5]  = data[4];
   value[6]  = data[5];
   value[9]  = data[6];
   value[10] = data[7];
   value[19] = data[8];
   value[20] = data[9];
   value[22] = data[10];
   value[23] = data[11];
   value[26] = data[12];
   value[27] = data[13];
   value[28] = data[14];
   value[29] = data[15];
   
   return value;
endfunction

endpackage
