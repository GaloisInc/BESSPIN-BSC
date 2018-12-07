// Copyright (c) 2008-2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiSharedMemory;

// This file is long, and contains sub-sections which could well be separate
// packages, if there was an appropriate place to put them

import GetPut::*;
import ClientServer::*;
import FIFOF::*;
import SpecialFIFOs::*;
import AlignedFIFOs::*;
import Connectable::*;
import ConfigReg::*;
import SceMiInternals::*;
import SceMiDefines::*;
import SceMiCore::*;
import SceMiSerialProbe::*;
import List::*;
import Clocks::*;
import DReg::*;
import Vector::*;
import BRAM::*;
import StmtFSM::*;
import HList::*;
import ModuleContext::*;
import UnitAppendList::*;
import NullCrossingFIFOF::*;
import CommitIfc::*;
import Probe::*;
import DDR3::*;
import XilinxDDR3::*;
import XilinxVirtex7DDR2::*;
import DiniSODIMM::*;
import BUtils::*;

export RAM_Request(..), RAM_Response(..);
export RAM_Server, RAM_Client;
export RAM_ServerCmt, RAM_ClientCmt;
export mkRAM_Server;
export MemClientIfc(..), DutWithSceMiMemIfc(..), MemContextIfc, MemContext(..);
export CompleteMemContexts, ProbeMemModule, CompleteMemContextsIfc;
export mkInitialCompleteContextWithClocks, runWithSharedMemory;
export Mem_addr_t, Mem_data_t, Mem_addr_size, Mem_data_size;
export Cached_RAM_Server(..), mkSharedMem;
export mkBRAMXactor, mkHostMemXactor ;
export initMemContext;
export InternalMemXActor(..);


// ============

// GLOBAL PARAMETERS

// The dimensions of the "central" memory:
typedef 31 Mem_addr_size;
typedef 64 Mem_data_size;

// ============

// COMMON DEFINITIONS AND INTERFACES

// The RAM request type:
// the physical layout is
//   +-+--...---+---...---+
//   |r|  data  | address |
//   +-+--...---+---...---+
// where r is the "read?" tag, or the active-low write-enable

typedef struct {
   Bool read;
   data_t data;
   addr_t address;
} RAM_Request #(type addr_t, type data_t) deriving(Eq, Bits);

instance DefaultValue#(RAM_Request#(a,d))
   provisos (DefaultValue#(a)
             ,DefaultValue#(d));
   defaultValue = RAM_Request { read:True
                               ,data : defaultValue
                               ,address : defaultValue };
endinstance

// The RAM response type:
// the physical layout is
//   +--...---+
//   |  data  |
//   +--...---+

typedef data_t RAM_Response#(type data_t);


// The interface presented by a RAM:
//   the request is "put" and the response "got" by the connected client

typedef ServerCommit#(RAM_Request#(addr_t, data_t), RAM_Response#(data_t))
                                 RAM_ServerCmt #(type addr_t, type data_t);

typedef Server#(RAM_Request#(addr_t, data_t), RAM_Response#(data_t))
                                 RAM_Server #(type addr_t, type data_t);

// The interface presented by a RAM client (controller, uP, etc.):
//   the request is "got" and the response "put" by the connected RAM

typedef ClientCommit#(RAM_Request#(addr_t, data_t), RAM_Response#(data_t))
				 RAM_ClientCmt #(type addr_t, type data_t);

typedef Client#(RAM_Request#(addr_t, data_t), RAM_Response#(data_t))
				 RAM_Client #(type addr_t, type data_t);


interface RawSharedMemory#(type usr_addr_t, type usr_data_t,
		   type ctr_addr_t, type ctr_data_t,
		   numeric type idxSize);
   interface RAM_Server#(usr_addr_t, usr_data_t) toUser;
   interface RAM_ClientCmt#(ctr_addr_t, ctr_data_t) toCentre;
   (*always_ready*)
   method Action flush;
   (*always_ready*)
   method Bool flushing;
   (*always_ready*)
   method Bool allow_cclocks;
endinterface

// Address is given as a word address.
module mkRAM_Server#(parameter Integer memSize)(RAM_Server #(addr_t, data_t))
   provisos (Bits#(addr_t, ats)
             ,Bits#(data_t, dts)
             );

   BRAM_Configure c = defaultValue;
   c.memorySize = memSize;
   BRAM1Port#(addr_t, data_t) memport <- mkBRAM1Server(c);
   let mem = memport.portA;

   interface Put request;
   method Action put(r);
      let br = BRAMRequest {
	    write: !r.read,
	    responseOnWrite: False,
	    address: r.address,
	    datain: r.data };
	 mem.request.put(br);
      endmethod
   endinterface
   interface Get response = mem.response;

endmodule

typedef Bit#(Mem_addr_size) Mem_addr_t;
typedef Bit#(Mem_data_size) Mem_data_t;

interface MemClientIfc;
   interface RAM_ClientCmt#(Mem_addr_t, Mem_data_t) client;
   (*always_ready*)
   method Bool allow;
endinterface

interface DutWithSceMiMemIfc#(type i);
   interface i dutIfc;
   interface MemClientIfc memClient;
endinterface

// CLIENTREPLICATOR

typedef 7 TagSize;
typedef UInt#(TagSize) Tag;


module mkRAMClientReplicator#(List#(RAM_ClientCmt#(a, d)) csints)(RAM_ClientCmt#(a, d))
   provisos (Bits#(a, sa)
             ,Bits#(d, sd)
             );

   Integer size = List::length(csints) ;
   SendCommit#(RAM_Request#(a,d)) sendReq ;
   RecvCommit#(RAM_Response#(d))  recvResp;

   if (size == 0) begin
      RWire#(RAM_Request#(a,d)) reqW  <- mkRWire;
      RWire#(d)                 respW <- mkRWire;
      sendReq <-  mkSendCommit(reqW);
      recvResp <- mkRecvCommit(respW);
      rule error_misdirected_response (respW.wget matches tagged Valid .d);
         $display ("Error: %m received a unexpected response from Memroy!");
      endrule
   end
   else if (size == 1) begin
      let h = List::head(csints);
      sendReq =  h.request;
      recvResp = h.response;
   end
   else begin
      List#(Tag) tags = List::map(fromInteger,List::upto(0,size-1));
      FIFOF#(Tag) tagFF <- mkSizedFIFOF(9);
      FIFOF#(RAM_Request#(a,d)) reqFF <- mkLFIFOF;
      FIFOF#(d) respFF <- mkFIFOF;

      List#(RAM_Client#(a,d)) cs <- List::mapM (mkClientFromClientCommit, csints);

      function Rules mkRuleUp(RAM_Client#(a,d) c, Tag t);
         return
         (rules
             // connect each SendCommit to FIFO(enq)
	     rule requestUp (True);
                let r <- c.request.get();
	        reqFF.enq(r);
	        if (r.read) tagFF.enq(t);
	     endrule
          endrules);
      endfunction

      function Rules mkRuleDown(RAM_Client#(a,d) c, Tag t);
         return
         (rules
             // connect each SendCommit to FIFO(enq)
             (* fire_when_enabled *)
	     rule responseDown(tagFF.first()==t);
	        let x = respFF.first();
	        respFF.deq();
	        tagFF.deq();
                c.response.put(x);
             endrule
          endrules);
      endfunction

      addRules(List::foldr(rJoinDescendingUrgency, emptyRules,
		                          List::zipWith(mkRuleUp, cs, tags)));
      addRules(List::foldr(rJoin, emptyRules,
		                          List::zipWith(mkRuleDown, cs, tags)));

      // Export a ClientCommit Interface
      sendReq <-  mkSendCommit(reqFF);
      recvResp <- mkRecvCommit(respFF);
   end

   interface SendCommit request = sendReq;
   interface RecvCommit response = recvResp;
endmodule

// ============

// THE PLUMBING USING MODULECONTEXT

typedef MemClientIfc MemContextIfc;

typedef struct {
   UAList#(MemContextIfc) ms;
		} MemContext;

// Note: the following could deal with the empty case more optimally:

instance Expose#(MemContext, MemContextIfc, CLOCKCONTEXTSIZE);
   module unburyContextWithClocks#(MemContext mc, ClockContext cc)(MemContextIfc);
      let uclock = cc.clks[0];
      let ureset = cc.rsts[0];

      function cl(ifc) = ifc.client;
      function al(ifc) = ifc.allow;

      let mss = flatten(mc.ms);
      let memoryContext <- mkRAMClientReplicator(List::map(cl, mss),
				       clocked_by uclock, reset_by ureset);
      let all = List::all(al, mss);

      interface client = memoryContext;
      method allow = all;
   endmodule
endinstance


instance Hide#(mc, MemContextIfc)
   provisos (IsModule#(mc, a), Context#(mc, MemContext));
   module [mc] reburyContext#(MemContextIfc i)(Empty);
      MemContext c <- getContext();
      c.ms = tagged Append tuple2(c.ms, tagged One i);
      putContext(c);
   endmodule
endinstance

MemContext initMemContext = MemContext{ms: NoItems};

typedef HList3#(MemContext, ProbeContext, ClockContext) CompleteMemContexts;
typedef ModuleContext#(CompleteMemContexts) ProbeMemModule;
typedef Tuple3#(MemContextIfc, ProbeContextIfc, Empty) CompleteMemContextsIfc;

module mkInitialCompleteContextWithClocks#(Vector#(CLOCKCONTEXTSIZE, Clock) cks,
					   Vector#(CLOCKCONTEXTSIZE, Reset) rs)
   (CompleteMemContexts);

   let initP  <- mkInitialProbeContextWithClocks(cks,rs);
   return hCons(initMemContext, initP);
endmodule

module [ModuleContext#(c1)] runWithSharedMemory#(ModuleContext#(HCons#(MemContext,c1), i) mkM)
                                                                       (DutWithSceMiMemIfc#(i))
   provisos (Context#(ModuleContext#(c1),ClockContext));

   ClockContext cc <- getContext();
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   match {.memc, .i} <- runWithContext(initMemContext, mkM);
   let mifc <- unburyContextWithClocks(memc, cc);

   interface dutIfc = i;
   interface memClient = mifc;
endmodule

// ============

// RAWSHAREDMEMORY

// Types internal to mkRawSharedMemory:

typedef enum {
   Initializing,       // cache tags being invalidated after reset
   Ready,              // ready to accept next request
   Waiting_For_Memory, // waiting for a read from main memory
   Still_Waiting_For_Memory,
   Flushing            // writing all valid lines to main memory
	      } State deriving (Bits, Eq);

// cache tags in cache SRAMs
// data invariant:
//   tag_way0 == Nothing || tag_way0 != tag_way1
// whenever the cache is operational
typedef struct {
   Vector#(2, Maybe#(tag_t)) tags;
   Bool next_evict_way0;
		} Cache_Tag#(type tag_t) deriving(Bits);

typedef struct {
   c_addr_t addr;
   Bit#(1) which;
   Bool reading;
		} Req#(type c_addr_t) deriving (Bits);

interface CPart#(type usr_addr_t, type usr_data_t);
   interface RAM_Server#(usr_addr_t, usr_data_t) mem;
   (*always_ready*)
   method Action flush;
   (*always_ready*)
   method Bool flushing;
endinterface

interface UPart#(type ctr_addr_t, type ctr_data_t);
   interface RAM_ClientCmt#(ctr_addr_t, ctr_data_t) toCtr;
   (*always_ready*)
   method Bool allow_cclocks;
endinterface

// idxs is the index size for the caches;
module mkRawSharedMemory#(ctr_addr_t base, Bool write_through,
			  Clock uclock, Reset ureset,
			  Clock cclock, Reset creset)
   (RawSharedMemory#(usr_addr_t, usr_data_t, ctr_addr_t, ctr_data_t, idxs))

   provisos (Bits#(usr_addr_t, uas), Bits#(usr_data_t, uds),
	     Bits#(ctr_addr_t, cas), Bits#(ctr_data_t, cds), Arith#(ctr_addr_t),

	     Mul#(bytesPerLine, 8, cds),     // cds is size of one cache line
	     Mul#(uds, itemsPerLine, cds),
	     Add#(uds, _5, cds),     // at least one item per line
	     Log#(itemsPerLine, uis),

	     Add#(idxs, tgs, rs), Add#(rs, uis, uas), Add#(rs, _4, cas),
	     Alias#(Bit#(idxs), idx_t), Alias#(Bit#(tgs), tag_t)
	     );

   function Tuple4#(ctr_addr_t, tag_t, idx_t, Bit#(uis)) splitAddr(usr_addr_t a);
      match {.r, .ub} = Tuple2#(Bit#(rs), Bit#(uis))'(split(pack(a)));
      match {.t, .i} = split(r);
      ctr_addr_t ca = unpack(extend(r)) + base;
      return tuple4(ca, t, i, ub);
   endfunction

   // THE STATE ELEMENTS WHICH STRADDLE CLOCK DOMAINS:

   // The FIFOs between the cclock and uclock domains:
   CrossingFIFOF#(RAM_Request#(usr_addr_t, usr_data_t)) cToUFF <-
                        mkCtoUNullCrossingFIFOF(cclock, creset, uclock, ureset);
   // ---
   CrossingFIFOF#(RAM_Response#(usr_data_t)) uToCFF <-
                        mkUtoCNullCrossingFIFOF(uclock, ureset, cclock, creset);


   // For communicating a flush command:
   CrossingFIFOF#(void) flushFF <- mkCtoUNullCrossingFIFOF(cclock, creset, uclock, ureset);

   // THE INTERNAL MODULES, ONE FOR EACH CLOCK DOMAIN:

   module mkCClkPart(CPart#(usr_addr_t, usr_data_t));
      interface RAM_Server mem;
	 interface Put request = toPut(cToUFF.fifo);
	 interface Get response = toGet(uToCFF.fifo);
      endinterface
      method Action flush();
	 flushFF.fifo.enq(?);   // void data
      endmethod
      method flushing = !flushFF.fifo.notFull;
   endmodule

   module mkUClkPart(UPart#(ctr_addr_t, ctr_data_t));
      // The caches:
      BRAM_Configure bram_configure = defaultValue;
      bram_configure.outFIFODepth = 2;
      Vector#(2,
	 Vector#(itemsPerLine, BRAM1Port#(idx_t, usr_data_t))) cachesI <-
             replicateM(replicateM(mkBRAM1Server(bram_configure)));
      function BRAMServer#(idx_t, abyte) fa (BRAM1Port#(idx_t, abyte) x) = x.portA;
      let caches = map(map(fa), cachesI);
      // The cache-tag memory:
      BRAM1Port#(idx_t, Cache_Tag#(tag_t))  tgmp <- mkBRAM1Server(bram_configure);
      let tagmem = tgmp.portA;

      // controller state (see definition of State above)
      Reg#(State) state <- mkReg(Initializing);
      Reg#(Bit#(1)) the_way <- mkRegU;
      Reg#(Maybe#(ctr_addr_t)) held_over <- mkReg(Invalid);
      Reg#(Bool) allow_clock <- mkReg(True);

      // For communication with the centre:
      FIFOF#(RAM_Request#(ctr_addr_t, ctr_data_t)) reqFF <- mkFIFOF1;
      FIFOF#(ctr_data_t) rspFF <- mkFIFOF1;

      // FIFO for the last request the cpu made (e.g., previous cycle):
      FIFOF#(RAM_Request#(usr_addr_t, usr_data_t)) last_cpu_request <- mkFIFOF1; // code assumes 1 deep fifo
      // Separate register to remember whether reading (using
      // last_cpu_request.first for this would introduce unwelcome implicit
      // conditions):
      Reg#(Bool) reading <- mkRegU;

      Bool can_allow_cclocks =
	 // allow if no memory transaction in progress, or it's ready to continue:
	 cToUFF.allowCclock && state == Ready && allow_clock &&
	 !(last_cpu_request.notEmpty && reading);

      Probe#(Bool) allow_cclocks_P <- mkProbe;
      rule doProbe ; allow_cclocks_P <= can_allow_cclocks; endrule

      function ActionValue#(usr_data_t) getFromServer(BRAMServer#(idx_t, usr_data_t) ifc)
	 = ifc.response.get;

      function Action putRequestIndexAction(idx_t i, BRAMServer#(idx_t, usr_data_t) ifc, usr_data_t x);
	 action
	    let req = BRAMRequest {write: True, responseOnWrite: False,
				   address: i, datain: x};
	    ifc.request.put(req);
	 endaction
      endfunction

      function Action putRequestAction(BRAMRequest#(idx_t, abyte) req,
                                       BRAMServer#(idx_t, abyte) ifc
                                       ) = ifc.request.put(req);

      // INITIALIZATION:

      // Which cache location we're clearing (used only during post-reset init
      // and flushing)
      Reg#(idx_t) cache_init_location <- mkReg(0);

      let invalidate_write_data =
      Cache_Tag {tags: replicate(Invalid),
		 next_evict_way0: True };
      let invalidate_req =
      BRAMRequest {write: True, responseOnWrite: False,
		   address: cache_init_location, datain: invalidate_write_data};

      // at reset time, invalidate the contents of the cache
      rule initialize(state == Initializing);
	 tagmem.request.put(invalidate_req);
	 state <= cache_init_location == unpack('1) ? Ready : Initializing;
	 cache_init_location <= cache_init_location + 1;
      endrule

      // FLUSHING
      let flush_req =
      BRAMRequest {write: False, responseOnWrite: False,
		   address: cache_init_location, datain: ?};

      Reg#(Maybe#(tag_t)) way1tag <- mkRegU;

      rule start_flush(can_allow_cclocks && flushFF.fifo.notEmpty);
	 state <= Flushing; // (this makes can_allow_cclocks False)
	 cache_init_location <= 0;
      endrule

      Stmt flusher = (
	 seq
	    while (True)
	       seq
		  action
		     // issue read to tagmem
		     tagmem.request.put(flush_req);
		     mapM_(mapM_(putRequestAction(flush_req)), caches);
		  endaction
		  action
		     // write way0 cacheline if appropriate
		     let resp_tag  <- tagmem.response.get();
		     way1tag <= resp_tag.tags[1];
		     let resp_way0v <- mapM(getFromServer, caches[0]);
	 	     tagmem.request.put(invalidate_req); // clear cache
		     if (resp_tag.tags[0] matches tagged Valid .tg)
			begin
			   let i = cache_init_location;
	 		   let old_ca = unpack(extend({tg,i}))+base;
			   reqFF.enq(RAM_Request {
						  read: False,
						  address: old_ca,
						  data:
						  unpack(pack(resp_way0v)) });
			end
		  endaction
		  action
		     // write way1 cacheline if appropriate
		     let resp_way1v <- mapM(getFromServer, caches[1]);
		     if (way1tag matches tagged Valid .tg)
			begin
			   let i = cache_init_location;
	 		   let old_ca = unpack(extend({tg,i}))+base;
			   reqFF.enq(RAM_Request {
						  read: False,
						  address: old_ca,
						  data:
						  unpack(pack(resp_way1v)) });
			end
		     state <= cache_init_location == unpack('1) ? Ready : Flushing;
		     if (cache_init_location == unpack('1)) flushFF.fifo.deq();
		     cache_init_location <= cache_init_location + 1;
		  endaction
	       endseq
	 endseq
		      );

	 let flushFSM <- mkFSMWithPred(flusher, state==Flushing);
	 rule startFlushFSM;
	    flushFSM.start();
	 endrule


      // NORMAL OPERATION:
      // take request from incoming fifo (cToUFF), start cache lookup and store request in
      // U domain.
      rule process_request (state == Ready);
	 let cpu_request = cToUFF.fifo.first;
	 cToUFF.fifo.deq;

	 match {.ca, .t, .i, .j} = splitAddr(cpu_request.address);
	 let req = BRAMRequest {write: False, responseOnWrite: False,
				address: i, datain: ?};
	 tagmem.request.put(req);
	 mapM_(mapM_(putRequestAction(req)), caches);
	 last_cpu_request.enq(cpu_request); // implicit condition: only room for one
	 reading <= cpu_request.read;
      endrule

      // While waiting for the main memory, request the cache tag again
      // so that we know where to write things back
      rule request_cache_tag(state == Waiting_For_Memory);
	 match {.ca, .t, .i, .j} = splitAddr(last_cpu_request.first().address);
	 let req = BRAMRequest {write: False, responseOnWrite: False,
				address: i, datain: ?};
	 tagmem.request.put(req);

	 // Do any held over read request:
	 if (held_over matches tagged Valid .caddr)
	    begin
	       reqFF.enq(RAM_Request {read: True,
				      address: caddr,
				      data: ? });
	       held_over <= tagged Invalid;
	    end

	 state <= Still_Waiting_For_Memory;
      endrule

      // Must deal with (1) read; (2) whole-line write; (3) part-line write.


      // when a line comes back from the cache, check if it's valid: if it is,
      // send it back to the processor; otherwise, ask the cached memory
      rule complete_read_request ((state == Ready ||
				   (state == Still_Waiting_For_Memory && rspFF.notEmpty)) &&
				  last_cpu_request.first().read);
	 match {.ca, .t, .i, .j} = splitAddr(last_cpu_request.first().address);
	 let resp_tag  <- tagmem.response.get();

	 Maybe#(Vector#(itemsPerLine, usr_data_t)) m_items = Invalid;
	 Vector#(itemsPerLine, usr_data_t) old_items = ? ; // XXX check usage here

	 if (state == Still_Waiting_For_Memory)
	    // Completion of main memory read:  update the cache line
	    begin
	       // Response to a read from main memory: place in relevant cache:
	       Vector#(itemsPerLine, usr_data_t) items = unpack(pack(rspFF.first()));
	       m_items = tagged Valid items;

	       joinActions(zipWith(putRequestIndexAction(i), caches[the_way], items));

	       state <= Ready;
	       rspFF.deq;
	    end
	 else
	    begin // Look at tags if we have a hit set m_items  (used below)
	       let resp_way0v <- mapM(getFromServer, caches[0]);
	       let resp_way1v <- mapM(getFromServer, caches[1]);
	       old_items = (resp_tag.next_evict_way0 ? resp_way0v : resp_way1v);

	       function same_as_orig(tag) = (tag == t);

	       if (resp_tag.tags[0] matches tagged Valid .tag &&& same_as_orig(tag))
		  m_items = tagged Valid resp_way0v;
	       else
	       if (resp_tag.tags[1] matches tagged Valid .tag &&& same_as_orig(tag))
		  m_items = tagged Valid resp_way1v;
	    end

	 if (m_items matches tagged Valid .items)
	    begin
               // We have the data,  move it to the use via uToCFF
               // delay C-clock 1 more cycle to allow C domain to see data
	       last_cpu_request.deq();
               allow_clock <= False;
	       uToCFF.fifo.enq(items[j]);
	    end
         else // no hits, talk to main memory then wait
	    begin
	       let chosen_way = resp_tag.next_evict_way0 ? 0 : 1;
	       if (!write_through &&& resp_tag.tags[chosen_way] matches tagged Valid .tg)
		  begin
		     // Write old contents:
		     let old_ca = unpack(extend({tg,i}))+base;
		     reqFF.enq(RAM_Request {read: False,
					    address: old_ca,
					    data: unpack(pack(old_items)) });
		     held_over <= tagged Valid ca;
		  end
	       else reqFF.enq(RAM_Request {read: True,
					   address: ca,
					   data: ?});
	       // Update tag memory:
	       resp_tag.next_evict_way0 = !resp_tag.next_evict_way0;
	       resp_tag.tags[chosen_way] = tagged Valid t;
	       tagmem.request.put(BRAMRequest {write: True, responseOnWrite: False,
					       address: i, datain: resp_tag });
	       state <= Waiting_For_Memory;
	       the_way <= chosen_way;

	       // $display("INFO (cache controller) nothing matches tag %h",
	       // orig_tag);
	    end
      endrule

      // delay the C-clock to allow the uToC fifo time for the user to see data.
      rule allowAgain (! allow_clock);
         allow_clock <= True;
      endrule

      if (valueof(itemsPerLine)!=1) begin
        rule complete_part_write_request ((state == Ready ||
					 (state == Still_Waiting_For_Memory && rspFF.notEmpty)) &&
					!last_cpu_request.first().read);
	 match {.ca, .t, .i, .j} = splitAddr(last_cpu_request.first().address);
	 let resp_tag  <- tagmem.response.get();
	 let chosen_way = resp_tag.next_evict_way0 ? 0 : 1;
	 let evicting = True;

	 Maybe#(Vector#(itemsPerLine, usr_data_t)) m_items = Invalid;
	 Vector#(itemsPerLine, usr_data_t) old_items = ?; // XXX Check usage in all paths

	 if (state == Still_Waiting_For_Memory)
	    // Completion of main memory read:
	    begin
	       Vector#(itemsPerLine, usr_data_t) items = unpack(pack(rspFF.first()));
	       m_items = tagged Valid items;
	       chosen_way = the_way;
	       state <= Ready;
	       rspFF.deq;
	    end
	 else
	    begin
	       let resp_way0v <- mapM(getFromServer, caches[0]);
	       let resp_way1v <- mapM(getFromServer, caches[1]);
	       old_items = (resp_tag.next_evict_way0 ? resp_way0v : resp_way1v);

	       function same_as_orig(tag) = (tag == t);

	       if (resp_tag.tags[0] matches tagged Valid .tag &&& same_as_orig(tag))
		  begin
		     m_items = tagged Valid resp_way0v;
		     chosen_way = 0;
		  end
	       else
	       if (resp_tag.tags[1] matches tagged Valid .tag &&& same_as_orig(tag))
		  begin
		     m_items = tagged Valid resp_way1v;
		     chosen_way = 1;
		  end
	    end

	 if (m_items matches tagged Valid .items0)
	    begin
	       let items = items0;
	       items[j] = last_cpu_request.first().data;
	       last_cpu_request.deq();

	       if (write_through)
		  // Write to main memory:
		  reqFF.enq(RAM_Request {read: False,
					 address: ca,
					 data: unpack(pack(items)) });
	       // Update cache:
	       joinActions(zipWith(putRequestIndexAction(i), caches[chosen_way], items));
	    end
         else // no hits, talk to cached memory
	    begin
	       if (!write_through &&& resp_tag.tags[chosen_way] matches tagged Valid .tg)
		  begin
		     // Write old contents:
		     let old_ca = unpack(extend({tg,i}))+base;
		     reqFF.enq(RAM_Request {read: False,
					    address: old_ca,
					    data: unpack(pack(old_items)) });
		     held_over <= tagged Valid ca;
		  end
	       else reqFF.enq(RAM_Request {read: True,
					   address: ca,
					   data: ?});
	       // Update tag memory:
	       resp_tag.next_evict_way0 = !resp_tag.next_evict_way0;
	       resp_tag.tags[chosen_way] = tagged Valid t;
	       tagmem.request.put(BRAMRequest {write: True, responseOnWrite: False,
					       address: i, datain: resp_tag });
	       state <= Waiting_For_Memory;
	       the_way <= chosen_way;

	       // $display("INFO (cache controller) nothing matches tag %h",
	       // orig_tag);

	    end
        endrule
      end // valueOf(lines)...

      rule complete_whole_write_request (valueof(itemsPerLine)==1 && state == Ready &&
					 !last_cpu_request.first().read);
	 match {.ca, .t, .i, .j} = splitAddr(last_cpu_request.first().address);
	 Vector#(itemsPerLine, usr_data_t) items =
	       unpack(extend(pack(last_cpu_request.first().data)));
	 let resp_tag  <- tagmem.response.get();
	 let chosen_way = resp_tag.next_evict_way0 ? 0 : 1;
	 let orig_chosen_way = chosen_way;
	 let evicting = True;

	 // Read data previously in cache:
	 let resp_way0v <- mapM(getFromServer, caches[0]);
	 let resp_way1v <- mapM(getFromServer, caches[1]);

	 Vector#(itemsPerLine, usr_data_t) old_items =
	    (resp_tag.next_evict_way0 ? resp_way0v : resp_way1v);

	 function same_as_orig(tag) = (tag == t);

	 if (resp_tag.tags[0] matches tagged Valid .tag &&& same_as_orig(tag))
	    begin
	       chosen_way = 0;
	       evicting = False;
	    end
	 else
	 if (resp_tag.tags[1] matches tagged Valid .tag &&& same_as_orig(tag))
	    begin
	       chosen_way = 1;
	       evicting = False;
	    end
	 // Write to main memory:
	 if (write_through)
	    begin
	       // Write new contents:
	       reqFF.enq(RAM_Request {
				      read: False,
				      address: ca,
				      data: unpack(pack(items)) });
	    end
	 else if (evicting &&& resp_tag.tags[orig_chosen_way] matches tagged Valid .tg)
	    begin
	       // Write old contents:
	       let old_ca = unpack(extend({tg,i}))+base;
	       reqFF.enq(RAM_Request {
				      read: False,
				      address: old_ca,
				      data: unpack(pack(old_items)) });
	    end
	 // Update cache:
	 joinActions(zipWith(putRequestIndexAction(i), caches[chosen_way], items));

	 // Update tag memory:
	 if (evicting)
	    begin
	       resp_tag.next_evict_way0 = !resp_tag.next_evict_way0;
	       resp_tag.tags[chosen_way] = tagged Valid t;
	       tagmem.request.put(BRAMRequest {write: True, responseOnWrite: False,
					       address: i, datain: resp_tag });
	    end
	 last_cpu_request.deq();
      endrule

      let sendReq <- mkSendCommit(reqFF);
      let recvResp <- mkRecvCommit(rspFF);
      interface RAM_ClientCmt toCtr;
	 interface SendCommit request = sendReq;
	 interface RecvCommit response = recvResp;
      endinterface

      method allow_cclocks = can_allow_cclocks;
   endmodule


   let usr      <- mkCClkPart(clocked_by cclock, reset_by creset);
   let uCPart   <- mkUClkPart(clocked_by uclock, reset_by ureset);

   interface toUser     = usr.mem;
   interface toCentre   = uCPart.toCtr;
   method flush         = usr.flush;
   method flushing      = usr.flushing;
   method allow_cclocks = uCPart.allow_cclocks;
endmodule

// ============

// SHAREDMEMORY

// This module instantiates a local cached memory model.

interface Cached_RAM_Server#(type a, type d, numeric type n);
   interface RAM_Server#(a,d) server;
   method Action flush();
   method Bool flushing();
endinterface

module [mc] mkSharedMem#(Mem_addr_t base, Bool write_through
                         )(
                           Cached_RAM_Server#(addr_t, data_t, idxSize))
   provisos (IsModule#(mc, _a),
	     Context#(mc, MemContext), Context#(mc, ClockContext),
	     Bits#(addr_t, asz), Bits#(data_t, dsz),

	     Mul#(dsz, a__, Mem_data_size),
	     Add#(dsz, b__, Mem_data_size),
	     Log#(a__, c__),
	     Mul#(d__, 8, dsz),
	     Add#(e__, c__, asz),
	     Add#(e__, f__, Mem_addr_size),
	     Add#(idxSize, g__, e__)
	     );

   let cclock <- exposeCurrentClock;
   let creset <- exposeCurrentReset;
   ClockContext cc <- getContext();
   let uclock = cc.clks[0];
   let ureset = cc.rsts[0];

   RawSharedMemory#(addr_t, data_t, Mem_addr_t, Mem_data_t, idxSize) memleaf <-
       mkRawSharedMemory(base, write_through,
			 uclock, ureset,
			 cclock, creset);

   MemContext mcx <- getContext();
   let item = (interface MemContextIfc;
		  interface client = memleaf.toCentre;
		  method allow = memleaf.allow_cclocks;
	       endinterface );
   mcx = MemContext { ms: tagged Append tuple2(tagged One item, mcx.ms) };
   putContext(mcx);

   interface Server server = memleaf.toUser;
   method Action flush = memleaf.flush;
   method Bool flushing = memleaf.flushing;
endmodule

// ========

// THE SCEMILAYER PROVISIONS

module [SceMiModule] mkUclockServer(Server#(rq, rsp))
   provisos (Bits#(rq, srq), Bits#(rsp, srsp));

   SceMiMessageOutPortIfc#(rq)  req <- mkSceMiMessageOutPort();
   SceMiMessageInPortIfc#(rsp) resp <- mkSceMiMessageInPort();

   rule poll;
      resp.request();
   endrule

   interface Put request = toPut(req);
   interface Get response = toGet(resp);
endmodule

module [SceMiModule] mkUclockClient(Client#(rq, rsp))
   provisos (Bits#(rq, srq), Bits#(rsp, srsp));

   SceMiMessageOutPortIfc#(rsp) resp <- mkSceMiMessageOutPort();
   SceMiMessageInPortIfc #(rq)  req  <- mkSceMiMessageInPort();

   rule poll;
      req.request();
   endrule

   interface Get request  = toGet(req);
   interface Put response = toPut(resp);
endmodule

typedef enum { USER, HOST } Requester deriving(Bits, Eq);

// BRAM backed memory store with a SceMi Client Xactor
module [SceMiModule] mkBRAMXactor#(  parameter Integer memSize
				   , parameter SceMiClockConfiguration conf
				   , MemClientIfc memuser)
                                  (Empty);
   RAM_Server#(Mem_addr_t, Mem_data_t) mem <- mkRAM_Server(memSize);

   RAM_Client#(Mem_addr_t, Mem_data_t) host_client <- mkUclockClient;
   RAM_Client#(Mem_addr_t, Mem_data_t) user_client <- mkClientFromClientCommit(memuser.client);

   FIFOF#(Requester) requester <- mkSizedFIFOF(8);

   rule userCommand (True);
      let c <- user_client.request.get;
      mem.request.put(c);
      if (c.read)
	 requester.enq(USER);
   endrule


   (* descending_urgency = "hostCommand, userCommand" *)
   rule hostCommand (True);
      let c <- host_client.request.get();
      mem.request.put(c);
      if (c.read)
	 requester.enq(HOST);
   endrule

   rule userResponse (requester.first() == USER);
      requester.deq();
      let x <- mem.response.get();
      user_client.response.put(x);
   endrule

   rule hostResponse (requester.first() == HOST);
      requester.deq();
      let x <- mem.response.get();
      host_client.response.put(x);
   endrule

   // stall the clock if the DUT requests it (to control perceived memory latency)
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl(conf.clockNum,
							 memuser.allow(),
							 memuser.allow());
endmodule

module [SceMiModule] mkHostMemXactor#( parameter SceMiClockConfiguration conf
				     , MemClientIfc memuser)
                                     ();
   // instantiate the SCE-MI transactor for communicating with memory model on the host
   RAM_Server#(Mem_addr_t, Mem_data_t) server <- mkUclockServer;

   // connect the transactor to the memory client in the DUT
   mkConnection(server.request,memuser.client.request);
   mkConnection(server.response,memuser.client.response);

   // stall the clock if the DUT requests it (to control perceived memory latency)
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl(conf.clockNum,
                                                         memuser.allow(),
                                                         memuser.allow());
endmodule


typeclass InternalMemXActor #(type ifc);
   module [SceMiModule] mkHwMemXactor#(parameter SceMiClockConfiguration conf
				   , MemClientIfc memuser)
                                   (ifc);
endtypeclass


instance InternalMemXActor#(RAM_Client#(Mem_addr_t, Mem_data_t));
// Adds a SceMi Client XActor in to the MemClient,  providing a RAM_client
module [SceMiModule] mkHwMemXactor#(parameter SceMiClockConfiguration conf
				   , MemClientIfc memuser)
                                   (RAM_Client#(Mem_addr_t, Mem_data_t));

   FIFOF#(RAM_Request#(Mem_addr_t, Mem_data_t)) cmds <- mkFIFOF();
   FIFOF#(RAM_Response#(Mem_data_t))           rdata <- mkFIFOF();

   RAM_Client#(Mem_addr_t, Mem_data_t) host_client <- mkUclockClient;
   RAM_Client#(Mem_addr_t, Mem_data_t) user_client <- mkClientFromClientCommit(memuser.client);

   FIFOF#(Requester) requester <- mkSizedFIFOF(4);

   rule userCommand;
      let c <- user_client.request.get;
      cmds.enq(c);
      if (c.read)
	 requester.enq(USER);
   endrule

   (* descending_urgency = "hostCommand, userCommand" *)
   rule hostCommand;
      let c <- host_client.request.get();
      cmds.enq(c);
      if (c.read)
	 requester.enq(HOST);
   endrule

   rule userResponse (requester.first() == USER);
      requester.deq();
      user_client.response.put(rdata.first);
      rdata.deq();
   endrule

   rule hostResponse (requester.first() == HOST);
      requester.deq();
      host_client.response.put(rdata.first());
      rdata.deq();
   endrule

   // stall the clock if the DUT requests it (to control perceived memory latency)
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl(conf.clockNum,
							 memuser.allow(),
							 memuser.allow());

   interface Get request = toGet(cmds);
   interface Put response = toPut(rdata);
endmodule
endinstance

instance InternalMemXActor#(RAM_ClientCmt#(Mem_addr_t, Mem_data_t));
// Adds a SceMi Client XActor in to the MemClient,  providing a RAM_ClientCmt
module [SceMiModule] mkHwMemXactor#(parameter SceMiClockConfiguration conf
				   , MemClientIfc memuser)
                                   (RAM_ClientCmt#(Mem_addr_t, Mem_data_t));

   FIFOF#(RAM_Request#(Mem_addr_t, Mem_data_t)) cmds <- mkFIFOF();
   FIFOF#(RAM_Response#(Mem_data_t))           rdata <- mkFIFOF();

   RAM_Client#(Mem_addr_t, Mem_data_t) host_client <- mkUclockClient;
   RAM_Client#(Mem_addr_t, Mem_data_t) user_client <- mkClientFromClientCommit(memuser.client);

   FIFOF#(Requester) requester <- mkSizedFIFOF(4);

   rule userCommand;
      let c <- user_client.request.get;
      cmds.enq(c);
      if (c.read)
	 requester.enq(USER);
   endrule

   (* descending_urgency = "hostCommand, userCommand" *)
   rule hostCommand;
      let c <- host_client.request.get();
      cmds.enq(c);
      if (c.read)
	 requester.enq(HOST);
   endrule

   rule userResponse (requester.first() == USER);
      requester.deq();
      user_client.response.put(rdata.first);
      rdata.deq();
   endrule

   rule hostResponse (requester.first() == HOST);
      requester.deq();
      host_client.response.put(rdata.first());
      rdata.deq();
   endrule

   // stall the clock if the DUT requests it (to control perceived memory latency)
   SceMiClockControlIfc dutclkctrl<- mkSceMiClockControl(conf.clockNum,
							 memuser.allow(),
							 memuser.allow());
   let sendReq <- mkSendCommit(cmds);
   let recvResp <- mkRecvCommit(rdata);
   interface SendCommit request = sendReq;
   interface RecvCommit response = recvResp;
endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_ML605)
   provisos (Alias#(mem_addr_t, Bit#(asz)), Alias#(mem_data_t, Bit#(dsz)),
	     Add#(27, _1, asz), Add#(dsz, _2, 512),
	     Add#(27, _3, TAdd#(3, asz)), Mul#(dsz, _4, 512));

   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_ML605 ddr3
			) (Empty);
      let sz = valueof(dsz);

      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
	 let request = reqFIFO.first;
	 reqFIFO.deq;
	 Bit#(TAdd#(3, asz)) r = zeroExtend(request.address);
	 Bit#(64) enables = 0;
	 case (sz)
	    64: begin
		   case(request.address[2:0])
		      0: enables = 64'h00_00_00_00_00_00_00_FF;
		      1: enables = 64'h00_00_00_00_00_00_FF_00;
		      2: enables = 64'h00_00_00_00_00_FF_00_00;
		      3: enables = 64'h00_00_00_00_FF_00_00_00;
		      4: enables = 64'h00_00_00_FF_00_00_00_00;
		      5: enables = 64'h00_00_FF_00_00_00_00_00;
		      6: enables = 64'h00_FF_00_00_00_00_00_00;
		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64,128,256 or 512");
	 endcase
	 Bit#(27) address = truncate(r);
         ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_KC705)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(28, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(28, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_KC705 ddr3
			) (Empty);
   
      let sz = valueof(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3,asz)) r = zeroExtend(request.address);
      	 Bit#(64) enables = 0;
	 case(sz)
	    64: begin
		   case(request.address[2:0])
      		      0: enables = 64'h00_00_00_00_00_00_00_FF;
      		      1: enables = 64'h00_00_00_00_00_00_FF_00;
      		      2: enables = 64'h00_00_00_00_00_FF_00_00;
      		      3: enables = 64'h00_00_00_00_FF_00_00_00;
      		      4: enables = 64'h00_00_00_FF_00_00_00_00;
      		      5: enables = 64'h00_00_FF_00_00_00_00_00;
      		      6: enables = 64'h00_FF_00_00_00_00_00_00;
      		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(28) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_10GK7LL)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(30, _1, asz), 
	     Add#(dsz, _2, 576),
	     Add#(30, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_10GK7LL ddr3
			) (Empty);
   
      let sz = valueof(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3,asz)) r = zeroExtend(request.address);
      	 Bit#(72) enables = 0;
	 Bit#(576) data = 0;
	 case(sz)
	    64: begin
		   Bit#(72) d = cExtend(request.data);
		   data = duplicate(d);
		   case(request.address[2:0])
		      0: enables = 72'h00_00_00_00_00_00_00_01_FF;
		      1: enables = 72'h00_00_00_00_00_00_03_FE_00;
		      2: enables = 72'h00_00_00_00_00_07_FC_00_00;
		      3: enables = 72'h00_00_00_00_0F_F8_00_00_00;
		      4: enables = 72'h00_00_00_1F_F0_00_00_00_00;
		      5: enables = 72'h00_00_3F_E0_00_00_00_00_00;
		      6: enables = 72'h00_7F_C0_00_00_00_00_00_00;
		      7: enables = 72'hFF_80_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    Bit#(144) d = cExtend(request.data);
		    data = duplicate(d);
		    case(request.address[1:0])
		       0: enables = 72'h00_00_00_00_00_00_03_FF_FF;
		       1: enables = 72'h00_00_00_00_0F_FF_FC_00_00;
		       2: enables = 72'h00_00_3F_FF_F0_00_00_00_00;
		       3: enables = 72'hFF_FF_C0_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    Bit#(288) d = cExtend(request.data);
		    data = duplicate(d);
		    case(request.address[0])
		       0: enables = 72'h00_00_00_00_0F_FF_FF_FF_FF;
		       1: enables = 72'hFF_FF_FF_FF_F0_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    data = cExtend(request.data);
		    enables = 72'hFF_FF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(30) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), data);
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_VC707)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(29, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(29, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_VC707 ddr3
			) (Empty);
   
      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3, asz)) r = zeroExtend(request.address);
      	 Bit#(64) enables = 0;
	 case(sz)
	    64: begin
		   case(request.address[2:0])
      		      0: enables = 64'h00_00_00_00_00_00_00_FF;
      		      1: enables = 64'h00_00_00_00_00_00_FF_00;
      		      2: enables = 64'h00_00_00_00_00_FF_00_00;
      		      3: enables = 64'h00_00_00_00_FF_00_00_00;
      		      4: enables = 64'h00_00_00_FF_00_00_00_00;
      		      5: enables = 64'h00_00_FF_00_00_00_00_00;
      		      6: enables = 64'h00_FF_00_00_00_00_00_00;
      		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(29) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_VC709)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(30, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(30, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_VC709 ddr3
			) (Empty);
   
      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3, asz)) r = zeroExtend(request.address);
      	 Bit#(64) enables = 0;
	 case(sz)
	    64: begin
		   case(request.address[2:0])
      		      0: enables = 64'h00_00_00_00_00_00_00_FF;
      		      1: enables = 64'h00_00_00_00_00_00_FF_00;
      		      2: enables = 64'h00_00_00_00_00_FF_00_00;
      		      3: enables = 64'h00_00_00_00_FF_00_00_00;
      		      4: enables = 64'h00_00_00_FF_00_00_00_00;
      		      5: enables = 64'h00_00_FF_00_00_00_00_00;
      		      6: enables = 64'h00_FF_00_00_00_00_00_00;
      		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(30) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_DNV7F2A)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(29, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(29, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_DNV7F2A ddr3
			) (Empty);
   
      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3, asz)) r = zeroExtend(request.address);
      	 Bit#(64) enables = 0;
	 case(sz)
	    64: begin
		   case(request.address[2:0])
      		      0: enables = 64'h00_00_00_00_00_00_00_FF;
      		      1: enables = 64'h00_00_00_00_00_00_FF_00;
      		      2: enables = 64'h00_00_00_00_00_FF_00_00;
      		      3: enables = 64'h00_00_00_00_FF_00_00_00;
      		      4: enables = 64'h00_00_00_FF_00_00_00_00;
      		      5: enables = 64'h00_00_FF_00_00_00_00_00;
      		      6: enables = 64'h00_FF_00_00_00_00_00_00;
      		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(29) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR2_User)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(30, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(30, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
                        ,DDR2_User ddr2
                        ) (Empty);

      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr2Clk = ddr2.clock;
      Reset ddr2Rst = ddr2.reset_n;

      Reset uRst_dclk   <- mkAsyncReset (2, uRst, ddr2Clk);
      Reset jointReset1 <- mkResetEither( ddr2Rst, uRst_dclk, clocked_by ddr2Clk);
      Reset jointReset  <- mkAsyncReset (2, jointReset1, ddr2Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr2Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);
   
      FIFOF#(Tuple3#(Bit#(32), Bit#(30), Bit#(256))) oddReq <- mkFIFOF1(clocked_by ddr2Clk, reset_by ddr2Rst);
      
      rule connectRequestsDDR2 if (oddReq.notFull);
         let request = reqFIFO.first;
         reqFIFO.deq;
	 Bit#(TAdd#(3,asz)) r = zeroExtend(request.address);
	 Bit#(30) address = truncate(r);
	 
	 case(sz)
	    64: begin
		   ddr2.put(((request.read) ? 0 : 32'hFF), address, cExtend(request.data));
		end
	    128: begin
		    ddr2.put(((request.read) ? 0 : 32'hFFFF), address, cExtend(request.data));
		 end
	    256: begin
		    ddr2.put(((request.read) ? 0 : 32'hFFFFFFFF), address, cExtend(request.data));
		 end
	    512: begin
		    ddr2.put(((request.read) ? 0 : 32'hFFFFFFFF), address, cExtend(request.data));
		    oddReq.enq(tuple3(((request.read) ? 0 : 32'hFFFFFFFF), address + 30'h20, cExtend(request.data >> 256)));
		 end
	    default: error("DDR2 connection: data width must be 64, 128, 256, or 512");
	 endcase
      endrule
      
      rule connectOddRequestsDDR2 if (oddReq.notEmpty);
	 match { .enables, .address, .data } <- toGet(oddReq).get;
	 ddr2.put(enables, address, data);
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(8, ddr2Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);
      FIFOF#(Bit#(256)) oddResp <- mkFIFOF1(clocked_by ddr2Clk, reset_by ddr2Rst);
      
      if (sz == 512) begin
	 rule connectFirstResponseDDR2 if (oddResp.notFull);
	    let response <- ddr2.read;
	    oddResp.enq(response);
	 endrule
	 
	 rule connectSecondResponseDDR2 if (oddResp.notEmpty);
	    let response <- ddr2.read;
	    oddResp.deq;
	    respFIFO.enq(cExtend({ response, oddResp.first }));
	 endrule
      end
      else begin
	 rule connectResponseDDR2;
            let response <- ddr2.read;
            respFIFO.enq(cExtend(response));
	 endrule
      end
      
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR2_User_V7)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(28, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(28, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
                        ,DDR2_User_V7 ddr2
                        ) (Empty);

      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr2Clk = ddr2.clock;
      Reset ddr2Rst = ddr2.reset_n;

      Reset uRst_dclk   <- mkAsyncReset (2, uRst, ddr2Clk);
      Reset jointReset1 <- mkResetEither( ddr2Rst, uRst_dclk, clocked_by ddr2Clk);
      Reset jointReset  <- mkAsyncReset (2, jointReset1, ddr2Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr2Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);
      
      rule connect_requests;
         let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(28) address = 0;
	 Bit#(64) enables = 0;
	 Bit#(512) datain = duplicate(request.data);

	 case(sz)
	    64:  begin
		    address = cExtend(request.address);
		    enables = 64'hFF;
		 end
	    128: begin
		    address = cExtend(request.address) << 1;
		    enables = 64'hFFFF;
		 end
	    256: begin
		    address = cExtend(request.address) << 2;
		    enables = 64'hFFFFFFFF;
		 end
	    512: begin
		    address = cExtend(request.address) << 3;
		    enables = 64'hFFFFFFFFFFFFFFFF;
		 end
	    default: error("DDR2 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 ddr2.put((request.read) ? 0 : enables, address, datain);
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(1, ddr2Clk, jointReset, uClk);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClk, reset_by uRst);

      rule connect_response;
         let response <- ddr2.read;
         respFIFO.enq(cExtend(response));
      endrule      
   endmodule
endinstance

instance Connectable#(RAM_ClientCmt#(mem_addr_t, mem_data_t), DDR3_User_B2000T)
   provisos (Alias#(mem_addr_t, Bit#(asz)), 
	     Alias#(mem_data_t, Bit#(dsz)),
	     Add#(29, _1, asz), 
	     Add#(dsz, _2, 512),
	     Add#(29, _3, TAdd#(3, asz)), 
	     Mul#(dsz, _4, 512)
	     );
   module mkConnection#(RAM_ClientCmt#(mem_addr_t, mem_data_t) client
			,DDR3_User_B2000T ddr3
			) (Empty);
   
      let sz = valueOf(dsz);
      
      Clock uClk = clockOf(client);
      Reset uRst = resetOf(client);

      Clock ddr3Clk = ddr3.clock;
      Reset ddr3Rst = ddr3.reset_n;

      Reset uRst_dclk    <- mkAsyncReset (2, uRst, ddr3Clk);
      Reset jointReset1  <- mkResetEither( ddr3Rst, uRst_dclk, clocked_by ddr3Clk);
      Reset jointReset   <- mkAsyncReset( 2, jointReset1, ddr3Clk);

      SyncFIFOIfc#(RAM_Request#(mem_addr_t, mem_data_t))  reqFIFO <- mkSyncFIFO(4, uClk, uRst, ddr3Clk);
      let connectReqUser <- mkConnection(client.request,reqFIFO, clocked_by uClk, reset_by uRst);

      rule connectRequestsDDR3;
      	 let request = reqFIFO.first;
      	 reqFIFO.deq;
      	 Bit#(TAdd#(3, asz)) r = zeroExtend(request.address);
      	 Bit#(64) enables = 0;
	 case(sz)
	    64: begin
		   case(request.address[2:0])
      		      0: enables = 64'h00_00_00_00_00_00_00_FF;
      		      1: enables = 64'h00_00_00_00_00_00_FF_00;
      		      2: enables = 64'h00_00_00_00_00_FF_00_00;
      		      3: enables = 64'h00_00_00_00_FF_00_00_00;
      		      4: enables = 64'h00_00_00_FF_00_00_00_00;
      		      5: enables = 64'h00_00_FF_00_00_00_00_00;
      		      6: enables = 64'h00_FF_00_00_00_00_00_00;
      		      7: enables = 64'hFF_00_00_00_00_00_00_00;
		   endcase
		end
	    128: begin
		    r = r << 1;
		    case(request.address[1:0])
		       0: enables = 64'h00_00_00_00_00_00_FF_FF;
		       1: enables = 64'h00_00_00_00_FF_FF_00_00;
		       2: enables = 64'h00_00_FF_FF_00_00_00_00;
		       3: enables = 64'hFF_FF_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    r = r << 2;
		    case(request.address[0])
		       0: enables = 64'h00_00_00_00_FF_FF_FF_FF;
		       1: enables = 64'hFF_FF_FF_FF_00_00_00_00;
		    endcase
		 end
	    512: begin
		    r = r << 3;
		    enables = 64'hFF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 Bit#(29) address = truncate(r);
	 ddr3.request(address, (request.read ? 0 : enables), duplicate(request.data));
      endrule

      SyncFIFOIfc#(RAM_Response#(mem_data_t)) respFIFO <- mkSyncFIFO(4, ddr3Clk, jointReset, uClk);
      let connRespUser <- mkConnection(client.response, respFIFO, clocked_by uClk, reset_by uRst);

      rule connectResponseDDR3;
	 let response <- ddr3.read_data;
	 respFIFO.enq(truncate(response));
      endrule
   endmodule
endinstance

endpackage: SceMiSharedMemory
