// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package DiniSODIMM;

import Connectable       ::*;
import Clocks            ::*;
import FIFOF             ::*;
import FIFOLevel         ::*;
import SpecialFIFOs      ::*;
import TriState          ::*;
import DReg              ::*;
import Vector            ::*;
import Counter           ::*;
import Altera            ::*;
import I2C               ::*;
import StmtFSM           ::*;
import CommitIfc         ::*;
import ClientServer      ::*;
import GetPut            ::*;
import BUtils            ::*;
import Memory            ::*;

// SRAM exports

export SRAM_Pins(..);
export SRAM_User(..);
export SRAM_Controller(..);
export mkSRAMController;

export SRAM_Memory_Ifc(..);
export mkSRAMMemory;

// DDR2 exports

export DDR2_Pins(..);
export DDR2_User(..);
export DDR2_Controller(..);
export mkDDR2Controller;

export DDR2_Memory_Ifc(..);
export mkDDR2Memory;

////////////////////////////////////////////////////////////////////////////////
// SRAM controller implementation
////////////////////////////////////////////////////////////////////////////////

// Pin-level interface routed to the FPGA boundary
interface SRAM_Pins;
   interface Vector#(2, Clock) clock;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))  dq;
   (* always_ready, prefix = "", result = "A" *)
   method Bit#(21)   out_address();
   (* always_ready, prefix = "", result = "nBW" *)
   method Bit#(1)    out_byte_write_enable_n();
   (* always_ready, prefix = "", result = "nBE" *)
   method Bit#(8)    out_bank_write_enable_n();
   (* always_ready, prefix = "", result = "nGW" *)
   method Bit#(1)    out_global_write_enable_n();
   (* always_ready, prefix = "", result = "nE1" *)
   method Bit#(1)    out_chip_enable_1_n();
   (* always_ready, prefix = "", result = "E2" *)
   method Bit#(1)    out_chip_enable_2();
   (* always_ready, prefix = "", result = "nE3" *)
   method Bit#(1)    out_chip_enable_3_n();
   (* always_ready, prefix = "", result = "nG" *)
   method Bit#(1)    out_output_enable_n();
   (* always_ready, prefix = "", result = "nADV" *)
   method Bit#(1)    out_advance_n();
   (* always_ready, prefix = "", result = "nADSP" *)
   method Bit#(1)    out_address_strobe_processor_n();
   (* always_ready, prefix = "", result = "nADSC" *)
   method Bit#(1)    out_address_strobe_cache_n();
   (* always_ready, prefix = "", result = "ZZ" *)
   method Bit#(1)    out_power_down();
   (* always_ready, prefix = "", result = "nFT" *)
   method Bit#(1)    out_flow_through_mode_n();
   (* always_ready, prefix = "", result = "nLBO" *)
   method Bit#(1)    out_linear_burst_order_n();
endinterface: SRAM_Pins

// User-level interface for interacting with the SRAM controller from
// BSV
interface SRAM_User;
   method Action                 put(Bit#(8) writeen, Bit#(21) address, Bit#(64) datain);
   method ActionValue#(Bit#(64)) read();
endinterface: SRAM_User

// Controller interface combines user-facing interface and pin-level
// interface
interface SRAM_Controller;
   (* prefix = "" *)
   interface SRAM_Pins sram;
   (* prefix = "" *)
   interface SRAM_User user;
endinterface: SRAM_Controller

// Request structure used inside the SRAM controller
typedef struct {
                Bool     write;
                Bit#(8)  byteen;
                Bit#(21) address;
                Bit#(64) data;
               } SRAMRequest deriving (Bits, Eq, Bounded);

typedef struct {
                Bit#(64) data;
                } SRAMResponse deriving (Bits, Eq, Bounded);

// Controller state
typedef union tagged {
   void     Deselect;
   UInt#(1) Read;
   void     Write;
   } State deriving (Bits, Eq);

// This is the exported SRAM controller module
module mkSRAMController(SRAM_Controller);


   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   let                             sclk                <- exposeCurrentClock;

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   Reg#(State)                     rState              <- mkRegA(Deselect);
   FIFOF#(SRAMRequest)             fRequest            <- mkSizedFIFOF(4);
   FIFOF#(SRAMResponse)            fResponse           <- mkSizedFIFOF(4);
   Counter#(3)                     rReadsInFlight      <- mkCounter(0);

   Reg#(Bit#(1))                   rWriteN             <- mkDReg('1);
   Reg#(Bit#(8))                   rByteEnN            <- mkDReg('1);
   Reg#(Bit#(1))                   rEnableN            <- mkDReg('1);
   Reg#(Bit#(1))                   rStrobeN            <- mkDReg('1);
   Reg#(Bit#(1))                   rOutEnableN         <- mkRegA('1);
   Reg#(Bit#(21))                  rAddress            <- mkRegA(0);
   Reg#(Bit#(64))                  rDataOut            <- mkRegU;

   TriState#(Bit#(64))             tData               <- mkTriState( (rOutEnableN == 1), rDataOut );

   ////////////////////////////////////////////////////////////////////////////////
   /// Function
   ////////////////////////////////////////////////////////////////////////////////
   function Action process_request(SRAMRequest request);
      action
	 if (request.write) begin
	    rWriteN     <= 0;
	    rByteEnN    <= ~request.byteen;
	    rEnableN    <= 0;
	    rStrobeN    <= 0;
	    rOutEnableN <= 1;
	    rAddress    <= request.address;
	    rDataOut    <= request.data;
	    rState      <= Write;
	 end
	 else begin
	    rReadsInFlight.up;
	    rWriteN     <= 1;
	    rEnableN    <= 0;
	    rStrobeN    <= 0;
	    rOutEnableN <= 0;
	    rAddress    <= request.address;
	    rState      <= Read(0);
	 end
      endaction
   endfunction


   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule deselect(rState matches tagged Deselect &&& rReadsInFlight.value() < 4);
      let request = fRequest.first; fRequest.deq;
      process_request(request);
   endrule

   rule read0(rState matches tagged Read .cycle &&& cycle == 0);
      rState      <= Read(1);
   endrule
   rule read1(rState matches tagged Read .cycle &&& cycle == 1);
      rOutEnableN <= 1;
      fResponse.enq(unpack(tData));
      rState      <= Deselect;
   endrule

   rule writeContinue(rState matches tagged Write &&& fRequest.notEmpty);
      let request = fRequest.first; fRequest.deq;
      process_request(request);
   endrule
   rule writeDeselect(rState matches tagged Write &&& ! fRequest.notEmpty);
      rState      <= Deselect;
   endrule


   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface SRAM_User user;
      method    Action   put(writeen, address, datain);
	 fRequest.enq(SRAMRequest {
	    write:   (writeen != 0),
	    byteen:  writeen,
	    address: address,
	    data:    datain
	    });
      endmethod

      method    ActionValue#(Bit#(64)) read();
         rReadsInFlight.down;
	 fResponse.deq;
	 return fResponse.first.data;
      endmethod
   endinterface

   interface SRAM_Pins sram;
      interface clock                             = replicate(sclk);
      interface dq                                = tData.io;
      method    out_address                       = rAddress;
      method    out_byte_write_enable_n           = rWriteN;
      method    out_bank_write_enable_n           = rByteEnN;
      method    out_global_write_enable_n         = 1;
      method    out_chip_enable_1_n               = rEnableN;
      method    out_chip_enable_2                 = 1;
      method    out_chip_enable_3_n               = 0;
      method    out_output_enable_n               = rOutEnableN;
      method    out_advance_n                     = 1;
      method    out_address_strobe_processor_n    = 1;
      method    out_address_strobe_cache_n        = rStrobeN;
      method    out_power_down                    = 0;
      method    out_flow_through_mode_n           = 0;
      method    out_linear_burst_order_n          = 0;
   endinterface

endmodule

module mkFIFOLevelAdapter(FIFOLevelIfc#(a,b) coreFIFO, FIFOF#(a) ifc )
   provisos (Bits#(a,sa) );

   method enq = coreFIFO.enq;
   method deq = coreFIFO.deq;
   method first = coreFIFO.first;
   method clear = coreFIFO.clear;
   method notFull = coreFIFO.notFull;
   method notEmpty = coreFIFO.notEmpty;

endmodule

module mkBypassFIFOAdapter (FIFOF#(a) coreFIFO, FIFOF#(a) ifc )
   provisos (Bits#(a,sa) );

   RWire#(a) enqData    <- mkRWire ;
   RWire#(a) outData    <- mkRWire ;
   PulseWire deqCalled  <- mkPulseWire ;

   Bool okDeq = coreFIFO.notEmpty || isValid (enqData.wget);

   // Rule to set the output first data
   rule setFirstCore (coreFIFO.notEmpty) ;
      outData.wset (coreFIFO.first) ;
   endrule
   rule setFirstEnq (!coreFIFO.notEmpty &&& enqData.wget matches tagged Valid .d);
      outData.wset (d);
   endrule

   // Rules for enq or deq
   rule enqOnly (! deqCalled &&&
                 enqData.wget matches tagged Valid .d );
      coreFIFO.enq (d);
   endrule
   rule deqOnly (deqCalled && !isValid(enqData.wget) );
      coreFIFO.deq ;
   endrule
   rule enqAndDeq ( deqCalled &&&
                   enqData.wget matches tagged Valid .d );
      coreFIFO.enq (d);
      coreFIFO.deq ;
   endrule

   method Action enq (a din) if (coreFIFO.notFull);
      enqData.wset (din);
   endmethod
   method Action deq if (okDeq) ;
      deqCalled.send ;
   endmethod
   method a first if (outData.wget matches tagged Valid .d) ;
      return d ;
   endmethod

   method Action clear ;
      coreFIFO.clear ;
   endmethod

   method Bool notFull ;
      return coreFIFO.notFull;
   endmethod
   method Bool notEmpty ;
      return okDeq ;
   endmethod

endmodule

////////////////////////////////////////////////////////////////////////////////
/// SRAM memory model implementation
////////////////////////////////////////////////////////////////////////////////
interface SRAM_Memory_Ifc;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))  dq;
   (* prefix = "" *)
   method   Action      in_address((* port = "A" *)Bit#(21) x);
   (* prefix = "" *)
   method   Action      in_byte_write_enable_n((* port = "nBW" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_bank_write_enable_n((* port = "nBE" *)Bit#(8) x);
   (* prefix = "" *)
   method   Action      in_global_write_enable_n((* port = "nGW" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_chip_enable_1_n((* port = "nE1" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_chip_enable_2((* port = "E2" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_chip_enable_3_n((* port = "nE3" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_output_enable_n((* port = "nG" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_advance_n((* port = "nADV" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_address_strobe_processor_n((* port = "nADSP" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_address_strobe_cache_n((* port = "nADSC" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_power_down((* port = "ZZ" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_flow_through_mode_n((* port = "nFT" *)Bit#(1) x);
   (* prefix = "" *)
   method   Action      in_linear_burst_order_n((* port = "nLBO" *)Bit#(1) x);
endinterface

// imported SRAM memory model
import "BVI" dini_sram_sodimm =
module vMkSRAMMemory#(Vector#(2, Clock) clocks)(SRAM_Memory_Ifc);
   default_clock clk(CLK0) = clocks[0];
   no_reset;

   input_clock clk1(CLK1) = clocks[1];

   ifc_inout dq(DQ);

   method   in_address(A)                        enable((*inhigh*)en0);
   method   in_byte_write_enable_n(nBW)          enable((*inhigh*)en1);
   method   in_bank_write_enable_n(nBE)          enable((*inhigh*)en2);
   method   in_global_write_enable_n(nGW)        enable((*inhigh*)en3);
   method   in_chip_enable_1_n(nE1)              enable((*inhigh*)en4);
   method   in_chip_enable_2(E2)                 enable((*inhigh*)en5);
   method   in_chip_enable_3_n(nE3)              enable((*inhigh*)en6);
   method   in_output_enable_n(nG)               enable((*inhigh*)en7);
   method   in_advance_n(nADV)                   enable((*inhigh*)en8);
   method   in_address_strobe_processor_n(nADSP) enable((*inhigh*)en9);
   method   in_address_strobe_cache_n(nADSC)     enable((*inhigh*)ena);
   method   in_power_down(ZZ)                    enable((*inhigh*)enb);
   method   in_flow_through_mode_n(nFT)          enable((*inhigh*)enc);
   method   in_linear_burst_order_n(nLBO)        enable((*inhigh*)ene);


   schedule (in_address, in_byte_write_enable_n, in_bank_write_enable_n,
      in_global_write_enable_n, in_chip_enable_1_n, in_chip_enable_2,
      in_chip_enable_3_n, in_output_enable_n, in_advance_n,
      in_address_strobe_processor_n, in_address_strobe_cache_n, in_power_down,
      in_flow_through_mode_n, in_linear_burst_order_n) CF (in_address,
      in_byte_write_enable_n, in_bank_write_enable_n,
      in_global_write_enable_n, in_chip_enable_1_n, in_chip_enable_2,
      in_chip_enable_3_n, in_output_enable_n, in_advance_n,
      in_address_strobe_processor_n, in_address_strobe_cache_n, in_power_down,
      in_flow_through_mode_n, in_linear_burst_order_n);

endmodule

module mkSRAMMemory#(Vector#(2, Clock) clocks)(SRAM_Memory_Ifc);
   let _m <- vMkSRAMMemory(clocks);
   return _m;
endmodule

// Connectable instances for memory controller and memory model

instance Connectable#(SRAM_Memory_Ifc, SRAM_Pins);
   module mkConnection#(SRAM_Memory_Ifc mem, SRAM_Pins ctrl)(Empty);
      mkConnection(mem.dq, ctrl.dq);
      rule connect;
         mem.in_byte_write_enable_n(ctrl.out_byte_write_enable_n);
         mem.in_bank_write_enable_n(ctrl.out_bank_write_enable_n);
         mem.in_global_write_enable_n(ctrl.out_global_write_enable_n);
         mem.in_advance_n(ctrl.out_advance_n);
         mem.in_address_strobe_processor_n(ctrl.out_address_strobe_processor_n);
         mem.in_power_down(ctrl.out_power_down);
         mem.in_flow_through_mode_n(ctrl.out_flow_through_mode_n);
         mem.in_linear_burst_order_n(ctrl.out_linear_burst_order_n);
      endrule
      rule connect2;
         mem.in_address_strobe_cache_n(ctrl.out_address_strobe_cache_n);
      endrule
      rule connect3;
         mem.in_chip_enable_1_n(ctrl.out_chip_enable_1_n);
         mem.in_chip_enable_2(ctrl.out_chip_enable_2);
         mem.in_chip_enable_3_n(ctrl.out_chip_enable_3_n);
      endrule
      rule connect4;
         mem.in_address(ctrl.out_address);
      endrule
      rule connect5;
         mem.in_output_enable_n(ctrl.out_output_enable_n);
      endrule
   endmodule
endinstance

instance Connectable#(SRAM_Pins, SRAM_Memory_Ifc);
   module mkConnection#(SRAM_Pins ctrl, SRAM_Memory_Ifc mem)(Empty);
      mkConnection(mem, ctrl);
   endmodule
endinstance


////////////////////////////////////////////////////////////////////////////////
// DDR2 controller implementation
////////////////////////////////////////////////////////////////////////////////

// Pin-level interface routed to the FPGA boundary
interface DDR2_Pins;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))     dq;
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(8))      dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(8))      dqs_n;
   (* prefix = "CLK_P" *)
   interface Inout#(Bit#(2))      clk_p;
   (* prefix = "CLK_N" *)
   interface Inout#(Bit#(2))      clk_n;
   (* always_ready, prefix = "", result = "A" *)
   method Bit#(16)  address();
   (* always_ready, prefix = "", result = "BA" *)
   method Bit#(3)   ba();
   (* always_ready, prefix = "", result = "CASN" *)
   method Bit#(1)   cas_n();
   (* always_ready, prefix = "", result = "CKE" *)
   method Bit#(2)   cke();
   (* always_ready, prefix = "", result = "CSN" *)
   method Bit#(2)   cs_n();
   (* always_ready, prefix = "", result = "DM" *)
   method Bit#(8)   dm();
   (* always_ready, prefix = "", result = "ODT" *)
   method Bit#(2)   odt();
   (* always_ready, prefix = "", result = "RASN" *)
   method Bit#(1)   ras_n();
   (* always_ready, prefix = "", result = "WEN" *)
   method Bit#(1)   we_n();
   (* always_ready, always_enabled, prefix = "" *)
   method Action    oct_rdn((* port = "RDN" *)Bool i);
   (* always_ready, always_enabled, prefix = "" *)
   method Action    oct_rup((* port = "RUP" *)Bool i);
   (* prefix = "SCL" *)
   interface Inout#(Bit#(1)) scl;
   (* prefix = "SDA" *)
   interface Inout#(Bit#(1)) sda;
endinterface: DDR2_Pins

// User-level interface for interacting with the DDR2 controller from
// BSV
interface DDR2_User;
   interface Clock                clock;
   interface Reset                reset_n;
   method Bool                    init_done;
   method Action                  put(Bit#(32) writeen, Bit#(30) address, Bit#(256) datain);
   method ActionValue#(Bit#(256)) read();
endinterface

// Controller interface combines user-facing interface and pin-level
// interface
interface DDR2_Controller;
   (* prefix = "" *)
   interface DDR2_Pins ddr2;
   (* prefix = "" *)
   interface DDR2_User user;
endinterface: DDR2_Controller

interface VDDR2_Pins;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))     dq;
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(8))      dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(8))      dqs_n;
   (* prefix = "CLK_P" *)
   interface Inout#(Bit#(2))      clk_p;
   (* prefix = "CLK_N" *)
   interface Inout#(Bit#(2))      clk_n;
   (* always_ready, prefix = "", result = "A" *)
   method Bit#(16)  address();
   (* always_ready, prefix = "", result = "BA" *)
   method Bit#(3)   ba();
   (* always_ready, prefix = "", result = "CASN" *)
   method Bit#(1)   cas_n();
   (* always_ready, prefix = "", result = "CKE" *)
   method Bit#(2)   cke();
   (* always_ready, prefix = "", result = "CSN" *)
   method Bit#(2)   cs_n();
   (* always_ready, prefix = "", result = "DM" *)
   method Bit#(8)   dm();
   (* always_ready, prefix = "", result = "ODT" *)
   method Bit#(2)   odt();
   (* always_ready, prefix = "", result = "RASN" *)
   method Bit#(1)   ras_n();
   (* always_ready, prefix = "", result = "WEN" *)
   method Bit#(1)   we_n();
   (* always_ready, always_enabled, prefix = "" *)
   method Action    oct_rdn((* port = "RDN" *)Bool i);
   (* always_ready, always_enabled, prefix = "" *)
   method Action    oct_rup((* port = "RUP" *)Bool i);
endinterface: VDDR2_Pins

// Wrapper-facing subinterface of imported DDR2 memory controller
(* always_ready, always_enabled *)
interface VDDR2_User;
   interface Clock     clock;
   interface Reset     reset_n;
   method    Bool      init_done();
   method    Bool      cal_fail();
   method    Bool      cal_success();
   method    Action    local_address(Bit#(30) i);
   method    Action    local_write_req(Bool i);
   method    Action    local_read_req(Bool i);
   method    Action    local_burstbegin(Bool i);
   method    Action    local_wdata(Bit#(256) i);
   method    Action    local_be(Bit#(32) i);
   method    Bool      local_ready();
   method    Bit#(256) local_rdata();
   method    Bool      local_rdata_valid();
endinterface: VDDR2_User

(* always_ready, always_enabled *)
interface VDDR2_Csr;
   method    Action    addr(Bit#(16) i);
   method    Action    be(Bit#(4) i);
   method    Action    read_req(Bool i);
   method    Action    write_req(Bool i);
   method    Action    write_data(Bit#(32) i);
   method    Bit#(32)  read_data();
   method    Bool      read_data_valid();
   method    Bool      waitrequest();
endinterface

// Imported controller interface combines wrapper-facing interface and
// pin-level interface
interface VDDR2_Controller;
   (* prefix = "" *)
   interface VDDR2_Pins   ddr2;
   (* prefix = "" *)
   interface VDDR2_User   user;
   (* prefix = "" *)
   interface VDDR2_Csr    csr;
endinterface: VDDR2_Controller

// Imported DDR2 controller
import "BVI" ddr2_v12_1 =
module vMkDDR2Controller(VDDR2_Controller);
   default_clock clk(pll_ref_clk);
   default_reset rstn(global_reset_n);

   port soft_reset_n     = (Bit#(1))'(1);
   port avl_size         = (Bit#(3))'(1); // no bursts
   
   interface VDDR2_Pins ddr2;
      ifc_inout   dq(mem_dq)                 reset_by(no_reset);
      ifc_inout   dqs_p(mem_dqs)             reset_by(no_reset);
      ifc_inout   dqs_n(mem_dqs_n)           reset_by(no_reset);
      ifc_inout   clk_p(mem_ck)              reset_by(no_reset);
      ifc_inout   clk_n(mem_ck_n)            reset_by(no_reset);
      method      mem_a     address 	     reset_by(no_reset);
      method      mem_ba    ba      	     reset_by(no_reset);
      method      mem_cas_n cas_n   	     reset_by(no_reset);
      method      mem_cke   cke     	     reset_by(no_reset);
      method      mem_cs_n  cs_n    	     reset_by(no_reset);
      method      mem_dm    dm      	     reset_by(no_reset);
      method      mem_odt   odt     	     reset_by(no_reset);
      method      mem_ras_n ras_n   	     reset_by(no_reset);
      method      mem_we_n  we_n    	     reset_by(no_reset);
      method                oct_rdn(oct_rdn) enable((*inhigh*)en98) reset_by(no_reset);
      method                oct_rup(oct_rup) enable((*inhigh*)en99) reset_by(no_reset);
   endinterface

   interface VDDR2_User user;
      output_clock   clock(afi_clk);
      output_reset   reset_n(afi_reset_n) clocked_by(user_clock);
      method      local_init_done       init_done       clocked_by(no_clock)   reset_by(no_reset);
      method      local_cal_fail        cal_fail        clocked_by(no_clock)   reset_by(no_reset);
      method      local_cal_success     cal_success     clocked_by(no_clock)   reset_by(no_reset);
      method                            local_address(avl_addr)            enable((*inhigh*)en0) clocked_by(user_clock) reset_by(user_reset_n);
      method                            local_write_req(avl_write_req)     enable((*inhigh*)en1) clocked_by(user_clock) reset_by(user_reset_n);
      method                            local_read_req(avl_read_req)       enable((*inhigh*)en2) clocked_by(user_clock) reset_by(user_reset_n);
      method                            local_burstbegin(avl_burstbegin)   enable((*inhigh*)en3) clocked_by(user_clock) reset_by(user_reset_n);
      method                            local_wdata(avl_wdata)             enable((*inhigh*)en4) clocked_by(user_clock) reset_by(user_reset_n);
      method                            local_be(avl_be)                   enable((*inhigh*)en5) clocked_by(user_clock) reset_by(user_reset_n);
      method      avl_ready             local_ready()                                            clocked_by(user_clock) reset_by(user_reset_n);
      method      avl_rdata             local_rdata()                                            clocked_by(user_clock) reset_by(user_reset_n);
      method      avl_rdata_valid       local_rdata_valid()                                      clocked_by(user_clock) reset_by(user_reset_n);
   endinterface
   
   interface VDDR2_Csr csr;
      method                            addr(csr_addr)                     enable((*inhigh*)en6) clocked_by(user_clock) reset_by(user_reset_n);
      method                            be(csr_be)                         enable((*inhigh*)en7) clocked_by(user_clock) reset_by(user_reset_n);
      method                            read_req(csr_read_req)             enable((*inhigh*)en8) clocked_by(user_clock) reset_by(user_reset_n);
      method                            write_req(csr_write_req)           enable((*inhigh*)en9) clocked_by(user_clock) reset_by(user_reset_n);
      method                            write_data(csr_wdata)              enable((*inhigh*)en10) clocked_by(user_clock) reset_by(user_reset_n);
      method      csr_rdata             read_data()                                              clocked_by(user_clock) reset_by(user_reset_n);
      method      csr_rdata_valid       read_data_valid()                                        clocked_by(user_clock) reset_by(user_reset_n);
      method      csr_waitrequest       waitrequest()                                            clocked_by(user_clock) reset_by(user_reset_n);
   endinterface
   
   schedule
   (
    ddr2_address, ddr2_ba, ddr2_cas_n, ddr2_cke,
    ddr2_cs_n, ddr2_dm, ddr2_odt, ddr2_ras_n, 
    ddr2_we_n, user_init_done, user_cal_fail, user_cal_success,
    ddr2_oct_rup, ddr2_oct_rdn
    )
   CF
   (
    ddr2_address, ddr2_ba, ddr2_cas_n, ddr2_cke, 
    ddr2_cs_n, ddr2_dm, ddr2_odt, ddr2_ras_n, 
    ddr2_we_n, user_init_done, user_cal_fail, user_cal_success,
    ddr2_oct_rup, ddr2_oct_rdn
    );
   
   schedule
   (
    user_local_address, user_local_write_req, user_local_read_req, 
    user_local_burstbegin, user_local_wdata, user_local_be, user_local_ready,
    user_local_rdata, user_local_rdata_valid,
    csr_addr, csr_be, csr_read_req,
    csr_write_req, csr_write_data, csr_read_data, csr_read_data_valid,
    csr_waitrequest
    )
   CF
   (
    user_local_address, user_local_write_req, user_local_read_req,
    user_local_burstbegin, user_local_wdata, user_local_be, user_local_ready,
    user_local_rdata, user_local_rdata_valid,
    csr_addr, csr_be, csr_read_req,
    csr_write_req, csr_write_data, csr_read_data, csr_read_data_valid,
    csr_waitrequest
    );
   
endmodule: vMkDDR2Controller

// This is the exported DDR2 controller module, which wraps the imported
// DDR2 controller.
module mkDDR2Controller(DDR2_Controller);

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR2_Controller                ddr2ctrl            <- vMkDDR2Controller;
   Clock                           user_clock           = ddr2ctrl.user.clock;
   Reset                           user_reset_n         = ddr2ctrl.user.reset_n;
   
   I2C                             i2cctrl             <- mkI2C(1024, clocked_by user_clock, reset_by user_reset_n);
 
   Wire#(Bool)                     wWriteReq           <- mkDWire(False, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bool)                     wReadReq            <- mkDWire(False, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(30))                 wAddress            <- mkDWire(0,     clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(32))                 wByteEn             <- mkDWire(0,     clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(256))                wWriteData          <- mkDWire(0,     clocked_by user_clock, reset_by user_reset_n);

   Reg#(Bit#(8))                   rColumns            <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(8))                   rRows               <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(4))                   rBanks              <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(4))                   rRanks              <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   
   Reg#(Bit#(16))                  rCSRAddr            <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(4))                   rCSRBe              <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(32))                  rCSRWData           <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                      rCSRWrite           <- mkRegA(False, clocked_by user_clock, reset_by user_reset_n);   
   Reg#(Bool)                      rCSRRead            <- mkRegA(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(32))                  rCSRRData           <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   
   FIFOF#(Bit#(256))               fReadData           <- mkSizedFIFOF(4, clocked_by user_clock, reset_by user_reset_n);
   Counter#(3)                     rReadsPending       <- mkCounter(0, clocked_by user_clock, reset_by user_reset_n);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Functions
   ////////////////////////////////////////////////////////////////////////////////
   function Bit#(30) convert(Bit#(30) address); // 64-bit aligned
      Bit#(30) addr = 0;
      let tempaddr = address;
      
      case(rColumns)
	 8:  begin addr[9:0] = zeroExtend(tempaddr[5:0]); tempaddr = tempaddr >> 6; end
	 9:  begin addr[9:0] = zeroExtend(tempaddr[6:0]); tempaddr = tempaddr >> 7; end
	 10: begin addr[9:0] = zeroExtend(tempaddr[7:0]); tempaddr = tempaddr >> 8; end
	 11: begin addr[9:0] = zeroExtend(tempaddr[8:0]); tempaddr = tempaddr >> 9; end
	 12: begin addr[9:0] = zeroExtend(tempaddr[9:0]); tempaddr = tempaddr >> 10; end
      endcase
      
      case(rRows)
	 12: begin addr[25:10] = zeroExtend(tempaddr[11:0]); tempaddr = tempaddr >> 12; end
	 13: begin addr[25:10] = zeroExtend(tempaddr[12:0]); tempaddr = tempaddr >> 13; end
	 14: begin addr[25:10] = zeroExtend(tempaddr[13:0]); tempaddr = tempaddr >> 14; end
	 15: begin addr[25:10] = zeroExtend(tempaddr[14:0]); tempaddr = tempaddr >> 15; end
	 16: begin addr[25:10] = zeroExtend(tempaddr[15:0]); tempaddr = tempaddr >> 16; end
      endcase

      case(rBanks)
	 2:  begin addr[28:26] = zeroExtend(tempaddr[1:0]); tempaddr = tempaddr >> 2; end
	 3:  begin addr[28:26] = zeroExtend(tempaddr[2:0]); tempaddr = tempaddr >> 3; end
      endcase
      
      case(rRanks)
	 0:  begin addr[29]    = 1'b0;        end
	 1:  begin addr[29]    = tempaddr[0]; end
      endcase
      
      return addr;
   endfunction
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_user;
      ddr2ctrl.user.local_write_req(wWriteReq);
      ddr2ctrl.user.local_read_req(wReadReq);
      ddr2ctrl.user.local_burstbegin(wWriteReq || wReadReq);
      ddr2ctrl.user.local_address(wAddress);
      ddr2ctrl.user.local_be(wByteEn);
      ddr2ctrl.user.local_wdata(wWriteData);
   endrule
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_csr_inputs;
      ddr2ctrl.csr.addr(rCSRAddr);
      ddr2ctrl.csr.write_data(rCSRWData);
      ddr2ctrl.csr.be(rCSRBe);
      ddr2ctrl.csr.write_req(rCSRWrite);
      ddr2ctrl.csr.read_req(rCSRRead);
   endrule
   
   rule get_ddr2_read_data(ddr2ctrl.user.init_done && ddr2ctrl.user.local_rdata_valid);
      fReadData.enq(ddr2ctrl.user.local_rdata);
   endrule
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_csr_outputs(ddr2ctrl.csr.read_data_valid());
      rCSRRData <= ddr2ctrl.csr.read_data();
   endrule
      
   Stmt spd_eeprom =
   seq
      // read out rows
      i2cctrl.user.request.put(I2CRequest { write: False, slaveaddr: 7'h50, address: 8'd03, data: ? });
      // read out columns
      i2cctrl.user.request.put(I2CRequest { write: False, slaveaddr: 7'h50, address: 8'd04, data: ? });
      // read out ranks
      i2cctrl.user.request.put(I2CRequest { write: False, slaveaddr: 7'h50, address: 8'd05, data: ? });
      // read out banks
      i2cctrl.user.request.put(I2CRequest { write: False, slaveaddr: 7'h50, address: 8'd17, data: ? });
      
      // rows
      action
	 let rows <- i2cctrl.user.response.get;
	 rRows <= pack(rows);
      endaction
      
      // columns
      action
	 let cols <- i2cctrl.user.response.get;
	 rColumns <= pack(cols);
      endaction

      // ranks
      action
	 let ranks <- i2cctrl.user.response.get;
	 rRanks <= truncate(pack(ranks));
      endaction
      
      // banks
      action
	 let banks <- i2cctrl.user.response.get;
	 case(pack(banks))
	    8:       rBanks <= 3;
	    default: rBanks <= 2;
	 endcase
      endaction
      
      // read out some values from the CSR
      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRRead  <= True;
	 rCSRAddr  <= 'h100;
      endaction
      rCSRRead <= False;
      
      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRRead  <= True;
	 rCSRAddr  <= 'h120;
      endaction
      rCSRRead <= False;

      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRRead  <= True;
	 rCSRAddr  <= 'h122;
      endaction
      rCSRRead <= False;
      
      // now update the CSRs in the controller
      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRWrite <= True;
	 rCSRAddr  <= 'h120;
	 rCSRBe    <= 'b0111;
	 rCSRWData <= { 8'h00, rRanks, rBanks, rRows, rColumns };
      endaction
      rCSRWrite <= False;

      await(!ddr2ctrl.csr.waitrequest);
      action
	 Bit#(8) rankmask = 0;
	 case(rRanks)
	    0: rankmask = 'h01;
	    1: rankmask = 'h03;
	 endcase

	 rCSRWrite <= True;
	 rCSRAddr  <= 'h122;
	 rCSRBe    <= 'b0001;
	 rCSRWData <= { 8'h00, 8'h00, 8'h00, rankmask};
      endaction
      rCSRWrite <= False;

      // recalibrate!
      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRWrite <= True;
	 rCSRAddr  <= 'h100;
	 rCSRBe    <= 'b0001;
	 rCSRWData <= 'h04;
      endaction
      rCSRWrite <= False;
      
      await(!ddr2ctrl.csr.waitrequest);
      action
	 rCSRWrite <= True;
	 rCSRAddr  <= 'h100;
	 rCSRBe    <= 'b0001;
	 rCSRWData <= 'h00;
      endaction
      rCSRWrite <= False;
      
      await(!ddr2ctrl.user.init_done);
   endseq;
   
   FSM                             fsmSPD              <- mkFSM(spd_eeprom, clocked_by user_clock, reset_by user_reset_n);
   
   rule start_i2c_spd(fsmSPD.done);
      fsmSPD.start;
   endrule
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface DDR2_Pins ddr2;
      interface dq      = ddr2ctrl.ddr2.dq;
      interface dqs_p   = ddr2ctrl.ddr2.dqs_p;
      interface dqs_n   = ddr2ctrl.ddr2.dqs_n;
      interface clk_p   = ddr2ctrl.ddr2.clk_p;
      interface clk_n   = ddr2ctrl.ddr2.clk_n;
      method 	address = ddr2ctrl.ddr2.address;
      method 	ba      = ddr2ctrl.ddr2.ba;
      method 	cas_n   = ddr2ctrl.ddr2.cas_n;
      method 	cke     = ddr2ctrl.ddr2.cke;
      method 	cs_n    = ddr2ctrl.ddr2.cs_n;
      method 	dm      = ddr2ctrl.ddr2.dm;
      method 	odt     = ddr2ctrl.ddr2.odt;
      method 	ras_n   = ddr2ctrl.ddr2.ras_n;
      method 	we_n    = ddr2ctrl.ddr2.we_n;
      method 	oct_rdn = ddr2ctrl.ddr2.oct_rdn;
      method 	oct_rup = ddr2ctrl.ddr2.oct_rup;
      interface	scl     = i2cctrl.i2c.scl;
      interface sda     = i2cctrl.i2c.sda;
   endinterface
      
   interface DDR2_User user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr2ctrl.user.init_done;
      method Action put(writeen, address, datain) if (ddr2ctrl.user.init_done && ddr2ctrl.user.local_ready && rReadsPending.value < 4);
	 wAddress   <= convert(address);
	 wByteEn    <= writeen;
	 wWriteData <= datain;
	 wWriteReq  <= (writeen != 0);
	 wReadReq   <= (writeen == 0);
	 if (writeen == 0) rReadsPending.up;
      endmethod
      method ActionValue#(Bit#(256)) read();
	 rReadsPending.down;
	 fReadData.deq;
	 return fReadData.first;
      endmethod
   endinterface
   
endmodule: mkDDR2Controller

////////////////////////////////////////////////////////////////////////////////
// DDR2 memory model implementation
////////////////////////////////////////////////////////////////////////////////

// DDR2 memory model interface
interface DDR2_Memory_Ifc; // input clk_p/clk_n
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))     dq;
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(8))      dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(8))      dqs_n;
   (* prefix = "CLK_P" *)
   interface Inout#(Bit#(2))      clk_p;
   (* prefix = "CLK_N" *)
   interface Inout#(Bit#(2))      clk_n;
   (* prefix = "" *)
   interface Reset                global_reset_n;
   (* always_enabled, prefix = "" *)
   method Action    address((* port = "A" *) Bit#(16) x);
   (* always_enabled, prefix = "" *)
   method Action    ba((* port = "BA" *) Bit#(3) x);
   (* always_enabled, prefix = "" *)
   method Action    cas_n((* port = "CASN" *) Bit#(1) x);
   (* always_enabled, prefix = "" *)
   method Action    cke((* port = "CKE" *) Bit#(2) x);
   (* always_enabled, prefix = "" *)
   method Action    cs_n((* port = "CSN" *) Bit#(2) x);
   (* always_enabled, prefix = "" *)
   method Action    dm((* port = "DM" *) Bit#(8) x);
   (* always_enabled, prefix = "" *)
   method Action    odt((* port = "ODT" *) Bit#(2) x);
   (* always_enabled, prefix = "" *)
   method Action    ras_n((* port = "RASN" *) Bit#(1) x);
   (* always_enabled, prefix = "" *)
   method Action    we_n((* port = "WEN" *) Bit#(1) x);
endinterface: DDR2_Memory_Ifc

// imported DDR2 memory model
import "BVI" altera_ddr2_sodimm =
module vMkDDR2Memory(DDR2_Memory_Ifc);
   default_clock ();
   no_reset;
   
   output_reset global_reset_n(global_reset_n);
   
   ifc_inout dq(mem_dq);
   ifc_inout dqs_p(mem_dqs);
   ifc_inout dqs_n(mem_dqsn);
   ifc_inout clk_p(mem_clk);
   ifc_inout clk_n(mem_clk_n);
   
   method address(mem_addr) enable((*inhigh*)en0);
   method ba(mem_ba) enable((*inhigh*)en1);
   method cas_n(mem_cas_n) enable((*inhigh*)en2);
   method cke(mem_cke) enable((*inhigh*)en3);
   method cs_n(mem_cs_n) enable((*inhigh*)en4);
   method dm(mem_dm) enable((*inhigh*)en5);
   method odt(mem_odt) enable((*inhigh*)en6);
   method ras_n(mem_ras_n) enable((*inhigh*)en7);
   method we_n(mem_we_n) enable((*inhigh*)en8);

   schedule (address, ba, cas_n, cke, cs_n, dm, odt, ras_n, we_n) CF
            (address, ba, cas_n, cke, cs_n, dm, odt, ras_n, we_n);

endmodule: vMkDDR2Memory

// Simple BSV wrapper around import
module mkDDR2Memory(DDR2_Memory_Ifc);
   let _m <- vMkDDR2Memory;
   return _m;
endmodule

// Connectable instances for memory controller and memory model

instance Connectable#(DDR2_Memory_Ifc, DDR2_Pins);
   module mkConnection#(DDR2_Memory_Ifc mem, DDR2_Pins ctrl)(Empty);
      mkConnection(mem.dq, ctrl.dq);
      mkConnection(mem.dqs_p, ctrl.dqs_p);
      mkConnection(mem.dqs_n, ctrl.dqs_n);
      mkConnection(mem.clk_p, ctrl.clk_p);
      mkConnection(mem.clk_n, ctrl.clk_n);

      rule connect;
         mem.address(ctrl.address);
         mem.ba(ctrl.ba);
         mem.cas_n(ctrl.cas_n);
         mem.cke(ctrl.cke);
         mem.cs_n(ctrl.cs_n);
         mem.dm(ctrl.dm);
         mem.odt(ctrl.odt);
         mem.ras_n(ctrl.ras_n);
         mem.we_n(ctrl.we_n);
      endrule
   endmodule
endinstance

instance Connectable#(DDR2_Pins, DDR2_Memory_Ifc);
   module mkConnection#(DDR2_Pins ctrl, DDR2_Memory_Ifc mem)(Empty);
      mkConnection(mem, ctrl);
   endmodule
endinstance

instance Connectable#(MemoryClient#(a, d), DDR2_User)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR2_User ddr2)(Empty);
      let sz = valueOf(d);
   
      Clock uClock              <- exposeCurrentClock;
      Reset uReset              <- exposeCurrentReset;
      Clock dClock              = ddr2.clock;
      Reset dReset              = ddr2.reset_n;
      
      Reset uRst_dclk           <- mkAsyncReset( 2, uReset, dClock );
      Reset jointReset1         <- mkResetEither( dReset, uRst_dclk, clocked_by dClock );
      Reset jointReset          <- mkAsyncReset( 2, jointReset1, dClock );
      
      SyncFIFOIfc#(MemoryRequest#(a,d)) reqFIFO <- mkSyncFIFO(1, uClock, uReset, dClock);
      let connectReqUser <- mkConnection(client.request, toPut(reqFIFO), clocked_by uClock, reset_by uReset);
      FIFOF#(Tuple3#(Bit#(32), Bit#(30), Bit#(256))) oddReq <- mkFIFOF1(clocked_by dClock, reset_by dReset);
      
      rule connect_requests if (oddReq.notFull);
         let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(30) address = cExtend(request.address);
	 
	 case(sz)
	    64:  begin
		    ddr2.put(((request.write) ? 32'hFF : 0), address, cExtend(request.data));
		 end
	    128: begin
		    ddr2.put(((request.write) ? 32'hFFFF : 0), address, cExtend(request.data));
		 end
	    256: begin
		    ddr2.put(((request.write) ? 32'hFFFFFFFF : 0), address, cExtend(request.data));
		 end
	    512: begin
		    ddr2.put(((request.write) ? 32'hFFFFFFFF : 0), address, cExtend(request.data));
		    oddReq.enq(tuple3(((request.write) ? 32'hFFFFFFFF : 0), address + 30'h20, cExtendLSB(request.data)));
		 end
	    default: error("DDR2 connection: data width must be 64, 128, 256, or 512");
	 endcase
      endrule
      
      rule connect_odd_requests if (oddReq.notEmpty);
	 match { .enables, .address, .data } <- toGet(oddReq).get;
	 ddr2.put(enables, address, data);
      endrule

      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);
      FIFOF#(Bit#(256)) oddResp <- mkFIFOF1(clocked_by dClock, reset_by dReset);

      if (sz == 512) begin
	 rule connect_first_response if (oddResp.notFull);
	    let response <- ddr2.read;
	    oddResp.enq(response);
	 endrule
	 
	 rule connect_second_response if (oddResp.notEmpty);
	    let response <- ddr2.read;
	    oddResp.deq;
	    respFIFO.enq(cExtend({ response, oddResp.first }));
	 endrule
      end
      else begin
	 rule connect_response;
            let response <- ddr2.read;
            respFIFO.enq(cExtend(response));
	 endrule
      end
   endmodule
endinstance


endpackage: DiniSODIMM
