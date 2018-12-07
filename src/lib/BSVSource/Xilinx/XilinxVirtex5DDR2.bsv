////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtex5DDR2.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtex5DDR2;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Connectable       ::*;
import Clocks            ::*;
import FIFO              ::*;
import FIFOF             ::*;
import SpecialFIFOs      ::*;
import TriState          ::*;
import Vector            ::*;
import Cntrs             ::*;
import DefaultValue      ::*;
import GetPut            ::*;
import ClientServer      ::*;
import BUtils            ::*;

import Memory            ::*;
import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export DDR2_Pins(..);
export DDR2_User(..);
export DDR2_Controller(..);
export DDR2_Memory_Ifc(..);
export DDR2_Configure(..);

export mkDDR2Controller;
export mkDDR2Memory;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bit#(32)  byteen;
   Bit#(31)  address;
   Bit#(256) data;
   } DDR2Request deriving (Bits, Eq);

typedef struct {
   Bit#(256)          data;
   } DDR2Response deriving (Bits, Eq);

typedef struct {
   Bool               fast_train_sim_only;
   Integer            clk_period_in_ps;
   Integer            num_reads_in_flight;
} DDR2_Configure;

instance DefaultValue#(DDR2_Configure);
   defaultValue = DDR2_Configure {
        fast_train_sim_only:    False
      , clk_period_in_ps:       8000
      , num_reads_in_flight:    1
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface DDR2_Pins;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(2)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(2)           clk_n;
   (* prefix = "", result = "A" *)
   method    Bit#(13)          a;
   (* prefix = "", result = "BA" *)
   method    Bit#(2)           ba;
   (* prefix = "", result = "RAS_N" *)
   method    Bit#(1)           ras_n;
   (* prefix = "", result = "CAS_N" *)
   method    Bit#(1)           cas_n;
   (* prefix = "", result = "WE_N" *)
   method    Bit#(1)           we_n;
   (* prefix = "", result = "CS_N" *)
   method    Bit#(1)           cs_n;
   (* prefix = "", result = "ODT" *)
   method    Bit#(1)           odt;
   (* prefix = "", result = "CKE" *)
   method    Bit#(1)           cke;
   (* prefix = "", result = "DM" *)
   method    Bit#(8)           dm;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(64))  dq;
   (* prefix = "DQS" *)
   interface Inout#(Bit#(8))   dqs;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(8))   dqs_n;
endinterface

(* always_ready, always_enabled *)
interface DDR2_Memory_Ifc;
   method    Action            clk_p(Bit#(2) i);
   method    Action            clk_n(Bit#(2) i);
   method    Action            cke(Bit#(1) i);
   method    Action            cs_n(Bit#(1) i);
   method    Action            ras_n(Bit#(1) i);
   method    Action            cas_n(Bit#(1) i);
   method    Action            we_n(Bit#(1) i);
   method    Action            dm(Bit#(8) i);
   method    Action            ba(Bit#(2) i);
   method    Action            addr(Bit#(13) i);
   method    Action            odt(Bit#(1) i);
   interface Inout#(Bit#(64))  dq;
   interface Inout#(Bit#(8))   dqs;
   interface Inout#(Bit#(8))   dqs_n;
endinterface

interface DDR2_User;
   interface Clock                   clock;
   interface Reset                   reset_n;
   method    Bool                    init_done();
   method    Action                  put(Bit#(32) writeen, Bit#(31) address, Bit#(256) datain);
   method    ActionValue#(Bit#(256)) read();
endinterface

interface DDR2_Controller;
   (* prefix = "" *)
   interface DDR2_Pins ddr2;
   (* prefix = "" *)
   interface DDR2_User user;
endinterface

interface VDDR2_User;
   interface Clock       clock;
   interface Reset       reset_n;
   method    Bool        init_done();
   method    Action      address_phase(Bit#(3) command, Bit#(31) address);
   method    Action      data_phase(Bit#(16) mask, Bit#(128) data);
   method    Bit#(128)   read_phase();
endinterface

interface VDDR2_Controller;
   (* prefix = "" *)
   interface DDR2_Pins   ddr2;
   (* prefix = "" *)
   interface VDDR2_User  user;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of DDR2 Coregen BSV Wrapper
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr2_wrapper =
module vMkDDR2Controller#(DDR2_Configure cfg, Clock iodelay_refclk)(VDDR2_Controller);
   default_clock clk(sys_clk);
   default_reset rst(sys_rst_n);

   parameter SIM_ONLY      = pack(cfg.fast_train_sim_only);
   parameter CLK_PERIOD_PS = cfg.clk_period_in_ps;

   input_clock   (idly_clk_200) = iodelay_refclk;

   interface DDR2_Pins ddr2;
      ifc_inout dq(ddr2_dq)             clocked_by(no_clock) reset_by(no_reset);
      ifc_inout dqs(ddr2_dqs)           clocked_by(no_clock) reset_by(no_reset);
      ifc_inout dqs_n(ddr2_dqs_n)       clocked_by(no_clock) reset_by(no_reset);

      method    ddr2_ck       clk_p     clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_ck_n     clk_n     clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_a        a         clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_ba       ba        clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_ras_n    ras_n     clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_cas_n    cas_n     clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_we_n     we_n      clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_cs_n     cs_n      clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_odt      odt       clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_cke      cke       clocked_by(no_clock) reset_by(no_reset);
      method    ddr2_dm       dm        clocked_by(no_clock) reset_by(no_reset);
   endinterface

   interface VDDR2_User user;
      output_clock  clock(clk0_tb);
      output_reset  reset_n(rst0n_tb) clocked_by(user_clock);
      method    init_done       init_done                                 clocked_by(no_clock) reset_by(no_reset);

      method                    address_phase(app_af_cmd, app_af_addr)  enable(app_af_enable) ready(app_af_ready) clocked_by(user_clock) reset_by(user_reset_n);
      method                    data_phase(app_wdf_mask, app_wdf_data)  enable(app_wdf_enable) ready(app_wdf_ready) clocked_by(user_clock) reset_by(user_reset_n);
      method    app_rd_data     read_phase() ready(app_rd_ready) clocked_by(user_clock) reset_by(user_reset_n);
   endinterface

   // Mark DDR2 pins are conflict free since BSV doesn't use them.
   schedule
   (
    ddr2_clk_p, ddr2_clk_n, ddr2_a, ddr2_ba, ddr2_ras_n, ddr2_cas_n, ddr2_we_n,
    ddr2_cs_n, ddr2_odt, ddr2_cke, ddr2_dm, user_init_done
   )
   CF
   (
    ddr2_clk_p, ddr2_clk_n, ddr2_a, ddr2_ba, ddr2_ras_n, ddr2_cas_n, ddr2_we_n,
    ddr2_cs_n, ddr2_odt, ddr2_cke, ddr2_dm, user_init_done
   );

   // User Interface Schedule
   schedule (user_address_phase) C (user_address_phase);
   schedule (user_data_phase) C (user_data_phase);
   schedule (user_read_phase) CF (user_address_phase, user_data_phase, user_read_phase);
   schedule (user_address_phase) CF (user_data_phase);
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of BSV Wrapper for DDR2 controller
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkDDR2Controller#(DDR2_Configure cfg, Clock iodelay_refclk)(DDR2_Controller);

   if (cfg.num_reads_in_flight < 1)
      error("The number of reads in flight has to be at least 1");
   if (cfg.num_reads_in_flight > 127)
      error("The arbitrary limit on the number of reads in flight is set at 126.  If you would like more, then you will need to update the Counter in this module to be wider than 8-bits.");
   if (cfg.clk_period_in_ps > 8000)
      error("The minimum frequency of the Xilinx DDR2 controller is 125 MHz");

   Integer reads = cfg.num_reads_in_flight;
   Integer beats = reads * 2;

   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Reset
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     clk                 <- exposeCurrentClock;
   Reset                                     rst_n               <- exposeCurrentReset;

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR2_Controller                          ddr2ctrl            <- vMkDDR2Controller(cfg, iodelay_refclk);
   Clock                                     user_clock           = ddr2ctrl.user.clock;
   Reset                                     user_reset_n         = ddr2ctrl.user.reset_n;

   FIFO#(DDR2Request)                        fifoRequest         <- mkFIFO(clocked_by user_clock, reset_by user_reset_n);
   FIFO#(DDR2Response)                       fifoResponse        <- mkFIFO(clocked_by user_clock, reset_by user_reset_n);

   Reg#(Bool)                                rDeqWriteReq        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rEnqReadResp        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(128))                           rFirstResponse      <- mkRegU(clocked_by user_clock, reset_by user_reset_n);

   Count#(Bit#(8))                           rReadsPending       <- mkCount(0, clocked_by user_clock, reset_by user_reset_n);

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule ready(ddr2ctrl.user.init_done);
      rule process_write_request_first((fifoRequest.first.byteen != 0) && !rDeqWriteReq);
	 rDeqWriteReq <= True;
	 ddr2ctrl.user.address_phase('b000, fifoRequest.first.address);
	 ddr2ctrl.user.data_phase(~truncate(fifoRequest.first.byteen), truncate(fifoRequest.first.data));
      endrule

      rule process_write_request_second((fifoRequest.first.byteen != 0) && rDeqWriteReq);
	 fifoRequest.deq;
	 ddr2ctrl.user.data_phase(~truncateLSB(fifoRequest.first.byteen), truncateLSB(fifoRequest.first.data));
	 rDeqWriteReq <= False;
      endrule

      rule process_read_request(fifoRequest.first.byteen == 0);
	 fifoRequest.deq;
	 ddr2ctrl.user.address_phase('b001, fifoRequest.first.address);
	 rReadsPending.incr(2);
      endrule

      rule process_read_response_first(!rEnqReadResp);
	 rFirstResponse <= ddr2ctrl.user.read_phase();
	 rEnqReadResp   <= True;
	 rReadsPending.decr(1);
      endrule

      rule process_read_response_second(rEnqReadResp);
	 fifoResponse.enq(unpack({ ddr2ctrl.user.read_phase(), rFirstResponse }));
	 rEnqReadResp   <= False;
	 rReadsPending.decr(1);
      endrule
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface DDR2_Pins ddr2 = ddr2ctrl.ddr2;
   interface DDR2_User user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr2ctrl.user.init_done;
      method Action put(writeen, address, datain);
	 let req = DDR2Request {
	    byteen:  writeen,
	    address: pack(address),
	    data:    datain
	    };
	 fifoRequest.enq(req);
      endmethod
      method ActionValue#(Bit#(256)) read();
	 fifoResponse.deq;
	 return fifoResponse.first.data;
      endmethod
   endinterface
endmodule


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" xilinx_ddr2_sodimm =
module vMkDDR2Memory(DDR2_Memory_Ifc);
   default_clock clk();
   default_reset rst();

   method clk_p(CLK_P) enable((*inhigh*)en0);
   method clk_n(CLK_N) enable((*inhigh*)en1);
   method cke(CKE) enable((*inhigh*)en2);
   method cs_n(CS_N) enable((*inhigh*)en3);
   method ras_n(RAS_N) enable((*inhigh*)en4);
   method cas_n(CAS_N) enable((*inhigh*)en5);
   method we_n(WE_N) enable((*inhigh*)en6);
   method dm(DM) enable((*inhigh*)en7);
   method ba(BA) enable((*inhigh*)en8);
   method addr(ADDR) enable((*inhigh*)en9);
   method odt(ODT) enable((*inhigh*)en10);

   ifc_inout dq(DQ)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs(DQS)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs_n(DQS_N)       clocked_by(no_clock) reset_by(no_reset);

   schedule (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, addr, odt) CF
            (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, addr, odt);

endmodule

module mkDDR2Memory(DDR2_Memory_Ifc);
   let _m <- vMkDDR2Memory;
   return _m;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
instance Connectable#(DDR2_Memory_Ifc, DDR2_Pins);
   module mkConnection#(DDR2_Memory_Ifc m, DDR2_Pins c)(Empty);
      rule connect;
         m.clk_p(c.clk_p);
         m.clk_n(c.clk_n);
         m.cke(c.cke);
         m.cs_n(c.cs_n);
         m.ras_n(c.ras_n);
         m.cas_n(c.cas_n);
         m.we_n(c.we_n);
         m.dm(c.dm);
         m.ba(c.ba);
         m.addr(c.a);
         m.odt(c.odt);
      endrule

      mkConnection(m.dq, c.dq);
      mkConnection(m.dqs, c.dqs);
      mkConnection(m.dqs_n, c.dqs_n);
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
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
      FIFOF#(Tuple3#(Bit#(32), Bit#(31), Bit#(256))) oddReq <- mkFIFOF1(clocked_by dClock, reset_by dReset);
      
      rule connect_requests if (oddReq.notFull);
         let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(31) address = cExtend(request.address);
	 
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
		    oddReq.enq(tuple3(((request.write) ? 32'hFFFFFFFF : 0), address + 31'h20, cExtendLSB(request.data)));
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

endpackage: XilinxVirtex5DDR2

