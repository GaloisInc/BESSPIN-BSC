////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtex7DDR2.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtex7DDR2;

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
export DDR2_Pins_V7(..);
export DDR2_User_V7(..);
export DDR2_Controller_V7(..);
export DDR2_Configure_V7(..);

export mkDDR2ControllerV7;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bit#(64)    byteen;
   Bit#(28)    address;
   Bit#(512)   data;
} DDR2RequestV7 deriving (Bits, Eq);

typedef struct {
   Bit#(512)   data;
} DDR2ResponseV7 deriving (Bits, Eq);

typedef struct {
   Bool               fast_train_sim_only;
   Integer            num_reads_in_flight;
} DDR2_Configure_V7;

instance DefaultValue#(DDR2_Configure_V7);
   defaultValue = DDR2_Configure_V7 {
      fast_train_sim_only:  False,
      num_reads_in_flight:  1
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface DDR2_Pins_V7;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(2)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(2)           clk_n;
   (* prefix = "", result = "A" *)
   method    Bit#(14)          a;
   (* prefix = "", result = "BA" *)
   method    Bit#(3)           ba;
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
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(8))   dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(8))   dqs_n;
endinterface

interface DDR2_User_V7;
   interface Clock                   clock;
   interface Reset                   reset_n;
   method    Bool                    init_done();
   method    Action                  put(Bit#(64) writeen, Bit#(28) address, Bit#(512) datain);
   method    ActionValue#(Bit#(512)) read();
endinterface

interface DDR2_Controller_V7;
   (* prefix = "" *)
   interface DDR2_Pins_V7 ddr2;
   (* prefix = "" *)
   interface DDR2_User_V7 user;
endinterface

interface VDDR2_User_V7;
   interface Clock       clock;
   interface Reset       reset_n;
   method    Bool        init_done();
   method    Action      app_addr(Bit#(28) i);
   method    Action      app_cmd(Bit#(3) i);
   method    Action      app_en(Bool i);
   method    Bool        app_rdy();
   method    Action      app_wdf_data(Bit#(256) i);
   method    Action      app_wdf_end(Bool i);
   method    Action      app_wdf_mask(Bit#(32) i);
   method    Action      app_wdf_wren(Bool i);
   method    Bool        app_wdf_rdy();
   method    Bit#(256)   app_rd_data();
   method    Bool        app_rd_data_end();
   method    Bool        app_rd_data_valid();
endinterface

interface VDDR2_Controller_V7;
   (* prefix = "" *)
   interface DDR2_Pins_V7   ddr2;
   (* prefix = "" *)
   interface VDDR2_User_V7  user;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of DDR2 Coregen BSV Wrapper
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr2_wrapper =
module vMkDDR2ControllerV7#(DDR2_Configure_V7 params)(VDDR2_Controller_V7);
   default_clock clk(sys_clk);
   default_reset rst(sys_rst_n);
   
   parameter SIM_ONLY      = pack(params.fast_train_sim_only);
   
   interface DDR2_Pins_V7 ddr2;
      ifc_inout dq(ddr2_dq)              clocked_by(no_clock) reset_by(no_reset);
      ifc_inout dqs_p(ddr2_dqs_p)        clocked_by(no_clock) reset_by(no_reset);
      ifc_inout dqs_n(ddr2_dqs_n)        clocked_by(no_clock) reset_by(no_reset);
      method ddr2_ck_p     clk_p         clocked_by(no_clock) reset_by(no_reset);
      method ddr2_ck_n     clk_n         clocked_by(no_clock) reset_by(no_reset);
      method ddr2_a        a             clocked_by(no_clock) reset_by(no_reset);
      method ddr2_ba       ba            clocked_by(no_clock) reset_by(no_reset);
      method ddr2_ras_n    ras_n         clocked_by(no_clock) reset_by(no_reset);
      method ddr2_cas_n    cas_n         clocked_by(no_clock) reset_by(no_reset);
      method ddr2_we_n     we_n          clocked_by(no_clock) reset_by(no_reset);
      method ddr2_cs_n     cs_n          clocked_by(no_clock) reset_by(no_reset);
      method ddr2_odt      odt           clocked_by(no_clock) reset_by(no_reset);
      method ddr2_cke      cke           clocked_by(no_clock) reset_by(no_reset);
      method ddr2_dm       dm            clocked_by(no_clock) reset_by(no_reset);
   endinterface
   
   interface VDDR2_User_V7 user;
      output_clock  clock(clk0_tb);
      output_reset  reset_n(rst0n_tb) clocked_by(user_clock);
      method init_done     init_done     clocked_by(no_clock) reset_by(no_reset);
      method               app_addr(app_addr)         enable((*inhigh*)en1) clocked_by(user_clock) reset_by(user_reset_n);
      method               app_cmd(app_cmd)           enable((*inhigh*)en2) clocked_by(user_clock) reset_by(user_reset_n);
      method               app_en(app_en)             enable((*inhigh*)en3) clocked_by(user_clock) reset_by(user_reset_n);
      method app_rdy       app_rdy                                          clocked_by(user_clock) reset_by(user_reset_n);
      method               app_wdf_data(app_wdf_data) enable((*inhigh*)en4) clocked_by(user_clock) reset_by(user_reset_n);
      method               app_wdf_end(app_wdf_end)   enable((*inhigh*)en5) clocked_by(user_clock) reset_by(user_reset_n);
      method               app_wdf_mask(app_wdf_mask) enable((*inhigh*)en6) clocked_by(user_clock) reset_by(user_reset_n);
      method               app_wdf_wren(app_wdf_wren) enable((*inhigh*)en7) clocked_by(user_clock) reset_by(user_reset_n);
      method app_wdf_rdy   app_wdf_rdy                                      clocked_by(user_clock) reset_by(user_reset_n);
      method app_rd_data   app_rd_data                                      clocked_by(user_clock) reset_by(user_reset_n);
      method app_rd_data_end app_rd_data_end                                clocked_by(user_clock) reset_by(user_reset_n);
      method app_rd_data_valid app_rd_data_valid                            clocked_by(user_clock) reset_by(user_reset_n);
   endinterface

   schedule
   (
    ddr2_clk_p, ddr2_clk_n, ddr2_a, ddr2_ba, ddr2_ras_n, ddr2_cas_n, ddr2_we_n,
    ddr2_cs_n, ddr2_odt, ddr2_cke, ddr2_dm, user_init_done,
    user_app_addr, user_app_cmd, user_app_en, user_app_rdy, user_app_wdf_data, 
    user_app_wdf_end, user_app_wdf_mask, user_app_wdf_rdy, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_wdf_wren
   )
   CF
   (
    ddr2_clk_p, ddr2_clk_n, ddr2_a, ddr2_ba, ddr2_ras_n, ddr2_cas_n, ddr2_we_n,
    ddr2_cs_n, ddr2_odt, ddr2_cke, ddr2_dm, user_init_done,
    user_app_addr, user_app_cmd, user_app_en, user_app_rdy, user_app_wdf_data, 
    user_app_wdf_end, user_app_wdf_mask, user_app_wdf_rdy, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_wdf_wren
   );

endmodule


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of BSV Wrapper for DDR2 controller
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkDDR2ControllerV7#(DDR2_Configure_V7 cfg)(DDR2_Controller_V7);

   if (cfg.num_reads_in_flight < 1)
      error("The number of reads in flight has to be at least 1");
   if (cfg.num_reads_in_flight > 127)
      error("The arbitrary limit on the number of reads in flight is set at 126.  If you would like more, then you will need to update the Counter in this module to be wider than 8-bits.");
   
   Integer reads = cfg.num_reads_in_flight;
   Integer beats = reads * 2;

   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     clk                 <- exposeCurrentClock;
   Reset                                     rst_n               <- exposeCurrentReset;

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR2_Controller_V7                       ddr2ctrl            <- vMkDDR2ControllerV7(cfg);
   Clock                                     user_clock           = ddr2ctrl.user.clock;
   Reset                                     user_reset_n         = ddr2ctrl.user.reset_n;
   
   FIFO#(DDR2RequestV7)                      fRequest            <- mkFIFO(clocked_by user_clock, reset_by user_reset_n);
   FIFO#(DDR2ResponseV7)                     fResponse           <- mkSizedFIFO(reads, clocked_by user_clock, reset_by user_reset_n);
   
   Count#(UInt#(8))                          rReadsInFlight      <- mkCount(0, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rWrite2ndPhase      <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rRead2ndPhase       <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(256))                           rSaved1stPhase      <- mkReg(0, clocked_by user_clock, reset_by user_reset_n);
   
   Wire#(Bool)                               wAppEn              <- mkDWire(False, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bool)                               wAppWdfEnd          <- mkDWire(False, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bool)                               wAppWdfWren         <- mkDWire(False, clocked_by user_clock, reset_by user_reset_n);
   
   Wire#(Bit#(3))                            wAppCmd             <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(28))                           wAppAddr            <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(32))                           wAppWdfMask         <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(256))                          wAppWdfData         <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
      
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_control_signals;
      ddr2ctrl.user.app_en(wAppEn);
      ddr2ctrl.user.app_wdf_end(wAppWdfEnd);
      ddr2ctrl.user.app_wdf_wren(wAppWdfWren);
   endrule
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_data_signals;
      ddr2ctrl.user.app_cmd(wAppCmd);
      ddr2ctrl.user.app_addr(wAppAddr);
      ddr2ctrl.user.app_wdf_data(wAppWdfData);
      ddr2ctrl.user.app_wdf_mask(wAppWdfMask);
   endrule

   rule process_next_read_request if (fRequest.first.byteen == 0 && ddr2ctrl.user.app_rdy() && rReadsInFlight <= fromInteger(beats));
      let request <- toGet(fRequest).get;
      wAppAddr <= request.address;
      wAppCmd  <= 1;
      wAppEn <= True;
      rReadsInFlight.incr(2);
   endrule
   
   rule process_next_write_request_1st if (fRequest.first.byteen != 0 && ddr2ctrl.user.app_rdy() && ddr2ctrl.user.app_wdf_rdy() && !rWrite2ndPhase);
      let request = fRequest.first;
      wAppAddr <= request.address;
      wAppCmd  <= 0;
      wAppEn   <= True;
      wAppWdfData <= truncate(request.data);
      wAppWdfMask <= ~truncate(request.byteen);
      wAppWdfEnd  <= False;
      wAppWdfWren <= True;
      rWrite2ndPhase <= True;
   endrule
   
   rule process_next_write_request_2nd if (fRequest.first.byteen != 0 && ddr2ctrl.user.app_wdf_rdy() && rWrite2ndPhase);
      let request <- toGet(fRequest).get;
      wAppWdfData <= truncateLSB(request.data);
      wAppWdfMask <= ~truncateLSB(request.byteen);
      wAppWdfEnd <= True;
      wAppWdfWren <= True;
      rWrite2ndPhase <= False;
   endrule
   
   rule process_read_response_1st if (!rRead2ndPhase && ddr2ctrl.user.app_rd_data_valid() && rReadsInFlight > 0);
      rRead2ndPhase <= True;
      rSaved1stPhase <= ddr2ctrl.user.app_rd_data();
      rReadsInFlight.decr(1);
   endrule
   
   rule process_read_response_2nd if (rRead2ndPhase && ddr2ctrl.user.app_rd_data_valid() && rReadsInFlight > 0);
      rRead2ndPhase <= False;
      rReadsInFlight.decr(1);
      let response = DDR2ResponseV7 { data: { ddr2ctrl.user.app_rd_data(), rSaved1stPhase } };
      fResponse.enq(response);
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface DDR2_Pins_V7 ddr2 = ddr2ctrl.ddr2;
   interface DDR2_User_V7 user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr2ctrl.user.init_done;
      method Action put(writeen, address, datain) if (ddr2ctrl.user.init_done);
	 fRequest.enq(DDR2RequestV7 { byteen: writeen, address: address, data: datain });
      endmethod
      method ActionValue#(Bit#(512)) read();
	 fResponse.deq;
	 return fResponse.first.data;
      endmethod
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
instance Connectable#(MemoryClient#(a, d), DDR2_User_V7)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR2_User_V7 ddr2)(Empty);
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
      
      rule connect_requests;
         let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(28) address = 0;
	 Bit#(64) enables = cExtend(request.byteen);
	 Bit#(512) datain = duplicate(request.data);

	 case(sz)
	    64:  address = cExtend(request.address);
	    128: address = cExtend(request.address) << 1;
	    256: address = cExtend(request.address) << 2;
	    512: address = cExtend(request.address) << 3;
	    default: error("DDR2 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 ddr2.put((request.write) ? enables : 0, address, datain);
      endrule

      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);

      rule connect_response;
         let response <- ddr2.read;
         respFIFO.enq(cExtend(response));
      endrule
   endmodule
endinstance

endpackage: XilinxVirtex7DDR2

