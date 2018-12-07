////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxKintex7DDR3.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxKintex7DDR3;

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
import DefaultValue      ::*;
import Counter           ::*;
import CommitIfc         ::*;
import Memory            ::*;
import ClientServer      ::*;
import GetPut            ::*;
import BUtils            ::*;
import StmtFSM           ::*;
import I2C               ::*;

import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export DDR3_Pins_K7(..);
export DDR3_User_K7(..);
export DDR3_Controller_K7(..);
export DDR3_Configure_K7(..);

export mkKintex7DDR3Controller;

export DDR3_Pins_K7_uDIMM(..);
export DDR3_User_K7_uDIMM(..);
export DDR3_Controller_K7_uDIMM(..);
export DDR3_uDIMM_Memory_Ifc(..);

export mkKintex7DDR3uDIMMController;
export mkDDR3uDIMMMemory;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bit#(64)    byteen;
   Bit#(28)    address;
   Bit#(512)   data;
} DDR3Request deriving (Bits, Eq);

typedef struct {
   Bit#(72)    byteen;
   Bit#(30)    address;
   Bit#(576)   data;
} DDR3RequestUD deriving (Bits, Eq);		

typedef struct {
   Bit#(512)   data;
} DDR3Response deriving (Bits, Eq);	       

typedef struct {
   Bit#(576)  data;
} DDR3ResponseUD deriving (Bits, Eq);		

typedef enum {
   WRITE     = 0,
   READ      = 1
} DDR3Command deriving (Eq);

instance Bits#(DDR3Command, 3);
   function Bit#(3) pack(DDR3Command x);
      case(x) 
	 WRITE:   return 0;
	 READ:    return 1;
      endcase
   endfunction
   
   function DDR3Command unpack(Bit#(3) x);
      DDR3Command cmd;
      case(x)
	 0:       cmd = WRITE;
	 1:       cmd = READ;
	 default: cmd = READ;
      endcase
      return cmd;
   endfunction
endinstance

typedef struct {
   Bool               fast_train_sim_only;
   Integer            num_reads_in_flight;
} DDR3_Configure_K7;

instance DefaultValue#(DDR3_Configure_K7);
   defaultValue = DDR3_Configure_K7 {
      fast_train_sim_only:    False,
      num_reads_in_flight:    2
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface DDR3_Pins_K7;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(1)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(1)           clk_n;
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
   (* prefix = "", result = "RESET_N" *)
   method    Bit#(1)           reset_n;
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

interface DDR3_User_K7;
   interface Clock             	     clock;
   interface Reset             	     reset_n;
   method    Bool              	     init_done;
   method    Action                  request(Bit#(28) addr, Bit#(64) mask, Bit#(512) data);
   method    ActionValue#(Bit#(512)) read_data;
endinterface

interface DDR3_Controller_K7;
   (* prefix = "" *)
   interface DDR3_Pins_K7      ddr3;
   (* prefix = "" *)
   interface DDR3_User_K7      user;
endinterface

(* always_ready, always_enabled *)
interface VDDR3_User_K7;
   interface Clock             clock;
   interface Reset             reset;
   method    Bool              init_done;
   method    Action            app_addr(Bit#(28) i);
   method    Action            app_cmd(DDR3Command i);
   method    Action            app_en(Bool i);
   method    Action            app_wdf_data(Bit#(256) i);
   method    Action            app_wdf_end(Bool i);
   method    Action            app_wdf_mask(Bit#(32) i);
   method    Action            app_wdf_wren(Bool i);
   method    Bit#(256)         app_rd_data;
   method    Bool              app_rd_data_end;
   method    Bool              app_rd_data_valid;
   method    Bool              app_rdy;
   method    Bool              app_wdf_rdy;
endinterface

interface VDDR3_Controller_K7;
   (* prefix = "" *)
   interface DDR3_Pins_K7      ddr3;
   (* prefix = "" *)
   interface VDDR3_User_K7     user;
endinterface   

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr3_wrapper =
module vMkKintex7DDR3Controller#(DDR3_Configure_K7 cfg)(VDDR3_Controller_K7);
   default_clock clk(sys_clk_i);
   default_reset rst(sys_rst);
   
   parameter SIM_BYPASS_INIT_CAL = (cfg.fast_train_sim_only) ? "FAST" : "OFF";
   parameter SIMULATION          = (cfg.fast_train_sim_only) ? "TRUE" : "FALSE";
   
   interface DDR3_Pins_K7 ddr3;
      ifc_inout   dq(ddr3_dq)          clocked_by(no_clock)  reset_by(no_reset);
      ifc_inout   dqs_p(ddr3_dqs_p)    clocked_by(no_clock)  reset_by(no_reset);
      ifc_inout   dqs_n(ddr3_dqs_n)    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ck_p    clk_p   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ck_n    clk_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cke     cke     clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cs_n    cs_n    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ras_n   ras_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cas_n   cas_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_we_n    we_n    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_reset_n reset_n clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_dm      dm      clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ba      ba      clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_addr    a       clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_odt     odt     clocked_by(no_clock)  reset_by(no_reset);
   endinterface
   
   interface VDDR3_User_K7 user;
      output_clock    clock(ui_clk);
      output_reset    reset(ui_clk_sync_rst);
      method init_calib_complete      init_done    clocked_by(no_clock) reset_by(no_reset);
      method          		      app_addr(app_addr) enable((*inhigh*)en0) clocked_by(user_clock) reset_by(no_reset);
      method                          app_cmd(app_cmd)   enable((*inhigh*)en00) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_en(app_en)     enable((*inhigh*)en1) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_data(app_wdf_data) enable((*inhigh*)en2) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_end(app_wdf_end)   enable((*inhigh*)en3) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_mask(app_wdf_mask) enable((*inhigh*)en4) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_wren(app_wdf_wren) enable((*inhigh*)en5) clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data              app_rd_data clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data_end          app_rd_data_end clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data_valid        app_rd_data_valid clocked_by(user_clock) reset_by(no_reset);
      method app_rdy                  app_rdy clocked_by(user_clock) reset_by(no_reset);
      method app_wdf_rdy              app_wdf_rdy clocked_by(user_clock) reset_by(no_reset);
   endinterface
   
   schedule
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, 
    ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    )
   CF
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, 
    ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    );
   
   schedule 
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd
    )
   CF
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd
    );

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of BSV wrapper for DDR3 (Kintex7) controller
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkKintex7DDR3Controller#(DDR3_Configure_K7 cfg)(DDR3_Controller_K7);

   if (cfg.num_reads_in_flight < 1)
      error("The number of reads in flight has to be at least 1");

   Integer reads = cfg.num_reads_in_flight;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     clock               <- exposeCurrentClock;
   Reset                                     reset_n             <- exposeCurrentReset;
   
   Reset                                     dly_reset_n         <- mkAsyncResetLong( 40000, reset_n, clock );

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR3_Controller_K7                       ddr3ctrl            <- vMkKintex7DDR3Controller(cfg, reset_by dly_reset_n);
   Clock                                     user_clock           = ddr3ctrl.user.clock;
   Reset                                     user_reset0_n       <- mkResetInverter(ddr3ctrl.user.reset);
   Reset                                     user_reset_n        <- mkAsyncReset(2, user_reset0_n, user_clock);
   
   FIFO#(DDR3Request)                        fRequest            <- mkBypassFIFO(clocked_by user_clock, reset_by user_reset_n);
   FIFO#(DDR3Response)                       fResponse           <- mkBypassFIFO(clocked_by user_clock, reset_by user_reset_n);
   
   Counter#(32)                              rReadsPending       <- mkCounter(0, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rDeqWriteReq        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rEnqReadResp        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(256))                           rFirstResponse      <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   
   PulseWire                                 pwAppEn             <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   PulseWire                                 pwAppWdfWren        <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   PulseWire                                 pwAppWdfEnd         <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   
   Wire#(DDR3Command)                        wAppCmd             <- mkDWire(WRITE, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(28))                           wAppAddr            <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(32))                           wAppWdfMask         <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(256))                          wAppWdfData         <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
      
   Bool initialized      = ddr3ctrl.user.init_done;
   Bool ctrl_ready_req   = ddr3ctrl.user.app_rdy;
   Bool write_ready_req  = ddr3ctrl.user.app_wdf_rdy;
   Bool read_data_ready  = ddr3ctrl.user.app_rd_data_valid;

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_enables;
      ddr3ctrl.user.app_en(pwAppEn);
      ddr3ctrl.user.app_wdf_wren(pwAppWdfWren);
      ddr3ctrl.user.app_wdf_end(pwAppWdfEnd);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_data_signals;
      ddr3ctrl.user.app_cmd(wAppCmd);
      ddr3ctrl.user.app_addr(wAppAddr);
      ddr3ctrl.user.app_wdf_data(wAppWdfData);
      ddr3ctrl.user.app_wdf_mask(wAppWdfMask);
   endrule
   
   rule ready(initialized);
      rule process_write_request_first((fRequest.first.byteen != 0) && !rDeqWriteReq && ctrl_ready_req && write_ready_req);
	 rDeqWriteReq <= True;
	 wAppCmd      <= WRITE;
	 wAppAddr     <= fRequest.first.address;
	 pwAppEn.send;
	 wAppWdfData  <= truncate(fRequest.first.data);
	 wAppWdfMask  <= ~truncate(fRequest.first.byteen);
	 pwAppWdfWren.send;
      endrule
      
      rule process_write_request_second((fRequest.first.byteen != 0) && rDeqWriteReq && write_ready_req);
	 fRequest.deq;
	 rDeqWriteReq <= False;
	 wAppWdfData  <= truncateLSB(fRequest.first.data);
	 wAppWdfMask  <= ~truncateLSB(fRequest.first.byteen);
	 pwAppWdfWren.send;
	 pwAppWdfEnd.send;
      endrule
      
      rule process_read_request(fRequest.first.byteen == 0 && ctrl_ready_req);
	 fRequest.deq;
	 wAppCmd  <= READ;
	 wAppAddr <= fRequest.first.address;
	 pwAppEn.send;
	 rReadsPending.inc(2);
      endrule
      
      rule process_read_response_first(!rEnqReadResp && read_data_ready);
	 rFirstResponse <= ddr3ctrl.user.app_rd_data;
	 rEnqReadResp   <= True;
	 rReadsPending.down;
      endrule
      
      rule process_read_response_second(rEnqReadResp && read_data_ready);
	 fResponse.enq(unpack({ ddr3ctrl.user.app_rd_data, rFirstResponse }));
	 rEnqReadResp   <= False;
	 rReadsPending.down;
      endrule
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface ddr3 = ddr3ctrl.ddr3;
   interface DDR3_User_K7 user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr3ctrl.user.init_done;

      method Action request(Bit#(28) addr, Bit#(64) mask, Bit#(512) data);
	 let req = DDR3Request { byteen: mask, address: addr, data: data };
	 fRequest.enq(req);
      endmethod
	 
      method ActionValue#(Bit#(512)) read_data;
	 fResponse.deq;
	 return fResponse.first.data;
      endmethod
   endinterface
endmodule

module mkAsyncResetLong#(Integer cycles, Reset rst_in, Clock clk_out)(Reset);
   Reg#(UInt#(32)) count <- mkReg(fromInteger(cycles), clocked_by clk_out, reset_by rst_in);
   let rstifc <- mkReset(0, True, clk_out);
   
   rule count_down if (count > 0);
      count <= count - 1;
      rstifc.assertReset();
   endrule
   
   return rstifc.new_rst;
endmodule


instance Connectable#(MemoryClient#(a, d), DDR3_User_K7)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR3_User_K7 ddr3)(Empty);
      let sz = valueOf(d);
   
      Clock uClock              <- exposeCurrentClock;
      Reset uReset              <- exposeCurrentReset;
      Clock dClock              = ddr3.clock;
      Reset dReset              = ddr3.reset_n;
      
      Reset uRst_dclk           <- mkAsyncReset( 2, uReset, dClock );
      Reset jointReset1         <- mkResetEither( dReset, uRst_dclk, clocked_by dClock );
      Reset jointReset          <- mkAsyncReset( 2, jointReset1, dClock );
      
      SyncFIFOIfc#(MemoryRequest#(a,d)) reqFIFO <- mkSyncFIFO(1, uClock, uReset, dClock);
      let connectReqUser <- mkConnection(client.request, toPut(reqFIFO), clocked_by uClock, reset_by uReset);
      
      rule connect_requests;
	 let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(28)  address = 0;
	 Bit#(64)  enables = 0;
	 Bit#(512) datain = duplicate(request.data);
	 case(sz)
	    64:  begin
		    address = cExtend(request.address);
		    case(request.address[2:0])
		       0: enables = cExtend(request.byteen);
		       1: enables = cExtend(request.byteen) << 8;
		       2: enables = cExtend(request.byteen) << 16;
		       3: enables = cExtend(request.byteen) << 24;
		       4: enables = cExtend(request.byteen) << 32;
		       5: enables = cExtend(request.byteen) << 40;
		       6: enables = cExtend(request.byteen) << 48;
		       7: enables = cExtend(request.byteen) << 56;
		    endcase
		 end
	    128: begin
		    address = cExtend(request.address) << 1;
		    case(request.address[1:0])
		       0: enables = cExtend(request.byteen);
		       1: enables = cExtend(request.byteen) << 16;
		       2: enables = cExtend(request.byteen) << 32;
		       3: enables = cExtend(request.byteen) << 48;
		    endcase
		 end
	    256: begin
		    address = cExtend(request.address) << 2;
		    case(request.address[0])
		       0: enables = cExtend(request.byteen);
		       1: enables = cExtend(request.byteen) << 32;
		    endcase
		 end
	    512: begin
		    address = cExtend(request.address) << 3;
		    enables = cExtend(request.byteen);
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 ddr3.request(address, (request.write) ? enables : 0, datain);
      endrule

      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);
      
      rule connect_responses;
	 let response <- ddr3.read_data;
	 respFIFO.enq(cExtend(response));
      endrule
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface VDDR3_Pins_K7_uDIMM;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(1)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(1)           clk_n;
   (* prefix = "", result = "A" *)
   method    Bit#(16)          a;
   (* prefix = "", result = "BA" *)
   method    Bit#(3)           ba;
   (* prefix = "", result = "RAS_N" *)
   method    Bit#(1)           ras_n;
   (* prefix = "", result = "CAS_N" *)
   method    Bit#(1)           cas_n;
   (* prefix = "", result = "WE_N" *)
   method    Bit#(1)           we_n;
   (* prefix = "", result = "RESET_N" *)
   method    Bit#(1)           reset_n;
   (* prefix = "", result = "CS_N" *)
   method    Bit#(1)           cs_n;
   (* prefix = "", result = "ODT" *)
   method    Bit#(1)           odt;
   (* prefix = "", result = "CKE" *)
   method    Bit#(1)           cke;
   (* prefix = "", result = "DM" *)
   method    Bit#(9)           dm;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(72))  dq;
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(9))   dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(9))   dqs_n;
endinterface   

(* always_enabled, always_ready *)
interface DDR3_Pins_K7_uDIMM;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(1)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(1)           clk_n;
   (* prefix = "", result = "A" *)
   method    Bit#(16)          a;
   (* prefix = "", result = "BA" *)
   method    Bit#(3)           ba;
   (* prefix = "", result = "RAS_N" *)
   method    Bit#(1)           ras_n;
   (* prefix = "", result = "CAS_N" *)
   method    Bit#(1)           cas_n;
   (* prefix = "", result = "WE_N" *)
   method    Bit#(1)           we_n;
   (* prefix = "", result = "RESET_N" *)
   method    Bit#(1)           reset_n;
   (* prefix = "", result = "CS_N" *)
   method    Bit#(1)           cs_n;
   (* prefix = "", result = "ODT" *)
   method    Bit#(1)           odt;
   (* prefix = "", result = "CKE" *)
   method    Bit#(1)           cke;
   (* prefix = "", result = "DM" *)
   method    Bit#(9)           dm;
   (* prefix = "DQ" *)
   interface Inout#(Bit#(72))  dq;
   (* prefix = "DQS_P" *)
   interface Inout#(Bit#(9))   dqs_p;
   (* prefix = "DQS_N" *)
   interface Inout#(Bit#(9))   dqs_n;
   (* prefix = "SCL" *)
   interface Inout#(Bit#(1))   scl;
   (* prefix = "SDA" *)
   interface Inout#(Bit#(1))   sda;
endinterface   

interface DDR3_User_K7_uDIMM;
   interface Clock             	     clock;
   interface Reset             	     reset_n;
   method    Bool              	     init_done;
   method    Action                  request(Bit#(30) addr, Bit#(72) mask, Bit#(576) data);
   method    ActionValue#(Bit#(576)) read_data;
endinterface

interface DDR3_Controller_K7_uDIMM;
   (* prefix = "" *)
   interface DDR3_Pins_K7_uDIMM      ddr3;
   (* prefix = "" *)
   interface DDR3_User_K7_uDIMM      user;
endinterface

(* always_ready, always_enabled *)
interface VDDR3_User_K7_uDIMM;
   interface Clock             clock;
   interface Reset             reset;
   method    Bool              init_done;
   method    Action            app_addr(Bit#(30) i);
   method    Action            app_cmd(DDR3Command i);
   method    Action            app_en(Bool i);
   method    Action            app_wdf_data(Bit#(576) i);
   method    Action            app_wdf_end(Bool i);
   method    Action            app_wdf_mask(Bit#(72) i);
   method    Action            app_wdf_wren(Bool i);
   method    Bit#(576)         app_rd_data;
   method    Bool              app_rd_data_end;
   method    Bool              app_rd_data_valid;
   method    Bool              app_rdy;
   method    Bool              app_wdf_rdy;
endinterface

interface VDDR3_Controller_K7_uDIMM;
   (* prefix = "" *)
   interface VDDR3_Pins_K7_uDIMM     ddr3;
   (* prefix = "" *)
   interface VDDR3_User_K7_uDIMM     user;
endinterface   

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr3_wrapper =
module vMkKintex7DDR3uDIMMController#(DDR3_Configure_K7 cfg, Clock refclk)(VDDR3_Controller_K7_uDIMM);
   default_clock clk(sys_clk_i);
   default_reset rst(sys_rst);
   
   input_clock refclk(ref_clk_i) = refclk;
   
   parameter SIM_BYPASS_INIT_CAL = (cfg.fast_train_sim_only) ? "FAST" : "OFF";
   parameter SIMULATION          = (cfg.fast_train_sim_only) ? "TRUE" : "FALSE";
   
   interface VDDR3_Pins_K7_uDIMM ddr3;
      ifc_inout   dq(ddr3_dq)          clocked_by(no_clock)  reset_by(no_reset);
      ifc_inout   dqs_p(ddr3_dqs_p)    clocked_by(no_clock)  reset_by(no_reset);
      ifc_inout   dqs_n(ddr3_dqs_n)    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ck_p    clk_p   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ck_n    clk_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cke     cke     clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cs_n    cs_n    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ras_n   ras_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_cas_n   cas_n   clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_we_n    we_n    clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_reset_n reset_n clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_dm      dm      clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_ba      ba      clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_addr    a       clocked_by(no_clock)  reset_by(no_reset);
      method      ddr3_odt     odt     clocked_by(no_clock)  reset_by(no_reset);
   endinterface
   
   interface VDDR3_User_K7_uDIMM user;
      output_clock    clock(ui_clk);
      output_reset    reset(ui_clk_sync_rst);
      method init_calib_complete      init_done    clocked_by(no_clock) reset_by(no_reset);
      method          		      app_addr(app_addr) enable((*inhigh*)en0) clocked_by(user_clock) reset_by(no_reset);
      method                          app_cmd(app_cmd)   enable((*inhigh*)en00) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_en(app_en)     enable((*inhigh*)en1) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_data(app_wdf_data) enable((*inhigh*)en2) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_end(app_wdf_end)   enable((*inhigh*)en3) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_mask(app_wdf_mask) enable((*inhigh*)en4) clocked_by(user_clock) reset_by(no_reset);
      method          		      app_wdf_wren(app_wdf_wren) enable((*inhigh*)en5) clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data              app_rd_data clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data_end          app_rd_data_end clocked_by(user_clock) reset_by(no_reset);
      method app_rd_data_valid        app_rd_data_valid clocked_by(user_clock) reset_by(no_reset);
      method app_rdy                  app_rdy clocked_by(user_clock) reset_by(no_reset);
      method app_wdf_rdy              app_wdf_rdy clocked_by(user_clock) reset_by(no_reset);
   endinterface
   
   schedule
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, 
    ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    )
   CF
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, 
    ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    );
   
   schedule 
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd
    )
   CF
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data, 
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd
    );

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of BSV wrapper for DDR3 (Kintex7) controller
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkKintex7DDR3uDIMMController#(DDR3_Configure_K7 cfg, Clock refclk)(DDR3_Controller_K7_uDIMM);

   if (cfg.num_reads_in_flight < 1)
      error("The number of reads in flight has to be at least 1");

   Integer reads = cfg.num_reads_in_flight;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Clocks & Resets
   ////////////////////////////////////////////////////////////////////////////////
   Clock                                     clock               <- exposeCurrentClock;
   Reset                                     reset_n             <- exposeCurrentReset;
   
   Reset                                     dly_reset_n         <- mkAsyncResetLong( 40000, reset_n, clock );

   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR3_Controller_K7_uDIMM                 ddr3ctrl            <- vMkKintex7DDR3uDIMMController(cfg, refclk, reset_by dly_reset_n);
   Clock                                     user_clock           = ddr3ctrl.user.clock;
   Reset                                     user_reset0_n       <- mkResetInverter(ddr3ctrl.user.reset);
   Reset                                     user_reset_n        <- mkAsyncReset(2, user_reset0_n, user_clock);
   
   I2C                                       spdctrl             <- mkI2C(1024, clocked_by user_clock, reset_by user_reset_n);
   
   FIFO#(DDR3RequestUD)                      fRequest            <- mkBypassFIFO(clocked_by user_clock, reset_by user_reset_n);
   FIFO#(DDR3ResponseUD)                     fResponse           <- mkBypassFIFO(clocked_by user_clock, reset_by user_reset_n);
   
   Reg#(Bool)                                rSPDDone            <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(8))                             rSPDAddr            <- mkReg(0, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(8))                             rSPDData            <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   Counter#(32)                              rReadsPending       <- mkCounter(0, clocked_by user_clock, reset_by user_reset_n);

   PulseWire                                 pwAppEn             <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   PulseWire                                 pwAppWdfWren        <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   PulseWire                                 pwAppWdfEnd         <- mkPulseWire(clocked_by user_clock, reset_by user_reset_n);
   
   Wire#(DDR3Command)                        wAppCmd             <- mkDWire(WRITE, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(30))                           wAppAddr            <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(72))                           wAppWdfMask         <- mkDWire('1, clocked_by user_clock, reset_by user_reset_n);
   Wire#(Bit#(576))                          wAppWdfData         <- mkDWire(0, clocked_by user_clock, reset_by user_reset_n);
      
   Bool initialized      = ddr3ctrl.user.init_done && rSPDDone;
   Bool ctrl_ready_req   = ddr3ctrl.user.app_rdy;
   Bool write_ready_req  = ddr3ctrl.user.app_wdf_rdy;
   Bool read_data_ready  = ddr3ctrl.user.app_rd_data_valid;

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_enables;
      ddr3ctrl.user.app_en(pwAppEn);
      ddr3ctrl.user.app_wdf_wren(pwAppWdfWren);
      ddr3ctrl.user.app_wdf_end(pwAppWdfEnd);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule drive_data_signals;
      ddr3ctrl.user.app_cmd(wAppCmd);
      ddr3ctrl.user.app_addr(wAppAddr);
      ddr3ctrl.user.app_wdf_data(wAppWdfData);
      ddr3ctrl.user.app_wdf_mask(wAppWdfMask);
   endrule
   
   rule ready(initialized);
      rule process_write_request((fRequest.first.byteen != 0) && ctrl_ready_req && write_ready_req);
	 let request <- toGet(fRequest).get;
	 wAppCmd     <= WRITE;	
	 wAppAddr    <= request.address;
	 wAppWdfData <= request.data;
	 wAppWdfMask <= ~request.byteen;
	 pwAppEn.send;
	 pwAppWdfWren.send;
	 pwAppWdfEnd.send;
      endrule
      
      rule process_read_request((fRequest.first.byteen == 0) && ctrl_ready_req);
	 let request <- toGet(fRequest).get;
	 wAppCmd     <= READ;
	 wAppAddr    <= request.address;
	 pwAppEn.send;
	 rReadsPending.up;
      endrule
      
      rule process_read_response(read_data_ready);
	 fResponse.enq(unpack(ddr3ctrl.user.app_rd_data));
	 rReadsPending.down;
      endrule
   endrule
   
   Stmt spd_eeprom =
   seq
      for(rSPDAddr <= 0; rSPDAddr < 255; rSPDAddr <= rSPDAddr + 1) seq
	 spdctrl.user.request.put(I2CRequest { write: False, slaveaddr: 'h50, address: rSPDAddr, data: rSPDData });
	 action
	    let data <- spdctrl.user.response.get;
	    rSPDData <= pack(data);
	 endaction
      endseq
      rSPDDone <= True;
   endseq;
   
   FSM                                       fsmSPD              <- mkFSM(spd_eeprom, clocked_by user_clock, reset_by user_reset_n);
   
   rule start_i2c_spd(fsmSPD.done);
      fsmSPD.start;
   endrule
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface DDR3_Pins_K7_uDIMM ddr3;
      interface dq      = ddr3ctrl.ddr3.dq;
      interface dqs_p   = ddr3ctrl.ddr3.dqs_p;
      interface dqs_n   = ddr3ctrl.ddr3.dqs_n;
      interface clk_p   = ddr3ctrl.ddr3.clk_p;
      interface clk_n   = ddr3ctrl.ddr3.clk_n;
      method 	a       = ddr3ctrl.ddr3.a;
      method 	ba      = ddr3ctrl.ddr3.ba;
      method 	cas_n   = ddr3ctrl.ddr3.cas_n;
      method 	cke     = ddr3ctrl.ddr3.cke;
      method 	cs_n    = ddr3ctrl.ddr3.cs_n;
      method 	dm      = ddr3ctrl.ddr3.dm;
      method 	odt     = ddr3ctrl.ddr3.odt;
      method 	ras_n   = ddr3ctrl.ddr3.ras_n;
      method 	we_n    = ddr3ctrl.ddr3.we_n;
      method    reset_n = ddr3ctrl.ddr3.reset_n;
      interface	scl     = spdctrl.i2c.scl;
      interface sda     = spdctrl.i2c.sda;
   endinterface

   interface DDR3_User_K7_uDIMM user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr3ctrl.user.init_done;

      method Action request(Bit#(30) addr, Bit#(72) mask, Bit#(576) data);
	 let req = DDR3RequestUD { byteen: mask, address: addr, data: data };
	 fRequest.enq(req);
      endmethod
	 
      method ActionValue#(Bit#(576)) read_data;
	 fResponse.deq;
	 return fResponse.first.data;
      endmethod
   endinterface
endmodule

instance Connectable#(MemoryClient#(a, d), DDR3_User_K7_uDIMM)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR3_User_K7_uDIMM ddr3)(Empty);
      let sz = valueOf(d);
   
      Clock uClock              <- exposeCurrentClock;
      Reset uReset              <- exposeCurrentReset;
      Clock dClock              = ddr3.clock;
      Reset dReset              = ddr3.reset_n;
      
      Reset uRst_dclk           <- mkAsyncReset( 2, uReset, dClock );
      Reset jointReset1         <- mkResetEither( dReset, uRst_dclk, clocked_by dClock );
      Reset jointReset          <- mkAsyncReset( 2, jointReset1, dClock );
      
      SyncFIFOIfc#(MemoryRequest#(a,d)) reqFIFO <- mkSyncFIFO(1, uClock, uReset, dClock);
      let connectReqUser <- mkConnection(client.request, toPut(reqFIFO), clocked_by uClock, reset_by uReset);
      
      function Bit#(9) augmentEnables(Bit#(8) be);
	 return { 1'b1, be };
      endfunction
      
      rule connect_requests;
	 let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(30)  address = 0;
	 Bit#(72)  enables = 0;
	 Bit#(576) datain = 0;
	 case(sz)
	    64:  begin
		    address = cExtend(request.address);
		    Bit#(72)  d = cExtend(request.data);
		    datain = duplicate(d);
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
		    address = cExtend(request.address) << 1;
		    Bit#(144)  d = cExtend(request.data);
		    datain = duplicate(d);
		    case(request.address[1:0])
		       0: enables = 72'h00_00_00_00_00_00_03_FF_FF;
		       1: enables = 72'h00_00_00_00_0F_FF_FC_00_00;
		       2: enables = 72'h00_00_3F_FF_F0_00_00_00_00;
		       3: enables = 72'hFF_FF_C0_00_00_00_00_00_00;
		    endcase
		 end
	    256: begin
		    address = cExtend(request.address) << 2;
		    Bit#(288)  d = cExtend(request.data);
		    datain = duplicate(d);
		    case(request.address[0])
		       0: enables = 72'h00_00_00_00_0F_FF_FF_FF_FF;
		       1: enables = 72'hFF_FF_FF_FF_F0_00_00_00_00;
		    endcase
		 end
	    512: begin
		    address = cExtend(request.address) << 3;
		    datain = cExtend(request.data);
		    enables = 72'hFF_FF_FF_FF_FF_FF_FF_FF_FF;
		 end
	    default: error("DDR3 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 ddr3.request(address, (request.write) ? enables : 0, datain);
      endrule

      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);
      
      rule connect_responses;
	 let response <- ddr3.read_data;
	 respFIFO.enq(cExtend(response));
      endrule
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface DDR3_uDIMM_Memory_Ifc;
   method    Action            clk_p(Bit#(1) i);
   method    Action            clk_n(Bit#(1) i);
   method    Action            a(Bit#(16) i);
   method    Action            ba(Bit#(3) i);
   method    Action            ras_n(Bit#(1) i);
   method    Action            cas_n(Bit#(1) i);
   method    Action            we_n(Bit#(1) i);
   method    Action            reset_n(Bit#(1) i);
   method    Action            cs_n(Bit#(1) i);
   method    Action            odt(Bit#(1) i);
   method    Action            cke(Bit#(1) i);
   method    Action            dm(Bit#(9) i);
   interface Inout#(Bit#(72))  dq;
   interface Inout#(Bit#(9))   dqs_p;
   interface Inout#(Bit#(9))   dqs_n;
endinterface   

import "BVI" xilinx_ddr3_mini_udimm =
module vMkDDR3uDIMMMemory(DDR3_uDIMM_Memory_Ifc);
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
   method a(ADDR) enable((*inhigh*)en9);
   method odt(ODT) enable((*inhigh*)en10);
   method reset_n(RESET_N) enable((*inhigh*)en11);

   ifc_inout dq(DQ)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs_p(DQS_P)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs_n(DQS_N)       clocked_by(no_clock) reset_by(no_reset);

   schedule (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, a, odt, reset_n) CF
            (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, a, odt, reset_n);

endmodule

module mkDDR3uDIMMMemory(DDR3_uDIMM_Memory_Ifc);
   (* hide_all *)
   let _m <- vMkDDR3uDIMMMemory;
   return _m;
endmodule

instance Connectable#(DDR3_uDIMM_Memory_Ifc, DDR3_Pins_K7_uDIMM);
   module mkConnection#(DDR3_uDIMM_Memory_Ifc m, DDR3_Pins_K7_uDIMM c)(Empty);
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
         m.a(c.a);
         m.odt(c.odt);
      endrule
      
      mkConnection(m.dq, c.dq);
      mkConnection(m.dqs_p, c.dqs_p);
      mkConnection(m.dqs_n, c.dqs_n);
   endmodule
endinstance

instance Connectable#(DDR3_Pins_K7_uDIMM, DDR3_uDIMM_Memory_Ifc);
   module mkConnection#(DDR3_Pins_K7_uDIMM c, DDR3_uDIMM_Memory_Ifc m)(Empty);
      mkConnection(m, c);
   endmodule
endinstance


endpackage: XilinxKintex7DDR3

