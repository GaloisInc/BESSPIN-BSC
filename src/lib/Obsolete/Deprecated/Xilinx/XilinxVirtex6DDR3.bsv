////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxVirtex6DDR3.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxVirtex6DDR3;

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
import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export DDR3_Pins(..);
export DDR3_User(..);
export DDR3_Controller(..);
export DDR3_Memory_Ifc(..);
export DDR3_Configure(..);

export mkDDR3Controller;
export mkDDR3Memory;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bit#(64)  byteen;
   Bit#(27)  address;
   Bit#(512) data;
} DDR3Request deriving (Bits, Eq);

typedef struct {
   Bit#(512)          data;
} DDR3Response deriving (Bits, Eq);

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
   Integer            clk_period_in_ps;
   Integer            num_reads_in_flight;
} DDR3_Configure;

instance DefaultValue#(DDR3_Configure);
   defaultValue = DDR3_Configure {
        fast_train_sim_only:    False
      , clk_period_in_ps:       5000
      , num_reads_in_flight:    1
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////
(* always_enabled, always_ready *)
interface DDR3_Pins;
   (* prefix = "", result = "CLK_P" *)
   method    Bit#(1)           clk_p;
   (* prefix = "", result = "CLK_N" *)
   method    Bit#(1)           clk_n;
   (* prefix = "", result = "A" *)
   method    Bit#(13)          a;
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

(* always_ready, always_enabled *)
interface DDR3_Memory_Ifc;
   method    Action            clk_p(Bit#(1) i);
   method    Action            clk_n(Bit#(1) i);
   method    Action            cke(Bit#(1) i);
   method    Action            cs_n(Bit#(1) i);
   method    Action            ras_n(Bit#(1) i);
   method    Action            cas_n(Bit#(1) i);
   method    Action            we_n(Bit#(1) i);
   method    Action            reset_n(Bit#(1) i);
   method    Action            dm(Bit#(8) i);
   method    Action            ba(Bit#(3) i);
   method    Action            addr(Bit#(13) i);
   method    Action            odt(Bit#(1) i);
   interface Inout#(Bit#(64))  dq;
   interface Inout#(Bit#(8))   dqs_p;
   interface Inout#(Bit#(8))   dqs_n;
endinterface

interface DDR3_User;
   interface Clock                   clock;
   interface Reset                   reset_n;
   method    Bool                    init_done;
   method    Action                  put(Bit#(64) writeen, Bit#(27) address, Bit#(512) datain);
   method    ActionValue#(Bit#(512)) read();
endinterface

interface DDR3_Controller;
   (* prefix = "" *)
   interface DDR3_Pins ddr3;
   (* prefix = "" *)
   interface DDR3_User user;
endinterface

interface VDDR3_User;
   interface Clock       clock;
   interface Reset       reset_n;
   method    Bool        init_done;
   method    Action      address_phase(DDR3Command command, Bit#(27) address);
   method    Action      data_phase(Bit#(32) mask, Bit#(256) data);
   method    Action      last_data();
   method    Bit#(256)   read_phase();
endinterface

interface VDDR3_Controller;
   (* prefix = "" *)
   interface DDR3_Pins   ddr3;
   (* prefix = "" *)
   interface VDDR3_User  user;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr3_wrapper =
module vMkDDR3Controller#(DDR3_Configure cfg, Clock clk_ref_p, Clock clk_ref_n)(VDDR3_Controller);
   default_clock clk();
   default_reset rst(sys_rst_n);
   
   input_clock (clk_ref_p) = clk_ref_p;
   input_clock (clk_ref_n) = clk_ref_n;
   
   parameter SIM_INIT_OPTION = (cfg.fast_train_sim_only) ? "SKIP_PU_DLY" : "NONE";
   parameter SIM_CAL_OPTION  = (cfg.fast_train_sim_only) ? "FAST_CAL"    : "NONE";
   parameter CLK_PERIOD      = cfg.clk_period_in_ps;
      
   interface DDR3_Pins ddr3;
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
   
   interface VDDR3_User user;
      output_clock      clock(user_clock);
      output_reset      reset_n(user_reset_n) clocked_by(user_clock);
      
      method      init_done         init_done                                            clocked_by(no_clock)   reset_by(no_reset);
      method                        address_phase(app_cmd, app_addr)   enable(app_enable) ready(app_ready) clocked_by(user_clock) reset_by(user_reset_n);
      method                        data_phase(app_wdf_mask, app_wdf_data)  enable(app_wdf_enable) ready(app_wdf_ready) clocked_by(user_clock) reset_by(user_reset_n);
      method                        last_data() enable(app_wdf_end) clocked_by(user_clock) reset_by(user_reset_n);
      method      app_rd_data       read_phase() ready(app_rd_ready) clocked_by(user_clock) reset_by(user_reset_n);
   endinterface
   
   // Mark DDR3 pins as conflict free since BSV doesn't use them.
   schedule
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    )
   CF
   (
    ddr3_clk_p, ddr3_clk_n, ddr3_cke, ddr3_cs_n, ddr3_ras_n, ddr3_cas_n, ddr3_we_n, ddr3_reset_n, ddr3_dm, ddr3_ba, ddr3_a, ddr3_odt, user_init_done
    );
   
   // User interface schedule
   schedule (user_address_phase) C  (user_address_phase);
   schedule (user_data_phase)    C  (user_data_phase);
   schedule (user_last_data)     C  (user_last_data);
   schedule (user_read_phase)    CF (user_address_phase, user_data_phase, user_read_phase, user_last_data);
   schedule (user_address_phase) CF (user_data_phase, user_last_data);
   schedule (user_data_phase)    CF (user_last_data);
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation of BSV wrapper for DDR3 controller
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkDDR3Controller#(DDR3_Configure cfg, Clock clk_ref_p, Clock clk_ref_n)(DDR3_Controller);
   
   if (cfg.num_reads_in_flight < 1)
      error("The number of reads in flight has to be at least 1");
   if (cfg.num_reads_in_flight > 127)
      error("The arbitrary limit on the number of reads in flight is set at 126.  If you would like more, then you will need to update the Counter in this module to be wider than 8-bits.");
   if (cfg.clk_period_in_ps > 5000)
      error("The minimum frequency of the Xilinx DDR3 controller is 200 MHz");
   
   Integer reads = cfg.num_reads_in_flight;
   Integer beats = reads * 2;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   VDDR3_Controller                          ddr3ctrl            <- vMkDDR3Controller(cfg, clk_ref_p, clk_ref_n);
   Clock                                     user_clock           = ddr3ctrl.user.clock;
   Reset                                     user_reset_n         = ddr3ctrl.user.reset_n;
   
   FIFO#(DDR3Request)                        fRequest            <- mkFIFO1(clocked_by user_clock, reset_by user_reset_n);
   FIFO#(DDR3Response)                       fResponse           <- mkFIFO1(clocked_by user_clock, reset_by user_reset_n);
   
   Reg#(Bool)                                rDeqWriteReq        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bool)                                rEnqReadResp        <- mkReg(False, clocked_by user_clock, reset_by user_reset_n);
   Reg#(Bit#(256))                           rFirstResponse      <- mkRegU(clocked_by user_clock, reset_by user_reset_n);
   
   Counter#(8)                               rReadsPending       <- mkCounter(0, clocked_by user_clock, reset_by user_reset_n);
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////
   rule ready(ddr3ctrl.user.init_done);
      rule process_write_request_first((fRequest.first.byteen != 0) && !rDeqWriteReq);
	 rDeqWriteReq <= True;
	 ddr3ctrl.user.address_phase(WRITE, fRequest.first.address);
	 ddr3ctrl.user.data_phase(~truncate(fRequest.first.byteen), truncate(fRequest.first.data));
      endrule
   
      rule process_write_request_second((fRequest.first.byteen != 0) && rDeqWriteReq);
	 fRequest.deq;
	 ddr3ctrl.user.data_phase(~truncateLSB(fRequest.first.byteen), truncateLSB(fRequest.first.data));
	 ddr3ctrl.user.last_data();
	 rDeqWriteReq <= False;
      endrule
      
      rule process_read_request(fRequest.first.byteen == 0);
	 fRequest.deq;
	 ddr3ctrl.user.address_phase(READ, fRequest.first.address);
	 rReadsPending.inc(2);
      endrule
      
      rule process_read_response_first(!rEnqReadResp);
	 rFirstResponse <= ddr3ctrl.user.read_phase();
	 rEnqReadResp   <= True;
	 rReadsPending.down();
      endrule
      
      rule process_read_response_second(rEnqReadResp);
	 fResponse.enq(unpack({ ddr3ctrl.user.read_phase(), rFirstResponse }));
	 rEnqReadResp   <= False;
	 rReadsPending.down();
      endrule
   endrule
           
   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface DDR3_Pins ddr3 = ddr3ctrl.ddr3;
   interface DDR3_User user;
      interface clock   = user_clock;
      interface reset_n = user_reset_n;
      method    init_done = ddr3ctrl.user.init_done;
      method Action put(writeen, address, datain);
	 let req = DDR3Request {
	    byteen:  writeen,
	    address: pack(address),
	    data:    datain
	    };
	 fRequest.enq(req);
      endmethod
      method ActionValue#(Bit#(512)) read();
	 fResponse.deq;
	 return fResponse.first.data;
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
import "BVI" xilinx_ddr3_sodimm =
module vMkDDR3Memory(DDR3_Memory_Ifc);
   default_clock clk();
   default_reset rst();

   method clk_p(CLK_P) enable((*inhigh*)en0);
   method clk_n(CLK_N) enable((*inhigh*)en1);
   method cke(CKE) enable((*inhigh*)en2);
   method cs_n(CS_N) enable((*inhigh*)en3);
   method ras_n(RAS_N) enable((*inhigh*)en4);
   method cas_n(CAS_N) enable((*inhigh*)en5);
   method we_n(WE_N) enable((*inhigh*)en6);
   method reset_n(RESET_N) enable((*inhigh*)en11);
   method dm(DM) enable((*inhigh*)en7);
   method ba(BA) enable((*inhigh*)en8);
   method addr(ADDR) enable((*inhigh*)en9);
   method odt(ODT) enable((*inhigh*)en10);

   ifc_inout dq(DQ)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs_p(DQS_P)       clocked_by(no_clock) reset_by(no_reset);
   ifc_inout dqs_n(DQS_N)       clocked_by(no_clock) reset_by(no_reset);

   schedule (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, addr, odt, reset_n) CF
            (clk_p, clk_n, cke, cs_n, ras_n, cas_n, we_n, dm, ba, addr, odt, reset_n);

endmodule

module mkDDR3Memory(DDR3_Memory_Ifc);
   let _m <- vMkDDR3Memory;
   return _m;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
instance Connectable#(DDR3_Memory_Ifc, DDR3_Pins);
   module mkConnection#(DDR3_Memory_Ifc m, DDR3_Pins c)(Empty);
      rule connect;
         m.clk_p(c.clk_p);
         m.clk_n(c.clk_n);
         m.cke(c.cke);
         m.cs_n(c.cs_n);
         m.ras_n(c.ras_n);
         m.cas_n(c.cas_n);
         m.we_n(c.we_n);
	 m.reset_n(c.reset_n);
         m.dm(c.dm);
         m.ba(c.ba);
         m.addr(c.a);
         m.odt(c.odt);
      endrule

      mkConnection(m.dq, c.dq);
      mkConnection(m.dqs_p, c.dqs_p);
      mkConnection(m.dqs_n, c.dqs_n);
   endmodule
endinstance


instance Connectable#(MemoryClient#(a, d), DDR3_User)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR3_User ddr3)(Empty);
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
	 Bit#(27)  address = 0;
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
	 ddr3.put((request.write) ? enables : 0, address, datain);
      endrule
      
      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);
      
      rule connect_responses;
	 let response <- ddr3.read;
	 respFIFO.enq(cExtend(response));
      endrule
   endmodule
endinstance


endpackage: XilinxVirtex6DDR3

