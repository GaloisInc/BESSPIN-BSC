////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2014  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxKCU105DDR4.bsv
//  Description   :
////////////////////////////////////////////////////////////////////////////////
package XilinxKCU105DDR4;

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
import Memory            ::*;
import ClientServer      ::*;
import GetPut            ::*;
import BUtils            ::*;
import I2C               ::*;
import StmtFSM           ::*;
import DDR4              ::*;

import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
/*
ddr4addrsize    28
ddr4datasize   512
ddr4besize      64
ddr4beats        1
datawidth       64
bewidth          8
rowwidth        17
colwidth        10
bankwidt         2
bankgroupwidth   1
rankwidth        1
clkwidth         1
cswidth          1
ckewidth         1
odtwidth         1
*/

 `define DDR4_KCU105 28, 512, 64, 1, 64, 8, 17, 10, 2, 1, 1, 1, 1, 1, 1


typedef DDR4_Pins#(`DDR4_KCU105) DDR4_Pins_KCU105;
typedef DDR4_User#(`DDR4_KCU105) DDR4_User_KCU105;
typedef DDR4_Controller#(`DDR4_KCU105) DDR4_Controller_KCU105;
typedef VDDR4_User_Xilinx#(`DDR4_KCU105) VDDR4_User_Xilinx_KCU105;
typedef VDDR4_Controller_Xilinx#(`DDR4_KCU105) VDDR4_Controller_Xilinx_KCU105;

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
import "BVI" ddr4_1 =
module vMkKCU105DDR4Controller#(Clock c0_sys_clk_p, Clock c0_sys_clk_n)(VDDR4_Controller_Xilinx_KCU105);
   default_clock no_clock;
   default_reset rst(sys_rst);
   input_clock (c0_sys_clk_p) = c0_sys_clk_p;
   input_clock (c0_sys_clk_n) = c0_sys_clk_n;

   //parameter SIM_BYPASS_INIT_CAL = (cfg.simulation) ? "FAST" : "OFF";
   //parameter SIMULATION          = (cfg.simulation) ? "TRUE" : "FALSE";

   interface DDR4_Pins ddr4;
      ifc_inout   dm_dbi_n(c0_ddr4_dm_dbi_n)reset_by(no_reset);
      ifc_inout   dq(c0_ddr4_dq)            reset_by(no_reset);
      ifc_inout   dqs_c(c0_ddr4_dqs_c)      reset_by(no_reset);
      ifc_inout   dqs_t(c0_ddr4_dqs_t)      reset_by(no_reset);
      method      c0_ddr4_act_n   act_n     reset_by(no_reset);
      method      c0_ddr4_adr     a         reset_by(no_reset);
      method      c0_ddr4_ba      ba        reset_by(no_reset);
      method      c0_ddr4_bg      bg        reset_by(no_reset);
      method      c0_ddr4_cke     cke       reset_by(no_reset);
      method      c0_ddr4_odt     odt       reset_by(no_reset);
      method      c0_ddr4_cs_n    cs_n      reset_by(no_reset);
      method      c0_ddr4_ck_t    ck_t      reset_by(no_reset);
      method      c0_ddr4_ck_c    ck_c      reset_by(no_reset);
      method      c0_ddr4_reset_n reset_n   reset_by(no_reset);
   endinterface

   interface VDDR4_User_Xilinx user;
      output_clock    clock(c0_ddr4_ui_clk);
      output_reset    reset(c0_ddr4_ui_clk_sync_rst);
      method c0_init_calib_complete   init_done    clocked_by(no_clock) reset_by(no_reset);
      method          		      app_addr(c0_ddr4_app_addr) enable((*inhigh*)en0) clocked_by(user_clock) reset_by(user_reset);
      method                          app_cmd(c0_ddr4_app_cmd)   enable((*inhigh*)en00) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_en(c0_ddr4_app_en)     enable((*inhigh*)en1) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_hi_pri(c0_ddr4_app_hi_pri) enable((*inhigh*)en6) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_wdf_data(c0_ddr4_app_wdf_data) enable((*inhigh*)en2) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_wdf_end(c0_ddr4_app_wdf_end)   enable((*inhigh*)en3) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_wdf_mask(c0_ddr4_app_wdf_mask) enable((*inhigh*)en4) clocked_by(user_clock) reset_by(user_reset);
      method          		      app_wdf_wren(c0_ddr4_app_wdf_wren) enable((*inhigh*)en5) clocked_by(user_clock) reset_by(user_reset);
      method c0_ddr4_app_rd_data         app_rd_data clocked_by(user_clock) reset_by(user_reset);
      method c0_ddr4_app_rd_data_end     app_rd_data_end clocked_by(user_clock) reset_by(user_reset);
      method c0_ddr4_app_rd_data_valid   app_rd_data_valid clocked_by(user_clock) reset_by(user_reset);
      method c0_ddr4_app_rdy             app_rdy clocked_by(user_clock) reset_by(user_reset);
      method c0_ddr4_app_wdf_rdy         app_wdf_rdy clocked_by(user_clock) reset_by(user_reset);
      method dbg_bus                  dbg_bus clocked_by(user_clock) reset_by(user_reset);
   endinterface

   schedule
   (
    ddr4_act_n, ddr4_a, ddr4_ba, ddr4_bg, ddr4_cke, ddr4_odt, ddr4_cs_n,
    ddr4_ck_t, ddr4_ck_c, ddr4_reset_n, user_init_done
    )
   CF
   (
    ddr4_act_n, ddr4_a, ddr4_ba, ddr4_bg, ddr4_cke, ddr4_odt, ddr4_cs_n,
    ddr4_ck_t, ddr4_ck_c, ddr4_reset_n, user_init_done
    );

   schedule
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data,
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd, user_app_hi_pri, user_dbg_bus
    )
   CF
   (
    user_app_addr, user_app_en, user_app_wdf_data, user_app_wdf_end, user_app_wdf_mask, user_app_wdf_wren, user_app_rd_data,
    user_app_rd_data_end, user_app_rd_data_valid, user_app_rdy, user_app_wdf_rdy, user_app_cmd, user_app_hi_pri, user_dbg_bus
    );

endmodule

module mkDDR4Controller_KCU105#(DDR4_Configure cfg, Clock refclk_p, Clock refclk_n)(DDR4_Controller_KCU105);
   (* hide_all *)
   let _v <- vMkKCU105DDR4Controller(refclk_p, refclk_n);
   let _m <- mkXilinxDDR4Controller_1beat(_v, cfg, clocked_by _v.user.clock, reset_by _v.user.reset);
   return _m;
endmodule

instance Connectable#(MemoryClient#(a, d), DDR4_User_KCU105)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     , Log#(_3, m)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR4_User_KCU105 ddr4)(Empty);
      let sz = valueOf(d);

      Clock uClock              <- exposeCurrentClock;
      Reset uReset              <- exposeCurrentReset;
      Clock dClock              = ddr4.clock;
      Reset dReset              = ddr4.reset_n;

      Reset uRst_dclk           <- mkAsyncReset( 2, uReset, dClock );
      Reset jointReset1         <- mkResetEither( dReset, uRst_dclk, clocked_by dClock );
      Reset jointReset          <- mkAsyncReset( 2, jointReset1, dClock );

      SyncFIFOIfc#(MemoryRequest#(a,d)) reqFIFO <- mkSyncFIFO(1, uClock, uReset, dClock);
      let connectReqUser <- mkConnection(client.request, toPut(reqFIFO), clocked_by uClock, reset_by uReset);

      // Allow for 16 reads in flight (twice default value):
      FIFOF#(Bit#(m)) rspShftFF  <- mkSizedFIFOF(16, clocked_by dClock, reset_by dReset); //JES 05/17/18

      Reg#(Bool) clearFF <- mkReg(True, clocked_by dClock, reset_by uRst_dclk);
      rule clearRl (clearFF);
	 if (rspShftFF.notEmpty) rspShftFF.clear;
	 else clearFF <= False;
      endrule

      rule connect_requests;
	 let request = reqFIFO.first; reqFIFO.deq;
	 Bit#(28)  address = 0;
	 Bit#(64)  enables = 0;
	 Bit#(512) datain  = duplicate(request.data);
	 Bit#(m)   shft    = 0;
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
		    shft = cExtend(request.address[2:0]);
		 end
	    128: begin
		    address = cExtend(request.address) << 1;
		    case(request.address[1:0])
		       0: enables = cExtend(request.byteen);
		       1: enables = cExtend(request.byteen) << 16;
		       2: enables = cExtend(request.byteen) << 32;
		       3: enables = cExtend(request.byteen) << 48;
		    endcase
		    shft = cExtend(request.address[1:0]);
		 end
	    256: begin
		    address = cExtend(request.address) << 2;
		    case(request.address[0])
		       0: enables = cExtend(request.byteen);
		       1: enables = cExtend(request.byteen) << 32;
		    endcase
		    shft = cExtend(request.address[0]);
		 end
	    512: begin
		    address = cExtend(request.address) << 3;
		    enables = cExtend(request.byteen);
		 end
	    default: error("DDR4 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 if (!request.write) rspShftFF.enq(shft);
	 ddr4.request(address, (request.write) ? enables : 0, datain);
      endrule

      SyncFIFOIfc#(MemoryResponse#(d)) respFIFO <- mkSyncFIFO(1, dClock, jointReset, uClock);
      let connectRespUser <- mkConnection(client.response, toGet(respFIFO), clocked_by uClock, reset_by uReset);

      rule connect_responses;
	 let shft <- toGet(rspShftFF).get();
	 let response <- ddr4.read_data;
	 case(sz)
	    64:  response = response >> {shft,6'b0};
	    128: response = response >> {shft,7'b0};
	    256: response = response >> {shft,8'b0};
	    512: begin end
	    default: error("DDR4 connection: data width must be 64, 128, 256, or 512");
	 endcase
	 respFIFO.enq(cExtend(response));
      endrule
   endmodule
endinstance

endpackage: XilinxKCU105DDR4
