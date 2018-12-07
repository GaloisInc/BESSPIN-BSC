////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2014  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxDN10GK7LLDDR3.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxDN10GK7LLDDR3;

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
import I2C               ::*;
import StmtFSM           ::*;
import DDR3              ::*;

import XilinxCells       ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
`define DDR3_10GK7LL 30, 576, 72, 1, 72, 9, 16, 10, 3, 1, 1, 1, 1, 1


typedef DDR3_Pins#(`DDR3_10GK7LL) DDR3_Pins_10GK7LL;
typedef DDR3_User#(`DDR3_10GK7LL) DDR3_User_10GK7LL;
typedef DDR3_Controller#(`DDR3_10GK7LL) DDR3_Controller_10GK7LL;
typedef VDDR3_User_Xilinx#(`DDR3_10GK7LL) VDDR3_User_Xilinx_10GK7LL;
typedef VDDR3_Controller_Xilinx#(`DDR3_10GK7LL) VDDR3_Controller_Xilinx_10GK7LL;

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
import "BVI" ddr3_wrapper =
module vMk10GK7LLDDR3Controller#(DDR3_Configure cfg, Clock refclk)(VDDR3_Controller_Xilinx_10GK7LL);
   default_clock clk(sys_clk_i);
   default_reset rst(sys_rst);
   
   input_clock refclk(ref_clk_i) = refclk;
   
   parameter SIM_BYPASS_INIT_CAL = (cfg.simulation) ? "FAST" : "OFF";
   parameter SIMULATION          = (cfg.simulation) ? "TRUE" : "FALSE";
   
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
   
   interface VDDR3_User_Xilinx user;
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

module mkDDR3Controller_10GK7LL#(DDR3_Configure cfg, Clock refclk)(DDR3_Controller_10GK7LL);
   (* hide_all *)
   let _v <- vMk10GK7LLDDR3Controller(cfg, refclk);
   let _m <- mkXilinxDDR3Controller_1beat(_v, cfg);
   return _m;
endmodule
   
instance Connectable#(MemoryClient#(a, d), DDR3_User_10GK7LL)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR3_User_10GK7LL ddr3)(Empty);
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
   

endpackage: XilinxDN10GK7LLDDR3

