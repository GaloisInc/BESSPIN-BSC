////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2014  Bluespec, Inc.  ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
//  Filename      : XilinxML605DDR3.bsv
//  Description   : 
////////////////////////////////////////////////////////////////////////////////
package XilinxML605DDR3;

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
`define DDR3_ML605 27, 256, 32, 2, 64, 8, 13, 10, 3, 1, 1, 1, 1, 1

typedef DDR3_Pins#(`DDR3_ML605) DDR3_Pins_ML605;
typedef DDR3_User#(`DDR3_ML605) DDR3_User_ML605;
typedef DDR3_Controller#(`DDR3_ML605) DDR3_Controller_ML605;
typedef VDDR3_User_Xilinx#(`DDR3_ML605) VDDR3_User_Xilinx_ML605;
typedef VDDR3_Controller_Xilinx#(`DDR3_ML605) VDDR3_Controller_Xilinx_ML605;

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
module vMkML605DDR3Controller#(DDR3_Configure cfg, Clock clk_ref_p, Clock clk_ref_n)(VDDR3_Controller_Xilinx_ML605);
   default_clock clk();
   default_reset rst(sys_rst_n);
   
   input_clock (clk_ref_p) = clk_ref_p;
   input_clock (clk_ref_n) = clk_ref_n;
     
   parameter SIM_INIT_OPTION = (cfg.simulation) ? "SKIP_PU_DLY" : "NONE";
   parameter SIM_CAL_OPTION  = (cfg.simulation) ? "FAST_CAL"    : "NONE";
   parameter CLK_PERIOD      = 5000;
   
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
      output_clock    clock(user_clock);
      output_reset    reset(user_reset) clocked_by (user_clock);
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

module mkDDR3Controller_ML605#(DDR3_Configure cfg, Clock clk_ref_p, Clock clk_ref_n)(DDR3_Controller_ML605);
   (* hide_all *)
   let _v <- vMkML605DDR3Controller(cfg, clk_ref_p, clk_ref_n);
   let _m <- mkXilinxDDR3Controller_2beats(_v, cfg);
   return _m;
endmodule

instance Connectable#(MemoryClient#(a, d), DDR3_User_ML605)
   provisos (  Add#(24, _1, a)
	     , Add#(d, _2, 512)
	     , Mul#(d, _3, 512)
	     );
   module mkConnection#(MemoryClient#(a, d) client, DDR3_User_ML605 ddr3)(Empty);
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


endpackage: XilinxML605DDR3

