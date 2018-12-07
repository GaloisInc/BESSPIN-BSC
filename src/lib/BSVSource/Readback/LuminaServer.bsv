////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013-2016  Bluespec, Inc.  ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//  Filename      : LuminaServer.bsv
//  Description   : Wrapper around the Readback core that adds Lumina controls
//                  (no clock control) via Put of commands and Get of data,
//                  which can be transported over any link (JTAG, SceMi, etc).
////////////////////////////////////////////////////////////////////////////////
package LuminaServer;

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import ReadbackCore      ::*;
import ReadbackDefines   ::*;
import GetPut            ::*;
import FShow             ::*;
import FIFO              ::*;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {
   void            LuminaClear;
   RdBackStoreCmd  LuminaStore;
   ConfigNum       LuminaFinish;
   Bit#(16)        LuminaBreakCode;
   void            LuminaReadState;
} LuminaCommand deriving (Eq, Bits, FShow);

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

interface LuminaServer;
   interface Put#(LuminaCommand) cmd;
   interface GetS#(Bit#(31))     rdback;
   method RdBackCycleStamp       cycle;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Implementation
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

module mkLuminaServer#(  parameter XilinxFamily family
		       , parameter Bool         is_dummy
		       )(LuminaServer);
   
   Bool trace = False;
   
   ////////////////////////////////////////////////////////////////////////////////
   /// Design Elements
   ////////////////////////////////////////////////////////////////////////////////
   Reg#(RdBackCycleStamp)          rCycleStamp         <- mkReg(0);
   Reg#(ConfigNum)                 rConfigNum          <- mkReg(maxBound);

   Reg#(Bool)                      rStoreConfigured    <- mkReg(False);

   ReadBackCore                    mCntrl              <- mkReadbackCore(family, is_dummy);

   FIFO#(LuminaCommand)            fCmd                <- mkFIFO;

   ////////////////////////////////////////////////////////////////////////////////
   /// Rules
   ////////////////////////////////////////////////////////////////////////////////

   rule process_cmd_clear if (fCmd.first() matches tagged LuminaClear);
      fCmd.deq;
      mCntrl.clear;
      rStoreConfigured <= False;
      if (trace) $display("[%t] CLEAR", $time);
   endrule
   
   rule process_cmd_store if (fCmd.first() matches tagged LuminaStore .code);
      fCmd.deq;
      mCntrl.cmd(tagged Insert code);
      if (trace) $display("[%t] STORE ", $time, fshow(code));
   endrule      
   
   rule process_cmd_finish if (fCmd.first() matches tagged LuminaFinish .cfg);
      fCmd.deq;
      mCntrl.cmd(tagged Finish);
      rStoreConfigured <= True;
      // XXX why don't we do this any more?
      //rConfigNum <= cfg;
      rConfigNum <= rConfigNum + 1;
      if (trace) $display("[%t] FINISH (%d)", $time, rConfigNum);
   endrule
   
   rule process_cmd_break if (fCmd.first() matches tagged LuminaBreakCode .code);
      fCmd.deq;
      mCntrl.brkCode(code);
      if (trace) $display("[%t] BREAKCODE %04X", $time, code);
   endrule
   
   rule process_cmd_readstate if (fCmd.first() matches tagged LuminaReadState);
      fCmd.deq;
      mCntrl.startReadback;
      if (trace) $display("[%t] READSTATE", $time);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule incr_and_update_stamp;
      rCycleStamp <= rCycleStamp + 1;
      mCntrl.stamp(rCycleStamp);
      mCntrl.cfg(rConfigNum);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////

   interface cmd    = toPut(fCmd);
   interface rdback = mCntrl.rdback;
   method    cycle  = rCycleStamp;
   
endmodule: mkLuminaServer

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage: LuminaServer

