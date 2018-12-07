// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiCore;

// This is the main SceMi package.  It exports the standard
// interfaces and modules which implement them.  These modules
// are parameterized on the SceMiLinkType, and are really
// just wrappers which dispatch to the specific module
// implementations for the chosen SceMiLinkType.

import List          :: *;
import Vector        :: *;
import ModuleContext :: *;
import DefaultValue  :: *;
import Connectable   :: *;
import TieOff        :: *;
import BlueNoC       :: *;
import XilinxPCIE    :: *;
import XilinxVirtex5PCIE :: *;
import XilinxVirtex6PCIE :: *;
import XilinxKintex7PCIE :: *;
import XilinxVirtex7PCIE :: *;
import XilinxVirtexUltraScalePCIE :: *;


import SceMiDefines     :: *;
import SceMiInternals   :: *;
import SceMiNoC         :: *;
import SceMiMacros      :: *;
import SceMiEveMacros   :: *;
import SceMiAldecMacros :: *;
import SceMiPCIE        :: *;

export SceMiMessageInPortIfc(..), mkSceMiMessageInPort;
export SceMiMessageOutPortIfc(..), mkSceMiMessageOutPort;

export Visibility(..);
export SceMiInputPipeIfc(..), mkSceMiInputPipe;
export SceMiOutputPipeIfc(..), mkSceMiOutputPipe;

export   SceMiInputPipeParameters(..), InpipeCreditReturnMode (..), mkSceMiInputPipeP;
export   SceMiOutputPipeParameters(..), mkSceMiOutputPipeP;

export SceMiClockPortIfc(..), mkSceMiClockPort;
export SceMiClockControlIfc(..), mkSceMiClockControl;

export SceMiModule, SceMiBuilder(..);

export SceMiClockArgs(..);
export SceMiNoCArgs(..), SceMiNoCArgsNoClock(..), SceMiNoCIfc(..);
export SceMiV5PCIEArgs(..), SceMiV5PCIEIfc(..);
export SceMiV6PCIEArgs(..), SceMiV6PCIEIfc(..);
export SceMiK7PCIEArgs(..), SceMiK7PCIEIfc(..);
export SceMiV7PCIEArgs(..), SceMiV7PCIEIfc(..);
export SceMiV7PCIE3Args(..), SceMiV7PCIE3NoClkArgs(..);
export SceMiV7PCIENoClkArgs(..), SceMiV7PCIENoClkArgsNoClock(..);
export SceMiVUPCIE3Args(..), SceMiVUPCIE3NoClkArgs(..), SceMiVUPCIE3NoClkArgsNoClock(..), SceMiVUPCIEIfc(..);
export SceMiDiniPCIEArgs(..), SceMiDiniPCIEIfc(..);
export SceMiV7Args(..), SceMiV7Ifc(..);

export mkSceMiExternalClockPort, buildSceMiWithExternalClocks;
export SceMiClockConfiguration(..);
export DomainInfo(..), sceMiLookupClock, sceMiCycleStamp;
export sceMiSetUClock, sceMiGetUClock, sceMiGetUReset;

export mkSceMiConstant;

export SceMiCycleStamp, SceMiLinkType(..);

//
// SCE-MI 1.1 Port Interfaces
//

// Instantiate the message input port for the given link type
// Implicit Clock and reset are not used
module [SceMiModule] mkSceMiMessageInPortRaw (SceMiMessageInPortRawIfc#(a) ifc)
   provisos(Bits#(a,_));

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to input port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
                SCEMI:        mkSceMiMacroMessageInPort (clocked_by uClock, reset_by uReset);
                EVE:          mkSceMiEveMessageInPort   (clocked_by uClock, reset_by uReset);
                ALDEC:        mkSceMiMacroMessageInPort (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
		PCIE_KINTEX7: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX7: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEXU: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCMessageInPort   (clocked_by uClock, reset_by uReset);
             endcase;
   return _m;
endmodule: mkSceMiMessageInPortRaw

// This wrapper translates from the raw request/grant protocol to the
// desired ready/enable protocol.
module [SceMiModule] mkSceMiMessageInPort(SceMiMessageInPortIfc#(a))
   provisos(Bits#(a,sz));

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;
   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   (* hide *)
   let _m <- rawInPortToBSV(mkSceMiMessageInPortRaw, clocked_by uClock, reset_by uReset);
   return _m;
endmodule

// Instantiate the message output port for the given link type
// Implicit Clock and reset are not used
module [SceMiModule] mkSceMiMessageOutPort (SceMiMessageOutPortIfc#(a) ifc)
   provisos(Bits#(a,_));

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;

   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);
   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to output port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                SCEMI:        mkSceMiMacroMessageOutPort (clocked_by uClock, reset_by uReset);
                EVE:          mkSceMiEveMessageOutPort   (clocked_by uClock, reset_by uReset);
                ALDEC:        mkSceMiMacroMessageOutPort (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_KINTEX7: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX7: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_VIRTEXU: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCMessageOutPort   (clocked_by uClock, reset_by uReset);
             endcase;
   return _m;
endmodule: mkSceMiMessageOutPort

//
// SCE-MI Clock Ports and Clock Controllers
//

typedef struct { Integer clockNum;
                 Integer clockGroup;
                 Integer ratioNumerator;
                 Integer ratioDenominator;
                 Integer dutyHi;
                 Integer dutyLo;
                 Integer phase;
                 Integer resetCycles;
               } SceMiClockConfiguration deriving (Eq);

instance DefaultValue#(SceMiClockConfiguration);
   function SceMiClockConfiguration defaultValue();
      SceMiClockConfiguration conf;
      conf.clockNum         = 0;
      conf.clockGroup       = noClockGroup;
      conf.ratioNumerator   = 1;
      conf.ratioDenominator = 1;
      conf.dutyHi           = 0;
      conf.dutyLo           = 100;
      conf.phase            = 0;
      conf.resetCycles      = 8;
      return conf;
   endfunction
endinstance

// Instantiate a clock port for the given link type
module [SceMiModule] mkSceMiClockPort#( parameter SceMiClockConfiguration conf
                                      )
                                      (SceMiClockPortIfc ifc);

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;

   // record parameters for infrastructure linkage tool
   Ignored param_clockNum   <- mkSceMiUInt32Parameter(fromInteger(conf.clockNum));
   UInt#(32) cg = (conf.clockGroup == noClockGroup) ? unpack('1) : fromInteger(conf.clockGroup);
   Ignored param_clockGroup <- mkSceMiUInt32Parameter(cg);
   Ignored param_ratioNum   <- mkSceMiUInt64Parameter(fromInteger(conf.ratioNumerator));
   Ignored param_ratioDen   <- mkSceMiUInt64Parameter(fromInteger(conf.ratioDenominator));
   Ignored param_dutyHi     <- mkSceMiUInt32Parameter(fromInteger(conf.dutyHi));
   Ignored param_dutyLo     <- mkSceMiUInt32Parameter(fromInteger(conf.dutyLo));
   Ignored param_phase      <- mkSceMiUInt32Parameter(fromInteger(conf.phase));
   Ignored param_rstStage   <- mkSceMiUInt32Parameter(fromInteger(conf.resetCycles));
   Ignored param_link_type  <- mkSceMiLinkTypeParameter(link_type);

   // Dispatch to clock port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:   mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                SCEMI: mkSceMiMacroClockPort( conf.clockNum
                                            , conf.ratioNumerator, conf.ratioDenominator
                                            , conf.dutyHi, conf.dutyLo, conf.phase
                                            , conf.resetCycles
                                            );
                EVE:   mkSceMiEveClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          );
                ALDEC: mkSceMiAldecClockPort( conf.clockNum
                                            , conf.ratioNumerator, conf.ratioDenominator
                                            , conf.dutyHi, conf.dutyLo, conf.phase
                                            , conf.resetCycles
                                            , conf.clockGroup
                                            );
                PCIE_VIRTEX5:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                         );
                PCIE_VIRTEX6:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                PCIE_KINTEX7:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                PCIE_VIRTEX7:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                PCIE_VIRTEXU:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                PCIE_DINI:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
                PCIE_ARRIA10:
                       mkSceMiNoCClockPort( conf.clockNum
                                          , conf.ratioNumerator, conf.ratioDenominator
                                          , conf.dutyHi, conf.dutyLo, conf.phase
                                          , conf.resetCycles
                                          , conf.clockGroup
                                          , link_type
                                          );
             endcase;
   return _m;
endmodule: mkSceMiClockPort

// Instantiate a clock controller for the given link type
module [SceMiModule] mkSceMiClockControl#( parameter Integer clockNum
                                         , Bool allow_pos_edge
                                         , Bool allow_neg_edge
                                         )
                                         (SceMiClockControlIfc ifc);

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;

   // record parameters for infrastructure linkage tool
   Ignored param_clockNum  <- mkSceMiUInt32Parameter(fromInteger(clockNum));
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   // Dispatch to clock control for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                SCEMI:        mkSceMiMacroClockControl (clockNum, allow_pos_edge, allow_neg_edge);
                EVE:          mkSceMiEveClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                ALDEC:        mkSceMiMacroClockControl (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_VIRTEX5: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_VIRTEX6: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_KINTEX7: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_VIRTEX7: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_VIRTEXU: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_DINI:    mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
                PCIE_ARRIA10: mkSceMiNoCClockControl   (clockNum, allow_pos_edge, allow_neg_edge);
             endcase;
   return _m;
endmodule: mkSceMiClockControl


// Instantiate an external clock port for the given link type.
module mkSceMiExternalClockPort#( parameter SceMiClockConfiguration conf
                                , parameter SceMiLinkType link_type
                                )
                                (SceMiClockPortIfc ifc);

   // record parameters for infrastructure linkage tool
   Ignored param_clockNum   <- mkSceMiUInt32Parameter(fromInteger(conf.clockNum));
   UInt#(32) cg = (conf.clockGroup == noClockGroup) ? unpack('1) : fromInteger(conf.clockGroup);
   Ignored param_clockGroup <- mkSceMiUInt32Parameter(cg);
   Ignored param_ratioNum   <- mkSceMiUInt64Parameter(fromInteger(conf.ratioNumerator));
   Ignored param_ratioDen   <- mkSceMiUInt64Parameter(fromInteger(conf.ratioDenominator));
   Ignored param_dutyHi     <- mkSceMiUInt32Parameter(fromInteger(conf.dutyHi));
   Ignored param_dutyLo     <- mkSceMiUInt32Parameter(fromInteger(conf.dutyLo));
   Ignored param_phase      <- mkSceMiUInt32Parameter(fromInteger(conf.phase));
   Ignored param_rstStage   <- mkSceMiUInt32Parameter(fromInteger(conf.resetCycles));
   Ignored param_link_type  <- mkSceMiLinkTypeParameter(link_type);

   // Dispatch to clock port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                EVE:     mkSceMiEveExternalClockPort(conf.clockNum);
                default: error("mkSceMiExternalClockPort is supported only for the EVE link type");
             endcase;
   return _m;
endmodule: mkSceMiExternalClockPort

//
// SCE-MI 2.1 Pipe Interfaces
//

// Instantiate the message input port for the given link type
// Implicit Clock and reset are not used
module [SceMiModule] mkSceMiInputPipe#(Integer depth, Visibility vis)
                                      (SceMiInputPipeIfc#(m,e) ifc)
   provisos( Bits#(e, esz)
           , Add#(1, _1, m)
             // We must be able to index "m" by a 16-bit pointer
            , Add#(_2, TLog#(m), 16)
           );


   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;
   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to input port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_KINTEX7: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX7: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEXU: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCInputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                default:      error("Link type doesn't support pipes");
             endcase;
   return _m;
endmodule: mkSceMiInputPipe


module [SceMiModule] mkSceMiInputPipeP#(SceMiInputPipeParameters params)
                                       (SceMiInputPipeIfc#(m,e) ifc)
   provisos( Bits#(e, esz)
           , Add#(1, _1, m)
             // We must be able to index "m" by a 16-bit pointer
            , Add#(_2, TLog#(m), 16)
             // BSC should be able to figure these out
           );


   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;
   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to input port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_KINTEX7: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX7: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEXU: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCInputPipeP(params, clocked_by uClock, reset_by uReset);
                default:      error("Link type doesn't support pipes");
             endcase;
   return _m;
endmodule: mkSceMiInputPipeP

// Instantiate the message output port for the given link type
// Implicit Clock and reset are not used
module [SceMiModule] mkSceMiOutputPipe#(Integer depth, Visibility vis)
                                       (SceMiOutputPipeIfc#(m,e) ifc)
   provisos( Bits#(e, esz)
            , Add#(1, _1, m)  // The max payload size "m" must be at least 1
            , Add#(_xx1, TLog#(m), 16) // index "m" by a 16-bit pointer
           );

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;
   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to input port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_KINTEX7: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX7: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEXU: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCOutputPipe(depth, vis, clocked_by uClock, reset_by uReset);
                default:      error("Link type doesn't support pipes");
             endcase;
   return _m;
endmodule: mkSceMiOutputPipe

// Instantiate the message output port for the given link type
// Implicit Clock and reset are not used
module [SceMiModule] mkSceMiOutputPipeP#(SceMiOutputPipeParameters params)
                                       (SceMiOutputPipeIfc#(m,e) ifc)
   provisos( Bits#(e, esz)
            , Add#(1, _1, m) // The max payload size "m" must be at least 1
            , Add#(_xx1, TLog#(m), 16) // index "m" by a 16-bit pointer
            );

   SceMiModuleState state <- getContext();
   SceMiLinkType link_type = state.link_type;
   // record link type parameter for infrastructure linkage tool
   Ignored param_link_type <- mkSceMiLinkTypeParameter(link_type);

   Clock uClock <- sceMiGetUClock;
   Reset uReset <- sceMiGetUReset;

   // Dispatch to input port for specific linkage type
   (* hide *)
   let _m <- case (link_type) matches
                TCP:          mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX5: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX6: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_KINTEX7: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEX7: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_VIRTEXU: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_DINI:    mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                PCIE_ARRIA10: mkSceMiNoCOutputPipeP(params, clocked_by uClock, reset_by uReset);
                default:      error("Link type doesn't support pipes");
             endcase;
   return _m;
endmodule: mkSceMiOutputPipeP

//
// Type Classes for Building SCE-MI Infrastructure
//

// Argument structure used for adding externally defined clocks
typedef struct {
   Vector#(n,Clock) clks_in;
   Vector#(n,Reset) rsts_in;
   SceMiLinkType    link_type;
} SceMiClockArgs#(numeric type n);

// Instantiate the given module in the SceMiModule monad,
// add in all necessary SceMi infrastructure for the given
// link type, and return the whole bundle in the normal
// Module monad.  This is parameterized over the argument type
// for input and the returned interface to allow for
// flexibility to use with EVE, PCIE, TCP, etc. links.
typeclass SceMiBuilder#(type ifc_type, type arg_type, type extended_ifc_type);
   module buildSceMi#( SceMiModule#(ifc_type) mod
                     , arg_type arg
                     )
                     (extended_ifc_type ifc);
endtypeclass

// Basic buildSceMi instance
instance SceMiBuilder#(i,SceMiLinkType,i);

   module buildSceMi#(SceMiModule#(i) mod, SceMiLinkType link_type)(i);
      if (link_type == TCP || link_type == TCP_NOCLOCK ) begin
         TCPtoBNoC#(BPB) bridge <- mkTCPtoBNoC();

         // name must be "tcp_port_number" to have its value set during SCE-MI
         // infrastructure linkage
         ReadOnly#(UInt#(32)) tcp_port_number <- mkSceMiConstant();

         (* fire_when_enabled *)
         rule open_port if (!bridge.active());
            Bool ok <- bridge.listen(tcp_port_number);
            if (!ok)
               $finish(0);
         endrule

         Clock clk <- exposeCurrentClock();
         Reset rst <- exposeCurrentReset();

         SceMiNoCIfc#(i) _m;

	 if (link_type == TCP) begin
            SceMiNoCArgs args;
            args.clk           = clk;
            args.rst           = rst;
            args.noc_is_active = True;
            args.link_type     = TCP;

            (* hide *)
            _m <- buildSceMi(mod,args);
	 end
	 else begin
            SceMiNoCArgsNoClock args;
            args.clk           = clk;
            args.rst           = rst;
            args.noc_is_active = True;
            args.link_type     = TCP;

            (* hide *)
            _m <- buildSceMi(mod,args);
	 end

         mkConnection(_m.noc_src,bridge.noc);
	 mkTieOff(_m.noc_cont); // XXX this should be exported

         return _m.orig_ifc;
      end
      else begin
         SceMiClockArgs#(0) args;
         args.clks_in   = Vector::nil;
         args.rsts_in   = Vector::nil;
         args.link_type = link_type;

         (* hide *)
         let _m <- buildSceMi(mod,args);
         return _m;
      end
   endmodule

endinstance

// A buildSceMi instance that supports passing in external clocks and resets
instance SceMiBuilder#(i,SceMiClockArgs#(n),i);

   module buildSceMi#(SceMiModule#(i) mod, SceMiClockArgs#(n) args)(i);

      // record link type parameter for infrastructure linkage tool
      (*hide_all*) Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build =
                case (args.link_type) matches
                   SCEMI:   buildSceMiMacro(mod, args.clks_in, args.rsts_in, args.link_type);
                   EVE:     buildSceMiEve(mod, args.clks_in, args.rsts_in, args.link_type);
                   ALDEC:   buildSceMiMacro(mod, args.clks_in, args.rsts_in, args.link_type);
                   default: error("Selected link type does not support external clocks");
                endcase;
      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via NoC
instance SceMiBuilder#(i,SceMiNoCArgs,SceMiNoCIfc#(i));

   module buildSceMi#(SceMiModule#(i) mod, SceMiNoCArgs args)
                     (SceMiNoCIfc#(i));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      let build = buildSceMiNoC( mod, args.clk, args.rst, args.noc_is_active, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via NoC (same as above, but no clocks)
instance SceMiBuilder#(i,SceMiNoCArgsNoClock,SceMiNoCIfc#(i));

   module buildSceMi#(SceMiModule#(i) mod, SceMiNoCArgsNoClock args)
                     (SceMiNoCIfc#(i));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      let build = buildSceMiNoCNoClock( mod, args.clk, args.rst, args.noc_is_active, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex5
instance SceMiBuilder#(i,SceMiV5PCIEArgs,SceMiV5PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex5PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV5PCIEArgs args)
                     (SceMiV5PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex5PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEV5( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex6
instance SceMiBuilder#(i,SceMiV6PCIEArgs,SceMiV6PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex6PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV6PCIEArgs args)
                     (SceMiV6PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex6PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEV6( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Kintex7
instance SceMiBuilder#(i,SceMiK7PCIEArgs,SceMiK7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectKintex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiK7PCIEArgs args)
                     (SceMiK7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectKintex7PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEK7( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex7
instance SceMiBuilder#(i,SceMiV7PCIEArgs,SceMiV7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7PCIEArgs args)
                     (SceMiV7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEV7( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE3 for Virtex7
instance SceMiBuilder#(i,SceMiV7PCIE3Args,SceMiV7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex7PCIE3#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7PCIE3Args args)
                     (SceMiV7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex7PCIE3#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIE3V7( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE3 for Virtex7
instance SceMiBuilder#(i,SceMiV7PCIE3NoClkArgs,SceMiV7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex7PCIE3#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7PCIE3NoClkArgs args)
                     (SceMiV7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex7PCIE3#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIE3NoClkV7( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex7
instance SceMiBuilder#(i,SceMiV7PCIENoClkArgs,SceMiV7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7PCIENoClkArgs args)
                     (SceMiV7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIENoClkV7( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex7
instance SceMiBuilder#(i,SceMiV7PCIENoClkArgsNoClock,SceMiV7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7PCIENoClkArgsNoClock args)
                     (SceMiV7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex7PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIENoClkV7NoClock( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Virtex7
instance SceMiBuilder#(i,SceMiV7Args,SceMiV7Ifc#(i));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV7Args args)
                     (SceMiV7Ifc#(i));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiV7( mod, args.fpga_clk, args.fpga_rst, args.noc_q_clk, args.noc_a_clk, args.noc_reset_n, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE3 for Virtex UltraScale
instance SceMiBuilder#(i,SceMiVUPCIE3Args,SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiVUPCIE3Args args)
                     (SceMiVUPCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIE3VU( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE3, with an external SceMi
// clock, for Virtex UltraScale
instance SceMiBuilder#(i,SceMiVUPCIE3NoClkArgs,SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiVUPCIE3NoClkArgs args)
                     (SceMiVUPCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIE3NoClkVU( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

instance SceMiBuilder#(i,SceMiVUPCIE3NoClkArgsNoClock,SceMiVUPCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiVUPCIE3NoClkArgsNoClock args)
                     (SceMiVUPCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtexUltraScalePCIE3#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIE3NoClkVUNoClock( mod, args.pci_sys_clk_p, args.pci_sys_clk_n, args.pci_sys_reset, args.ref_clk, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Arria10
instance SceMiBuilder#(i,SceMiA10PCIEArgs,SceMiA10PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectKintex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiA10PCIEArgs args)
                     (SceMiA10PCIEIfc#(i,lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEA10( mod, args.ref_clk, args.pin_perst, args.clock_period, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// A buildSceMi instance to support a link via PCIE for Dini boards
instance SceMiBuilder#(i,SceMiDiniPCIEArgs,SceMiDiniPCIEIfc#(i));

   module buildSceMi#(SceMiModule#(i) mod, SceMiDiniPCIEArgs args)
                     (SceMiDiniPCIEIfc#(i));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build = buildSceMiPCIEDini( mod, args.fpga_clk, args.fpga_rst, args.noc_q_clk, args.noc_a_clk, args.noc_reset_n, args.link_type );

      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule

endinstance

// Utility wrapper that creates the SceMiClockArgs structure needed to
// pass in externally defined clocks.
module buildSceMiWithExternalClocks#( SceMiModule#(i) mod
                                    , Vector#(n,Clock) clks_in
                                    , Vector#(n,Reset) rsts_in
                                    , SceMiLinkType link_type)
                                    (i ifc);
   SceMiClockArgs#(n) args;
   args.clks_in   = clks_in;
   args.rsts_in   = rsts_in;
   args.link_type = link_type;

   (* hide *)
   let _m <- buildSceMi(mod,args);
   return _m;
endmodule: buildSceMiWithExternalClocks

//
// SCE-MI Utility Functions and Modules
//

// Placeholder for a constant supplied during infrastructure linkage
module mkSceMiConstant(ReadOnly#(a)) provisos(Bits#(a,_));
   (*hide*)
   Reg#(a) _x <- mkRegU();
   return regToReadOnly(_x);
endmodule: mkSceMiConstant

// Look up domain info for a given clock number
module [SceMiModule] sceMiLookupClock#(Integer clockNum)
                                      (Maybe#(DomainInfo));

   SceMiModuleState state <- getContext();
   return List::lookup(clockNum, state.cdomains);

endmodule: sceMiLookupClock

module [SceMiModule] sceMiSetUClock#(SceMiClockPortIfc clk_port) ();

   DomainInfo di = DomainInfo { clk: clk_port.cclock, rst: clk_port.creset, external: False };
   SceMiModuleState state <- getContext();
   state.udomain =tagged Valid di;
   putContext(state);

endmodule


// Get the uncontrolled clock, if available
module [SceMiModule] sceMiGetUClock(Clock);

   Clock clk = noClock;

   SceMiModuleState state <- getContext();
   if (state.udomain matches tagged Valid .dom)
      clk = dom.clk;
   else begin
      SceMiClockControlIfc clockControl0 <- mkSceMiClockControl(0,True, True);
      clk = clockControl0.uclock;
   end

   return clk;

endmodule: sceMiGetUClock

// Get the uncontrolled reset, if available
module [SceMiModule] sceMiGetUReset(Reset);

   Reset rst = noReset;

   SceMiModuleState state <- getContext();
   if (state.udomain matches tagged Valid .dom)
      rst = dom.rst;
   else begin
      SceMiClockControlIfc clockControl0 <- mkSceMiClockControl(0,True, True);
      rst = clockControl0.ureset;
   end

   return rst;

endmodule: sceMiGetUReset

// Retrieve the current cycle stamp (in the uclock domain)
module [SceMiModule] sceMiCycleStamp(ReadOnly#(SceMiCycleStamp));

   SceMiModuleState state <- getContext();
   return state.stamp;

endmodule: sceMiCycleStamp


endpackage: SceMiCore
