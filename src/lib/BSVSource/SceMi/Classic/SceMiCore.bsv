// Copyright (c) 2008- 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package SceMiCore;

// This is the main SceMi package.  It exports the standard
// interfaces and modules which implement them.  These modules
// are parameterized on the SceMiLinkType, and are really
// just wrappers which dispatch to the specific module
// implementations for the chosen SceMiLinkType.

import SceMiDefines::*;
import SceMiInternals::*;
import SceMiTCP::*;
import SceMiMacros::*;
import SceMiEveMacros::*;
import SceMiAldecMacros::*;
import SceMiPCIE::*;

import XilinxPCIE::*; // workaround bug in typeclass exporting
import XilinxVirtex5PCIE::*;
import XilinxVirtex6PCIE::*;
import XilinxKintex7PCIE::*;

import List::*;
import Vector::*;
import ModuleContext::*;
import DefaultValue::*;

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

export SceMiClockArgs(..), SceMiV5PCIEArgs(..), SceMiDiniPCIEArgs(..), SceMiV6PCIEArgs(..);
export SceMiV5PCIEIfc(..), SceMiV6PCIEIfc(..), SceMiDiniPCIEIfc(..);
export SceMiK7PCIEIfc(..), SceMiK7PCIEArgs(..);
export mkSceMiExternalClockPort, buildSceMiWithExternalClocks;
export SceMiClockConfiguration(..);
export DomainInfo(..), sceMiLookupClock, sceMiCycleStamp;
export sceMiSetUClock, sceMiGetUClock, sceMiGetUReset;

export mkSceMiConstant;

export SceMiCycleStamp, SceMiLinkType(..);


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
		TCP:          mkSceMiTCPMessageInPort   (clocked_by uClock, reset_by uReset);
		SCEMI:        mkSceMiMacroMessageInPort (clocked_by uClock, reset_by uReset);
		EVE:          mkSceMiEveMessageInPort   (clocked_by uClock, reset_by uReset);
		ALDEC:        mkSceMiMacroMessageInPort (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX5: mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX6: mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
		PCIE_KINTEX7: mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX7: mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEXU: mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
		PCIE_DINI:    mkSceMiPCIEMessageInPort  (clocked_by uClock, reset_by uReset);
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
		TCP:          mkSceMiTCPMessageOutPort   (clocked_by uClock, reset_by uReset);
		SCEMI:        mkSceMiMacroMessageOutPort (clocked_by uClock, reset_by uReset);
		EVE:          mkSceMiEveMessageOutPort   (clocked_by uClock, reset_by uReset);
		ALDEC:        mkSceMiMacroMessageOutPort (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX5: mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX6: mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
		PCIE_KINTEX7: mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEX7: mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
		PCIE_VIRTEXU: mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
		PCIE_DINI:    mkSceMiPCIEMessageOutPort  (clocked_by uClock, reset_by uReset);
	     endcase;
   return _m;
endmodule: mkSceMiMessageOutPort

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
		TCP:   mkSceMiTCPClockPort( conf.clockNum
			  		  , conf.ratioNumerator, conf.ratioDenominator
					  , conf.dutyHi, conf.dutyLo, conf.phase
					  , conf.resetCycles
					  , conf.clockGroup
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
		       mkSceMiPCIEClockPort( conf.clockNum
			  		   , conf.ratioNumerator, conf.ratioDenominator
					   , conf.dutyHi, conf.dutyLo, conf.phase
					   , conf.resetCycles
					   , conf.clockGroup
					   , link_type
					   );
		PCIE_VIRTEX6:
		       mkSceMiPCIEClockPort( conf.clockNum
			  		   , conf.ratioNumerator, conf.ratioDenominator
					   , conf.dutyHi, conf.dutyLo, conf.phase
					   , conf.resetCycles
					   , conf.clockGroup
					   , link_type
					   );
		PCIE_KINTEX7:
		       mkSceMiPCIEClockPort( conf.clockNum
			  		   , conf.ratioNumerator, conf.ratioDenominator
					   , conf.dutyHi, conf.dutyLo, conf.phase
					   , conf.resetCycles
					   , conf.clockGroup
					   , link_type
					   );
		PCIE_VIRTEX7:
		       mkSceMiPCIEClockPort( conf.clockNum
			  		   , conf.ratioNumerator, conf.ratioDenominator
					   , conf.dutyHi, conf.dutyLo, conf.phase
					   , conf.resetCycles
					   , conf.clockGroup
					   , link_type
					   );
		PCIE_VIRTEXU:
		       mkSceMiPCIEClockPort( conf.clockNum
			  		   , conf.ratioNumerator, conf.ratioDenominator
					   , conf.dutyHi, conf.dutyLo, conf.phase
					   , conf.resetCycles
					   , conf.clockGroup
					   , link_type
					   );
		PCIE_DINI:
		       mkSceMiPCIEClockPort( conf.clockNum
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
		TCP:          mkSceMiTCPClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		SCEMI:        mkSceMiMacroClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		EVE:          mkSceMiEveClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		ALDEC:        mkSceMiMacroClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_VIRTEX5: mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_VIRTEX6: mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_KINTEX7: mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_VIRTEX7: mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_VIRTEXU: mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
		PCIE_DINI:    mkSceMiPCIEClockControl(clockNum, allow_pos_edge, allow_neg_edge);
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
      SceMiClockArgs#(0) args;
      args.clks_in   = Vector::nil;
      args.rsts_in   = Vector::nil;
      args.link_type = link_type;
   
      if (link_type == TCP) begin
	 ReadOnly#(UInt#(32)) tcp_port_number <- mkSceMiConstant();
      end
      
      (* hide *)
      let _m <- buildSceMi(mod,args);
      return _m;
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
		   TCP:     buildSceMiTCP(mod, args.clks_in, args.rsts_in);
		   SCEMI:   buildSceMiMacro(mod, args.clks_in, args.rsts_in, args.link_type);
		   EVE:     buildSceMiEve(mod, args.clks_in, args.rsts_in, args.link_type);
		   ALDEC:   buildSceMiMacro(mod, args.clks_in, args.rsts_in, args.link_type);
		   default: error("Selected link type does not support external clocks");
		endcase;
      (* hide *)
      let _m <- liftModule(build);

      // This is required for the TCP link type, to
      // detect when a shutdown signal arrives from
      // the TB, even if the DUT is not sending or
      // receiving any messages.
      if (args.link_type == TCP) begin
	 (* hide *)
	 Empty _poll <- mkSceMiTCPShutdownPoll();
      end

      return _m;
   endmodule
endinstance

// A buildSceMi instance to support a V5 PCIE link
instance SceMiBuilder#(i,SceMiV5PCIEArgs,SceMiV5PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex5PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV5PCIEArgs args)
		     (SceMiV5PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex5PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build =
                case (args.link_type) matches
		   PCIE_VIRTEX5: buildSceMiPCIEV5( mod
		                                 , args.pci_sys_clk_p
					         , args.pci_sys_clk_n
						 , args.pci_sys_reset
						 , args.ref_clk
						 , args.link_type
					         );
		   default: error("Link type does not match PCIE_VIRTEX5");
		endcase;
      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule
endinstance

// A buildSceMi instance to support a V6 PCIE link
instance SceMiBuilder#(i,SceMiV6PCIEArgs,SceMiV6PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectVirtex6PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiV6PCIEArgs args)
		     (SceMiV6PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectVirtex6PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build =
                case (args.link_type) matches
		   PCIE_VIRTEX6: buildSceMiPCIEV6( mod
		                                 , args.pci_sys_clk_p
					         , args.pci_sys_clk_n
						 , args.pci_sys_reset
						 , args.ref_clk
						 , args.link_type
					         );
		   default: error("Link type does not match PCIE_VIRTEX6");
		endcase;
      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule
endinstance

// A buildSceMi instance to support a K7 PCIE link
instance SceMiBuilder#(i,SceMiK7PCIEArgs,SceMiK7PCIEIfc#(i,lanes))
   provisos(Add#(1,_,lanes), SelectKintex7PCIE#(lanes));

   module buildSceMi#(SceMiModule#(i) mod, SceMiK7PCIEArgs args)
		     (SceMiK7PCIEIfc#(i,lanes))
      provisos(Add#(1,_,lanes), SelectKintex7PCIE#(lanes));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build =
                case (args.link_type) matches
		   PCIE_KINTEX7: buildSceMiPCIEK7( mod
		                                 , args.pci_sys_clk_p
					         , args.pci_sys_clk_n
						 , args.pci_sys_reset
						 , args.ref_clk
						 , args.link_type
					         );
		   default: error("Link type does not match PCIE_KINTEX7");
		endcase;
      (* hide *)
      let _m <- liftModule(build);

      return _m;
   endmodule
endinstance

// A buildSceMi instance to support a Dini PCIE link
instance SceMiBuilder#(i,SceMiDiniPCIEArgs,SceMiDiniPCIEIfc#(i));

   module buildSceMi#(SceMiModule#(i) mod, SceMiDiniPCIEArgs args)
		     (SceMiDiniPCIEIfc#(i));

      // record link type parameter for infrastructure linkage tool
      Ignored param_link_type <- mkSceMiLinkTypeParameter(args.link_type);

      // Dispatch to builder for specific linkage type
      let build =
                case (args.link_type) matches
		   PCIE_DINI: buildSceMiPCIEDini( mod
		                                , args.pci_sample_clk
					        , args.pci_sample_rstn
		                                , args.link_type
					        );
		   default: error("Link type does not match PCIE_DINI");
		endcase;
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

////////////////////////////////////////////////////////////////////////////////
/// "Filler" SceMi pipes definitions to allow bluenoc-only .bsv code to
/// compile in classic (but of course not function).
////////////////////////////////////////////////////////////////////////////////

module [SceMiModule] mkSceMiInputPipe#(Integer depth, Visibility vis)
                                      (SceMiInputPipeIfc#(m,e) ifc);
   error("mkSceMiInputPipe is not supported for SceMi Classic.");
   return ?;
endmodule

module [SceMiModule] mkSceMiInputPipeP#(SceMiInputPipeParameters params)
                                       (SceMiInputPipeIfc#(m,e) ifc);
   error("mkSceMiInputPipeP is not supported for SceMi Classic.");
   return ?;
endmodule

module [SceMiModule] mkSceMiOutputPipe#(Integer depth, Visibility vis)
                                       (SceMiOutputPipeIfc#(m,e) ifc);
   error("mkSceMiOutputPipe is not supported for SceMi Classic.");
   return ?;
endmodule

module [SceMiModule] mkSceMiOutputPipeP#(SceMiOutputPipeParameters params)
					(SceMiOutputPipeIfc#(m,e) ifc);
   error("mkSceMiOutputPipeP is not supported for SceMi Classic.");
   return ?;
endmodule

endpackage: SceMiCore
