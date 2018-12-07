// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxRPP2SPLIT;

import NoCUtils::*;
import XilinxPCIE::*;
import XilinxDDR3::*;

import PCIE::*;
import LEDController::*;

export RPP2SPLIT_FPGA(..);
export RPP2SPLIT_FPGA_DDR3(..);
export RPP2SPLIT_FPGA_Q(..);

interface RPP2SPLIT_FPGA;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* always_ready *)
   method Bit#(8) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
endinterface

interface RPP2SPLIT_FPGA_DDR3;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* always_ready *)
   method Bit#(8) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
   (* prefix = "DDR3" *)
   interface DDR3_Pins_DNV7F2A ddr3;
endinterface

interface RPP2SPLIT_FPGA_Q;
   interface Clock             refclk125;
   interface Reset             refrst125;
   interface Clock             fpga_clk;
   interface LED#(8)           leds;
   interface PCIE_EXP#(4)      pcie;
   interface TrainingSignals   training;
   interface FPGASource#(4)    source;
   interface FPGASink#(4)      sink;
   (* always_ready, always_enabled *)
   method    Action isDDRReady(Bool i);
   (* always_ready, always_enabled *)
   method    Action isOutOfReset(Bool i);
   (* always_ready *)
   method Bit#(1) cprsnt();
   (* always_ready *)
   method Bit#(1) cwake();
   (* always_ready *)
   method Bit#(1) osc_oe_real();
   (* always_ready *)
   method Bit#(2) osc_fs();
endinterface

endpackage: XilinxRPP2SPLIT
