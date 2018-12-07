// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxB2000T;

import NoCUtils::*;
import XilinxPCIE::*;
import XilinxDDR3::*;

import PCIE::*;
import LEDController::*;

export B2000T_FPGA(..);
export B2000T_FPGA_DDR3(..);
export B2000T_FPGA_Q(..);

interface B2000T_FPGA;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* always_ready *)
   method Bit#(4) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
endinterface

interface B2000T_FPGA_DDR3;
   (* prefix="TRAIN" *)
   interface TrainingSignals training;
   (* prefix="NOC_SOURCE" *)
   interface FPGASource#(4) source;
   (* prefix="NOC_SINK" *)
   interface FPGASink#(4) sink;
   (* prefix = "DDR3" *)
   interface DDR3_Pins_B2000T ddr3;
   (* always_ready *)
   method Bit#(4) leds();
   (* always_ready *)
   method Bool isDDRReady();
   (* always_ready *)
   method Bool isOutOfReset();
endinterface

interface B2000T_FPGA_Q;
   interface Clock             refclk125;
   interface Reset             refrst125;
   interface Clock             fpga_clk;
   interface LED#(4)           leds;
   interface PCIE_EXP#(4)      pcie;
   interface TrainingSignals   training;
   interface FPGASource#(4)    source;
   interface FPGASink#(4)      sink;
   (* always_ready, always_enabled *)
   method    Action isDDRReady(Bool i);
   (* always_ready, always_enabled *)
   method    Action isOutOfReset(Bool i);
endinterface

endpackage: XilinxB2000T
