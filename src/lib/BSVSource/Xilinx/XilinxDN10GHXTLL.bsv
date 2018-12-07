// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxDN10GHXTLL;

import XilinxPCIE::*;
import Xilinx10GE::*;

export DN10GHXTLL_FPGA(..);

interface DN10GHXTLL_FPGA;
   interface Clock    dclk;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* prefix="SFP0" *)
   interface SFP_Pins sfp0;
   (* prefix="SFP1" *)
   interface SFP_Pins sfp1;
   (* prefix="SFP2" *)
   interface SFP_Pins sfp2;      
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxDN10GHXTLL
