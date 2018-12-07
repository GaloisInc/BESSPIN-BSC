// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxDN10GK7LL;

import XilinxPCIE::*;
import XilinxDDR3::*;

export DN10GK7LL_FPGA(..);
export DN10GK7LL_FPGA_DDR3(..);

interface DN10GK7LL_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready *)
   method Bool    clk_enable();
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface DN10GK7LL_FPGA_DDR3;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* prefix="DDR3" *)
   interface DDR3_Pins_10GK7LL ddr3;
   (* always_ready *)
   method Bool    clk_enable();
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxDN10GK7LL
