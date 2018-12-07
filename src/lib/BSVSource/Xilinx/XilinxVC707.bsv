// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxVC707;

import XilinxPCIE::*;
import XilinxDDR3::*;

export VC707_FPGA(..);
export VC707_FPGA_DDR3(..);

interface VC707_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface VC707_FPGA_DDR3;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* prefix="DDR3" *)
   interface DDR3_Pins_VC707 ddr3;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxVC707
