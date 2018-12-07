// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxML50x;

import XilinxPCIE::*;
import XilinxDDR2::*;

export ML50x_FPGA(..);
export ML50x_FPGA_DDR2(..);

interface ML50x_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(1) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface ML50x_FPGA_DDR2;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(1) pcie;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxML50x
