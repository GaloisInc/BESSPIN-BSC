// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxKC705;

import XilinxPCIE::*;
import XilinxDDR3::*;
import Vector::*;

export KC705_FPGA(..);
export KC705_FPGA_DDR3(..);

interface KC705_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface KC705_FPGA_DDR3;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* prefix="DDR3" *)
   interface DDR3_Pins_KC705 ddr3;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxKC705
