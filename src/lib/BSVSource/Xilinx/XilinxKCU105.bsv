// Copyright (c) 2007 - 2016 Bluespec, Inc.  All rights reserved.
// $Revision: 32966 $
// $Date: 2014-01-07 08:15:07 -0500 (Tue, 07 Jan 2014) $

package XilinxKCU105;

import XilinxPCIE::*;
import XilinxDDR4::*;
import Vector::*;

export KCU105_FPGA(..);
export KCU105_FPGA_DDR4(..);

interface KCU105_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* always_ready *)
   method Bit#(8)         leds();
endinterface

interface KCU105_FPGA_DDR4;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(8)     pcie;
   (* prefix="DDR4" *)
   interface DDR4_Pins_KCU105 ddr4;
   (* always_ready *)
   method Bit#(8)             leds();
endinterface

endpackage: XilinxKCU105
