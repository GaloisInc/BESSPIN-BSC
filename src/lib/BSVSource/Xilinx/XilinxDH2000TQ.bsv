// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxDH2000TQ;

import XilinxPCIE::*;
import XilinxDDR2::*;

export DH2000TQ_FPGA(..);
export DH2000TQ_FPGA_DDR2(..);

interface DH2000TQ_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface DH2000TQ_FPGA_DDR2;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* prefix="DDR2" *)
   interface DDR2_Pins_V7 ddr2;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxDH2000TQ
