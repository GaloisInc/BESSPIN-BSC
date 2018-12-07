// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxPDV72KR2;

import XilinxPCIE::*;

export PDV72KR2_FPGA(..);

interface PDV72KR2_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready, result = "PCIE_NCWAKE" *)
   method Bit#(1) ncwake();
   (* always_ready, result = "PCIE_NCPRSNT" *)
   method Bit#(1) ncprsnt();
   (* always_ready *)
   method Bit#(4) leds();
endinterface

endpackage: XilinxPDV72KR2
