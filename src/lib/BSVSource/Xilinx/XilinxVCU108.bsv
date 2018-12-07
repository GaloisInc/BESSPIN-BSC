// Copyright (c) 2007 - 2016 Bluespec, Inc.  All rights reserved.
// $Revision: 32966 $
// $Date: 2014-01-07 08:15:07 -0500 (Tue, 07 Jan 2014) $

package XilinxVCU108;

import XilinxPCIE::*;

export VCU108_FPGA(..);

interface VCU108_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxVCU108
