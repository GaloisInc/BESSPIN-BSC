// Copyright (c) 2007 - 2017 Bluespec, Inc.  All rights reserved.
// $Revision: 32966 $
// $Date: 2014-01-07 08:15:07 -0500 (Tue, 07 Jan 2014) $

package XilinxKLVUF4A;

import XilinxPCIE::*;

export KLVUF4A_FPGA(..);

interface KLVUF4A_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready, result = "PCIE_CPRSNT" *)
   method Bit#(1) cprsnt();
   (* always_ready, result = "PCIE_CWAKE" *)
   method Bit#(1) cwake();
   (* always_ready *)
   method Bit#(4) leds();
endinterface

endpackage: XilinxKLVUF4A
