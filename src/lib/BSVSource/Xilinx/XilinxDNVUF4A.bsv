// Copyright (c) 2007 - 2016 Bluespec, Inc.  All rights reserved.
// $Revision: 32966 $
// $Date: 2014-01-07 08:15:07 -0500 (Tue, 07 Jan 2014) $

package XilinxDNVUF4A;

import XilinxPCIE::*;

export DNVUF4A_FPGA(..);

interface DNVUF4A_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready, result = "PCIE_CPRSNT" *)
   method Bit#(1) cprsnt();
   (* always_ready, result = "PCIE_CWAKE" *)
   method Bit#(1) cwake();
   (* always_ready *)
   method Bit#(4) leds();
endinterface

endpackage: XilinxDNVUF4A
