// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxDNV7F2A;

import XilinxPCIE::*;
import XilinxDDR3::*;

export DNV7F2A_FPGA(..);
export DNV7F2A_FPGA_DDR3(..);

interface DNV7F2A_FPGA;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* always_ready, result = "PCIE_CPRSNT" *)
   method Bit#(1) cprsnt();
   (* always_ready, result = "PCIE_CWAKE" *)
   method Bit#(1) cwake();
   (* always_ready, result = "PCIE_OSC_OE_REAL" *)
   method Bit#(1) osc_oe_real();
   (* always_ready, result = "PCIE_OSC_FS" *)
   method Bit#(2) osc_fs();
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface DNV7F2A_FPGA_DDR3;
   (* prefix = "PCIE" *)
   interface PCIE_EXP#(4) pcie;
   (* prefix = "DDR3" *)
   interface DDR3_Pins_DNV7F2A ddr3;
   (* always_ready, result = "PCIE_CPRSNT" *)
   method Bit#(1) cprsnt();
   (* always_ready, result = "PCIE_CWAKE" *)
   method Bit#(1) cwake();
   (* always_ready, result = "PCIE_OSC_OE_REAL" *)
   method Bit#(1) osc_oe_real();
   (* always_ready, result = "PCIE_OSC_FS" *)
   method Bit#(2) osc_fs();
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxDNV7F2A
