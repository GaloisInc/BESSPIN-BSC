// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package XilinxML605;

import XilinxPCIE::*;
import XilinxDDR3::*;
import Vector::*;
import LCDController::*;
import GPIOController::*;

export ML605_FPGA(..);
export ML605_FPGA_DDR3(..);

interface ML605_FPGA;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

interface ML605_FPGA_DDR3;
   (* prefix="PCIE" *)
   interface PCIE_EXP#(8) pcie;
   (* prefix="DDR3" *)
   interface DDR3_Pins_ML605 ddr3;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: XilinxML605
