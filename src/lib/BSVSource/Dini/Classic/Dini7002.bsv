// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Dini7002;

import DiniPCIE::*;
import DiniRS232::*;
import DiniSODIMM::*;

export DN7002_FPGA_A(..),      DN7002_FPGA_B(..);
export DN7002_FPGA_A_DDR2(..), DN7002_FPGA_B_DDR2(..);
export DN7002_FPGA_A_SRAM(..), DN7002_FPGA_B_SRAM(..);

// Non-SODIMM variants

interface DN7002_FPGA_A;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

// DDR2 variants

interface DN7002_FPGA_A_DDR2;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

// SRAM variants

interface DN7002_FPGA_A_SRAM;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(6) leds();
endinterface

interface DN7002_FPGA_B_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(8) leds();
endinterface

endpackage: Dini7002
