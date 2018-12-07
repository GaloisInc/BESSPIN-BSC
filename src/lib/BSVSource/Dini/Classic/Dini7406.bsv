// Copyright (c) 2007 - 2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Dini7406;

import DiniPCIE::*;
import DiniRS232::*;
import DiniSODIMM::*;

export DN7406_FPGA_A(..);
export DN7406_FPGA_B(..);
export DN7406_FPGA_C(..);
export DN7406_FPGA_D(..);
export DN7406_FPGA_E(..);
export DN7406_FPGA_F(..);
export DN7406_FPGA_A_DDR2(..);
export DN7406_FPGA_C_DDR2(..);
export DN7406_FPGA_D_DDR2(..);
export DN7406_FPGA_F_DDR2(..);
export DN7406_FPGA_A_SRAM(..);
export DN7406_FPGA_C_SRAM(..);
export DN7406_FPGA_D_SRAM(..);
export DN7406_FPGA_F_SRAM(..);

// non-SODIMM variants

interface DN7406_FPGA_A;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_B;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(13) leds();
endinterface

interface DN7406_FPGA_C;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_E;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_F;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

// DDR2 variants

interface DN7406_FPGA_A_DDR2;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_C_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_F_DDR2;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="DDR2" *)
   interface DDR2_Pins ddr2;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

// SRAM variants

interface DN7406_FPGA_A_SRAM;
   (* prefix="PCIE" *)
   interface PCIE_Pins pcie;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(3) leds();
endinterface

interface DN7406_FPGA_C_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(11) leds();
endinterface

interface DN7406_FPGA_D_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

interface DN7406_FPGA_F_SRAM;
   (* prefix="RS232" *)
   interface RS232_Pins rs232;
   (* prefix="SRAM" *)
   interface SRAM_Pins sram;
   (* always_ready *)
   method Bit#(14) leds();
endinterface

endpackage: Dini7406
